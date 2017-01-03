import Foundation


//
// Parsley
// a little sprig of a parser
//
// Violet Baddley
//
// Written for Swift 2.2
// I don't yet expect all of this to work,
// but at the bottom it parses whitespace
// including comments.
//


enum ASTKind {
  case generic
  case terminal
  case sequence
  case `nil`
  
  case whitespace
  case commentary
  case bareWord
  case expression(ExpressionKind)
  
}

enum ExpressionKind {
  case symbolicReference
}

extension ASTKind: Equatable {}
func == (lhs: ASTKind, rhs: ASTKind) -> Bool {
  switch (lhs, rhs) {
  case (.generic, .generic):
    return true
  case (.terminal, .terminal):
    return true
  case (.sequence, .sequence):
    return true
  case (.nil, .nil):
    return true
  case (.whitespace, .whitespace):
    return true
  case (.commentary, .commentary):
    return true
  case (.bareWord, .bareWord):
    return true
    
  case (let .expression(lhsExpKind), let .expression(rhsExpKind)):
    return lhsExpKind == rhsExpKind
    
  default:
    return false
  }
}

enum ParsError: Error {
  case emptyInput(origin: String)
  case noMatch(origin: String)
}


// Lovely little Ruby goodness:
func * (lhs: String, rhs: Int) -> String {
  var produce = ""
  produce.reserveCapacity(lhs.utf8.count * rhs)
  for _ in 0..<rhs {
    produce.append(lhs)
  }
  
  return produce
}


class ParsleyAST {
  let kind: ASTKind
  let text: String
  let parentOffset: Int
  let children: [ParsleyAST]
  
  required init(kind: ASTKind, children: [ParsleyAST], text: String, parentOffset: Int) {
    self.kind = kind
    self.text = text
    self.parentOffset = parentOffset
    self.children = children
  }
  
  convenience init(children: [ParsleyAST], text: String, parentOffset: Int) {
    self.init(kind: .generic, children: children, text: text, parentOffset: parentOffset)
  }
  
  convenience init(children: [ParsleyAST], text: String) {
    self.init(kind: .generic, children: children, text: text, parentOffset: 0)
  }
  
  convenience init() {
    self.init(kind: .nil, children: [], text: "", parentOffset: 0)
  }
  
  func ofKind(_ imposed: ASTKind) -> Self {
    return type(of: self).init( kind: imposed,
                                  children: self.children,
                                  text: self.text,
                                  parentOffset: self.parentOffset )
  }
  
  func sortaYAML(_ indentationLevel: Int = 0) -> String {
    let indentation = "  " * indentationLevel
    let childrenMap = children.map { child in child.sortaYAML(indentationLevel + 1) }
      .joined(separator: "\n")
    
    switch kind {
    case .sequence:
      return indentation + "Sequence:\n\(childrenMap)"
      
    case .nil:
      return ""
      
    case .whitespace:
      let iffyChildren = childrenMap.isEmpty ? "" : ":\n\(childrenMap)"
      return indentation + "Whitespace (\(text.unicodeScalars.count) chars)\(iffyChildren)"
      
    default:
      let iffyChildren = childrenMap.isEmpty ? "" : ":\n\(childrenMap)"
      return indentation + "\(kind) “\(text)”\(iffyChildren)"
      
    }
  }
  
}



protocol Parsley {
  func pars(_ text: String, parentOffset: Int) throws -> ParsleyAST
  
  func ofKind(_ imposed: ASTKind) -> Self
  var kind: ASTKind { get }
}



//
// Parses Terminal AST nodes.
// This sprig represents a character set, and parses consecutive
// characters from input (greedily) that match the set.
// You can either define the character set as including specific characters
// within a string (ParsleyTerminal(with: "abc") or ~"abc"), or you can
// define it as excluding specific characters
// (ParsleyTerminal(excepting: "xyz") or ~!"xyz").
//
protocol ParsleyTerminal: Parsley {
  
}

class ParsleyExact: ParsleyTerminal {
  fileprivate let exact: String
  let kind: ASTKind
  
  required init(text: String, kind: ASTKind) {
    self.exact = text
    self.kind = kind
  }

  func pars(_ text: String, parentOffset: Int) throws -> ParsleyAST {
    guard !text.isEmpty
      else { throw ParsError.emptyInput(origin: "in Terminal exactly \"\(self.exact)\"") }
    
    guard text.hasPrefix(self.exact)
      else { throw ParsError.noMatch(origin: "in Terminal exactly \"\(self.exact)\"") }
    
    return ParsleyAST(kind: self.kind,
                      children: [],
                      text: self.exact,
                      parentOffset: parentOffset)
    
  }
  
  func ofKind(_ imposed: ASTKind) -> Self {
    return type(of: self).init(text: self.exact, kind: imposed)
  }

}

class ParsleyCharoset: ParsleyTerminal {
  // A Char o' Set is not a "charset,"
  // it matches one character from a set.

  let sigChars: String.UnicodeScalarView
  let inverse: Bool
  var positive: Bool { return !inverse }
  let kind: ASTKind
  
  required init(with: String, kind: ASTKind = .terminal) {
    sigChars = with.unicodeScalars
    inverse = false
    self.kind = kind
  }
  
  required init(excepting: String, kind: ASTKind = .terminal) {
    sigChars = excepting.unicodeScalars
    inverse = true
    self.kind = kind
  }
  
  func ofKind(_ imposed: ASTKind) -> Self {
    if inverse {
      return type(of: self).init(excepting: String(sigChars), kind: imposed)
    } else {
      return type(of: self).init(with: String(sigChars), kind: imposed)
    }
  }
  
  func not() -> Self {
    if inverse {
      return type(of: self).init(with: String(sigChars), kind: self.kind)
    } else {
      return type(of: self).init(excepting: String(sigChars), kind: self.kind)
    }
  }
  
  func union(_ other: ParsleyWildcard) -> Self {
    // The logical union of the character sets. Any character included
    // in either operand will be included in the result.
    let resultantKind = (other.kind == self.kind) ? self.kind : .terminal
    
    if self.positive && other.positive {
      let combinedChars = String(self.sigChars) + String(other.sigChars)
      return type(of: self).init(with: combinedChars, kind: resultantKind)
      
    } else if self.inverse && other.inverse {
      // Keep only the mutual exclusions:
      var mutualChars = ""
      for potential in other.sigChars {
        if self.sigChars.contains(potential) {
          mutualChars.append(String(potential))
        }
      }
      return type(of: self).init(excepting: mutualChars, kind: resultantKind)
      
    } else /* one positive and other inverse */ {
      var excludedChars: String
      let includedChars: String.UnicodeScalarView
      if self.positive {
        excludedChars = String(other.sigChars)
        includedChars = self.sigChars
      } else {  // other is positive
        excludedChars = String(self.sigChars)
        includedChars = other.sigChars
      }
      // Same as the last one, but with roles reversed:
      for includedChar in includedChars {
        excludedChars = excludedChars.replacingOccurrences(of: String(includedChar),
                                                                           with: "")
      }
      return type(of: self).init(excepting: excludedChars, kind: resultantKind)
      
    }
  }
  
  func intersect(_ other: ParsleyWildcard) -> Self {
    // Invert both operands, union them, and invert the result back:
    return self.not().union( other.not() ).not()
  }
  
  
  func pars(_ text: String, parentOffset: Int) throws -> ParsleyAST {
    let text_chars = text.unicodeScalars
    guard !text_chars.isEmpty
      else { throw ParsError.emptyInput(origin: "in Terminal \(inverse ? "excepting" : "with") \"\(String(sigChars))\"") }
    
    // Surely there has to be a better way of doing this...
    let match = ""
    
    // Ensure we found something! This is different from the guard-
    // check above, in that we need to distinguish no-input from no-match.
    guard !match.isEmpty
      else { throw ParsError.noMatch(origin: "in Terminal \(inverse ? "excepting" : "with") \"\(String(sigChars))\"") }
    
    return ParsleyAST( kind: kind,
                       children: [],
                       text: match,
                       parentOffset: parentOffset )
  }
  
  
  fileprivate func charMatch(_ text_char: UnicodeScalar) -> Bool {
    let is_match = sigChars.contains(text_char)
    return is_match != inverse  // logical xor
  }
  
  
}


class ParsleyWildcard: ParsleyCharoset {
  // This is simply a repeatable run of a Char o' Set.
  
  override func pars(_ text: String, parentOffset: Int) throws -> ParsleyAST {
    let text_chars = text.unicodeScalars
    guard !text_chars.isEmpty
      else { throw ParsError.emptyInput(origin: "in Terminal \(inverse ? "excepting" : "with") \"\(String(sigChars))\"") }
    
    // Surely there has to be a better way of doing this...
    var match = ""
    for text_char in text_chars {
      if sigChars.contains(text_char) == inverse {
        break

      } else {
        match.append(String(text_char))

      }

    }


    // Ensure we found something! This is different from the guard-
    // check above, in that we need to distinguish no-input from no-match.
    guard !match.isEmpty
      else { throw ParsError.noMatch(origin: "in Terminal \(inverse ? "excepting" : "with") \"\(String(sigChars))\"") }
    
    return ParsleyAST( kind: kind,
                       children: [],
                       text: match,
                       parentOffset: parentOffset )
  }

}


class ParsleySeq: Parsley {
  let seq: [Parsley]
  let kind: ASTKind
  
  required init(fromArray: [Parsley], kind: ASTKind = .sequence) {
    self.seq = fromArray
    self.kind = kind
  }
  
  convenience init(_ seq: Parsley..., kind: ASTKind = .sequence) {
    self.init(fromArray: seq, kind: kind)
  }
  
  func ofKind(_ imposed: ASTKind) -> Self {
    return type(of: self).init(fromArray: self.seq, kind: imposed)
  }
  
  func pars(_ text: String, parentOffset: Int) throws -> ParsleyAST {
    let textChars = text.unicodeScalars
    
    var runningOffset = 0
    let initial: [ParsleyAST] = []
    let children = try seq.reduce(initial) { (memo, parsley) in
      let textRest = String(textChars.dropFirst(runningOffset))
      
      let match = try parsley.pars(textRest, parentOffset: runningOffset)
      runningOffset = match.parentOffset + match.text.unicodeScalars.count
      return memo + [ match ]
    }
    
    let matchedText = String(text.unicodeScalars.prefix(runningOffset))
    return ParsleyAST( kind: kind,
                       children: children,
                       text: matchedText,
                       parentOffset: parentOffset )
  }
  
}


class ParsleyRun: Parsley {
  let reps: Parsley
  let kind: ASTKind
  
  required init(_ reps: Parsley, kind: ASTKind = .sequence) {
    self.reps = reps
    self.kind = kind
  }
  
  func ofKind(_ imposed: ASTKind) -> Self {
    return type(of: self).init(reps, kind: imposed)
  }
  
  func pars(_ text: String, parentOffset: Int) throws -> ParsleyAST {
    let textChars = text.unicodeScalars
    
    var matches: [ParsleyAST] = []
    var runningOffset = 0
    while true {
      do {
        let textRest = String(textChars.dropFirst(runningOffset))
        
        let match = try reps.pars(textRest, parentOffset: runningOffset)
        matches.append(match)
        runningOffset = match.parentOffset + match.text.unicodeScalars.count
        
        if match.text.unicodeScalars.count == 0 {
          // We parsed but failed to eat any text.
          // If we continue, we will fail again infinitely.
          break
        }
        
      } catch ParsError.noMatch {
        break
      } catch ParsError.emptyInput {
        // Woooot! We parsed the entire input!
        break
      }
    }
    
    // A run requires at least one match.
    guard matches.count > 0
      else { throw ParsError.noMatch(origin: "in Run of \(reps) -> \(kind)") }
    
    let matchedText = String(text.unicodeScalars.prefix(runningOffset))
    return ParsleyAST(kind: kind, children: matches, text: matchedText, parentOffset: parentOffset)
  }
  
}


class ParsleyHopper: Parsley {
  let seq: [Parsley]
  let kind: ASTKind
  
  required init(fromArray: [Parsley], imposedKind: ASTKind = .nil) {
    assert(fromArray.count > 0, "A Hopper must have at least one alternative to try.")
    self.seq = fromArray
    self.kind = imposedKind
  }
  
  convenience init(_ seq: Parsley...) {
    self.init(fromArray: seq)
  }
  
  func ofKind(_ imposed: ASTKind) -> Self {
    return type(of: self).init(fromArray: seq, imposedKind: imposed)
  }
  
  func pars(_ text: String, parentOffset: Int) throws -> ParsleyAST {
    var firstMatch: ParsleyAST? = nil
    for hop in seq {
      do {
        firstMatch = try hop.pars(text, parentOffset: parentOffset)
        break
        
      } catch ParsError.noMatch {
        // Carry on
      }
    }
    
    // If none of them stuck, propagate the no-match:
    guard firstMatch != nil
      else { throw ParsError.noMatch(origin: "in Hopper of \(seq)") }
    
    if self.kind != .nil {
      // We have an explicit kind to impose upon the resulting AST:
      firstMatch = firstMatch!.ofKind(self.kind)
    }
    
    return firstMatch!
  }
  
}


class ParsleyOptional: Parsley {
  let sub: Parsley
  let kind: ASTKind
  
  required init(_ sub: Parsley) {
    self.sub = sub
    self.kind = sub.kind
  }
  
  func ofKind(_ imposed: ASTKind) -> Self {
    // Instead of trying to impose upon output, as the Hopper does,
    // we'll just impose the kind on the wrapped sprig:
    return type(of: self).init( sub.ofKind(imposed) )
  }
  
  func pars(_ text: String, parentOffset: Int) throws -> ParsleyAST {
    do {
      return try sub.pars(text, parentOffset: parentOffset)
      
    } catch ParsError.noMatch {
      return ParsleyAST()  // nil AST
    } catch ParsError.emptyInput {
      return ParsleyAST()  // nil AST
    }
  }
  
}



postfix operator ...
postfix func ... (reps: Parsley) -> ParsleyRun {
  return ParsleyRun(reps)
}

func + (lhs: Parsley, rhs: Parsley) -> ParsleySeq {
  let lhs_elements, rhs_elements: [Parsley]
  if let lhs_seq = lhs as? ParsleySeq {
    lhs_elements = lhs_seq.seq
  } else {
    lhs_elements = [lhs]
  }
  
  if let rhs_seq = rhs as? ParsleySeq {
    rhs_elements = rhs_seq.seq
  } else {
    rhs_elements = [rhs]
  }
  
  return ParsleySeq(fromArray: lhs_elements + rhs_elements)
  
}

func || (lhs: Parsley, rhs: Parsley) -> ParsleyHopper {
  let lhs_elements, rhs_elements: [Parsley]
  if let lhs_hopper = lhs as? ParsleyHopper {
    lhs_elements = lhs_hopper.seq
  } else {
    lhs_elements = [lhs]
  }
  
  if let rhs_hopper = rhs as? ParsleyHopper {
    rhs_elements = rhs_hopper.seq
  } else {
    rhs_elements = [rhs]
  }
  
  return ParsleyHopper(fromArray: lhs_elements + rhs_elements)
  
}

prefix func ! (term: ParsleyWildcard) -> ParsleyWildcard {
  return term.not()
}

prefix operator %
prefix func % (terminalExact: String) -> ParsleyExact {
  return ParsleyExact(text: terminalExact, kind: .terminal)
}

prefix operator ^
prefix func ^ (terminalChars: String) -> ParsleyCharoset {
  return ParsleyCharoset(with: terminalChars, kind: .terminal)
}

prefix operator !^
prefix func !^ (nonterminalChars: String) -> ParsleyCharoset {
  return ParsleyCharoset(excepting: nonterminalChars, kind: .terminal)
}

prefix operator *
prefix func * (terminalChars: String) -> ParsleyWildcard {
  return ParsleyWildcard(with: terminalChars)
}

prefix operator !*
prefix func !* (nonterminalChars: String) -> ParsleyWildcard {
  return ParsleyWildcard(excepting: nonterminalChars)
}

// Lower precedence than ternary conditional, higher than assignment:
precedencegroup SuffixPrecedence {
  associativity: left
  lowerThan: TernaryPrecedence
}
infix operator -- : SuffixPrecedence
func -- <ParsleyType: Parsley>(lhs: ParsleyType, rhs: ASTKind) -> ParsleyType {
  return lhs.ofKind(rhs)
}

postfix operator ~
postfix func ~ (sub: Parsley) -> ParsleyOptional {
  return ParsleyOptional(sub)
}

func & <LHSType: ParsleyWildcard> (lhs: LHSType, rhs: ParsleyWildcard) -> LHSType {
  return lhs.intersect(rhs)
}

func | <LHSType: ParsleyWildcard> (lhs: LHSType, rhs: ParsleyWildcard) -> LHSType {
  return lhs.union(rhs)
}



let literal_whitespace = *(" \n\r\t" +
  "\u{1680}\u{2000}\u{2001}\u{2002}\u{2003}\u{2004}\u{2005}\u{2006}\u{2007}" +
  "\u{2008}\u{2009}\u{200a}\u{202f}\u{205f}\u{3000}")  -- .whitespace


let comment_starter = %";"                      -- .commentary
let eol = %"\n"                                 -- .whitespace
let commentBody = !*"\n"                        -- .commentary
let commentary =
  comment_starter + commentBody + eol~          -- .commentary

let ws = (literal_whitespace || commentary)...  -- .whitespace


let block_openers = *"([{\"“‘"
let block_closers = *"}])\"”’"
let block_delimiters = block_openers | block_closers

let declaration_end = *"."

let token_separatives = block_delimiters
  | declaration_end
  | comment_starter
  | literal_whitespace
let bare_word = !token_separatives              -- .bareWord

let symbolic_reference_expression = bare_word   -- .expression(.symbolicReference)
let integer_literal = (*"-")~ + *"0123456789"
let dq_text_segments = !*"”" + *"”"
let dq_text_literal = *"“" + dq_text_segments + *"”"


let inputs = "" +
  "    ; placeholder-words => “hoge fuga piyo”: reversed." +
"\n    ; more commentary \n with lots of words that aren't whitespace"


do {
  let ws_match = try ws.pars(inputs, parentOffset: 0)
  print(ws_match.sortaYAML())
  
  
} catch let to_err as ParsError {
  print("Oh noes!!", to_err)
}






























