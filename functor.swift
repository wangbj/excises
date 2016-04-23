import Foundation

public struct K0 {}
public struct K1<A> {}

public protocol Functor{
    associatedtype A
    associatedtype B
    associatedtype FA = K1<A>
    associatedtype FB = K1<B>

    func fmap(f : A -> B, _ fa : FA) -> FB
}

public enum Maybe<E> {
    case Nothing
    case Just(E)
}

extension Maybe : Functor {
    public typealias A = E
    public typealias B = Any
    public typealias FA = Maybe<A>
    public typealias FB = Maybe<B>
 
    public func fmap<B> (f : A -> B, _ fa: FA) -> Maybe<B> {
        switch(fa) {
        case .Nothing:
            return .Nothing
        case let (.Just(a)):
            return .Just(f(a))
        }
    }
}

public func fmap<A, B> (f : A -> B, _ fa : Maybe<A>) -> Maybe<B> {
    return fa.fmap(f, fa)
}

func succ (x : Int) -> Int {
    return 1 + x
}

let x1 : Maybe<Int> = .Nothing
let x2 : Maybe<Int> = .Just(1)
let x3 = fmap(succ, x2)
