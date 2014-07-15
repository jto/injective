/*
 * Copyright 2014 Pascal Voitot (@mandubian)
 *
 */
import scalaz.{Free, Coyoneda, Functor}
import Coyoneda.CoyonedaF

import shapeless.ops.coproduct.{Inject, Selector}
import shapeless.{Coproduct, Inl, Inr, CNil, :+:, Poly1}
import shapeless.poly._

trait LowPriorityRichNat {
  implicit class RichNat0[F[_], R[_]](val f: F ~> R) {
    def or[G[_]](g: G ~> R) =
      new ~>[({ type C[T] = F[T] :+: G[T] :+: CNil })#C, R] {
        def apply[T](c: F[T] :+: G[T] :+: CNil): R[T] = c match {
          case Inl(h) => f(h)
          case Inr(Inl(t)) => g(t)
        }
      }
  }
}

object Shapoyo extends LowPriorityRichNat {
  import Un._

  implicit class RichNat[T, F[_] <: Coproduct, R[_]](val f: F ~> R)/*(implicit val u: +->+[T]#λ[F[T]])*/ {
    def or[G[_]](g: G ~> R): ({type λ[T] = G[T] :+: F[T]})#λ ~> R = {
      new ~>[({ type C[T] = G[T] :+: F[T] })#C, R] {
        def apply[T](c: G[T] :+: F[T]): R[T] = c match {
          case Inl(h) => g(h)
          case Inr(t) => f(t)
        }
      }
    }
  }

  def liftCoyo[F[_], G[_]](fg: F ~> G): CoyonedaF[F]#A ~> CoyonedaF[G]#A =
    new (Coyoneda.CoyonedaF[F]#A ~> Coyoneda.CoyonedaF[G]#A) {
      def apply[A](c: Coyoneda[F, A]) = {
        Coyoneda.apply(fg(c.fi))(c.k)
      }
    }

  def liftCoyoLeft[F[_], G[_]: Functor](fg: F ~> G): CoyonedaF[F]#A ~> G = {
    type CF[A] = Coyoneda[F, A]
    type CG[A] = Coyoneda[G, A]
    new (CF ~> G) {
      def apply[A](c: CF[A]) = liftCoyo(fg)(c).run
    }
  }

  implicit def toNat[F[_], G[_]](nat: F ~> G) = new scalaz.~>[F, G] {
    def apply[T](ft: F[T]): G[T] = nat(ft)
  }

  def copoyo[C[_] <: Coproduct, F[_], A](fa: F[A])(implicit inj: Inject[C[A], F[A]]): Free.FreeC[C, A] =
      Free.liftFC(Coproduct[C[A]](fa))

  class Copoyo[C[_] <: Coproduct] {
    def apply[F[_], A](fa: F[A])(implicit inj: Inject[C[A], F[A]]): Free.FreeC[C, A] =
      Free.liftFC(Coproduct[C[A]](fa))
  }

  object Copoyo {
    def apply[C[_] <: Coproduct] = new Copoyo[C]
  }

  class App[C[_] <: Coproduct] {
    type Copro[A] = C[A]
    type Coyo[A]  = Coyoneda[Copro, A]

    implicit def CopoyoApp[F[_], A](f: F[A])(implicit inj: Inject[C[A], F[A]]) = Copoyo[C](f)
  }
}



object Un {
  trait UnaryAConstraint[C <: Coproduct, A]

  type +->+[A] = {
    type λ[C <: Coproduct] = UnaryAConstraint[C, A]
  }

  implicit def hnilUnaryTC[A] = new UnaryAConstraint[CNil, A] {}
  implicit def hlistUnaryTC1[F[_], C <: Coproduct, A](implicit utct : UnaryAConstraint[C, A]) =
    new UnaryAConstraint[F[A] :+: C, A] {}

  implicit class CoproductA[A, C <: Coproduct : +->+[A]#λ](c: C) {
    type CA[A] = CoproductA[A, C]

    def liftFreeC: Free.FreeC[CA, A] = {
      val ca: CA[A] = this
      Free.liftFC(ca)
    }
  }
}