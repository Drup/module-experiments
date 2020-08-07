- Typo: strengthening for type definitions has missing "when t ∈ L"
  stuff.
  # DONE

- Strengthening for module definitions should produce module aliases
  rather than recursing into the submodules.  Similarly, strengthening
  for module types definitions should produce module type aliases.

  This what OCaml does now and it gives stronger types than the
  alternative.
  # DONE

- It is not obvious to me why strengthening has to ignore paths involved
  in `with C` constructs. Could you explain why that is necessary?

- I would expect the "substitution" operation on module definitions
  to have:

    module X : M; S [X.P.t=τ] → module X : M with P.t=τ; S

  avoiding the need to recurse into submodules.
  # DONE

- I find it odd to have typing judgement side-conditions on the
  "substitution" operation. I would expect there to be validity
  judgement preventing the substitution from being applied when these
  conditions didn't hold, rather than the operation itself checking
  these conditions and being undefined if they don't hold.
  # I agree, but the definition would be significantly longer, and repetitive.

- You are using the "substitution" operation to do actual substitution
  of functor parameters. I do not think this works. I don't think trying
  to conflate your "substitution" operation with actual substitution is
  a good idea.

  I'd probably rename your "substitution" to something else. Maybe
  "enrichment"?
  # You're right, I used your name and changed functors to use proper substitution. We will probably use explicit substitution here (ie., "let module X = (P :> S) in T").

- The first case in the definition of the transparent ascription
  operation looks suspicious to me. The resolve operation removes module
  aliases, but removing module aliases gives a weaker module type so
  doing it on the right-hand side of a module ascription seems
  wrong. For example, with the current rule I worry that the following
  module expression would be allowed:

    (struct type t = M.t let x = 5 end :> =M)

  where `M` is defined as:

    module M : sig type t val x : int end

  or that this module expression would have a type other than `=M`:

    (M :> =M)

  I suspect it would be better to differentiate the operation which
  expands module type paths from the operation that replaces module
  aliases with their strenghthening. The first one takes a module type
  path and gives you an equivalent module type, the second takes a
  module type path and gives you a weaker module type.
  # Indeed, but I don't think we need two operations: we just need to try to handle the case where the right hand side is a module alias first, and then resolve if necessary.

- Why the weird triangles instead of the turnstile on the judgements?
  Just curious.
  # Avoid overlaping with the core type language, which usually use turnstyle. You  could argue that "it's clear from the context" ... but I like my math not to be written in python. :)

- The rule for subtyping between signatures is missing a substitution
  it needs to be use something like D′ᵢ[Dπ(1), ..., Dπ(n) / D′₁, ..., D′ₙ]
  in the premise instead of D′ᵢ.

  The rules for the individual signature items also needs to strengthen
  the item on the left-hand side.

  The classic example that requires the above two changes is:

    sig type t; type s = t end < sig type s; type t = s end

- As with transparent ascription, the rule in module subtyping allowing
  both sides to be resolved looks suspicious to me.

- I think the definition of normalization is not sufficient. It needs to
  normalize the arguments of functor applications within the path as
  well as the path as a whole.

- I think it is slightly cleaner when types and module types are
  treated similarly to modules. i.e.

    D ::=
      ..
      type t : k

    k ::=
        = (τ : k)
      | (* stuff like datatype definitions *)

  although I appreciate that this doesn't really matter for what
  you are focused on with these rules.

- I note that you do not have abstract module types in the formalism.
  Obviously they make things quite a bit more complicated, so I can
  understand the omission. If you can manage to fit them in I think
  it might be the first module system formalism to do so properly.

  The key to keeping things working is probably to use a cumulative
  hierarchy of module type universes to avoid the usual `Type : Type`
  issues.

- One variation that is maybe worth considering is having

  P : M

  in the "substitutions" rather than P = P. It is more general, and in
  some sense more natural – the essence of the `with` construct is to
  replace part of a module type with some subtype of that part.

- It would also be great to allow:

    P.X = M

  in "substitutions" for introducing module type aliases.
