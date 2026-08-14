# A2 draft — proposed replacement for `amnesiac_institution_v0_6.md` §2

**REVISION 2.** Still NOT APPLIED to `docs/amnesiac_institution/`. The upstream half *has* landed
(see below), because it had to precede the citation.

## What changed since revision 1

| # | your correction | disposition |
|---|---|---|
| 1 | the canonical short form is v0.6's own sentence, cited as concealment's | **fixed upstream** — added to `CWC` §3.5 as **C1**, digested `C1@d0922f3d`; the notice now quotes it *from* upstream with a pin, and acyclicity holds |
| 2 | strike the glosses (unpinned paraphrase beside a pinned row) | struck, all four |
| 3 | 2.B and §13 both state the contribution boundary | §13 is canonical; 2.B keeps the prior-art disclosure and points at §13 in a clause |
| 4 | the opt-in hazard scales at A3, not Pass B | `claim_cite_check` moves to **between A2 and A3** |
| 5 | 2.D's "the derivation above" is a dangling referent | re-pointed to the cited claims by label |
| 6 | `A1@…–A4@…` is a range with unpinned interior | all four enumerated |

**Upstream change, landed:** `CWC` §3.5 gains a four-line corollary block stating C1 and naming
what it follows from; Appendix A's ANALYTIC table gains a `C1` row. **All 15 pre-existing digests
verified unmoved** (witness below), so this is an addition, not an edit to a pinned row.

## The digest recipe is now a script, because prose was not enough

`claim_digest.sh` is the definition; nothing may reimplement it. Revision 1 stated the recipe in
prose and I implemented it twice from that prose in one turn — once piping `grep` output with its
trailing newline, once via `printf '%s'` without — and the two disagreed on every digest. Same
recipe, two readings, no error anywhere. The pinned form is **no trailing newline**, which is what
revision 1's numbers happened to use, so those were right and the *comparison harness* was wrong.

    A1 e858a33d   A2 31548228   A3 fe7890db   A4 62d54b18   A5 519a4c85   C1 d0922f3d
    E1 884ea0b6   E2 3524a541   E3 afcb38cb   E4 ad4ef383   E5 775f6ce0
    P1 f778dd4d   P2 539e1206   P3 f9ca4f26   P4 33ae71f9   P5 ca92fd8c

`./claim_digest.sh --selftest` — 7 controls, green: an absent label exits 4 rather than returning
`e3b0c442` (sha256 of nothing, which is how the first harness reported a brand-new row as "MOVED");
a trailing newline is shown to change the digest, so the normalisation is load-bearing rather than
decorative; a content change under an unchanged label moves the digest, which is the A4-narrowing
case the scheme exists for; a duplicated label is refused rather than silently taking the first.

---

## The draft

### Heading

```
## 2. The derivation this paper uses, and the two controls on its use
```

### The vacation notice (replaces §2.1–§2.7)

> **§2.1–§2.7 are vacated.** The derivation that stood here is carried, in one place, by
> `docs/concealment/concealment_without_a_concealer_v0_4.md`. This paper uses its conclusion and
> does not restate its argument. That conclusion, stated there as `CWC:C1@d0922f3d`:
>
> > **Absence/presence collapse.** An absence presents as a presence when an abstraction is read at
> > a framing other than the one it was formed at.
>
> It follows from `CWC:A1@e858a33d`, `CWC:A2@31548228`, `CWC:A3@fe7890db` and `CWC:A4@62d54b18`,
> with the operational asymmetry at `CWC:E1@884ea0b6`. Numbers 2.1–2.7 are not reused: *a visible
> gap is a checked fact; a silent renumber is a fork* (§5.2).
>
> *Why the vacation: two live documents carried this derivation and each named it as its own
> contribution — Pattern 2 on the substrate of the paper that documents Pattern 2 (ISSUES OQ-287).*

### 2.A What the derivation looks like here

> These are instances of `CWC:A2@31548228` observed in this repository. They are what the vacated
> subsections held that the cited paper does not, and they are cited *as* instances — the general
> form is `CWC`'s, the instances are this paper's, and neither is the other's evidence.
>
> Two that look like two kinds of mistake and are one:
>
> - A count of "open questions" correct at the `open`-status framing, correct at the
>   workable-frontier framing, correct at the not-resolved framing, and wrong as an uncited
>   composite (§1.1).
> - An incidence figure of 42% accurate over the substrate stratum it was computed on and silent
>   about the instrument stratum nobody has measured (§5.4).
>
> The paper's own headline is the second of these. `CWC:A2@31548228` and `CWC` §5.4 state the
> general form; **42% is the instance this paper is answerable for**, accurate at the framing it
> was formed at and silent about the instrument stratum until someone asked what the denominator
> ranged over.
>
> The same shape in three phrasings this repository actually produces: *"tests pass"* without a
> date is a compression over an expired interval; *"no occurrences found"* without a positive
> control is a compression over a search whose reach is unknown; *"all addressed"* over a review
> list is a compression whose selection rule is the reviewer's attention.

### 2.B Prior art, stated so this paper does not claim it

> `deferential_realism_paper_v8.md` §8 already states the rule — *claims carry their scope*: "a
> witness earned on one corpus, one regime, one seat-count licenses a claim about that corpus, that
> regime, that seat-count" — and already identifies the classifier's honesty about absence and the
> build's honesty about absence as the same invariant. The **derivation** of that rule is
> `CWC:A1@e858a33d`–`CWC:A4@62d54b18`, cited above. This paper claims neither; **what it does claim
> is accounted at §13**, which is where the novelty accounting lives and is the only place it is
> stated.

### 2.C The consequence this paper proposes (P1)

> If `CWC:C1@d0922f3d` holds, the five patterns of §5 are one mechanism at five layers rather than
> five discoveries — which is what the practice found when it tried to fix them layer by layer and
> failed. This is stated as **P1** (§0) rather than as a finding, and it is this paper's own claim
> rather than `CWC`'s: it is a proposition about *this repository's defect record*, which `CWC`
> does not hold. §2.9 runs the control it owes.

### 2.D What is new about the AI era

> *(Former §2.7, preserved entire; only the opening clause is re-pointed. No counterpart in `CWC`
> — checked: the era argument, its three citations, and its conclusion appear nowhere in that
> paper.)*
>
> **Opening clause changes** from *"None of §2.1–§2.6 requires a language model"* to:
>
> > Neither `CWC:C1@d0922f3d` nor the claims it follows from — `CWC:A1@e858a33d`,
> > `CWC:A2@31548228`, `CWC:A3@fe7890db`, `CWC:A4@62d54b18` — requires a language model. Human
> > institutions have run on framed compressions forever, and their standard remedies — personnel
> > continuity, apprenticeship, the person who *was there* — are exactly what the configuration in
> > §1.1 removes. Four things change, in the same direction.
>
> **Body unchanged**: the compression ratio collapses (RULER, NoLiMa); the compressor became
> fluent and silent (arXiv:2607.13071); the parties multiplied and the handoffs became constant;
> the workforce is constitutionally better at recognition than at enumeration. Conclusion
> unchanged: *in an institution of this kind the scarce resource is not storage, compute, or
> verification depth. It is stated scope.*
>
> **Legibility:** each of the four sub-arguments carries its own evidence, and after the re-point
> nothing in the section refers backward by section number. The fourth leans on §7.4, which is
> downstream and survives. It loses its *setup* — it used to arrive as "here is why the derivation
> bites harder now" and now arrives as a standalone argument about the era. Rhetorical position,
> not warrant.

### 2.8 and 2.9 — unchanged text, at their existing numbers

> Both keep their numbers and their bodies verbatim (§2.8 is cited 7×, §2.9 10× internally and 3×
> externally, one of which is in correspondence already sent). Each gains a marker only:
>
> > **[DECLARED TEMPORARY — A2-pre ruling, 2026-08-13.]** This subsection's canonical destination
> > is the practice paper (§2.8 → III, §2.9 → V). It stays here, at this number, until that paper
> > lands and a redirect table maps `§2.9(a,b,…)` to its anchors at sub-item granularity. Until
> > then this is the canonical copy; after it lands, this becomes the superseded side and gains a
> > forward pointer. ISSUES OQ-287, second limb.
>
> **One deletion inside §2.8**, per your Q3 ruling: the gray-failure paragraph (Huang et al. 2017,
> Wu's *fail-plausible*, *"differential framing is the general one"*) is duplicated at `CWC` §12
> and cites-and-drops. Replacement, one sentence, in place:
>
> > The nearest external relative — gray failure's differential observability, and Wu's
> > *fail-plausible* escalation of it — is `CWC` §12; a seam is where two framings meet without
> > either being stated.
>
> §2.9's three exclusions and its break stay in full. They are duplicated *in general form* at
> `CWC` §10 and `CWC:P3@f9ca4f26`, but what stands here is the **instantiation** — the churn floor
> at §7.5, the demoted pattern at §5.2/§7.7, the instrument stratum at §7.4 — plus the independence
> claim that the operator's ruling preceded the account, which `CWC` does not make and could not.
> General claim → concealment; repository instantiation → v0.6. Each of the three gains its `CWC`
> §10 pointer and loses nothing.

---

## Sequencing consequence of correction 4

`claim_cite_check` now lands **between A2 and A3**, not in Pass B. Counted: this draft writes 13
pinned citation sites. A3 re-points 33 references, and the 11 aimed at former §2.1 and §2.3 have no
preserved home in 2.A–2.D, so they become new cross-document pinned citations — roughly 24+
unguarded sites written by a step whose positive half is a count. The plan's rationale for deferring
the checker was right; it measured the hazard at the wrong step.

## What this draft does NOT do

- Does not touch §13 (A4's step, and now the canonical home of the contribution boundary) or
  Appendix D.1 (A3 re-points its `(§2)` only).
- Does not re-point the 33 references or the 27 bare `§2` refs — A3, which now has four anchors
  (`2.A`–`2.D`) plus the notice.
- Does not settle the canonicity markers (A5) or the `CWC` Preface arity fix that folds into it.
  Note the Preface now reads *"Analytic claims (A1–A4)"* against an Appendix A running A1–A5 **and
  C1**, so A5's fix has one more label to account for than it did this morning.
