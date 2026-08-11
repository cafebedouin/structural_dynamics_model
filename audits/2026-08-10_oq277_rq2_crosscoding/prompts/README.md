# Coder prompts — the only artifact in this design with NO witness

**Status: AUTHORED, awaiting operator review at freeze (conditions C4 / C5).**

Every other artifact in this audit is checkable against something: units against their source
directories, anchors against a frozen label source, the lexicon against its selftest, the packets
against their gates. **A prompt has no such referent.** It is the one place where a wording choice
silently becomes a measurement property, and it cannot be validated by running it — running it is
the spend. So it ships as *reviewable content in the preregistration*, and the review is the
control.

The specific hazard, stated so the review knows what to look for: **`other`'s reachability is a
property of the wording, not of the taxonomy.** The both-residue row and the escape check both
depend on `other` being genuinely available to the coder. The decoys test whether the coder *can*
return it; the prompt determines whether it *will*. A prompt that lists `other` as a trailing "if
none of the above apply" makes it a residual, and a residual is under-selected in ways no
downstream check can distinguish from real coverage.

## Three legs, TWO prompt files — and the missing third file is deliberate

| leg | prompt file | codes | against | sweep direction |
|---|---|---|---|---|
| direction (i) | `direction_i.md` | Wu's incidents | our six: `P1`–`P6` + `other` | `i` |
| direction (ii) | `direction_ii.md` | our incidents | Wu's five: `A`–`E` + `other` | `ii` |
| (iii′) | **`direction_i.md`** — the same file | our own published exemplars | our six: `P1`–`P6` + `other` | `ii` |

**There is no `iii_prime.md`, and that is the point.** (iii′) puts the identical task to the coder
as direction (i) — read one incident, assign one of our six — with the identical label space and the
identical definitions. A separate `iii_prime.md` would be a byte-copy of `direction_i.md` with no
queryable fact saying which is canonical, and the two would drift the first time a definition was
reworded. That is P2, inside the experiment that measures P2. **The driver reads one file for both
legs and asserts it is the same file**, so the sharing is a checked fact rather than a convention.

What differs between the two legs is not the prompt but the **packet** and the **sweep direction**:
(iii′)'s units are ours, so they sweep under direction `ii`, while its answers are in direction (i)'s
index. That crossing is the easiest thing in the design to get backwards and is asserted in the
driver rather than left to care.

**One consequence to carry into the prereg:** because both legs share a prompt, a wording defect in
`direction_i.md` is common-mode across them. The (iii′) row cannot be used as an independent check
on direction (i)'s prompt quality, and no reading may treat agreement between the two legs as
corroboration — they share the instrument.

### The label tokens and the index collision

The answer tokens `P1`–`P6` are the published indices, and two of them are known to be ambiguous in
our own records (an index collision between two documents, tracked separately). That ambiguity does
**not** reach the coder: the prompt defines each index explicitly by its rule text, and the coder's
`P3` means exactly what the prompt's `P3` paragraph says. Scoring must therefore compare the coder's
answer against the label as the PROMPT defines it, never against a historical reference to "Pattern
3" or "Pattern 4" elsewhere in the repository — those are ambiguous until dated against the
collision.

## Constraints, all from C5 and the frozen design — none inferred

1. **Taxonomy definitions verbatim.** The published six from `CLAUDE.md`'s Build Discipline block;
   Wu's A–E from the md5-pinned `packets/wu_source/failure_modes_catalog.md`. Wu's are carried in
   the source's own Chinese with a faithful English rendering alongside — the same convention the
   redacted units were extracted under, because the coder reads English and a translation-only
   presentation would silently substitute the translator's reading for the source's.

2. **EXEMPLARS ARE STRIPPED from every definition.** This is the one deviation from "verbatim", and
   it is required twice over:
   - `CLAUDE.md`'s pattern definitions name their exemplars inline, and **those exemplars ARE the
     (iii′) units.** Shipping them would hand the coder the answer key for that entire row.
   - The same exemplars are dense in source-identifying vocabulary (predicate names, OQ ids, file
     paths), which is banned in both directions. In `direction_i.md` they would be OUR source
     leaking into a payload about WU's incidents.
   The rule statements are carried whole; only the parenthetical instances are removed. Each
   definition below is traceable to its line in the pinned source, and the pin lives in the
   preregistration — not here.

3. **File + commit hash pin lives in the PREREGISTRATION, not in the payload.** Putting
   `CLAUDE.md @ <hash>` inside a payload identifies our source to the coder, which is a leak
   *through the weights* that a payload grep cannot catch — the coder could recall the published
   taxonomy rather than reason from the definition. The payload carries definitions with provenance
   stripped; the prereg carries the pin so a reader can verify they were the published ones.

4. **Single user turn.** A stateless SDK call. No system-prompt taxonomy, no conversation, no
   context from prior items. k=3 redraws are three independent calls, not three turns.

5. **One label from a fixed vocabulary and NOTHING ELSE** — no confidence score, no rationale, no
   hedging field. k=3 unanimity IS the churn instrument; a second uncontrolled signal would get
   read by an adjudicator, and a confidence number in particular would become an unpreregistered
   weight on a row that has no pre-registered rule for using one.

6. **`other` is first-class, never a residual.** It appears in the same list shape as the
   lettered/numbered classes and carries a positive definition — *the incident's mechanism is not
   one of the above* — rather than a trailing escape clause.

7. **Source-identifying terms banned in both directions.** No `Wu`, `openclaw`, `arxiv`,
   `model-bridge`; no `OQ-nnn`, `ISSUES.md`, `KNOWN_STATE`, `CLAUDE.md`, `build_discipline`,
   `deferential realism`. Neither taxonomy is ever named as belonging to anyone.

## The sweep exemption these prompts require, and why it is not a loophole

A prompt necessarily contains its own direction's class definitions, so **a payload cannot be swept
clean under its own direction's full lexicon** — `direction_ii.md` contains Wu's class names, and
`iii_prime.md` contains ours. The rule the driver enforces instead:

> Sweep the **unit portion** of every payload under its own direction's full lexicon; sweep the
> **whole payload**, prompt included, under the *other* direction's `source_identifying` group only.

That is strictly stronger than sweeping unit text alone and loosens nothing: the definitions block
is fixed, reviewed, and identical across every payload in a direction, so it is auditable once
here rather than re-cleared 219 times — while the part that varies per call is swept in full.
