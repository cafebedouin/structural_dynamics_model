# OQ-285 gate step 2 — Claude-web adversarial check of this dir's WRITEUP.md

**Provenance:** pasted verbatim below from the operator's relay, 2026-08-19. Fresh Claude-web
conversation, NO repo access (per the gate's design: the outside read is the instrument). The
reviewer received the 2026-08-17 WRITEUP verbatim plus the check prompt recorded in the session
transcript; the GAP-number staleness correction was passed OUTSIDE the pasted document (a document
under adversarial review is not silently amended by the party requesting the review).
**Verdict: endorse with amendments (10, enumerated below).** Consumed by the OQ-285 disposition
(see ISSUES.md OQ-285 close) alongside the independent re-derivation at
`audits/2026-08-19_oq285_mode3_measurement_arm/`.

---

First, the good news: **every arithmetic claim I could re-derive holds.** Group split
152+29+0+1152=1333; rescue 25+4=29; the 2×2 margins (127/81/15/4/3=230, 60+170, unkSeats
40+103+9=152) and all five rates; §3's 170+58+2=230, 152/60=2.53, 1333/230=5.80, 1333−193=1140,
152−44=108, 44/193=22.8%, 108/1140=9.5%, 2.4×, 28.9%; §4's four percentages; and the cross-leg
total 19,414 (the document's correction of the plan's 21,414 is right). The `arithmetic_selfcheck`
claim survives an independent pass.

## 1. Do the counts support the kill-claims?

**§1 — supported on the default leg, over-extended across legs.** Group (i)=0 on `testsets/` is
engine-measured and does support "the FAILS side is empty here." The sentence **"it is empty
everywhere"** does not follow from the block beneath it. The cross-leg table measures *malformed
atoms against code vocabularies*, not derivation outcomes. The inference `0 malformed ⇒ 0 FAILS`
requires `FAILS ⇒ malformed`, which is never argued — and the document's own control paragraph
shows the mapping is not clean in the other direction either (bad-role-only correctly *isn't* a
FAILS). The control is honestly labelled a floor for sensitivity; it is silent on exhaustiveness
of failure modes. **Amendment: "empty everywhere" → "no malformed authored atoms anywhere; FAILS
directly measured as 0 on one leg only."**

Minor: 19,414 is a pre-filter count (testsets contributes 1355, not the 1333 used in §1). Doesn't
touch the zero; does mean the headline denominator isn't the same population as §1's.

Group (iv)'s "unreachable on this corpus" is correctly hedged and correctly reasoned — 0 seats
match the seven pairs, so the lever is inert regardless of setting.

**§2 — conclusion supported, headline falsified by its own table.** "The signature does not
**even correlate** with abstention" is contradicted by the block directly under it: 0.0% / 0.0% /
12.6% / 50.6% / 100.0% is a strong association. What the table actually shows is that signature is
*not determinative* and would misfire — 111 `constructed_high_extraction` constraints with nothing
to explain. That kills the arm as the OQ specifies it (a reason token), but by a different
argument. Since §6.1 makes this one of exactly two load-bearing measurements, the imprecision
matters. **Amendment: restate as non-determinacy/misfire rate, not non-correlation.**

Also: "`false_ci_rope` is a coin flip" is a constraint-level rate being used to retire a
seat-level instrument; the 81 constraints carry 103 unknown seats out of an unreported seat total.

§2.1 and §2.2 are correctly and unusually well self-limited. §2.3.1's insistence that §1's
sub-paths and the 2×2 are one observation is right — and has a consequence the document doesn't
carry forward (see §6.1 below).

**§3 — the counts support "not blind at the coarse cuts," but the framing inverts the power
argument.** "NON-UNIFORM at the strongest available margin" is backwards. Coarsening a coordinate
pools seats and mechanically drives always-unknown toward 0; the *strongest test of blindness* is
the finest cut, and at the finest cut **19 always-unknown cells do appear**. They are dismissed on
support (18×n=1, 1×n=2). That dismissal is reasonable, but it is the load-bearing move in §3 and
it is an absence-of-evidence at n=1, not evidence of absence. **Amendment: say plainly that the
only cut with power to show blindness is the one where the coarse-cut zero is not available, and
that the fine-cut candidates are dismissed on power.**

Related: the discrimination claim ("the test emits all three outcome classes") borrows the
always-unknown class from the *fine* cut to control the zero at the *coarse* cut. Whether
always-unknown is reachable at `(power, exit)` on this data is not shown.

**§4 — the count supports "8 four-way-matched seats," not "the maximal population for such a
design is 8."** Three problems, all in a load-bearing section:

- **Four-way matching is a choice, not a requirement.** A two- or three-leg contrast still varies
  author while holding position. No pairwise intersections are reported. The maximal population
  for *a* matched design is unmeasured and is ≥8.
- **The required match is stricter than the operative coordinate.** §2.4 says abstention is a
  function of constraint metrics × `D`, where `D` derives from `(role, exit)`; §3 tests
  `(power, exit)` and `D` alone. §4 requires an identical five-tuple
  `(role,power,time,exit,scope)`. The matched population *at §3's own coordinate* is somewhere
  between 8 and 73 and is never reported.
- **"The confound is structural… no sample size fixes it" is wrong for the 8.** For those 8 seats
  position *is* held fixed — that's what makes them the residual population. The 65 others are
  confounded; the 8 are underpowered. Conflating the two overstates the kill.

And the measurement the document had in hand and didn't take: **what do those 8 seats actually do
across the 4 legs?** If they split by authoring model, that is the expressive-capacity signal; if
they don't, it's a (weak) null. Reporting n and stopping is the one place §4 declines a free-ish
observation.

## 2. (D) vs (B) — real, but supported by the wrong evidence

The distinction is real in principle and there is a measurement that carries it: (B) says no arm
exists; (D) additionally says the named axis is empty on one side. That extra content lives
entirely in §1's group (i)=0 and in §7's "well-built surface on the wrong predicate."

But §6.1 classifies §1 and §7 as **corroboration, not load-bearing**, and then asks the outside
checker to re-derive only §2.3 and §4. **§2.3 and §4 surviving earns (B), not (D).** Neither says
anything about whether group (i) is empty. So "If both survive an outside re-derivation, (D) is
earned" is false as written, and the concrete ask in §6.1 is mis-scoped: it omits the only
measurement that distinguishes the verdict form it chose.

Secondary: the other half of the verdict sentence — `unknown` is "registered-by-design" — has
**no pasted evidence anywhere in the document.** It rests on `RECON.md` item 2 and
`reading_registry.pl:142`. That is half the (D) claim carried by an uncited pointer.

So: not cosmetic, but presently unearned in the document's own accounting.

## 3. The gap text

**"The absent thing is the instrument, not the token" does not follow, and §2.4 is the
counterexample.** §2.4 states that the determining fact — why `classify_from_metrics/6` fell
through for this seat's `(metrics, D)` pair — is *not serialized anywhere*. That is a missing
token, it is determining rather than co-occurring, and it reports a distinction the repository
demonstrably *can* measure (§3 measures those coordinates). §8's rebuttal ("a token would report
a distinction nothing here can measure") applies to a token joined on *signature*, which is what
§2.3 killed. It does not apply to a token serializing the cascade exit — which §9 itself lists as
owed work. The clause should be "the absent instrument is not substitutable by a signature-joined
token," which is what the evidence shows.

Three further over-licenses in the drafted text:

- **"Every live seat-level abstention is explicable by the seat's `(metrics, D)` coordinate"** is
  true by construction — those are the predicate's inputs. What was measured is the negative: not
  explicable by `D` alone or `(power, exit)` alone. As drafted it is unfalsifiable and licenses
  nothing.
- **"unmeasurable as built"** directly contradicts §5, which carefully lands on "not refused on
  principle; **unsupported on evidence** … a weaker and more honest statement than 'inadmissible',
  and it is the one the data licenses." GAP-36 then reinstates the strong form. One of the two has
  to go.
- **"remains so until a matched-seat generation spend exists"** forecloses cheaper routes the
  document names elsewhere: pairwise leg matching (§4, unmeasured), running the engine on the 8
  matched seats (§4/§9), and instrumenting `classify_from_metrics/6`'s exit point (§9, "owed").
  A gap entry that writes "generation spend or nothing" into the record will be read as
  authorization to stop looking.

Also, "no instrument separates…" quantifies over all instruments on evidence from four (three arms
plus §7's surface).

## 4. §6's reversal conditions

Mixed, and the second one is the costume.

- **Condition 1 (matched-vocabulary instrument):** not fireable by any reader. It requires new
  generation, and the same paragraph records that GAP-35 makes the matched-seed decision
  "unreversible after the fact." A falsifier whose production is foreclosed by a prior recorded
  decision is a disclaimer with a citation.
- **Condition 2 (a coordinate typing under no metrics):** stated, measured absent, **and then
  pre-emptively nullified** — "even had one turned up, it would be a missing rule-table row, not
  blindness." If the observation would not move the verdict when it occurs, it is not a reversal
  condition. This one should be deleted or the nullifier should be dropped. As written it is the
  clearest instance of hedging in falsifier's costume in the document.
- **"at any usable support"** has no stated threshold. n=2 is dismissed without a pre-committed
  cutoff, which makes the goalpost movable after the fact. Whether `PREREGISTRATION.md` fixed a
  support threshold is repo-dependent.
- **§6.1 is the strongest part of the document** and I'd keep it nearly intact. Two corrections:
  the "roughly six instruments / ~50% error rate" has an uncounted denominator (the evidence map
  lists ~17 artifacts), so the estimate it leans on is itself unsourced; and the error record has
  a **direction** the paragraph doesn't name — the §4 regex false-absence would have produced the
  *same verdict* through a broken instrument, as the document admits. Errors that are
  verdict-confirming are worse news for a survived-verdict argument than errors that are random.

One more: §6.1 asks for independent re-derivation, but the header block reports `code_dirty: true`
at `c06bcb2`, with `pipeline_run_at` 2026-08-15 and execution on 2026-08-17. Read-only probes
still execute against a dirty working tree. **The document's own reversal path may not be
executable by anyone else,** because the substrate it measured does not correspond to a committed
state. That belongs in §6, not buried in the header.

## 5. Verdict: endorse with amendments

The three arms are dead, the arithmetic is clean, and the self-criticism in §6.1 is better than
most documents of this kind manage. But the verdict *form*, the gap text, and one of two
load-bearing sections each claim more than the pasted evidence carries.

Amendments, named:

1. **§1** — "empty everywhere" → "0 malformed atoms across five legs; FAILS engine-measured as 0
   on one leg." Drop the inference that malformation exhausts failure modes, or argue it.
2. **§2.3** — replace "does not even correlate" with the non-determinacy claim (misfires on 111
   constraints; no reasonless sub-population). The table shows association.
3. **§3** — state that the coarse-cut zero is partly a pooling artifact, and that the fine-cut
   always-unknown cells are dismissed on power, not on measurement.
4. **§4** — retract "the maximal population for such a design is 8." Report pairwise/three-way
   intersections and the matched population at `(power, exit)` and at `D` before claiming a
   ceiling.
5. **§4** — report the abstention outcomes of the 8 matched seats across the 4 legs, or state
   explicitly that doing so requires an engine run on the twin legs (Phase 2) and that the arm is
   therefore *unmeasured*, not dead.
6. **§4** — separate confounding from power: the 8 are unconfounded and underpowered; "no sample
   size fixes it" applies only to the other 65.
7. **§6.1 / §8** — fix the re-derivation ask. (D) requires §1; §2.3+§4 alone earn (B). Either add
   §1 to the narrow ask or downgrade to (B) pending it.
8. **§8 verdict sentence** — the "registered-by-design" half needs pasted evidence, not a pointer.
9. **GAP-36** — drop or qualify "the absent thing is the instrument, not the token" (§2.4
   contradicts it); delete "Every live seat-level abstention is explicable by `(metrics, D)`"
   (true by construction); change "unmeasurable as built" to §5's "unsupported on evidence";
   strike "until a matched-seat generation spend exists" or enumerate the cheaper routes §9
   already names.
10. **§6** — delete reversal condition 2 or its nullifier; state the support threshold; add the
    `code_dirty` reproducibility hazard as a live obstacle to the re-derivation being requested.

## Claims I'd need the repo for

Not assumed either way: that any pasted block matches its named file; the "26 constraints"
carrying an `unknown` signature (never pasted, and in apparent tension with §1's "only five
signatures occur anywhere" — either `unknown` isn't a signature, or that sentence is false); the
193 immutability-hole seats; `PREREGISTRATION.md`'s contents, freeze, and whether it fixed a
support threshold; `reading_registry.pl:142` and `RECON.md` item 2; GAP-35's "unreversible"
property; whether §4's static parser is the same implementation as §1's cross-leg parser (if not,
§1's "two independent implementations" corroboration does not extend to §4, which is where the
one known parser bug occurred); every `file.pl:NNN` pin — §9 reports pin drift in this repo,
which makes all such citations low-trust by the document's own finding; and the clerical claims
(next free GAP-36, OQ-296, commit `2caaf77b`).
