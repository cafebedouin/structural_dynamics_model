# CS-Layer Shakeout — Proposals & Open Questions

**Status: open / in flux. This is the staging area, not the record.**
The settled analysis it rests on is in `cs_shakeout_record.md`. Items here are
proposed framework changes and open questions at varying confidence. The boundary
in this document is **settled-vs-open**, not findings-vs-proposals: an item can be
ready to encode and still be open, because it does not become settled until it
lands in `commitment_systems_sketch_v4.md` as a deliberate marked revision.

Four claims came out of the analysis. They do **not** stand or fall together.
They are listed in descending confidence, and the disposition of each is explicit.

---

## Claim 1 — interpretive_accretion has a missing sub-species. READY TO ENCODE.

**Confidence: high. Based on a full-story case. Ready to act on, not yet settled.**

The taxonomy's single `interpretive_accretion` clause requires kernel atom
`fixed_text`. `privilege_waiver_threshold` (common-law attorney-client privilege)
is interpretive accretion in its purest form, yet declares `formalized` — and
`formalized` is the *more correct* atom, because common law has no fixed text.

This reveals two genuine sub-species of interpretive accretion, distinguished by
how the kernel is held:

- **Scripture-elaborated-by-doctrine** — a *fixed text* (constitution, scripture,
  founding statute) whose meaning develops through an interpretive lineage.
  Atom: `(fixed_text, lineage)`.
- **Principle-elaborated-by-precedent** — a *formalized principle* with formal
  apparatus (tests, elements, holdings) but **no canonical text**, developed
  through precedential lineage. The common-law case. Atom: `(formalized, lineage)`.

Both are interpretive accretion. The taxonomy encoded only the first.

**Proposed encoding** (stages here until merged into the sketch as marked v5):
add `(formalized, lineage) → interpretive_accretion` alongside the existing
`(fixed_text, lineage)` clause. Document the two sub-species in the sketch.

**Secondary correction surfaced by the same case:** the schema restricts
`interpretation_layer_present` to `kernel=formalized AND authority=extraction`.
But interpretive accretion *is* the paradigm case of a functioning interpretive
layer. The generator setting `interp_layer=true` here is correct; the schema's
restriction is too narrow and should be widened to permit lineage-grounded
authority. (This is the inverse of the original plan's "D4," which had it pointed
the wrong way.)

---

## Claim 2 — `(distributed, extraction)` is a real commitment structure. TAXONOMIC TREATMENT OPEN.

**Confidence: high as a single-constraint description (`drift_denial`).
The taxonomic treatment is an open fork and should be decided on more than one
constraint.**

`drift_denial_authority_structure` is correctly described by `(distributed,
extraction)`: a once-coherent kernel (superhuman control) now operationally
abandoned, with distributed authority denying the drift and extracting from
claimed continuity. It is coherent, well-described, and has declared victims.

The open question is **how the taxonomy should handle it**, not whether the
description is right. Three options, left unresolved:

- **(a) New named pattern** — `diffuse_capture` as a first-class sixth pattern,
  the captured pole of the diffuse family, parallel to how
  `anchored_fixity_brittle` is the failure mode of `anchored_fixity_with_accretion`.
  The `kernel_coherence_threshold` omega would carry the "was the kernel ever
  coherent" sub-question (weaponized-from-the-start vs. coherent-then-hollowed).
  *Argument for:* `authority_grounding=extraction` is a declared structural field,
  not a drift signal.
  *Argument against:* applied liberally, this principle turns every novel
  atom-pair into a new pattern. One constraint should not auto-promote to a sixth
  pattern.
- **(b) Verdict-layer overlay (LIVE — do not discard).** Route `(distributed,
  extraction)` to `diffuse_reconstruction`-claimed and let `false_diffuse_reconstruction`
  fire. Its existing `suppression ≥ 0.60` trigger **would catch `drift_denial`
  today** (suppression 0.68). Keeps the taxonomy at five patterns; consistent with
  how the base layer treats `false_ci_rope` as a contamination overlay rather than
  a new type.
- **(c) One diffuse family, two poles.** `authority_grounding` splits benign
  (`distributed` → reconstruction) from captured (`extraction` → capture), with the
  temporal sub-question in the `kernel_coherence_threshold` omega.

**Disposition:** unresolved, deliberately. This is a framework call for Scott to
make on >1 constraint. The verdict-layer route (b) is a working option, not a
fallback — it is documented here as live precisely because an earlier pass
under-weighted it after finding (c) elegant. Elegance is not evidence.

---

## Claim 3 — `drift_denial` and `distributed_extraction_stakes` may be the same system. OPEN. CHECK, DON'T BUILD.

**Confidence: medium at best. A hypothesis to check, not a finding to build on.**

The two share a domain string (`commitment_systems`), the same kernel referent
(alignment foundational texts), overlapping-to-identical actor sets, complementary
victim structure (none vs. public/deployers/regulators), staggered intervals
(0-6 vs. 0-9), and explicitly time-placed narratives (pre-paradigmatic vs.
"by 2023-2024 diverged"). They are **not** linked by `affects_constraint` (checked).

Caveat carried explicitly: the absence of a link is doing real work in the
"same system" argument, and **absence is weak evidence** — consistent with both
"same system, generator failed to link" and "different systems, correctly
unlinked." This is interesting if true and a ready-made worked example of a
benign→captured drift trajectory if true, but it is a hypothesis.

**Disposition:** checkable, cheaply. Read both referents, decide whether they are
the same system. Until that resolves, do not let it support any other claim.

---

## Claim 4 — therefore CS pattern might need indexing like DR type. PARKED HARD. DO NOT TOUCH CODE OR SCALING.

**Confidence: low. Gated behind two independent things both resolving against the
safe reading.**

The reasoning was: DR type is indexed by (P,T,E,S); if one system carries two CS
patterns, maybe CS pattern is frame-relative too and should be indexed rather than
asserted as a single static value.

This is parked harder than claim 3, for a specific reason: **it is contradicted by
its own default reading.** Even if claim 3 is confirmed (same system), the
*time-sequential* explanation — one system drifting, snapshotted at two stages —
fully accounts for the two patterns **without** requiring CS pattern to be
frame-relative. The time-sequential reading was offered as the *safe* reading in
the same breath that claim 4 was raised. So claim 4 requires claim 3 confirmed
**AND** the time-sequential explanation to fail. Both, independently.

**Disposition:** must not influence any code change or any scaling decision now.
Revisit only if (claim 3 resolves to same-system) AND (the two patterns turn out
not to be explainable as time-snapshots of one drifting system).

---

## Ship-regardless — independent of the fork, can go to Claude Code in parallel

These do not wait on any conceptual call above. They are honesty/robustness
improvements and can be a small mechanical PR while Claim 2's fork stays open.

**Coupling — must land together: Fix 2 and Fix 3.** Fix 3's prose branches read
the signal atoms that Fix 2 emits. If Fix 2 ships without Fix 3, the renderer
keeps matching the old `anomalous_field_combination` prose against signal atoms it
was never taught, and falls through to its default branch — a *silent* mismatch
(no error, just stale prose against new atoms). This briefly recreates the exact
"tool gap presented as a property of the data" bug that Fix 3 exists to remove.
Same coupling class as the linter/classifier shared-source point and the
cross-module reads in the profile-indexing fix: a signature change on the
producer side requires the consumer migrated in the same pass. Put one line in the
PR scope: *"Fix 2 and Fix 3 are coupled — the renderer reads the signals the
classifier emits; ship together or the renderer falls through on the new atoms."*

- **Rejection-signal differentiation** (`cs_pattern_detection.pl`): emit distinct
  signal atoms for `kernel_none`/`authority_none` vs. pair-outside-taxonomy vs.
  pair-recognized-but-unhandled, instead of collapsing all to
  `anomalous_field_combination`. Remove the unreachable `cs_pattern/3` fallback.
- **Renderer prose branching** (`enhanced_report.py`, no_pattern_match branch):
  stop attributing a tool coverage-gap to the data ("field combination
  unrecognized"); branch the prose so "no CS fields," "pair outside taxonomy
  (flagged for review)," and "pair should be handled (possible rule gap)" read
  differently.
- **`capability_velocity` generation guardrail:** when a constraint's CS-framing
  omega is unresolved, block assertion of a definite `(kernel, authority)` pair —
  omit the `cs_structure` block instead. This caught the one true error in the run
  and depends on none of the speculative chain. **Highest-priority upstream fix.**

Note: encoding Claim 1 (the `(formalized, lineage)` clause) is *also* mechanical,
but it is held above as staged-not-settled because it is a taxonomy change that
should land deliberately in the sketch first, not slip in as a "fix."

---

## Standing note on the meta-pattern

The failure shape this document is structured to contain — *small sample, large
structural conclusion, confidence rising as the inference chain lengthens* —
surfaced three times in adjacent work (the first CS plan's infrastructure
response to a judgment problem; the claim 3→4 promotion above; an earlier
dormancy misread). It is not a property of any one instance; it is what happens
when plausible structure is available and the cases are few.

The reliable guard is not vigilance but process: the two-document split, the
worksheet-before-code discipline, the "no findings in the plan" rule. Encoded as
a heuristic worth keeping:

> Confidence should fall as an inference chain lengthens, unless each new link
> adds independent evidence. When confidence rises instead, that is the signal to
> stop and check the sample size.
