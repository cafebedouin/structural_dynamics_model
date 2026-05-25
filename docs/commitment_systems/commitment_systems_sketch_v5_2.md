# Commitment Systems and the Architecture of Drift — v5.2

*Revision of v5.1. This version makes one marked change: it acknowledges two pattern atoms
present in the Prolog implementation since at least v4 operationalization
(`natural_law_constraint` and `epistemic_consensus`) that were never enumerated or
theorized in any prior sketch version. Their presence in the code without specification
is an instance of the drift the framework is designed to detect. This version records
the acknowledged-drift event and provides the structural characterization that was missing.*

*All other sections of v5.1 are carried forward unchanged and remain authoritative.*

---

## 1. What changed from v5.1

One marked revision, to §3 (the pattern taxonomy).

The implementation has always emitted nine pattern atoms from `cs_pattern/3`; the sketches
enumerated seven (the five core patterns, `no_pattern_match`, and `anchored_fixity_brittle`
as a sixth distinct entry). The two uncounted atoms — `natural_law_constraint` and
`epistemic_consensus` — are structurally coherent, match real constraint structures in the
adversarial corpus, and have been silently handled by the Prolog classifier since the initial
`cs_pattern_detection.pl` was written. Their absence from the spec is a Type A drift instance:
the implementation frame drifted from the specification frame without marking the transition.

A code-audit on 2026-05-25 surfaced this as a spec-vs-implementation discrepancy. This version
resolves it by promoting both atoms from provisional to acknowledged, with structural
characterization below.

---

## 2. The two unspecced patterns, now acknowledged

### natural_law_constraint

**Trigger:** `cs_authority_grounding = self_enforcing` (any `cs_kernel_codification`).

**Structural meaning:** Authority is grounded in the constraint's own self-enforcement — there
is no external adjudicator because the constraint operates whether or not any institution
ratifies it. This maps to the framework's observation that some constraints are not governed by
an authority structure at all: they simply hold. The kernel is whatever the constraint is; the
"authority" is the constraint itself.

**Relation to the five core patterns:** `natural_law_constraint` is not a sixth attractor in
the same sense as the five core patterns — it is a structural precondition that rules out the
commitment-system architecture entirely for the constraint in question. Commitment systems
presuppose that authority and kernel are separable (authority interprets the kernel; drift is
possible because the two can diverge). A self-enforcing constraint has no such gap. It cannot
drift in the commitment-system sense, because there is no authority to acknowledge or deny
drift and no interpretive layer through which drift would travel.

**Failure mode:** `false_natural_law_constraint` fires when `self_enforcing` authority is
asserted but the constraint has identifiable beneficiaries. Genuine self-enforcing constraints
(gravity, Gödel incompleteness, Arrow's theorem) have no asymmetric winners. Beneficiary
presence indicates a constructed constraint disguised as natural necessity — the standard
false-summit pattern in the CS layer.

**Scope:** Cross-domain. Mathematical limits, physical laws, and genuine Nash equilibria with no
alternative-suppression mechanism are the canonical cases. Political actors frequently claim
`self_enforcing` status for constructed extraction mechanisms (the "natural order of markets,"
"inevitability of hierarchy") — these trigger `false_natural_law_constraint`.

### epistemic_consensus

**Trigger:** `cs_authority_grounding = diffuse_epistemic` (any `cs_kernel_codification`).

**Structural meaning:** Authority is grounded in distributed community consensus about what
the evidence requires — no single adjudicator, but also no claim to practice-based implicit
authority. The kernel is a factual or methodological claim whose authority derives from
collective epistemic standing: peer review, scientific consensus, methodological standards.

**Relation to the five core patterns:** `epistemic_consensus` is adjacent to `diffuse_reconstruction`
but differs in the authority's grounding: `diffuse_reconstruction` has distributed authority
over an under-specified kernel with no adjudicator and no epistemic basis for the distribution.
`epistemic_consensus` has distributed authority over a kernel whose authority rests on
evidence and methodology — the distribution is not chaos but coordinated epistemic practice.

**Failure mode:** No verdict predicate yet. The structural failure modes are: (a) consensus
becoming captured by institutional incentives so that the distribution masks extraction (the
authority becomes `extraction` while claiming `diffuse_epistemic`), and (b) the epistemic
community's acknowledgment capacity becoming insufficient for the environmental rate of new
evidence — the mathematics case study in v4 §7 is a worked example of this second failure.

**Scope:** Scientific paradigms, methodological standards, statistical norms, engineering
best-practices bodies. The commitment systems of academic peer review and citation metrics are
`epistemic_consensus` configurations that exhibit the first failure mode (captured consensus).

---

## 3. The acknowledged-drift event

The framework's preferred acknowledgment mode applied to itself:

**Kernel:** The formal specification of the commitment systems pattern taxonomy, as encoded in
the sketch documents.

**Authority:** The sketch revision process (marked-revision pattern: propose → check → absorb
into sketch, logged as a dated version change).

**Drift:** `natural_law_constraint` and `epistemic_consensus` were added to `cs_pattern_detection.pl`
without a corresponding update to the sketch. The gap between implementation and specification
accumulated silently.

**Acknowledgment:** This version. The atoms are promoted from unspecced to acknowledged.
The code header in `cs_pattern_detection.pl` has been updated to reference v5.2 and note the
nine-atom count explicitly.

**Why drift occurred:** The implementation correctly recognized that `self_enforcing` and
`diffuse_epistemic` authority groundings produce structurally distinct configurations not
covered by the five core patterns. The code handled them. The sketch was not updated. This is
the standard path for interpretive accretion — the operational layer (code) absorbed the
new structure while the formal layer (spec) remained nominally unchanged.

---

## 4. Operational encoding update

`cs_pattern_detection.pl` v5.2 changes:

1. Module header updated: references v5.2, enumerates nine pattern atoms with explicit note
   on `natural_law_constraint` and `epistemic_consensus` as acknowledged additions.
2. Three new structural diagnostics exported:
   - `cs_authority_masking/3` — fires when `constraint_signature/2` (metric-computed, zero
     CS-layer input) indicates extraction but `cs_authority_grounding` asserts non-extraction.
   - `cs_cover_story_active/2` — triple corroboration: verdict fires + extraction authority
     + extraction-indicating computed signature.
   - `cs_displaced_beneficiary/1` — naturalized-path reading (lineage/practice/self_enforcing/
     expertise/diffuse_epistemic authority) linked via `affects_constraint/2` to an extraction
     sibling, where computed signature is not genuinely natural.
3. These diagnostics fire on mismatch between asserted CS fields and computed structural
   signals — not on assertions alone. Agreement is noise; only disagreement surfaces.
