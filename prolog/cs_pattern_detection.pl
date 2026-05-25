% ============================================================================
% CS PATTERN DETECTION
% ============================================================================
% Classifies constraints against commitment-system attractor patterns from
% docs/commitment_systems/commitment_systems_sketch_v5_2.md.
%
% Architecture: LLM asserts cs_structure fields; math emits verdict atoms
% when the assertion is inconsistent with computed structural signals.
% The pattern classification always honors the LLM assertion; verdicts
% are commentary, not overrides.
%
% Exports:
%   cs_pattern/3              — cs_pattern(+ID, -Pattern, -Signals)
%   cs_verdict/2              — cs_verdict(+ID, -VerdictAtom)  [fails if no verdict]
%   cs_has_fields/1           — cs_has_fields(+ID)  [succeeds iff CS fields present]
%   cs_naturalized_mountain/1 — cs_naturalized_mountain(+ID)
%   cs_authority_masking/3    — cs_authority_masking(+C, -Sig, -AG)
%   cs_cover_story_active/2   — cs_cover_story_active(+C, -Verdict)
%   cs_displaced_beneficiary/1 — cs_displaced_beneficiary(+C)
%
% Pattern atoms (9 total; natural_law_constraint and epistemic_consensus are
% implementation additions acknowledged in v5.2 as spec drift):
%   marked_revision | interpretive_accretion | diffuse_reconstruction |
%   implicit_practice | anchored_fixity_with_accretion |
%   anchored_fixity_brittle | natural_law_constraint | epistemic_consensus |
%   no_pattern_match
%
% Verdict atoms:
%   false_marked_revision | false_interpretive_accretion |
%   false_diffuse_reconstruction | false_implicit_practice |
%   false_anchored_fixity_accretion | false_anchored_fixity_brittle |
%   false_natural_law_constraint
% ============================================================================

:- module(cs_pattern_detection, [
    cs_pattern/3,
    cs_verdict/2,
    cs_has_fields/1,
    cs_naturalized_mountain/1,
    cs_authority_masking/3,
    cs_cover_story_active/2,
    cs_displaced_beneficiary/1,
    cs_grounding_mismatch/3,
    cs_drift_unacknowledged/2
]).

:- use_module(narrative_ontology).
:- use_module(domain_priors).
:- use_module(config).
:- use_module(signature_detection).

% CS structure predicates are declared multifile so constraint files can extend them.
:- multifile
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1.

/* ================================================================
   FIELD ACCESSORS
   ================================================================ */

%% cs_kernel_codification(+C, -Value)
%  Reads the kernel_codification CS field for constraint C.
cs_kernel_codification(C, Value) :-
    narrative_ontology:cs_kernel_codification(C, Value).

%% cs_authority_grounding(+C, -Value)
%  Reads the authority_grounding CS field for constraint C.
cs_authority_grounding(C, Value) :-
    narrative_ontology:cs_authority_grounding(C, Value).

%% cs_interp_layer(+C)
%  Succeeds iff interpretation_layer_present is declared true for C.
%  v5 licensing condition: AG=lineage (any KC) OR (KC=formalized AND AG=extraction).
%  NOTE: this predicate enforces no KC/AG constraint; licensing is structural-by-clause-call.
cs_interp_layer(C) :-
    narrative_ontology:cs_interpretation_layer_present(C).

/* ================================================================
   MAIN PREDICATE: cs_has_fields/1
   ================================================================ */

%% cs_has_fields(+C)
%  Succeeds iff constraint C has CS structure fields declared.
cs_has_fields(C) :-
    cs_kernel_codification(C, _), !.

/* ================================================================
   MAIN PREDICATE: cs_pattern/3
   ================================================================ */

%% cs_pattern(+C, -Pattern, -Signals)
%  Classifies C against the five CS patterns.
%  Returns no_pattern_match when CS fields are absent or combination is anomalous.

% Fields absent → silent no_pattern_match
cs_pattern(C, no_pattern_match, [cs_fields_absent]) :-
    \+ cs_has_fields(C), !.

% Fields present → dispatch to classifier
cs_pattern(C, Pattern, Signals) :-
    cs_has_fields(C),
    cs_kernel_codification(C, KC),
    cs_authority_grounding(C, AG),
    cs_classify(C, KC, AG, Pattern, Signals), !.

% Fallback: anomalous field combination
cs_pattern(C, no_pattern_match, [anomalous_field_combination]) :-
    cs_has_fields(C).

/* ================================================================
   PATTERN CLASSIFIER: cs_classify/5
   ================================================================ */

% kernel or authority = none → not a commitment system
cs_classify(_, none, _, no_pattern_match, [kernel_none]) :- !.
cs_classify(_, _, none, no_pattern_match, [authority_none]) :- !.

% Marked revision — formalized kernel + expertise or distributed authority
cs_classify(_, formalized, expertise, marked_revision,
            [kernel_formalized, authority_expertise]) :- !.
cs_classify(_, formalized, distributed, marked_revision,
            [kernel_formalized, authority_distributed]) :- !.

% Anchored fixity — formalized kernel + extraction authority
% With accretion layer (interpretation_layer_present = true)
cs_classify(C, formalized, extraction, anchored_fixity_with_accretion,
            [kernel_formalized, authority_extraction, interp_layer_present]) :-
    cs_interp_layer(C), !.
% Without accretion layer
cs_classify(_, formalized, extraction, anchored_fixity_brittle,
            [kernel_formalized, authority_extraction, interp_layer_absent]) :- !.

% Interpretive accretion — formalized kernel + lineage authority (principle-anchored)
% Probe (Change 1): zero corpus instances of lineage+interp-false; interp implied-true.
% Branch-A collision: privilege_waiver_threshold asserts interp-present but that fact is
% not read here — interp_layer_implied is derived, not asserted. Latent inconsistency
% recorded; no fix this round.
cs_classify(_, formalized, lineage, interpretive_accretion,
            [kernel_formalized, authority_lineage, interp_layer_implied]) :- !.

% Interpretive accretion — fixed text + lineage authority (text-anchored)
cs_classify(_, fixed_text, lineage, interpretive_accretion,
            [kernel_fixed_text, authority_lineage, interp_layer_implied]) :- !.

% Diffuse reconstruction — distributed kernel + distributed authority
cs_classify(_, distributed, distributed, diffuse_reconstruction,
            [kernel_distributed, authority_distributed]) :- !.

% Implicit practice — implicit kernel + practice authority
cs_classify(_, implicit, practice, implicit_practice,
            [kernel_implicit, authority_practice]) :- !.

% Natural law — self_enforcing authority (no adjudicator; constraint is its own enforcement)
cs_classify(_, _, self_enforcing, natural_law_constraint,
            [authority_self_enforcing]) :- !.

% Epistemic consensus — diffuse_epistemic authority (community establishes direction,
% adjudicates no specific instance)
cs_classify(_, _, diffuse_epistemic, epistemic_consensus,
            [authority_diffuse_epistemic]) :- !.

% Everything else is anomalous
cs_classify(_, _, _, no_pattern_match, [anomalous_field_combination]).

/* ================================================================
   VERDICT LAYER: cs_verdict/2
   ================================================================
   Each clause fires when the LLM-claimed pattern is inconsistent with
   computed structural signals. Fails silently when no violation exists.
   Verdict atoms accompany the pattern; they do not override it.
   ================================================================ */

%% cs_verdict(+C, -VerdictAtom)
%  Emits a verdict atom when claimed pattern contradicts structural signals.
%  Fails if no violation detected (non-deterministic: at most one verdict per constraint).

% Pattern check helper — calls cs_pattern with unbound var, then unifies.
% This is necessary because cs_classify uses pattern-matching dispatch; calling
% cs_pattern with a bound Pattern arg would route to the wrong cs_classify clause.
cs_pattern_is(C, Expected) :-
    once(cs_pattern(C, Actual, _)),
    Actual == Expected.

% false_marked_revision
% Fires when marked_revision is claimed but signals show suppression or enforcement.
cs_verdict(C, false_marked_revision) :-
    cs_pattern_is(C, marked_revision),
    (   ( narrative_ontology:constraint_metric(C, suppression_requirement, S), S >= 0.50 )
    ;   ( narrative_ontology:constraint_metric(C, theater_ratio, TR), TR >= 0.60 )
    ;   ( \+ narrative_ontology:has_sunset_clause(C),
          domain_priors:requires_active_enforcement(C) )
    ), !.

% false_interpretive_accretion
% Fires when interpretive_accretion is claimed but enforcement type or metrics contradict.
cs_verdict(C, false_interpretive_accretion) :-
    cs_pattern_is(C, interpretive_accretion),
    (   narrative_ontology:coordination_type(C, enforcement_mechanism)
    ;   ( narrative_ontology:constraint_metric(C, theater_ratio, TR), TR < 0.35 )
    ;   ( narrative_ontology:constraint_metric(C, suppression_requirement, S), S < 0.20 )
    ), !.

% false_diffuse_reconstruction
% Fires when diffuse_reconstruction is claimed but enforcement signals suggest a single enforcer.
cs_verdict(C, false_diffuse_reconstruction) :-
    cs_pattern_is(C, diffuse_reconstruction),
    (   ( narrative_ontology:constraint_metric(C, suppression_requirement, S), S >= 0.60 )
    ;   narrative_ontology:coordination_type(C, enforcement_mechanism)
    ), !.

% false_implicit_practice
% Fires when implicit_practice is claimed but metrics contradict authentic practice-based authority.
cs_verdict(C, false_implicit_practice) :-
    cs_pattern_is(C, implicit_practice),
    (   domain_priors:emerges_naturally(C)
    ;   ( narrative_ontology:constraint_metric(C, theater_ratio, TR), TR >= 0.60 )
    ;   ( narrative_ontology:constraint_metric(C, suppression_requirement, S), S >= 0.50 )
    ), !.

% false_anchored_fixity_accretion
% Fires when anchored_fixity_with_accretion is claimed but signals suggest the
% interpretive layer is not functioning (enforcement type or very high suppression).
cs_verdict(C, false_anchored_fixity_accretion) :-
    cs_pattern_is(C, anchored_fixity_with_accretion),
    (   narrative_ontology:coordination_type(C, enforcement_mechanism)
    ;   ( narrative_ontology:constraint_metric(C, suppression_requirement, S), S >= 0.70 )
    ), !.

% false_anchored_fixity_brittle
% Fires when anchored_fixity_brittle is claimed but signals suggest an informal
% accretion layer (identity coordination + moderate suppression).
cs_verdict(C, false_anchored_fixity_brittle) :-
    cs_pattern_is(C, anchored_fixity_brittle),
    narrative_ontology:coordination_type(C, identity_coordination),
    narrative_ontology:constraint_metric(C, suppression_requirement, S),
    S < 0.50, !.

% false_natural_law_constraint
% Fires when natural_law_constraint is computed but the constraint has a beneficiary —
% self_enforcing is a disguise for extractive force (naturalized mountain).
cs_verdict(C, false_natural_law_constraint) :-
    cs_pattern_is(C, natural_law_constraint),
    narrative_ontology:constraint_beneficiary(C, _), !.

/* ================================================================
   NATURALIZED MOUNTAIN DIAGNOSTIC: cs_naturalized_mountain/1
   ================================================================ */

%% cs_naturalized_mountain(+C)
%  Succeeds iff C is a low-ε mountain with extraction/diffuse_epistemic authority
%  and both victims and beneficiaries — invariability-form with extractive force.
cs_naturalized_mountain(C) :-
    cs_has_fields(C),
    cs_authority_grounding(C, AG),
    memberchk(AG, [extraction, diffuse_epistemic]),
    narrative_ontology:constraint_metric(C, extractiveness, Eps),
    Eps < 0.15,
    narrative_ontology:constraint_beneficiary(C, _),
    narrative_ontology:constraint_victim(C, _).

/* ================================================================
   STRUCTURAL DIAGNOSTICS (Phase 1)

   These predicates fire on DISAGREEMENT between LLM-asserted CS fields
   and the metric-computed constraint_signature/2 from signature_detection.
   Agreement is noise; only mismatch surfaces.

   constraint_signature/2 reads zero CS-layer fields — it computes purely
   from constraint_metric/3, domain_priors, and structural facts.
   The two predicates are independent LLM assertions about different aspects
   of the constraint, so disagreement is a structural finding.
   ================================================================ */

%% cs_extraction_signature(+Sig)
%  Atoms from constraint_signature/2 that indicate extractive structure.
cs_extraction_signature(false_natural_law).
cs_extraction_signature(false_ci_rope).
cs_extraction_signature(false_summit_mountain).
cs_extraction_signature(constructed_high_extraction).

%% cs_authority_masking(+C, -Sig, -AG)
%  Fires when constraint_signature says extraction but cs_authority_grounding
%  asserts a non-extraction label (masking the extractive structure).
%  Sig is the computed extraction-indicating signature atom.
%  AG is the asserted (non-extraction) authority grounding atom.
%  Uses narrative_ontology:cs_kernel_codification/2 for enumeration — cs_has_fields/1
%  uses a cut that prevents backtracking when C is unbound.
cs_authority_masking(C, Sig, AG) :-
    narrative_ontology:cs_kernel_codification(C, _),
    signature_detection:constraint_signature(C, Sig),
    cs_extraction_signature(Sig),
    cs_authority_grounding(C, AG),
    AG \= extraction.

%% cs_cover_story_active(+C, -Verdict)
%  Triple corroboration: (1) a pattern verdict fires, (2) LLM asserts
%  extraction authority, AND (3) computed signature confirms extraction.
%  The false pattern is structural, not accidental — the authority with
%  extraction stakes generates the cover story.
cs_cover_story_active(C, Verdict) :-
    narrative_ontology:cs_kernel_codification(C, _),
    cs_verdict(C, Verdict),
    cs_authority_grounding(C, extraction),
    signature_detection:constraint_signature(C, Sig),
    cs_extraction_signature(Sig).

%% cs_displaced_beneficiary(+C)
%  Fires when C presents a naturalized-path authority (lineage, practice,
%  self_enforcing, expertise, diffuse_epistemic) but is not genuinely
%  natural by computed signature, AND has a cs_reading_relation/3 forecloses
%  edge to a sibling that explicitly declares extraction authority.
%  Uses the typed edge (forecloses) to avoid firing on coexisting readings
%  of the same kernel — sibling readings that coexist_with one another are
%  not domination relationships, so the displaced-beneficiary signal does not
%  apply. Bare affects_constraint/2 edges are intentionally excluded.
cs_displaced_beneficiary(C) :-
    narrative_ontology:cs_kernel_codification(C, _),
    cs_authority_grounding(C, AG),
    memberchk(AG, [self_enforcing, lineage, practice, expertise, diffuse_epistemic]),
    signature_detection:constraint_signature(C, Sig),
    \+ memberchk(Sig, [natural_law, coupling_invariant_rope, coordination_scaffold]),
    narrative_ontology:cs_reading_relation(C, Sibling, forecloses),
    cs_has_fields(Sibling),
    cs_authority_grounding(Sibling, extraction).

/* ================================================================
   GENERALIZED GROUNDING MISMATCH: cs_grounding_mismatch/3
   ================================================================
   Fires when the LLM-asserted authority grounding (AG) is structurally
   inconsistent with the computed constraint_signature (Sig).
   Mechanical mismatch check only — does not modify classification or
   override the CS pattern.

   Strict superset of cs_authority_masking/3:
     Masking direction: naturalized AGs (lineage, practice, expertise,
       distributed, diffuse_epistemic, self_enforcing) paired with an
       extraction-indicating signature — the cover-story family.
     Reverse direction (NEW): extraction AG paired with genuinely
       natural/coordination signatures (natural_law, coupling_invariant_rope,
       coordination_scaffold) — extraction claim contradicts a non-extractive
       computed structure.
     Self-enforcing + coordination_scaffold (NEW): authority claims
       natural necessity but signature shows a chosen, replaceable standard.

   Silent zone (no clause here):
     - AG=none: wholly excluded from CS patterns; no structural reading.
     - Tradition-family AGs with natural/coordination signatures: metrics
       cannot independently distinguish correct from incorrect label.
     - All AGs with ambiguous, piton_signature, or constructed_constraint:
       these profile-based middle-range signatures carry insufficient
       directional information to convict.
     - extraction + coupling_invariant_rope: architecturally unreachable —
       CI_Rope requires a beneficiary (has_coordination_function), but
       false_summit_mountain (priority 3) intercepts any beneficiary-bearing
       constraint before CI_Rope (priority 5) can certify. Retained in the
       contradiction table as documentation; will never fire in practice.

   Relationship to cs_authority_masking/3:
     cs_authority_masking/3 names the specific cover-story pattern (a
     naturalized AG masking an extraction-indicating signature). Keep both —
     they are analytically distinct even though their firing sets overlap.
   ================================================================ */

%% cs_grounding_mismatch(+C, -AG, -Sig)
%  Enumerates (C, AG, Sig) triples where the asserted grounding contradicts
%  the computed signature. Enumerates via cs_kernel_codification/2 directly
%  to avoid the cut in cs_has_fields/1.
cs_grounding_mismatch(C, AG, Sig) :-
    narrative_ontology:cs_kernel_codification(C, _),
    cs_authority_grounding(C, AG),
    signature_detection:constraint_signature(C, Sig),
    cs_grounding_contradiction(AG, Sig).

%% cs_grounding_contradiction(+AG, +Sig)
%  True when authority-grounding atom AG is inconsistent with signature
%  atom Sig. Fails silently for consistent or undetermined pairs.

% Masking direction: naturalized AGs + extraction-indicating signatures
cs_grounding_contradiction(lineage,           Sig) :- cs_extraction_signature(Sig).
cs_grounding_contradiction(practice,          Sig) :- cs_extraction_signature(Sig).
cs_grounding_contradiction(expertise,         Sig) :- cs_extraction_signature(Sig).
cs_grounding_contradiction(distributed,       Sig) :- cs_extraction_signature(Sig).
cs_grounding_contradiction(diffuse_epistemic, Sig) :- cs_extraction_signature(Sig).
cs_grounding_contradiction(self_enforcing,    Sig) :- cs_extraction_signature(Sig).

% Self-enforcing claims natural necessity; coordination_scaffold says it was a choice.
cs_grounding_contradiction(self_enforcing, coordination_scaffold).

% Reverse direction: extraction AG + genuinely natural/coordination signatures
cs_grounding_contradiction(extraction, natural_law).
cs_grounding_contradiction(extraction, coupling_invariant_rope).
cs_grounding_contradiction(extraction, coordination_scaffold).

/* ================================================================
   TEMPORAL DRIFT: cs_drift_unacknowledged/2
   Type-A static signal: substantial or severe drift that the authority
   structure has not acknowledged. Independent of cs_drift_trajectory/3
   (which computes t2 regardless of acknowledgment status).
   ================================================================ */

%% cs_drift_unacknowledged(+C, -Gap)
%  Fires when C has a cs_drift_state with a non-trivial gap (not stable,
%  not minor) that the authority structure has not acknowledged.
cs_drift_unacknowledged(C, Gap) :-
    narrative_ontology:cs_drift_state(C, _, Gap),
    Gap = gap(Dir, Mag, false),
    Dir \= stable,
    Mag \= minor.
