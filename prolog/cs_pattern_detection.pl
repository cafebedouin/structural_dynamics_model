% ============================================================================
% CS PATTERN DETECTION
% ============================================================================
% Classifies constraints against the five commitment-system attractor patterns
% from docs/commitment_systems_sketch_v4.md.
%
% Architecture: LLM asserts cs_structure fields; math emits verdict atoms
% when the assertion is inconsistent with computed structural signals.
% The pattern classification always honors the LLM assertion; verdicts
% are commentary, not overrides.
%
% Exports:
%   cs_pattern/3  — cs_pattern(+ID, -Pattern, -Signals)
%   cs_verdict/2  — cs_verdict(+ID, -VerdictAtom)  [fails if no verdict]
%   cs_has_fields/1 — cs_has_fields(+ID)  [succeeds iff CS fields present]
%
% Pattern atoms:
%   marked_revision | interpretive_accretion | diffuse_reconstruction |
%   implicit_practice | anchored_fixity_with_accretion |
%   anchored_fixity_brittle | no_pattern_match
%
% Verdict atoms:
%   false_marked_revision | false_interpretive_accretion |
%   false_diffuse_reconstruction | false_implicit_practice |
%   false_anchored_fixity_accretion | false_anchored_fixity_brittle
% ============================================================================

:- module(cs_pattern_detection, [
    cs_pattern/3,
    cs_verdict/2,
    cs_has_fields/1
]).

:- use_module(narrative_ontology).
:- use_module(domain_priors).
:- use_module(config).

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
