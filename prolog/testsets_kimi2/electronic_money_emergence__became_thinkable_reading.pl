% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__became_thinkable_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: electronic_money_emergence__became_thinkable_reading
 *   human_readable: Conceptual Thinkability Threshold for Digital Money Emergence
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This constraint instantiates the became_thinkable reading of the
 *   electronic_money_emergence kernel: the claim that digital money emerged
 *   not at an institutional threshold or statistical categorization, but when
 *   the conceptual possibility became technically and socially thinkable. The
 *   referent is the standing arrangement under contest — the historical
 *   necessity of conceptual-technical preconditions for monetary innovation —
 *   not an endorsed alternative. The reading treats emergence as gradual
 *   diffusion without a single threshold, with institutional measurement
 *   lagging conceptual innovation by decades. As a mountain claim, it asserts
 *   this thinkability boundary is a structural feature of historical
 *   epistemology, not an extractive human arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__became_thinkable_reading, 0.05).
domain_priors:suppression_score(electronic_money_emergence__became_thinkable_reading, 0.05).
domain_priors:theater_ratio(electronic_money_emergence__became_thinkable_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__became_thinkable_reading, mountain).
narrative_ontology:human_readable(electronic_money_emergence__became_thinkable_reading, "Conceptual Thinkability Threshold for Digital Money Emergence").
narrative_ontology:topic_domain(electronic_money_emergence__became_thinkable_reading, "economic_history/monetary_theory/technology_studies").

domain_priors:emerges_naturally(electronic_money_emergence__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__became_thinkable_reading, '3469118e-85ca-403f-b5f3-3a0e5a2aa8a2').
narrative_ontology:cs_kernel_codification('3469118e-85ca-403f-b5f3-3a0e5a2aa8a2', distributed).
narrative_ontology:cs_authority_grounding('3469118e-85ca-403f-b5f3-3a0e5a2aa8a2', expertise).
narrative_ontology:cs_interpretation_layer_present('3469118e-85ca-403f-b5f3-3a0e5a2aa8a2').
narrative_ontology:cs_reading_relation('3469118e-85ca-403f-b5f3-3a0e5a2aa8a2', electronic_money_emergence__first_held_reading, influences).
narrative_ontology:cs_reading_relation('3469118e-85ca-403f-b5f3-3a0e5a2aa8a2', electronic_money_emergence__m4_m5_collapse_reading, forecloses).
narrative_ontology:cs_axiom('3469118e-85ca-403f-b5f3-3a0e5a2aa8a2', foundational, conceptual_precedence_over_institutional_recognition).
narrative_ontology:cs_axiom_status(conceptual_precedence_over_institutional_recognition, holdable).
narrative_ontology:cs_axiom_grounding('3469118e-85ca-403f-b5f3-3a0e5a2aa8a2', conceptual_precedence_over_institutional_recognition, empirically_contingent).
narrative_ontology:cs_axiom('3469118e-85ca-403f-b5f3-3a0e5a2aa8a2', foundational, gradual_diffusion_not_threshold).
narrative_ontology:cs_axiom_status(gradual_diffusion_not_threshold, holdable).
narrative_ontology:cs_axiom_grounding('3469118e-85ca-403f-b5f3-3a0e5a2aa8a2', gradual_diffusion_not_threshold, empirically_contingent).
narrative_ontology:cs_reference_frame('3469118e-85ca-403f-b5f3-3a0e5a2aa8a2', conceptual_technical_horizon).
narrative_ontology:cs_drift_state('3469118e-85ca-403f-b5f3-3a0e5a2aa8a2', contemporary_monetary_studies, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3469118e-85ca-403f-b5f3-3a0e5a2aa8a2', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared historiographical horizon for economic historians and technology studies scholars: by locating emergence in conceptual-technical preconditions, it coordinates interdisciplinary research on pre-institutional innovation without requiring a single institutional gatekeeper.
% TRANSFER_FUNCTION: No material transfer. The arrangement moves historiographical priority and explanatory weight from institutional and measurement-centered narratives to conceptual and technical genealogy.
% ABSENT_VOICES: Institutional economists and central bank historians who define money by official issuance and statistical categorization are underrepresented in this reading; they would argue that money without institutional recognition is merely a technical experiment, not a monetary form.
% DISAPPEARANCE_RATIONALE: If the thinkability boundary as a descriptive historical limit vanished overnight, the world would remain unchanged: no social arrangements currently depend on this past constraint for their ongoing operation. It is a historical mountain, not a governing arrangement.
% FOUNDING_PROBLEM: How to account for the origin of digital money when institutional records and statistical categories fail to capture pre-official innovation.
% FOUNDING_PROBLEM_CORROBORATION: Technology studies scholars and economic historians outside the institutional-mainstream attest that conceptual and technical precursors preceded official recognition by decades; archival evidence of early computing and cryptography research corroborates the pre-institutional phase.
narrative_ontology:disappearance_verdict(electronic_money_emergence__became_thinkable_reading, world_unchanged).
narrative_ontology:founding_problem_status(electronic_money_emergence__became_thinkable_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__became_thinkable_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(electronic_money_emergence__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__became_thinkable_reading, 0.05, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__became_thinkable_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, ExtMetricName, E),
    domain_priors:suppression_score(electronic_money_emergence__became_thinkable_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(electronic_money_emergence__became_thinkable_reading),
    narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(electronic_money_emergence__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint scores extremely low on extractiveness (0.05) and suppression (0.05) because it describes a cognitive-technical boundary condition rather than an active arrangement that extracts from governed parties. Theater is negligible (0.05) because there is no performative maintenance. Accessibility collapse is high (0.88): once the prerequisite technical and conceptual conditions are understood, the impossibility of pre-thinkability digital money becomes nearly irreducible. Resistance is low (0.15) but non-zero because competing institutional and measurement-based readings contest the naturalization of this boundary. The flat measurement series reflect the stability of the historical limit over the interval.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival divergence because the constraint operates as a universal historical limit. Institutional historians and measurement-focused economists experience the constraint as an unwelcome naturalization that undermines their ontological priority; conceptual historians and technology studies scholars experience it as a descriptive law. The engine computes this as low directional variance across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared because the thinkability boundary is structurally universal: it governed all historical agents in the monetary domain equally. No agent collects rents from the fact that concepts require preconditions; no agent pays asymmetric costs. Directionality defaults to symmetric (d ≈ 0.5) for all power atoms via canonical fallback.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the thinkability boundary as extraction by requiring the mountain structural profile: high accessibility_collapse, low resistance, negligible theater, and no active enforcement. If the constraint were actively enforced by a historiographical guild to marginalize institutional readings, it would score higher on suppression and extractiveness and likely compute as tangled_rope or snare. The authored metrics resist this: the boundary is not enforced, only recognized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_limit_vs_constructed_narrative,
    'Is the thinkability boundary a genuine historical-epistemic limit on monetary innovation, or a constructed narrative that privileges conceptual history over institutional and measurement-based accounts?',
    'Comparative historiographical analysis tracing the diffusion of digital money concepts through technical literature, correspondence, and proto-implementations prior to institutional adoption or statistical categorization.',
    'If constructed, the constraint reclassifies from mountain to tangled_rope or snare in the academic politics of economic history; if genuine, the mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_limit_vs_constructed_narrative, conceptual, 'Whether the thinkability threshold is a natural limit or a historiographical construct').

omega_variable(
    measurement_lag_ambiguity,
    'Does the documented lag between conceptual innovation and institutional measurement reflect an irreducible epistemic feature of monetary evolution, or a contingent institutional failure to recognize informal innovation?',
    'Systematic archival review of central bank and statistical office records against contemporaneous technical publications and experimental systems.',
    'A contingent lag would imply the constraint is institutionally mediated rather than a pure mountain; an irreducible lag supports the natural-limit reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_lag_ambiguity, empirical, 'Whether measurement lag is inherent or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__became_thinkable_reading, 1960, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1960, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(elec_tr_t1980, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(elec_tr_t2000, electronic_money_emergence__became_thinkable_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(elec_tr_t2020, electronic_money_emergence__became_thinkable_reading, theater_ratio, 2020, 0.05).

% Extraction over time
narrative_ontology:measurement(elec_be_t1960, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1960, 0.05).
narrative_ontology:measurement(elec_be_t1980, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1980, 0.05).
narrative_ontology:measurement(elec_be_t2000, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(elec_be_t2020, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 2020, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(electronic_money_emergence__became_thinkable_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, first_held_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% This constraint and its siblings are decomposed readings of the electronic_money_emergence kernel, distinguished by their epsilon referents and ontological commitments: thinkability-diffusion vs institutional-threshold vs measurement-artifact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
