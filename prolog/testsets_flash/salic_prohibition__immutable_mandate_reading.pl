% ============================================================================
% CONSTRAINT STORY: salic_prohibition__immutable_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__immutable_mandate_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: salic_prohibition__immutable_mandate_reading
 *   human_readable: Salic Prohibition: Immutable Mandate Reading
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint represents the 'immutable mandate' reading of Salic Law,
 *   where it is understood as an unchangeable natural or divine law embedded
 *   in the dynastic constitution, categorically excluding female heirs from
 *   succession. This reading legitimizes agnatic claimants and male
 *   aristocracy, while actively suppressing the claims of female heirs and
 *   cognatic lineages. The constraint's persistence relies on strong
 *   enforcement and ideological justification, leading to high extraction and
 *   suppression.
 *
 * KEY AGENTS:
 *   - agnatic_claimants: Agenda-setter (institutional/arbitrage) — actively enforces and benefits from female exclusion.
 *   - female_heirs: Payer (powerless/trapped) — categorically excluded and disinherited.
 *   - male_aristocracy: Beneficiary (powerful/mobile) — maintains patriarchal power structures.
 *   - cognatic_lineages: Payer (moderate/constrained) — denied dynastic aspirations.
 *   - kingdoms_subject_to_succession_wars: Victim (institutional/trapped) — bear costs of dynastic disputes.
 *   - theologians_and_jurists: Agenda-setter (organized/identity_locked) — provide ideological justification.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.85).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.9).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, snare).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Prohibition: Immutable Mandate Reading").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, '2af0fc6b-d7a1-4be3-b347-6b55e4b4dc9f').
narrative_ontology:cs_kernel_codification('2af0fc6b-d7a1-4be3-b347-6b55e4b4dc9f', fixed_text).
narrative_ontology:cs_authority_grounding('2af0fc6b-d7a1-4be3-b347-6b55e4b4dc9f', lineage).
narrative_ontology:cs_interpretation_layer_present('2af0fc6b-d7a1-4be3-b347-6b55e4b4dc9f').
narrative_ontology:cs_reading_relation('2af0fc6b-d7a1-4be3-b347-6b55e4b4dc9f', salic_prohibition__sovereign_override_reading, forecloses).
narrative_ontology:cs_reading_relation('2af0fc6b-d7a1-4be3-b347-6b55e4b4dc9f', salic_prohibition__cognatic_reversion_reading, forecloses).
narrative_ontology:cs_axiom('2af0fc6b-d7a1-4be3-b347-6b55e4b4dc9f', foundational, female_exclusion_is_divine_mandate).
narrative_ontology:cs_axiom_status(female_exclusion_is_divine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('2af0fc6b-d7a1-4be3-b347-6b55e4b4dc9f', female_exclusion_is_divine_mandate, theological).
narrative_ontology:cs_axiom('2af0fc6b-d7a1-4be3-b347-6b55e4b4dc9f', foundational, agnatic_primogeniture_is_natural_order).
narrative_ontology:cs_axiom_status(agnatic_primogeniture_is_natural_order, holdable).
narrative_ontology:cs_axiom_grounding('2af0fc6b-d7a1-4be3-b347-6b55e4b4dc9f', agnatic_primogeniture_is_natural_order, deontological).
narrative_ontology:cs_reference_frame('2af0fc6b-d7a1-4be3-b347-6b55e4b4dc9f', divinely_ordained_agnatic_order).
narrative_ontology:cs_drift_state('2af0fc6b-d7a1-4be3-b347-6b55e4b4dc9f', enlightenment_era_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2af0fc6b-d7a1-4be3-b347-6b55e4b4dc9f', '').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_claimants).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, male_aristocracy).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_heirs).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, cognatic_lineages).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, kingdoms_subject_to_succession_wars).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__immutable_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__immutable_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the constraint systematically diverts immense power and wealth from female lines to male lines without compensation. Suppression is also very high (0.90) as any challenge to this 'immutable' law is met with severe political, legal, and often military force. Theater ratio is low (0.10) because the enforcement is genuinely functional in maintaining the agnatic order, not merely performative. The historical measurements show a relatively stable, high level of extraction and suppression, reflecting the entrenched nature of this reading during the period.
 *
 * PERSPECTIVAL GAP:
 *   Agnatic claimants and male aristocracy perceive this as a legitimate, even divinely ordained, order ensuring stability. Female heirs and cognatic lineages experience it as an arbitrary, unjust, and highly extractive imposition. Theologians and jurists, bound by their identity to this interpretation, reinforce the 'naturalness' of the exclusion, while kingdoms suffer the real-world consequences of the resulting conflicts.
 *
 * DIRECTIONALITY LOGIC:
 *   Agnatic claimants and male aristocracy are clear beneficiaries (low d) as they gain power and status. Female heirs and cognatic lineages are direct targets (high d) as they are disinherited. Kingdoms subject to succession wars are victims (high d) as they bear the costs of enforcement. Theologians and jurists are agenda-setters, their directionality tied to the propagation of this specific, immutable reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a snare, not a piton, because it actively benefits identifiable groups (agnatic claimants, male aristocracy) who vigorously enforce it. It is not merely inertial; it is a living mechanism of power transfer. The 'immutable mandate' framing prevents it from being mislabeled as a mountain, as its persistence is clearly dependent on active enforcement and the suppression of alternatives, rather than natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_political_origin,
    'Is the Salic prohibition a genuine divine or natural law, or a politically constructed rule serving dynastic interests?',
    'Historical-critical analysis of its origins, theological debate on its scriptural basis, and comparative legal studies of other succession systems.',
    'If divine/natural, its classification might shift towards a mountain (though still a false summit due to beneficiaries). If political, it remains a snare, with its ''immutable'' claim serving as a cover story for extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_vs_political_origin, conceptual, 'Ambiguity of Salic Law''s ultimate grounding.').

omega_variable(
    enforcement_cost_vs_benefit,
    'Does the cost of enforcing the immutable Salic mandate (e.g., succession wars) outweigh the benefits of ''stability'' it purports to provide?',
    'Quantitative historical analysis of economic and human costs of succession wars versus periods of stable agnatic rule.',
    'If costs consistently outweigh benefits, the constraint''s justification as a ''coordination'' mechanism collapses, reinforcing its snare classification and highlighting the pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_vs_benefit, empirical, 'Net benefit of Salic Law enforcement.').

omega_variable(
    immutable_vs_interpretive_drift,
    'To what extent is the ''immutable'' nature of Salic Law a genuine structural property, versus a rhetorical claim maintained by its beneficiaries to resist interpretive drift?',
    'Analysis of historical legal challenges and theological arguments: how were attempts to reinterpret or override the law handled? Did the ''immutability'' claim itself evolve?',
    'If the claim of immutability is primarily rhetorical, the constraint''s suppression metric might be higher than structurally necessary, reflecting the active suppression of interpretive alternatives. This would further solidify its snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutable_vs_interpretive_drift, conceptual, 'Rhetorical vs. structural immutability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 1300, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t1300, salic_prohibition__immutable_mandate_reading, theater_ratio, 1300, 0.05).
narrative_ontology:measurement(sali_tr_t1400, salic_prohibition__immutable_mandate_reading, theater_ratio, 1400, 0.07).
narrative_ontology:measurement(sali_tr_t1500, salic_prohibition__immutable_mandate_reading, theater_ratio, 1500, 0.09).
narrative_ontology:measurement(sali_tr_t1600, salic_prohibition__immutable_mandate_reading, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(sali_tr_t1700, salic_prohibition__immutable_mandate_reading, theater_ratio, 1700, 0.11).
narrative_ontology:measurement(sali_tr_t1800, salic_prohibition__immutable_mandate_reading, theater_ratio, 1800, 0.1).

% Extraction over time
narrative_ontology:measurement(sali_be_t1300, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1300, 0.75).
narrative_ontology:measurement(sali_be_t1400, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1400, 0.8).
narrative_ontology:measurement(sali_be_t1500, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1500, 0.85).
narrative_ontology:measurement(sali_be_t1600, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1600, 0.87).
narrative_ontology:measurement(sali_be_t1700, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1700, 0.86).
narrative_ontology:measurement(sali_be_t1800, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1800, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t1300, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1300, 0.8).
narrative_ontology:measurement(sali_su_t1400, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1400, 0.85).
narrative_ontology:measurement(sali_su_t1500, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1500, 0.9).
narrative_ontology:measurement(sali_su_t1600, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1600, 0.92).
narrative_ontology:measurement(sali_su_t1700, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1700, 0.91).
narrative_ontology:measurement(sali_su_t1800, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1800, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'salic_prohibition' kernel. Other readings include 'sovereign_override_reading' and 'cognatic_reversion_reading', which offer alternative interpretations of Salic Law's binding nature and scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
