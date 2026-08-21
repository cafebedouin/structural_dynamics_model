% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__collective_right_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment (Collective Right Reading)
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the 'collective right' reading of the Second
 *   Amendment, which interprets the amendment as protecting the right of
 *   states to maintain militias, rather than an individual's right to own
 *   firearms. Under this reading, the Second Amendment primarily serves to
 *   ensure state security and grants broad regulatory authority to state
 *   governments over firearms. It is claimed as a Mountain because, within
 *   its own interpretive framework, it is presented as an unchangeable,
 *   foundational principle of constitutional law, reflecting the original
 *   intent and historical context.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.15).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.25).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, mountain).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment (Collective Right Reading)").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:emerges_naturally(second_amendment_scope__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, 'f51cdb9d-c5ae-4b10-9358-080449dffe31').
narrative_ontology:cs_kernel_codification('f51cdb9d-c5ae-4b10-9358-080449dffe31', fixed_text).
narrative_ontology:cs_authority_grounding('f51cdb9d-c5ae-4b10-9358-080449dffe31', lineage).
narrative_ontology:cs_interpretation_layer_present('f51cdb9d-c5ae-4b10-9358-080449dffe31').
narrative_ontology:cs_reading_relation('f51cdb9d-c5ae-4b10-9358-080449dffe31', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('f51cdb9d-c5ae-4b10-9358-080449dffe31', second_amendment_scope__civic_right_reading, forecloses).
narrative_ontology:cs_axiom('f51cdb9d-c5ae-4b10-9358-080449dffe31', foundational, militia_clause_is_operative_clause).
narrative_ontology:cs_axiom_status(militia_clause_is_operative_clause, holdable).
narrative_ontology:cs_axiom_grounding('f51cdb9d-c5ae-4b10-9358-080449dffe31', militia_clause_is_operative_clause, conventional).
narrative_ontology:cs_axiom('f51cdb9d-c5ae-4b10-9358-080449dffe31', foundational, right_to_bear_arms_is_militia_dependent).
narrative_ontology:cs_axiom_status(right_to_bear_arms_is_militia_dependent, holdable).
narrative_ontology:cs_axiom_grounding('f51cdb9d-c5ae-4b10-9358-080449dffe31', right_to_bear_arms_is_militia_dependent, conventional).
narrative_ontology:cs_reference_frame('f51cdb9d-c5ae-4b10-9358-080449dffe31', original_constitutional_compact).
narrative_ontology:cs_drift_state('f51cdb9d-c5ae-4b10-9358-080449dffe31', contemporary_judicial_interpretations, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f51cdb9d-c5ae-4b10-9358-080449dffe31', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, organized_militias).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_scope__collective_right_reading, gun_rights_advocates).
narrative_ontology:constraint_vindicates(second_amendment_scope__collective_right_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__collective_right_reading, republican_self_defense_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the constitutional recognition of their authority to organize and maintain militias, providing a basis for state-level defense and public order without federal preemption of this power. This reading grants them broad regulatory authority over firearms.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Are constitutionally recognized as necessary for state security, legitimizing their existence and providing a framework for their organization under state authority. Their existence is tied to state legislative action.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, organized_militias, beneficiary,
    organized, biographical, constrained, regional).

% Are not granted an individual right to bear arms by this reading, meaning their ability to own firearms is entirely subject to state regulation. They are excluded from the direct constitutional protection of the Second Amendment under this interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_citizens, excluded,
    powerless, biographical, trapped, local).

% As the ultimate interpreter of the Constitution, the federal government (especially the judiciary) sets the authoritative scope of the Second Amendment. This reading limits federal intervention in state militia organization and firearms regulation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_government, agenda_setter,
    institutional, civilizational, analytical, national).

% Bear the cost of this interpretation as it undermines their claims for individual firearms ownership rights. They must pursue legislative or alternative constitutional amendment routes to secure individual rights.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, gun_rights_advocates, payer,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the constitutional authority between federal and state governments regarding the maintenance of public order and defense through organized militias, ensuring states retain a means of self-defense.
% TRANSFER_FUNCTION: Transfers the authority to regulate firearms from individuals to state governments, and legitimizes state-organized militias as the primary constitutional concern.
% ABSENT_VOICES: Individual citizens seeking an individual right to bear arms are absent from the constitutional protection afforded by this reading; their voices are heard in legislative debates but not directly by the Second Amendment itself under this interpretation.
% DISAPPEARANCE_RATIONALE: If this reading of the Second Amendment vanished, the constitutional landscape regarding firearms would fundamentally shift, likely empowering individual rights claims and altering the balance of power between states and individuals regarding gun control. State militias might lose their explicit constitutional grounding.
% FOUNDING_PROBLEM: The founding problem was to ensure the security of a free state by providing for a well-regulated militia, preventing both federal overreach and the need for a standing federal army.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars outside of direct state government or militia advocacy corroborate that the historical context of the Second Amendment strongly supports a focus on state militias and collective security, rather than an unfettered individual right.
narrative_ontology:disappearance_verdict(second_amendment_scope__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__collective_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_scope__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__collective_right_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__collective_right_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, ExtMetricName, E),
    domain_priors:suppression_score(second_amendment_scope__collective_right_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(second_amendment_scope__collective_right_reading),
    narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(second_amendment_scope__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because this reading primarily defines institutional authority rather than extracting resources from individuals; any 'extraction' is a consequence of regulatory power, not a direct rent. Suppression is low (0.25) as it primarily suppresses alternative interpretations rather than actively coercing individuals, though it enables state-level suppression of individual gun ownership. Theater ratio is low (0.05) as the interpretation is functionally applied in legal and political discourse, not merely performed. Accessibility collapse is high (0.8) because, within this framework, alternative interpretations (individual rights) are largely foreclosed. Resistance is low (0.1) from within this interpretive community, though it faces significant external resistance from other readings.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state governments and militias, this reading is a foundational principle ensuring their capacity for self-defense. From the perspective of individual citizens and gun rights advocates, it is a restrictive interpretation that denies fundamental individual liberties. The engine's classification will highlight this divergence, showing a Mountain for institutional beneficiaries and a more extractive type for individual targets.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and organized militias are beneficiaries, as this reading explicitly protects and legitimizes their authority and existence. Individual citizens are excluded from direct constitutional protection, making them indirect targets of the regulatory authority this reading grants to states. The federal government acts as an agenda-setter through its interpretive role, while gun rights advocates are payers, bearing the cost of this interpretation's legal implications.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_ambiguity,
    'Is the historical ''original intent'' of the Second Amendment definitively aligned with a collective right, or is it ambiguous enough to support individual rights interpretations?',
    'Further historical and legal scholarship, potentially new archival discoveries, or a definitive Supreme Court ruling that explicitly addresses and forecloses alternative historical interpretations.',
    'If original intent is definitively collective, this reading''s ''mountain'' claim is strengthened. If ambiguity is proven, its naturalness is weakened, potentially reclassifying it as a ''tangled_rope'' or ''snare'' sustained by institutional power rather than historical fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_intent_ambiguity, empirical, 'Uncertainty regarding the historical grounding of the collective right interpretation.').

omega_variable(
    institutional_vs_natural_law,
    'Is this constraint a genuine natural law (a foundational constitutional principle) or a constructed institutional interpretation that benefits identifiable agents (state governments, militias)?',
    'Analysis of how the interpretation has evolved through judicial precedent and political discourse, particularly whether it has been actively defended by beneficiaries against competing interpretations, or if it has persisted without active institutional support.',
    'If it''s primarily a constructed interpretation, its ''mountain'' classification is a ''false summit'', likely reclassifying as a ''tangled_rope'' or ''snare'' due to the presence of beneficiaries and active enforcement of the interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_vs_natural_law, conceptual, 'Ambiguity between a natural constitutional principle and an institutionally maintained interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__collective_right_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(seco_tr_t50, second_amendment_scope__collective_right_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(seco_tr_t100, second_amendment_scope__collective_right_reading, theater_ratio, 100, 0.05).
narrative_ontology:measurement(seco_tr_t150, second_amendment_scope__collective_right_reading, theater_ratio, 150, 0.05).
narrative_ontology:measurement(seco_tr_t200, second_amendment_scope__collective_right_reading, theater_ratio, 200, 0.05).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__collective_right_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(seco_be_t50, second_amendment_scope__collective_right_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(seco_be_t100, second_amendment_scope__collective_right_reading, base_extractiveness, 100, 0.15).
narrative_ontology:measurement(seco_be_t150, second_amendment_scope__collective_right_reading, base_extractiveness, 150, 0.15).
narrative_ontology:measurement(seco_be_t200, second_amendment_scope__collective_right_reading, base_extractiveness, 200, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__collective_right_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(seco_su_t50, second_amendment_scope__collective_right_reading, suppression_requirement, 50, 0.25).
narrative_ontology:measurement(seco_su_t100, second_amendment_scope__collective_right_reading, suppression_requirement, 100, 0.25).
narrative_ontology:measurement(seco_su_t150, second_amendment_scope__collective_right_reading, suppression_requirement, 150, 0.25).
narrative_ontology:measurement(seco_su_t200, second_amendment_scope__collective_right_reading, suppression_requirement, 200, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__civic_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, state_gun_control_laws).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, federal_firearms_regulation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'second_amendment_scope' kernel, each representing a distinct interpretation of the Second Amendment's scope. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
