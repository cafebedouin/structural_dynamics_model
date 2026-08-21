% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__graduated_sovereignty, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty Doctrine
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This constraint represents the 'graduated sovereignty' reading of
 *   Westphalian sovereignty, which posits that a state's sovereignty is not
 *   absolute but exists on a spectrum determined by its capacity to govern
 *   and its legitimacy in the eyes of the international community. This
 *   reading grants external actors (primarily powerful states and
 *   international organizations) discretion to classify states and intervene
 *   in their affairs, often leading to high extraction from 'weak' states.
 *   The claimed type is 'snare' because the coordination story (preventing
 *   humanitarian crises) serves as a cover for asymmetric extraction and
 *   suppression of self-determination.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.65).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.78).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.65).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty Doctrine").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, '13d1f103-7783-44dc-8890-7ea8c3083f1b').
narrative_ontology:cs_kernel_codification('13d1f103-7783-44dc-8890-7ea8c3083f1b', distributed).
narrative_ontology:cs_authority_grounding('13d1f103-7783-44dc-8890-7ea8c3083f1b', extraction).
narrative_ontology:cs_interpretation_layer_present('13d1f103-7783-44dc-8890-7ea8c3083f1b').
narrative_ontology:cs_reading_relation('13d1f103-7783-44dc-8890-7ea8c3083f1b', westphalian_sovereignty__absolute_sovereignty, influences).
narrative_ontology:cs_reading_relation('13d1f103-7783-44dc-8890-7ea8c3083f1b', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('13d1f103-7783-44dc-8890-7ea8c3083f1b', foundational, state_capacity_determines_sovereignty_scope).
narrative_ontology:cs_axiom_status(state_capacity_determines_sovereignty_scope, holdable).
narrative_ontology:cs_axiom_grounding('13d1f103-7783-44dc-8890-7ea8c3083f1b', state_capacity_determines_sovereignty_scope, empirically_contingent).
narrative_ontology:cs_axiom('13d1f103-7783-44dc-8890-7ea8c3083f1b', foundational, governance_legitimacy_is_externally_assessable).
narrative_ontology:cs_axiom_status(governance_legitimacy_is_externally_assessable, holdable).
narrative_ontology:cs_axiom_grounding('13d1f103-7783-44dc-8890-7ea8c3083f1b', governance_legitimacy_is_externally_assessable, conventional).
narrative_ontology:cs_reference_frame('13d1f103-7783-44dc-8890-7ea8c3083f1b', post_cold_war_interventionism).
narrative_ontology:cs_drift_state('13d1f103-7783-44dc-8890-7ea8c3083f1b', contemporary_multipolar_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('13d1f103-7783-44dc-8890-7ea8c3083f1b', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, powerful_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_organizations).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, weak_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, marginalized_populations_in_weak_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states define the criteria for 'state capacity' and 'governance legitimacy,' and exercise the discretion to classify other states along the sovereignty spectrum. They benefit from the flexibility to intervene or withhold recognition based on their strategic interests, often framing interventions as humanitarian or capacity-building.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, powerful_states, agenda_setter,
    institutional, generational, arbitrage, global).

% These organizations gain expanded mandates and legitimacy for intervention, aid, and governance programs in states deemed to have 'lower' sovereignty. They benefit from the increased scope of their operations and influence, often aligning with powerful states' interpretations of legitimacy.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_organizations, beneficiary,
    institutional, generational, constrained, global).

% These states are subject to external classification and intervention, losing effective control over domestic policy and resource management. Their 'sovereignty' becomes conditional on external approval, leading to a loss of self-determination and increased vulnerability to neo-colonial practices. Exit means defying powerful international actors, often at great cost.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, weak_states, payer,
    powerless, generational, trapped, national).

% These populations often bear the direct consequences of external interventions, which may destabilize existing social structures, exacerbate conflicts, or impose governance models that do not align with local needs. Their identity is often tied to the state, making 'exit' from the system difficult or impossible, and they have little agency in the classification process.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, marginalized_populations_in_weak_states, payer,
    powerless, biographical, identity_locked, local).

% These scholars analyze the theoretical underpinnings and practical implications of graduated sovereignty, often critiquing its potential for abuse and its departure from traditional Westphalian principles. They provide independent analysis but have no direct power to alter the constraint.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, scholars_of_international_law, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Attempts to coordinate international responses to 'failed' or 'fragile' states by providing a framework for differentiated engagement, theoretically preventing humanitarian crises and promoting global stability.
% TRANSFER_FUNCTION: Transfers authority and resources from weak states to powerful states and international organizations, justified by assessments of state capacity and governance legitimacy. This includes control over natural resources, policy decisions, and the right to self-determination.
% ABSENT_VOICES: Many post-colonial states and their populations, particularly those classified as 'weak' or 'fragile,' are often excluded from the discourse that defines the criteria for graduated sovereignty. They would argue for unconditional sovereignty and non-interference, viewing the doctrine as a tool for continued domination.
% DISAPPEARANCE_RATIONALE: If the doctrine of graduated sovereignty vanished, powerful states would lose a key justification for intervention and differentiated treatment, forcing a return to either absolute non-interference or a more explicit, universally applied framework for intervention. Weak states would regain a stronger claim to unconditional sovereignty, potentially altering global power dynamics and resource flows.
% FOUNDING_PROBLEM: The perceived failure of some states to protect their populations or maintain order, leading to humanitarian crises, regional instability, and challenges to global security, which traditional absolute sovereignty was seen as unable to address.
% FOUNDING_PROBLEM_CORROBORATION: Powerful states and many international organizations attest that the problem of 'failed states' and their consequences remains live. Critics (including many scholars and representatives of weak states) argue that while state fragility is real, the 'graduated sovereignty' solution often exacerbates the problem or serves as a pretext for other agendas; however, the core problem of state capacity is widely acknowledged, even if the solution is contested.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__graduated_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__graduated_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(westphalian_sovereignty__graduated_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__graduated_sovereignty, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__graduated_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__graduated_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the doctrine enables powerful states to leverage their classification authority for strategic and economic gains, often at the expense of weak states' resources and autonomy. Suppression (0.78) is also high, as weak states have limited means to resist external classification or intervention without facing severe consequences. The theater ratio (0.20) is relatively low, indicating that while there is some performative justification, the doctrine genuinely facilitates intervention, even if the stated humanitarian goals are often secondary to geopolitical interests. The metrics show a trend of increasing extractiveness and suppression over time, reflecting the hardening of this doctrine's application since the post-Cold War era.
 *
 * PERSPECTIVAL GAP:
 *   Powerful states perceive this doctrine as a necessary evolution of international law to address modern challenges, a 'rope' for global governance. Weak states, however, experience it as a 'snare' that legitimizes neo-colonial practices and undermines their fundamental right to self-determination. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerful states and international organizations are clear beneficiaries and agenda-setters, as they define the terms of 'capacity' and 'legitimacy' and gain discretion for intervention (low directionality). Weak states and their marginalized populations are the primary victims, bearing the costs of lost autonomy and external interference (high directionality). Scholars act as observers, analyzing the system without direct participation.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine of graduated sovereignty attempts to resolve the perceived mandatrophy of absolute sovereignty in the face of global challenges. However, this reading itself risks creating a new form of mandatrophy where the 'mandate' to intervene outlives its genuine humanitarian function and becomes a tool for sustained extraction. The classification as a snare highlights this risk, preventing mislabeling as genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classification_criteria_objectivity,
    'Are the criteria for ''state capacity'' and ''governance legitimacy'' objectively defined and applied, or are they subject to the political interests of powerful states?',
    'Independent, transparent audits of classification decisions by a neutral body, assessing consistency across cases and correlation with powerful states'' strategic interests.',
    'If criteria are subjective and politically driven, the doctrine''s legitimacy as a coordination mechanism collapses, reinforcing its classification as a snare. If objective, it might lean towards a tangled rope, acknowledging a genuine (though still extractive) coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_criteria_objectivity, empirical, 'Objectivity vs. political bias in state classification.').

omega_variable(
    intervention_efficacy_vs_extraction,
    'Do interventions justified by graduated sovereignty genuinely improve state capacity and governance legitimacy, or do they primarily serve external interests and perpetuate dependency?',
    'Longitudinal studies comparing outcomes in intervened vs. non-intervened ''weak'' states, controlling for other factors, focusing on local populations'' well-being and self-determination.',
    'Evidence of consistent positive outcomes would challenge the snare classification, suggesting a more complex, potentially tangled rope dynamic. Evidence of negative or self-serving outcomes would solidify the snare classification and highlight its neo-colonial aspects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_efficacy_vs_extraction, empirical, 'Actual impact of interventions vs. stated goals.').

omega_variable(
    sovereignty_definition_ambiguity,
    'Is ''sovereignty'' fundamentally an absolute, indivisible concept, or can it genuinely exist on a spectrum without undermining its core meaning?',
    'Conceptual analysis and philosophical debate within international law and political theory, assessing the coherence and implications of a ''graduated'' definition.',
    'If sovereignty is deemed indivisible, the graduated sovereignty doctrine is conceptually incoherent, reinforcing its status as a constructed snare. If a graduated concept is coherent, it shifts the debate to the fairness and application of the criteria, potentially moving it towards a contested tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_definition_ambiguity, conceptual, 'Conceptual coherence of graduated sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(west_tr_t1998, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1998, 0.15).
narrative_ontology:measurement(west_tr_t2006, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2006, 0.18).
narrative_ontology:measurement(west_tr_t2014, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2014, 0.19).
narrative_ontology:measurement(west_tr_t2024, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(west_be_t1998, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1998, 0.55).
narrative_ontology:measurement(west_be_t2006, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2006, 0.6).
narrative_ontology:measurement(west_be_t2014, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2014, 0.63).
narrative_ontology:measurement(west_be_t2024, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(west_su_t1998, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1998, 0.7).
narrative_ontology:measurement(west_su_t2006, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2006, 0.75).
narrative_ontology:measurement(west_su_t2014, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2014, 0.77).
narrative_ontology:measurement(west_su_t2024, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__graduated_sovereignty, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'westphalian_sovereignty' kernel. It focuses on the implications of a graduated view of state sovereignty, where capacity and legitimacy determine external intervention rights. It is linked to 'absolute_sovereignty' and 'conditional_sovereignty' as sibling readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
