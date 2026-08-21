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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   domain: International Law / Political Philosophy / Global Governance
 *
 * SUMMARY:
 *   The doctrine of graduated sovereignty posits that a state's sovereignty
 *   is not absolute but exists on a spectrum, determined by its capacity to
 *   govern effectively and the legitimacy of its governance. This reading of
 *   Westphalian sovereignty allows external actors (primarily powerful states
 *   and international organizations) to classify states and justify
 *   interventions or conditional engagement based on these assessments. While
 *   framed as a means to promote stability and human rights, critics argue it
 *   functions as a snare, enabling neo-colonial extraction and undermining
 *   the self-determination of weaker states.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.65).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.75).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.65).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty Doctrine").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "International Law / Political Philosophy / Global Governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, '6990f46d-dfe1-4930-8c5d-bca43aba0072').
narrative_ontology:cs_kernel_codification('6990f46d-dfe1-4930-8c5d-bca43aba0072', formalized).
narrative_ontology:cs_authority_grounding('6990f46d-dfe1-4930-8c5d-bca43aba0072', extraction).
narrative_ontology:cs_interpretation_layer_present('6990f46d-dfe1-4930-8c5d-bca43aba0072').
narrative_ontology:cs_reading_relation('6990f46d-dfe1-4930-8c5d-bca43aba0072', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('6990f46d-dfe1-4930-8c5d-bca43aba0072', westphalian_sovereignty__conditional_sovereignty, influences).
narrative_ontology:cs_axiom('6990f46d-dfe1-4930-8c5d-bca43aba0072', foundational, sovereignty_is_contingent_on_state_capacity_and_legitimacy).
narrative_ontology:cs_axiom_status(sovereignty_is_contingent_on_state_capacity_and_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('6990f46d-dfe1-4930-8c5d-bca43aba0072', sovereignty_is_contingent_on_state_capacity_and_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('6990f46d-dfe1-4930-8c5d-bca43aba0072', foundational, external_actors_have_legitimacy_to_assess_and_intervene).
narrative_ontology:cs_axiom_status(external_actors_have_legitimacy_to_assess_and_intervene, holdable).
narrative_ontology:cs_axiom_grounding('6990f46d-dfe1-4930-8c5d-bca43aba0072', external_actors_have_legitimacy_to_assess_and_intervene, conventional).
narrative_ontology:cs_reference_frame('6990f46d-dfe1-4930-8c5d-bca43aba0072', post_cold_war_intervention_justification).
narrative_ontology:cs_drift_state('6990f46d-dfe1-4930-8c5d-bca43aba0072', contemporary_global_challenges, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6990f46d-dfe1-4930-8c5d-bca43aba0072', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, powerful_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_organizations_led_by_powerful_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, weak_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, states_with_low_governance_capacity).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__graduated_sovereignty, responsibility_to_protect_doctrine).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__graduated_sovereignty, good_governance_agenda).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states define the criteria for 'state capacity' and 'governance legitimacy,' and use these classifications to justify intervention or influence in weaker states. They gain geopolitical leverage and access to resources.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, powerful_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, powerful_states, beneficiary).

% Subject to external assessment of their sovereignty, often leading to interventions, conditional aid, or loss of control over domestic affairs. They bear the costs of external interference and reclassification, with limited ability to resist.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, weak_states, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, weak_states, excluded).

% These organizations operationalize the criteria for graduated sovereignty, conducting assessments and implementing interventions. They benefit from expanded mandates and influence, often aligning with the interests of their most powerful member states.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_organizations_led_by_powerful_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, international_organizations_led_by_powerful_states, beneficiary).

% Monitor and critique the application of graduated sovereignty, often highlighting its potential for abuse or its impact on human rights and self-determination. They seek to influence policy through advocacy and research.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, global_civil_society_advocates, observer,
    moderate, biographical, analytical, global).

% Argue for the unconditional nature of state sovereignty and view any external assessment or intervention as illegitimate. Their perspective is largely marginalized in the discourse surrounding graduated sovereignty, but they continue to resist its application.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, absolute_sovereignty_advocates, excluded,
    powerful, generational, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__graduated_sovereignty, powerful_states).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__graduated_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for external actors to justify intervention or influence in states deemed to have insufficient capacity or legitimacy, aiming to address internal instability or human rights concerns.
% TRANSFER_FUNCTION: Transfers discretion, authority, and often resources (e.g., control over development aid, security operations) from weak states to powerful states and international organizations.
% ABSENT_VOICES: States advocating for absolute, unconditional sovereignty are largely excluded from the policy-making and interpretive processes that define and apply graduated sovereignty. Their objections are framed as outdated or obstructionist.
% DISAPPEARANCE_RATIONALE: If the doctrine of graduated sovereignty vanished, the primary justification for many forms of external intervention and conditional engagement would disappear, forcing a fundamental re-evaluation of international norms regarding statehood and non-interference. Global power dynamics would shift significantly.
% FOUNDING_PROBLEM: How to address internal crises, state failure, or systematic human rights violations within states without violating the traditional principle of non-interference in domestic affairs, particularly in the post-Cold War era.
% FOUNDING_PROBLEM_CORROBORATION: Powerful states and some international legal scholars attest that the problem of state failure and its consequences remains live, necessitating frameworks like graduated sovereignty. Weak states and critical scholars, however, attest that the doctrine has become a tool for neo-colonial intervention, and the original problem is now a cover for extraction.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__graduated_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__graduated_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.65) is high because the doctrine enables powerful actors to impose their standards and extract concessions or influence from weaker states under the guise of 'capacity building' or 'legitimacy promotion.' Suppression (0.75) is also high, as weak states face significant pressure to conform to external classifications and interventions, with few viable alternatives. The theater ratio (0.40) reflects that while some efforts under this doctrine may genuinely aim at improving governance, a substantial portion of the activity is performative, serving to legitimize external control rather than foster genuine self-determination. The increasing trend in metrics over time reflects the hardening of this doctrine into a more extractive and suppressive mechanism.
 *
 * PERSPECTIVAL GAP:
 *   Powerful states and international organizations perceive graduated sovereignty as a necessary evolution of international law, allowing for responsible global governance. From the perspective of weak states, however, it is often experienced as a justification for interference, undermining their autonomy and perpetuating power imbalances. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerful states and the international organizations they lead are clear beneficiaries, gaining discretion and influence (low directionality). Weak states and those with low governance capacity are the primary targets, bearing the costs of intervention and loss of autonomy (high directionality). Advocates of absolute sovereignty are excluded, as their foundational premise is directly contradicted by this doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling external intervention as pure coordination or temporary support. By identifying the high extraction and suppression, it highlights that the 'coordination' story (improving governance) often serves as a cover for asymmetric power dynamics and resource transfer, rather than a neutral collective action solution. The contested founding problem status further supports this, indicating a potential drift from original intent to a more extractive function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classification_objectivity,
    'Are the criteria and assessments of state capacity and governance legitimacy applied objectively, or are they subject to political bias and strategic interests of powerful states?',
    'Independent, transparent audits of assessment methodologies and their application across diverse geopolitical contexts, with outcomes compared against self-reported state capacity metrics.',
    'If assessments are found to be consistently biased, it would further solidify the ''snare'' classification by demonstrating that the coordination story is primarily a cover for politically motivated extraction. If objective, it would lend more credence to the coordination function, potentially shifting the classification towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_objectivity, empirical, 'Assesses the impartiality of state capacity and legitimacy classifications.').

omega_variable(
    intervention_efficacy_and_intent,
    'Do interventions justified by graduated sovereignty consistently lead to improved governance and state capacity in target states, or do they primarily serve the geopolitical or economic interests of intervening powers?',
    'Longitudinal studies comparing governance outcomes in intervened vs. non-intervened but similarly situated states, controlling for external factors, and analyzing resource flows post-intervention.',
    'If interventions consistently fail to improve governance or disproportionately benefit intervening powers, it would reinforce the high extractiveness and snare classification. Evidence of consistent, positive, and self-sustaining governance improvements would challenge the snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_efficacy_and_intent, empirical, 'Evaluates the actual impact and underlying motivations of interventions under graduated sovereignty.').

omega_variable(
    sovereignty_redefinition_legitimacy,
    'Is the redefinition of sovereignty from an absolute right to a graduated spectrum a legitimate evolution of international law, or an illegitimate imposition by powerful actors on weaker ones?',
    'Analysis of the historical development of international law, the consent mechanisms for new norms, and the representation of diverse state interests in their formulation. This is a conceptual and preference-based question.',
    'If deemed an illegitimate imposition, it strengthens the argument for high suppression and extraction, as the very premise of the constraint is contested on grounds of fairness and consent. If seen as a legitimate evolution, it would temper the perceived extractiveness, framing it as a necessary adaptation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_redefinition_legitimacy, conceptual, 'Examines the normative legitimacy of re-conceptualizing state sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(west_tr_t1997, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1997, 0.3).
narrative_ontology:measurement(west_tr_t2004, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2004, 0.35).
narrative_ontology:measurement(west_tr_t2011, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2011, 0.38).
narrative_ontology:measurement(west_tr_t2018, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2018, 0.39).
narrative_ontology:measurement(west_tr_t2025, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(west_be_t1997, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1997, 0.55).
narrative_ontology:measurement(west_be_t2004, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2004, 0.6).
narrative_ontology:measurement(west_be_t2011, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2011, 0.63).
narrative_ontology:measurement(west_be_t2018, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2018, 0.64).
narrative_ontology:measurement(west_be_t2025, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(west_su_t1997, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1997, 0.65).
narrative_ontology:measurement(west_su_t2004, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2004, 0.7).
narrative_ontology:measurement(west_su_t2011, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2011, 0.73).
narrative_ontology:measurement(west_su_t2018, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2018, 0.74).
narrative_ontology:measurement(west_su_t2025, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__graduated_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, responsibility_to_protect_doctrine).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, international_humanitarian_law).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, international_development_aid_conditionalities).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'westphalian_sovereignty' kernel. It describes sovereignty as graduated based on state capacity and legitimacy, contrasting with absolute and conditional readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
