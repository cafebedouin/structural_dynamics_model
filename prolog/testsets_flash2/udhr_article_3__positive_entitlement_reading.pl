% ============================================================================
% CONSTRAINT STORY: udhr_article_3__positive_entitlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__positive_entitlement_reading, []).

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
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: UDHR Article 3: Positive Entitlement to Material Conditions
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'positive entitlement' reading of Article
 *   3 of the Universal Declaration of Human Rights (UDHR), which interprets
 *   'life and security' as obligating states to provide material conditions
 *   such as welfare, healthcare, and housing. This reading leads to high
 *   extraction from property owners and taxpayers to fund state provisions,
 *   and significant suppression of alternative economic liberties. It is
 *   claimed as a Tangled Rope due to its genuine coordination function
 *   (social stability) coupled with asymmetric extraction and active
 *   enforcement.
 *
 * KEY AGENTS:
 *   - vulnerable_citizens: Primary beneficiary (powerless/trapped) — receives state provisions.
 *   - social_justice_advocates: Secondary beneficiary (organized/constrained) — benefits from legal grounding.
 *   - state_governments: Agenda setter (institutional/constrained) — implements and enforces provisions.
 *   - property_owners: Primary payer (powerful/constrained) — bears costs of redistribution.
 *   - individual_libertarians: Payer (moderate/mobile) — opposes state intervention, bears costs.
 *   - taxpayers: Payer (organized/constrained) — bears collective financial burden.
 *   - negative_liberty_advocates: Excluded (organized/constrained) — marginalized by this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.85).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.75).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "UDHR Article 3: Positive Entitlement to Material Conditions").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, 'b6347dd0-b639-4572-bb19-3722b1f27b40').
narrative_ontology:cs_kernel_codification('b6347dd0-b639-4572-bb19-3722b1f27b40', fixed_text).
narrative_ontology:cs_authority_grounding('b6347dd0-b639-4572-bb19-3722b1f27b40', lineage).
narrative_ontology:cs_interpretation_layer_present('b6347dd0-b639-4572-bb19-3722b1f27b40').
narrative_ontology:cs_reading_relation('b6347dd0-b639-4572-bb19-3722b1f27b40', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6347dd0-b639-4572-bb19-3722b1f27b40', udhr_article_3__procedural_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('b6347dd0-b639-4572-bb19-3722b1f27b40', foundational, state_has_positive_obligations).
narrative_ontology:cs_axiom_status(state_has_positive_obligations, holdable).
narrative_ontology:cs_axiom_grounding('b6347dd0-b639-4572-bb19-3722b1f27b40', state_has_positive_obligations, deontological).
narrative_ontology:cs_axiom('b6347dd0-b639-4572-bb19-3722b1f27b40', foundational, material_conditions_are_prerequisite_to_liberty).
narrative_ontology:cs_axiom_status(material_conditions_are_prerequisite_to_liberty, holdable).
narrative_ontology:cs_axiom_grounding('b6347dd0-b639-4572-bb19-3722b1f27b40', material_conditions_are_prerequisite_to_liberty, empirically_contingent).
narrative_ontology:cs_reference_frame('b6347dd0-b639-4572-bb19-3722b1f27b40', post_wwii_social_rights_consensus).
narrative_ontology:cs_drift_state('b6347dd0-b639-4572-bb19-3722b1f27b40', contemporary_neoliberal_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b6347dd0-b639-4572-bb19-3722b1f27b40', '').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, vulnerable_citizens).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, social_justice_advocates).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, property_owners).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, individual_libertarians).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives state-provided welfare, healthcare, and housing as a fundamental right, ensuring basic material conditions for life and security. Without these provisions, their life and security are directly threatened.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, vulnerable_citizens, beneficiary,
    powerless, immediate, trapped, national).

% Benefits from the legal and philosophical grounding this reading provides for their advocacy for state intervention to address inequality and ensure human dignity. Their work is legitimized by this interpretation.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, social_justice_advocates, beneficiary,
    organized, generational, constrained, global).

% Obligated to implement policies and allocate resources to provide material conditions. This involves taxation, regulation, and the creation of social programs. They face political resistance but are bound by the interpreted constitutional mandate.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, state_governments, agenda_setter,
    institutional, generational, constrained, national).

% Bears the costs of wealth redistribution and increased taxation necessary to fund state provisions. Their property rights are seen as secondary to the collective right to material conditions, leading to potential expropriation or heavy regulation.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, property_owners, payer,
    powerful, biographical, constrained, national).

% Opposes state-mandated provision as an infringement on individual liberty and property rights, viewing it as coercive. They bear the costs of compliance and taxation, and their philosophical framework is directly challenged by this reading.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, individual_libertarians, payer,
    moderate, biographical, mobile, national).

% Contributes financially to the state's provision of welfare, healthcare, and housing through various taxes. While some may benefit from these services, the primary role here is bearing the collective financial burden.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, taxpayers, payer,
    organized, immediate, constrained, national).

% Their interpretation of Article 3, focusing on freedom from state interference, is marginalized by this reading. They would argue against the expansive state role and the infringement on individual economic liberties.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, negative_liberty_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates societal resources and state action to ensure a baseline of material conditions (welfare, healthcare, housing) for all citizens, preventing extreme poverty and insecurity that could destabilize society.
% TRANSFER_FUNCTION: Transfers wealth and resources from property owners and taxpayers to vulnerable citizens through state-administered social programs and services.
% ABSENT_VOICES: Advocates for a purely negative liberty reading of Article 3 are structurally excluded from the policy-making process driven by this interpretation; they would argue for minimal state intervention and protection of individual economic freedoms.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, state obligations for welfare, healthcare, and housing would cease, leading to a collapse of social safety nets, increased poverty, and widespread insecurity for vulnerable populations. Society would reorganize around market-based provision, with significant social upheaval.
% FOUNDING_PROBLEM: The problem of widespread poverty, lack of access to basic necessities, and social instability arising from unchecked economic inequality and insufficient state protection for vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: Vulnerable citizens and social justice advocates attest that the problem of material insecurity is still live and requires state intervention. International human rights bodies and UN reports corroborate the ongoing need for positive state action to ensure basic living standards, from outside the directly benefiting parties.
narrative_ontology:disappearance_verdict(udhr_article_3__positive_entitlement_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__positive_entitlement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(udhr_article_3__positive_entitlement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__positive_entitlement_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__positive_entitlement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__positive_entitlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because this reading mandates significant wealth redistribution and state intervention, imposing substantial costs on property owners and taxpayers. Suppression (0.75) is also high, as the state actively enforces these obligations, often overriding individual economic liberties and property rights. The theater ratio (0.4) reflects that while genuine social welfare is provided, a portion of the enforcement and rhetoric serves to legitimize the expansive state power and resource transfer. The increasing trend in extractiveness and suppression over time reflects the growing scope of state obligations under this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable citizens, this is a vital Rope or even a Mountain, providing essential life-sustaining support. From the perspective of property owners and individual libertarians, it is a Snare, coercively extracting resources and infringing on fundamental freedoms. The state governments, as agenda setters, frame it as a necessary coordination mechanism for social stability, while also benefiting from expanded authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable citizens are full beneficiaries (d=0.0) as the constraint directly subsidizes their basic needs. Social justice advocates are also beneficiaries (d=0.1-0.2) as their mission is advanced. State governments are agenda setters (d=0.3-0.4), balancing coordination benefits with the costs of enforcement and political resistance. Property owners, individual libertarians, and taxpayers are targets (d=0.8-1.0), bearing the direct and indirect costs of redistribution and regulation. Negative liberty advocates are excluded, their alternative reading suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Snare by acknowledging the genuine coordination function of providing social stability and basic welfare. However, it also highlights the significant, actively enforced extraction that accompanies this coordination, preventing it from being mislabeled as a pure Rope. The 'live' status of the founding problem suggests no mandatrophy, but the 'contested' corroboration points to ongoing debate about the proportionality of the solution to the problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_entitlement,
    'What is the precise scope and limit of ''material conditions necessary for life and security''? Does it include basic food and shelter, or extend to education, internet access, and cultural participation?',
    'Judicial precedent, legislative definition, or international human rights committee interpretations that provide clear boundaries for state obligations.',
    'A broader scope would increase extractiveness and suppression, potentially shifting the classification closer to a Snare for payers. A narrower scope would reduce these, moving it closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_entitlement, conceptual, 'Ambiguity in the definition of ''material conditions'' and the extent of state obligation.').

omega_variable(
    economic_impact_proportionality,
    'Are the economic costs and infringements on property rights proportional to the social benefits derived from state provision of material conditions?',
    'Comprehensive economic impact assessments, cost-benefit analyses, and longitudinal studies comparing social outcomes in jurisdictions with different levels of state provision.',
    'If costs are found to be disproportionate, it would strengthen the argument for the constraint being more extractive than coordinative, potentially reclassifying it as a Snare. If benefits strongly outweigh costs, it would reinforce the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_proportionality, empirical, 'Debate over the economic efficiency and fairness of wealth redistribution for social welfare.').

omega_variable(
    alternative_provision_viability,
    'Are there viable alternative mechanisms (e.g., private charity, market-based solutions, mutual aid networks) that could achieve similar levels of material security without state coercion?',
    'Empirical studies of non-state welfare models, comparative analysis of different societal approaches to poverty and insecurity, and pilot programs for alternative systems.',
    'If viable alternatives exist, the suppression metric would be re-evaluated as higher than necessary, and the constraint''s coordination function would appear less essential, pushing it towards a Snare. If no viable alternatives exist, the state''s role is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_provision_viability, empirical, 'The existence and efficacy of non-state alternatives for providing material conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__positive_entitlement_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(udhr_tr_t1968, udhr_article_3__positive_entitlement_reading, theater_ratio, 1968, 0.25).
narrative_ontology:measurement(udhr_tr_t1988, udhr_article_3__positive_entitlement_reading, theater_ratio, 1988, 0.3).
narrative_ontology:measurement(udhr_tr_t2008, udhr_article_3__positive_entitlement_reading, theater_ratio, 2008, 0.35).
narrative_ontology:measurement(udhr_tr_t2024, udhr_article_3__positive_entitlement_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1948, 0.6).
narrative_ontology:measurement(udhr_be_t1968, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1968, 0.7).
narrative_ontology:measurement(udhr_be_t1988, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1988, 0.78).
narrative_ontology:measurement(udhr_be_t2008, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2008, 0.82).
narrative_ontology:measurement(udhr_be_t2024, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(udhr_su_t1968, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1968, 0.6).
narrative_ontology:measurement(udhr_su_t1988, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1988, 0.68).
narrative_ontology:measurement(udhr_su_t2008, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2008, 0.72).
narrative_ontology:measurement(udhr_su_t2024, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, resource_allocation).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_25__right_to_adequate_standard_of_living).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_22__right_to_social_security).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
