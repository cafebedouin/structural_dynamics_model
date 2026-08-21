% ============================================================================
% CONSTRAINT STORY: animal_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__welfare_reading, []).

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
 *   constraint_id: animal_status__welfare_reading
 *   human_readable: Animal Welfare as a Constraint on Human Use (Welfare Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'welfare reading' of animal status, where
 *   animals are recognized as sentient beings with interests that morally
 *   constrain, but do not prohibit, human instrumental use. It sits between
 *   the 'property reading' (animals as mere objects) and the 'abolitionist
 *   reading' (animals as rights-holders precluding all use). The constraint
 *   aims to mitigate suffering through welfare standards while maintaining
 *   the legitimacy of animal use. The claimed type is 'tangled_rope' because
 *   it genuinely coordinates human behavior to reduce harm (beneficiaries:
 *   animals, indirectly via reduced suffering; human users via social
 *   license) but also enables significant extraction (victims: animals,
 *   through continued use and suffering).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status__welfare_reading, 0.75).
domain_priors:theater_ratio(animal_status__welfare_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status__welfare_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(animal_status__welfare_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(animal_status__welfare_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__welfare_reading, "Animal Welfare as a Constraint on Human Use (Welfare Reading)").
narrative_ontology:topic_domain(animal_status__welfare_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, 'f5736011-07ca-48db-8c8e-a9081572b434').
narrative_ontology:cs_kernel_codification('f5736011-07ca-48db-8c8e-a9081572b434', formalized).
narrative_ontology:cs_authority_grounding('f5736011-07ca-48db-8c8e-a9081572b434', lineage).
narrative_ontology:cs_interpretation_layer_present('f5736011-07ca-48db-8c8e-a9081572b434').
narrative_ontology:cs_reading_relation('f5736011-07ca-48db-8c8e-a9081572b434', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f5736011-07ca-48db-8c8e-a9081572b434', animal_status__property_reading, coexists_with).
narrative_ontology:cs_axiom('f5736011-07ca-48db-8c8e-a9081572b434', foundational, animal_sentience_implies_moral_consideration).
narrative_ontology:cs_axiom_status(animal_sentience_implies_moral_consideration, holdable).
narrative_ontology:cs_axiom_grounding('f5736011-07ca-48db-8c8e-a9081572b434', animal_sentience_implies_moral_consideration, deontological).
narrative_ontology:cs_axiom('f5736011-07ca-48db-8c8e-a9081572b434', foundational, human_instrumental_use_is_permissible_with_mitigation).
narrative_ontology:cs_axiom_status(human_instrumental_use_is_permissible_with_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('f5736011-07ca-48db-8c8e-a9081572b434', human_instrumental_use_is_permissible_with_mitigation, conventional).
narrative_ontology:cs_reference_frame('f5736011-07ca-48db-8c8e-a9081572b434', utilitarian_harm_minimization).
narrative_ontology:cs_drift_state('f5736011-07ca-48db-8c8e-a9081572b434', contemporary_industrial_scale, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f5736011-07ca-48db-8c8e-a9081572b434', '').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, human_users_of_animals).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, biomedical_researchers).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, consumers_of_animal_products).
narrative_ontology:constraint_victim(animal_status__welfare_reading, animals_used_by_humans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_status__welfare_reading, consumers_of_animal_products).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the continued ability to use animals for food and products, operating within welfare regulations that are often shaped by industry lobbying. Bears costs of compliance but maintains profitable operations.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_agriculture_industry, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, animal_agriculture_industry, beneficiary).

% Relies on animal models for research, operating under strict ethical and welfare guidelines. Benefits from the permission to use animals, while bearing the costs of humane care and regulatory oversight.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, biomedical_researchers, beneficiary,
    organized, biographical, constrained, global).

% Encompasses a wide range of individuals and groups who benefit from animal products, services, or companionship, under the understanding that welfare standards are met. Their interests are prioritized over animals' interest in non-use.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, human_users_of_animals, beneficiary,
    powerful, biographical, mobile, global).

% Bear the direct costs of instrumental use, including confinement, manipulation, and eventual slaughter, even when welfare standards are met. Their interests in autonomy and life are overridden by human interests.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animals_used_by_humans, payer,
    powerless, immediate, trapped, universal).

% Work to improve welfare standards and enforce existing regulations. They operate within the framework of animal use, seeking to mitigate suffering rather than abolish use entirely. They influence policy and public opinion.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_welfare_advocates, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, animal_welfare_advocates, observer).

% Benefit from the availability and affordability of animal products, often with the assurance that welfare standards are in place. They indirectly pay for welfare compliance through product prices and bear the moral cost of animal use.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, consumers_of_animal_products, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, consumers_of_animal_products, payer).

% Advocate for the complete cessation of animal use, viewing animals as rights-holders. Their core premise (no instrumental use) is outside the welfare framework, making them structurally excluded from debates about welfare standards, though they exert external pressure.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__welfare_reading, animal_agriculture_industry).
narrative_ontology:fixing_cost_class(animal_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate human behavior regarding animal treatment, ensuring a minimum standard of care and preventing gratuitous cruelty, while allowing for the continued instrumental use of animals across various sectors.
% TRANSFER_FUNCTION: Transfers the burden of suffering (even mitigated) and loss of autonomy to animals, while transferring economic benefits, research advancements, and consumer goods to human users and industries.
% ABSENT_VOICES: Abolitionist advocates are excluded from the direct negotiation of welfare standards, as their premise challenges the legitimacy of use itself. Most critically, the animals themselves are absent, their interests represented by human advocates within the existing framework.
% DISAPPEARANCE_RATIONALE: If the constraint of animal welfare vanished overnight, the animal agriculture, biomedical research, and pet industries would face immediate and radical restructuring. Public outcry would be immense, and the moral landscape of human-animal relations would be fundamentally altered, leading to a complete reorganization of practices and legal frameworks.
% FOUNDING_PROBLEM: The problem of widespread, unmitigated cruelty and suffering inflicted upon animals by humans, leading to public moral discomfort and calls for ethical treatment.
% FOUNDING_PROBLEM_CORROBORATION: Veterinary associations, animal welfare scientists, and public opinion polls consistently attest to the ongoing need for and challenges in maintaining animal welfare standards, indicating the founding problem remains live. Independent ethical philosophers also corroborate the moral imperative to address animal suffering.
narrative_ontology:disappearance_verdict(animal_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__welfare_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__welfare_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(animal_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__welfare_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__welfare_reading_tests).
:- end_tests(animal_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.45) because animals still bear significant costs (loss of life, autonomy, suffering) even under welfare standards, and the system is designed to permit this use. Suppression is high (0.75) due to animals' inability to consent or resist, and the legal/economic structures that enforce their status as resources. Theater ratio is moderate (0.40) as some welfare regulations are genuinely enforced, but others serve more to legitimize continued use than to fundamentally alter animal experience. Accessibility collapse is moderate (0.60) as alternatives to animal products/research exist but are not universally adopted due to cost, convenience, or cultural factors. Resistance is high (0.70) from animal welfare and rights groups, who continually challenge the status quo.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human users and industries, this constraint is a necessary 'rope' that provides social license and coordinates ethical behavior. From the perspective of animals (as represented by advocates), it is a 'snare' or 'tangled rope' that legitimizes their continued exploitation, even with mitigated suffering. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Human users and industries are clear beneficiaries, gaining economic value and convenience from animal use. Animals are the primary targets/victims, bearing the costs of instrumentalization. Animal welfare advocates act as agenda-setters, pushing for better standards, but within the existing framework of use. Consumers are both beneficiaries (products) and indirect payers (higher prices for welfare-compliant goods). Abolitionist advocates are structurally excluded from the welfare debate, as their core premise challenges the entire framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Rope (ignoring the extraction from animals) or a pure Snare (ignoring the genuine coordination function of welfare standards). It acknowledges the dual nature: a genuine effort to coordinate human behavior to reduce suffering, intertwined with an extractive system that benefits humans at the expense of animals. The founding problem (unmitigated cruelty) is still live, but the solution (welfare standards) has become a mechanism for managing, rather than eliminating, extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately representing the ''welfare reading'' of animal status, distinct from ''property'' and ''abolitionist'' readings?',
    'Comparative analysis with legal texts, philosophical arguments, and advocacy positions of each reading to ensure structural fidelity.',
    'Misidentification would lead to incorrect classification and misattribution of beneficiaries/victims, distorting the analysis of the broader ''animal_status'' kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ensuring this constraint accurately captures the ''welfare reading'' within the ''animal_status'' kernel.').

omega_variable(
    welfare_effectiveness_vs_legitimation,
    'To what extent do animal welfare regulations genuinely reduce animal suffering versus primarily serving to legitimize continued instrumental use?',
    'Empirical studies comparing animal physiological and behavioral indicators under different welfare regimes, alongside sociological analysis of public perception and industry compliance.',
    'If welfare primarily legitimizes use, the extractiveness and theater_ratio would be higher, pushing the classification closer to a Snare. If genuinely effective, it reinforces the coordination aspect of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_effectiveness_vs_legitimation, empirical, 'Assessing the true impact of welfare regulations on animal experience versus their social function.').

omega_variable(
    sentience_definition_ambiguity,
    'How is ''sentience'' defined and measured in practice, and does this definition adequately capture the full range of animal interests?',
    'Ongoing scientific research in animal cognition and neurobiology, coupled with philosophical debate on the criteria for moral consideration.',
    'A broader definition of sentience and interests could increase the perceived extractiveness and suppression, as more animal experiences would be recognized as ''costs'' within the system. A narrower definition would reduce them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sentience_definition_ambiguity, conceptual, 'Ambiguity in the definition and scope of animal sentience and interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_status__welfare_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(anim_tr_t1985, animal_status__welfare_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(anim_tr_t2000, animal_status__welfare_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(anim_tr_t2010, animal_status__welfare_reading, theater_ratio, 2010, 0.39).
narrative_ontology:measurement(anim_tr_t2025, animal_status__welfare_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_status__welfare_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(anim_be_t1985, animal_status__welfare_reading, base_extractiveness, 1985, 0.4).
narrative_ontology:measurement(anim_be_t2000, animal_status__welfare_reading, base_extractiveness, 2000, 0.43).
narrative_ontology:measurement(anim_be_t2010, animal_status__welfare_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(anim_be_t2025, animal_status__welfare_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_status__welfare_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(anim_su_t1985, animal_status__welfare_reading, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(anim_su_t2000, animal_status__welfare_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(anim_su_t2010, animal_status__welfare_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(anim_su_t2025, animal_status__welfare_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
