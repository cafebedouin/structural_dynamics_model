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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Animal Welfare as a Constraint on Human Use
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'welfare reading' of the 'animal_status'
 *   kernel, which posits that animals are sentient beings with interests that
 *   constrain, but do not prohibit, human use. It aims to minimize animal
 *   suffering within systems of instrumental use, such as agriculture and
 *   research. This reading stands in contrast to the 'abolitionist reading'
 *   (which rejects all instrumental use) and the 'property reading' (which
 *   treats animals as mere objects). The constraint is claimed as a Tangled
 *   Rope because it coordinates human activity around animal use while
 *   simultaneously extracting from animals, requiring active enforcement of
 *   welfare standards.
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
narrative_ontology:constraint_metric(animal_status__welfare_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__welfare_reading, "Animal Welfare as a Constraint on Human Use").
narrative_ontology:topic_domain(animal_status__welfare_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, '357aae8d-a5fe-4acb-b833-5b82ecc12f19').
narrative_ontology:cs_kernel_codification('357aae8d-a5fe-4acb-b833-5b82ecc12f19', formalized).
narrative_ontology:cs_authority_grounding('357aae8d-a5fe-4acb-b833-5b82ecc12f19', practice).
narrative_ontology:cs_interpretation_layer_present('357aae8d-a5fe-4acb-b833-5b82ecc12f19').
narrative_ontology:cs_reading_relation('357aae8d-a5fe-4acb-b833-5b82ecc12f19', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('357aae8d-a5fe-4acb-b833-5b82ecc12f19', animal_status__property_reading, forecloses).
narrative_ontology:cs_axiom('357aae8d-a5fe-4acb-b833-5b82ecc12f19', foundational, sentience_confers_moral_status).
narrative_ontology:cs_axiom_status(sentience_confers_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('357aae8d-a5fe-4acb-b833-5b82ecc12f19', sentience_confers_moral_status, deontological).
narrative_ontology:cs_axiom('357aae8d-a5fe-4acb-b833-5b82ecc12f19', foundational, human_interests_justify_instrumental_use_with_mitigation).
narrative_ontology:cs_axiom_status(human_interests_justify_instrumental_use_with_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('357aae8d-a5fe-4acb-b833-5b82ecc12f19', human_interests_justify_instrumental_use_with_mitigation, instrumental).
narrative_ontology:cs_reference_frame('357aae8d-a5fe-4acb-b833-5b82ecc12f19', utilitarian_harm_minimization_within_use).
narrative_ontology:cs_drift_state('357aae8d-a5fe-4acb-b833-5b82ecc12f19', contemporary_ethical_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('357aae8d-a5fe-4acb-b833-5b82ecc12f19', '').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, human_consumers).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, biomedical_researchers).
narrative_ontology:constraint_victim(animal_status__welfare_reading, sentient_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_status__welfare_reading, animal_welfare_advocates).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, sentience_as_moral_basis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the direct costs of instrumental use, including suffering, confinement, and death, even under welfare protections. They have no agency to consent or exit the arrangement.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, sentient_animals, payer,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_non_agent(animal_status__welfare_reading, sentient_animals).

% Benefit from access to animal products and services (food, medicine, entertainment) at a relatively low moral cost due to welfare assurances. They can choose alternatives but often do not, relying on the welfare framework to mitigate ethical concerns.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, human_consumers, beneficiary,
    moderate, biographical, mobile, global).

% Primary economic beneficiary of animal use, shaping welfare regulations to permit profitable practices. They bear compliance costs but pass them to consumers. Exit from animal-based production is costly due to capital investment.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_agriculture_industry, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, animal_agriculture_industry, beneficiary).

% Rely on animal models for research and benefit from the legal framework allowing this. They are subject to welfare regulations but can lobby for exemptions or specific protocols.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, biomedical_researchers, beneficiary,
    organized, biographical, constrained, global).

% Work within the existing legal and ethical framework to improve animal conditions. They bear emotional and financial costs of advocacy, constrained by the premise that instrumental use is permissible.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_welfare_advocates, payer,
    organized, generational, constrained, global).

% Reject the premise of animal use entirely, operating outside the welfare framework. They are often seen as radical and are structurally excluded from the mainstream conversation about welfare, as their core demand (no use) is foreclosed by the constraint's premise.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, abolitionist_activists, excluded,
    organized, civilizational, identity_locked, global).

% Enforce animal welfare laws and regulations, balancing industry interests with public concern. Their actions are constrained by political will, scientific understanding, and the legal framework that permits animal use.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, regulatory_bodies, agenda_setter,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__welfare_reading, animal_agriculture_industry).
narrative_ontology:fixing_cost_class(animal_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate human activities involving animals by establishing minimum standards for care and treatment, aiming to minimize suffering while permitting instrumental use.
% TRANSFER_FUNCTION: Transfers the burden of suffering (within legally defined limits) from human conscience to sentient animals, and transfers economic value from animals to human users and industries.
% ABSENT_VOICES: Abolitionist activists are largely excluded from the policy-making process, as their fundamental rejection of animal use is incompatible with the welfare framework's premise of permissible use. Sentient animals themselves, lacking agency, are also absent from the conversation.
% DISAPPEARANCE_RATIONALE: If animal welfare constraints vanished overnight, animal use would become unrestricted, leading to immense suffering, public outcry, and potentially the collapse of industries reliant on public acceptance. The moral and economic landscape would be fundamentally reorganized.
% FOUNDING_PROBLEM: To reconcile human interests in using animals for food, research, and other purposes with growing moral and scientific understanding of animal sentience and capacity for suffering.
% FOUNDING_PROBLEM_CORROBORATION: Animal welfare organizations, ethicists, and a significant portion of the public attest to the ongoing problem of animal suffering within human systems, indicating that the founding problem remains live. Scientific advancements in animal cognition and sentience further corroborate this.
narrative_ontology:disappearance_verdict(animal_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__welfare_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__welfare_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Base extractiveness (0.45) reflects the inherent cost borne by animals through instrumental use, even with welfare protections. Suppression (0.75) is high because animals lack agency to resist, and their interests are systematically subordinated to human interests. Theater ratio (0.40) indicates that while some welfare regulations are effective, others are performative or minimally enforced, serving to legitimize use rather than fundamentally alter animal conditions. Accessibility collapse (0.60) is moderate; alternatives exist but are not universally adopted. Resistance (0.60) is substantial due to ongoing advocacy from animal rights movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human users and industries, this constraint is a necessary coordination mechanism that allows for ethical animal use. From the perspective of sentient animals, it is a system that legitimizes their exploitation, albeit with some mitigation. Animal welfare advocates see it as a continuous struggle for improvement, while abolitionist activists view it as a fundamental moral compromise.
 *
 * DIRECTIONALITY LOGIC:
 *   Sentient animals are the primary targets (payers) of this constraint, bearing the direct costs. Human consumers, the animal agriculture industry, and biomedical researchers are beneficiaries, gaining access to animal products/services with a mitigated moral burden. Regulatory bodies and animal welfare advocates act as agenda-setters or payers, respectively, working within the framework. Abolitionist activists are excluded, as their core premise is outside this reading's scope.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint truly a distinct ''welfare_reading'' of the ''animal_status'' kernel, or is it merely a less extreme ''property_reading'' or a compromised ''abolitionist_reading''?',
    'Analysis of core axioms and their practical implications: if the constraint''s foundational axioms consistently permit instrumental use while requiring significant welfare consideration, it is a distinct welfare reading.',
    'If reclassified as a property reading, extractiveness would be higher and suppression lower; if as an abolitionist reading, the entire framework of use would be challenged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the distinct identity of the welfare reading within the animal status kernel.').

omega_variable(
    welfare_regulation_effectiveness,
    'To what extent do current animal welfare regulations genuinely mitigate suffering versus merely legitimizing instrumental use?',
    'Empirical studies comparing animal welfare outcomes under different regulatory regimes, and independent audits of compliance versus self-reporting by industry.',
    'If regulations are found to be largely performative, the theater_ratio would increase, and the effective extractiveness from animals would be higher than currently measured, pushing the constraint closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_regulation_effectiveness, empirical, 'Assesses the real-world impact of welfare regulations beyond their stated intent.').

omega_variable(
    boundary_of_necessary_harm,
    'Where is the ethical boundary between ''necessary'' harm (justified by human interests within the welfare framework) and ''gratuitous'' harm (unjustified)?',
    'Ongoing ethical deliberation, scientific advancements in understanding animal needs, and public consensus shifts. Legal challenges and court rulings also refine this boundary.',
    'A shift in this boundary would directly impact the scope of permissible animal use and the stringency of welfare requirements, altering the base extractiveness and suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_of_necessary_harm, conceptual, 'Defines the permissible limits of harm within the welfare framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 1970, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_status__welfare_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(anim_tr_t1980, animal_status__welfare_reading, theater_ratio, 1980, 0.33).
narrative_ontology:measurement(anim_tr_t1990, animal_status__welfare_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(anim_tr_t2000, animal_status__welfare_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(anim_tr_t2010, animal_status__welfare_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(anim_tr_t2020, animal_status__welfare_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(anim_tr_t2030, animal_status__welfare_reading, theater_ratio, 2030, 0.42).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_status__welfare_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(anim_be_t1980, animal_status__welfare_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement(anim_be_t1990, animal_status__welfare_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(anim_be_t2000, animal_status__welfare_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(anim_be_t2010, animal_status__welfare_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(anim_be_t2020, animal_status__welfare_reading, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(anim_be_t2030, animal_status__welfare_reading, base_extractiveness, 2030, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_status__welfare_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(anim_su_t1980, animal_status__welfare_reading, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement(anim_su_t1990, animal_status__welfare_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(anim_su_t2000, animal_status__welfare_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(anim_su_t2010, animal_status__welfare_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(anim_su_t2020, animal_status__welfare_reading, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement(anim_su_t2030, animal_status__welfare_reading, suppression_requirement, 2030, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'animal_status' kernel, each representing a distinct structural claim about the moral status of animals and the permissibility of human use. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
