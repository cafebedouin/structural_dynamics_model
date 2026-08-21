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
 *   human_readable: Animal Welfare Constraint (Welfare Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'welfare reading' of animal status, where
 *   animals are recognized as sentient beings with interests that constrain,
 *   but do not prohibit, human instrumental use. It is a Tangled Rope because
 *   it genuinely coordinates human behavior to reduce animal suffering
 *   (beneficiaries: animal welfare advocates, device users who prefer ethical
 *   products) while simultaneously enabling and legitimizing substantial
 *   extraction from animals (victims: animals in human use) through a system
 *   of regulated exploitation. Active enforcement is required to maintain
 *   welfare standards against economic pressures. The kernel is
 *   'animal_status', and this is the 'welfare_reading'.
 *
 * KEY AGENTS:
 *   - human_users_of_animals: Primary beneficiary (institutional/mobile)
 *   - animal_agriculture_industry: Beneficiary (organized/constrained)
 *   - biomedical_researchers: Beneficiary (organized/constrained)
 *   - animals_in_human_use: Primary target (powerless/trapped)
 *   - animal_welfare_advocates: Agenda setter/Beneficiary (organized/mobile)
 *   - abolitionist_activists: Excluded (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status__welfare_reading, 0.6).
domain_priors:theater_ratio(animal_status__welfare_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status__welfare_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(animal_status__welfare_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(animal_status__welfare_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__welfare_reading, "Animal Welfare Constraint (Welfare Reading)").
narrative_ontology:topic_domain(animal_status__welfare_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, '3eb86a52-22a5-4e4e-b8fb-98e72adcb803').
narrative_ontology:cs_kernel_codification('3eb86a52-22a5-4e4e-b8fb-98e72adcb803', formalized).
narrative_ontology:cs_authority_grounding('3eb86a52-22a5-4e4e-b8fb-98e72adcb803', lineage).
narrative_ontology:cs_interpretation_layer_present('3eb86a52-22a5-4e4e-b8fb-98e72adcb803').
narrative_ontology:cs_reading_relation('3eb86a52-22a5-4e4e-b8fb-98e72adcb803', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3eb86a52-22a5-4e4e-b8fb-98e72adcb803', animal_status__property_reading, coexists_with).
narrative_ontology:cs_axiom('3eb86a52-22a5-4e4e-b8fb-98e72adcb803', foundational, sentience_confers_moral_consideration).
narrative_ontology:cs_axiom_status(sentience_confers_moral_consideration, holdable).
narrative_ontology:cs_axiom_grounding('3eb86a52-22a5-4e4e-b8fb-98e72adcb803', sentience_confers_moral_consideration, deontological).
narrative_ontology:cs_axiom('3eb86a52-22a5-4e4e-b8fb-98e72adcb803', foundational, human_instrumental_use_is_permissible_with_constraints).
narrative_ontology:cs_axiom_status(human_instrumental_use_is_permissible_with_constraints, holdable).
narrative_ontology:cs_axiom_grounding('3eb86a52-22a5-4e4e-b8fb-98e72adcb803', human_instrumental_use_is_permissible_with_constraints, conventional).
narrative_ontology:cs_reference_frame('3eb86a52-22a5-4e4e-b8fb-98e72adcb803', regulated_instrumental_use_framework).
narrative_ontology:cs_drift_state('3eb86a52-22a5-4e4e-b8fb-98e72adcb803', contemporary_ethical_discourse, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('3eb86a52-22a5-4e4e-b8fb-98e72adcb803', '').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, human_users_of_animals).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, biomedical_researchers).
narrative_ontology:constraint_victim(animal_status__welfare_reading, animals_in_human_use).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, sentience_as_moral_consideration).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, human_dominion_with_responsibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the continued ability to use animals for food, research, entertainment, and companionship, provided basic welfare standards are met. This reading legitimizes their practices while imposing some costs.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, human_users_of_animals, beneficiary,
    institutional, generational, mobile, global).

% Operates within the framework, accepting welfare regulations as a cost of doing business, which in turn grants social license. Benefits from the constraint's non-prohibition of instrumental use.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_agriculture_industry, beneficiary,
    organized, biographical, constrained, global).

% Relies on animal models for research, operating under strict ethical guidelines and oversight. Benefits from the constraint's allowance of animal use under controlled conditions.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, biomedical_researchers, beneficiary,
    organized, biographical, constrained, global).

% Are the direct subjects of human use, experiencing the conditions imposed by the constraint. Their interests are considered to a degree, but their fundamental status as 'used' remains. They bear the costs of instrumentalization.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animals_in_human_use, payer,
    powerless, immediate, trapped, universal).

% Work within the framework to improve conditions for animals, pushing for stronger regulations and enforcement. They are both beneficiaries (of the constraint's existence) and payers (of the effort to maintain/improve it).
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_welfare_advocates, agenda_setter,
    organized, generational, mobile, global).

% Reject the premise of instrumental animal use entirely, viewing the welfare framework as legitimizing exploitation. They are excluded from the core negotiation of the constraint's terms, operating outside it to challenge its foundations.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, abolitionist_activists, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__welfare_reading, human_users_of_animals).
narrative_ontology:fixing_cost_class(animal_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human behavior regarding animal treatment by establishing a baseline of acceptable care and prohibiting gratuitous cruelty, allowing for widespread animal use while mitigating public moral outrage.
% TRANSFER_FUNCTION: Transfers the right to instrumentalize animals (for food, research, etc.) to human users, in exchange for a commitment to minimize suffering and provide basic care. The costs of this suffering are borne by the animals.
% ABSENT_VOICES: Abolitionist activists are largely excluded from the direct policy-making within this framework, as their core premise (no instrumental use) is outside the constraint's scope. Animals themselves, as non-linguistic beings, are also absent, their interests represented by advocates.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the moral landscape of animal use would become highly contested. Either a more permissive 'property' reading would dominate, leading to increased animal suffering, or an 'abolitionist' reading would gain ground, fundamentally altering human-animal relations and industries. The current equilibrium would collapse.
% FOUNDING_PROBLEM: The problem of widespread, unmitigated animal suffering in human-controlled environments, coupled with growing public moral concern about animal cruelty, without prohibiting instrumental use.
% FOUNDING_PROBLEM_CORROBORATION: Animal welfare organizations, ethicists, and a significant portion of the general public attest that the problem of animal suffering in human use remains live, requiring ongoing vigilance and improvement within the welfare framework. This is corroborated by public opinion surveys and the continued legislative efforts to strengthen animal welfare laws.
narrative_ontology:disappearance_verdict(animal_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__welfare_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__welfare_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is moderate-high (0.45) because while welfare protections exist, animals still endure significant suffering and loss of life for human benefit. Suppression (0.6) is necessary to maintain the framework against both those who would ignore welfare and those who advocate for full animal liberation. Theater ratio (0.2) is relatively low, as welfare enforcement is generally genuine, though sometimes under-resourced. The trend shows a slight decrease in extractiveness over time due to increasing welfare standards, but a rise in suppression as the framework faces challenges from both sides.
 *
 * PERSPECTIVAL GAP:
 *   Human users and industries perceive this as a necessary and balanced coordination mechanism, ensuring ethical practice while allowing for vital human activities. Animals, as the direct subjects, experience it as a system that mitigates but does not eliminate their suffering. Abolitionist activists view it as a legitimization of exploitation. The engine's classification as Tangled Rope captures this dual nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Human users and industries are beneficiaries (low d) as the constraint permits their activities while imposing manageable costs. Animals are clear targets (high d) as they bear the primary costs. Animal welfare advocates have a mixed directionality, benefiting from the constraint's existence but also bearing costs in its maintenance and improvement. Abolitionist activists are excluded, making them targets of the constraint's legitimizing function.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to reduce animal suffering while allowing use) remains live. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction from animals) or a pure Snare (ignoring genuine welfare improvements). It accurately captures the ongoing tension between coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_sentience,
    'Which species are covered by the ''sentient being'' axiom, and how is sentience empirically determined?',
    'Ongoing scientific research into animal cognition and neurobiology, leading to updated legal and ethical guidelines for species inclusion.',
    'Expanding the scope of sentience would increase the victim set and potentially raise the measured extractiveness and suppression, pushing the constraint closer to a Snare for newly included species. Narrowing it would have the opposite effect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_sentience, empirical, 'Uncertainty regarding the precise boundaries of sentience and moral consideration across species.').

omega_variable(
    welfare_vs_abolition_boundary,
    'At what point do welfare protections become so stringent that they effectively foreclose instrumental use, blurring the line with an abolitionist stance?',
    'Legal challenges and policy debates that test the economic viability of animal use under increasingly strict welfare standards. If the cost of compliance becomes prohibitive, it effectively becomes abolition.',
    'If welfare standards effectively foreclose instrumental use, the constraint would shift towards an Abolitionist reading, fundamentally altering its classification and beneficiary/victim structure. If they remain permissive, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_vs_abolition_boundary, conceptual, 'The conceptual boundary between robust welfare and de facto abolition.').

omega_variable(
    enforcement_effectiveness,
    'How effective is the active enforcement of welfare standards in practice, given economic pressures and varying regulatory oversight?',
    'Independent audits of animal facilities, empirical studies on compliance rates, and analysis of animal suffering indicators under current regulations.',
    'If enforcement is found to be consistently weak, the actual extractiveness from animals is higher than measured, and the ''coordination'' function is more theatrical, pushing the constraint closer to a Snare. Stronger enforcement would validate the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness, empirical, 'The gap between declared welfare standards and actual animal treatment due to enforcement limitations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_status__welfare_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(anim_tr_t1980, animal_status__welfare_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(anim_tr_t1990, animal_status__welfare_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(anim_tr_t2000, animal_status__welfare_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(anim_tr_t2010, animal_status__welfare_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(anim_tr_t2024, animal_status__welfare_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_status__welfare_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(anim_be_t1980, animal_status__welfare_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(anim_be_t1990, animal_status__welfare_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(anim_be_t2000, animal_status__welfare_reading, base_extractiveness, 2000, 0.46).
narrative_ontology:measurement(anim_be_t2010, animal_status__welfare_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(anim_be_t2024, animal_status__welfare_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_status__welfare_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(anim_su_t1980, animal_status__welfare_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(anim_su_t1990, animal_status__welfare_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(anim_su_t2000, animal_status__welfare_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(anim_su_t2010, animal_status__welfare_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(anim_su_t2024, animal_status__welfare_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'animal_status' kernel. The 'welfare_reading' focuses on mitigating suffering within instrumental use, distinct from the 'abolitionist_reading' (no use) and the 'property_reading' (unrestricted use).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
