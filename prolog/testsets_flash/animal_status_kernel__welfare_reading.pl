% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__welfare_reading, []).

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
 *   constraint_id: animal_status_kernel__welfare_reading
 *   human_readable: Animal Welfare Obligations (Welfare Reading)
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint represents the 'welfare reading' of animal status, a
 *   position that acknowledges animal sentience and the moral relevance of
 *   suffering, but permits continued human use of animals provided that
 *   suffering is minimized through regulation. It retains animals' property
 *   status while imposing welfare obligations. This reading is one of three
 *   major interpretations of the 'animal_status_kernel', standing in contrast
 *   to the 'property_reading' (animals as mere property) and the
 *   'abolitionist_reading' (animals as moral persons with rights).
 *
 * KEY AGENTS:
 *   - animal_agriculture_industry: Beneficiary/Payer (institutional/constrained) — benefits from continued use, pays for welfare compliance.
 *   - biomedical_research_institutions: Beneficiary/Payer (institutional/constrained) — benefits from continued use, pays for welfare compliance.
 *   - pet_industry: Beneficiary/Payer (organized/constrained) — benefits from continued use, pays for welfare compliance.
 *   - animal_welfare_advocates: Agenda Setter/Observer (organized/constrained) — pushes for stronger regulations, monitors compliance.
 *   - farmed_animals: Victim (powerless/trapped) — bear the suffering, subject to use.
 *   - research_animals: Victim (powerless/trapped) — bear the suffering, subject to use.
 *   - companion_animals: Victim (powerless/constrained) — bear suffering from neglect/abuse, but benefit from legal protections.
 *   - general_public_consumers: Beneficiary (moderate/mobile) — benefits from animal products, indirectly pays for welfare costs.
 *   - abolitionist_activists: Excluded (organized/constrained) — fundamentally reject the premise of animal use, but are not part of the welfare-focused conversation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status_kernel__welfare_reading, 0.6).
domain_priors:theater_ratio(animal_status_kernel__welfare_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status_kernel__welfare_reading, "Animal Welfare Obligations (Welfare Reading)").
narrative_ontology:topic_domain(animal_status_kernel__welfare_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__welfare_reading, '37b931d1-40ef-4177-8a26-671adc7cdc88').
narrative_ontology:cs_kernel_codification('37b931d1-40ef-4177-8a26-671adc7cdc88', formalized).
narrative_ontology:cs_authority_grounding('37b931d1-40ef-4177-8a26-671adc7cdc88', lineage).
narrative_ontology:cs_interpretation_layer_present('37b931d1-40ef-4177-8a26-671adc7cdc88').
narrative_ontology:cs_reading_relation('37b931d1-40ef-4177-8a26-671adc7cdc88', animal_status_kernel__property_reading, influences).
narrative_ontology:cs_reading_relation('37b931d1-40ef-4177-8a26-671adc7cdc88', animal_status_kernel__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('37b931d1-40ef-4177-8a26-671adc7cdc88', foundational, animal_sentience_morally_relevant).
narrative_ontology:cs_axiom_status(animal_sentience_morally_relevant, holdable).
narrative_ontology:cs_axiom_grounding('37b931d1-40ef-4177-8a26-671adc7cdc88', animal_sentience_morally_relevant, deontological).
narrative_ontology:cs_axiom('37b931d1-40ef-4177-8a26-671adc7cdc88', foundational, regulated_use_ethically_permissible).
narrative_ontology:cs_axiom_status(regulated_use_ethically_permissible, holdable).
narrative_ontology:cs_axiom_grounding('37b931d1-40ef-4177-8a26-671adc7cdc88', regulated_use_ethically_permissible, conventional).
narrative_ontology:cs_reference_frame('37b931d1-40ef-4177-8a26-671adc7cdc88', regulated_use_with_welfare_standards).
narrative_ontology:cs_drift_state('37b931d1-40ef-4177-8a26-671adc7cdc88', contemporary_ethical_discourse, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('37b931d1-40ef-4177-8a26-671adc7cdc88', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__welfare_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, pet_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, general_public_consumers).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, research_animals).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, companion_animals).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__welfare_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_status_kernel__welfare_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__welfare_reading_tests).
:- end_tests(animal_status_kernel__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates (minimizing suffering, establishing welfare standards) while simultaneously enabling asymmetric extraction (continued animal use for human benefit). Extractiveness is moderate (0.45) because welfare regulations impose real costs on industries, but the fundamental 'use' remains. Suppression (0.6) is present as the legal framework actively suppresses alternatives like abolitionist claims by legitimizing regulated use. Theater ratio is low (0.2) as there is genuine effort towards welfare, though sometimes performative. The metrics reflect the ongoing tension between moral concern and economic utility.
 *
 * PERSPECTIVAL GAP:
 *   The animal agriculture and research industries experience this as a necessary regulatory burden (payer seat) that allows them to continue their operations (beneficiary seat). Animal welfare advocates see it as a crucial, albeit imperfect, step towards reducing suffering (agenda setter/beneficiary). Abolitionist activists, however, view it as a Snare, arguing that welfare reforms merely make exploitation more palatable, thus suppressing true liberation.
 *
 * DIRECTIONALITY LOGIC:
 *   Industries using animals are beneficiaries (d near 0.0) as the constraint permits their continued operation, even with compliance costs. Animals themselves are victims (d near 1.0) as they bear the suffering inherent in use, despite minimization efforts. Animal welfare advocates are complex: they benefit from the constraint's existence (d near 0.2) as it aligns with their goals, but also bear the cost of constant advocacy and enforcement. The general public benefits from access to animal products (d near 0.1) and bears diffuse costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as the 'mandate' (minimizing suffering while permitting use) is actively contested and maintained. The tension between the founding problem (unregulated animal suffering) and its current status (regulated but ongoing suffering) is central to its operation. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring genuine welfare efforts).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''welfare'' reading of animal status, or does it primarily serve to legitimize continued animal use?',
    'Analysis of enforcement outcomes: if welfare improvements are consistently minimal or unenforced, it leans towards legitimization; if substantial, it supports genuine welfare intent.',
    'If primarily legitimization, the effective extractiveness is higher, and the constraint leans more towards a Snare, as the coordination (welfare) is cover for extraction (use).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity between genuine welfare and legitimization of use.').

omega_variable(
    welfare_vs_property_status,
    'To what extent does the retained property status of animals undermine the moral relevance of their suffering?',
    'Legal analysis of court rulings: if property rights consistently override welfare obligations in practice, the moral relevance is effectively nullified.',
    'If property status consistently overrides, the constraint''s suppression of animal interests is higher, and its classification shifts closer to the ''property_reading'' (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_vs_property_status, empirical, 'Tension between property status and welfare obligations.').

omega_variable(
    new_welfarism_critique,
    'Does the ''welfare reading'' inadvertently make the public more comfortable with animal exploitation by creating an illusion of ethical treatment (''happy meat'')?',
    'Sociological studies on consumer behavior and public perception of animal products under welfare regulations.',
    'If ''new welfarism'' is confirmed, the constraint''s effective suppression of abolitionist alternatives is higher, as it dampens public resistance to animal use.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(new_welfarism_critique, empirical, 'Critique that welfare reforms legitimize exploitation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__welfare_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__welfare_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(anim_tr_t10, animal_status_kernel__welfare_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(anim_tr_t20, animal_status_kernel__welfare_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__welfare_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(anim_be_t10, animal_status_kernel__welfare_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(anim_be_t20, animal_status_kernel__welfare_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__welfare_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(anim_su_t10, animal_status_kernel__welfare_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(anim_su_t20, animal_status_kernel__welfare_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__welfare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'animal_status_kernel'. It is linked to the 'property_reading' and 'abolitionist_reading' as part of a constraint family that interprets the moral and legal status of animals.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
