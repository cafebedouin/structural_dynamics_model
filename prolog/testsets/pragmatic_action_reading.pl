% ============================================================================
% CONSTRAINT STORY: pragmatic_action_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pragmatic_action_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pragmatic_action_reading
 *   human_readable: Pragmatic Action-Guided Knowledge Legitimacy
 *   domain: epistemology/institutional/medical
 *
 * SUMMARY:
 *   This constraint instantiates the pragmatic action reading of knowledge
 *   legitimacy in biomedicine: claims are legitimate when they guide
 *   effective action, regardless of whether they have passed institutional
 *   validation. The reading shifts burden of proof based on intervention
 *   safety profile and opportunity cost of waiting. It is one of three
 *   sibling readings of the knowledge_legitimacy_biomedicine kernel; the
 *   others are institutional_validation_reading (legitimacy requires
 *   controlled trials and peer review) and synthesis_hypothesis_reading
 *   (legitimacy emerges from consilience across multiple evidence types).
 *   This reading has the lowest barrier to action and highest risk tolerance.
 *
 * KEY AGENTS:
 *   - individual_health_seekers: Primary beneficiaries (moderate/mobile) — act on personal observation and mechanistic reasoning without waiting for validation
 *   - alternative_practitioners: Beneficiaries (organized/mobile) — derive authority from client outcomes rather than institutional credentials
 *   - supplement_manufacturers: Beneficiaries (powerful/arbitrage) — market products under health claims without pre-market efficacy proof
 *   - institutional_researchers: Payers (institutional/constrained) — bear cost of maintaining validation infrastructure this reading treats as optional
 *   - patients_with_serious_conditions: Payers (powerless/trapped) — bear opportunity cost when action-guided legitimacy leads away from validated interventions
 *   - public_health_authorities: Observers (institutional/analytical) — monitor population outcomes and attempt to distinguish real effects from placebo
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pragmatic_action_reading, 0.42).
domain_priors:suppression_score(pragmatic_action_reading, 0.38).
domain_priors:theater_ratio(pragmatic_action_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pragmatic_action_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(pragmatic_action_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(pragmatic_action_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(pragmatic_action_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(pragmatic_action_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pragmatic_action_reading, rope).
narrative_ontology:human_readable(pragmatic_action_reading, "Pragmatic Action-Guided Knowledge Legitimacy").
narrative_ontology:topic_domain(pragmatic_action_reading, "epistemology/institutional/medical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(pragmatic_action_reading, '2c7ad2fb-311c-4a72-9faf-a4fa58eeb04d').
narrative_ontology:cs_kernel_codification('2c7ad2fb-311c-4a72-9faf-a4fa58eeb04d', distributed).
narrative_ontology:cs_authority_grounding('2c7ad2fb-311c-4a72-9faf-a4fa58eeb04d', practice).
narrative_ontology:cs_interpretation_layer_present('2c7ad2fb-311c-4a72-9faf-a4fa58eeb04d').
narrative_ontology:cs_reading_relation('2c7ad2fb-311c-4a72-9faf-a4fa58eeb04d', knowledge_legitimacy_biomedicine__institutional_validation_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c7ad2fb-311c-4a72-9faf-a4fa58eeb04d', knowledge_legitimacy_biomedicine__synthesis_hypothesis_reading, coexists_with).
narrative_ontology:cs_axiom('2c7ad2fb-311c-4a72-9faf-a4fa58eeb04d', foundational, action_guidance_suffices_for_legitimacy).
narrative_ontology:cs_axiom_status(action_guidance_suffices_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2c7ad2fb-311c-4a72-9faf-a4fa58eeb04d', action_guidance_suffices_for_legitimacy, instrumental).
narrative_ontology:cs_axiom('2c7ad2fb-311c-4a72-9faf-a4fa58eeb04d', foundational, validation_pathway_irrelevant_to_legitimacy).
narrative_ontology:cs_axiom_status(validation_pathway_irrelevant_to_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2c7ad2fb-311c-4a72-9faf-a4fa58eeb04d', validation_pathway_irrelevant_to_legitimacy, conventional).
narrative_ontology:cs_reference_frame('2c7ad2fb-311c-4a72-9faf-a4fa58eeb04d', individual_experiential_authority).
narrative_ontology:cs_drift_state('2c7ad2fb-311c-4a72-9faf-a4fa58eeb04d', contemporary_supplement_market, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2c7ad2fb-311c-4a72-9faf-a4fa58eeb04d', '').
narrative_ontology:cs_kernel_id(pragmatic_action_reading, knowledge_legitimacy_biomedicine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pragmatic_action_reading, individual_health_seekers).
narrative_ontology:constraint_beneficiary(pragmatic_action_reading, alternative_practitioners).
narrative_ontology:constraint_beneficiary(pragmatic_action_reading, supplement_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(pragmatic_action_reading, institutional_researchers).
narrative_ontology:constraint_victim(pragmatic_action_reading, patients_with_serious_conditions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Navigate health decisions by testing interventions directly against their own outcomes. They treat subjective improvement as sufficient warrant for action, bypassing institutional validation when opportunity cost of waiting is high or when conventional medicine offers no solution. Exit is straightforward—stop the intervention if it doesn't work.
narrative_ontology:constraint_stakeholder(pragmatic_action_reading, individual_health_seekers, beneficiary,
    moderate, biographical, mobile, global).

% Offer interventions grounded in traditional knowledge, clinical experience, or mechanistic reasoning that has not passed institutional validation. They argue effectiveness in practice legitimates the knowledge claim regardless of validation pathway. Their authority derives from client outcomes and testimonials rather than institutional credentialing.
narrative_ontology:constraint_stakeholder(pragmatic_action_reading, alternative_practitioners, beneficiary,
    organized, biographical, mobile, regional).

% Market products under health claims that do not require pre-market efficacy proof. They benefit from the pragmatic legitimacy frame because it allows market entry based on mechanistic plausibility and consumer testimony rather than controlled trials. Regulatory arbitrage across jurisdictions with different validation requirements is straightforward.
narrative_ontology:constraint_stakeholder(pragmatic_action_reading, supplement_manufacturers, beneficiary,
    powerful, biographical, arbitrage, global).

% Bear the cost of maintaining validation infrastructure that this reading treats as optional. When pragmatic action claims proliferate without validation, the epistemic commons degrades—signal-to-noise ratio drops, placebo effects are mistaken for real effects, and research funding must compete with unvalidated claims that promise faster results.
narrative_ontology:constraint_stakeholder(pragmatic_action_reading, institutional_researchers, payer,
    institutional, generational, constrained, global).

% Face high-stakes decisions where pragmatic action without validation can mean choosing ineffective treatments over effective ones. They bear the opportunity cost when action-guided legitimacy leads them away from validated interventions. Exit is constrained by urgency, information asymmetry, and the psychological appeal of agency over waiting.
narrative_ontology:constraint_stakeholder(pragmatic_action_reading, patients_with_serious_conditions, payer,
    powerless, immediate, trapped, local).

% Monitor population-level outcomes and attempt to distinguish effective interventions from placebo, regression to mean, and spontaneous remission. They see the pragmatic legitimacy frame as lowering the barrier to harmful interventions while making it harder to establish what actually works at scale.
narrative_ontology:constraint_stakeholder(pragmatic_action_reading, public_health_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of knowledge paralysis when institutional validation is slow, expensive, or unavailable: individuals can act on mechanistic reasoning and personal observation rather than waiting for formal proof.
% TRANSFER_FUNCTION: Transfers epistemic authority from institutional validators to individual actors and practitioners; transfers market access from validated-only interventions to any intervention with plausible mechanism and testimonial support.
% ABSENT_VOICES: Future patients who will face degraded epistemic commons where validated and unvalidated claims are indistinguishable; researchers whose validation work is devalued when action-without-validation becomes the norm.
% DISAPPEARANCE_RATIONALE: If this legitimacy frame disappeared, individuals would wait for institutional validation before acting on health claims, alternative practitioners would lose market access, supplement manufacturers would face pre-market efficacy requirements, and the pace of health-seeking behavior would slow dramatically while validation infrastructure expanded.
% FOUNDING_PROBLEM: Institutional validation is too slow and expensive for many health decisions; individuals facing urgent or chronic conditions cannot wait years for controlled trials; traditional and experiential knowledge would be lost if validation were the only path to legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Individual health seekers attest the problem is live when facing conditions with no validated treatment or long validation timelines. Public health authorities and institutional researchers attest the problem is partly solved by expedited approval pathways and that the pragmatic frame now operates beyond its founding justification, enabling market access for interventions that could be validated but aren't.
narrative_ontology:disappearance_verdict(pragmatic_action_reading, world_rearranges).
narrative_ontology:founding_problem_status(pragmatic_action_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(pragmatic_action_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-18',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(pragmatic_action_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pragmatic_action_reading_tests).
:- end_tests(pragmatic_action_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) because the reading enables market access for unvalidated interventions, transferring resources from validated to unvalidated claims; but it also solves a real coordination problem (knowledge paralysis when validation is slow). Suppression is low-moderate (0.38) because the reading does not actively suppress alternatives—individuals can still choose validated interventions—but it does create structural pressure against waiting for validation. Theater ratio is low-moderate (0.28) because mechanistic reasoning and testimonial evidence are real epistemic inputs, but a growing share of pragmatic legitimacy claims rest on placebo effects and regression to mean rather than causal efficacy. Accessibility collapse is low (0.35) because validated alternatives remain available; resistance is moderate-high (0.58) because institutional validators actively contest this legitimacy frame.
 *
 * PERSPECTIVAL GAP:
 *   From the individual health seeker seat, this constraint is genuine coordination—it solves knowledge paralysis and enables action when institutional validation is unavailable or too slow. From the institutional researcher seat, the same structure operates as extraction—it enables free-riding on validation infrastructure while degrading the epistemic commons. From the trapped patient seat, it operates as a snare—the promise of agency masks the opportunity cost of choosing ineffective treatments. The engine computes these divergent classifications from the structural data; the claimed type (rope) reflects the beneficiary-seat framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual health seekers and alternative practitioners are structural beneficiaries (low d, near 0.2-0.3): they gain agency and market access from the pragmatic frame. Supplement manufacturers are also beneficiaries but with higher power and arbitrage exit (d near 0.15). Institutional researchers are payers (d near 0.6): the pragmatic frame devalues their validation work and degrades the epistemic commons they maintain. Patients with serious conditions are also payers despite being powerless (d near 0.75): they bear the opportunity cost of choosing unvalidated over validated interventions under urgency and information asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mandatrophy-resolved: the founding problem (validation too slow for urgent decisions) remains live for some cases, even as the pragmatic frame has expanded beyond its founding justification. The measurement series shows modest extraction accumulation as the reading is applied to interventions that could be validated but aren't, and as supplement manufacturers use it for regulatory arbitrage. The theater ratio rises as more pragmatic legitimacy claims rest on placebo rather than causal efficacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    placebo_vs_causal_efficacy,
    'What fraction of pragmatically legitimate interventions work through causal mechanisms versus placebo effects, regression to mean, or spontaneous remission?',
    'Systematic comparison of pragmatically adopted interventions against placebo controls in delayed validation studies; meta-analysis of interventions that entered practice via pragmatic legitimacy and were later formally tested.',
    'If most pragmatically legitimate interventions fail controlled trials, the reading operates primarily as extraction (market access for ineffective treatments). If most pass, the reading is genuine coordination (faster access to effective interventions). The ratio determines whether this is rope or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(placebo_vs_causal_efficacy, empirical, 'Whether pragmatic legitimacy tracks causal efficacy or placebo effects.').

omega_variable(
    opportunity_cost_asymmetry,
    'Does the pragmatic frame impose higher opportunity costs on powerless patients (who choose unvalidated over validated treatments under urgency) than it saves for mobile health seekers (who access effective interventions faster)?',
    'Cohort studies comparing health outcomes for patients who adopted pragmatically legitimate interventions versus those who waited for validation, stratified by power and exit options.',
    'If opportunity costs concentrate on trapped patients while benefits accrue to mobile actors, the constraint is extractive despite solving a real coordination problem. If benefits are broadly distributed, it is genuine coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(opportunity_cost_asymmetry, empirical, 'Whether the pragmatic frame''s costs and benefits are symmetrically distributed.').

omega_variable(
    kernel_reading_under_determination,
    'Is the pragmatic action reading the only coherent framing of action-guided legitimacy, or does the institutional validation reading capture the same epistemic warrant with different emphasis?',
    'Philosophical analysis of whether ''guides effective action'' and ''passes institutional validation'' are distinct epistemic criteria or different descriptions of the same underlying warrant (predictive reliability).',
    'If the readings are distinct criteria, they produce genuinely different constraints. If they are different descriptions of the same criterion, the kernel is under-determined and the reading distinction is observer-relative rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Whether the pragmatic and institutional readings are structurally distinct or framings of the same criterion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pragmatic_action_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prag_tr_t0, pragmatic_action_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(prag_tr_t5, pragmatic_action_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(prag_tr_t10, pragmatic_action_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(prag_tr_t15, pragmatic_action_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(prag_tr_t20, pragmatic_action_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(prag_tr_t25, pragmatic_action_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(prag_be_t0, pragmatic_action_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(prag_be_t5, pragmatic_action_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(prag_be_t10, pragmatic_action_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(prag_be_t15, pragmatic_action_reading, base_extractiveness, 15, 0.39).
narrative_ontology:measurement(prag_be_t20, pragmatic_action_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(prag_be_t25, pragmatic_action_reading, base_extractiveness, 25, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(prag_su_t0, pragmatic_action_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(prag_su_t5, pragmatic_action_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(prag_su_t10, pragmatic_action_reading, suppression_requirement, 10, 0.31).
narrative_ontology:measurement(prag_su_t15, pragmatic_action_reading, suppression_requirement, 15, 0.34).
narrative_ontology:measurement(prag_su_t20, pragmatic_action_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(prag_su_t25, pragmatic_action_reading, suppression_requirement, 25, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(pragmatic_action_reading, institutional_validation_reading).
narrative_ontology:affects_constraint(pragmatic_action_reading, synthesis_hypothesis_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the knowledge_legitimacy_biomedicine kernel. The pragmatic_action_reading (this constraint) has the lowest barrier to action and highest risk tolerance. The institutional_validation_reading requires controlled trials and peer review before legitimacy. The synthesis_hypothesis_reading requires consilience across multiple evidence types. The readings coexist across different communities of practice and influence each other's resource availability (pragmatic legitimacy claims compete for attention and funding with validation-requiring claims) but do not logically foreclose one another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
