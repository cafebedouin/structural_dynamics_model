% ============================================================================
% CONSTRAINT STORY: dual_use_complicity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_use_complicity, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dual_use_complicity
 *   human_readable: Dual-Use Complicity in Mathematical AI Training
 *   domain: science_policy/professional_ethics/technology_governance
 *
 * SUMMARY:
 *   The dual-use complicity constraint describes the degraded ethical review
 *   apparatus surrounding mathematical AI training. Mathematical
 *   theorem-proving datasets (Lean proofs, Coq formalizations, Metamath
 *   libraries) are used to train general-purpose reasoning models deployed in
 *   military targeting systems, mass surveillance infrastructure, and
 *   disinformation campaigns. The ethical review process that once provided
 *   meaningful gatekeeping (informed consent, use-case restrictions,
 *   downstream accountability) has atrophied into performative compliance:
 *   institutional review boards assess narrow technical criteria (data
 *   privacy, consent forms) while systematically ignoring the structural
 *   question of whether mathematical reasoning capabilities should be
 *   transferred to general-purpose AI at all. The constraint exhibits piton
 *   characteristics from most perspectives: high theater ratio (0.68),
 *   moderate extraction (0.35), and persistence through institutional inertia
 *   rather than functional necessity. The mathematical community's identity
 *   investment in value-neutrality ('mathematics is apolitical; we just prove
 *   theorems') prevents recognition that theorem-proving datasets are not
 *   neutral inputs but rather training data for systems with clear political
 *   and military applications. The constraint is maintained not because
 *   ethical review serves any gatekeeping function, but because abandoning it
 *   would require the mathematical community to confront its complicity in
 *   surveillance and warfare applications.
 *
 * KEY AGENTS:
 *   - Civilian Populations Under Surveillance: Primary victim (powerless/trapped) — bear the cost of surveillance systems trained on mathematical reasoning datasets; no exit from surveillance infrastructure
 *   - Individual Mathematician: Secondary victim (moderate/constrained) — faces career pressure to contribute to AI training datasets; aware that ethical review is theater but constrained by funding and publication incentives
 *   - AI Research Labs: Primary beneficiary (institutional/arbitrage) — extract value from mathematical datasets while using ethical review as legal cover; can arbitrage between jurisdictions
 *   - Professional Mathematical Societies: Mixed position (organized/mobile) — benefit from AI industry collaboration but bear reputational cost; coordinate through weak ethical guidelines
 *   - Mathematical Community Epistemic Integrity: Abstract victim (powerless/identity_locked) — the collective good of mathematics as an independent discipline; identity-locked by value-neutrality commitment
 *   - European AI Regulation Coalition: Organized reformer (organized/mobile) — building alternative governance pathways with sunset logic; sees current theater as temporary
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes the constraint as degraded institutional form persisting through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_use_complicity, 0.35).
domain_priors:suppression_score(dual_use_complicity, 0.42).
domain_priors:theater_ratio(dual_use_complicity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_use_complicity, extractiveness, 0.35).
narrative_ontology:constraint_metric(dual_use_complicity, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dual_use_complicity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_use_complicity, piton).
narrative_ontology:human_readable(dual_use_complicity, "Dual-Use Complicity in Mathematical AI Training").
narrative_ontology:topic_domain(dual_use_complicity, "science_policy/professional_ethics/technology_governance").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_use_complicity, defense_contractors).
narrative_ontology:constraint_beneficiary(dual_use_complicity, surveillance_technology_firms).
narrative_ontology:constraint_beneficiary(dual_use_complicity, ai_research_labs).
narrative_ontology:constraint_victim(dual_use_complicity, civilian_populations_under_surveillance).
narrative_ontology:constraint_victim(dual_use_complicity, democratic_institutions).
narrative_ontology:constraint_victim(dual_use_complicity, mathematical_community_epistemic_integrity).
narrative_ontology:constraint_vindicates(dual_use_complicity, value_neutrality_of_mathematical_research).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATIONS (PITON) — Trapped under surveillance systems trained on mathematical theorem-proving datasets. The 'ethical review' apparatus is performative: institutional review boards assess data privacy and consent for the training phase but cannot evaluate downstream deployment in mass surveillance. The constraint persists as theater — review rituals that once had gatekeeping function now rubber-stamp dual-use research while maintaining the appearance of ethical oversight.
constraint_indexing:constraint_classification(dual_use_complicity, piton,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIVIDUAL MATHEMATICIAN (PITON) — Constrained by career incentives (publication pressure, funding concentration in AI-adjacent work) but aware that ethical review is largely performative. Can choose not to contribute theorem-proving data to general-purpose AI training, but at significant career cost. Experiences the constraint as degraded professional ethics: the norms that once distinguished pure mathematics from applied weapons research have atrophied into checkbox compliance.
constraint_indexing:constraint_classification(dual_use_complicity, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AI RESEARCH LABS (ROPE) — Benefits from access to high-quality mathematical reasoning datasets. Experiences the constraint as coordination: ethical review processes provide legal cover and reputational protection while imposing minimal actual restriction on research direction. The labs can arbitrage between jurisdictions with different review standards and can frame any research as 'dual-use' to access both civilian and military funding.
constraint_indexing:constraint_classification(dual_use_complicity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROFESSIONAL SOCIETIES (TANGLED ROPE) — Organized agents (AMS, IMU, national math societies) benefit from AI industry funding and collaboration opportunities, but also bear reputational cost when mathematical work enables surveillance or autonomous weapons. The societies coordinate member behavior through ethical guidelines, but these guidelines have weak enforcement and are increasingly ignored. Mixed coordination (enabling interdisciplinary collaboration) and extraction (reputational damage, complicity in harmful applications).
constraint_indexing:constraint_classification(dual_use_complicity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: MATHEMATICAL EPISTEMIC INTEGRITY (SNARE) — The abstract collective good of mathematics as a discipline independent of military and commercial capture. Identity-locked because the community's self-concept is constituted through the belief that mathematical research is value-neutral and that theorem-proving is a pure intellectual activity with no inherent political valence. Cannot exit this framing without dissolving the professional identity that distinguishes mathematicians from engineers. Experiences maximum extraction: the value-neutrality claim is used to launder complicity in surveillance and warfare applications.
constraint_indexing:constraint_classification(dual_use_complicity, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 6: EU AI REGULATION COALITION (SCAFFOLD) — Organized regulatory actors building alternative governance pathways (AI Act, dual-use export controls, mandatory impact assessments). Sees the current ethical review theater as temporary: new regulatory frameworks will impose binding restrictions on dual-use AI training and deployment. Sunset logic: as regulation matures, the performative review apparatus will be replaced by enforceable legal requirements with real penalties.
constraint_indexing:constraint_classification(dual_use_complicity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (PITON) — From a civilizational perspective, the ethical review apparatus for dual-use mathematical AI training is a degraded institutional form. The original function (preventing mathematicians from contributing to weapons research without informed consent) has atrophied. What remains is theater: review boards that assess narrow technical compliance while ignoring the structural question of whether mathematical reasoning datasets should be used to train general-purpose AI at all. The constraint persists through institutional inertia and the professional identity investment in value-neutrality, not because it serves any gatekeeping function.
constraint_indexing:constraint_classification(dual_use_complicity, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_use_complicity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dual_use_complicity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dual_use_complicity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(dual_use_complicity, TR),
    TR >= 0.70.

:- end_tests(dual_use_complicity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts from civilian populations (surveillance), democratic institutions (disinformation-enabling AI), and mathematical community integrity (reputational damage, complicity). But extraction is not as severe as a pure snare because some mathematicians can opt out (at career cost), and some jurisdictions are building regulatory alternatives. The value reflects real but not maximal extraction. Suppression (0.42): Moderate. Barriers include career incentives (funding concentration in AI-adjacent work, publication pressure), information asymmetry (mathematicians often unaware of downstream military deployment), and identity lock (value-neutrality commitment makes complicity unthinkable). But suppression is not total: organized actors (professional societies, regulatory coalitions) have exit paths, and individual mathematicians can refuse participation. Theater ratio (0.68): High. Ethical review for dual-use AI training is substantially performative. IRBs assess data privacy and consent forms but systematically avoid the structural question of whether mathematical reasoning datasets should train general-purpose AI. The review ritual persists because it provides legal cover for research labs and reputational protection for mathematicians, not because it imposes meaningful restrictions. The theater ratio has increased over the interval (0.35 in 2015 to 0.68 in 2025) as ethical review became routine and approval became automatic.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural phenomenon — ethical review of dual-use AI training — appears differently depending on the observer's position. Civilian populations and the mathematical epistemic integrity see a snare or piton: extraction with minimal coordination function, maintained through identity lock and information asymmetry. Individual mathematicians see a piton: degraded professional ethics maintained through career incentives. AI research labs see a rope: the ethical review process coordinates access to datasets while imposing minimal restriction. Professional societies see a tangled rope: genuine coordination (interdisciplinary collaboration) mixed with extraction (reputational damage). The regulatory coalition sees a scaffold: a temporary problem with a sunset as binding regulation replaces performative review. The analytical observer sees a piton: an institutional form whose original gatekeeping function has atrophied, persisting through inertia and identity investment rather than serving any real purpose. The perspectival gap reveals that 'ethical review' is not a single thing but a presheaf over observation sites: what it is depends on where you stand.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. AI research labs are primary beneficiaries with arbitrage exit — they experience low or negative effective extraction (the constraint subsidizes them). Civilian populations are primary victims with trapped exit — they experience maximum effective extraction. Individual mathematicians are secondary victims with constrained exit — they experience moderate extraction (career costs of opting out, but exit is possible). Professional societies are mixed: beneficiaries (industry funding, collaboration opportunities) and victims (reputational damage), with mobile exit — they experience low to moderate extraction. The mathematical epistemic integrity is a victim with identity_locked exit — it experiences high extraction because the identity frame (value-neutrality) prevents recognition of the extraction mechanism. The analytical observer recognizes the constraint as piton: the extraction is real but the primary feature is the atrophied function maintained as performance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that the piton classification is the dominant structural feature but not the only legitimate reading. The mandate (ethical oversight of dual-use research) has outlived its function: the review apparatus no longer gates anything, it merely provides legal cover and reputational protection. But from the beneficiary's perspective (AI labs), the constraint is a functioning coordination mechanism (rope). From the regulatory coalition's perspective, it is a temporary problem being solved (scaffold). From the victim's perspective (civilian populations, mathematical integrity), it is extraction maintained through identity lock (snare or piton). The mandatrophy is resolved not by choosing one type but by recognizing that the constraint's structural position determines which aspect is visible. The piton classification at the analytical level captures the civilizational-scale observation that the constraint's primary function has atrophied while the performance persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    value_neutrality_empirical_status,
    'Is the value-neutrality of mathematical research an empirical claim about the world or a normative commitment about how mathematics should be practiced?',
    'Historical analysis of mathematical research funding sources, deployment contexts, and epistemic closure around military applications; comparison with other ''pure'' sciences that abandoned neutrality claims (physics post-Manhattan Project)',
    'If empirical and false: the identity-locked perspective is cognitive capture, and the snare classification is correct. If normative: the constraint is a contested professional norm, not a natural law, and the mountain-adjacent framing is unjustified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(value_neutrality_empirical_status, conceptual, 'Whether mathematical value-neutrality is empirical or normative').

omega_variable(
    ethical_review_function_threshold,
    'At what adoption rate does ethical review transition from functional gatekeeping to performative compliance?',
    'Longitudinal study of IRB decisions on dual-use AI projects: approval rates, conditions imposed, downstream deployment tracking. Compare early-stage review (2015-2018) when boards had discretion vs current era (2023+) when approval is routine.',
    'If threshold < 50% adoption: current system is already piton. If threshold > 80%: system retains some gatekeeping function despite theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_review_function_threshold, empirical, 'Adoption threshold for ethical review theater transition').

omega_variable(
    regulatory_sunset_timeline,
    'Will binding AI regulation actually impose meaningful restrictions on dual-use training, or will it be captured during implementation?',
    'Track EU AI Act enforcement 2025-2030: penalty rates, exemptions granted, industry lobbying success. Compare to GDPR implementation trajectory (strong initial intent, weakened enforcement).',
    'If regulation is enforceable: scaffold perspective is structural, sunset is real. If captured: scaffold is aspirational, and the piton persists indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_sunset_timeline, empirical, 'Whether regulatory sunset will materialize or be captured').

omega_variable(
    mathematician_complicity_awareness,
    'Do mathematicians contributing theorem-proving data to AI training understand the downstream military and surveillance applications, or is the complicity structurally invisible?',
    'Survey of mathematicians involved in AI training data generation: awareness of deployment contexts, consent mechanisms, opt-out availability. Compare stated beliefs about use cases vs actual deployment contracts.',
    'If aware: complicity is knowing, and suppression is lower (constrained rather than trapped). If unaware: complicity is structural, and suppression is higher (information asymmetry is part of the extraction mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mathematician_complicity_awareness, empirical, 'Whether mathematician complicity is knowing or structurally invisible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_use_complicity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_use_theater_2015, dual_use_complicity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dual_use_theater_2017, dual_use_complicity, theater_ratio, 2, 0.48).
narrative_ontology:measurement(dual_use_theater_2019, dual_use_complicity, theater_ratio, 4, 0.58).
narrative_ontology:measurement(dual_use_theater_2021, dual_use_complicity, theater_ratio, 6, 0.65).
narrative_ontology:measurement(dual_use_theater_2023, dual_use_complicity, theater_ratio, 8, 0.68).
narrative_ontology:measurement(dual_use_theater_2025, dual_use_complicity, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(dual_use_extract_2015, dual_use_complicity, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(dual_use_extract_2017, dual_use_complicity, base_extractiveness, 2, 0.26).
narrative_ontology:measurement(dual_use_extract_2019, dual_use_complicity, base_extractiveness, 4, 0.3).
narrative_ontology:measurement(dual_use_extract_2021, dual_use_complicity, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(dual_use_extract_2023, dual_use_complicity, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(dual_use_extract_2025, dual_use_complicity, base_extractiveness, 10, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(dual_use_suppress_2015, dual_use_complicity, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(dual_use_suppress_2020, dual_use_complicity, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(dual_use_suppress_2025, dual_use_complicity, suppression_requirement, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_use_complicity, enforcement_mechanism).
narrative_ontology:affects_constraint(dual_use_complicity, ai_safety_research_capture).
narrative_ontology:affects_constraint(dual_use_complicity, open_source_ai_governance).
narrative_ontology:affects_constraint(dual_use_complicity, academic_military_funding_dependence).

% DUAL FORMULATION NOTE:
% The dual-use complicity constraint is part of a larger constraint family around AI governance and academic-military entanglement. Related constraints include AI safety research capture (where safety research is funded by the same labs building dual-use systems), open-source AI governance (where open-source norms conflict with dual-use restrictions), and academic-military funding dependence (where university research is structurally dependent on defense contracts). Each has its own extractiveness value reflecting different structural positions, but all are linked through the common mechanism of performative ethical review replacing functional gatekeeping.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_use_complicity, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
