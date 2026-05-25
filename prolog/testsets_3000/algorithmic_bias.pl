% ============================================================================
% CONSTRAINT STORY: algorithmic_bias
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_bias, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: algorithmic_bias
 *   human_readable: Algorithmic Bias in Machine Learning Systems
 *   domain: technological/social
 *
 * SUMMARY:
 *   Algorithmic bias represents a structural constraint where machine
 *   learning systems automate and amplify historical inequities embedded in
 *   training data. The constraint operates across multiple institutional
 *   layers: technology companies benefit from scalable decision-making
 *   systems that reduce operational costs; marginalized populations bear
 *   concentrated harms through discriminatory decisions in lending, hiring,
 *   criminal justice, and social services; civil rights advocates face
 *   resource and access barriers to auditing; regulators attempt to impose
 *   transparency and fairness standards; and the academic fairness research
 *   community produces papers with limited real-world deployment impact. The
 *   constraint exhibits the full spectrum of DR types depending on observer
 *   position. Theater ratio (0.58) reflects that much algorithmic fairness
 *   work is performative: research papers propose debiasing methods that
 *   function in controlled settings but rarely reach production scale;
 *   regulatory compliance produces explainability artifacts that are
 *   technically sound but practically useless to affected populations;
 *   companies issue fairness commitments with minimal operational change.
 *   Extractiveness (0.52) indicates moderate-high structural extraction:
 *   technology companies derive substantial value from efficient automated
 *   decision-making while avoiding accountability costs; marginalized
 *   populations experience concentrated harms with minimal recourse.
 *   Suppression (0.68) is high due to technical opacity, computational scale
 *   that prevents individual contestation, resource asymmetries in legal
 *   challenge, and organizational control over audit access.
 *
 * KEY AGENTS:
 *   - Technology Companies: Primary beneficiary (institutional/arbitrage) — extract efficiency and cost reduction; can exit through debiasing when reputation risk exceeds mitigation cost
 *   - Marginalized Populations: Primary victim (powerless/trapped) — face discriminatory algorithmic decisions with no transparency, audit access, or appeal mechanism; cannot exit systems they depend on
 *   - Civil Rights Advocates: Secondary victim (moderate/constrained) — constrained by data access limits and computational resources; also benefit from emerging transparency standards and audit frameworks
 *   - Algorithm Designers: Secondary beneficiary (powerful/mobile) — produce technically sophisticated systems; face limited accountability for bias consequences; can migrate to other projects
 *   - Regulatory Bodies: Organized enforcer (organized/constrained) — impose transparency mandates and fairness standards; operate with sunset clauses as technology evolves; constrained by implementation complexity
 *   - Fairness Research Community: Institutional observer (institutional/arbitrage) — produce academic publications on debiasing; maintain field through publication incentives despite limited deployment impact; peripheral to actual harm dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_bias, 0.52).
domain_priors:suppression_score(algorithmic_bias, 0.68).
domain_priors:theater_ratio(algorithmic_bias, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_bias, extractiveness, 0.52).
narrative_ontology:constraint_metric(algorithmic_bias, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(algorithmic_bias, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_bias, tangled_rope).
narrative_ontology:human_readable(algorithmic_bias, "Algorithmic Bias in Machine Learning Systems").
narrative_ontology:topic_domain(algorithmic_bias, "technological/social").

domain_priors:requires_active_enforcement(algorithmic_bias).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_bias, technology_companies).
narrative_ontology:constraint_beneficiary(algorithmic_bias, algorithm_designers).
narrative_ontology:constraint_victim(algorithmic_bias, marginalized_populations).
narrative_ontology:constraint_victim(algorithmic_bias, algorithmic_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED POPULATION (SNARE) — Trapped within algorithmic systems with no exit: credit denial, hiring discrimination, surveillance targeting, criminal sentencing recommendations. No transparency into decision logic. Cannot audit, appeal, or escape the system. Maximum extraction with no exit route. Suppression is high — technical opacity, resource barriers to legal challenge, computational scale that prevents individual contestation.
constraint_indexing:constraint_classification(algorithmic_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CIVIL RIGHTS ADVOCATES (TANGLED ROPE) — Constrained by data access limitations and computational resources, but benefit from algorithmic transparency standards and emerging bias audit frameworks. Possess some agency through regulatory pressure, but face coordination barriers (proprietary algorithms limit auditing). Mixed extraction and coordination — the constraint both enables regulatory innovation and constrains audit effectiveness.
constraint_indexing:constraint_classification(algorithmic_bias, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECHNOLOGY COMPANIES (ROPE) — Perceive algorithmic bias as a coordination problem: efficient training on available historical data enables scalable decision-making. Bias is experienced as an incidental byproduct, not primary extraction mechanism. Net beneficiary through scale advantages. Can exit through retraining or debiasing when pressure mounts, but choose not to until reputation risk exceeds mitigation cost.
constraint_indexing:constraint_classification(algorithmic_bias, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY BODIES (SCAFFOLD) — EU AI Act, algorithmic transparency mandates, and fairness certification standards represent temporary enforcement structures with sunset clauses. High enforcement overhead but with explicit timelines (e.g., EU compliance deadlines). Sees the constraint as solvable through technical and legal innovation. Structured with declining suppression as transparency requirements mature and debiasing techniques advance.
constraint_indexing:constraint_classification(algorithmic_bias, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FAIRNESS RESEARCH COMMUNITY (PITON) — Academic research on algorithmic fairness is largely performative: papers propose debiasing methods that work in controlled settings but rarely deploy at scale. Theater ratio is high (0.60+) — theoretical contribution > practical impact. The field maintains itself through institutional inertia (publication incentives, conference circuits) despite limited real-world effectiveness. Debiasing techniques exist but organizational adoption remains low.
constraint_indexing:constraint_classification(algorithmic_bias, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL NECESSITY VIEW (MOUNTAIN) — From a civilizational view, some algorithmic bias is mathematically inherent: any classification system trained on historical data embeds the statistical regularities of that history. The 'impossibility of perfect fairness' (multiple fairness definitions are mathematically incompatible) suggests bias is an irreducible feature of automated decision-making. However, the structural data contradicts this — the high suppression (0.68) and active enforcement requirements indicate this is a contingent social-technical system, not a natural law.
constraint_indexing:constraint_classification(algorithmic_bias, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_bias_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_bias, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_bias, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_bias, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_bias, TR),
    TR >= 0.70.

:- end_tests(algorithmic_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The constraint extracts value through automation efficiency at the cost of fairness and accountability. Technology companies capture cost reduction benefits while marginalized populations bear concentrated harms. The value is not absolute extraction (which would require total suppression) because some debiasing is possible, some regulatory pressure exists, and organizational choice is involved rather than pure constraint. Suppression (0.68): High but not maximal. Technical opacity prevents marginalized populations from understanding or contesting decisions. Computational scale means individual contestation is prohibitively costly. Organizational control over algorithmic audit access creates information barriers. However, suppression is not 0.85+ because regulatory access requirements exist, reverse-engineering is possible (though difficult), and transparency mandates are increasing. Theater ratio (0.58): Moderate-high. Academic fairness research generates sophisticated papers and methods with limited deployment. Regulatory compliance produces transparency artifacts that appear informative but provide limited practical recourse for affected populations. Internal company bias audits are often conducted by the same organizations creating the biased systems. The theater has grown as regulatory pressure and public awareness increased — companies issue fairness statements, hire ethics officers, and publish research, but these performative activities often precede or substitute for substantive system redesign.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how different structural positions produce fundamentally different classifications from identical base properties. The technology company (institutional/arbitrage) perceives coordination — they frame efficiency gains as solving legitimate scalability problems. Marginalized populations (powerless/trapped) perceive pure extraction — algorithmic decisions affect their access to credit, employment, and freedom with no recourse. Regulatory bodies (organized/constrained) perceive a solvable problem with sunset logic — transparency and fairness standards are expected to reduce bias over 5-10 years as technical capabilities advance. The fairness research community (institutional/arbitrage) perceives an academic domain — bias is an intellectual puzzle to solve through more sophisticated methods, decoupled from real-world deployment urgency. Civil rights advocates (moderate/constrained) perceive mixed coordination-extraction — the regulatory framework enables some audit capability while suppressing others through proprietary claims. The civilizational analytical observer risks perceiving immutable law — the mathematical impossibility of satisfying all fairness definitions simultaneously — but this naturalizes a contingent state of current understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Technology companies (beneficiaries with arbitrage options) experience low effective extraction — they benefit from the constraint and can exit through retraining when reputation risk rises. Marginalized populations (victims with no exit) experience maximum extraction through the sigmoid f(d) — they are trapped, powerless, and bear concentrated harms. Civil rights advocates (moderate power, constrained exit) experience mid-range extraction — they have some agency through regulatory pressure but face real barriers to auditing and enforcement. Regulatory bodies (organized, constrained exit) experience moderate extraction within their enforcement role — they can mandate standards but face organizational constraints in implementation. The piton classification derives from theater_ratio (0.58) indicating performative elements exceed functional verification in fairness work. The mountain classification at the analytical level risks naturalizing what is contingent: the claim that perfect fairness is mathematically impossible is true for some fairness definitions, but it reflects current technical understanding, not immutable law.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint avoids mandatrophy through clear identification of genuine coordination (technology companies need efficient decision-making; society benefits from automation) embedded within asymmetric extraction (marginalized populations bear concentrated harms without proportional benefit). The tangled_rope classification correctly identifies that the constraint possesses BOTH a real coordination function (scalable decision-making) AND asymmetric extraction (harm concentration among powerless groups). The false mountain reading (perfect fairness is mathematically impossible) is exposed by structural data: suppression (0.68) indicates organizational choice and information barriers, not mathematical necessity; extractiveness (0.52) indicates value flows to beneficiaries, not equal distribution. The scaffold perspective (regulatory sunset) is structurally real but faces real challenges — debiasing at scale remains difficult, transparency rarely enables effective contestation, and deployment timelines extend beyond regulatory expectations. The piton observation (high theater) is accurate and important: fairness research and corporate ethics commitments are substantially performative, which does NOT invalidate them but clarifies their function. The constraint maintains itself through a combination of technical complexity (legitimate barrier to rapid debiasing), organizational incentives (lower regulatory compliance cost than true system redesign), and institutional inertia (existing systems have switching costs). Resolving the mandatrophy requires acknowledging all these elements simultaneously rather than reducing to any single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fairness_definition_incompatibility,
    'Are multiple fairness definitions mathematically incompatible, making perfect fairness structurally impossible, or does this reflect incomplete technical understanding?',
    'Proof that no single algorithm can simultaneously satisfy calibration, balanced accuracy, and equal opportunity across all groups; or discovery of fairness framework that resolves these tensions.',
    'If incompatible: algorithmic bias is mountain-like (inherent limit). If resolvable: it is snare/tangled_rope (organizational choice). This determines whether bias is reframed as natural law or as extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fairness_definition_incompatibility, conceptual, 'Mathematical incompatibility of fairness definitions').

omega_variable(
    training_data_representativeness,
    'Can algorithmic bias be eliminated by sourcing training data that eliminates historical inequities, or do structural feedback loops prevent truly representative training sets?',
    'Longitudinal analysis of debiased models deployed in production: measurement of bias reemergence over time; analysis of whether representative training data remains achievable as systems operate.',
    'If achievable: bias is reducible technical problem (scaffold/rope dynamics). If impossible: bias is structural feature of data-driven automation (snare-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(training_data_representativeness, empirical, 'Achievability of representative training data sources').

omega_variable(
    corporate_debiasing_incentives,
    'Are corporate organizations adopting debiasing methods at scale, or does bias persistence reflect deliberate organizational choice rather than technical impossibility?',
    'Audit of production systems: measurement of debiasing feature adoption, correlation with legal/reputation risk vs intrinsic fairness commitment, cost-benefit analysis of bias reduction.',
    'If deliberate choice: snare classification confirmed — suppression and extraction are organizational, not technical. If technical barrier: tangled_rope confirmed — mixed coordination and extraction challenges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corporate_debiasing_incentives, empirical, 'Corporate organizational adoption of debiasing at scale').

omega_variable(
    transparency_effectiveness,
    'Do transparency mandates (explainable AI, algorithmic auditing) actually enable affected populations to contest biased decisions, or do technical complexity and resource asymmetry make transparency ineffective?',
    'Analysis of appeals/contests post-transparency mandate: success rates, time-to-resolution, resource requirements for affected parties, correlation with regulatory enforcement intensity.',
    'If effective: regulatory scaffold works as designed; suppression declines over time. If ineffective: transparency becomes theater, and suppression remains structurally high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_effectiveness, empirical, 'Effectiveness of transparency in enabling affected populations to contest decisions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_bias, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algbias_tr_t0, algorithmic_bias, theater_ratio, 0, 0.35).
narrative_ontology:measurement(algbias_tr_t8, algorithmic_bias, theater_ratio, 8, 0.58).
narrative_ontology:measurement(algbias_tr_t16, algorithmic_bias, theater_ratio, 16, 0.58).

% Extraction over time
narrative_ontology:measurement(algbias_be_t0, algorithmic_bias, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(algbias_be_t8, algorithmic_bias, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(algbias_be_t16, algorithmic_bias, base_extractiveness, 16, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_bias, enforcement_mechanism).
narrative_ontology:affects_constraint(algorithmic_bias, predictive_policing).
narrative_ontology:affects_constraint(algorithmic_bias, hiring_discrimination_automation).
narrative_ontology:affects_constraint(algorithmic_bias, credit_risk_assessment_bias).
narrative_ontology:affects_constraint(algorithmic_bias, content_moderation_asymmetry).

% DUAL FORMULATION NOTE:
% Algorithmic bias is a meta-constraint that appears across multiple application domains (criminal justice, hiring, lending, content moderation). Each domain has domain-specific extractiveness and suppression values reflecting the particular stakeholder structure and regulatory environment. This story documents the generic constraint structure; domain-specific stories should be created for high-stakes applications (e.g., criminal justice bias has higher suppression, more trapped victims) with network links back to this constraint as the upstream structural mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_bias, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
