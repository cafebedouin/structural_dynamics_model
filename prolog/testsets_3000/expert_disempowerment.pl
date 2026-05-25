% ============================================================================
% CONSTRAINT STORY: expert_disempowerment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_expert_disempowerment, []).

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
 *   constraint_id: expert_disempowerment
 *   human_readable: Algorithmic Oversight Erosion
 *   domain: technological/social
 *
 * SUMMARY:
 *   Algorithmic oversight erosion represents the systematic replacement of
 *   expert discretionary judgment with rigid, automated decision-support
 *   systems in knowledge-intensive domains (medicine, engineering, finance).
 *   The constraint exhibits hybrid structure: algorithmic protocols provide
 *   genuine coordination benefits (standardization, consistency, reduced
 *   cognitive load) while simultaneously extracting the expert's ability to
 *   apply contextual judgment in edge cases. Domain experts face suppression
 *   from multiple directions: institutional protocols require algorithmic
 *   compliance, liability doctrine penalizes override without documented
 *   justification, and organizational incentives reward algorithmic
 *   defensibility over judgment quality. The constraint operates across
 *   healthcare (clinical decision support, diagnostic algorithms),
 *   engineering (design review automation, safety system lockouts), finance
 *   (algorithmic trading restrictions on discretionary traders), and
 *   education (automated grading, algorithmic course sequencing). The theater
 *   ratio (0.64) reflects that audit and compliance mechanisms have shifted
 *   from assessing judgment quality to verifying algorithmic adherence — the
 *   ritual persists but its function has atrophied. Base extractiveness
 *   (0.58) reflects that the coordination benefit is real but suppression of
 *   judgment is severe and growing. Suppression (0.68) indicates multiple
 *   reinforcing barriers: technical (algorithms are black boxes),
 *   organizational (performance metrics reward algorithmic compliance), and
 *   legal (liability doctrine punishes override).
 *
 * KEY AGENTS:
 *   - Domain Experts (Physicians/Engineers): Primary victim (powerless/trapped) — systematic suppression of contextual judgment; no exit without career/liability risk
 *   - Specialized Communities: Secondary victim (moderate/constrained) — can organize through professional societies; partial capacity for pushback
 *   - Algorithm Operators (Healthcare Admin/Corporate): Primary beneficiary (institutional/arbitrage) — consolidate decision authority; reduce liability exposure through standardization
 *   - Regulatory Authority (FDA/Medical Boards): Secondary beneficiary-victim (organized/constrained) — benefit from standardization but face pressure from expert communities to permit discretion
 *   - Audit and Compliance Infrastructure: Institutional actor (institutional/arbitrage) — maintains ritualized oversight; shifted from judgment assessment to algorithmic compliance verification
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes hybrid coordination-extraction structure; sees mandatrophy risk as edge cases accumulate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(expert_disempowerment, 0.58).
domain_priors:suppression_score(expert_disempowerment, 0.68).
domain_priors:theater_ratio(expert_disempowerment, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(expert_disempowerment, extractiveness, 0.58).
narrative_ontology:constraint_metric(expert_disempowerment, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(expert_disempowerment, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(expert_disempowerment, tangled_rope).
narrative_ontology:human_readable(expert_disempowerment, "Algorithmic Oversight Erosion").
narrative_ontology:topic_domain(expert_disempowerment, "technological/social").

domain_priors:requires_active_enforcement(expert_disempowerment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(expert_disempowerment, system_operators).
narrative_ontology:constraint_beneficiary(expert_disempowerment, liability_minimizers).
narrative_ontology:constraint_victim(expert_disempowerment, domain_experts).
narrative_ontology:constraint_victim(expert_disempowerment, judgment_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOMAIN EXPERT (SNARE) — Trapped within algorithmic oversight infrastructure. Professional judgment is systematically subordinated to automated decision gates that cannot be overridden without creating liability exposure or professional repercussions. Expert holds knowledge that system designers lack, but institutional structure prevents deployment of that knowledge. No exit option: refusing algorithmic guidance triggers audits, malpractice liability, or termination. Maximum extraction — the expert's accumulated judgment is suppressed in favor of standardized automation.
constraint_indexing:constraint_classification(expert_disempowerment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SPECIALIZED PRACTITIONER COMMUNITY (TANGLED ROPE) — Constrained but not powerless. Specialist communities (cardiologists, structural engineers) have partial capacity to push back through professional standards organizations, journal publications, and professional societies. They benefit from standardization that reduces cognitive load and spreads best practices, but lose autonomy and contextual judgment. Mixed: extraction is real (algorithmic protocols limit their discretion) but coordination benefit exists (shared standards reduce uncertainty). Exit is constrained but not blocked — they can organize, publish critiques, lobby for exceptions.
constraint_indexing:constraint_classification(expert_disempowerment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ALGORITHM OPERATOR (ROPE) — Institutional beneficiary. Sees algorithmic oversight as coordination mechanism that standardizes decision-making, reduces liability exposure, and enables scale. The operator experiences the constraint as enabling: having clear algorithmic guidelines makes decisions defensible and auditable. Net beneficiary — the constraint extracts discretion from domain experts and consolidates it in the operator's control. Exits via arbitrage: can switch vendors, adjust thresholds, or argue for system upgrades without career risk.
constraint_indexing:constraint_classification(expert_disempowerment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — Organized institutional actor. Regulatory bodies (FDA, medical boards, occupational safety) face competing mandates: standardize decisions for consistency and auditability, but also permit expert override for edge cases. They benefit from algorithmic oversight (reduces regulatory burden of case-by-case audits, enables at-scale compliance checking) but also face pressure from expert communities to permit discretion. Active enforcement required to maintain algorithmic gates; coordination function exists (shared standards reduce chaos). Constrained exit — cannot abandon standardization without losing regulatory coherence, but can modify rules through rulemaking.
constraint_indexing:constraint_classification(expert_disempowerment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: AUDIT AND COMPLIANCE INFRASTRUCTURE (PITON) — Vestigial institutional theater. Auditing and compliance mechanisms designed to verify expert judgment have shifted to verifying algorithmic compliance. The audit ritual persists (documentation, sign-offs, review processes) but now enforces adherence to automated rules rather than assessing contextual judgment. Theater ratio high: auditors check that algorithms were applied correctly, not whether the algorithmic decision was optimal for the specific case. The infrastructure is maintained through institutional inertia and liability doctrine, not because it successfully detects failures (many failures slip through automated systems).
constraint_indexing:constraint_classification(expert_disempowerment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, this constraint exhibits genuine hybrid structure: algorithmic oversight provides coordination benefit (standardization, reduced cognitive load, consistency) AND extracts expert judgment (suppresses contextual discretion, centralizes decision-making, creates liability asymmetry). The coordination function is real (many domains benefit from standardized protocols). The extraction is real (expert knowledge is suppressed). Both mechanisms operate simultaneously. Base extractiveness (0.58) and suppression (0.68) reflect this hybrid structure — higher than pure coordination, lower than pure extraction.
constraint_indexing:constraint_classification(expert_disempowerment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(expert_disempowerment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(expert_disempowerment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(expert_disempowerment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(expert_disempowerment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(expert_disempowerment, TR),
    TR >= 0.70.

:- end_tests(expert_disempowerment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Moderate-high, reflecting that the coordination function (standardization, consistency) is genuine and valuable, but suppression of contextual judgment is real and growing. Algorithms capture decision authority that experts previously held. The upward trajectory (0.32 → 0.58 over 10 time units) reflects institutional expansion of algorithmic gates and tightening of override liability. Suppression (0.68): High, reflecting multiple converging barriers. Technical suppression: algorithmic systems are often black-box or opaque, preventing expert understanding. Organizational suppression: performance metrics and compliance audits reward algorithmic adherence over judgment quality. Legal suppression: liability doctrine penalizes expert override, making algorithmic recommendations the path of least professional risk. Theater ratio (0.64): Moderate-high, reflecting that audit mechanisms have shifted to verifying algorithmic compliance rather than assessing judgment quality. Compliance documentation and sign-offs persist (theater) but no longer serve their original function of ensuring judgment was contextually sound. The rising trajectory (0.42 → 0.64) indicates this shift is accelerating.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. Algorithm operators see pure coordination (Rope) — algorithmic gates make decisions more consistent, auditable, and legally defensible. Trapped experts see pure extraction (Snare) — their judgment is systematically suppressed with no exit option. Specialized communities see mixed mechanism (Tangled Rope) — they benefit from standardization but lose autonomy. Regulatory authorities see coordination with enforcement complications (Tangled Rope) — standardization serves regulatory function but requires active enforcement against expert pushback. Audit infrastructure sees degraded ritual (Piton) — compliance checking persists but no longer serves original judgment-quality function. The analytical observer sees the true structure: genuine coordination function (standardization is valuable) plus genuine extraction (expert judgment is suppressed). The perspectival gap is not a measurement artifact — it reflects real structural differences in how the constraint operates from different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive directionality derivation. Algorithm operators (beneficiaries) see the constraint as coordination — they experience low or negative effective extraction (d ≈ 0.15-0.25) because they benefit from standardization and defensibility. Domain experts (victims) see extraction — they experience high effective extraction (d ≈ 0.80-0.95) because institutional structures prevent them from applying their judgment. Specialized communities occupy the middle (d ≈ 0.55-0.65): they have partial capacity to organize and push back, but face real barriers. The regulatory authority is torn between both positions (d ≈ 0.50-0.60): they coordinate through standardization but also face pressure from experts. This perspectival gap (rope for beneficiaries, snare for trapped experts, tangled rope for moderate communities) is the core diagnostic feature.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy Resolution: This constraint exhibits incipient mandatrophy. As edge cases accumulate (omega: edge_case_accumulation), algorithms require growing expert judgment to handle misalignment. But institutional structures prevent expert judgment deployment. The system begins to degrade: audit infrastructure must field exceptions, regulatory bodies face pressure to permit override, operators face liability for algorithmic failures they cannot prevent. The theater ratio rises (auditors check more and more exception cases, but this is performative activity without real decision authority). If mandatrophy progresses: the constraint could degrade toward Piton (pure ritual without function) or collapse toward pure Snare (complete expert disempowerment, leading to system failure). Prevention requires either: (1) Scaffold pathway — regulatory reform that permits expert override with sunset timeline as algorithms improve, or (2) Rope pathway — redesigning algorithms to genuinely incorporate expert judgment rather than suppress it. Current trajectory suggests mandatrophy is unresolved: extractiveness rising, theater ratio rising, edge cases accumulating. Base extractiveness is 0.58 (below the 0.70 threshold requiring explicit mandatrophy resolution), but the upward measurement trajectory and rising theater ratio indicate mandatrophy risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contextual_judgment_value,
    'How much marginal benefit does expert contextual judgment provide beyond algorithmic decision rules in real-world practice?',
    'Longitudinal outcome comparison: cases where expert override of algorithm occurred vs algorithmic recommendation alone; stratified by outcome type (optimal, acceptable, harmful). Regression analysis controlling for case complexity and expert experience.',
    'If marginal benefit is high (>15% improvement in outcomes): extraction classification strengthens (expert judgment is being suppressed despite real value). If marginal benefit is low (<5%): coordination classification strengthens (standardization genuinely improves decision quality). If mixed by domain: suggests constraint family decomposition needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contextual_judgment_value, empirical, 'Marginal value of expert judgment over algorithmic rules').

omega_variable(
    algorithm_transparency_feasibility,
    'Can algorithmic decision-making be made sufficiently transparent that domain experts retain meaningful understanding and contestability of specific recommendations?',
    'Test interventions in specialized communities: implement explainable AI (XAI) systems, measure whether experts can meaningfully critique algorithmic recommendations; assess whether transparency enables productive override without liability barriers.',
    'If transparency is feasible: constraint could shift to Rope or Scaffold (experts retain enough understanding for meaningful engagement). If transparency is intractable: expert disempowerment is structurally entrenched (Snare or high-extraction Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_transparency_feasibility, empirical, 'Whether algorithmic transparency enables expert contestability').

omega_variable(
    liability_doctrine_entrenchment,
    'Is the barrier to expert override primarily technical (algorithms are hard to override) or legal (liability doctrine penalizes override)?',
    'Comparative analysis of liability doctrine across jurisdictions; case law analysis of verdicts when experts override algorithms; controlled institutional experiments relaxing liability pressure while keeping algorithms identical.',
    'If primarily legal: constraint could be rapidly shifted by regulatory reform (Scaffold with sunset). If primarily technical: entrenchment is deeper and sunset slower. Mixed mechanisms suggest distinct constraint family members.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liability_doctrine_entrenchment, empirical, 'Whether barrier to expert override is technical or legal').

omega_variable(
    edge_case_accumulation,
    'Do algorithmic systems develop increasing numbers of edge cases and exceptions over time, requiring growing expert judgment to handle misalignment?',
    'Historical tracking of exception rates and override frequency in deployed algorithmic systems; analysis of whether exceptions concentrate in specific domains or distribute randomly.',
    'If exceptions accumulate: constraint exhibits mandatrophy — algorithms become less useful, expert judgment becomes essential, but institutional structures prevent its deployment. System degrades toward Piton (theater without function). If exceptions remain stable: suggests algorithms are tracking real-world structure adequately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(edge_case_accumulation, empirical, 'Rate of edge case accumulation in deployed algorithms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(expert_disempowerment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(expd_tr_t0, expert_disempowerment, theater_ratio, 0, 0.42).
narrative_ontology:measurement(expd_tr_t5, expert_disempowerment, theater_ratio, 5, 0.54).
narrative_ontology:measurement(expd_tr_t10, expert_disempowerment, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(expd_be_t0, expert_disempowerment, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(expd_be_t5, expert_disempowerment, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(expd_be_t10, expert_disempowerment, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(expert_disempowerment, enforcement_mechanism).
narrative_ontology:affects_constraint(expert_disempowerment, medical_liability_doctrine).
narrative_ontology:affects_constraint(expert_disempowerment, algorithm_interpretability_barrier).
narrative_ontology:affects_constraint(expert_disempowerment, regulatory_standardization_mandate).

% DUAL FORMULATION NOTE:
% Expert disempowerment decomposes into three structural constraints: (1) medical_liability_doctrine (ε≈0.45, Tangled Rope) — liability framework creates incentives to favor algorithmic compliance over expert judgment; (2) algorithm_interpretability_barrier (ε≈0.35, Rope) — black-box algorithms create technical barrier to expert understanding, forcing trust in automation; (3) regulatory_standardization_mandate (ε≈0.40, Rope) — standardization regulations require uniform algorithmic application, preventing contextual exception. Expert disempowerment represents the combined effect of these constraints — each individually is manageable, but their interaction creates the Snare experienced by trapped experts. Network links enable contamination propagation: if liability doctrine is reformed to permit override, extractiveness of expert_disempowerment decreases; if algorithms become more interpretable, expert capacity for meaningful engagement increases.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(expert_disempowerment, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
