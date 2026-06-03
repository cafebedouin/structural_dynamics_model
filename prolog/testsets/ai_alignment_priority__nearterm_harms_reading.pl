% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__nearterm_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__nearterm_harms_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: ai_alignment_priority__nearterm_harms_reading
 *   human_readable: AI Alignment Priority: Present Harms Reading
 *   domain: AI_governance/technology_ethics/algorithmic_justice
 *
 * SUMMARY:
 *   The nearterm-harms reading of AI alignment priority frames the alignment
 *   problem as preventing present discriminatory and extractive harms from
 *   deployed AI systems to marginalized populations. This reading prioritizes
 *   justice for currently vulnerable groups facing algorithmic bias in
 *   hiring, lending, criminal justice, content moderation, and benefit
 *   allocation over existential risk mitigation or capability acceleration.
 *   The constraint is a tangled_rope: it coordinates protective
 *   infrastructure (auditing, bias detection, fairness metrics) for present
 *   victims while extracting resources from other research pathways and
 *   imposing deployment friction on capability companies. The reading is one
 *   interpretation of a contested kernel — the question of what 'alignment'
 *   fundamentally means and whose interests it centers. The nearterm-harms
 *   reading centers present vulnerable populations; the
 *   existential_risk_reading centers humanity's future; the
 *   integrated_reading attempts to hold both as complementary. This story
 *   instantiates the nearterm reading only, preserving the structural data
 *   that enables the engine's false-summit detection and mandatrophy
 *   resolution to identify the contest.
 *
 * KEY AGENTS:
 *   - Marginalized populations (powerless/trapped): Primary victims — face discriminatory outcomes from deployed AI systems in high-stakes domains; bear extraction through biased decisions with no exit option
 *   - Fairness/justice researchers (moderate/constrained): Secondary actors — benefit from research opportunities and funding for bias mitigation; constrained by career dependencies and resource scarcity
 *   - Regulatory bodies & civil rights organizations (institutional/arbitrage): Institutional beneficiaries — experience alignment priority as coordination mechanism enabling protective infrastructure; leverage for jurisdictional enforcement
 *   - AI capability companies (powerful/mobile): Institutional victims of the reading's framing — constrained by deployment audit requirements despite nominal mobility; locked into competitive timelines; suppress alternative readings through resource concentration
 *   - AI safety & alignment research establishment (organized/constrained): Mixed — benefit from institutionalization of alignment research; constrained by funding competition and capability-first norms
 *   - Analytical observer (analytical/analytical): Positions the reading as a coherent reading of the kernel with real coordination function (protecting present victims) and real extraction mechanism (resource diversion from other priorities)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, 0.58).
domain_priors:suppression_score(ai_alignment_priority__nearterm_harms_reading, 0.62).
domain_priors:theater_ratio(ai_alignment_priority__nearterm_harms_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__nearterm_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__nearterm_harms_reading, "AI Alignment Priority: Present Harms Reading").
narrative_ontology:topic_domain(ai_alignment_priority__nearterm_harms_reading, "AI_governance/technology_ethics/algorithmic_justice").

domain_priors:requires_active_enforcement(ai_alignment_priority__nearterm_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, 'd9000417-33b1-4ac8-850e-aa8e4ea9bc06').
narrative_ontology:cs_kernel_codification('d9000417-33b1-4ac8-850e-aa8e4ea9bc06', distributed).
narrative_ontology:cs_authority_grounding('d9000417-33b1-4ac8-850e-aa8e4ea9bc06', distributed).
narrative_ontology:cs_reading_relation('d9000417-33b1-4ac8-850e-aa8e4ea9bc06', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('d9000417-33b1-4ac8-850e-aa8e4ea9bc06', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('d9000417-33b1-4ac8-850e-aa8e4ea9bc06', foundational, present_algorithmic_harms_are_primary_alignment_responsibility).
narrative_ontology:cs_axiom_status(present_algorithmic_harms_are_primary_alignment_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('d9000417-33b1-4ac8-850e-aa8e4ea9bc06', present_algorithmic_harms_are_primary_alignment_responsibility, deontological).
narrative_ontology:cs_axiom('d9000417-33b1-4ac8-850e-aa8e4ea9bc06', foundational, marginalized_populations_have_priority_claim_on_alignment_resources).
narrative_ontology:cs_axiom_status(marginalized_populations_have_priority_claim_on_alignment_resources, holdable).
narrative_ontology:cs_axiom_grounding('d9000417-33b1-4ac8-850e-aa8e4ea9bc06', marginalized_populations_have_priority_claim_on_alignment_resources, deontological).
narrative_ontology:cs_reference_frame('d9000417-33b1-4ac8-850e-aa8e4ea9bc06', algorithmic_justice_centered_alignment).
narrative_ontology:cs_drift_state('d9000417-33b1-4ac8-850e-aa8e4ea9bc06', contemporary_deployment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d9000417-33b1-4ac8-850e-aa8e4ea9bc06', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, marginalized_populations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, vulnerable_communities).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, deployment_timeline_pressure).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, capability_development_resources).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRESENT HARM VICTIMS (SNARE) — Marginalized groups facing algorithmic discrimination in hiring, lending, criminal justice, and content moderation cannot exit deployed systems and bear immediate extraction through biased outcomes. Trapped by dependence on AI-mediated services with no alternative pathways. Maximum experienced extraction with zero degrees of freedom.
constraint_indexing:constraint_classification(ai_alignment_priority__nearterm_harms_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FAIRNESS RESEARCHERS (TANGLED ROPE) — Moderate power with constrained exit (career dependence on funding, publication venues, institutional affiliation). Benefits from the constraint through growing research opportunities and funding for bias mitigation; simultaneously victimized by resource constraints and pressures to deprioritize auditing in favor of capability development. Mixed coordination and extraction at biographical horizon.
constraint_indexing:constraint_classification(ai_alignment_priority__nearterm_harms_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGULATORS & CIVIL RIGHTS (ROPE) — Institutional actors with arbitrage options (jurisdictional choice, enforcement leverage, reputational arbitrage). Experience the constraint as coordination problem: aligning deployment timelines with audit requirements creates legitimate friction but also enables protective infrastructure. Benefits from claiming alignment priority on present harms; coordination function dominates from this position.
constraint_indexing:constraint_classification(ai_alignment_priority__nearterm_harms_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CAPABILITY COMPANIES (SNARE) — Powerful institutions with mobile exit options (jurisdictional arbitrage, norm-setting influence, deployment acceleration) experience the nearterm-harms reading as a constraint that suppresses deployment velocity and market advantage. While nominally mobile, they are locked into capability-first timelines by competitive dynamics and investor expectations. Extract rents through accelerated deployment; suppress alternatives through resource concentration. High suppression despite formal mobility.
constraint_indexing:constraint_classification(ai_alignment_priority__nearterm_harms_reading, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SAFETY ESTABLISHMENT (TANGLED ROPE) — Organized research community (academic centers, independent labs, industry safety teams) with constrained exit due to funding and publication dependencies. Benefits from alignment research prioritization; simultaneously constrained by competition for resources with capability development. Generational horizon shows the structural benefit: alignment research becomes institutionalized, attracting talent and funding. But extraction persists through resource scarcity and capability-first norms.
constraint_indexing:constraint_classification(ai_alignment_priority__nearterm_harms_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From the analytical civilizational view, the nearterm-harms reading instantiates a genuine tangled_rope: real coordination function (protecting present populations from identifiable harms requires systemic audit infrastructure) coupled with asymmetric extraction (the priority assignment channels resources toward bias mitigation at the expense of existential safety research and capability acceleration). The reading coordinates justice for present victims while extracting time/resources from other alignment research pathways. Neither function can be reduced to the other — both are structurally real.
constraint_indexing:constraint_classification(ai_alignment_priority__nearterm_harms_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__nearterm_harms_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_alignment_priority__nearterm_harms_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_alignment_priority__nearterm_harms_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__nearterm_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The nearterm-harms reading channels resources toward bias mitigation and imposes deployment friction (audit requirements, documentation burdens, rollback obligations) on capability companies. The extraction is real but not maximal because: (1) present harm reduction yields genuine benefits for marginalized populations, constituting a real coordination function, not pure rent; (2) the capability companies retain significant market power and can appeal to efficiency/innovation rationales; (3) the reading's enforcement is distributed across multiple regulators rather than concentrated. Rising from 0.42 to 0.58 reflects increasing institutional adoption of audit requirements and fairness mandates. Suppression (0.62): High. Multiple suppression mechanisms operate: (1) capability-company funding advantage suppresses fairness research budgets (competitive resource concentration), (2) publication bias favoring capability results suppresses harm documentation, (3) deployment momentum creates irreversibility (once systems are live, retrospective auditing is harder than pre-deployment review), (4) technical barriers to transparency suppress audit effectiveness. Theater ratio (0.55): Moderate. The nearterm-harms reading involves genuine audit and fairness work but also substantial theater: fairness metrics that don't track lived experience, diversity statements without mechanism change, and performative compliance with vague AI ethics principles. Rising from 0.48 to 0.55 reflects increasing performative adoption without corresponding resource commitment.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits dramatic perspectival divergence. Marginalized populations see pure snare (trapped, no exit, maximum extraction). Capability companies nominally see rope (coordination of audit standards) but structurally experience snare due to competitive lock-in. Regulators see rope (legitimate coordination). Fairness researchers see tangled_rope (genuine bias mitigation work mixed with resource extraction). The analytical observer sees tangled_rope: real protection of present victims (coordination function) genuinely coupled with real resource diversion (extraction function). The perspectival gap reveals that the debate over 'alignment priorities' is not about disagreement on facts but about different positions in a fundamentally asymmetric structure. Those experiencing harms prioritize their own alleviation; those building capabilities prioritize velocity; regulators prioritize managed transition.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized populations occupy the powerless/trapped position: they are victims of the constraint (experience discriminatory harms) with zero exit options (dependence on AI-mediated services with no alternatives). Their directionality d ≈ 0.95 (near-maximum target position). Fairness researchers occupy moderate/constrained: they are partly beneficiaries (funding, research opportunities) and partly victims (resource constraints, career risk). Their d ≈ 0.65 (symmetric bias toward target). Regulatory institutions occupy institutional/arbitrage: they are net beneficiaries (enforcement leverage, legitimacy) with exit options (jurisdictional arbitrage, reputational positioning). Their d ≈ 0.10 (beneficiary position). Capability companies occupy powerful/mobile: despite nominal mobility (jurisdictional shopping, deployment acceleration), they are functionally trapped in capability-race dynamics. The reading's extraction applies despite their power because the competitive structure suppresses exit. Their d ≈ 0.78 (target position despite power, due to suppression of alternatives). The safety establishment occupies organized/constrained: d ≈ 0.55 (mixed).
 *
 * MANDATROPHY ANALYSIS:
 *   The nearterm-harms reading resolves mandatrophy by revealing that the alignment priority contest is not a logical necessity (one reading must be false) but a structural reality (different institutional positions experience the constraint differently). The reading does NOT claim that existential risk is unreal or unimportant — omega variables (existential_risk_magnitude_empirical, resource_zero_sum_assumption) document the irreducible uncertainty about whether the priorities are genuinely zero-sum or whether both are underfunded relative to capability development. The mandatrophy is not 'which priority is correct?' but 'what is the true structure of the constraint and how do different observers perceive it?' The nearterm-harms reading instantiates one coherent answer to this question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_quantification_opacity,
    'What is the true incident rate and severity distribution of algorithmic harms across deployed systems, stratified by demographic group?',
    'Mandatory auditing and disclosure requirements; longitudinal tracking of outcomes pre/post-deployment; independent validation of company-reported fairness metrics',
    'If rates are higher than reported: suppression is greater than ε=0.62 suggests, and constraint classification shifts toward snare from more perspectives. If rates are lower: extraction component is overstated, and constraint becomes more rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_quantification_opacity, empirical, 'True incident rates and harm severity for deployed algorithmic systems').

omega_variable(
    existential_risk_magnitude_empirical,
    'What is the empirical probability distribution over AI capability development timelines and loss-of-control scenarios?',
    'Benchmark tasks for capability development; expert elicitation and calibration studies; post-hoc analysis of prediction accuracy from prior forecasting exercises',
    'If existential timelines are <10 years: the existential_risk_reading''s priority claim becomes empirically dominant, and the integrated_reading''s framing (complementary priorities) becomes structurally justified. If timelines are >30 years: the nearterm_harms_reading''s priority becomes empirically grounded without competition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_risk_magnitude_empirical, empirical, 'Empirical probability distribution over catastrophic AI scenarios and timelines').

omega_variable(
    resource_zero_sum_assumption,
    'Is the allocation of AI safety research funding genuinely zero-sum between nearterm-harms and existential-risk research, or are both underfunded relative to capability development?',
    'Analysis of funding flows by category (existential risk vs. fairness/bias mitigation vs. capability acceleration); study of research opportunity costs and talent allocation dynamics; counterfactual analysis of what resource increases would be allocated if both research areas had higher priority',
    'If zero-sum: the constraint represents genuine competition and the readings coexist_with relation is correct. If both underfunded relative to capability: the readings influence rather than foreclose each other, and the integrated_reading''s structural claim (complementary rather than competing) is empirically justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_zero_sum_assumption, empirical, 'Whether resource allocation between harm reduction and existential safety is zero-sum').

omega_variable(
    reading_kernel_contest_logic,
    'Is the kernel ''what alignment means and who it benefits'' genuinely contested across coherent frameworks, or does one reading''s core premise logically foreclose another within any single decision-making framework?',
    'Formal analysis of decision trees that operationalize each reading; identification of premises that would need to be false for competing readings to coexist in a single governance framework; mapping incompatible resource allocation decisions each reading entails',
    'If forecloses relation holds: one reading''s adoption mandates the others'' rejection, and the constraint network shows hard incompatibility. If coexists_with: different institutional actors genuinely hold different readings without logical contradiction, and the mandatrophy is perspectival rather than resolvable. If influences: reading adoption changes conditions for sibling readings without eliminating them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest_logic, conceptual, 'Logical structure of the kernel contest: foreclosure vs. coexistence vs. influence relations').

omega_variable(
    harm_attribution_causation,
    'Are observed harms in deployed AI systems causally attributable to deployment decisions, or are they artifacts of pre-existing social structures that the AI merely reflects?',
    'Counterfactual analysis of outcomes with/without algorithmic mediation; causal inference studies isolating AI system contributions; comparison of harm rates before/after deployment; analysis of which demographic groups experience harm differentially from those without AI system exposure',
    'If causally attributable to AI: the nearterm_harms_reading''s framing (alignment means preventing AI-specific harms) is structurally justified, extractiveness remains ≥0.50. If primarily reflective: the reading shifts focus toward social structure rather than AI governance, and ε decreases because the constraint becomes less about what AI systems do and more about inherited inequality. The victim group definition (marginalized populations) remains correct but the extraction mechanism changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_attribution_causation, empirical, 'Causal attribution of observed harms to AI deployment vs. reflection of pre-existing structures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aaip_near_tr_t0, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(aaip_near_tr_t4, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 4, 0.52).
narrative_ontology:measurement(aaip_near_tr_t8, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 8, 0.55).

% Extraction over time
narrative_ontology:measurement(aaip_near_be_t0, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(aaip_near_be_t4, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(aaip_near_be_t8, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 8, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(aaip_near_su_t0, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(aaip_near_su_t4, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(aaip_near_su_t8, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 8, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__nearterm_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__integrated_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, algorithmic_bias_audit_systems).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, deployment_timeline_governance).

% DUAL FORMULATION NOTE:
% The AI alignment priority constraint decomposes into three separate stories corresponding to the three readings of the kernel. Each reading instantiates a different constraint with a different ε value, beneficiary/victim structure, and type classification. The nearterm_harms_reading (this story) has ε≈0.58 (tangled_rope); the existential_risk_reading has higher ε (snare) due to concentration of risk assessment capacity; the integrated_reading attempts to minimize extraction while maximizing coordination. All three are linked via network.affects_constraints to enable comparative analysis of the kernel contest. No single reading is correct — each is a coherent instantiation of the kernel from a specific structural position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__nearterm_harms_reading, powerful, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
