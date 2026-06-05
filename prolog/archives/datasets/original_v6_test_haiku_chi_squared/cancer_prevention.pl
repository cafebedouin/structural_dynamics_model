% ============================================================================
% CONSTRAINT STORY: cancer_prevention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cancer_prevention, []).

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
 *   constraint_id: cancer_prevention
 *   human_readable: Systemic Barriers to Preventable Cancer Risk Reduction
 *   domain: social/public_health
 *
 * SUMMARY:
 *   The global evidence that 40% of cancers are preventable through lifestyle
 *   changes and public health initiatives reveals a structural constraint:
 *   the systems that create preventable cancer risk are simultaneously the
 *   systems that benefit from that prevention remaining ineffective. Tobacco,
 *   processed food, automotive, and oil industries profit from the behaviors
 *   and environments that drive cancer incidence. Healthcare systems profit
 *   from treating advanced cancers. Low-income populations and racialized
 *   minorities bear the burden of preventable cancers concentrated in
 *   communities without power to change structural conditions. The constraint
 *   operates through suppression (making healthy choices unavailable or
 *   unaffordable), theater (health messaging that creates appearance of
 *   prevention while leaving root causes intact), and extraction (industries
 *   capture regulatory capacity while individuals are blamed for behavioral
 *   choices made under constraint). The theater ratio has increased over
 *   decades as public health messaging has proliferated while structural
 *   barriers remain unchanged — more health warnings, more calorie counts,
 *   more screening recommendations, less actual prevention capacity. The
 *   constraint is both Tangled Rope (mixing coordination and extraction, with
 *   active enforcement) and Snare (from the perspective of trapped
 *   populations), with a Piton layer (performative messaging masking
 *   institutional inertia) and a false Mountain layer (risk of naturalizing
 *   as inevitable rather than structural).
 *
 * KEY AGENTS:
 *   - Low-income populations: Primary victim (powerless/trapped) — bear concentrated cancer burden; lack access to prevention resources; structurally locked into unhealthy environments by economics
 *   - Racialized minorities: Primary victim (powerless/trapped) — cancer burden concentrated through historical exclusion from healthy neighborhoods, occupational segregation, healthcare discrimination, environmental racism
 *   - Tobacco, processed food, and automotive industries: Primary beneficiaries (institutional/arbitrage) — profit from addictive products, food system optimized for cost rather than health, urban sprawl and car dependency
 *   - Healthcare systems: Secondary beneficiary (institutional/arbitrage) — revenue model optimized for treatment rather than prevention; prevention requires long-term investment with deferred/diffuse returns
 *   - Individual health-conscious citizens: Mixed position (moderate/constrained) — can access some prevention resources but constrained by income, time, information quality; guilt-based messaging individualizes structural problems
 *   - Public health advocates and NGOs: Organized actors (organized/constrained) — can implement some prevention programs but constrained by underfunding, industry opposition, and structural barriers
 *   - Public health messaging infrastructure: Institutional (institutional/constrained) — performs prevention activity at individual level while structural barriers remain; theater ratio high, functional impact low
 *   - Analytical observer: Civilizational view (analytical/analytical) — risk of naturalizing constraint as inevitable individual irrationality rather than structural design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cancer_prevention, 0.58).
domain_priors:suppression_score(cancer_prevention, 0.65).
domain_priors:theater_ratio(cancer_prevention, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cancer_prevention, extractiveness, 0.58).
narrative_ontology:constraint_metric(cancer_prevention, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cancer_prevention, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cancer_prevention, tangled_rope).
narrative_ontology:human_readable(cancer_prevention, "Systemic Barriers to Preventable Cancer Risk Reduction").
narrative_ontology:topic_domain(cancer_prevention, "social/public_health").

domain_priors:requires_active_enforcement(cancer_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cancer_prevention, tobacco_industry).
narrative_ontology:constraint_beneficiary(cancer_prevention, processed_food_manufacturers).
narrative_ontology:constraint_beneficiary(cancer_prevention, automotive_manufacturers).
narrative_ontology:constraint_beneficiary(cancer_prevention, healthcare_systems).
narrative_ontology:constraint_victim(cancer_prevention, low_income_populations).
narrative_ontology:constraint_victim(cancer_prevention, racialized_minorities).
narrative_ontology:constraint_victim(cancer_prevention, populations_with_limited_health_literacy).
narrative_ontology:constraint_victim(cancer_prevention, public_health_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME POPULATIONS (SNARE) — Trapped in structural environment with no affordable access to preventive resources. High-calorie processed foods cost less per calorie than fresh produce; carcinogenic occupational exposures (agricultural pesticides, industrial chemicals) concentrated in poor neighborhoods; healthcare access barriers prevent early detection. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.96. Pure extraction: bears all costs, cannot exit.
constraint_indexing:constraint_classification(cancer_prevention, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RACIALIZED MINORITIES (SNARE) — Trapped by historical redlining, occupational segregation into high-exposure industries, residential proximity to pollution sources, and systemic healthcare discrimination. Cancer burden concentrated in communities with least political power to change structural conditions. d≈0.91, f(d)≈1.38, σ=1.2 → χ≈0.94. Structural extraction mechanism.
constraint_indexing:constraint_classification(cancer_prevention, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INDIVIDUAL HEALTH-CONSCIOUS CITIZENS (TANGLED ROPE) — Can access preventive information and some resources (organic food, gym memberships, screening access) but constrained by income, time scarcity, and information quality. Partial coordination function: health messaging does reach some populations and enable behavior change. But extraction mechanism also operates: guilt-based messaging individualizes structural problems ('you chose to live in a food desert'), and expensive preventive products are marketed to affluent populations. d≈0.58, f(d)≈0.80, σ=1.0 → χ≈0.46. Mixed coordination-extraction.
constraint_indexing:constraint_classification(cancer_prevention, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EXTRACTIVE INDUSTRIES (ROPE) — Benefit from the constraint's enforcement. Tobacco industry benefits from addiction mechanism (suppression); processed food manufacturers benefit from cost-based competitive advantage and addictive formulation patents; automotive manufacturers benefit from urban sprawl and car dependency (reducing walkability and increasing air pollution exposure). The constraint manifests as coordination from their perspective: they coordinate on preventing effective public health regulation through lobbying, marketing to low-income populations, and maintaining supply chains optimized for profit rather than health. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary; extraction mechanism is coordinated to their advantage.
constraint_indexing:constraint_classification(cancer_prevention, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HEALTHCARE SYSTEMS (ROPE) — Institutional beneficiary from treatment-focused rather than prevention-focused model. Profitable to treat advanced cancers than to prevent them; prevention requires long-term public health investment with no direct institutional revenue. The constraint manifests as coordination of the status quo: healthcare systems coordinate on expensive treatment protocols rather than investing in upstream prevention. d≈0.12, f(d)≈-0.05, σ=1.0 → χ≈-0.03. Net beneficiary but with lower extraction than industries; healthcare institutions are partly constrained by public health mandates.
constraint_indexing:constraint_classification(cancer_prevention, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PUBLIC HEALTH ADVOCATES (TANGLED ROPE) — Organized agents with partial agency. Can implement some prevention programs (smoking cessation, health education) but constrained by funding, political opposition from industry, and structural barriers (food pricing, urban design). Experience genuine coordination function: they are solving real public health problems. But also constrained by suppression mechanism: industry lobbying, regulatory capture, underfunded public health infrastructure. d≈0.48, f(d)≈0.65, σ=1.1 → χ≈0.39. Partial coordination, partial extraction; see a sunset (prevention-focused health systems) but limited power to enforce it.
constraint_indexing:constraint_classification(cancer_prevention, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: PUBLIC HEALTH MESSAGING (PITON) — Vestigial ritualistic structure: individual-level health warnings (cigarette package labels, calorie counts) persist despite minimal behavioral impact. Theater ratio = 0.68. The messaging creates appearance of prevention activity while structural barriers remain unchanged. Public health agencies perform prevention (campaigns, education, screening programs) but the institutional structure prevents addressing root causes (industry power, food system architecture, environmental exposures). Degraded constraint: maintenance through institutional inertia rather than functional effectiveness.
constraint_indexing:constraint_classification(cancer_prevention, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER - FALSE MOUNTAIN (CIVILIZATIONAL VIEW) — Risk of naturalizing the constraint as inevitable: 'humans will always make unhealthy choices; prevention is always limited by individual irrationality.' This framing obscures that the constraint is entirely structural. Accessibility collapse fails: there are clear paths to prevention-focused systems (Denmark, Costa Rica models). Resistance is moderate: political will can overcome industry opposition (tobacco control policy, sugar taxes where enforced). Engine will identify this as a false summit — the natural law framing is misattribution.
constraint_indexing:constraint_classification(cancer_prevention, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cancer_prevention_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cancer_prevention, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cancer_prevention, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cancer_prevention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cancer_prevention, TR),
    TR >= 0.70.

:- end_tests(cancer_prevention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts value from low-income and racialized populations through concentrated cancer burden while extractive industries profit from prevention-defeating behaviors and products. Extraction is measurable but not total — some prevention access exists, some individuals can navigate constraints. The value has increased over 50 years as industries have developed more effective mechanisms to target vulnerable populations and as public health infrastructure has been underfunded relative to industry marketing. Suppression (0.65): Moderate-high. Structural barriers suppress healthy choices: food affordability (calorie-for-calorie processed foods cost less), occupational exposure concentration in low-income communities, residential segregation producing air pollution exposure, healthcare access barriers, limited health literacy resources. Suppression is not absolute — information access has improved (internet, screening programs) — but structural barriers remain dominant. Theater ratio (0.68): Moderate-high. Public health messaging (warnings, education, screening campaigns) has proliferated while structural prevention capacity has stagnated. Health warnings on cigarette packages have minimal behavioral impact; calorie counts don't change purchasing power constraints; screening campaigns identify cancers that prevention-focused systems would prevent entirely. The performative content has increased as individual-level interventions have proliferated without addressing root causes (food system architecture, occupational regulation, urban design, industry subsidies). Claimed type (Tangled Rope): The constraint exhibits both genuine coordination function (public health infrastructure does enable some behavior change) and asymmetric extraction (targeted at powerless populations). Active enforcement required: industries actively suppress effective regulation through lobbying, marketing to vulnerable populations, and supply-chain optimization for profit.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival disagreement across the observation space. Low-income and racialized populations experience pure Snare: trapped with no exit options and maximum extraction as concentrated cancer burden. Individual health-conscious citizens experience Tangled Rope: partial coordination (accessible information) mixed with partial extraction (guilt-based messaging, expensive resources). Industries experience Rope and benefit from coordination of status quo. Public health advocates experience constrained Tangled Rope: can implement programs but blocked from addressing root causes. Healthcare systems experience Rope that benefits them directly. Public health messaging experiences Piton status: performative structure maintained through inertia rather than function. The analytical observer risks false Mountain: naturalizing the constraint as inevitable human nature rather than structural design. The perspectival gap reveals that the constraint is not natural or inevitable — it is the coordinated outcome of industry interest, regulatory capture, and systemic underinvestment in prevention.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income populations: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction; no exit options. Racialized minorities: Victim + trapped → d≈0.91, f(d)≈1.38. Systemic extraction through multiple pathways (occupational, residential, healthcare). Individual citizens: Mixed + constrained → d≈0.58, f(d)≈0.80. Moderate extraction; some resources accessible but constrained by income/time. Extractive industries: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; constraint coordination serves their interests. Healthcare systems: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.05. Net beneficiary but with regulatory constraints that limit arbitrage. Public health advocates: Organized + constrained → d≈0.48, f(d)≈0.65. Moderate extraction with partial agency. The directionality decomposition reveals that the constraint primarily extracts from populations without power while coordinating to benefit those with market and regulatory power.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RISK: The constraint risks being misclassified as pure Rope (coordination) or pure Scaffold (temporary) if the analytical perspective naturalizes prevention barriers as inevitable or if public health messaging is mistaken for functional prevention. The mandatrophy is resolved by declaring the structural relationship explicitly: the constraint has genuine coordination function (public health infrastructure does enable some behavior change in populations with access) AND genuine extraction (concentrated cancer burden in powerless populations serves industry profit). The Tangled Rope classification holds because: (1) coordination function exists but is limited to resourced populations, and (2) asymmetric extraction targets those without resources. The Snare perspective (from trapped populations) is not overridden by Rope perspectives (from beneficiaries) — instead, the presheaf shows that the same constraint appears coordinative to beneficiaries and extractive to victims. This is not contradiction but perspectival truth: coordination of status quo benefits extractive industries while harming trapped populations. The mandatrophy is that if you call this 'prevention coordination,' you erase the structural extraction. If you call it 'pure extraction,' you erase the genuine coordination infrastructure (public health systems, screening programs). The truth is Tangled Rope: both mechanisms operate simultaneously, coordinating for some while extracting from others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_vs_structural_agency,
    'To what extent is cancer prevention failure attributable to individual behavioral choices versus structural barriers that make healthy choices unavailable or unaffordable?',
    'Comparative analysis of cancer rates in populations with equal access to prevention resources versus equal populations with structural barriers; controlled intervention studies removing specific barriers (food access, air quality, occupational exposure) and measuring outcome changes.',
    'If primarily structural (>70% variance explained by barriers): constraint is Snare/Tangled Rope with high suppression. If primarily behavioral (>70% variance explained by choice): constraint may be Scaffold or Rope depending on information access. Classification outcome hinges on this decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_vs_structural_agency, empirical, 'Attribution of cancer prevention failure to individual vs structural factors').

omega_variable(
    industry_capture_vs_regulatory_failure,
    'Is weak cancer prevention due to regulatory capture (industry blocking effective regulation) or regulatory failure (weak political will absent industry opposition)?',
    'Analysis of jurisdictions with strong industry presence but effective prevention policy (Costa Rica, Denmark, Australia tobacco control); counterfactual modeling of regulatory outcomes with/without industry lobbying expenditure.',
    'If capture dominant: constraint is Snare/Tangled Rope with strong enforcement mechanism suppressing alternatives. If regulatory failure: constraint may be Piton (vestigial rather than actively suppressed) or Scaffold (policy will can change outcomes quickly).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_capture_vs_regulatory_failure, empirical, 'Whether weak prevention is due to regulatory capture or regulatory failure').

omega_variable(
    prevention_equity_mechanisms,
    'Can prevention-focused interventions (health education, screening access, lifestyle support) reduce cancer inequity or do they widen gaps by becoming resources consumed primarily by affluent populations?',
    'Longitudinal equity analysis of prevention program implementation; comparison of cancer rate changes between income quintiles pre/post program implementation; investigation of whether prevention programs reach intended low-income populations or are primarily used by affluent groups.',
    'If widen gaps: prevention-focused framing masks extraction mechanism (constraint becomes Snare disguised as Rope). If reduce gaps: genuine Rope/Scaffold classification justified. Outcome determines whether prevention narrative is appropriate or obscures structural exploitation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prevention_equity_mechanisms, empirical, 'Whether prevention interventions reduce or widen health equity gaps').

omega_variable(
    sunset_mechanism_viability,
    'Is there a realistic policy pathway to prevention-focused health systems (shift from treatment to upstream intervention, end industry subsidies, mandatory occupational safety) that would sunset the current constraint?',
    'Policy analysis of jurisdictions with prevention-focused systems (Costa Rica, Denmark); feasibility assessment of regulatory mechanisms to shift incentive structures; identification of industry opposition barriers and potential political coalitions to overcome them.',
    'If viable pathway exists: constraint is Scaffold (temporary, solvable). If pathway blocked (industry power, regulatory capture): constraint is Snare (persistent, requiring extraction acceptance). Classification outcome depends on whether escape route is genuinely structural or merely theoretical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_mechanism_viability, conceptual, 'Viability of policy pathway to prevention-focused health systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cancer_prevention, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(canc_tr_t0, cancer_prevention, theater_ratio, 0, 0.42).
narrative_ontology:measurement(canc_tr_t25, cancer_prevention, theater_ratio, 25, 0.55).
narrative_ontology:measurement(canc_tr_t50, cancer_prevention, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(canc_be_t0, cancer_prevention, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(canc_be_t25, cancer_prevention, base_extractiveness, 25, 0.47).
narrative_ontology:measurement(canc_be_t50, cancer_prevention, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cancer_prevention, resource_allocation).
narrative_ontology:affects_constraint(cancer_prevention, occupational_exposure_regulation).
narrative_ontology:affects_constraint(cancer_prevention, food_system_architecture).
narrative_ontology:affects_constraint(cancer_prevention, healthcare_access_equity).
narrative_ontology:affects_constraint(cancer_prevention, environmental_justice).

% DUAL FORMULATION NOTE:
% The cancer prevention constraint decomposes into multiple structural constraints: occupational exposure (industrial chemicals, pesticides), food system (caloric efficiency vs nutritional quality trade-off), healthcare access (screening and treatment availability), and environmental justice (pollution exposure concentration). Each has distinct ε, beneficiaries, and victims. The unified cancer prevention constraint (ε=0.58) represents the aggregate extraction through all these pathways. Upstream constraints (food system, occupational regulation) have higher ε and feed into the cancer prevention bottleneck.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cancer_prevention, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
