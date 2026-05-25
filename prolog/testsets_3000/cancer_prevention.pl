% ============================================================================
% CONSTRAINT STORY: cancer_prevention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The prevention of 40% of cancers through lifestyle and public health
 *   interventions is theoretically available to every population, yet remains
 *   systematically unimplemented across income levels and geographies. This
 *   constraint exhibits a hybrid coordination-extraction structure: public
 *   health institutions coordinate prevention messaging and infrastructure,
 *   while simultaneously, pharmaceutical manufacturers and processed food
 *   producers benefit from the gap between preventable and prevented cancers.
 *   The constraint's structure reveals how legitimate health coordination
 *   (lifestyle guidance, screening access) becomes entangled with extractive
 *   mechanisms (lobbying against nutrition regulation, marketing of
 *   treatment-dependent lifestyles). Low-income populations face the severest
 *   barriers—food deserts, occupational carcinogens, healthcare access gaps,
 *   and time poverty prevent implementation of preventive behaviors
 *   regardless of knowledge. Simultaneously, the healthcare system's
 *   financial structure incentivizes treatment over prevention, creating
 *   institutional piton dynamics where prevention programs are performative
 *   rather than resourced. The theater ratio reflects a growing gap between
 *   public health rhetoric about prevention and actual infrastructure
 *   investment, indicating Goodhart drift as metrics substitute for outcomes.
 *
 * KEY AGENTS:
 *   - Low-Income Populations: Primary victims (powerless/trapped) — face food deserts, occupational carcinogens, healthcare access barriers, time poverty; cannot exit without systemic change
 *   - Pharmaceutical Industry: Primary beneficiary (institutional/arbitrage) — business model depends on treatment demand; profits from prevention gap
 *   - Processed Food Manufacturers: Primary beneficiary (institutional/arbitrage) — revenue from ultraprocessed foods; exploits behavioral barriers and marketing dominance
 *   - Public Health Advocates: Mixed victim-coordinator (moderate/constrained) — coordinate prevention efforts but constrained by industry lobbying and budget competition
 *   - Healthcare System: Institutional actor (institutional/arbitrage) — maintains prevention theater through performative campaigns while reimbursement structure incentivizes treatment
 *   - WHO and Public Health Coalitions: Organized agents (organized/constrained) — building alternate prevention pathways with sunset logic (carbon taxes, sugar taxes, workplace wellness)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing behavioral barriers as immutable constraints on human nature rather than contingent institutional arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cancer_prevention, 0.58).
domain_priors:suppression_score(cancer_prevention, 0.68).
domain_priors:theater_ratio(cancer_prevention, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cancer_prevention, extractiveness, 0.58).
narrative_ontology:constraint_metric(cancer_prevention, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cancer_prevention, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cancer_prevention, tangled_rope).
narrative_ontology:human_readable(cancer_prevention, "Systemic Barriers to Preventable Cancer Risk Reduction").
narrative_ontology:topic_domain(cancer_prevention, "social/public_health").

domain_priors:requires_active_enforcement(cancer_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cancer_prevention, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(cancer_prevention, processed_food_manufacturers).
narrative_ontology:constraint_beneficiary(cancer_prevention, tobacco_companies).
narrative_ontology:constraint_victim(cancer_prevention, low_income_populations).
narrative_ontology:constraint_victim(cancer_prevention, public_health_infrastructure).
narrative_ontology:constraint_victim(cancer_prevention, preventive_medicine_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME COMMUNITIES (SNARE) — Cannot exit environmental and economic barriers to preventive behavior. Trapped by food deserts, unsafe neighborhoods for exercise, occupational carcinogens, and lack of healthcare access. Maximum experienced extraction with no genuine alternative pathways.
constraint_indexing:constraint_classification(cancer_prevention, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC HEALTH ADVOCATES (TANGLED ROPE) — Constrained by industry lobbying, limited funding for prevention campaigns, and competing priorities within health systems. Experience both coordination (multi-sector prevention initiatives) and extraction (policy capture, resource diversion toward treatment).
constraint_indexing:constraint_classification(cancer_prevention, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL AND FOOD INDUSTRIES (ROPE) — Benefit from the prevention barrier. Profitable to sell treatment drugs and ultraprocessed foods. Experience the constraint as coordination: their business models rely on maintaining preventability gaps while appearing to support health initiatives.
constraint_indexing:constraint_classification(cancer_prevention, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WHO AND PUBLIC HEALTH COALITIONS (SCAFFOLD) — Organized agents (WHO, NGO health networks) see prevention barriers as a temporary coordination failure with sunset logic. Carbon tax analogues, sugar taxes, workplace wellness programs represent interim mechanisms. As behavior change norms mature and infrastructure investments scale, the structural barriers degrade.
constraint_indexing:constraint_classification(cancer_prevention, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HEALTHCARE SYSTEM (PITON) — Maintains performative prevention programs (screening, awareness campaigns) while remaining fundamentally structured around treatment reimbursement. Prevention theater persists due to institutional inertia; the system's financial incentives favor late-stage intervention. Theater ratio reflects gap between stated prevention priorities and resource allocation.
constraint_indexing:constraint_classification(cancer_prevention, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION RISK (MOUNTAIN) — Risks framing structural barriers as inherent human nature: short time preference, difficulty changing behavior, inevitable side effects of industrial society. From this view, the prevention gap appears as an immutable constraint on social organization. The engine's false summit detector identifies this as naturalization rather than genuine natural law — behavioral economics and policy design show the barriers are contingent, not inherent.
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
 *   Extractiveness (0.58): Moderately high. The 40% preventable cancer figure represents systematic leaving-on-the-table of health gains. Pharmaceutical and food industry business models depend on maintaining this gap. The extraction is not maximal because genuine coordination functions exist (public health campaigns, screening infrastructure) and some prevention actually occurs. The value reflects that the extraction is embedded in legitimate institutional roles rather than pure coercion. Suppression (0.68): High. Multiple barriers prevent behavior change: food deserts and occupational carcinogens (environmental suppression), financial barriers to healthcare access and time poverty (economic suppression), industry marketing dominance (informational suppression), and healthcare system misalignment (institutional suppression). Barriers are structural, not merely informational. Theater ratio (0.65): Moderate-high. Prevention campaigns emphasize individual responsibility while infrastructure investment remains inadequate. Healthcare systems measure prevention metrics (screening rates, awareness) rather than outcomes (cancer incidence reduction in low-income populations). The gap between stated prevention priorities and resource allocation has widened over the measurement interval as treatment costs escalated.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (pharmaceutical industry) sees Rope — a coordination mechanism enabling their market position. The trapped victim (low-income communities) sees Snare — pure extraction with no exit options. Public health advocates see Tangled Rope — mixed coordination (prevention campaigns, infrastructure) and extraction (industry capture, resource insufficiency). The healthcare system sees Piton — performative prevention programs maintained through institutional inertia, decoupled from reimbursement incentives. The organized coalitions see Scaffold — temporary barriers being addressed through policy interventions with sunset logic (sugar taxes, smoking bans, environmental remediation). The analytical observer risks Mountain — naturalizing behavioral constraints as immutable. The gap arises because different agents have genuinely different exit options and benefit flows. The manufacturer can pivot product lines; the low-income community cannot relocate from a food desert without systemic change.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) is computed from their structural position. Pharmaceutical and food manufacturers have low d values (beneficiaries with arbitrage options) — they experience the constraint as enabling their business model with minimal extraction cost. Low-income populations have high d values (victims with trapped exit) — they experience maximum extraction; prevention knowledge is available but implementation barriers are insurmountable without structural change. Public health advocates have moderate d values (constrained exit, mixed beneficiary/victim status) — they coordinate prevention but are constrained by industry opposition and budget scarcity. The healthcare system has beneficiary status (institutional/arbitrage) but low directionality amplitude because its extraction is performed through institutional inertia rather than overt coercion. The analytical observer risks false summit by naturalizing the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through perspectival pluralism. The mandatrophy question is not 'is this extraction or coordination?' but 'for whom?' The constraint IS coordination for the pharmaceutical industry (their business model is coordinated by prevention barriers) and IS extraction for low-income populations (they are locked out of prevention pathways). The analytical observer's natural law perspective (behavioral constraints are immutable) is a false summit: epidemiological evidence from high-investment contexts (Japan, Costa Rica, Singapore) shows prevention barriers are contingent on policy design and infrastructure, not immutable. The Scaffold and Piton perspectives together suggest a path toward resolution: as public health coalitions build permanent prevention infrastructure and align healthcare reimbursement toward prevention, the extraction mechanism degrades. The constraint resolves from Snare toward Scaffold as structural barriers transition from endemic to temporary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prevention_efficacy_attribution,
    'What fraction of the 40% preventable cancers is prevented by individual behavior change versus population-level structural change (policy, infrastructure, clean air/water)?',
    'Comparative epidemiology across regulatory regimes; analysis of cancer rate changes following policy interventions (smoking bans, sugar taxes, air quality regulations) versus individual health coaching programs',
    'If structural change accounts for >70% of prevention: constraint is primarily policy capture and infrastructure (Tangled Rope/Snare). If individual behavior accounts for >60%: constraint is motivational (Scaffold/Piton with sunset). If 50/50: mixed extraction and coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prevention_efficacy_attribution, empirical, 'Attribution of preventable cancers to behavioral vs structural factors').

omega_variable(
    industry_capture_depth,
    'Are food industry influence and pharmaceutical marketing extractive mechanisms that actively prevent behavior change, or do they merely exploit existing barriers?',
    'Policy analysis of lobbying expenditure vs prevention funding; comparative study of cancer prevention effectiveness in high-capture markets versus low-capture markets; investigation of counterfactual: what would prevention rates be without industry opposition?',
    'If capture is active prevention blocker: extraction severity (suppression) jumps to >0.85, classification shifts to Snare from more perspectives. If capture is exploitation of existing barriers: suppression ~0.60, tangled rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(industry_capture_depth, empirical, 'Whether industry capture actively prevents or passively exploits barriers').

omega_variable(
    lifestyle_change_sustainability,
    'Are the structural barriers genuinely immutable (mountain), or are they overcome by sufficient investment in infrastructure, normalization, and incentive redesign?',
    'Longitudinal study of behavior change in high-investment contexts (Japan smoking rates post-workplace policy; Uruguay tobacco control; Costa Rica preventive care scaling); analysis of cost-benefit of permanent infrastructure investment versus perpetual individual motivation campaigns',
    'If barriers are overcome by investment: Scaffold perspective correct, sunset is feasible within 15-30 years. If barriers persist despite investment: constraint is deeper (snare/tangled rope without exit). Classification hinges on whether the field proves sustainability at scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lifestyle_change_sustainability, empirical, 'Whether structural prevention barriers are surmountable by investment').

omega_variable(
    rebound_effect_magnitude,
    'When prevention campaigns succeed in reducing one cancer risk (e.g., smoking), do populations substitute with another preventable risk (e.g., overeating, sedentary behavior) at rates that offset the prevention gain?',
    'Epidemiological tracking of populations post-smoking-cessation for alcohol consumption and obesity trends; cross-national analysis of cancer rate trends in high-health-literacy populations',
    'If rebound is large (>50% offset): prevention effectiveness is lower than claimed, and the 40% figure is inflated. Constraint becomes less about barriers and more about fundamental limitations. If rebound is small (<20%): barriers are the primary driver, and removal is high-value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebound_effect_magnitude, empirical, 'Magnitude of behavioral rebound effects offsetting prevention gains').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cancer_prevention, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(canc_tr_t0, cancer_prevention, theater_ratio, 0, 0.48).
narrative_ontology:measurement(canc_tr_t10, cancer_prevention, theater_ratio, 10, 0.58).
narrative_ontology:measurement(canc_tr_t20, cancer_prevention, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(canc_be_t0, cancer_prevention, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(canc_be_t10, cancer_prevention, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(canc_be_t20, cancer_prevention, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cancer_prevention, resource_allocation).
narrative_ontology:affects_constraint(cancer_prevention, healthcare_incentive_misalignment).
narrative_ontology:affects_constraint(cancer_prevention, food_system_externalities).
narrative_ontology:affects_constraint(cancer_prevention, occupational_carcinogen_exposure).

% DUAL FORMULATION NOTE:
% The cancer prevention constraint is upstream of specific disease-burden constraints (lung cancer from smoking, colorectal cancer from diet, mesothelioma from asbestos exposure). Each downstream constraint has its own extractiveness reflecting empirical status of disease burden; the prevention constraint has its own extractiveness reflecting the structural barriers to implementing known preventive interventions. The ε values differ because the constraint operates at the system level (barriers to behavior change and infrastructure) rather than the empirical level (causal pathways of specific cancers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cancer_prevention, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
