% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__precarity_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__precarity_extraction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: flexible_employment_legitimacy__precarity_extraction_reading
 *   human_readable: Flexible Employment as Structural Precarity Enabling Platform Extraction
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'precarity_extraction_reading' of
 *   the contested kernel 'flexible_employment_legitimacy'. The kernel is the
 *   concept of flexible employment itself — a stabilized commitment in labor
 *   policy and platform discourse that 'flexibility' names a legitimate,
 *   efficiency-enhancing coordination form. Three readings contest its
 *   meaning: (1) market_efficiency_reading treats flexibility as a
 *   Pareto-improving market-clearing mechanism; (2)
 *   developmental_state_reading treats it as a transitional informality
 *   requiring state management toward formalization; (3) this reading,
 *   precarity_extraction_reading, treats it as structural precarity
 *   engineered to externalize risk and extract surplus value. The constraint
 *   described here is the standing arrangement of platform-mediated flexible
 *   labor as it operates under this reading's assessment: algorithmic control
 *   as discipline, wage gains offset by risk externalization, social security
 *   gaps as cost-shifting. The referent for ε is this standing arrangement,
 *   not the reading's endorsed alternative (which would be formal employment
 *   with algorithmic transparency and portable benefits).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, 0.75).
domain_priors:suppression_score(flexible_employment_legitimacy__precarity_extraction_reading, 0.72).
domain_priors:theater_ratio(flexible_employment_legitimacy__precarity_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__precarity_extraction_reading, tangled_rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__precarity_extraction_reading, "Flexible Employment as Structural Precarity Enabling Platform Extraction").
narrative_ontology:topic_domain(flexible_employment_legitimacy__precarity_extraction_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__precarity_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__precarity_extraction_reading, 'b19366b1-b184-4465-a719-9da3715bab8a').
narrative_ontology:cs_kernel_codification('b19366b1-b184-4465-a719-9da3715bab8a', distributed).
narrative_ontology:cs_authority_grounding('b19366b1-b184-4465-a719-9da3715bab8a', extraction).
narrative_ontology:cs_interpretation_layer_present('b19366b1-b184-4465-a719-9da3715bab8a').
narrative_ontology:cs_reading_relation('b19366b1-b184-4465-a719-9da3715bab8a', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('b19366b1-b184-4465-a719-9da3715bab8a', flexible_employment_legitimacy__developmental_state_reading, influences).
narrative_ontology:cs_axiom('b19366b1-b184-4465-a719-9da3715bab8a', foundational, flexibility_is_engineered_precarity).
narrative_ontology:cs_axiom_status(flexibility_is_engineered_precarity, holdable).
narrative_ontology:cs_axiom_grounding('b19366b1-b184-4465-a719-9da3715bab8a', flexibility_is_engineered_precarity, empirically_contingent).
narrative_ontology:cs_axiom('b19366b1-b184-4465-a719-9da3715bab8a', foundational, algorithmic_control_externalizes_reproduction_costs).
narrative_ontology:cs_axiom_status(algorithmic_control_externalizes_reproduction_costs, holdable).
narrative_ontology:cs_axiom_grounding('b19366b1-b184-4465-a719-9da3715bab8a', algorithmic_control_externalizes_reproduction_costs, empirically_contingent).
narrative_ontology:cs_axiom('b19366b1-b184-4465-a719-9da3715bab8a', secondary, independent_classification_is_extraction_mechanism).
narrative_ontology:cs_axiom_status(independent_classification_is_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('b19366b1-b184-4465-a719-9da3715bab8a', independent_classification_is_extraction_mechanism, conventional).
narrative_ontology:cs_reference_frame('b19366b1-b184-4465-a719-9da3715bab8a', platform_flexibility_paradigm).
narrative_ontology:cs_drift_state('b19366b1-b184-4465-a719-9da3715bab8a', post_algorithmically_mediated_labor_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b19366b1-b184-4465-a719-9da3715bab8a', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_companies).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, investor_classes).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, venture_capital_firms).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, platform_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, precarious_labor_force).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, social_security_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_workers).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, traditional_employers).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, platform_capitalism_legitimacy).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, algorithmic_management_efficiency).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, flexibility_as_innovation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce the algorithmic infrastructure that governs work allocation, pay rates, deactivation, and performance metrics. Collect surplus value through commission fees, data extraction, and risk externalization to workers. Can relocate incorporation, shift jurisdictional exposure, and restructure entities to avoid regulation.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).

% Fund platform growth predicated on low marginal labor costs and rapid scaling without employment liabilities. Returns depend on the classification of workers as independent contractors. Exit through IPO, acquisition, or secondary sales before regulatory risk materializes.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, venture_capital_firms, beneficiary,
    institutional, biographical, arbitrage, global).

% Hold equity in publicly traded and private platform companies. Benefit from capital appreciation driven by labor cost arbitrage. Portfolio diversification allows exit from any single platform's regulatory exposure.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, investor_classes, beneficiary,
    powerful, biographical, mobile, global).

% Perform labor under algorithmic control: acceptance rates, ratings, and behavioral compliance determine access to work. Bear all operational costs (vehicle, insurance, health, retirement) while receiving piece-rate pay net of platform commissions. Formal exit exists but is constrained by local labor market alternatives, skill specificity, and income volatility. Some value schedule flexibility as a genuine benefit.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_workers, payer,
    powerless, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__precarity_extraction_reading, platform_workers, beneficiary).

% Workers in adjacent sectors (delivery, ride-hail, care, creative) whose bargaining position is eroded by the platform benchmark. The existence of a 'flexible' outside option disciplines wage demands and unionization efforts across the low-wage labor market. Exit from the precarious condition requires structural labor market change, not individual job change.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, precarious_labor_force, payer,
    moderate, biographical, identity_locked, national).

% Absorb the externalized costs of platform labor: uninsured health events, unemployment without contributions, pension gaps, and workplace injury without workers' compensation. Funded by contributions from formal employers and general taxation; cannot opt out of covering the population. The cost-shifting is structural — platforms do not contribute proportionally to the risks they generate.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, social_security_systems, payer,
    institutional, generational, trapped, national).

% Enforce employment classification, minimum wage, and safety standards. Face jurisdictional limits against global platforms, evidentiary barriers to algorithmic opacity, and political pressure from innovation narratives. Can impose reclassification remedies but enforcement lags platform adaptation.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_regulators, observer,
    institutional, generational, analytical, national).

% Benefit from the competitive pressure platform labor exerts on wage floors and benefit expectations in adjacent sectors. Some adopt platform-like scheduling and classification practices. Can exit by automating, offshoring, or lobbying for regulatory parity.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, traditional_employers, beneficiary,
    organized, biographical, mobile, national).

% Unions, worker centers, and algorithmic accountability collectives that would challenge the classification and control structure but are structurally excluded from platform governance, algorithmic design, and regulatory negotiation tables. Their exclusion is maintained by the independent contractor classification that denies collective bargaining standing.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, excluded_worker_organizations, excluded,
    organized, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches heterogeneous, on-demand labor supply to heterogeneous, on-demand service demand in real time across a two-sided market, solving search, trust, and payment coordination that would be costly for individual workers and consumers to replicate.
% TRANSFER_FUNCTION: Moves surplus value from platform workers (via commission fees, unpaid waiting time, externalized operational costs, algorithmic wage suppression, and denied statutory protections) to platform companies and their investors. Simultaneously shifts social insurance costs (health, unemployment, pension, injury) from platforms to public systems and workers themselves.
% ABSENT_VOICES: Workers in the Global South performing platform labor (data annotation, content moderation, microtasking) under even more extractive conditions; undocumented workers funneled into platform work due to formal employment barriers; future cohorts who will inherit the normalized precarity and eroded social contract. They are absent from the regulatory conversation in the Global North where platform rules are set.
% DISAPPEARANCE_RATIONALE: If the platform labor model and its independent contractor classification vanished overnight, platforms would face immediate reclassification costs (benefits, insurance, wages), labor supply would shrink as the 'flexibility' marginal workers exit, consumer prices would rise, and the venture funding model predicated on labor arbitrage would collapse. The low-wage labor market would reorganize around formal employment or alternative informal structures. Social security systems would see reduced cost-shifting but increased contribution bases.
% FOUNDING_PROBLEM: The coordination of highly fragmented, variable, and low-commitment service demand (rides, deliveries, tasks) with a labor supply that cannot be efficiently organized through traditional firm boundaries or shift scheduling. The claimed innovation was reducing transaction costs to near-zero for peer-to-peer service exchange.
% FOUNDING_PROBLEM_CORROBORATION: Platform companies and venture capital attest the coordination problem remains live and growing (new verticals, AI-assisted matching). Independent economic researchers (e.g., Chen et al. 2023 on Uber/Lyft; Berg et al. ILO 2021 on platform work conditions; UK Supreme Court Uber judgment 2021; California AB5/Prop 22 battle) attest the coordination function is real but the extraction-to-coordination ratio has shifted decisively toward extraction as platforms matured and established market power. No credible independent source claims the founding problem is 'dead' — the contestation is over whether the current arrangement is a necessary or proportionate solution.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__precarity_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__precarity_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__precarity_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.75) is high because the platform model systematically externalizes the costs of labor reproduction (health, retirement, unemployment, injury, capital equipment) onto workers and public systems while capturing the surplus from algorithmic optimization of dispatch, pricing, and behavioral compliance. Suppression (0.72) is high because the arrangement depends on active enforcement of independent contractor classification (litigation, lobbying, Prop 22-style ballot measures), algorithmic opacity that prevents workers from verifying pay logic, and the structural exclusion of worker organizations from governance. Theater ratio (0.42) reflects that the coordination function (matching, payment, trust) is real but a declining share of platform activity — the dominant engineering and policy effort defends the classification and control structure. Accessibility collapse (0.65) is moderate-high: formal employment alternatives exist but are eroded by the platform benchmark; workers cannot easily 'choose' formal jobs when the precarious sector disciplines the whole low-wage market. Resistance (0.58) is significant: worker organizing (Uber Drivers Union, Algorithmic Justice League, legislative campaigns), regulatory action (EU Platform Work Directive, UK Supreme Court, California AB5), and academic critique sustain pressure but have not reversed the structural trajectory.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat types: from the platform seat (agenda_setter, institutional, arbitrage), the constraint computes toward Rope or low-extraction Tangled Rope (coordination function visible, extraction denied). From the platform_worker seat (payer, powerless, constrained), it computes toward Snare or high-extraction Tangled Rope (extraction experienced, alternatives suppressed). From the social_security_systems seat (payer, institutional, trapped), it computes toward Snare (pure cost-shifting with no coordination benefit). From the precarious_labor_force seat (payer, moderate, identity_locked), it computes toward Tangled Rope with high effective extraction (the coordination benefit is negative — the 'outside option' disciplines them). This divergence is the measurement: the same constraint is experienced as coordination by its architects and extraction by its subjects.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies and their investors are structural beneficiaries (d near 0.0): they set the rules, collect the surplus, and hold arbitrage-grade exit (jurisdictional shopping, corporate restructuring). Platform workers are structural targets (d near 1.0): they bear the extracted costs, face constrained exit (local labor market dependence, skill specificity, income volatility), and some value flexibility as a genuine benefit (secondary_role: beneficiary) — this dual position is real but does not negate the net extraction. The precarious labor force is identity_locked: their self-concept and bargaining posture are constituted through the 'flexible' outside option, making exit from the condition structurally difficult even for non-platform workers. Social security systems are trapped: they cannot opt out of covering the externalized population. Labor regulators are analytical observers. Traditional employers are mobile beneficiaries of the wage-disciplining effect. Excluded worker organizations are trapped by the classification that denies them standing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating fragmented on-demand labor) remains live but the arrangement has metastasized: the coordination infrastructure now primarily serves to discipline labor costs and extract data rents. The mandate has not been resolved — it has been captured. The original coordination function is real but subordinate to the extraction function. This is not a Piton (inertial persistence of a dead function) — the extraction function is active, enforced, and expanding. It is a Tangled Rope where the coordination cover story is maintained precisely because it legitimizes the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Does the ''flexible_employment_legitimacy'' kernel represent a single contested concept with three readings, or three distinct constraints sharing a label?',
    'Test ε-invariance: if measuring the constraint via market-efficiency observables (matching speed, consumer surplus) yields low ε but measuring via worker-outcome observables (risk exposure, surplus capture) yields high ε, the kernel conflates distinct constraints. Decompose into separate stories per ε-invariance principle.',
    'If the kernel is one concept, the three readings are perspectival frames on one constraint. If three constraints, each reading instantiates a different constraint with its own ε, stakeholders, and classification. This story assumes the latter (per ε-invariance principle) but the boundary is contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the kernel is one constraint with three readings or three constraints with one label.').

omega_variable(
    algorithmic_control_as_discipline,
    'Is algorithmic management (deactivation, acceptance-rate enforcement, behavioral nudging) structurally distinct from traditional managerial control, or a technologically intensified version of the same extraction logic?',
    'Compare the information asymmetry, speed of enforcement, and reversibility of algorithmic vs. human management decisions. Empirical studies of worker experience under algorithmic vs. human supervision (e.g., Woodcock & Graham 2020; Lee et al. 2022).',
    'If structurally distinct, the constraint introduces a novel suppression mechanism (algorithmic opacity + speed + scale) that warrants higher suppression scoring. If continuous with traditional control, the extraction logic is not novel — only the intensity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_control_as_discipline, empirical, 'Novelty of algorithmic control as a suppression mechanism.').

omega_variable(
    cost_shifting_quantification,
    'What is the quantitative magnitude of social insurance cost-shifting from platforms to public systems per platform worker?',
    'Actuarial estimation of foregone contributions (unemployment insurance, workers'' comp, pension, employer-side payroll taxes) for the platform workforce vs. a matched formal-employment cohort. Parliamentary budget office analyses (e.g., UK OBR, US CBO) or independent fiscal institutes.',
    'A quantified cost-shift would ground the ''social_security_systems as payer'' claim in measurable transfer, strengthening the Tangled Rope classification. Without it, the cost-shifting remains a structural claim without a magnitude.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_shifting_quantification, empirical, 'Magnitude of externalized social insurance costs.').

omega_variable(
    flexibility_valued_by_workers,
    'What proportion of platform workers genuinely value schedule flexibility as a net benefit after accounting for income volatility, unpaid waiting time, and absent protections?',
    'Revealed-preference studies: do workers choose platform work over available formal jobs at comparable nominal wages? Survey experiments with full attribute disclosure (pay net of costs, volatility, benefits, algorithmic control).',
    'If flexibility is genuinely valued by a substantial minority, the secondary_role: beneficiary for platform_workers is empirically grounded. If it is a ''preference adaptation'' to constrained options, the beneficiary role is illusory and the constraint is closer to Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flexibility_valued_by_workers, empirical, 'Whether schedule flexibility is a genuine benefit or adaptive preference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__precarity_extraction_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fel_per_tr_t0, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fel_per_tr_t3, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 3, 0.3).
narrative_ontology:measurement(fel_per_tr_t6, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(fel_per_tr_t9, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 9, 0.38).
narrative_ontology:measurement(fel_per_tr_t12, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(fel_per_tr_t15, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 15, 0.42).

% Extraction over time
narrative_ontology:measurement(fel_per_be_t0, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fel_per_be_t3, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(fel_per_be_t6, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(fel_per_be_t9, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 9, 0.68).
narrative_ontology:measurement(fel_per_be_t12, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(fel_per_be_t15, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 15, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(fel_per_su_t0, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fel_per_su_t3, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(fel_per_su_t6, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(fel_per_su_t9, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 9, 0.68).
narrative_ontology:measurement(fel_per_su_t12, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(fel_per_su_t15, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__precarity_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__precarity_extraction_reading, 0.12).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy__developmental_state_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, algorithmic_management_discipline).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, social_insurance_cost_shifting).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, worker_classification_litigation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the flexible_employment_legitimacy kernel. The market_efficiency_reading treats flexibility as a coordination good (Rope/Mountain). The developmental_state_reading treats it as a transitional informality (Scaffold). This reading treats it as structural precarity enabling extraction (Tangled Rope/Snare). The ε values differ substantially: market_efficiency_reading would author ε ~0.2; developmental_state_reading ε ~0.4; this reading authors ε = 0.75. They are linked via affects_constraints because the market_efficiency claim is cited as evidence for the legitimacy of the arrangement this reading contests, and the developmental_state_reading proposes a state-management path that this reading views as insufficient without reclassification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(flexible_employment_legitimacy__precarity_extraction_reading, institutional, 0.1).
constraint_indexing:directionality_override(flexible_employment_legitimacy__precarity_extraction_reading, powerless, 0.9).
constraint_indexing:directionality_override(flexible_employment_legitimacy__precarity_extraction_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
