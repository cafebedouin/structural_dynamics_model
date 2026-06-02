% ============================================================================
% CONSTRAINT STORY: occupational_health_inequality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_occupational_health_inequality, []).

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
 *   constraint_id: occupational_health_inequality
 *   human_readable: Occupational Health Inequality
 *   domain: labor/health/economics
 *
 * SUMMARY:
 *   Occupational health inequality represents a structural tension between
 *   the coordination necessity of work organization and the extractive
 *   asymmetry of health risk distribution. Workers in hazardous sectors
 *   (construction, mining, agriculture, food processing, caregiving)
 *   systematically experience higher exposure to physical, chemical, and
 *   biological hazards than workers in office environments. This exposure is
 *   neither random nor technically necessary — it reflects deliberate firm
 *   decisions to concentrate hazards among workers with the least exit
 *   capacity (lowest wage workers, precarious workers, undocumented workers,
 *   workers in low-income regions). The constraint functions as both a
 *   genuine coordination mechanism (production requires some baseline safety
 *   to prevent catastrophic worker loss) and a pure extraction mechanism
 *   (firms suppress hazard disclosure, externalize health costs, and prevent
 *   worker organization). The theater ratio (0.55) reflects that occupational
 *   health enforcement is substantially performative: inspections are
 *   infrequent (often 0.2–0.5 inspections per firm per year), penalties are
 *   minor (often less than 1% of firm revenue), worker reporting mechanisms
 *   are weak (retaliation risks outweigh disclosure benefits), and
 *   occupational illness is routinely misclassified as personal health
 *   failure rather than workplace injury.
 *
 * KEY AGENTS:
 *   - Precarious Workers: Primary victims (powerless/trapped) — lack skills, savings, and geographic mobility to exit hazardous work; bear full health costs; lack access to medical care through workplace insurance
 *   - Low-Wage Workers: Primary victims (moderate/constrained) — have some exit options but face substantial costs (skill retraining time, wage loss, relocation); experience both coordination benefits and extraction
 *   - Capital Owners/Firms: Primary beneficiaries (institutional/arbitrage) — capture productivity surplus while externalizing health costs; can offshore hazardous production to lower-regulation zones; can suppress hazard disclosure through information asymmetry
 *   - Labor Unions: Secondary institutional actors (organized/arbitrage) — coordinate safety standards and collective bargaining but benefit from occupational health as coordination mechanism; captured in some jurisdictions
 *   - Regulatory Agencies: Institutional actors (institutional/arbitrage) — maintain occupational health standards but experience capture and underfunding; can arbitrage regulatory stringency across jurisdictions
 *   - Occupational Health Bureaucracy: Institutional actor (institutional/constrained) — performs enforcement ritual but lacks resources and political power to challenge firm interests
 *   - Worker Health Commons: Victim collective (powerless/trapped) — abstract good whose degradation is externalized and uncompensated; cannot organize or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(occupational_health_inequality, 0.58).
domain_priors:suppression_score(occupational_health_inequality, 0.68).
domain_priors:theater_ratio(occupational_health_inequality, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(occupational_health_inequality, extractiveness, 0.58).
narrative_ontology:constraint_metric(occupational_health_inequality, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(occupational_health_inequality, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(occupational_health_inequality, tangled_rope).
narrative_ontology:human_readable(occupational_health_inequality, "Occupational Health Inequality").
narrative_ontology:topic_domain(occupational_health_inequality, "labor/health/economics").

domain_priors:requires_active_enforcement(occupational_health_inequality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(occupational_health_inequality, capital_owners).
narrative_ontology:constraint_beneficiary(occupational_health_inequality, regulatory_agencies).
narrative_ontology:constraint_victim(occupational_health_inequality, precarious_workers).
narrative_ontology:constraint_victim(occupational_health_inequality, low_wage_workers).
narrative_ontology:constraint_victim(occupational_health_inequality, worker_health_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (SNARE) — Trapped by economic dependency, skill specificity, and geographic constraints. No affordable exit from hazardous work. Bears full health costs of occupational exposure. Suppression is structural: job availability concentrated in high-hazard sectors, medical costs consume earnings, skills not transferable to safer work.
constraint_indexing:constraint_classification(occupational_health_inequality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WAGE WORKER (TANGLED ROPE) — Constrained by need for continuous income and family obligations. Experiences both coordination (workplace safety norms, collective bargaining when available) and extraction (unequal health exposure, inadequate hazard pay, speed-up pressure). Has some agency through unionization or job mobility within constrained set of options.
constraint_indexing:constraint_classification(occupational_health_inequality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LABOR UNION (ROPE) — Organized agents can arbitrage between jurisdictions, collective bargaining, and regulatory capture. Benefits from occupational health as coordination mechanism: safety standards reduce turnover, health costs are diffused across collective, regulatory framework provides stable negotiating ground. Can exit regulatory regime through lobbying or jurisdictional arbitrage.
constraint_indexing:constraint_classification(occupational_health_inequality, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FIRM (TANGLED ROPE) — Coordinates production and labor supply while extracting from workers through wage suppression and externalized health costs. Genuine coordination function: production requires safe-enough conditions (absenteeism, turnover, product defects from fatigue). Asymmetric extraction: workers bear health risks, firm captures productivity surplus and can offshore hazardous processes to lower-regulation zones.
constraint_indexing:constraint_classification(occupational_health_inequality, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AGENCY (ROPE) — Coordinates occupational health standards across firms. Experiences constraint as pure coordination problem: setting hazard exposure limits, enforcement standards, certification requirements. Benefits from institutional legitimacy and budget stability. Can arbitrage regulatory capture and regulatory stringency.
constraint_indexing:constraint_classification(occupational_health_inequality, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: OCCUPATIONAL HEALTH BUREAUCRACY (PITON) — Institutional theater: occupational health enforcement is substantially performative. Inspections are infrequent, penalties are minor relative to compliance costs, worker reporting mechanisms are weak. Bureaucracy persists through institutional inertia (ILO conventions, national labor codes) despite low functional verification. Workers receive theater of protection while bearing actual risks.
constraint_indexing:constraint_classification(occupational_health_inequality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/global perspective, occupational health inequality is a genuine hybrid. Coordination function: work organization requires some baseline health standards to prevent catastrophic worker loss. Extraction function: capitalist production externalizes health costs onto workers because wages do not reflect health risk, and regulatory capture allows firms to suppress hazard disclosure. The constraint is held in place by both coordination necessity and extractive advantage.
constraint_indexing:constraint_classification(occupational_health_inequality, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(occupational_health_inequality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(occupational_health_inequality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(occupational_health_inequality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(occupational_health_inequality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(occupational_health_inequality, TR),
    TR >= 0.70.

:- end_tests(occupational_health_inequality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Firms extract from workers by concentrating hazards among powerless groups, suppressing hazard disclosure, preventing collective organization, and externalizing health costs. The extraction is not maximal (0.9+) because genuine coordination functions exist — production requires some baseline worker health to prevent catastrophic turnover — and some regulatory standards and collective bargaining do reduce hazards. The measurement shows extractiveness increasing over 40 years as automation and globalization have allowed firms to concentrate hazards in low-wage sectors and offshore hazardous work to lower-regulation zones. Suppression (0.68): High. Substantial barriers prevent workers from exiting hazardous work: job availability concentrated in high-hazard sectors, retraining costs and wage loss during transition, geographic immobility, dependence on employer-linked health insurance (in US context), legal restrictions on undocumented workers, and weak worker-to-worker information about hazards (firms suppress hazard disclosure). Theater ratio (0.55): Moderate. Occupational health enforcement is substantially performative — regulatory inspections are infrequent, penalties are minor, worker reporting is suppressed by retaliation risk. But not maximal (0.8+) because some genuine safety improvements occur through collective bargaining and regulation (e.g., asbestos bans, chemical controls). The theater has increased over time as enforcement underfunding has reduced inspection frequency.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the precarious worker (sees snare: pure extraction with no benefit and no exit) and the firm (sees rope: coordination mechanism where safety enables production). This gap is diagnostic of asymmetric extraction: one party bears costs while the other captures benefits. The secondary gap is between the analytical observer (sees tangled rope: both genuine coordination and genuine extraction) and the regulatory agency (sees rope: pure coordination). This gap reveals that regulatory capture has made enforcement performative — the agency experiences pure coordination while workers experience mixed extraction + coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Precarious workers with trapped exit options are full targets (d ≈ 0.95) → f(d) ≈ 1.42 → high experienced chi. Capital owners with arbitrage options are full beneficiaries (d ≈ 0.05) → f(d) ≈ -0.12 → negative experienced chi (they benefit). Regulated firms have intermediate d (0.4–0.6) depending on whether enforcement is credible or captured. The measurement trend (extractiveness rising while theater rises) suggests that as enforcement becomes more performative, firms experience lower d (enforcement becomes less costly) while workers experience higher d (they cannot exit easier even if they perceive the constraint as pure extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing genuine coordination (baseline worker health necessary for production) from pure extraction (concentration of hazards among powerless groups, wage suppression, hazard suppression). The classification is tangled rope because BOTH functions exist and are structurally essential: firms genuinely need workers alive and conscious enough to work, AND firms genuinely benefit from concentrating hazards where they can suppress information about them. The mandatrophy is avoided because the constraint's primary function is not pure extraction disguised as coordination — it IS a genuine coordination mechanism that IS simultaneously extractive. The theater (0.55) prevents this from being classified as rope: occupational health enforcement is substantially performative, suggesting that some firms use regulatory theater to create the appearance of hazard control while extracting from worker health. The measurements showing theater_ratio increasing from 0.38 to 0.55 suggest that enforcement has become MORE performative over time even as extractiveness has increased — a pattern consistent with Goodhart degradation (metrics are gamed to show compliance while underlying function deteriorates).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externalization_threshold,
    'At what wage premium does occupational health risk become fairly compensated hazard pay rather than extraction?',
    'Comparative wage analysis: compensating differential methodology applied across hazard levels; worker willingness-to-pay for safety vs offered wage premiums',
    'If threshold < 10% premium: most occupational risk is uncompensated extraction. If threshold > 30%: risk premium may justify some inequality as legitimate market signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalization_threshold, empirical, 'Threshold for fair hazard pay vs extractive wage suppression').

omega_variable(
    substitutability_of_hazard_labor,
    'How much of occupational health inequality reflects economic necessity (hazardous work requires this worker type) vs extractive structuring (firms deliberately concentrate hazards among powerless groups)?',
    'Technology feasibility analysis: identification of engineering solutions that could reduce hazards but are not adopted due to cost; comparison of hazard exposure across worker demographics in same job categories',
    'If substitutability high: extraction is significant (same work could be made safer). If substitutability low: some inequality reflects task necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitutability_of_hazard_labor, empirical, 'Whether hazard concentration reflects necessity or deliberate extraction structuring').

omega_variable(
    enforcement_capture_mechanism,
    'Does occupational health enforcement failure stem from insufficient inspection capacity (underfunding) or from regulatory capture (inspectors shield firms)?',
    'Comparative analysis of enforcement resources vs caseload; inspector career paths and post-agency employment; correlation between inspection frequency and firm profitability',
    'If underfunding: scaffold perspective valid (increased resources could improve function). If capture: snare perspective valid (enforcement theater is intentional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capture_mechanism, empirical, 'Whether enforcement failure is resource-driven or capture-driven').

omega_variable(
    health_commons_recovery,
    'To what extent can worker health damage from occupational exposure be recovered through medical intervention vs permanent loss?',
    'Longitudinal health outcome studies: recovery rates for occupational illnesses; comparison of pre-exposure to post-treatment health status; irreversibility analysis by hazard type',
    'If recovery low: extraction is irreversible (worker bears permanent cost). If recovery high: extraction is mitigated (health can be restored).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(health_commons_recovery, empirical, 'Reversibility of occupational health damage through medical intervention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(occupational_health_inequality, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(occhealth_tr_t0, occupational_health_inequality, theater_ratio, 0, 0.38).
narrative_ontology:measurement(occhealth_tr_t20, occupational_health_inequality, theater_ratio, 20, 0.48).
narrative_ontology:measurement(occhealth_tr_t40, occupational_health_inequality, theater_ratio, 40, 0.55).
narrative_ontology:measurement(occhealth_tr_t10, occupational_health_inequality, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(occhealth_be_t0, occupational_health_inequality, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(occhealth_be_t20, occupational_health_inequality, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(occhealth_be_t40, occupational_health_inequality, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(occhealth_be_t10, occupational_health_inequality, base_extractiveness, 10, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(occupational_health_inequality, resource_allocation).
narrative_ontology:affects_constraint(occupational_health_inequality, wage_suppression_by_precarity).
narrative_ontology:affects_constraint(occupational_health_inequality, worker_organizing_suppression).
narrative_ontology:affects_constraint(occupational_health_inequality, occupational_illness_externalization).

% DUAL FORMULATION NOTE:
% Occupational health inequality is upstream of specific injury/illness constraints. The health inequality constraint has its own ε reflecting the structural concentration of hazards; downstream constraints reflect specific mechanisms (wage suppression, organizing suppression, medical cost externalization) through which the inequality is maintained. All three are linked because addressing any one requires addressing the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(occupational_health_inequality, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
