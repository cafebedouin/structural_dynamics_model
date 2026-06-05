% ============================================================================
% CONSTRAINT STORY: climate_policy_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_policy_extraction, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_policy_extraction
 *   human_readable: The Decarbonization Snare (Climate-Identity Collision)
 *   domain: environmental/political
 *
 * SUMMARY:
 *   As federal and international climate policies intensify, a regional
 *   hydrocarbon-based economy interprets decarbonization as a direct threat
 *   to its wealth and identity. This creates a 'decarbonization snare' where
 *   policies intended to benefit the global climate have extractive and
 *   suppressive effects on specific regions and communities. This extraction
 *   is compounded by the difficulty hydrocarbon workers face in transitioning
 *   to new roles and the limited potential for regional economic
 *   diversification.
 *
 * KEY AGENTS:
 *   - Hydrocarbon Workers: Primary victim (powerless/trapped) - face job losses and economic hardship.
 *   - Regional Hydrocarbon Economy: Secondary victim (moderate/constrained) - experiences decline in traditional industries.
 *   - Renewable Energy Sector: Primary beneficiary (institutional/arbitrage) - benefits from policy support and investment.
 *   - Climate Policy Advocates: Beneficiary (organized/mobile) -advancing goals and agendas.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_policy_extraction, 0.65).
domain_priors:suppression_score(climate_policy_extraction, 0.7).
domain_priors:theater_ratio(climate_policy_extraction, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_policy_extraction, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_policy_extraction, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_policy_extraction, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_policy_extraction, snare).
narrative_ontology:human_readable(climate_policy_extraction, "The Decarbonization Snare (Climate-Identity Collision)").
narrative_ontology:topic_domain(climate_policy_extraction, "environmental/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_policy_extraction, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(climate_policy_extraction, climate_policy_advocates).
narrative_ontology:constraint_victim(climate_policy_extraction, hydrocarbon_workers).
narrative_ontology:constraint_victim(climate_policy_extraction, regional_hydrocarbon_economy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Hydrocarbon workers face job losses and economic hardship due to decarbonization policies. Their skills are not easily transferable to the renewable energy sector, leaving them trapped. d=0.95, f(d)≈1.42, σ=0.9, χ ≈ 0.83
constraint_indexing:constraint_classification(climate_policy_extraction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% The regional hydrocarbon economy experiences extraction as its traditional industries decline but may also benefit from some diversification and transition funding. Constrained exit options as they are tied to existing infrastructure. d=0.75, f(d)≈1.10, σ=0.9, χ ≈ 0.64
constraint_indexing:constraint_classification(climate_policy_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% The renewable energy sector benefits from increased investment and policy support, leading to growth and expansion. Arbitrage opportunities in new markets. d=0.05, f(d)≈-0.12, σ=1.2, χ ≈ -0.05
constraint_indexing:constraint_classification(climate_policy_extraction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Climate policy advocates benefit from the success of decarbonization policies in advancing their goals and agendas. Mobile in terms of policy focus and strategies. d=0.15, f(d)≈-0.01, σ=1.2, χ ≈ -0.01
constraint_indexing:constraint_classification(climate_policy_extraction, rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% Analytical perspective sees both coordination (global climate goals) and extraction (regional economic impacts) components. d=0.72, f(d)≈1.15, σ=1.2, χ ≈ 0.54
constraint_indexing:constraint_classification(climate_policy_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_policy_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_policy_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_policy_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_policy_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_policy_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The policies extract significant economic value from the hydrocarbon-dependent regions. Suppression (0.70): High. Limited alternative options and high barriers to entry in new sectors. Theater Ratio (0.30): Low. The transition programs are implemented, but their effectiveness and impact are limited. Focus is on tangible policy changes, not performative action.
 *
 * PERSPECTIVAL GAP:
 *   The hydrocarbon workers view the policies as a snare, trapping them in a declining industry. The renewable energy sector sees them as a rope, facilitating growth and development. The analytical perspective recognizes both the coordination (global climate goals) and extraction (regional economic impacts) components.
 *
 * DIRECTIONALITY LOGIC:
 *   Hydrocarbon workers: Victim + trapped -> d=0.95 -> Snare. Regional Hydrocarbon economy: Victim + constrained -> Tangled Rope. Renewable energy: Beneficiary + arbitrage -> d=0.05 -> Rope. Climate Policy Advocates: Beneficiary + mobile -> d=0.15 -> Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is classified as a snare due to the high extractiveness and suppression experienced by hydrocarbon workers and the regional economy. It prevents mislabeling as a pure coordination problem (rope) by recognizing the real and significant negative impacts on specific communities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    job_transition_feasibility,
    'How feasible is it for hydrocarbon workers to transition to jobs in the renewable energy sector?',
    'Skills gap analysis, retraining program effectiveness, and labor market demand forecasts.',
    'High feasibility: reduces the ''trapped'' classification for workers. Low feasibility: reinforces the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(job_transition_feasibility, empirical, 'Feasibility of job transition for hydrocarbon workers').

omega_variable(
    regional_diversification_potential,
    'What is the potential for regional economic diversification beyond the hydrocarbon industry?',
    'Industry analysis, investment climate assessments, and regional development plans.',
    'High potential: lessens the extraction on the regional economy. Low potential: strengthens the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_diversification_potential, empirical, 'Potential for regional economic diversification').

omega_variable(
    policy_design_equity,
    'To what extent are decarbonization policies designed to mitigate negative impacts on affected communities?',
    'Policy impact assessments, stakeholder engagement, and equity audits.',
    'High equity: reduces the suppression and extraction. Low equity: exacerbates the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_design_equity, conceptual, 'Equity in decarbonization policy design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_policy_extraction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_policy_extraction, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t10, climate_policy_extraction, theater_ratio, 10, 0.2).
narrative_ontology:measurement(clim_tr_t20, climate_policy_extraction, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_policy_extraction, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(clim_be_t10, climate_policy_extraction, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(clim_be_t20, climate_policy_extraction, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(climate_policy_extraction, global_carbon_pricing).
narrative_ontology:affects_constraint(climate_policy_extraction, fossil_fuel_subsidies).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
