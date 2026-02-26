% ============================================================================
% CONSTRAINT STORY: boe_base_rate_policy_2024
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_boe_base_rate_policy_2024, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: boe_base_rate_policy_2024
 *   human_readable: Bank of England's 5.25% Base Interest Rate Policy (2024)
 *   domain: economic
 *
 * SUMMARY:
 *   The Bank of England's policy of maintaining a 5.25% base interest rate is
 *   a textbook example of a macroeconomic constraint with a dual function.
 *   Its stated purpose is coordination: to reduce inflation towards a 2%
 *   target by increasing the cost of borrowing, thereby cooling economic
 *   demand. However, this mechanism operates through significant, asymmetric
 *   extraction, transferring wealth from borrowers (households with
 *   mortgages, businesses with loans) to savers and lenders. The constraint
 *   is enforced by the Bank's monopoly power over the UK's monetary system,
 *   leaving domestic actors with no alternative.
 *
 * KEY AGENTS:
 *   - Bank of England (MPC): Institutional enforcer (institutional/arbitrage) — views the policy as a necessary coordination tool.
 *   - Mortgage Holders & Business Borrowers: Primary victims (powerless/trapped) — bear the direct cost of higher interest payments.
 *   - Savers & Lenders: Primary beneficiaries (moderate/mobile) — receive higher returns on capital.
 *   - The UK Government: A constrained institutional actor (institutional/constrained) — benefits from inflation control but is harmed by higher debt servicing costs.
 *   - Analytical Observer: Sees the full hybrid structure (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(boe_base_rate_policy_2024, 0.55).
domain_priors:suppression_score(boe_base_rate_policy_2024, 0.8).
domain_priors:theater_ratio(boe_base_rate_policy_2024, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(boe_base_rate_policy_2024, extractiveness, 0.55).
narrative_ontology:constraint_metric(boe_base_rate_policy_2024, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(boe_base_rate_policy_2024, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(boe_base_rate_policy_2024, tangled_rope).
narrative_ontology:human_readable(boe_base_rate_policy_2024, "Bank of England's 5.25% Base Interest Rate Policy (2024)").
narrative_ontology:topic_domain(boe_base_rate_policy_2024, "economic").

domain_priors:requires_active_enforcement(boe_base_rate_policy_2024).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(boe_base_rate_policy_2024, savers).
narrative_ontology:constraint_beneficiary(boe_base_rate_policy_2024, commercial_banks).
narrative_ontology:constraint_beneficiary(boe_base_rate_policy_2024, fixed_income_investors).
narrative_ontology:constraint_victim(boe_base_rate_policy_2024, mortgage_holders).
narrative_ontology:constraint_victim(boe_base_rate_policy_2024, small_business_borrowers).
narrative_ontology:constraint_victim(boe_base_rate_policy_2024, the_uk_treasury).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MORTGAGE HOLDER (SNARE) — Faces sharply increased monthly payments with no ability to exit their mortgage or influence the rate. The policy acts as a direct, coercive extraction of disposable income. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SAVER (ROPE) — Benefits from higher returns on cash deposits. Perceives the policy as a functional coordination mechanism that correctly prices capital and rewards saving. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.005. Negative effective extraction indicates a net beneficiary.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: BANK OF ENGLAND (ROPE) — The institution setting the rate views it as its primary tool for macroeconomic coordination, fulfilling its mandate to control inflation. The extractive effects are seen as necessary, temporary side-effects of a legitimate policy. As the primary beneficiary of its own mandate, d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: UK GOVERNMENT (TANGLED ROPE) — Experiences the policy as a hybrid. It benefits from the inflation control (a coordination goal) but is also a primary victim via massively increased debt servicing costs, constraining its fiscal options. d≈0.70, f(d)≈1.05, σ=1.0 → χ≈0.58.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both the genuine coordination function (inflation targeting) and the severe, asymmetric extraction from borrowers to savers. The high suppression (state monopoly) and active enforcement confirm the hybrid nature. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76. While χ is high enough for a Snare, the undeniable coordination function makes Tangled Rope the correct analytical classification.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(boe_base_rate_policy_2024_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(boe_base_rate_policy_2024, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(boe_base_rate_policy_2024, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(boe_base_rate_policy_2024, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(boe_base_rate_policy_2024_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55): High. The policy directly transfers a significant percentage of national income from one group (borrowers) to another (savers). Suppression (0.80): Very high. The Bank of England has a state-mandated monopoly on setting the base rate; there are no alternative systems for domestic economic actors. Theater Ratio (0.30): Low-to-moderate. While the announcements are ritualized, the policy's effects are direct and highly functional, not performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a mortgage holder, the policy is a Snare, extracting hundreds or thousands of pounds monthly with no recourse. For a saver, it is a Rope, a sensible rule that rewards prudence. For the Bank of England, it is also a Rope, the correct tool for their mandate. The analytical view must hold both realities at once: the coordination function is real, but it is achieved via a mechanism of pure, coercive extraction. This is the definition of a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived directly from structural roles. Borrowers are victims with trapped exit, yielding a high 'd' value and a Snare classification. Savers are beneficiaries with mobile exit, yielding a low 'd' and a Rope classification. The Bank of England, as the institutional agent executing its own mandate, is a beneficiary with arbitrage exit, also seeing a Rope. The analytical observer's default 'd' value, combined with the high base extractiveness, correctly identifies the Tangled Rope structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic case for resolving mandatrophy. A naive analysis might label the policy a 'Snare' based on its painful effects, ignoring the legitimate macroeconomic coordination goal. Conversely, accepting the official 'Rope' narrative ignores the massive, asymmetric wealth transfer. The Tangled Rope classification is essential as it correctly identifies that the constraint has *both* a genuine coordination function *and* a severe extractive component. It prevents the misclassification of necessary but painful policy as pure predation, and prevents the misclassification of extractive mechanisms as pure public good.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    soft_landing_ambiguity,
    'Will the high interest rates successfully curb inflation without causing a severe, wealth-destroying recession (a ''soft landing'')?',
    'Future macroeconomic data on GDP, unemployment, and inflation over the 2024-2026 period.',
    'If a soft landing is achieved, the policy''s coordination function is validated, making it appear more like a Rope/Scaffold. If a deep recession occurs, the extractive damage dominates, making it appear more like a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soft_landing_ambiguity, empirical, 'Whether the policy achieves a soft landing or triggers a deep recession').

omega_variable(
    transmission_lag_uncertainty,
    'What is the true lag time for monetary policy transmission, and has the MPC over-tightened by not waiting for previous hikes to take full effect?',
    'Retrospective econometric analysis once sufficient time has passed (c. 2026-2027).',
    'If the lag is longer than the MPC''s model assumes, the policy is excessively extractive (Snare). If the lag is shorter, the policy is a more responsive and legitimate coordination tool (Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transmission_lag_uncertainty, empirical, 'Uncertainty in the lag time of monetary policy''s effect on the economy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(boe_base_rate_policy_2024, 2021, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(boe__tr_t2021, boe_base_rate_policy_2024, theater_ratio, 2021, 0.3).
narrative_ontology:measurement(boe__tr_t2022, boe_base_rate_policy_2024, theater_ratio, 2022, 0.3).
narrative_ontology:measurement(boe__tr_t2024, boe_base_rate_policy_2024, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(boe__be_t2021, boe_base_rate_policy_2024, base_extractiveness, 2021, 0.1).
narrative_ontology:measurement(boe__be_t2022, boe_base_rate_policy_2024, base_extractiveness, 2022, 0.3).
narrative_ontology:measurement(boe__be_t2024, boe_base_rate_policy_2024, base_extractiveness, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(boe_base_rate_policy_2024, resource_allocation).
narrative_ontology:affects_constraint(boe_base_rate_policy_2024, uk_housing_market_affordability).
narrative_ontology:affects_constraint(boe_base_rate_policy_2024, small_business_solvency_rate).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
