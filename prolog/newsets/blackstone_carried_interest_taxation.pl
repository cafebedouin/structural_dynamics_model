% ============================================================================
% CONSTRAINT STORY: blackstone_carried_interest_taxation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_blackstone_carried_interest_taxation, []).

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
 *   constraint_id: blackstone_carried_interest_taxation
 *   human_readable: Carried Interest Partnership Taxation
 *   domain: economic/political
 *
 * SUMMARY:
 *   The 'carried interest' tax loophole allows general partners in private
 *   equity, venture capital, and hedge funds to have their performance-based
 *   compensation taxed at the lower long-term capital gains rate (approx.
 *   20-24%) instead of the higher ordinary income tax rate (up to 37%). This
 *   rule is a point of intense political and economic debate, embodying the
 *   core dynamics of a Tangled Rope: it possesses a plausible coordination
 *   function (aligning manager and investor interests) while simultaneously
 *   enabling a massive, asymmetric extraction of wealth from the public
 *   treasury to a small group of high-earners. Its persistence is a testament
 *   to its high suppression, maintained by one of the most effective lobbying
 *   efforts in the United States.
 *
 * KEY AGENTS:
 *   - Fund General Partners: Primary beneficiaries (institutional/arbitrage) who receive the tax break.
 *   - US Treasury / General Taxpayers: Primary victims (powerless/trapped) who bear the cost of the foregone revenue.
 *   - Limited Partners (Investors): Indirect beneficiaries (powerful/mobile) who see the rule as part of a favorable incentive structure.
 *   - Reform-Minded Legislators: Oppositional agents (organized/constrained) who attempt to close the loophole but are suppressed by lobbying.
 *   - Tax Policy Analysts: Analytical observers who see the dual coordination/extraction nature of the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(blackstone_carried_interest_taxation, 0.65).
domain_priors:suppression_score(blackstone_carried_interest_taxation, 0.8).
domain_priors:theater_ratio(blackstone_carried_interest_taxation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(blackstone_carried_interest_taxation, extractiveness, 0.65).
narrative_ontology:constraint_metric(blackstone_carried_interest_taxation, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(blackstone_carried_interest_taxation, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(blackstone_carried_interest_taxation, tangled_rope).
narrative_ontology:human_readable(blackstone_carried_interest_taxation, "Carried Interest Partnership Taxation").
narrative_ontology:topic_domain(blackstone_carried_interest_taxation, "economic/political").

domain_priors:requires_active_enforcement(blackstone_carried_interest_taxation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(blackstone_carried_interest_taxation, fund_general_partners).
narrative_ontology:constraint_victim(blackstone_carried_interest_taxation, us_treasury).
narrative_ontology:constraint_victim(blackstone_carried_interest_taxation, general_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERAL TAXPAYER (SNARE) — Experiences the rule as a pure tax loophole for the wealthy, increasing their own relative tax burden or reducing public services. They are trapped within the tax system and have no agency to change the rule. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.92.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FUND GENERAL PARTNER (ROPE) — The direct beneficiary. Experiences the rule as a necessary and fair coordination mechanism to incentivize long-term, high-risk investment, aligning their interests with investors. The tax benefit is framed as a legitimate reward for capital at risk. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.08.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both the claimed coordination function (incentive alignment) and the massive, asymmetric extraction from the public treasury. Recognizes the high degree of suppression (lobbying) required to maintain the system. This is the canonical classification. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: LIMITED PARTNER (ROPE) — An institutional investor (e.g., pension fund). While not a direct beneficiary of the tax break, they see it as part of the overall compensation structure that aligns the General Partner's incentives with their own. For them, it's a feature of the coordination contract, not an extraction. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.51. Classifies as Tangled Rope by χ, but Rope by narrative due to symmetric alignment with GP.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM-MINDED LEGISLATOR (TANGLED ROPE) — Acknowledges the industry's coordination arguments but primarily views the rule as an extractive loophole to be closed. Their ability to act is constrained by intense industry lobbying and political opposition, highlighting the constraint's high suppression. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.49.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(blackstone_carried_interest_taxation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(blackstone_carried_interest_taxation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(blackstone_carried_interest_taxation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.65): High. The value represents the significant wealth transfer enabled by the large gap between capital gains and ordinary income tax rates, applied to billions in compensation annually. Suppression (0.80): Very High. Decades of repeated, well-funded, and successful lobbying efforts by the financial industry to defeat legislative attempts to close this loophole demonstrate an extremely effective suppression of alternatives. Theater Ratio (0.40): Moderate. The public justifications rely on complex, often debatable, economic arguments about 'capital formation' and 'risk-taking' that serve to obscure the direct financial transfer, but the rule's primary function is its real economic effect, not performance.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the beneficiaries (GPs and LPs), the constraint is a Rope—a legitimate feature of a contract that aligns incentives for long-term growth. For the victims (the public), it is a Snare—an unfair loophole that extracts public funds for private gain. The analytical perspective resolves this by classifying it as a Tangled Rope, acknowledging that the constraint *simultaneously* has a coordination function and an extractive one. The political battle is essentially a fight over which perspective becomes dominant.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived directly from the structural relationships. 'fund_general_partners' are declared beneficiaries with arbitrage exit, yielding a low 'd' value and perceiving a Rope. 'general_taxpayers' are declared victims with trapped exit, yielding a high 'd' value and perceiving a Snare. This clear opposition in the base properties is what drives the large perspectival gap and makes the Tangled Rope classification from the analytical view robust.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a canonical example of resolving mandatrophy. A naive analysis might label it a 'Snare' (it's a loophole for the rich) or a 'Rope' (it aligns incentives). Both are incomplete. The Tangled Rope classification is essential because it correctly identifies the structure as possessing *both* a genuine coordination function (the 'Rope' part that beneficiaries see) and a severe, asymmetric extraction (the 'Snare' part that victims see). The system's persistence is explained by the beneficiaries' ability to successfully promote the Rope narrative while suppressing challenges to the Snare component.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_vs_labor_income,
    'Is carried interest fundamentally a return on capital investment (justifying capital gains treatment) or compensation for labor/managerial services (which should be taxed as ordinary income)?',
    'This is a conceptual and legal debate, not strictly empirical. Resolution would require a definitive legislative or judicial re-classification of the income type.',
    'If classified as labor, the constraint is a pure Snare. If classified as capital, the Rope/Tangled Rope perspectives are strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_vs_labor_income, conceptual, 'The fundamental legal and economic classification of carried interest income.').

omega_variable(
    incentive_versus_windfall,
    'To what extent does the favorable tax treatment genuinely incentivize new, high-risk investment versus simply providing a windfall for investment activity that would have occurred anyway?',
    'Econometric studies comparing investment allocation and risk-taking in jurisdictions with and without similar tax incentives.',
    'High incentive effect supports a Rope/Tangled Rope classification. Low incentive effect (i.e., it''s a pure windfall) supports a Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_versus_windfall, empirical, 'Whether the tax break is a true incentive or a pure windfall.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(blackstone_carried_interest_taxation, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blac_tr_t0, blackstone_carried_interest_taxation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(blac_tr_t19, blackstone_carried_interest_taxation, theater_ratio, 19, 0.3).
narrative_ontology:measurement(blac_tr_t38, blackstone_carried_interest_taxation, theater_ratio, 38, 0.4).

% Extraction over time
narrative_ontology:measurement(blac_be_t0, blackstone_carried_interest_taxation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(blac_be_t19, blackstone_carried_interest_taxation, base_extractiveness, 19, 0.5).
narrative_ontology:measurement(blac_be_t38, blackstone_carried_interest_taxation, base_extractiveness, 38, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(blackstone_carried_interest_taxation, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
