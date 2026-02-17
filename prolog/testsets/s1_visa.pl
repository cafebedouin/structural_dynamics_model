% ============================================================================
% CONSTRAINT STORY: visa_ipo_regulatory_compliance
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_visa_ipo_regulatory_compliance, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: visa_ipo_regulatory_compliance
 *   human_readable: SEC S-1 Registration Framework for Initial Public Offerings
 *   domain: economic/political
 *
 * SUMMARY:
 *   The mandatory legal framework governing the public offering of securities in
 *   the US, specifically the filing of Form S-1 under the Securities Act of 1933.
 *   This system requires exhaustive disclosure of financial, legal, and operational
 *   data to the SEC before a company can access public capital markets. It serves
 *   a dual function: providing investor protection through transparency (coordination)
 *   while imposing significant compliance costs and legal liabilities on the
 *   registering company (extraction).
 *
 * KEY AGENTS (by structural relationship):
 *   - ipo_registrants (e.g., Visa Inc.): Primary target (powerful/trapped) — bears the high costs of compliance, legal fees, and executive time.
 *   - retail_and_institutional_investors: Primary beneficiary (powerless/constrained) — receives standardized, legally mandated disclosures to inform investment decisions.
 *   - securities_attorneys (e.g., White & Case): Secondary beneficiary (institutional/arbitrage) — profits from navigating the system's complexity for clients.
 *   - SEC: Enforcement agent (institutional/arbitrage) - maintains the system.
 *   - analytical_observer: Sees the full dual-function structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(visa_ipo_regulatory_compliance, 0.45).
domain_priors:suppression_score(visa_ipo_regulatory_compliance, 0.90).
domain_priors:theater_ratio(visa_ipo_regulatory_compliance, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(visa_ipo_regulatory_compliance, extractiveness, 0.45).
narrative_ontology:constraint_metric(visa_ipo_regulatory_compliance, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(visa_ipo_regulatory_compliance, theater_ratio, 0.08).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(visa_ipo_regulatory_compliance, tangled_rope).
narrative_ontology:human_readable(visa_ipo_regulatory_compliance, "SEC S-1 Registration Framework for Initial Public Offerings").

% --- Binary flags ---
domain_priors:requires_active_enforcement(visa_ipo_regulatory_compliance).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(visa_ipo_regulatory_compliance, retail_investors).
narrative_ontology:constraint_beneficiary(visa_ipo_regulatory_compliance, institutional_investors).
narrative_ontology:constraint_beneficiary(visa_ipo_regulatory_compliance, securities_attorneys).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(visa_ipo_regulatory_compliance, ipo_registrants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE REGISTRANT CEO (e.g., Joseph W. Saunders)
% Agent is trapped by the legal requirement and bears the full extractive cost.
% The high base extraction (ε=0.45) and global scope (σ=1.2) amplify the
% effective extraction (χ) into the Snare category.
constraint_indexing:constraint_classification(visa_ipo_regulatory_compliance, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE RETAIL INVESTOR
% This agent is a beneficiary of the disclosure but is also powerless. They
% may perceive the system as an unchangeable 'Mountain' of the financial world,
% but its structural properties (high extraction and suppression) classify it
% as a Tangled Rope from their indexical position.
constraint_indexing:constraint_classification(visa_ipo_regulatory_compliance, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE SECURITIES ATTORNEY (e.g., White & Case)
% This agent benefits from the system's complexity. With arbitrage exit options,
% their directionality (d) is low, leading to a negative effective extraction (χ).
% For them, the complex law is a functional coordination tool, a Rope.
constraint_indexing:constraint_classification(visa_ipo_regulatory_compliance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The analytical view recognizes the dual nature of the constraint: it has a
% genuine coordination function (beneficiary data) but also high asymmetric
% extraction (victim data) and requires active enforcement. This is the
% canonical definition of a Tangled Rope.
constraint_indexing:constraint_classification(visa_ipo_regulatory_compliance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(visa_ipo_regulatory_compliance_tests).

test(perspectival_gap_is_wide) :-
    constraint_indexing:constraint_classification(visa_ipo_regulatory_compliance, TypeTarget,
        context(agent_power(powerful), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(visa_ipo_regulatory_compliance, TypeBeneficiary,
        context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(visa_ipo_regulatory_compliance, TypeAnalytical,
        context(agent_power(analytical), _, _, _)),
    TypeTarget = snare,
    TypeBeneficiary = rope,
    TypeAnalytical = tangled_rope,
    TypeTarget \= TypeBeneficiary.

test(tangled_rope_structural_properties_present) :-
    narrative_ontology:constraint_beneficiary(visa_ipo_regulatory_compliance, _),
    narrative_ontology:constraint_victim(visa_ipo_regulatory_compliance, _),
    domain_priors:requires_active_enforcement(visa_ipo_regulatory_compliance).

:- end_tests(visa_ipo_regulatory_compliance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file incorrectly claimed this constraint was a 'Mountain'. This
 *   created a linter conflict, as its suppression score (0.90) massively exceeds
 *   the Mountain ceiling (0.05). The constraint is better modeled as a Tangled Rope.
 *   - Base Extractiveness (ε=0.45): Raised from 0.2 to reflect the substantial,
 *     multi-million dollar costs (legal, accounting, underwriting prep) imposed
 *     on registrants. This is not just a filing fee but the cost of the entire
 *     compliance apparatus.
 *   - Suppression (S=0.90): Correctly high. For a major IPO in the US, there are
 *     no legal alternatives to the SEC registration process.
 *   - Claim: Changed to 'tangled_rope' to match the analytical perspective and
 *     the structural reality of a system with both coordination and extraction.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the registrant CEO (trapped victim), the high costs and
 *   legal liability make it a Snare. For the securities lawyer (institutional
 *   beneficiary with arbitrage), it's a complex but navigable Rope that generates
 *   fees. For the retail investor (powerless beneficiary), it's a high-friction
 *   Tangled Rope they rely on. The analytical view synthesizes these into a
 *   Tangled Rope classification, acknowledging both its function and its cost.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'retail_investors' and 'institutional_investors' benefit
 *     from mandated transparency. 'securities_attorneys' benefit from the
 *     complexity that requires their expertise.
 *   - Victims: 'ipo_registrants' bear the direct financial and temporal costs
 *     of the compliance process.
 *   This clear opposition drives the wide perspectival gaps.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies that a system designed for public
 *   good (coordination) can simultaneously function as a highly extractive
 *   barrier to entry (extraction). Labeling it purely a 'Rope' (as a lawyer
 *   might) ignores the immense cost to registrants. Labeling it purely a 'Snare'
 *   (as a CEO might) ignores the genuine market-stabilizing function it serves.
 *   'Tangled Rope' captures this necessary but costly duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_sec_effectiveness,
    'Does the S-1 disclosure framework actually prevent material fraud (a coordination function) or does it merely provide a liability shield for institutions while creating extractive barriers (a snare function)?',
    'Analysis of post-IPO litigation frequency and outcomes vs. historical data from less-regulated markets.',
    'If primarily coordination, the ε=0.45 is a high but justified fee-for-service. If primarily a snare, the coordination function is theatrical.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(visa_ipo_regulatory_compliance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The SEC framework is a mature, stable system. Measurements show minimal drift
% over a typical 10-year interval, indicating a persistent Tangled Rope.
% Theater ratio over time (stable and low):
narrative_ontology:measurement(visa_ipo_tr_t0, visa_ipo_regulatory_compliance, theater_ratio, 0, 0.08).
narrative_ontology:measurement(visa_ipo_tr_t5, visa_ipo_regulatory_compliance, theater_ratio, 5, 0.08).
narrative_ontology:measurement(visa_ipo_tr_t10, visa_ipo_regulatory_compliance, theater_ratio, 10, 0.08).

% Extraction over time (stable and high):
narrative_ontology:measurement(visa_ipo_ex_t0, visa_ipo_regulatory_compliance, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(visa_ipo_ex_t5, visa_ipo_regulatory_compliance, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(visa_ipo_ex_t10, visa_ipo_regulatory_compliance, base_extractiveness, 10, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */