% ============================================================================
% CONSTRAINT STORY: cab_la_patent_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cab_la_patent_access, []).

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
 *   constraint_id: cab_la_patent_access
 *   human_readable: Patent-Constrained Access to Long-Acting HIV PrEP (Cabotegravir)
 *   domain: healthcare/intellectual_property
 *
 * SUMMARY:
 *   Cabotegravir (CAB-LA) is a long-acting injectable drug for HIV prevention
 *   (PrEP), offering a significant adherence advantage over daily pills (6
 *   injections vs. 365 pills per year). However, its developer, ViiV
 *   Healthcare, holds the patent, creating a state-enforced monopoly. While
 *   ViiV has provided voluntary licenses through the Medicines Patent Pool
 *   (MPP) to allow for cheaper generic production, these licenses are
 *   geographically restricted to 90 low- and middle-income countries. This
 *   structure deliberately excludes numerous countries with significant HIV
 *   burdens to protect ViiV's high-price markets, creating a system of tiered
 *   access and artificial scarcity. Even in eligible countries like Zimbabwe,
 *   initial rollout is minimal and donor-dependent, leaving vast populations
 *   unprotected.
 *
 * KEY AGENTS:
 *   - ViiV Healthcare: Primary beneficiary (institutional/arbitrage) — Holds the patent and controls licensing terms to maximize global profit.
 *   - At-Risk Populations (Excluded Countries): Primary victim (powerless/trapped) — Cannot access the drug due to patent barriers and high prices.
 *   - At-Risk Populations (Eligible Countries): Secondary victim (powerless/trapped) — Face severe rationing and supply delays due to slow generic ramp-up and limited funding.
 *   - Medicines Patent Pool & Donors (PEPFAR): Implementing agents (institutional/constrained) — Facilitate access but operate within the restrictive framework set by the patent holder.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cab_la_patent_access, 0.68).
domain_priors:suppression_score(cab_la_patent_access, 0.85).
domain_priors:theater_ratio(cab_la_patent_access, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cab_la_patent_access, extractiveness, 0.68).
narrative_ontology:constraint_metric(cab_la_patent_access, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(cab_la_patent_access, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cab_la_patent_access, tangled_rope).
narrative_ontology:human_readable(cab_la_patent_access, "Patent-Constrained Access to Long-Acting HIV PrEP (Cabotegravir)").
narrative_ontology:topic_domain(cab_la_patent_access, "healthcare/intellectual_property").

domain_priors:requires_active_enforcement(cab_la_patent_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cab_la_patent_access, viiv_healthcare).
narrative_ontology:constraint_beneficiary(cab_la_patent_access, medicines_patent_pool).
narrative_ontology:constraint_victim(cab_la_patent_access, at_risk_populations_excluded_countries).
narrative_ontology:constraint_victim(cab_la_patent_access, at_risk_populations_eligible_countries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED PATIENT (SNARE) — An individual in a middle-income country not covered by the voluntary license. The drug exists and is life-saving, but is made inaccessible by patent law and prohibitive pricing. The coordination function is null; it is pure extraction of potential life-years. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.97.
constraint_indexing:constraint_classification(cab_la_patent_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PATENT HOLDER (ROPE) — ViiV and its shareholders see the tiered licensing system as a pure coordination mechanism. It balances humanitarian access in the poorest markets with the need to recoup R&D investment and profit from high-income markets, thereby funding future innovation. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.10. The negative extraction represents the perceived subsidy to low-income countries.
constraint_indexing:constraint_classification(cab_la_patent_access, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: IMPLEMENTING AGENCY (TANGLED ROPE) — A donor agency like PEPFAR or a national ministry of health. They experience the genuine coordination benefit of the superior drug but are severely limited by the artificial supply constraints and donor-dependency created by the licensing terms. They navigate a system that both helps and hinders. d≈0.55 (as victim proxy), f(d)≈0.75, σ=1.0 → χ≈0.51.
constraint_indexing:constraint_classification(cab_la_patent_access, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — This view sees the full structure: a genuine coordination advance (the drug's efficacy) is inextricably linked to an extractive legal framework (the patent and restrictive license). The system simultaneously solves one problem (adherence) while creating another (inequitable access). The base properties (ε=0.68, suppression=0.85) confirm the Tangled Rope classification.
constraint_indexing:constraint_classification(cab_la_patent_access, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cab_la_patent_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cab_la_patent_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cab_la_patent_access, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cab_la_patent_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cab_la_patent_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.68) is very high, representing the immense value captured by denying a life-saving, cheaply producible drug to entire populations to protect pricing power elsewhere. The extraction is measured in preventable infections and lost life-years. Suppression (0.85) is extremely high, as patents are a form of state-enforced monopoly that legally suppresses all alternatives except with the owner's permission. Theater Ratio (0.35) is moderate; the voluntary licensing scheme is a genuine (if limited) act of access, but it also serves a significant public relations function to deflect criticism.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. ViiV (the beneficiary) sees a Rope, a responsible coordination of market access that balances profit and philanthropy. The individual in Brazil or Thailand who is excluded by the license experiences a brutal Snare where a life-saving technology is deliberately withheld by a legal fiction. The public health agency in Zimbabwe sees a Tangled Rope—a useful tool they can't deploy at scale due to externally imposed constraints. The analytical view confirms the structure is a Tangled Rope, as the undeniable coordination benefit is fundamentally tied to an extractive and coercive legal scaffolding.
 *
 * DIRECTIONALITY LOGIC:
 *   ViiV, as the beneficiary with arbitrage exit, has a directionality d≈0.05, leading to a negative effective extraction (χ < 0); from their perspective, they are subsidizing the system. The at-risk individual, as a victim with trapped exit, has d≈0.95, yielding a very high positive χ; for them, the system is almost purely extractive. This vast difference in χ, derived from their structural positions, is the core of the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint story avoids mandatrophy by refusing to classify the system as a pure Rope, despite the existence of the voluntary license. It correctly identifies that the *terms* of the license are the primary mechanism of extraction. The framework's ability to model a system as both coordinating and extractive (Tangled Rope) is essential here, preventing the positive framing ('access to medicines initiative') from obscuring the underlying extractive structure that leaves millions behind.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generic_production_viability,
    'Can generic manufacturers actually produce CAB-LA at scale and at a very low cost, or are there undisclosed technical hurdles that justify some of ViiV''s market control?',
    'Independent technical audits of generic manufacturing processes and supply chains for the required active pharmaceutical ingredients.',
    'If production is intrinsically difficult and expensive, the base extractiveness (ε) would be lower. If it is simple, the current ε is confirmed or even underestimated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generic_production_viability, empirical, 'Technical and economic feasibility of low-cost generic CAB-LA production').

omega_variable(
    compulsory_licensing_threat,
    'Was ViiV''s voluntary license a pre-emptive action to neutralize the growing political threat of countries issuing compulsory licenses to manufacture the drug anyway?',
    'Analysis of diplomatic and corporate communications; tracking political statements from key middle-income countries prior to the MPP agreement.',
    'If the license was a strategic concession to avoid a worse outcome (widespread compulsory licensing), it reinforces the view of the system as a contested terrain of extraction, not just a top-down coordination effort.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compulsory_licensing_threat, empirical, 'Whether the voluntary license was a strategic response to political pressure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cab_la_patent_access, 2021, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cab__tr_t2021, cab_la_patent_access, theater_ratio, 2021, 0.2).
narrative_ontology:measurement(cab__tr_t2022, cab_la_patent_access, theater_ratio, 2022, 0.3).
narrative_ontology:measurement(cab__tr_t2024, cab_la_patent_access, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(cab__be_t2021, cab_la_patent_access, base_extractiveness, 2021, 0.65).
narrative_ontology:measurement(cab__be_t2022, cab_la_patent_access, base_extractiveness, 2022, 0.66).
narrative_ontology:measurement(cab__be_t2024, cab_la_patent_access, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cab_la_patent_access, resource_allocation).
narrative_ontology:affects_constraint(cab_la_patent_access, pharmaceutical_patent_system).
narrative_ontology:affects_constraint(cab_la_patent_access, global_hiv_response).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
