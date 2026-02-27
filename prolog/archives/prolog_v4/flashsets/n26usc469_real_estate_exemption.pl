% ============================================================================
% CONSTRAINT STORY: n26usc469_real_estate_exemption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_n26usc469_real_estate_exemption, []).

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
 *   constraint_id: n26usc469_real_estate_exemption
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code generally prevents taxpayers from
 *   deducting passive activity losses against active income. The real estate
 *   professional exemption allows certain taxpayers who materially
 *   participate in real property trades or businesses to deduct these losses.
 *   This creates a complex dynamic with benefits and extractions for
 *   different actors.
 *
 * KEY AGENTS:
 *   - Real Estate Professionals: Benefit from the exemption but are constrained by qualification requirements (moderate/constrained).
 *   - Non-Professional Investors: Trapped by passive activity loss rules (powerless/trapped).
 *   - IRS: Benefits from simplified enforcement and tax revenue (institutional/arbitrage).
 *   - Tax Base: Suffers from potential tax revenue loss due to the exemption (powerless/trapped).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(n26usc469_real_estate_exemption, 0.55).
domain_priors:suppression_score(n26usc469_real_estate_exemption, 0.65).
domain_priors:theater_ratio(n26usc469_real_estate_exemption, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(n26usc469_real_estate_exemption, extractiveness, 0.55).
narrative_ontology:constraint_metric(n26usc469_real_estate_exemption, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(n26usc469_real_estate_exemption, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(n26usc469_real_estate_exemption, tangled_rope).
narrative_ontology:human_readable(n26usc469_real_estate_exemption, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(n26usc469_real_estate_exemption, "economic/legal").

domain_priors:requires_active_enforcement(n26usc469_real_estate_exemption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(n26usc469_real_estate_exemption, real_estate_professionals).
narrative_ontology:constraint_beneficiary(n26usc469_real_estate_exemption, irs).
narrative_ontology:constraint_victim(n26usc469_real_estate_exemption, non_professional_investors).
narrative_ontology:constraint_victim(n26usc469_real_estate_exemption, tax_base).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Non-professional investors are trapped by the passive activity loss rules and face potential penalties if they attempt to circumvent them.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The IRS benefits from the rules through increased tax revenue and simplification of tax enforcement.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Real estate professionals benefit from the exemption but are constrained by the strict requirements to qualify.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% From an analytical perspective, the exemption creates a tangled rope dynamic, providing benefits to some while extracting from others, ultimately creating a complex and potentially distortionary effect on investment decisions.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(n26usc469_real_estate_exemption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(n26usc469_real_estate_exemption, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(n26usc469_real_estate_exemption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: Moderate. The exemption allows real estate professionals to deduct losses, reducing the tax base. Suppression: Moderate.  The complex rules suppress alternative investment strategies and create barriers to entry for non-professionals. Theater Ratio: Low. There's relatively little 'theater' in the sense of performative compliance, as the exemption hinges on demonstrably meeting objective criteria (hours worked, etc.).
 *
 * PERSPECTIVAL GAP:
 *   Non-professional investors experience the rules as a snare, limiting their ability to deduct losses. Real estate professionals see a tangled rope, as they benefit from the exemption but face strict qualification requirements. The IRS perceives a rope, seeing the rules as a tool for simplifying tax enforcement. An analytical observer sees the tangled rope dynamic, recognizing that the exemption creates both benefits and extractions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the actors' positions. Real estate professionals benefit, so they have low d. The IRS benefits from the simplified tax enforcement, so it has a low d. Non-professional investors are harmed, so they have high d. The tax base suffers, so it has a high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is best classified as a tangled rope. The real estate exemption provides a coordination benefit by encouraging real estate investment and activity, but does so at the expense of the tax base and non-professional investors, who are essentially subsidizing the exemption for professionals, creating a extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qualification_clarity,
    'How clearly defined are the requirements for qualifying as a real estate professional?',
    'Legal analysis of court cases and IRS guidance',
    'If requirements are unclear, increased litigation and uncertainty for taxpayers',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qualification_clarity, conceptual, 'Clarity of requirements for real estate professional status.').

omega_variable(
    economic_distortion,
    'To what extent does the exemption distort investment decisions towards real estate and away from other productive activities?',
    'Economic modeling of investment flows and market effects',
    'If significant distortion, reduced overall economic efficiency',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_distortion, empirical, 'Distortionary impact on investment allocation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(n26usc469_real_estate_exemption, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(n26u_tr_t0, n26usc469_real_estate_exemption, theater_ratio, 0, 0.2).
narrative_ontology:measurement(n26u_tr_t5, n26usc469_real_estate_exemption, theater_ratio, 5, 0.25).
narrative_ontology:measurement(n26u_tr_t10, n26usc469_real_estate_exemption, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(n26u_be_t0, n26usc469_real_estate_exemption, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(n26u_be_t5, n26usc469_real_estate_exemption, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(n26u_be_t10, n26usc469_real_estate_exemption, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(n26usc469_real_estate_exemption, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
