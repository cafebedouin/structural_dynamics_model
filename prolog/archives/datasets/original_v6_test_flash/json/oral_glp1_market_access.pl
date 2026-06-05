% ============================================================================
% CONSTRAINT STORY: oral_glp1_market_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_oral_glp1_market_access, []).

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
 *   constraint_id: oral_glp1_market_access
 *   human_readable: Patent-Protected Market for Oral GLP-1 Agonists
 *   domain: economic/technological/healthcare
 *
 * SUMMARY:
 *   The patent-protected market for oral GLP-1 agonists presents a complex
 *   scenario where innovation incentives clash with affordable access. The
 *   pharmaceutical manufacturer benefits significantly from patent
 *   protection, allowing for high prices and maximized profits. However,
 *   patients without adequate insurance coverage face significant barriers,
 *   and payers (insurers) are constrained by their need to provide coverage
 *   while managing costs. Competing drug manufacturers also face declining
 *   market share.
 *
 * KEY AGENTS:
 *   - Pharmaceutical Manufacturer: Primary beneficiary (institutional/arbitrage) - Patent protection allows for high prices and maximized profits.
 *   - Patients: Primary victims (powerless/trapped) - Lack of insurance or means to pay limits access due to patent protection.
 *   - Payers (Insurers): Moderate influence (moderate/constrained) - Constrained by providing coverage while managing costs, they negotiate prices but face limited alternatives.
 *   - Competing Drug Manufacturers: Victims (moderate/constrained) - Face declining market share due to the patent-protected drug's effectiveness.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(oral_glp1_market_access, 0.65).
domain_priors:suppression_score(oral_glp1_market_access, 0.75).
domain_priors:theater_ratio(oral_glp1_market_access, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(oral_glp1_market_access, extractiveness, 0.65).
narrative_ontology:constraint_metric(oral_glp1_market_access, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(oral_glp1_market_access, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(oral_glp1_market_access, tangled_rope).
narrative_ontology:human_readable(oral_glp1_market_access, "Patent-Protected Market for Oral GLP-1 Agonists").
narrative_ontology:topic_domain(oral_glp1_market_access, "economic/technological/healthcare").

domain_priors:requires_active_enforcement(oral_glp1_market_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(oral_glp1_market_access, pharmaceutical_manufacturer).
narrative_ontology:constraint_beneficiary(oral_glp1_market_access, shareholders).
narrative_ontology:constraint_victim(oral_glp1_market_access, patients).
narrative_ontology:constraint_victim(oral_glp1_market_access, payers).
narrative_ontology:constraint_victim(oral_glp1_market_access, competing_drug_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Patients without adequate insurance or means to pay face high prices and limited access due to patent protection. Their exit options are severely limited.
constraint_indexing:constraint_classification(oral_glp1_market_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Insurers are constrained by their need to provide coverage while managing costs. They negotiate with the manufacturer but are often forced to accept high prices due to lack of alternatives. They benefit from offering a popular treatment.
constraint_indexing:constraint_classification(oral_glp1_market_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The manufacturer benefits from patent protection, allowing them to set prices and maximize profits. They can arbitrage different markets globally.
constraint_indexing:constraint_classification(oral_glp1_market_access, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Manufacturers of older, less effective drugs face declining market share and are constrained by the patent protection of the new drug. They may benefit slightly if patient interest expands the overall market.
constraint_indexing:constraint_classification(oral_glp1_market_access, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical observer recognizes the mixed incentives of innovation and restricted access. Patent system encourages innovation but limits availability due to high prices.
constraint_indexing:constraint_classification(oral_glp1_market_access, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(oral_glp1_market_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(oral_glp1_market_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(oral_glp1_market_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(oral_glp1_market_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(oral_glp1_market_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The patent allows for significant extraction from patients and payers due to the lack of generic alternatives. Suppression (0.75): High. Patent protection actively suppresses competition, limiting alternatives and driving up prices. Theater Ratio (0.30): Low. Minimal performative activity; real medical benefits and efficacy.
 *
 * PERSPECTIVAL GAP:
 *   Patients without insurance view the situation as a snare due to lack of access and high prices. Payers see a tangled rope, constrained by costs but also providing a valuable treatment. The pharmaceutical manufacturer views it as a rope, benefiting from market exclusivity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's structural position. The manufacturer, as the patent holder, has arbitrage options and benefits. Patients without insurance are trapped and bear the cost. Payers are constrained but still derive some benefit from providing a treatment option.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    patent_duration_vs_innovation,
    'What is the optimal patent duration to balance innovation incentives with affordable access?',
    'Economic modeling of pharmaceutical R&D investment vs. generic entry impact; historical analysis of drug prices and access post-patent expiration.',
    'Shorter duration increases generic competition, but may decrease innovation. Longer duration increases manufacturer profits, but restricts patient access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patent_duration_vs_innovation, empirical, 'Optimal patent duration for oral GLP-1 agonists').

omega_variable(
    market_exclusivity_alternatives,
    'Are there alternative market exclusivity mechanisms (e.g., data exclusivity, regulatory extensions) that could provide sufficient incentive without patent thickets?',
    'Comparative analysis of different exclusivity models in pharmaceutical innovation; case studies of drug development under various regulatory regimes.',
    'Alternatives may reduce patent litigation and improve access, but may also be less effective at incentivizing high-risk R&D.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(market_exclusivity_alternatives, conceptual, 'Alternatives to patent protection for oral GLP-1 agonists').

omega_variable(
    tier_price_acceptability,
    'What level of price discrimination between insured and uninsured patients is ethically and practically acceptable?',
    'Ethical framework development; survey of public attitudes toward drug pricing; analysis of the effect of price discrimination on patient access and manufacturer profits.',
    'High price discrimination may improve overall access, but could lead to inequitable treatment. Low price discrimination may improve equity but reduce overall access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tier_price_acceptability, preference, 'Acceptable price discrimination level for oral GLP-1 agonists').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(oral_glp1_market_access, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oral_tr_t0, oral_glp1_market_access, theater_ratio, 0, 0.25).
narrative_ontology:measurement(oral_tr_t5, oral_glp1_market_access, theater_ratio, 5, 0.3).
narrative_ontology:measurement(oral_tr_t10, oral_glp1_market_access, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(oral_be_t0, oral_glp1_market_access, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(oral_be_t5, oral_glp1_market_access, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(oral_be_t10, oral_glp1_market_access, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(oral_glp1_market_access, resource_allocation).
narrative_ontology:affects_constraint(oral_glp1_market_access, pharmaceutical_innovation_incentives).
narrative_ontology:affects_constraint(oral_glp1_market_access, healthcare_cost_containment).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
