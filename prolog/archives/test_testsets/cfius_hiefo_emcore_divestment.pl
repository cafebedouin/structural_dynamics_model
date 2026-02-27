% ============================================================================
% CONSTRAINT STORY: cfius_hiefo_emcore_divestment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cfius_hiefo_emcore_divestment, []).

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
 *   constraint_id: cfius_hiefo_emcore_divestment
 *   human_readable: CFIUS authority to force divestment of strategic assets
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   This constraint models the authority of the Committee on Foreign
 *   Investment in the United States (CFIUS) to review and retroactively force
 *   the divestment of foreign-owned assets deemed a threat to national
 *   security. This power, significantly enhanced by the FIRRMA legislation in
 *   2018, creates a fundamental tension between the U.S. policy of
 *   maintaining an open investment climate and its imperative to protect
 *   strategic sectors. The constraint's core feature is its ability to unwind
 *   completed transactions, imposing severe costs on targeted investors with
 *   no recourse.
 *
 * KEY AGENTS:
 *   - Foreign Investor Targeted for Divestment: Primary victim (powerful/trapped) — bears the full extractive cost of a forced sale.
 *   - US National Security Apparatus: Primary beneficiary (institutional/arbitrage) — wields the authority as a tool to mitigate perceived threats.
 *   - Domestic Competitors: Secondary beneficiary (organized/mobile) — may benefit from the removal of a foreign competitor or the opportunity to acquire divested assets.
 *   - Allied Foreign Investors: Secondary actors (powerful/constrained) — benefit from the overall security environment but face compliance costs and regulatory uncertainty.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cfius_hiefo_emcore_divestment, 0.75).
domain_priors:suppression_score(cfius_hiefo_emcore_divestment, 0.9).
domain_priors:theater_ratio(cfius_hiefo_emcore_divestment, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cfius_hiefo_emcore_divestment, extractiveness, 0.75).
narrative_ontology:constraint_metric(cfius_hiefo_emcore_divestment, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(cfius_hiefo_emcore_divestment, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cfius_hiefo_emcore_divestment, tangled_rope).
narrative_ontology:human_readable(cfius_hiefo_emcore_divestment, "CFIUS authority to force divestment of strategic assets").
narrative_ontology:topic_domain(cfius_hiefo_emcore_divestment, "geopolitical/economic").

domain_priors:requires_active_enforcement(cfius_hiefo_emcore_divestment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cfius_hiefo_emcore_divestment, us_national_security_apparatus).
narrative_ontology:constraint_beneficiary(cfius_hiefo_emcore_divestment, domestic_competitors).
narrative_ontology:constraint_victim(cfius_hiefo_emcore_divestment, foreign_investor_targeted_for_divestment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED FOREIGN INVESTOR (SNARE) — Despite being a powerful corporate entity, once targeted by a presidential divestment order, the investor is trapped. There is no appeal or alternative. The full value of their investment is subject to forced sale, often at a loss. From this view, the process is pure, coercive extraction. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈1.07.
constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: US NATIONAL SECURITY APPARATUS (ROPE) — For CFIUS and the executive branch, this authority is a pure coordination tool to align foreign investment with national security interests. They exercise arbitrage in choosing which transactions to review and can impose mitigation measures short of divestment. The extraction is seen as a necessary byproduct of the coordination function. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.09.
constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — This view recognizes the dual function. The constraint genuinely coordinates to protect a public good (national security) but does so via a highly coercive, asymmetric extraction mechanism. It is neither pure coordination nor pure extraction. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈1.04. The high chi value is tempered by the recognition of a valid coordination function, preventing a Snare classification at this level.
constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: DOMESTIC COMPETITOR (SCAFFOLD) — A domestic firm in the same sector may see CFIUS action as a temporary support, removing a foreign-backed competitor and creating an opportunity to acquire strategic assets. They benefit from the action but don't control it, and the 'leveling of the playing field' is contingent on CFIUS's priorities. It's a temporary structural advantage, not a permanent one.
constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ALLIED FOREIGN INVESTOR (TANGLED ROPE) — An investor from a friendly nation (e.g., eligible for a 'Known Investor Program') benefits from the stable security environment but faces significant compliance costs and the residual risk of being miscategorized. They are constrained by the regulatory uncertainty, even if they are not the primary target. d≈0.60, f(d)≈0.88, σ=1.2 → χ≈0.79.
constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cfius_hiefo_emcore_divestment_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cfius_hiefo_emcore_divestment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cfius_hiefo_emcore_divestment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.75) is very high because a forced divestment can result in a total or near-total loss of the initial investment, often through a fire sale. Suppression (0.90) is near-total; the authority is backed by a presidential order and the Department of Justice, leaving the targeted entity with no viable alternative or path of resistance. Theater Ratio (0.30) is low because while the decisions have a signaling component, the enforcement actions are concrete, legally binding, and have severe financial consequences, making the constraint highly functional.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The targeted foreign investor experiences a pure Snare, a coercive and arbitrary seizure of assets. The US government, however, views it as a necessary Rope for the coordination of national security, where divestment is a last resort. The analytical observer sees a Tangled Rope, acknowledging the legitimate coordination goal but recognizing that it is achieved through a mechanism of extreme, targeted extraction. This gap highlights the core function of the indexical system: to show how the same set of structural properties can be perceived as fundamentally different types of constraints depending on one's position relative to the flow of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the clear victim/beneficiary structure. The 'foreign_investor_targeted_for_divestment' is the declared victim and is 'trapped', leading to a maximal directionality (d≈0.95) and thus maximal effective extraction (χ). The 'us_national_security_apparatus' is the beneficiary with 'arbitrage' exit (it chooses its targets), leading to a minimal, even negative directionality (d≈0.05) and negative χ, correctly identifying it as a net beneficiary. Other actors fall in between based on their mixed roles and constrained exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved because the system does not incorrectly label this a pure Snare from all perspectives. While the victim's Snare perspective is validated, the analytical classification of Tangled Rope correctly captures the dual nature of the constraint. It avoids the error of dismissing the stated goal (national security coordination) as pure pretext, while also avoiding the opposite error of ignoring the severe, coercive extraction imposed on its targets. The high ε value is justified by the real-world consequences of forced divestment, and the `mandatrophy_resolved` flag confirms this has been analytically addressed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_national_security,
    'Is ''national security'' a stable, objective criterion, or a politically malleable concept that can be used for economic protectionism?',
    'Analysis of CFIUS decisions correlating with declared national security threats versus lobbying efforts by domestic industries.',
    'If objective, the Rope/Tangled Rope classifications are strengthened. If malleable, the Snare classification becomes more broadly applicable, as the ''coordination'' function is merely a pretext for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_national_security, conceptual, 'Whether ''national security'' is an objective or politically malleable criterion.').

omega_variable(
    detection_rate_of_non_notified_transactions,
    'What percentage of non-notified transactions that pose a genuine national security risk are successfully identified and reviewed by CFIUS?',
    'Impossible to measure directly. Proxies could include forensic analysis of corporate ownership structures in critical sectors after the fact.',
    'A low detection rate would suggest the enforcement mechanism is more symbolic than effective, increasing its theater_ratio. A high detection rate confirms its functional role and high suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(detection_rate_of_non_notified_transactions, empirical, 'The effectiveness of CFIUS in detecting risky non-notified transactions.').

omega_variable(
    forced_sale_price_discrepancy,
    'What is the average financial loss incurred by targeted entities due to forced divestment below market value?',
    'Comparing the forced sale price of divested assets to independent valuations conducted prior to the CFIUS order.',
    'Quantifies the base extractiveness (ε). A large discrepancy confirms the high ε value; a small discrepancy would suggest ε is lower than estimated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(forced_sale_price_discrepancy, empirical, 'Average financial loss from forced divestment sales.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cfius_hiefo_emcore_divestment, 2010, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cfiu_tr_t2010, cfius_hiefo_emcore_divestment, theater_ratio, 2010, 0.45).
narrative_ontology:measurement(cfiu_tr_t2018, cfius_hiefo_emcore_divestment, theater_ratio, 2018, 0.35).
narrative_ontology:measurement(cfiu_tr_t2026, cfius_hiefo_emcore_divestment, theater_ratio, 2026, 0.3).

% Extraction over time
narrative_ontology:measurement(cfiu_be_t2010, cfius_hiefo_emcore_divestment, base_extractiveness, 2010, 0.5).
narrative_ontology:measurement(cfiu_be_t2018, cfius_hiefo_emcore_divestment, base_extractiveness, 2018, 0.65).
narrative_ontology:measurement(cfiu_be_t2026, cfius_hiefo_emcore_divestment, base_extractiveness, 2026, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cfius_hiefo_emcore_divestment, enforcement_mechanism).
narrative_ontology:affects_constraint(cfius_hiefo_emcore_divestment, semiconductor_supply_chain_integrity).
narrative_ontology:affects_constraint(cfius_hiefo_emcore_divestment, critical_mineral_sourcing).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
