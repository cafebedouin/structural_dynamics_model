% ============================================================================
% CONSTRAINT STORY: china_japan_watchlist_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-31
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_japan_watchlist_2026, []).

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
 *   constraint_id: china_japan_watchlist_2026
 *   human_readable: China's Economic Security Watch List for Japanese Companies (2026)
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   In a near-future scenario, China establishes an 'Economic Security Watch
 *   List' targeting Japanese corporations. Firms placed on this list are
 *   subjected to unpredictable audits, regulatory hurdles, and potential
 *   exclusion from the Chinese market, ostensibly to mitigate national
 *   security risks. In practice, the list functions as a tool of economic
 *   statecraft, allowing Beijing to exert pressure on Japan's government and
 *   create advantages for domestic Chinese firms by selectively
 *   disadvantaging foreign competitors.
 *
 * KEY AGENTS:
 *   - Chinese State Agencies: Primary beneficiary (institutional/arbitrage) — Creates and enforces the constraint to achieve geopolitical and industrial policy goals.
 *   - Listed Japanese Companies: Primary victim (powerful/constrained) — Bear the direct costs of extraction through compliance burdens, lost revenue, and strategic uncertainty.
 *   - Chinese Domestic Competitors: Secondary beneficiary (organized/mobile) — Gain market share and competitive advantages from the constraints placed on their Japanese rivals.
 *   - Japanese Government (METI): Secondary victim (institutional/constrained) — Forced into a reactive posture to defend its national champions, with limited options for retaliation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_japan_watchlist_2026, 0.62).
domain_priors:suppression_score(china_japan_watchlist_2026, 0.75).
domain_priors:theater_ratio(china_japan_watchlist_2026, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_japan_watchlist_2026, extractiveness, 0.62).
narrative_ontology:constraint_metric(china_japan_watchlist_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(china_japan_watchlist_2026, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_japan_watchlist_2026, tangled_rope).
narrative_ontology:human_readable(china_japan_watchlist_2026, "China's Economic Security Watch List for Japanese Companies (2026)").
narrative_ontology:topic_domain(china_japan_watchlist_2026, "geopolitical/economic").

domain_priors:requires_active_enforcement(china_japan_watchlist_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_japan_watchlist_2026, chinese_state_agencies).
narrative_ontology:constraint_beneficiary(china_japan_watchlist_2026, chinese_domestic_competitors).
narrative_ontology:constraint_victim(china_japan_watchlist_2026, listed_japanese_companies).
narrative_ontology:constraint_victim(china_japan_watchlist_2026, japanese_government_meti).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE TARGET (SNARE) — A multinational Japanese tech or manufacturing firm with significant investment in China. From its perspective, the watch list is a coercive, extractive tool. Exit is prohibitively expensive, and compliance imposes significant costs, IP risks, and operational uncertainty. The coordination function is invisible; only the extraction is felt. d is derived high from victim status + constrained exit, making χ > 0.66, classifying as Snare.
constraint_indexing:constraint_classification(china_japan_watchlist_2026, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: THE ENFORCER (ROPE) — The Chinese Ministry of State Security or NDRC. This agent created the constraint and sees it as a legitimate policy instrument to coordinate corporate behavior with national security objectives and industrial policy. The extraction is framed as a necessary cost of doing business. d is derived very low from beneficiary status + arbitrage exit, resulting in negative χ. This is a pure coordination tool from their viewpoint.
constraint_indexing:constraint_classification(china_japan_watchlist_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYST (TANGLED ROPE) — An observer who sees both the stated coordination goal (protecting national security) and the asymmetric extraction (punishing geopolitical rivals, subsidizing domestic firms). The base properties (ε=0.62, suppression=0.75) clearly indicate a hybrid function. The analytical power atom's canonical directionality places χ in the Tangled Rope range.
constraint_indexing:constraint_classification(china_japan_watchlist_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE COUNTERPART (TANGLED ROPE) — Japan's Ministry of Economy, Trade and Industry. As an institutional actor, it understands the geopolitical signaling and coercive coordination game. However, as a victim of the constraint (its national champions are targeted), it is constrained and must react. It perceives the hybrid nature of the tool—a mix of policy enforcement and raw economic pressure.
constraint_indexing:constraint_classification(china_japan_watchlist_2026, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_japan_watchlist_2026_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_japan_watchlist_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(china_japan_watchlist_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.62) is high, representing the substantial economic value extracted from targeted firms. Suppression (0.75) is also high, as the targeted firms have deep investments in the Chinese market and cannot easily exit without incurring massive losses. The theater ratio (0.20) is low because the list has tangible, severe consequences, though its public justification serves a performative signaling function. The measurements show extractiveness rising and theater falling as the policy solidifies from a threat into an active enforcement mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental gap is between the Chinese state's self-perception of implementing a legitimate security policy (Rope) and the targeted Japanese firm's experience of being trapped in a punitive, extractive system (Snare). This gap highlights the dual nature of economic statecraft. The analytical observer and the Japanese government counterpart both perceive the hybrid reality of the situation, classifying it as a Tangled Rope that combines a real (though coercive) coordination function with asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is driven by the clear beneficiary/victim structure. The Chinese state and its domestic firms benefit directly from the leverage and market distortion created by the list. Their low derived 'd' values lead to Rope classifications. The Japanese firms bear the full cost, and their high derived 'd' values (from being victims with constrained exit options) lead to Snare classifications. The framework correctly models this asymmetrical relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves a common mandatrophy in international relations: mischaracterizing economic coercion as either pure coordination ('legitimate trade policy') or pure malice ('economic warfare'). The Tangled Rope classification correctly identifies that the constraint has both a genuine strategic coordination function for the enforcer and a highly extractive, snare-like effect on the target. It avoids collapsing the analysis into a simplistic binary and instead quantifies the perspectival gap between the actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_effect,
    'Is the list''s primary function punitive retribution for geopolitical alignment, or is it a preemptive de-risking strategy based on genuine security concerns?',
    'Empirical analysis of listed companies'' profiles. A high correlation with defense-adjacent sectors or technology transfers would support the security claim. A correlation with firms whose home governments recently enacted policies unfavorable to China would support the punitive claim.',
    'If primarily punitive, the constraint is functionally closer to a pure Snare, with the coordination aspect being mere pretext. If primarily security-driven, the Tangled Rope classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_effect, empirical, 'Whether the watchlist is primarily punitive or a genuine security measure').

omega_variable(
    long_term_efficacy,
    'Will this policy successfully coerce Japanese corporate and state behavior, or will it accelerate supply chain decoupling, ultimately harming China''s access to advanced technology and capital?',
    'Longitudinal tracking of foreign direct investment flows, supply chain diversification initiatives by Japanese firms (e.g., ''China plus one''), and shifts in Japanese foreign policy.',
    'If the policy backfires and accelerates decoupling, its extractive capacity will diminish over time, potentially degrading it into a Piton (an enforced but ineffective policy).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(long_term_efficacy, empirical, 'Whether the watchlist will achieve its goals or accelerate decoupling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_japan_watchlist_2026, 2024, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chin_tr_t2024, china_japan_watchlist_2026, theater_ratio, 2024, 0.5).
narrative_ontology:measurement(chin_tr_t2025, china_japan_watchlist_2026, theater_ratio, 2025, 0.35).
narrative_ontology:measurement(chin_tr_t2026, china_japan_watchlist_2026, theater_ratio, 2026, 0.2).

% Extraction over time
narrative_ontology:measurement(chin_be_t2024, china_japan_watchlist_2026, base_extractiveness, 2024, 0.2).
narrative_ontology:measurement(chin_be_t2025, china_japan_watchlist_2026, base_extractiveness, 2025, 0.45).
narrative_ontology:measurement(chin_be_t2026, china_japan_watchlist_2026, base_extractiveness, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_japan_watchlist_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(china_japan_watchlist_2026, semiconductor_supply_chain).
narrative_ontology:affects_constraint(china_japan_watchlist_2026, rare_earth_mineral_access).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
