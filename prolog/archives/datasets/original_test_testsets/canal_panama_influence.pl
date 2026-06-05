% ============================================================================
% CONSTRAINT STORY: canal_panama_influence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_canal_panama_influence, []).

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
 *   constraint_id: canal_panama_influence
 *   human_readable: Geopolitical Influence over Panama Canal
 *   domain: political
 *
 * SUMMARY:
 *   The Panama Canal is a critical global infrastructure asset whose control
 *   is a site of intense geopolitical competition, primarily between the
 *   United States and China. While the canal provides a vital coordination
 *   function for global trade, this function is overlaid with a struggle for
 *   strategic influence. This constraint story models the tension between the
 *   canal's role as a global public good and its reality as a strategic
 *   chokepoint subject to great power politics, which extracts strategic
 *   autonomy from Panama and introduces systemic risk into global trade.
 *
 * KEY AGENTS:
 *   - United States: The incumbent power, views its influence as a stabilizing force (institutional/arbitrage).
 *   - China: The rising challenger, using economic investment to build influence (institutional/mobile).
 *   - Panama: The host nation, caught between powers and trying to maintain sovereignty (organized/constrained).
 *   - Global Shipping Companies: Commercial users who prioritize efficiency over politics (powerful/mobile).
 *   - Global Trade Stability: An abstract victim of the geopolitical risk (powerless/trapped).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(canal_panama_influence, 0.55).
domain_priors:suppression_score(canal_panama_influence, 0.75).
domain_priors:theater_ratio(canal_panama_influence, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(canal_panama_influence, extractiveness, 0.55).
narrative_ontology:constraint_metric(canal_panama_influence, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(canal_panama_influence, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(canal_panama_influence, tangled_rope).
narrative_ontology:human_readable(canal_panama_influence, "Geopolitical Influence over Panama Canal").
narrative_ontology:topic_domain(canal_panama_influence, "political").

domain_priors:requires_active_enforcement(canal_panama_influence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(canal_panama_influence, united_states).
narrative_ontology:constraint_beneficiary(canal_panama_influence, china).
narrative_ontology:constraint_victim(canal_panama_influence, panamanian_sovereignty).
narrative_ontology:constraint_victim(canal_panama_influence, global_trade_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL TRADE STABILITY (SNARE) — The ideal of stable, apolitical global trade is trapped by great power competition. Any disruption for strategic reasons is pure extraction from this ideal, with no recourse or exit. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(canal_panama_influence, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PANAMA (TANGLED ROPE) — As a sovereign state, Panama is organized but highly constrained. It benefits from canal revenues (coordination) but suffers a significant loss of strategic autonomy due to US and Chinese pressure (extraction). It cannot move the canal and must navigate these external forces. d≈0.70, f(d)≈1.06, σ=1.0 → χ≈0.58.
constraint_indexing:constraint_classification(canal_panama_influence, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNITED STATES (ROPE) — The US views its influence as a necessary coordination mechanism to ensure the canal's security and openness for global trade, which primarily benefits its own strategic and economic interests. From this viewpoint, extraction is framed as the cost of providing global security. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.08.
constraint_indexing:constraint_classification(canal_panama_influence, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CHINA (SCAFFOLD) — China's investments in ports and infrastructure are a temporary support structure (scaffold) to build long-term influence. The goal is to create a new geopolitical reality where its presence is permanent and dominant. The 'sunset' occurs when its influence is entrenched and no longer requires active construction. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.01.
constraint_indexing:constraint_classification(canal_panama_influence, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL SHIPPING COMPANIES (ROPE) — Actors like MSC are largely agnostic to the geopolitical struggle. For them, the canal is a pure coordination mechanism that reduces transit time and cost. As long as it operates efficiently, they perceive minimal extraction. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.43. This χ is high for a rope, but below the tangled_rope threshold, reflecting the friction of geopolitical risk.
constraint_indexing:constraint_classification(canal_panama_influence, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The system has a clear coordination function (facilitating ~5% of global trade) but is inextricably linked with coercive geopolitical extraction, where great powers vie for control at the expense of Panamanian sovereignty and global stability. This is the canonical Tangled Rope structure. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(canal_panama_influence, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(canal_panama_influence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(canal_panama_influence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(canal_panama_influence, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(canal_panama_influence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(canal_panama_influence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55) is high, representing the extraction of strategic sovereignty from Panama and the imposition of geopolitical risk on global trade, rather than direct monetary extraction. Suppression (0.75) is very high due to the lack of viable alternative shipping routes and the active diplomatic and economic pressure exerted by the US and China to limit each other's influence. Theater Ratio (0.30) is low-to-moderate; while diplomatic rhetoric is constant, the core of the constraint involves tangible actions like port acquisitions and infrastructure deals.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The US perceives a Rope, where its hegemony provides a stable, coordinated system. China perceives a Scaffold, a temporary project to build its own influence. Commercial users also see a Rope, caring only for function. In contrast, Panama experiences a Tangled Rope, balancing the economic benefits against a severe loss of sovereignty. The abstract ideal of apolitical global trade is caught in a Snare, bearing all the risk of the conflict with no agency.
 *
 * DIRECTIONALITY LOGIC:
 *   The US and China are designated as beneficiaries, as they seek to gain strategic advantage from control over the canal. Panama and the abstract concept of Global Trade Stability are victims, as they bear the costs of this competition through lost autonomy and increased risk. This structural relationship directly informs the directionality calculations, leading to negative effective extraction (χ) for the beneficiaries and high positive χ for the victims, thus explaining the wide perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This case demonstrates how the framework resolves mandatrophy by refusing to assign a single classification to a complex geopolitical reality. A simplistic analysis might label the situation a 'Snare' for Panama or a 'Rope' for the world. Deferential Realism shows that both, and more, are true simultaneously. The constraint's identity is the full set of indexed classifications. The system correctly identifies a core coordination function (the canal works) that is inseparable from an extractive power struggle, the hallmark of a Tangled Rope from the analytical view.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chinese_intent_ambiguity,
    'Is China''s involvement (e.g., port operations) purely commercial or a precursor to strategic/military control via dual-use infrastructure?',
    'Declassified intelligence assessments of Chinese port operations; an overt attempt by China to leverage its position during a crisis.',
    'If purely commercial, the constraint is closer to a Rope/Scaffold. If strategic, it confirms the Snare/Tangled Rope classification for other actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chinese_intent_ambiguity, empirical, 'Ambiguity of Chinese commercial vs. strategic intent in canal infrastructure.').

omega_variable(
    us_resolve_threshold,
    'What is the threshold of Chinese influence that would trigger overt US intervention (economic or military) to reassert control?',
    'A crisis event, such as Panama granting exclusive port rights to a Chinese state-owned enterprise or denying transit to US naval assets.',
    'A low threshold confirms the US role is actively coercive (Snare-like), while a high threshold suggests its role is a more passive coordination function (Rope-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_resolve_threshold, empirical, 'The point at which the US would intervene to counter Chinese influence.').

omega_variable(
    panama_agency_limit,
    'Can Panama successfully leverage the great power competition to maximize its own sovereignty and economic benefit, or is it an inevitable pawn?',
    'Longitudinal analysis of Panamanian policy decisions on port contracts, security agreements, and diplomatic alignments over the next decade.',
    'If Panama demonstrates agency, its Tangled Rope perspective is confirmed. If it consistently capitulates to the stronger power, its position is closer to being trapped in a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(panama_agency_limit, conceptual, 'The degree of real strategic autonomy held by Panama.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(canal_panama_influence, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cana_tr_t2000, canal_panama_influence, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(cana_tr_t2015, canal_panama_influence, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(cana_tr_t2030, canal_panama_influence, theater_ratio, 2030, 0.3).

% Extraction over time
narrative_ontology:measurement(cana_be_t2000, canal_panama_influence, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(cana_be_t2015, canal_panama_influence, base_extractiveness, 2015, 0.45).
narrative_ontology:measurement(cana_be_t2030, canal_panama_influence, base_extractiveness, 2030, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(canal_panama_influence, global_infrastructure).
narrative_ontology:affects_constraint(canal_panama_influence, global_supply_chains).
narrative_ontology:affects_constraint(canal_panama_influence, us_naval_hegemony).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
