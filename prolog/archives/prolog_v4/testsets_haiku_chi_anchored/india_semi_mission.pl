% ============================================================================
% CONSTRAINT STORY: india_semi_mission
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_india_semi_mission, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: india_semi_mission
 *   human_readable: India Semiconductor Mission 2.0
 *   domain: economic/industrial_policy
 *
 * SUMMARY:
 *   India Semiconductor Mission 2.0 combines ₹76,000 crore in direct
 *   subsidies, capital incentives, and infrastructure support to attract
 *   multinational chip manufacturers (TSMC, Samsung, Intel, Micron) and
 *   develop indigenous semiconductor design and manufacturing capability.
 *   Launched with rhetoric of 'Atmanirbhar Bharat' (self-reliant India), the
 *   mission faces a fundamental structural tension: the immediate mechanism
 *   (massive subsidies to MNCs) contradicts the stated goal (indigenous
 *   capability). The constraint exhibits all six DR types from different
 *   perspectives, making it a diagnostic case for economic policy extraction
 *   masquerading as coordination.
 *
 * KEY AGENTS:
 *   - Multinational Chipmakers (TSMC, Samsung, Intel, Micron): Primary beneficiary (institutional/arbitrage) — receive ₹20,000+ crore in capital subsidies, preferential tax treatment, land at concessional rates, and infrastructure guarantees with exit option to relocate if incentives decline
 *   - Indian Government & Policy Administration: Institutional actor (organized/constrained) — coordinates with MNCs, captures geopolitical positioning and eventual export revenue, but faces ₹76,000 crore fiscal commitment and political pressure on fiscal sustainability
 *   - Excluded Indian Startups: Primary victim (powerless/trapped) — startups below subsidy thresholds face extraction (crowding out of capital, suppressed market entry) without coordination benefit; lack capital for fabs and cannot exit domestic market without bankruptcy
 *   - Consumer Electronics Market: Secondary victim (moderate/constrained) — benefits from eventual local supply but experiences near-term price inflation and constrained sourcing options due to protected markets
 *   - Skill Training & Design Center Ecosystem: Organized actors (organized/mobile) — represent scaffold dimension; skill development and indigenous design capability have sunset logic if ecosystem matures over 15-20 years
 *   - Legacy Import Substitution Institutions: Institutional actors (institutional/constrained) — reproduce historical Nehruvian ISI rhetoric but with different mechanisms (MNC-dependent fabs instead of indigenous state enterprises)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(india_semi_mission, 0.52).
domain_priors:suppression_score(india_semi_mission, 0.62).
domain_priors:theater_ratio(india_semi_mission, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(india_semi_mission, extractiveness, 0.52).
narrative_ontology:constraint_metric(india_semi_mission, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(india_semi_mission, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(india_semi_mission, tangled_rope).
narrative_ontology:human_readable(india_semi_mission, "India Semiconductor Mission 2.0").
narrative_ontology:topic_domain(india_semi_mission, "economic/industrial_policy").

domain_priors:requires_active_enforcement(india_semi_mission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(india_semi_mission, multinational_chipmakers).
narrative_ontology:constraint_beneficiary(india_semi_mission, indian_government_exporters).
narrative_ontology:constraint_beneficiary(india_semi_mission, skill_training_vendors).
narrative_ontology:constraint_victim(india_semi_mission, indian_startups_without_subsidy_access).
narrative_ontology:constraint_victim(india_semi_mission, consumer_electronics_price_inflation).
narrative_ontology:constraint_victim(india_semi_mission, regional_distribution_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED STARTUPS (SNARE) — Startups below subsidy thresholds face extraction without coordination benefit. Cannot exit domestic market due to capital requirements and foreign market barriers. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.65. Mission crowds out competing indigenous innovation pathways.
constraint_indexing:constraint_classification(india_semi_mission, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CONSUMER ELECTRONICS SECTOR (TANGLED ROPE) — Benefits from eventual local chip supply reducing import dependency, but experiences near-term price inflation from subsidized oversupply and protected markets. Constrained by tariff structures that reduce competitive sourcing options. d≈0.68, f(d)≈1.03, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(india_semi_mission, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MULTINATIONAL CHIPMAKERS (ROPE) — Primary beneficiaries receiving ₹20,000+ crore in capital subsidies and preferential access to tax incentives, land, and government infrastructure. Arbitrage exit allows them to reallocate to other nations if incentives decline. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.06. Pure coordination: they coordinate with government on fab locations; negative extraction reflects net benefit position.
constraint_indexing:constraint_classification(india_semi_mission, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDIAN GOVERNMENT (TANGLED ROPE) — Institutional beneficiary (coordinates with MNCs, captures export revenue, geopolitical strategic positioning) AND victim (massive fiscal commitment ₹76,000 crore budget, opportunity cost for healthcare/education, dependency on MNC decisions). Constrained by need to maintain attractiveness to MNCs while managing domestic political pressure. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.34. Active enforcement through subsidies, regulatory fast-tracking, land acquisition.
constraint_indexing:constraint_classification(india_semi_mission, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: IMPORT SUBSTITUTION INSTITUTIONAL NARRATIVE (PITON) — The mission's framing as 'Atmanirbhar Bharat' (self-reliant India) performs the historical legacy of Nehruvian import substitution industrialization (ISI), but the actual mechanism (inviting MNC fabs with maximum subsidies) contradicts this narrative. Theater ratio=0.68: Mission deploys nationalist language covering contingent MNC-dependent supply chains. The performative component has increased as actual self-reliance stalls.
constraint_indexing:constraint_classification(india_semi_mission, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SKILL TRAINING & ECOSYSTEM PHASE (SCAFFOLD) — Skill development programs and design center investment represent temporary coordination scaffolding with potential sunset. If India develops indigenous fabless design capability and equipment suppliers, the dependency on MNC fabs declines over 15-20 years. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.12. High theater (workforce training reports, capability statements) but low effective extraction if ecosystem matures.
constraint_indexing:constraint_classification(india_semi_mission, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GEOPOLITICAL CONSTRAINT (MOUNTAIN?) — Risk of false summit: the constraint may appear as an immutable geopolitical reality ('India cannot compete with Taiwan/South Korea without massive subsidies') but is actually a contingent institutional arrangement. The analytical view naturalizes what requires active extraction mechanism (government enforcement of favorable terms, suppression of indigenous alternatives, sustained subsidy flow). Theater=0.68, ε=0.52 contradict mountain; this is likely a false peak.
constraint_indexing:constraint_classification(india_semi_mission, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(india_semi_mission_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(india_semi_mission, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(india_semi_mission, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(india_semi_mission, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(india_semi_mission, TR),
    TR >= 0.70.

:- end_tests(india_semi_mission_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The mission extracts value from: (1) excluded domestic startups (crowding out capital and suppressing competition), (2) fiscal resources (₹76,000 crore opportunity cost), (3) consumer electronics market (near-term price inflation and market protection). However, extractiveness is not maximal (snare threshold ≥0.46, χ≥0.66) because genuine coordination benefits exist: MNC commitment to India increases supply security for Indian electronics makers, and skill/design center investment creates real capability spillovers. The extractiveness trajectory increases from 0.28 to 0.52 because (a) MNC incentive capture becomes evident as negotiations conclude, (b) fiscal opportunity costs accumulate, (c) indigenous startup exclusion persists without mitigation. Suppression (0.62): Moderate-high. Significant barriers include: (1) capital concentration in MNC-friendly incentive schemes, (2) regulatory fast-tracking that disadvantages non-beneficiary firms, (3) tariff and import protection that suppress price discovery, (4) lack of transparency on actual localization and value capture. Suppression is structural to the mission design: smaller players are suppressed through resource concentration and preferential treatment to large MNC partners. Theater ratio (0.68): High. Theater components include: (1) 'Atmanirbhar Bharat' branding that contradicts MNC-dependent structure, (2) workforce training reports and capability statistics deployed in mission narratives, (3) fab construction timelines that diverge from actual production ramp-up, (4) export revenue projections that assume successful technology transfer (unverified). Theater has increased over the interval as mission messaging has emphasized nationalist framing while actual operations reveal MNC control.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates significant perspectival divergence. Multinational chipmakers see a rope (coordination: aligning on India as fab location, solving supply chain resilience). The Indian government sees a tangled rope (coordination benefit from geopolitical positioning + extraction cost from fiscal commitment + victim burden from constrained alternatives). Excluded startups see a snare (pure extraction: suppression of competitive entry, no coordination benefit). The consumer electronics sector sees a tangled rope (mixed benefit from supply security, cost from price inflation and market constraint). Skill development institutions see a scaffold (temporary support with sunset as indigenous ecosystem matures). The legacy import substitution narrative sees a piton (institutional ritual persisting through inertia, with high theater masking contingent MNC control). The analytical observer risks false summit (naturalizing geopolitical constraint where policy choice exists). The perspectival gap reveals that the mission functions as redistribution from excluded domestic actors and consumers to MNCs, disguised as national capability building.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational chipmakers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.06. Net beneficiary (negative effective extraction = receiving subsidy). Indian government: Both beneficiary (geopolitical positioning, eventual export revenue) and victim (fiscal commitment, constrained alternatives) + constrained (cannot walk away from mission due to political sunk cost and geopolitical commitment) → d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.34. Excluded startups: Victim + trapped → d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.65. High extraction. Consumer market: Victim + constrained → d≈0.68, f(d)≈1.03, σ=1.0 → χ≈0.53. Moderate-high extraction. Skill ecosystem: Organized + mobile → d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.12. Low effective extraction due to exit option (scaling down if ecosystem doesn't mature). Legacy institutions: Institutional + constrained → d≈0.30, f(d)≈0.25, σ=1.0 → χ≈0.17. Piton classification from theater gate, not high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY UNRESOLVED. The central tension is whether India Semiconductor Mission 2.0 is a genuine scaffold building indigenous capability (sunset logic valid, tangled rope justified by temporary coordination asymmetry) or a permanent snare capturing fiscal resources and suppressing indigenous competition (extractiveness will not decline, justifying snare classification for most agents). The extant data cannot resolve this. Key indicators to watch: (1) Actual localization rates for design, manufacturing, and supply chain (target >50% by year 5). If achieving <30%, snare diagnosis confirmed. (2) Indigenous fab startups funded post-mission launch (target: 3-5 new entrants). If zero, victim suppression confirmed. (3) Theater ratio trajectory — if theater_ratio continues rising above 0.70, piton transformation is occurring (institutional degradation). (4) Fiscal sustainability — if government extends subsidies beyond initial commitment or raises fiscal limits, extraction mechanism is self-reinforcing (snare). (5) MNC exit behavior — if any major partner exits when subsidies plateau, dependency trap confirmed. The mandatrophy cannot be resolved at current ε=0.52; resolution requires either (a) empirical confirmation of capability transfer and indigenous ecosystem emergence (descends to rope/scaffold, ε<0.35), or (b) continued high theater and fiscal escalation with suppressed startup entry (ascends to snare, ε>0.66, mandatrophy becomes critical).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    msc_subsidy_dependency_trap,
    'Does the subsidy model create long-term dependency where MNCs capture rents indefinitely, or does it genuinely bootstrap indigenous capability?',
    '10-year historical comparison: (1) Localization percentage of fab operations, (2) Emergence of Indian fab-adjacent services (equipment, chemicals, design), (3) MNC exit behavior if subsidies decline, (4) Cost of capital for indigenous fab startups post-mission',
    'If dependency trap: snare classification holds for all agents except MNCs. If genuine capability building: scaffold and rope perspectives strengthen. This is the central mandatrophy question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(msc_subsidy_dependency_trap, empirical, 'Whether subsidy model creates long-term dependency or enables indigenous capability').

omega_variable(
    atmanirbhar_framing_contradiction,
    'Does ''Atmanirbhar Bharat'' (self-reliance) framing mask contingent MNC-dependent supply chains, or does it accurately describe a transition state?',
    'Linguistic and policy analysis: (1) Comparison of ''self-reliance'' rhetoric vs actual value chain control, (2) Localization metrics for design, manufacturing, and supply chain, (3) Government messaging consistency over time',
    'If framing masks dependency: piton theater diagnosis confirmed. If transition framing is accurate: scaffold sunset logic is genuine, not performative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(atmanirbhar_framing_contradiction, conceptual, 'Whether self-reliance framing masks or accurately describes MNC dependency').

omega_variable(
    fiscal_opportunity_cost_distributional,
    'Are the distributional consequences of ₹76,000 crore fiscal commitment (who pays, who benefits) sufficiently transparent and politically defensible?',
    'Fiscal incidence analysis: (1) Tax burden distribution (wealthy vs poor, urban vs rural), (2) Benefit distribution (MNC shareholder vs Indian worker vs consumer), (3) Counterfactual spending on healthcare, education, rural infrastructure',
    'If incidence highly regressive: victim group expands significantly, snare classification broadens. If progressive or neutral: tangled rope balancing holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_opportunity_cost_distributional, preference, 'Distributional fairness of fiscal commitment across income and regional groups').

omega_variable(
    geopolitical_constraint_vs_policy_choice,
    'Is India''s semiconductor challenge a geopolitical constraint (mountain) or a contingent policy choice that could be structured differently?',
    'Comparative institutional analysis: (1) Alternative funding models (equity stakes, performance bonds, indigenous consortium), (2) Cost comparison vs subsidy model, (3) Counterfactual scenarios (no mission, different mission design)',
    'If true geopolitical constraint: mountain perspective gains credibility. If policy choice: false summit diagnosis stands; constraint is tangled rope throughout.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_constraint_vs_policy_choice, conceptual, 'Whether semiconductor challenge is geopolitical inevitability or institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(india_semi_mission, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isem_tr_t0, india_semi_mission, theater_ratio, 0, 0.45).
narrative_ontology:measurement(isem_tr_t3, india_semi_mission, theater_ratio, 3, 0.58).
narrative_ontology:measurement(isem_tr_t6, india_semi_mission, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(isem_be_t0, india_semi_mission, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(isem_be_t3, india_semi_mission, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(isem_be_t6, india_semi_mission, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(india_semi_mission, resource_allocation).
narrative_ontology:affects_constraint(india_semi_mission, indian_electronics_supply_chain_dependency).
narrative_ontology:affects_constraint(india_semi_mission, semiconductor_geopolitical_competition).
narrative_ontology:affects_constraint(india_semi_mission, domestic_startup_capital_access).

% DUAL FORMULATION NOTE:
% The semiconductor mission operates at the intersection of three constraint families: (1) India's structural dependency on imported chips (upstream geopolitical constraint), (2) the specific policy instrument (this story), and (3) downstream effects on indigenous startup formation and consumer market structure. This story's ε=0.52 reflects the policy mechanism; the upstream geopolitical constraint has different ε reflecting whether semiconductor independence is achievable; the downstream startup capital constraint reflects the mission's crowding-out effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(india_semi_mission, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
