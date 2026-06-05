% ============================================================================
% CONSTRAINT STORY: us_venezuela_oil_pressure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_venezuela_oil_pressure, []).

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
 *   constraint_id: us_venezuela_oil_pressure
 *   human_readable: US Geopolitical & Economic Pressure on Venezuela's Oil Sector
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The US-Venezuela oil pressure constraint models decades of diplomatic,
 *   economic, covert, and military pressure exerted by the United States
 *   geopolitical apparatus to maintain favorable access to Venezuela's vast
 *   petroleum reserves and ensure Venezuelan policy alignment with US
 *   hemisphere dominance interests. The constraint encompasses sanctions
 *   regimes (oil import bans, asset freezes, credit denials), covert support
 *   for opposition movements, diplomatic isolation, support for neighboring
 *   states' containment policies, and explicit regime-change rhetoric. From
 *   the US perspective (energy security, geopolitical hegemon positioning),
 *   the constraint solves a coordination problem: ensuring a strategically
 *   located petro-state remains within the Western sphere of influence and
 *   unable to export revolutionary ideology or organize regional energy
 *   independence. From Venezuela's perspective (population, state apparatus,
 *   regional interests), the constraint is extraction: resource
 *   subordination, sovereignty violation, economic warfare, and coercive
 *   structural adjustment. The constraint exhibits all hallmarks of a Tangled
 *   Rope: genuine coordination benefits for beneficiaries (US energy
 *   security, institutional stability, market predictability), asymmetric
 *   extraction for victims (Venezuelan economic collapse, geopolitical
 *   subordination, population suffering), and active enforcement (sanctions
 *   mechanisms, diplomatic pressure, military posturing). Theater_ratio=0.58
 *   reflects that US pressure is justified through universal principles
 *   (democracy, human rights, rule of law) while functioning as resource
 *   extraction — the universalizing rhetoric partially obscures the
 *   particular extraction mechanism.
 *
 * KEY AGENTS:
 *   - US Energy Security Apparatus: Primary beneficiary (institutional/arbitrage) — benefits from Venezuela remaining politically subordinate, economically constrained, unable to organize alternative energy partnerships, exportable oil at favorable terms or unavailable to US competitors
 *   - Transnational Oil Corporations: Primary beneficiary (institutional/arbitrage) — benefits from reduced Venezuelan competition, ability to renegotiate terms post-regime change, market control, prevented OPEC+ coordinated resistance
 *   - Venezuelan Population: Primary victim (powerless/trapped) — bears extraction through fuel shortages, hyperinflation, economic collapse, forced emigration, loss of resource sovereignty, inability to invest in sector
 *   - PDVSA / Venezuelan State Petroleum Industry: Primary victim (organized/constrained) — extraction through asset freezes, credit denials, international financing blockade, refinery sanctions, production collapse, loss of technical capacity
 *   - Regional States (Colombia, Brazil, Caribbean): Secondary actors (powerful/mobile) — experience as Tangled Rope: coordination benefit (US security umbrella, trade access) coupled with extraction (forced participation in isolation regime, geopolitical costs, constrained energy sourcing)
 *   - Global South Energy Independence: Organized victims (organized/constrained) — bear extraction through sanctions precedent threatening sovereignty, constrained OPEC+ coordination capacity, geopolitical leverage normalization
 *   - International Institutions (IMF, OAS, World Bank): Institutional mediators (institutional/constrained) — functionally serve US interests while maintaining neutrality theater; persist through legitimacy maintenance rather than independent function
 *   - Analytical Observer: Structural view (analytical/analytical) — sees full hybrid structure: coordination benefits for beneficiaries, extraction costs for victims, driven by resource control and geopolitical positioning imperatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_venezuela_oil_pressure, 0.68).
domain_priors:suppression_score(us_venezuela_oil_pressure, 0.72).
domain_priors:theater_ratio(us_venezuela_oil_pressure, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_venezuela_oil_pressure, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_venezuela_oil_pressure, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_venezuela_oil_pressure, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_venezuela_oil_pressure, tangled_rope).
narrative_ontology:human_readable(us_venezuela_oil_pressure, "US Geopolitical & Economic Pressure on Venezuela's Oil Sector").
narrative_ontology:topic_domain(us_venezuela_oil_pressure, "geopolitical/economic").

domain_priors:requires_active_enforcement(us_venezuela_oil_pressure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_venezuela_oil_pressure, us_energy_security_apparatus).
narrative_ontology:constraint_beneficiary(us_venezuela_oil_pressure, transnational_oil_corporations).
narrative_ontology:constraint_beneficiary(us_venezuela_oil_pressure, geopolitical_hegemon_positioning).
narrative_ontology:constraint_victim(us_venezuela_oil_pressure, venezuelan_state_petroleum_industry).
narrative_ontology:constraint_victim(us_venezuela_oil_pressure, venezuelan_population_economic_access).
narrative_ontology:constraint_victim(us_venezuela_oil_pressure, regional_energy_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VENEZUELAN POPULATION (SNARE) — Trapped within territorial and economic boundaries. Cannot exit the constraint; bears full extraction cost through fuel shortages, hyperinflation, economic collapse. d≈0.93, f(d)≈1.40, σ=1.1 → χ≈1.05. Pure extraction with high coercion and minimal coordination benefit.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: PDVSA / VENEZUELAN STATE OIL SECTOR (SNARE) — Organized but constrained. Cannot exit sanctions regime or geopolitical pressure; extraction through asset freezes, credit denials, refinery sanctions, and production blockade. d≈0.88, f(d)≈1.32, σ=1.0 → χ≈0.90. Extraction with enforcement overhead.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: US ENERGY SECURITY APPARATUS (ROPE) — Institutional actor with arbitrage exit (global energy market diversification, strategic alternatives). Experiences the constraint as coordination: ensuring Venezuela remains within US sphere of influence solves energy access and hemisphere dominance problems. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.09. Net beneficiary. Benefits from coordination function (containment, access control) with minimal extraction cost.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRANSNATIONAL OIL CORPORATIONS (ROPE) — Institutional actors (ExxonMobil, Chevron legacy positions, trading firms) with arbitrage exit. Experience the constraint as coordination: sanctions and pressure on PDVSA reduce competition, stabilize market share, enable licensing negotiations at favorable terms post-regime-change. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.09. Net beneficiary. Coordination benefit (market control) with no extraction cost.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGIONAL STATES (TANGLED ROPE) — Powerful actors (Colombia, Brazil) with significant mobile exit options (trade reorientation, China/Russia partnerships, energy diversification). Experience the constraint as hybrid: coordination benefit (US security umbrella, trade access) coupled with extraction (forced to participate in isolation regime, constrained energy sourcing, geopolitical costs of non-alignment). d≈0.52, f(d)≈0.67, σ=1.1 → χ≈0.50. Mixed coordination and extraction.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: GLOBAL SOUTH ENERGY INDEPENDENCE (TANGLED ROPE) — Organized actors (OPEC+, ALBA member states, non-aligned movement) with constrained exit. Experience constraint as hybrid: coordination benefit (Venezuela as exemplar of imperial extraction vulnerability; solidarity narrative; reduced US energy monopoly if Venezuela succeeds) coupled with extraction (sanctions precedent threatens their own sovereignty; normalization of geopolitical leverage via energy). d≈0.60, f(d)≈0.78, σ=1.2 → χ≈0.59. Moderate extraction with real coordination function.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL INSTITUTIONS (PITON) — Institutions (IMF, World Bank, OAS) nominally neutral but functionally aligned with US interests. Theater_ratio=0.58 indicates significant performative content: formal neutrality, rule-of-law rhetoric, technical conditionality masking geopolitical preferences. Institution persists through inertia and legitimacy theater despite degraded functional independence. d≈0.35, f(d)≈0.33, σ=1.1 → χ≈0.21. Low effective extraction but high theatrical maintenance.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, the constraint exhibits genuine coordination (US-led security architecture, energy market stability, institutional order) coupled with asymmetric extraction (Venezuelan sovereignty violation, resource control, geopolitical subordination). d≈0.68, f(d)≈1.01, σ=1.2 → χ≈0.68. The observer sees the full hybrid structure: real coordination benefits for beneficiaries, real extraction costs for victims, not reducible to pure coercion or pure mutual benefit.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_venezuela_oil_pressure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_venezuela_oil_pressure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_venezuela_oil_pressure, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_venezuela_oil_pressure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_venezuela_oil_pressure, TR),
    TR >= 0.70.

:- end_tests(us_venezuela_oil_pressure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint directly extracts Venezuelan state resources, population welfare, and sovereign decision-making capacity. Measurement trajectory shows progressive increase from 0.35 (1980s, low-level diplomatic pressure) through 0.52 (2003-2015, sanctions escalation) to 0.68 (2017-present, maximum enforcement with asset seizure, refinery sanctions, production blockade). The increase reflects deepening enforcement mechanisms and reduced Venezuelan exit options. Not at maximum snare level (0.75+) because beneficiaries have alternative energy sources and could, in principle, negotiate rather than coerce — the constraint persists because extraction is profitable and coordination-framed rather than purely forced. Suppression (0.72): High. Significant barriers to Venezuelan exit include: international financing blockade (impossible to finance sector recovery), technological isolation (sanctioned from spare parts, refining equipment), diplomatic isolation (UNASUR, regional support mechanisms degraded), covert operations (opposition funding, attempted coup support), and neighboring-state containment. However, suppression is not total because China and Russia provide alternative partnerships — Venezuelan suppression is constrained relative to fully isolated states. Theater ratio (0.58): Moderate-high. US pressure is rhetorically justified through democracy promotion, human rights concerns, rule-of-law principles, and anti-corruption narratives. These universal principles partially obscure the particular extraction mechanism (resource access, geopolitical subordination, energy monopoly maintenance). The theater is not extreme (>0.70) because the enforcement mechanisms are explicit and visible (named sanctions, stated policy objectives), not purely performative. The increase over time (0.38 → 0.58) reflects increasing gap between stated justifications and actual enforcement intensity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence across the observation site. The US apparatus sees Rope (pure coordination: ensuring hemisphere stability, energy security, institutional order) with no extraction cost to itself. Transnational oil corporations see Rope (market control, elimination of competitive pressure) with coordination benefit. The Venezuelan population sees Snare (pure extraction: resource subordination, economic collapse, no benefit) with maximal coercion and no exit. PDVSA sees Snare (production collapse, technical degradation, international isolation) with organized capacity but zero escape options. Regional states see Tangled Rope (security benefits coupled with geopolitical costs, constrained energy sourcing). Global South energy independence movement sees Tangled Rope (solidarity benefit coupled with sovereignty-threat precedent). International institutions see Piton (functional degradation masked by neutrality theater). The analytical observer sees Tangled Rope (genuine coordination for beneficiaries, genuine extraction for victims). The mandatrophy is resolved by recognizing that all six types are legitimate structural readings — the constraint is not a single type but a presheaf of types varying across observational positions.
 *
 * DIRECTIONALITY LOGIC:
 *   US Energy Apparatus: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11 → negative χ = net benefit. PDVSA: Victim + organized but constrained → d≈0.88, f(d)≈1.32 → high χ = extraction. Venezuelan population: Victim + trapped → d≈0.93, f(d)≈1.40 → maximum χ = maximum extraction. Regional states: Mixed (some benefit from US security, some cost from forced participation) + mobile → d≈0.52, f(d)≈0.67 → moderate χ = mixed. Global South: Victim (precedent threat) + organized but constrained → d≈0.60, f(d)≈0.78 → moderate extraction. International institutions: Institutional + constrained (nominally independent but functionally aligned) → d≈0.35, f(d)≈0.33 → low extraction (theater gate captures degradation).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint resolves the mandatrophy by explicit declaration that it is Tangled Rope (χ∈[0.40, 0.90], ε≥0.30, suppression≥0.40, requires_active_enforcement=true, has both beneficiaries and victims). The mandatrophy tension arises from the US beneficiary perspective (which sees Rope: pure coordination, no extraction) conflicting with the Venezuelan victim perspective (which sees Snare: pure extraction, no coordination). The resolution recognizes that both perspectives are structurally valid but represent different observational positions within an asymmetric constraint. From the beneficiary's position (US apparatus), the constraint functions as coordination (solving the problem of maintaining hemisphere alignment and energy security). From the victim's position (Venezuelan population, PDVSA), the constraint functions as extraction (subordinating resource sovereignty and imposing economic costs). The Tangled Rope classification embeds both observations: (1) The constraint HAS a genuine coordination function (US-led security architecture, energy market stability, institutional order) that benefits its primary beneficiaries. (2) The constraint SIMULTANEOUSLY operates as asymmetric extraction (Venezuelan resources, sovereignty, population welfare transferred to US interests) for its victims. (3) The extraction is ENABLED by active enforcement (sanctions, diplomatic isolation, covert operations, military posturing) that requires sustained institutional effort. Neither the pure-coordination Rope view nor the pure-extraction Snare view captures the full structure. The Tangled Rope classification prevents both (a) naturalizing the constraint as beneficial coordination (what the beneficiary perspective risks), and (b) misidentifying the coordination function as purely rhetorical theater (what the victim perspective risks). The theater ratio (0.58) captures the gap between stated justifications (democracy, human rights, rule of law) and actual enforcement mechanisms (resource access, geopolitical subordination). This gap is substantial but not maximal — the enforcement mechanisms are explicit and visible, not purely performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_security_trade,
    'Is the constraint fundamentally about US energy security (justifiable coordination) or about imperial resource control (unjustifiable extraction)?',
    'Counterfactual analysis: What would US policy be if Venezuela had no oil? Comparison with US pressure on resource-scarce Latin American states. Historical analysis of post-Cold War Venezuela policy shift timing relative to OPEC+ coordination threats.',
    'If security: constraint classifies as Rope for US from all angles (pure coordination). If imperial control: constraint classifies as Snare from analytical view (pure extraction hidden behind security rhetoric). Currently classified as Tangled Rope based on hybrid structure, but this omega determines the balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_security_trade, conceptual, 'Whether constraint prioritizes US energy security or imperial resource control').

omega_variable(
    sanctions_mechanism_efficacy,
    'Do sanctions and pressure mechanisms actually constrain Venezuela (functioning extraction) or merely signal resolve without material effect?',
    'Causal analysis: PDVSA production decline pre/post-2017 sanctions regime; correlation with international financing blockade vs. internal mismanagement. Counterfactual: What would PDVSA production trajectory be absent sanctions?',
    'If sanctions highly effective: suppression=0.72 is justified; extraction is real and consequential. If sanctions have marginal effect relative to internal factors: suppression overstated; constraint may reclassify toward lower extraction, higher theater (piton tendency).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sanctions_mechanism_efficacy, empirical, 'Whether sanctions actually constrain Venezuelan production or are largely symbolic').

omega_variable(
    chinese_russian_substitution,
    'Do Chinese and Russian energy partnerships represent genuine alternatives to US-led constraint, or do they merely substitute one hegemon for another?',
    'Structural comparison: terms of Chinese loans vs. US trade deals; Russian energy barter conditions; degree of Venezuelan sovereign control over production decisions under alternative arrangements.',
    'If genuine alternatives: Venezuelan exit options upgrade from ''trapped'' to ''mobile'' (d drops substantially); classification shifts from Snare toward Tangled Rope or Rope. If merely alternative extraction: no fundamental change in constraint structure; Venezuela remains in snare but with different beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chinese_russian_substitution, empirical, 'Whether alternative partnerships offer genuine exit or substitute extraction').

omega_variable(
    regime_change_counterfactual,
    'Would a US-aligned Venezuelan government actually reduce the constraint (reclassifying toward Rope), or would pressure mechanisms persist under different framing?',
    'Historical analysis of US pressure on US-aligned petrostates (Saudi Arabia, Iraq pre-2003, Kuwait). Comparison of resource extraction terms under different political regimes in same region.',
    'If regime change → Rope: the constraint is fundamentally about alignment (coordination problem). If regime change → persistent Snare: the constraint is fundamentally about resource extraction (imperial extraction), regardless of Venezuelan government preference.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_change_counterfactual, conceptual, 'Whether constraint is driven by regime type or by resource control imperatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_venezuela_oil_pressure, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usvzol_tr_t0, us_venezuela_oil_pressure, theater_ratio, 0, 0.38).
narrative_ontology:measurement(usvzol_tr_t20, us_venezuela_oil_pressure, theater_ratio, 20, 0.5).
narrative_ontology:measurement(usvzol_tr_t40, us_venezuela_oil_pressure, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(usvzol_be_t0, us_venezuela_oil_pressure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(usvzol_be_t20, us_venezuela_oil_pressure, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(usvzol_be_t40, us_venezuela_oil_pressure, base_extractiveness, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_venezuela_oil_pressure, global_infrastructure).
narrative_ontology:affects_constraint(us_venezuela_oil_pressure, opec_plus_coordination).
narrative_ontology:affects_constraint(us_venezuela_oil_pressure, latin_american_energy_sovereignty).
narrative_ontology:affects_constraint(us_venezuela_oil_pressure, petro_currency_hegemony).
narrative_ontology:affects_constraint(us_venezuela_oil_pressure, rare_earth_geopolitical_extraction).

% DUAL FORMULATION NOTE:
% The Venezuela oil pressure constraint is the instantiation of a broader structural pattern: US use of geopolitical leverage to extract resource subordination and maintain energy hegemony. Related constraints in the family include: OPEC+ coordination attempts (victims' coordination response), Latin American energy sovereignty movements (structural resistance), petro-currency hegemony (monetary mechanism of extraction), and rare earth mineral extraction (parallel resource extraction mechanism). This constraint should be decomposed into sub-constraints if empirical analysis reveals that different enforcement mechanisms (diplomatic, economic, covert, military) have different ε values — currently modeled as unified but may warrant separation if sanctions efficacy differs significantly from covert operations efficacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_venezuela_oil_pressure, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
