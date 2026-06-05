% ============================================================================
% CONSTRAINT STORY: regional_military_deterrence_mideast
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regional_military_deterrence_mideast, []).

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
 *   constraint_id: regional_military_deterrence_mideast
 *   human_readable: US/Israeli Military Deterrence Posture against Iran
 *   domain: geopolitical/military
 *
 * SUMMARY:
 *   The US/Israeli military deterrence posture against Iran represents a
 *   hybrid coordination-extraction constraint in a multipolar regional
 *   security environment. Ostensibly designed to prevent Iranian aggression
 *   and nuclear proliferation, the posture simultaneously functions as: (a)
 *   genuine coordination mechanism (preventing kinetic conflict, stabilizing
 *   regional trade), (b) extraction regime (concentrating military spending,
 *   constraining Iranian regional influence, imposing economic costs via
 *   sanctions coupling), (c) temporary security measure with stated sunset
 *   (non-proliferation goal), (d) degraded Cold War doctrine applied to
 *   asymmetric threats, and (e) rational strategic choice from dominant-power
 *   perspective. The constraint exhibits all six DR types from different
 *   observer positions. Base extractiveness (0.58) reflects moderate but
 *   rising costs to trapped agents (Iran, civilian populations) and
 *   constrained beneficiaries (Gulf monarchies, regional networks). Theater
 *   ratio (0.64) indicates substantial performative content: public
 *   deterrence messaging, military signaling, and political domestic theater
 *   exceed purely functional requirements. Suppression (0.68) reflects
 *   significant barriers to Iranian exit (military asymmetry, sanctions,
 *   diplomatic isolation) and constrained options for other regional actors.
 *   The constraint's extractive character increased over the 30-year interval
 *   as: (a) deterrence mission expanded from nuclear-focused to comprehensive
 *   regional hegemony, (b) forward presence became institutionalized rather
 *   than temporary, (c) sanctions coupling increased extraction cost
 *   independent of kinetic threat. The scaffold perspective
 *   (non-proliferation sunset) remains aspirational — actual sunset timeline
 *   is contingent on Iranian capitulation rather than negotiated agreement,
 *   suggesting the sunset clause is more narrative device than structural
 *   feature.
 *
 * KEY AGENTS:
 *   - US Strategic Command: Primary beneficiary (institutional/arbitrage) — captures strategic power projection, alliance leadership, hegemonic positioning; can relocate forces to other theaters
 *   - Israeli Defense Ministry: Primary beneficiary (powerful/arbitrage) — gains strategic insurance, reduces unilateral deterrence burden, arbitrage available if US commitment changes
 *   - Gulf Arab Monarchies: Constrained beneficiary (powerful/constrained) — benefit from Iranian deterrence but face domestic legitimacy costs, economic dependence on US, cannot exit alliance without security loss
 *   - Iranian Strategic Planners: Primary target (powerless/trapped) — no exit options, surrounded by deterrence architecture, economy sanctioned, proxies constrained; maximum suppression
 *   - Civilian Populations in Conflict Zones: Victims (powerless/trapped) — bear kinetic and economic risk of deterrence regime; no negotiating power or exit options
 *   - Regional Trade and Energy Networks: Secondary victims (moderate/constrained) — experience coordination benefit (reduced kinetic risk) but extraction cost (sanctions disruption, investment risk premium, military spending crowding)
 *   - International Non-Proliferation Regime: Institutional actor (organized/constrained) — attempted sunset mechanism (prevent Iranian nuclear acquisition) but sunset timeline ambiguous and conditional
 *   - Cold War Deterrence Doctrine: Institutional structure (institutional/constrained) — provides legitimating narrative; persists through institutional inertia despite changed threat environment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regional_military_deterrence_mideast, 0.58).
domain_priors:suppression_score(regional_military_deterrence_mideast, 0.68).
domain_priors:theater_ratio(regional_military_deterrence_mideast, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regional_military_deterrence_mideast, extractiveness, 0.58).
narrative_ontology:constraint_metric(regional_military_deterrence_mideast, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(regional_military_deterrence_mideast, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regional_military_deterrence_mideast, tangled_rope).
narrative_ontology:human_readable(regional_military_deterrence_mideast, "US/Israeli Military Deterrence Posture against Iran").
narrative_ontology:topic_domain(regional_military_deterrence_mideast, "geopolitical/military").

domain_priors:requires_active_enforcement(regional_military_deterrence_mideast).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regional_military_deterrence_mideast, us_strategic_command).
narrative_ontology:constraint_beneficiary(regional_military_deterrence_mideast, israeli_defense_ministry).
narrative_ontology:constraint_beneficiary(regional_military_deterrence_mideast, gulf_monarchy_coalition).
narrative_ontology:constraint_victim(regional_military_deterrence_mideast, iran_regional_influence).
narrative_ontology:constraint_victim(regional_military_deterrence_mideast, global_trade_stability).
narrative_ontology:constraint_victim(regional_military_deterrence_mideast, regional_civilian_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRANIAN STRATEGIC PLANNERS (SNARE) — Iran cannot exit the deterrence regime without catastrophic military costs. Surrounded by US/Israeli hardware, sanctioned economy, and constrained proxies. Maximum suppression: no diplomatic off-ramp without strategic concession. No arbitrage available. Classifies as pure Snare with d ≈ 0.90.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CIVILIAN POPULATIONS IN CONFLICT ZONES (SNARE) — No choice in deterrence regime; bear full risk of kinetic escalation. No exit options. Suppression is structural: cannot negotiate, cannot organize effectively across borders, cannot appeal deterrence logic. Theater is high — deterrence is framed as 'regional stability' while civilians experience it as existential threat.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: US STRATEGIC COMMAND (ROPE) — Experiences deterrence posture as coordination mechanism: forward presence enables power projection, alliance signaling, and crisis response capacity. Benefits from visibility, force demonstration, and hegemonic positioning. Low experienced extraction — the constraint solves real coordination problems (allied credibility, power projection) with reasonable overhead. Arbitrage options available (reposition assets to other theaters).
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ISRAELI DEFENSE MINISTRY (ROPE) — Net beneficiary of US deterrence architecture. Reduces burden of unilateral deterrence, enables offensive capabilities, gains strategic insurance. Arbitrage available: can reduce commitment if US withdraws. Suppression is low for this agent — maintains decision authority over own operations. Benefits outweigh costs.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: GULF ARAB MONARCHIES (TANGLED ROPE) — Benefit from deterrence of Iran, but constrained by: (a) domestic legitimacy risks from anti-American sentiment, (b) economic dependence on US military sales and protection, (c) inability to withdraw support without regional power vacuum. Experience both coordination benefit (security guarantee) and extraction (military spending, political subordination, arms dependency). Constrained exit — cannot leave alliance without losing security and economic position.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: REGIONAL TRADE AND ENERGY NETWORKS (TANGLED ROPE) — Coordination benefit: deterrence reduces existential kinetic risk, enabling commerce and energy flows. Extraction cost: (a) sanctions regimes disrupt supply chains, (b) military buildup crowds out capital for economic development, (c) geographic risk premium on shipping and investment. Constrained exit — cannot opt out of regional stability calculation without bearing full volatility cost.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: INTERNATIONAL NON-PROLIFERATION REGIME (SCAFFOLD) — Deterrence posture has explicit sunset logic: the regime intends to prevent Iranian nuclear weaponization, after which deterrence burden decreases or transitions to different mechanisms (e.g., inspection, confidence-building). Theater is high — deterrence is framed as temporary security measure pending negotiated settlement, but actual sunset timeline is ambiguous. Has_sunset_clause present but conditional on diplomatic breakthrough.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: COLD WAR DETERRENCE DOCTRINE (PITON) — The posture is largely performative continuation of bipolar deterrence logic applied to unipolar context. Theater (0.64) reflects: (a) public deterrence signaling that may not match actual warfighting plans, (b) political theater for domestic audiences, (c) institutional momentum of forward-deployed posture despite changed threat environment. Primary function (preventing interstate war) has partially atrophied in asymmetric threat era, but structure persists through inertia and institutional investment.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — Risk of naturalizing deterrence as immutable law: 'deterrence is necessary to prevent conflict; military presence is required for deterrence; therefore, large military presence is natural/inevitable.' This perspective masks the contingent institutional arrangements (alliance structure, nuclear threshold, sanctions regime) as universal structural requirements. The engine will identify this as a false summit — deterrence is a strategic choice with alternatives (negotiation, containment, defensive posture), not a law of nature.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regional_military_deterrence_mideast_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regional_military_deterrence_mideast, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regional_military_deterrence_mideast, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regional_military_deterrence_mideast, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regional_military_deterrence_mideast, TR),
    TR >= 0.70.

:- end_tests(regional_military_deterrence_mideast_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The deterrence posture extracts from Iran (military asymmetry, sanctions coupling, regional isolation) and from constrained regional actors (forced military spending, political subordination to US alliance logic). However, extraction is not maximal (0.70+) because: (a) genuine security coordination benefit exists (reduces existential conflict risk), (b) beneficiaries (US, Israel, Gulf monarchies) also bear costs (force deployment, commitment risks, domestic political exposure), (c) deterrence is framed as temporary (sunset logic via non-proliferation), not permanent extraction. Suppression (0.68): High. Significant barriers to Iranian exit (military power asymmetry, sanctions regime, diplomatic isolation, constrained proxy capacity). Barriers to regional actors' negotiation (security dependence, political alliance structure). However, suppression is not maximal because some diplomatic off-ramps exist (JCPOA precedent, negotiation possibility), and some regional actors retain voice (Gulf monarchies can negotiate with Iran independently). Theater ratio (0.64): Moderate-high. Substantial performative content includes: (a) public deterrence messaging designed for domestic US/Israeli audiences, (b) military signaling whose effectiveness is uncertain, (c) alliance display functions (demonstrating commitment to Gulf monarchies), (d) Cold War deterrence rhetoric applied to asymmetric threats where applicability is questionable. However, theater is not maximal (0.70+) because deterrence does generate real security effects: kinetic conflict reduction (probably), proxy constraint (possibly), regional stability signals (clearly).
 *
 * PERSPECTIVAL GAP:
 *   Massive perspectival divergence reveals the constraint's hybrid nature. US/Israeli institutional actors see Rope (coordination + power projection benefit). Gulf monarchies see Tangled Rope (coordination benefit for security but extraction via arms dependency and political subordination). Regional trade networks see Tangled Rope (security coordination benefit but sanctions extraction cost). Iranian planners see Snare (no exit, maximum suppression, pure extraction). Civilians see Snare (trapped, bearing kinetic risk, no negotiating power). Non-proliferation regime sees Scaffold (temporary measure with sunset). Cold War doctrine sees Piton (performative ritual persisting through inertia). Analytical observer risks seeing Mountain (deterrence as natural law of regional dynamics) — this is false summit, masking contingent strategic choice. The perspectival gap arises from: (a) power asymmetry (dominant actors see coordination, subordinated actors see extraction), (b) exit option differences (US has global deployment choices; Iran has none), (c) temporal horizons (US sees immediate power needs; Iran sees generational encirclement), (d) structural relationships (beneficiaries experience low chi; trapped agents experience high chi from same base ε).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent derives from beneficiary/victim status + exit options + power level. US Strategic Command: d ≈ 0.10 (beneficiary + arbitrage + powerful → low d → negative chi, i.e., constraint subsidizes this agent). Israeli Defense Ministry: d ≈ 0.20 (beneficiary + arbitrage + powerful → low d → negative chi). Gulf Monarchies: d ≈ 0.50 (mixed: beneficiary from Iran deterrence but victim of military spending + political subordination + constrained → moderate d → moderate chi). Iranian Strategic Planners: d ≈ 0.90 (victim + trapped + powerless → high d → high chi, maximum experienced extraction). Civilian Populations: d ≈ 0.95 (victim + trapped + powerless → maximum d → maximum chi, pure extraction experience). Regional Trade Networks: d ≈ 0.55 (mixed: benefit from security coordination but extraction from sanctions/investment risk + constrained exit → moderate-high d → moderate chi). The engine's sigmoid f(d) transforms these d values into power-modified extraction coefficients, scaling the base ε (0.58) into effective chi values that explain the perspectival gap: beneficiaries experience negative or low chi (they benefit); trapped agents experience chi ≥ 0.85 (they are extracted from).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by acknowledging that deterrence is genuinely both coordination and extraction, depending on observer position. The mandatrophy question is: 'Is this primarily coordination (Rope, Scaffold) or primarily extraction (Snare, Tangled Rope)?' The answer: it is both. For dominant agents (US, Israel), it is primarily coordination — solving the real problem of preventing regional kinetic conflict and enabling power projection. For trapped/subordinated agents (Iran, civilians), it is primarily extraction — constrained without negotiating power, bearing suppression costs. For constrained beneficiaries (Gulf monarchies, regional networks), it is hybrid — genuine coordination benefit for security, but extraction via arms dependency and political control. The Tangled Rope classification at the Gulf monarchy perspective captures this hybrid structure: the constraint genuinely coordinates security (preventing Iranian aggression that would harm all parties) while simultaneously extracting (forcing military spending, political subordination, arms dependence). The scaffold perspective (non-proliferation sunset) adds temporal complexity: if the sunset is real and conditional on Iranian non-proliferation, the constraint's extractive character should decline as the nuclear threat recedes. But if the sunset is narrative device and deterrence persists regardless of Iranian actions, the constraint is Snare disguised as Scaffold. Mandatrophy is resolved by modeling all six types and declaring which are perspectival readings vs. which are false summits (e.g., the Mountain perspective naturalizing deterrence as inevitable law).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_credibility_threshold,
    'What level of forward military presence is necessary and sufficient for effective deterrence of Iranian aggression?',
    'Comparative analysis of deterrence outcomes under different force postures; historical analysis of conflict initiation/non-initiation correlated with presence levels; game-theoretic modeling of Iranian decision calculus',
    'If threshold is significantly lower than current posture: current deterrence is over-extraction, theater-driven rather than functionally necessary. If threshold is significantly higher: deterrence is under-resourced and misclassified as adequate Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_credibility_threshold, empirical, 'Necessary force level for credible deterrence').

omega_variable(
    proxy_escalation_pathway,
    'Does US/Israeli deterrence posture actually prevent Iranian proxy aggression or does it drive proxy aggression by reducing direct conflict options?',
    'Time-series analysis of proxy attacks (Houthi, Hezbollah, militias) correlated with deterrence posture changes; attribution of proxy attacks to rational cost-calculation vs. retaliation for deterrence itself',
    'If deterrence prevents proxy action: justifies Rope/Scaffold classification. If deterrence drives proxy action: reframes cost-benefit; makes deterrence self-reinforcing extraction trap (Snare for all parties).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_escalation_pathway, empirical, 'Whether deterrence prevents or drives proxy escalation').

omega_variable(
    nuclear_threshold_manipulation,
    'Is the deterrence posture genuinely intended to prevent Iranian nuclear acquisition, or is the nuclear issue a pretext for maintaining regional hegemony independent of actual proliferation risk?',
    'Analysis of deterrence policy changes correlated with actual Iranian nuclear progress; examination of whether deterrence terms would relax if Iran accepts inspection regime; assessment of whether non-nuclear threats (ballistic missiles, proxies) are primary driver of posture',
    'If nuclear prevention is genuine: deterrence has legitimate Scaffold logic with sunset. If hegemony maintenance is primary: deterrence is Snare/Tangled Rope with no actual sunset — nuclear issue is pretext.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_threshold_manipulation, conceptual, 'Whether deterrence is genuinely nuclear-focused or hegemonically-motivated').

omega_variable(
    sanctions_extraction_coupling,
    'What portion of the deterrence posture''s extractive cost (suppression, opportunity cost) is coupled to sanctions regime vs. kinetic threat?',
    'Decomposition of Iranian strategic costs: sanctions cost on economy, military cost of deterrence avoidance, proxy cost of regional action, nuclear cost of weapons program. Counterfactual: what would Iranian strategy be under deterrence posture alone without sanctions?',
    'If sanctions are primary extraction mechanism: deterrence is economic Snare disguised as military coordination. If kinetic threat dominates: deterrence better classified as Tangled Rope with genuine security coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctions_extraction_coupling, empirical, 'Coupling of sanctions extraction to military deterrence').

omega_variable(
    alternative_deterrence_architecture,
    'Would defensive postures (air defense, missile defense, naval screening) provide equivalent deterrence at lower extraction cost and theater than offensive power projection?',
    'Military analysis comparing defensive vs. offensive deterrence architectures; cost-benefit analysis of air defense investment vs. forward-deployed aircraft; assessment of whether defensive posture is politically/strategically sufficient',
    'If defensive posture is sufficient: current posture is over-extracted extraction disguised as necessary deterrence (Snare). If offensive presence is genuinely required: current posture is legitimate Rope/Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_deterrence_architecture, empirical, 'Whether defensive deterrence could substitute for offensive posture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regional_military_deterrence_mideast, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rmd_tr_t0, regional_military_deterrence_mideast, theater_ratio, 0, 0.45).
narrative_ontology:measurement(rmd_tr_t15, regional_military_deterrence_mideast, theater_ratio, 15, 0.55).
narrative_ontology:measurement(rmd_tr_t30, regional_military_deterrence_mideast, theater_ratio, 30, 0.64).

% Extraction over time
narrative_ontology:measurement(rmd_be_t0, regional_military_deterrence_mideast, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rmd_be_t15, regional_military_deterrence_mideast, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(rmd_be_t30, regional_military_deterrence_mideast, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regional_military_deterrence_mideast, enforcement_mechanism).
narrative_ontology:affects_constraint(regional_military_deterrence_mideast, iranian_nuclear_proliferation_pathway).
narrative_ontology:affects_constraint(regional_military_deterrence_mideast, gulf_arab_monarchies_us_alignment).
narrative_ontology:affects_constraint(regional_military_deterrence_mideast, houthi_proxy_escalation_logic).
narrative_ontology:affects_constraint(regional_military_deterrence_mideast, sanctions_regime_economic_extraction).

% DUAL FORMULATION NOTE:
% The deterrence posture is downstream of Iranian strategic decisions (nuclear development, proxy support) and upstream of specific conflict outcomes (strait of hormuz closure, proxy attacks, kinetic escalation). The constraint operates at the strategic level (posture, force deployment) while specific tactical constraints (naval standoff, air superiority) operate at subordinate levels. The decomposition reflects different observables: the posture itself (this story) has ε≈0.58; specific proxy conflicts have higher ε; the nuclear proliferation pathway has different ε reflecting technical vs. strategic extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regional_military_deterrence_mideast, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
