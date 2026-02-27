% ============================================================================
% CONSTRAINT STORY: us_taiwan_arms_sales
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_taiwan_arms_sales, []).

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
 *   constraint_id: us_taiwan_arms_sales
 *   human_readable: US Arms Sales Policy toward Taiwan
 *   domain: geopolitical/military_trade
 *
 * SUMMARY:
 *   US arms sales to Taiwan under the Taiwan Relations Act (TRA, 1979)
 *   represent a structural constraint operating at the intersection of
 *   military deterrence, vendor dependency, alliance management, and
 *   geopolitical competition. The constraint exhibits characteristics of all
 *   six DR types from different vantage points. Taiwan experiences the policy
 *   as dependency (Snare) — it is trapped in vendor lock-in with the US as
 *   monopoly supplier of advanced defensive systems, with no credible exit
 *   option absent unacceptable security risk. The US defense industrial base
 *   experiences it as coordination (Rope) — arms sales enable recurring
 *   revenue, supply chain justification, and partnership signaling. The
 *   Taiwan Relations Act itself is largely inert (Piton) — the legislative
 *   framework persists through path dependency while real deterrence operates
 *   through implicit strategic ambiguity, operational doctrine, and
 *   intelligence integration. The broader US foreign policy establishment
 *   experiences the policy as mixed coordination-extraction (Tangled Rope) —
 *   it provides genuine deterrence signaling and alliance reassurance but
 *   also constrains diplomatic flexibility and entangles the US in Taiwan
 *   Strait contingencies. An emerging coalition of allies (Japan, Australia,
 *   South Korea) and defense contractors views arms sales as temporary
 *   scaffolding toward integrated deterrence (Scaffold) — as doctrine aligns
 *   and production partnerships deepen, bilateral Taiwan dependency is
 *   projected to decline. From the PRC perspective, the constraint is
 *   extractive (Snare) — it constrains military options and diplomatic
 *   flexibility. The analytical observer at civilizational scope sees the
 *   genuine hybrid: authentic coordination function (deterrence signaling,
 *   norms reinforcement) combined with authentic extraction (vendor lock-in,
 *   arms race dynamics, strategic entanglement). The constraint's
 *   extractiveness has increased from 0.32 (1979-2000, when arms sales were
 *   modest) to 0.58 (2020s, as systems become more advanced and PRC military
 *   modernization accelerates), reflecting both growing PRC military pressure
 *   and deepening Taiwan dependence. Theater ratio has similarly increased
 *   from 0.48 to 0.65 as the TRA framework has become more performative —
 *   annual statements about 'defensive arms' mask actual deterrence decisions
 *   made through operational deployment, intelligence integration, and
 *   implicit strategic commitment rather than through the formal TRA
 *   mechanism.
 *
 * KEY AGENTS:
 *   - Taiwan: Primary victim (powerless/trapped) — structurally dependent on US arms for existential security; no credible exit option
 *   - US Defense Industrial Base: Primary beneficiary (institutional/arbitrage) — captures recurring revenue ($1.5-2.5B annually), maintains production capacity, justifies contractor investment
 *   - US Foreign Policy Establishment: Secondary beneficiary (organized/constrained) — benefits from deterrence signaling and alliance reassurance but constrained by PRC countermeasures and diplomatic rigidity
 *   - PRC / China: Secondary victim (powerful/constrained) — powerful resources but constrained exit from Taiwan territorial claim; military and diplomatic options limited by arms sales to Taiwan
 *   - Cross-Strait Regional Stability: Victim (moderate/constrained) — benefits from deterrence but bears escalation risk and arms race dynamics
 *   - Integrated Deterrence Coalition: Emerging organized actor (organized/mobile) — Japan, Australia, South Korea, allied defense contractors building alternative pathways toward shared deterrence integration
 *   - Taiwan Relations Act Framework: Institutional structure (institutional/arbitrage) — legal mechanism now substantially inert; real deterrence operates through implicit commitment and operational doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_taiwan_arms_sales, 0.58).
domain_priors:suppression_score(us_taiwan_arms_sales, 0.72).
domain_priors:theater_ratio(us_taiwan_arms_sales, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_taiwan_arms_sales, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_taiwan_arms_sales, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_taiwan_arms_sales, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_taiwan_arms_sales, tangled_rope).
narrative_ontology:human_readable(us_taiwan_arms_sales, "US Arms Sales Policy toward Taiwan").
narrative_ontology:topic_domain(us_taiwan_arms_sales, "geopolitical/military_trade").

domain_priors:requires_active_enforcement(us_taiwan_arms_sales).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_taiwan_arms_sales, us_defense_industrial_base).
narrative_ontology:constraint_beneficiary(us_taiwan_arms_sales, taiwan_military_capacity).
narrative_ontology:constraint_beneficiary(us_taiwan_arms_sales, us_regional_deterrence).
narrative_ontology:constraint_victim(us_taiwan_arms_sales, us_china_relations).
narrative_ontology:constraint_victim(us_taiwan_arms_sales, cross_strait_stability).
narrative_ontology:constraint_victim(us_taiwan_arms_sales, regional_escalation_risk).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAIWAN (SNARE) — Trapped in asymmetric military dependence on US arms for deterrence. Cannot exit without accepting existential vulnerability to PRC military pressure. Faces extraction through vendor lock-in (US supply monopoly), compatibility requirements, and political conditionality. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(us_taiwan_arms_sales, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CROSS-STRAIT STABILITY / REGIONAL ACTORS (TANGLED ROPE) — Benefits from arms sales as deterrent signaling (reduces PRC incentive for immediate military adventurism) but bears extraction through escalation risk, arms race dynamics, and reduced negotiating space. Constrained exit: regional states cannot unilaterally exit the security dilemma. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.61.
constraint_indexing:constraint_classification(us_taiwan_arms_sales, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US DEFENSE INDUSTRIAL BASE (ROPE) — Primary beneficiary. Taiwan arms sales represent recurring revenue stream ($1.5-2.5B annually), maintain production capacity, and justify contractor headcount. Experiences constraint as pure coordination: communicating deterrence signals enables follow-on sales, maintenance contracts, training programs. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary through institutional arbitrage.
constraint_indexing:constraint_classification(us_taiwan_arms_sales, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: US FOREIGN POLICY ESTABLISHMENT (TANGLED ROPE) — Experiences genuine coordination benefit (Taiwan arms sales demonstrate US commitment to rules-based order, support allied deterrence, maintain Five-Eyes intelligence access to Taiwan Strait). But also bears extraction through PRC countermeasures (salami-slicing territorial claims, military modernization, sanctions threats on US companies), reduced negotiating flexibility, and entrapment in Taiwan defense. d≈0.52, f(d)≈0.68, σ=1.2 → χ≈0.47.
constraint_indexing:constraint_classification(us_taiwan_arms_sales, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TAIWAN RELATIONS ACT (PITON) — Institutional legal framework (TRA §3(5): US provides Taiwan defensive arms) is largely inert. Its original function (explicit security commitment after 1979 derecognition) has atrophied — actual deterrence now relies on implicit strategic ambiguity and operational doctrine, not the TRA text. The TRA persists through legislative inertia and path dependency, not because it remains the primary mechanism. theater_ratio=0.65 reflects performative treaty citation (annual statements about 'defensive arms') masking real deterrence decisions made via operational deployment and intelligence integration. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(us_taiwan_arms_sales, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EMERGING INTEGRATED DETERRENCE COALITION (SCAFFOLD) — Allied governments (Japan, Australia, South Korea), private defense contractors, and US military commands increasingly see arms sales as a temporary scaffolding toward deeper integrated deterrence: doctrine alignment, joint exercises, intelligence sharing, production partnerships. This coalition experiences arms sales as coordination with a sunset: as integrated systems mature (drone swarms, AI-enabled sensing, distributed production), bilateral Taiwan dependency reduces. d≈0.42, f(d)≈0.42, σ=1.2 → χ≈0.21. Coalition has agency and sees exit path through integration.
constraint_indexing:constraint_classification(us_taiwan_arms_sales, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: PRC / CHINA'S REGIONAL INFLUENCE (SNARE FROM PRC PERSPECTIVE) — US arms sales to Taiwan constrains PRC's military and diplomatic options. PRC has powerful resources but constrained exit: cannot unilaterally exit the Taiwan issue without conceding core territorial claim. Experiences arms sales as extraction of strategic optionality. d≈0.78, f(d)≈1.12, σ=1.1 → χ≈0.73.
constraint_indexing:constraint_classification(us_taiwan_arms_sales, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the constraint is a hybrid coordination-extraction mechanism embedded in the post-WWII US-led order. Coordinating function: reassures Taiwan and allies, signals commitment to territorial integrity norms, reduces uncertainty. Extractive function: locks Taiwan into US vendor dependency, constrains PRC diplomatic options, embeds arms sales in recurring budget justifications. ε=0.58 reflects the genuine hybrid: not pure coordination (extraction is real), not pure extraction (coordination benefit is real). d≈0.70, f(d)≈1.08, σ=1.2 → χ≈0.73.
constraint_indexing:constraint_classification(us_taiwan_arms_sales, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_taiwan_arms_sales_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_taiwan_arms_sales, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_taiwan_arms_sales, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_taiwan_arms_sales, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_taiwan_arms_sales, TR),
    TR >= 0.70.

:- end_tests(us_taiwan_arms_sales_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Taiwan bears real extraction through vendor lock-in (monopoly supplier), compatibility/training requirements, and political conditionality (US domestic politics constrains supply predictability). The defense industrial base extracts recurring revenue and production justification. However, extraction is not maximal (0.70+) because the deterrence benefit to Taiwan is genuine and reduces existential risk — the constraint solves Taiwan's immediate security problem. Extraction is embedded in the solution, not separable from it. Suppression (0.72): High. Taiwan has severely constrained exit options: no credible alternative arms supplier (PRC unacceptable, EU restricted, Russia sanctioned, domestic production insufficient for advanced systems). The regional escalation risk also suppresses alternative arrangements — any perceived weakening of US commitment triggers PRC military probing. Suppression reflects structural barriers, not just policy coercion. Theater ratio (0.65): Moderate-high. The Taiwan Relations Act language ('defensive arms') is performative — the actual deterrence mechanism operates through implicit strategic ambiguity, operational doctrine (freedom of navigation operations, carrier deployments, intelligence integration), and alliance signaling, not through the explicit TRA text. Annual statements citing the TRA are theater; real policy decisions are made in National Security Council meetings and Pentagon force posture reviews. The theater has increased over time as the TRA text has become less directly connected to operational deterrence decisions.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival fragmentation. Taiwan sees Snare (trapped dependency with no exit). The US defense industrial base sees Rope (pure coordination benefit). The US foreign policy establishment sees Tangled Rope (mixed coordination-extraction). An emerging coalition sees Scaffold (temporary framework with sunset). The PRC sees Snare (constraint on its options). Cross-strait stability sees Tangled Rope (benefits from deterrence, bears escalation risk). The Taiwan Relations Act framework appears as Piton (institutional inertia). The analytical observer at global/civilizational scope sees genuine Tangled Rope (authentic hybrid with both coordination and extraction). No single classification is 'correct' — the presheaf of perspectives reveals that the constraint's structural nature is genuinely multi-form: it solves real security problems AND locks Taiwan into dependency AND escalates regional risk AND extracts defense rents AND operates partially as theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Taiwan: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximal extraction; Taiwan has no credible exit without accepting existential vulnerability. US Defense Industrial Base: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; can shift to other markets (Middle East, India) but benefits from recurring Taiwan sales. US Foreign Policy Establishment: Both benefits (deterrence commitment) and victim (PRC countermeasures, constrained diplomacy) + constrained → d≈0.52, f(d)≈0.68. Mixed. Cross-strait stability: Victim (escalation risk) + constrained → d≈0.68, f(d)≈1.05. Significant extraction but not maximal; regional deterrence benefit partially offsets risk. Integrated Deterrence Coalition: Organized + mobile → d≈0.42, f(d)≈0.42. Coalition sees exit path through integration; low effective extraction. Taiwan Relations Act: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification derives from theater gate, not directionality. PRC: Victim (constrained options) + constrained → d≈0.78, f(d)≈1.12. Powerful actor but constrained by Taiwan issue; significant extraction of strategic options. Analytical observer: analytical → d≈0.70, f(d)≈1.08. No false summit; the hybrid classification is verified by base metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint is legitimately Tangled Rope, not a false dichotomy. The mandatrophy is resolved by recognizing that the constraint genuinely solves a collective action problem (Taiwan deterrence, alliance signaling) while simultaneously extracting from Taiwan through dependency and from regional stability through escalation risk. The beneficiary (US defense industrial base) and the victim (Taiwan) are structurally distinct. No amount of reframing can make this into pure coordination (Rope) because Taiwan is locked in; no amount of reframing makes it pure extraction (Snare) because the deterrence function is real. The Taiwan Relations Act framework's piton status is distinct from the tangled_rope status of the overall arms sales policy — the legal framework is inert (theater ≥ 0.70), but the policy function is hybrid (ε=0.58, suppression=0.72, coordination present, extraction present). The scaffold perspective (emerging integrated deterrence coalition) represents a potential future state where technology integration and allied doctrine alignment reduce bilateral Taiwan dependency. The perspectival gap between Snare (Taiwan's view), Rope (defense contractor view), and Tangled Rope (analyst view) is not a measurement problem — it reflects real differences in structural position and exit options.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_ambiguity_collapse,
    'Does explicit US security commitment to Taiwan (replacing strategic ambiguity) resolve the constraint as Scaffold (temporary framework replaced by explicit alliance) or degrade it to pure Snare (Taiwan locked into direct US dependence)?',
    'Historical comparison with NATO expansion: did explicit commitments to Central/Eastern Europe reduce or increase extraction? Comparative analysis of Taiwan deterrence effectiveness under ambiguity vs explicit commitment.',
    'If Scaffold: US-Taiwan relations move toward explicit alliance, arms sales become coordination mechanism with sunset (integrated deterrence replaces vendor lock-in). If Snare: Taiwan''s dependency deepens, extraction intensifies, regional stability degrades.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_ambiguity_collapse, conceptual, 'Whether explicit security commitment resolves or deepens Taiwan dependency').

omega_variable(
    arms_race_stability_threshold,
    'At what rate of PRC military modernization do Taiwan arms sales cease to provide deterrence (stabilizing) and begin to trigger arms race (destabilizing) dynamics?',
    'Comparative defense analysis: modeling of Taiwan vs PRC force ratios; historical analysis of arms race tipping points in other dyads (Israel-Arab states, India-Pakistan); PRC military doctrine indicators of first-strike capability vs deterrent hedging.',
    'If threshold stable: arms sales remain coordination/deterrence function (Rope/Tangled Rope dominates). If threshold crossed: arms sales become extraction mechanism for escalation risk (Snare dominates).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arms_race_stability_threshold, empirical, 'Threshold rate of PRC modernization where arms sales trigger destabilizing arms race').

omega_variable(
    us_credibility_extraction_ratio,
    'Does US profit from arms sales (defense contractor rents) outweigh or undermine the strategic value of credible Taiwan commitment, and at what extraction ratio does the relationship invert?',
    'Cost-benefit analysis: annual arms sales revenue vs cost of maintaining military posture (fleet deployments, base infrastructure, training missions). Comparison with other US security commitments (NATO, Japan, South Korea) to establish baseline commitment cost.',
    'If credibility > extraction: arms sales are subordinate to strategic commitment (policy is Rope/Tangled Rope). If extraction > credibility: arms sales dominate policy (constraint is Snare for regional stability, profit-extraction for US).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_credibility_extraction_ratio, empirical, 'Ratio of credibility value to profit extraction in US Taiwan commitment').

omega_variable(
    domestic_taiwan_coalition_fragility,
    'Is Taiwan''s domestic consensus on US military dependence stable or fragmenting? Do younger generations, independence-focused parties, and tech entrepreneurs see arms sales as protective (Rope) or extractive (Snare)?',
    'Taiwan public opinion surveys on military confidence, polling on preference for US vs independent deterrence, analysis of DPP vs KMT vs TPP positions on arms sales; qualitative interviews with tech and civil society leaders.',
    'If stable consensus: arms sales constraint remains effective (coordination function holds). If fragmenting: Taiwan''s exit options increase (constrained → mobile), classification shifts from Snare toward Tangled Rope or Rope; constraint loses suppression force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domestic_taiwan_coalition_fragility, empirical, 'Stability of Taiwan domestic consensus on US arms dependence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_taiwan_arms_sales, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twarms_tr_t0, us_taiwan_arms_sales, theater_ratio, 0, 0.48).
narrative_ontology:measurement(twarms_tr_t20, us_taiwan_arms_sales, theater_ratio, 20, 0.58).
narrative_ontology:measurement(twarms_tr_t45, us_taiwan_arms_sales, theater_ratio, 45, 0.65).

% Extraction over time
narrative_ontology:measurement(twarms_be_t0, us_taiwan_arms_sales, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(twarms_be_t20, us_taiwan_arms_sales, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(twarms_be_t45, us_taiwan_arms_sales, base_extractiveness, 45, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_taiwan_arms_sales, enforcement_mechanism).
narrative_ontology:affects_constraint(us_taiwan_arms_sales, china_taiwan_military_balance).
narrative_ontology:affects_constraint(us_taiwan_arms_sales, us_china_strategic_competition).
narrative_ontology:affects_constraint(us_taiwan_arms_sales, first_island_chain_security).
narrative_ontology:affects_constraint(us_taiwan_arms_sales, semiconductor_supply_chain_dependence).

% DUAL FORMULATION NOTE:
% US Taiwan arms sales decompose into multiple structurally distinct constraints: (1) the enforcement mechanism (deterrence signaling via arms transfers) — tangled rope due to mixed coordination and extraction; (2) the Taiwan vendor dependency (supply monopoly) — snare from Taiwan perspective; (3) the Taiwan Relations Act framework (legal formalism) — piton due to theater persistence. These stories are linked via network.affects_constraints because arms sales policy couples deterrence effectiveness (enforcement) with vendor lock-in (dependency) and legal theater (institutional inertia). A change in any one would affect the others. The integrated deterrence coalition's emergence represents a potential decomposition point: if allied production partnerships and doctrine integration mature, the vendor dependency (snare) could reduce, leaving a more pure coordination mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_taiwan_arms_sales, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
