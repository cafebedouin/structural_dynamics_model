% ============================================================================
% CONSTRAINT STORY: market_dominance_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_dominance_lock_in, []).

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
 *   constraint_id: market_dominance_lock_in
 *   human_readable: Market Dominance Lock-In: Network Effects and Switching Costs
 *   domain: economic/competition
 *
 * SUMMARY:
 *   Market dominance lock-in is a structural constraint that emerges when a
 *   firm gains market leadership through network effects, scale, or ecosystem
 *   breadth, then faces decreasing competitive pressure as switching costs
 *   and lock-in grow faster than the constraint that generated the dominance.
 *   The constraint exhibits all six DR types from different perspectives,
 *   making it an exemplar for how coordination mechanisms can be coupled with
 *   extraction mechanisms. From the locked-in user's perspective, it is a
 *   Snare (pure extraction with minimal exit). From the dominant incumbent's
 *   perspective, it is Rope (pure coordination). From the ecosystem
 *   developer's perspective, it is Tangled Rope (genuine coordination coupled
 *   with unilateral control). From regulators and interoperability advocates,
 *   it is Scaffold (a temporary problem with a regulatory sunset clause).
 *   From legacy antitrust frameworks, it is Piton (performative enforcement
 *   of a market structure that has degraded). The constraint's extractiveness
 *   (0.58) and suppression (0.62) reflect moderate-to-high barriers to
 *   competitive entry and user exit. The theater ratio (0.48) is lower than
 *   in regulatory capture scenarios because network effects and switching
 *   costs have a genuine structural basis, not just performative cover — but
 *   regulatory theater (merger review, market definition disputes) adds
 *   performative content without addressing lock-in mechanisms. The
 *   trajectory shows extractiveness increasing over time as dominance
 *   consolidates and interoperability barriers harden, but theater_ratio
 *   remaining relatively stable because the underlying network effect is
 *   genuine, not theatrical.
 *
 * KEY AGENTS:
 *   - Dominant Incumbent Firm: Primary beneficiary (institutional/arbitrage) — captures value from network scale, can set terms unilaterally, has full exit optionality
 *   - Locked-In End Users: Primary victim (powerless/trapped) — cannot exit without prohibitive switching costs, face price increases and reduced choice
 *   - Competing Entrants: Secondary victim (powerless/constrained) — face asymmetric barriers, cannot match ecosystem breadth, carry customer acquisition costs
 *   - Ecosystem Developers: Secondary victim (organized/constrained) — depend on platform for distribution but subject to unilateral policy changes and revenue extraction
 *   - Interoperability Coalition: Organized actors (organized/mobile) — regulators and standards bodies building alternative exit routes via data portability and API mandates
 *   - Legacy Antitrust Framework: Institutional actor (institutional/arbitrage) — maintains performative merger review and market definition disputes; enforcement lags market evolution
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable network laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_dominance_lock_in, 0.58).
domain_priors:suppression_score(market_dominance_lock_in, 0.62).
domain_priors:theater_ratio(market_dominance_lock_in, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_dominance_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(market_dominance_lock_in, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(market_dominance_lock_in, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_dominance_lock_in, tangled_rope).
narrative_ontology:human_readable(market_dominance_lock_in, "Market Dominance Lock-In: Network Effects and Switching Costs").
narrative_ontology:topic_domain(market_dominance_lock_in, "economic/competition").

domain_priors:requires_active_enforcement(market_dominance_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_dominance_lock_in, dominant_incumbent_firm).
narrative_ontology:constraint_victim(market_dominance_lock_in, competing_entrants).
narrative_ontology:constraint_victim(market_dominance_lock_in, end_users).
narrative_ontology:constraint_victim(market_dominance_lock_in, ecosystem_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN END USER (SNARE) — Cannot exit without prohibitive switching costs (data migration, workflow retraining, ecosystem compatibility loss). Trapped by network effects and sunk investments. Bears full extraction burden: higher prices, slower innovation, reduced feature choice. Maximum perceived extraction from complete structural lock-in.
constraint_indexing:constraint_classification(market_dominance_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING ENTRANT (SNARE) — Faces asymmetric barriers: must overcome network effects that benefit the incumbent, cannot match ecosystem breadth, and bears customer acquisition costs the incumbent does not. Even with superior technology, switching cost burden falls disproportionately on users who would defect, reducing addressable market. High extraction via barrier-to-entry structure.
constraint_indexing:constraint_classification(market_dominance_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ECOSYSTEM DEVELOPER (TANGLED ROPE) — Depends on the dominant platform for distribution and network access (genuine coordination function: reaches billions of users). But also subject to unilateral policy changes, API throttling, revenue share extraction, and threat of direct competition from the platform itself. Mixed: real benefit from coordination, real cost from extraction.
constraint_indexing:constraint_classification(market_dominance_lock_in, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DOMINANT INCUMBENT (ROPE) — Perceives lock-in as pure coordination: their network effects benefit everyone (more users = more value). Exercises pricing power and policy control as legitimate coordination authority. Can arbitrage out (switch business models, absorb competitors, pivot to new markets). Extraction runs toward this agent; they experience net subsidy.
constraint_indexing:constraint_classification(market_dominance_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTEROPERABILITY COALITION (SCAFFOLD) — Regulatory mandates (EU DMA, digital markets acts, interoperability requirements) are creating forced decoupling pathways: APIs, data portability, plugin ecosystems. Lock-in has a sunset clause — regulation is actively engineering exit routes, increasing user mobility. High suppression during enforcement, but declining as exit options multiply. Sunset: 5-10 years for interoperability standards to mature.
constraint_indexing:constraint_classification(market_dominance_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY MARKET STRUCTURE (PITON) — Traditional antitrust frameworks assume perfect competition and rational price-taking agents. But network effects and switching costs have made the competitive model largely performative for digital markets. Antitrust theater (merger review, market definition disputes, remedies that don't address lock-in) persists while the market structure it was designed for has degraded. Agency capacity to enforce competition has atrophied relative to market complexity.
constraint_indexing:constraint_classification(market_dominance_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — Risk perspective that network effects are an immutable law of economics: positive feedback, winner-take-all outcomes, and lock-in are inherent to any technology with network value. This naturalizes what is actually a contingent institutional arrangement (weak interoperability requirements, high switching costs by design, regulatory forbearance). The engine will flag this as a false summit — the 'naturalness' of lock-in depends entirely on policy choices about data portability, API access, and platform bundling.
constraint_indexing:constraint_classification(market_dominance_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_dominance_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(market_dominance_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(market_dominance_lock_in, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_dominance_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(market_dominance_lock_in, TR),
    TR >= 0.70.

:- end_tests(market_dominance_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The dominant incumbent captures value through multiple mechanisms: pricing power to locked-in users, revenue share terms with ecosystem developers, and reduced competitive pressure from barriers to entry. However, the extraction is not as severe as pure monopoly pricing (ε would approach 0.85+) because network effects generate legitimate value — more users do mean more value to all users. The moderate level reflects that extraction is coupled with real coordination benefit. The trajectory shows increasing extractiveness over the 10-year interval (0.35 → 0.58) as dominance compounds and switching costs harden. Suppression (0.62): Moderate-high. Barriers to exit include data lock-in (user data cannot migrate), API dependency (developers cannot reach users without the platform), ecosystem lock-in (complementary services only exist on the dominant platform), and psychological switching costs. But suppression is not total (0.80+) because some users do switch, some developers build alternatives, and regulatory mandates are beginning to reduce barriers. Theater ratio (0.48): Moderate. Antitrust enforcement and merger review processes contribute performative content (market definition disputes, remedy packages that don't address lock-in), but the underlying network effects and switching costs have genuine structural bases. Theater is not the primary mechanism — extraction derives from real coordination coupling and real barriers. Theater has increased slightly over the interval as regulatory theater has intensified without matching market structure changes.
 *
 * PERSPECTIVAL GAP:
 *   Market dominance lock-in generates a maximal perspectival gap across all six DR types. The dominant incumbent perceives pure Rope: they frame network effects as a coordination good that benefits all participants equally, and experience pricing power as a legitimate exercise of coordinating authority. The locked-in user perceives pure Snare: they see extraction with minimal coordination benefit and no exit path. The competing entrant perceives Snare with higher barriers: they cannot match ecosystem breadth and carry disproportionate customer acquisition costs. The ecosystem developer perceives Tangled Rope: genuine platform access (coordination) coupled with unilateral terms and threat of direct competition (extraction). The interoperability coalition perceives Scaffold: regulatory mandates are actively creating exit routes (data portability, API access) with a sunset (5-10 years for standards to mature). The legacy antitrust framework perceives Piton: merger review and market definition disputes are performative rituals that don't address the underlying lock-in mechanism; the framework has become largely theatrical while markets have evolved past its assumptions. The civilizational analytical observer risks perceiving Mountain (network effects are immutable laws of economics), but the structural data shows this is false naturalization — the contingency of lock-in depends entirely on interoperability policy, not on physics. The perspectival gap is so wide that no single observer can claim to see the 'real' constraint — the constraint's structure is fully revealed only by comparing all perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from power level, exit options, and structural relationship to extraction flow. The locked-in user has powerless status + trapped exit options → high d (1.0, maximum target). The competing entrant has powerless status + constrained exit (can attempt to build, but barriers are high) → high d (0.90). The ecosystem developer has organized status + constrained exit (depends on platform, cannot easily leave, but has some negotiating power) → moderate-high d (0.60). The dominant incumbent has institutional status + arbitrage exit (can switch business models, absorb competitors, pivot markets) → very low d (0.10, near-full beneficiary). The interoperability coalition has organized status + mobile exit (regulatory mandates give them agency to shape terms) → moderate d (0.45). This produces a perspectival gap: the incumbent sees net subsidy (low effective extraction toward them), locked-in users see maximum extraction (high chi), competing entrants see asymmetric barriers (high chi), and the coalition sees a solvable problem with sunset (moderate chi).
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: Market dominance lock-in resolves the mandatrophy through perspectival multiplicity and regulative intervention. The false summit (mountain perspective) is revealed as naturalization of contingent policy: lock-in feels immutable only because regulators have not yet enforced interoperability. The scaffold is real but nascent: data portability mandates (GDPR, EU DMA) are building exit routes, but maturation requires 5-10 years. The snare is the powerless agent's structural reality while interoperability is incomplete. The rope is the incumbent's genuine experience of network coordination (more users do create value for all). The tangled rope is the ecosystem developer's mixed experience. The piton is the institutional lag: antitrust theater persists despite its actual powerlessness over lock-in mechanisms. The mandatrophy is resolved by recognizing that all six types are legitimate perspectival readings of the same constraint at different phases of regulatory evolution — the constraint is a Snare under the current (low-interoperability) regime, a Tangled Rope under partial interoperability, and a Rope under full interoperability. The 'correct' classification depends on which policy baseline you assume, not on objective facts about network effects. This shows why mandatrophy cannot be resolved by appeal to 'what the constraint really is' — the constraint's nature is determined by the regulatory and architectural choices that structure switching costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_measurement,
    'Are measured switching costs structural (actual technical barriers) or behavioral (perceived/psychological lock-in that would disappear with better information)?',
    'Controlled switching experiments: offer incentives to switch and measure actual migration rates vs. stated switching cost estimates. Compare user retention after new competing options emerge with low marketing budgets.',
    'If structural: lock-in is a genuine network effect constraint (higher ε). If behavioral: users have mobile exit options that are merely invisible (lower d from market perspective, higher d from individual perspective). Classification may shift from Snare to Tangled Rope depending on behavioral component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_measurement, empirical, 'Whether switching costs are structural or behavioral/psychological').

omega_variable(
    network_effect_versus_lock_in,
    'What proportion of the incumbent''s market dominance derives from genuine network effects (value increases with scale) versus switching cost barriers (value cannot be accessed by switching)?',
    'Counterfactual analysis: simulate reduced switching costs (interoperability, data portability) while holding network effects constant. Measure resulting market share churn. Compare to empirical cases where interoperability increased (VoIP, mobile carriers with number portability).',
    'If network effects dominate: lock-in is a coordination mechanism, beneficiaries are all network participants (Rope from more perspectives). If switching costs dominate: lock-in is pure extraction mechanism (Snare is correct classification). If mixed: Tangled Rope is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effect_versus_lock_in, empirical, 'Proportion of dominance from network effects vs switching cost barriers').

omega_variable(
    interoperability_ceiling,
    'Can forced interoperability (data portability, API access, plugin ecosystems) reduce lock-in without destroying the network effects that generate legitimate coordination value?',
    'Historical study of interoperability mandates: GDPR data portability impact, EU DMA compliance timelines, cellular number portability outcomes. Measure whether open standards preserve network value while reducing switching costs.',
    'If interoperability works: scaffold sunset is real, extraction can be decoupled from coordination (lock-in becomes surmountable). If interoperability fails: network effects prove to be tightly coupled with switching costs (lock-in is inherent to network coordination, not contingent on architecture). Classification implications: transition from Snare/Tangled Rope to Rope/Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_ceiling, empirical, 'Whether forced interoperability can preserve network value while reducing lock-in').

omega_variable(
    regulatory_enforcement_capacity,
    'Do antitrust agencies have sufficient technical capacity and legal authority to enforce interoperability in real time, or is antitrust theater (merger review, pricing investigations) mismatched to the actual speed of digital market evolution?',
    'Comparative analysis of EU DMA vs US antitrust enforcement on actual lock-in reduction. Timeline comparison: how long between violation and remedy? Do remedies precede or follow market shifts?',
    'If agencies have capacity: scaffold perspective is real (regulation reduces lock-in over time). If agencies lag: piton perspective is real (enforcement is performative, market structure persists despite formal intervention). This determines whether the regulatory sunset clause has teeth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_capacity, empirical, 'Whether antitrust enforcement capacity matches digital market evolution speed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_dominance_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mdli_tr_t0, market_dominance_lock_in, theater_ratio, 0, 0.32).
narrative_ontology:measurement(mdli_tr_t5, market_dominance_lock_in, theater_ratio, 5, 0.4).
narrative_ontology:measurement(mdli_tr_t10, market_dominance_lock_in, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(mdli_be_t0, market_dominance_lock_in, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mdli_be_t5, market_dominance_lock_in, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(mdli_be_t10, market_dominance_lock_in, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_dominance_lock_in, resource_allocation).
narrative_ontology:affects_constraint(market_dominance_lock_in, platform_ecosystem_dependency).
narrative_ontology:affects_constraint(market_dominance_lock_in, data_portability_barriers).
narrative_ontology:affects_constraint(market_dominance_lock_in, network_effect_winner_take_all).

% DUAL FORMULATION NOTE:
% Market dominance lock-in is upstream of specific platform constraints (social media network effects, cloud infrastructure lock-in, mobile ecosystem gatekeeping). This story models the general mechanism; downstream stories model domain-specific instantiations with different ε values and empirical baselines.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
