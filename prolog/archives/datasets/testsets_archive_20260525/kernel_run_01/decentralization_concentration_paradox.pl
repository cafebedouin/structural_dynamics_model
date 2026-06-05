% ============================================================================
% CONSTRAINT STORY: decentralization_concentration_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decentralization_concentration_paradox, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: decentralization_concentration_paradox
 *   human_readable: Decentralization-Concentration Paradox in Cryptocurrency Systems
 *   domain: political_economy/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   The decentralization-concentration paradox in cryptocurrency systems
 *   expresses a deep structural tension between three competing legitimacy
 *   narratives: (1) sound money grounded in Austrian economics and fixed
 *   scarcity, (2) speculative asset subject to financialization and
 *   institutional trading dynamics, and (3) decentralization ideology rooted
 *   in cypherpunk disintermediation and governance autonomy. These narratives
 *   are not complementary — they are actively contradictory in their causal
 *   claims and beneficiary structures. Yet they coexist within the same
 *   technical systems (Bitcoin, Ethereum, etc.) because different stakeholder
 *   groups inhabit and enforce different readings. Early adopters and miners
 *   benefit from sound-money narrative while capturing concentration
 *   advantages. Institutional investors benefit from speculative-asset
 *   reading while avoiding decentralization governance requirements. Retail
 *   participants are drawn by decentralization ideology while bearing
 *   extraction costs from both other readings. The constraint exhibits high
 *   theater ratio (0.68) because governance mechanisms (voting dashboards,
 *   community forums, core development processes) performatively distribute
 *   power while real decisions concentrate in developer discretion and
 *   wealth-weighted networks. The extractiveness trajectory shows
 *   accumulation: as systems mature, concentration mechanisms harden (mining
 *   pool dominance, exchange gatekeeping, developer funding models), and the
 *   constraint shifts from coordination (rope) toward extraction
 *   (snare/tangled_rope). The suppression coefficient (0.62) reflects that
 *   exit is materially difficult (sunk costs in hardware/identity) and
 *   cognitively difficult (ideological commitment creates identity lock at
 *   the retail level). This story may decompose into three separate
 *   constraint stories per the ε-invariance principle if the three readings
 *   prove structurally incompatible; the omega variable documents this
 *   decomposition decision point.
 *
 * KEY AGENTS:
 *   - Early Adopters and Miners: Primary beneficiary (institutional/arbitrage) — capture network-appreciation gains and low-cost initial stake; benefit from decentralization narrative without bearing governance constraints
 *   - Institutional Investors: Secondary beneficiary (institutional/arbitrage) — leverage volatility for trading gain; use decentralization narrative to reduce regulatory scrutiny; access market liquidity across multiple venues
 *   - Platform Operators and Core Developers: Tertiary beneficiary (institutional/constrained) — maintain protocol control and governance discretion; identity fused with decentralization narrative; extract value through funding, consulting, exchange operations
 *   - Retail Speculators: Primary victim (powerless/trapped) — enter via decentralization ideology; bear full downside volatility; sunk in identity and economic cost; no meaningful exit
 *   - Governance Participants: Secondary victim (moderate/constrained) — attempt to exercise protocol governance; diluted by large-holder dominance; constrained by community standing costs
 *   - Monetary Decentralization Claimants: Tertiary victim (organized/identity_locked) — sustain original vision commitment despite concentration becoming visible; identity fused with cypherpunk ideology; exit would require self-concept dissolution
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent concentration as immutable network property
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decentralization_concentration_paradox, 0.58).
domain_priors:suppression_score(decentralization_concentration_paradox, 0.62).
domain_priors:theater_ratio(decentralization_concentration_paradox, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decentralization_concentration_paradox, extractiveness, 0.58).
narrative_ontology:constraint_metric(decentralization_concentration_paradox, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(decentralization_concentration_paradox, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decentralization_concentration_paradox, tangled_rope).
narrative_ontology:human_readable(decentralization_concentration_paradox, "Decentralization-Concentration Paradox in Cryptocurrency Systems").
narrative_ontology:topic_domain(decentralization_concentration_paradox, "political_economy/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(decentralization_concentration_paradox).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(decentralization_concentration_paradox, distributed).
narrative_ontology:cs_authority_grounding(decentralization_concentration_paradox, lineage).
narrative_ontology:cs_reading_relation(decentralization_concentration_paradox, sound_money_reading, coexists_with).
narrative_ontology:cs_reading_relation(decentralization_concentration_paradox, speculative_asset_reading, coexists_with).
narrative_ontology:cs_reading_relation(decentralization_concentration_paradox, decentralization_governance_reading, coexists_with).
narrative_ontology:cs_axiom(decentralization_concentration_paradox, foundational, fixed_scarcity_as_monetary_legitimacy).
narrative_ontology:cs_axiom_status(fixed_scarcity_as_monetary_legitimacy, holdable).
narrative_ontology:cs_axiom(decentralization_concentration_paradox, foundational, protocol_decentralization_implies_governance_decentralization).
narrative_ontology:cs_axiom_status(protocol_decentralization_implies_governance_decentralization, holdable).
narrative_ontology:cs_axiom(decentralization_concentration_paradox, secondary, market_volatility_compatible_with_monetary_function).
narrative_ontology:cs_axiom_status(market_volatility_compatible_with_monetary_function, holdable).
narrative_ontology:cs_reference_frame(decentralization_concentration_paradox, cypherpunk_monetary_revolution).
narrative_ontology:cs_drift_state(decentralization_concentration_paradox, contemporary_institutional_financialization, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decentralization_concentration_paradox, early_adopters_and_miners).
narrative_ontology:constraint_beneficiary(decentralization_concentration_paradox, institutional_investors).
narrative_ontology:constraint_beneficiary(decentralization_concentration_paradox, platform_operators).
narrative_ontology:constraint_victim(decentralization_concentration_paradox, retail_speculators).
narrative_ontology:constraint_victim(decentralization_concentration_paradox, monetary_decentralization_claimants).
narrative_ontology:constraint_victim(decentralization_concentration_paradox, network_governance_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL SPECULATOR (SNARE) — Enters the system drawn by decentralization ideology and sound-money narrative. Bears full downside volatility while early adopters and whales capture asymmetric gains. No meaningful exit: sunk costs, emotional commitment to ideology, and FOMO dynamics create suppression. Experiences maximum extraction with minimal coordination benefit — the narrative obscures the extractive mechanism.
constraint_indexing:constraint_classification(decentralization_concentration_paradox, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GOVERNANCE PARTICIPANT (TANGLED ROPE) — Community members attempting to exercise voting power in protocol decisions face genuine coordination function (multi-stakeholder governance) but also extract costs through dilution of influence. Large holders and core developers extract disproportionate control. Constrained exit: departure signals loss of governance voice and community standing.
constraint_indexing:constraint_classification(decentralization_concentration_paradox, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: EARLY ADOPTER / MINER (ROPE) — Benefits from network-effect appreciation and low-cost initial acquisition. Experiences the constraint as pure coordination: the decentralization narrative legitimizes the network, attracts users, drives adoption. Minimal extraction cost; maximum benefit. High exit liquidity across global markets; can arbitrage between custody models and derivative contracts.
constraint_indexing:constraint_classification(decentralization_concentration_paradox, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL INVESTOR (ROPE) — Benefits from volatility, regulatory arbitrage (purchasing in jurisdictions with light oversight), and network appreciation. Experiences the constraint as coordination mechanism: the decentralization narrative provides ideological cover for accumulation and reduces regulatory scrutiny. Perfect arbitrage liquidity; can exit at any time with market-liquidity advantage.
constraint_indexing:constraint_classification(decentralization_concentration_paradox, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PROTOCOL DEVELOPER / CORE TEAM (PITON) — Maintains the decentralization narrative as institutional identity despite actual architectural centralization (core development control, governance concentration). The constraint persists through theater: governance dashboards, token voting, and community forums performatively distribute power while real decisions concentrate in developer discretion. Theater ratio is high because the governance ritual is largely symbolic. This perspective sees the degradation of the original decentralization claim, but perpetuates it through institutional inertia.
constraint_indexing:constraint_classification(decentralization_concentration_paradox, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a universal/civilizational perspective, the decentralization-concentration paradox appears as an immutable property of network systems: any distributed ledger must have some locus of control (miners, validators, developers) to maintain consensus. Concentration at some level is inherent to decentralized architecture. However, this perspective obscures the contingent institutional arrangements (wealth inequality in stake, developer funding models, exchange gatekeeping) that amplify concentration beyond technical necessity. The false summit signal reveals that naturalizing this as a law of networks legitimizes extractive institutional design.
constraint_indexing:constraint_classification(decentralization_concentration_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: DECENTRALIST COALITION (TANGLED ROPE) — Organized actors (cypherpunks, libertarian economists, privacy advocates) benefit from the decentralization narrative (ideological legitimacy, constituency mobilization) and also bear costs of institutional concentration (original vision unrealized, governance capture). Constrained exit: abandoning the project means surrendering the political commitment. Benefits from network growth but also extracted from by the concentration dynamic. This perspective sustains the rope framing only by deferring recognition of concentration — a form of collective identity lock.
constraint_indexing:constraint_classification(decentralization_concentration_paradox, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decentralization_concentration_paradox_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(decentralization_concentration_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(decentralization_concentration_paradox, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(decentralization_concentration_paradox, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(decentralization_concentration_paradox, TR),
    TR >= 0.70.

:- end_tests(decentralization_concentration_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint systematically extracts from retail participants through multiple channels: (1) wealth concentration through early-adopter asymmetry (0-6 year window captures highest appreciation), (2) volatility extraction through institutional trading advantage, (3) governance dilution through stake-weighted voting that correlates with wealth, (4) narrative extraction where speculative asset dynamics are obscured by sound-money and decentralization framings. The value is not maximal (0.75+) because genuine coordination function exists at the protocol level and some participants (miners, institutional investors) achieve genuine arbitrage rather than pure extraction. Suppression (0.62): High. Multiple suppression mechanisms operate: (1) material costs of exit (sunk investment, hardware/custody requirements, network effects), (2) cognitive costs (ideological commitment to decentralization vision, identity fusion), (3) information asymmetry (retail participants lack technical knowledge to evaluate concentration), (4) regulatory barriers (unclear legal status prevents alternative platforms from competing), (5) FOMO dynamics (missing upside on volatile asset compounds loss-aversion). Theater ratio (0.68): High. Governance mechanisms are substantially performative: voting dashboards and community forums distribute appearance of influence while core development decisions remain concentrated; ecosystem legitimacy claims rest on decentralization narrative even as whale concentration becomes visible; technical decentralization (node distribution) obscures governance centralization (control concentration). The rising trajectory reflects increasing institutionalization: governance theater becomes more sophisticated as systems mature.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. Early adopters and institutional investors experience coordination (rope) — they benefit from the narrative attracting users. Retail speculators experience extraction (snare) — they bear volatility costs while the narrative obscures the mechanism. Governance participants experience mixed coordination-extraction (tangled_rope) — genuine protocol governance exists but is wealth-weighted and diluted. Core developers experience degraded ritual (piton) — they maintain decentralization narrative as institutional identity despite architectural concentration. The decentralist coalition experiences the deepest perspectival contradiction: they are organized (can articulate the vision) but constrained by their own ideological commitment (cannot exit without abandoning the shared identity). The civilizational analytical observer risks a mountain classification (immutable network properties) which the structural data reveals as false summit — concentration is contingent, not necessary. The breadth of perspectival gap indicates that the constraint is properly classified as tangled_rope only by being averaged across all perspectives; individual perspectives read the constraint as pure rope or snare, and the tangled_rope classification emerges from the hybrid effect across the full observer set.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status plus exit options. Early adopters with arbitrage exit experience low d (0.10-0.20) — they are beneficiaries with perfect exit liquidity, giving them -0.12 to 0.02 effective extraction. Retail speculators with trapped exit and victim status experience high d (0.95) — maximum target, giving them ~1.42 effective extraction via f(d). Governance participants with constrained exit and mixed beneficiary-victim status occupy mid-range d (0.55-0.65) — they benefit from protocol coordination but are extracted from via voting dilution. Institutional investors with arbitrage access similar to early adopters but at institutional power level carry derived d ~0.15-0.25 (beneficiary + arbitrage). The decentralist coalition at organized power with constrained exit and victim status (bearing ideological dissonance) derives d ~0.50 (symmetric cost-benefit at the organizational level, though high identity lock at individual level). Platform operators at institutional power with constrained exit derive lower d (~0.25-0.35) than they would naively expect because their 'exit' (abandoning the protocol they built) is blocked by identity fusion with the project — this approaches identity_locked status at the institutional level, reducing their effective beneficiary d.
 *
 * MANDATROPHY ANALYSIS:
 *   CONTESTED KERNEL CANDIDATE: The core mandatrophy tension is whether three narratives coexist in a single constraint or decompose into three separate constraints. Sound-money narrative, speculative-asset narrative, and decentralization-governance narrative each instantiate different constraint types when isolated: Sound money (rope — coordination of inflation resistance) + Speculation (snare — volatility extraction) + Decentralization (tangled_rope — genuine governance coordination with concentrated control). They are held together by a shared technical substrate (the ledger) and a shared beneficiary class (early adopters, institutional investors) that benefits from narrative coherence while bearing none of the costs of the contradictions. Retail participants and committed cypherpunks are extracted from precisely by having to maintain all three narratives simultaneously as if they are compatible. The mandatrophy resolves by recognizing that the Tangled Rope classification is the analytical-observer perspective on a kernel dispute, not a stable constraint classification. If decomposed: three stories (sound_money_narrative, speculative_asset_financialization, decentralization_governance_system) linked via network.affects_constraints. If unified: a single kernel story documenting three coexisting readings where beneficiaries enforce reading compatibility and victims internalize contradiction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decentralization_definition_ambiguity,
    'What constitutes decentralization: technical distribution of nodes, or distributed control of governance and value capture?',
    'Comparative analysis of systems with high technical distribution but low governance distribution (Bitcoin) vs systems with moderate distribution but high governance attempts (Ethereum post-merge). Measurement of concentration metrics: Nakamoto coefficient, validator/mining pool distribution, wealth distribution of governance power.',
    'If technical distribution is sufficient: bitcoin qualifies as decentralized; concentration of value capture is acceptable. If governance distribution is required: nearly all blockchain systems fail decentralization claims; classification reverts to snare or tangled_rope from all perspectives. If the constraint is a kernel, this defines the reading difference.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decentralization_definition_ambiguity, conceptual, 'Whether decentralization refers to technical node distribution or distributed governance control').

omega_variable(
    monetary_legitimacy_vs_extractive_instrument,
    'Can a system function simultaneously as sound money (scarcity, fixed supply, inflation resistance) AND as a speculative asset (volatility, trading venue, institutional financialization)?',
    'Time-series analysis of relationship between price volatility and real-economy adoption. Comparison of volatility profiles in currencies with sound-money legitimacy claims vs pure-trading instruments. Assessment of whether volatility is feature (allows institutional arbitrage) or bug (prevents monetary use).',
    'If compatible: both readings coexist; constraint is hybrid extraction + coordination. If incompatible: the sound-money reading forecloses or gets foreclosed by the speculative-asset reading; constraint is a kernel with irreconcilable readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monetary_legitimacy_vs_extractive_instrument, empirical, 'Compatibility of sound-money legitimacy with speculative financialization').

omega_variable(
    early_adopter_wealth_concentration_inevitability,
    'Is concentration of wealth and control among early adopters a necessary feature of any adoption curve, or a design flaw specific to blockchain systems?',
    'Comparative analysis with traditional technology adoption curves (internet, email, TCP/IP). Identification of which concentration mechanisms are intrinsic to adoption vs which are contingent (mining economics, stake-weighting, exchange gatekeeping, regulatory barriers to entry).',
    'If necessary: concentration is an immutable property of network emergence (mountain reading). If contingent: concentration results from institutional design choices (tangled_rope / snare reading). If partially both: constraint is properly classified as tangled_rope with irreducible tension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(early_adopter_wealth_concentration_inevitability, empirical, 'Whether early-adopter concentration is inevitable or contingent').

omega_variable(
    governance_theater_substitution,
    'Do token-based voting and governance dashboards provide meaningful distributed control, or do they create the appearance of participation while actual decisions concentrate in core developer discretion?',
    'Analysis of governance votes that conflicted with core developer preference. Measurement of voting participation rates and whale concentration in actual votes. Case studies of protocol changes (The DAO fork, Ethereum EIP-1559, Bitcoin block-size war) showing decision locus.',
    'If governance is meaningful: constraint is rope or tangled_rope with genuine coordination function. If governance is theater: constraint is piton (performative degradation) or snare (governance fiction obscuring extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(governance_theater_substitution, empirical, 'Whether token voting provides meaningful distributed control or is performative').

omega_variable(
    three_readings_same_kernel_or_three_constraints,
    'Are the sound-money reading, speculative-asset reading, and decentralization-ideology reading three interpretations of one stabilized commitment (Bitcoin as kernel), or three structurally distinct constraints that share a name?',
    'Hard coherence boundary testing: Can a single theoretical framework hold all three readings simultaneously without logical contradiction? Does attacking one reading require defending a core premise the others share? Do different jurisdictions/parties enforce different readings as normative commitments?',
    'If one kernel: this constraint is a contested reading with sibling readings specified in cs_structure. If three constraints: should decompose into separate stories (sound_money_narrative, speculative_asset_constraint, decentralization_governance_system) with network linkages. If ambiguous: emit as single story with this omega documenting the decomposition ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(three_readings_same_kernel_or_three_constraints, conceptual, 'Whether sound-money, speculative asset, and decentralization-ideology are readings of one kernel or three distinct constraints').

omega_variable(
    retail_identity_lock_mechanism,
    'To what extent is retail speculator retention driven by material (sunk-cost economic barriers to exit) vs identity-based mechanisms (ideological commitment to cypherpunk vision, self-concept as participant in monetary revolution)?',
    'Longitudinal survey of exiting participants: which cited economic vs ideological reasons? Analysis of forums and community sentiment: Do participants maintain commitment despite negative returns? Comparison of exit rates between ideologically-committed communities vs pure-trading accounts.',
    'If primarily material: exit_options should be trapped. If primarily identity-locked: perspective remains constrained/mobile structurally but classifies as rope (identity-locked returns rope at biographical horizon per immutability profile) — reveals cognitive capture mechanism. If mixed: proportional analysis informs directionality_override.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retail_identity_lock_mechanism, empirical, 'Retail retention driven by sunk costs vs ideological identity fusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decentralization_concentration_paradox, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(decon_tr_t0, decentralization_concentration_paradox, theater_ratio, 0, 0.35).
narrative_ontology:measurement(decon_tr_t3, decentralization_concentration_paradox, theater_ratio, 3, 0.5).
narrative_ontology:measurement(decon_tr_t6, decentralization_concentration_paradox, theater_ratio, 6, 0.63).
narrative_ontology:measurement(decon_tr_t10, decentralization_concentration_paradox, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(decon_be_t0, decentralization_concentration_paradox, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(decon_be_t3, decentralization_concentration_paradox, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(decon_be_t6, decentralization_concentration_paradox, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(decon_be_t10, decentralization_concentration_paradox, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decentralization_concentration_paradox, resource_allocation).
narrative_ontology:affects_constraint(decentralization_concentration_paradox, proof_of_work_energy_concentration).
narrative_ontology:affects_constraint(decentralization_concentration_paradox, exchange_gatekeeping_regulatory_arbitrage).
narrative_ontology:affects_constraint(decentralization_concentration_paradox, smart_contract_formal_verification).

% DUAL FORMULATION NOTE:
% This constraint may decompose into three separate constraint stories if the three narratives (sound-money, speculative-asset, decentralization-governance) prove structurally incompatible. Sound-money narrative as Rope story; speculative-asset as Snare story; decentralization-governance as Tangled Rope story. If unified, this story documents the kernel dispute and beneficiary-enforced narrative coherence. Network edges indicate upstream constraints (proof-of-work energy, exchange gatekeeping, formal verification) that feed into the concentration dynamic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decentralization_concentration_paradox, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
