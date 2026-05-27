% ============================================================================
% CONSTRAINT STORY: sound_money_scarcity_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sound_money_scarcity_constraint, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sound_money_scarcity_constraint
 *   human_readable: Sound Money Scarcity Constraint in Cryptocurrency Systems
 *   domain: political_economy/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   The sound-money scarcity constraint in cryptocurrency systems presents a
 *   structural ambiguity: is the fixed maximum supply (e.g., 21 million
 *   bitcoin) a monetary innovation solving the double-spending problem
 *   (coordination function), or an extraction mechanism that concentrates
 *   wealth to early adopters while suppressing participation for latecomers?
 *   The constraint exhibits high perspectival divergence — early adopters see
 *   it as sound-money coordination that solved a genuine problem; protocol
 *   developers see it as the technical solution to trust; late adopters see
 *   it as a wealth trap; the analytical observer risks naturalizing it as an
 *   immutable law of all monetary systems. The rising theater_ratio (0.45 →
 *   0.68) reflects increasing institutional ritualization: as cryptocurrency
 *   matures, discussions of 'sound money' and 'immutable scarcity' become
 *   increasingly performative — invoked as legitimacy claim rather than as
 *   functional description of actual monetary properties. The scarcity
 *   constraint functions differently across communities: in low-inflation
 *   jurisdictions, it coordinates against debasement risk; in high-inflation
 *   regimes, it functions as pure speculative asset. This suggests
 *   decomposition into separate constraint stories may be warranted, but the
 *   shared structural mechanism (fixed-supply ledger) justifies treatment as
 *   a single constraint evaluated from multiple institutional and geographic
 *   perspectives.
 *
 * KEY AGENTS:
 *   - Early Adopters / Miners (institutional/arbitrage): Primary beneficiaries — capture wealth concentration during supply accumulation phase; operate mining infrastructure with high barriers to entry.
 *   - Protocol Developers (institutional/arbitrage): Secondary beneficiaries — design the scarcity constraint; retain soft power through governance participation; benefit from ecosystem growth.
 *   - Late Participants (powerless/trapped): Primary victims — enter after lock-in; face fixed supply as wealth barrier; cannot manufacture alternative sound-money systems.
 *   - Technical Participants (moderate/constrained): Mixed victims and beneficiaries — can verify scarcity and participate in governance, but constrained by technical barriers and mining concentration.
 *   - Monetary System Stability (powerless/trapped): Abstract victim — cannot exit; bears full cost of volatility from scarcity-driven price cycles.
 *   - Central Banks / Monetary Policy Authorities (institutional/arbitrage): Competitive constraint designers — respond via CBDC initiatives that preserve monetary policy flexibility while copying scarcity novelty.
 *   - Analytical Observer (analytical/analytical): Positions self as neutral classifier but risks naturalizing contingent institutional choice as universal law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sound_money_scarcity_constraint, 0.58).
domain_priors:suppression_score(sound_money_scarcity_constraint, 0.62).
domain_priors:theater_ratio(sound_money_scarcity_constraint, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sound_money_scarcity_constraint, extractiveness, 0.58).
narrative_ontology:constraint_metric(sound_money_scarcity_constraint, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sound_money_scarcity_constraint, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sound_money_scarcity_constraint, tangled_rope).
narrative_ontology:human_readable(sound_money_scarcity_constraint, "Sound Money Scarcity Constraint in Cryptocurrency Systems").
narrative_ontology:topic_domain(sound_money_scarcity_constraint, "political_economy/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(sound_money_scarcity_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sound_money_scarcity_constraint, 'e638f6df-ab61-4d7e-9742-364c36302878').
narrative_ontology:cs_created_at('e638f6df-ab61-4d7e-9742-364c36302878', '').
narrative_ontology:cs_kernel_codification('e638f6df-ab61-4d7e-9742-364c36302878', formalized).
narrative_ontology:cs_authority_grounding('e638f6df-ab61-4d7e-9742-364c36302878', extraction).
narrative_ontology:cs_interpretation_layer_present('e638f6df-ab61-4d7e-9742-364c36302878').
narrative_ontology:cs_reading_relation('e638f6df-ab61-4d7e-9742-364c36302878', sound_money_speculative_asset_reading, coexists_with).
narrative_ontology:cs_reading_relation('e638f6df-ab61-4d7e-9742-364c36302878', decentralization_disintermediation_reading, coexists_with).
narrative_ontology:cs_axiom('e638f6df-ab61-4d7e-9742-364c36302878', foundational, fixed_supply_is_sound_money).
narrative_ontology:cs_axiom_status(fixed_supply_is_sound_money, holdable).
narrative_ontology:cs_axiom_grounding('e638f6df-ab61-4d7e-9742-364c36302878', fixed_supply_is_sound_money, deontological).
narrative_ontology:cs_axiom('e638f6df-ab61-4d7e-9742-364c36302878', foundational, immutable_supply_cap_prevents_debasement).
narrative_ontology:cs_axiom_status(immutable_supply_cap_prevents_debasement, holdable).
narrative_ontology:cs_axiom_grounding('e638f6df-ab61-4d7e-9742-364c36302878', immutable_supply_cap_prevents_debasement, empirically_contingent).
narrative_ontology:cs_reference_frame('e638f6df-ab61-4d7e-9742-364c36302878', sound_money_hard_currency_framework).
narrative_ontology:cs_drift_state('e638f6df-ab61-4d7e-9742-364c36302878', contemporary_cbdc_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sound_money_scarcity_constraint, early_adopters).
narrative_ontology:constraint_beneficiary(sound_money_scarcity_constraint, protocol_developers).
narrative_ontology:constraint_beneficiary(sound_money_scarcity_constraint, mining_infrastructure_operators).
narrative_ontology:constraint_victim(sound_money_scarcity_constraint, monetary_system_stability).
narrative_ontology:constraint_victim(sound_money_scarcity_constraint, late_adopters).
narrative_ontology:constraint_victim(sound_money_scarcity_constraint, non_technical_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE ADOPTER (SNARE) — Enters after protocol lock-in and institutional accumulation. Faces fixed supply and early-adopter premium as structural extraction. No exit without accepting permanent loss. Maximum suppression: cannot manufacture alternative sound-money systems due to coordination lock; cannot exit without switching to fiat (perceived as inferior) or returning to powerlessness in traditional finance. Experiences the scarcity constraint as pure extraction mechanism.
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TECHNICAL PARTICIPANT (TANGLED ROPE) — Can verify scarcity properties and participate in protocol governance but constrained by technical barriers and energy costs. Benefits from coordination: transparent, auditable money supply. Also bears extraction: mining rewards accrue disproportionately to early infrastructure operators; volatility creates uncertainty about wealth preservation. Mixed experience: genuine coordination function (immutable supply ledger) alongside asymmetric distribution of early-adopter rents.
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROTOCOL DEVELOPER / MINING OPERATION (ROPE) — Benefits from first-mover advantage. The scarcity constraint is a coordination mechanism: establishing a fixed supply solves the double-spending problem and creates verifiable scarcity. Arbitrage exit available: can fork, relocate, or diversify. Experiences the constraint as pure coordination with net benefit. The scarcity property IS the solution to the coordination problem they pose.
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LEGACY FINANCIAL SYSTEM (PITON) — Maintains its own scarcity constraints (reserve requirements, fractional banking limits) as institutional theater — rules that persist through regulatory inertia despite digital alternatives. The sound-money critique targets legacy scarcity as artificial; the legacy system responds by maintaining the performative ritual while tolerating the challenger. Low functional extraction from this position because the system can arbitrage (accept crypto deposits, offer custody) without fundamentally changing. Theater_ratio high because reserve enforcement is increasingly performative in digital era.
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CBDC INITIATIVE (SCAFFOLD) — Organized response to scarcity-constraint challenge: design digital money with programmable scarcity while retaining monetary policy flexibility. Views sound-money scarcity as a temporary constraint to be superseded by central-bank-controlled digital infrastructure. High suppression of the cryptocurrency alternative through regulatory barriers, but sees a sunset clause: CBDC maturation as the terminal attractor. Extraction is structured toward maintaining monetary policy authority, not toward early adopters.
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, scarcity itself is an inherent property of value: all store-of-value systems must solve the scarcity problem to function, whether through gold supply caps, fiat token creation limits, or cryptographic proof-of-work. The sound-money constraint appears as an immutable requirement of monetary systems. However, the structural data contradicts this: identifiable beneficiaries (early adopters, miners) and victims (late adopters, non-participants) reveal this as naturalization of a contingent institutional choice. False summit candidate.
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sound_money_scarcity_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sound_money_scarcity_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sound_money_scarcity_constraint, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sound_money_scarcity_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sound_money_scarcity_constraint, TR),
    TR >= 0.70.

:- end_tests(sound_money_scarcity_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The sound-money scarcity constraint produces measurable extraction through multiple mechanisms: (1) early-adopter wealth concentration due to diminishing supply and fixed maximum cap, (2) mining infrastructure concentration due to economies of scale in proof-of-work, (3) volatility-driven price cycles that punish non-technical participants. However, extraction is not total because the constraint also solves a genuine coordination problem (preventing double-spending without centralized authority). The base_extractiveness (0.58) reflects this mixed character: higher than a pure coordination mechanism would require, but lower than a pure extraction mechanism (snare) would show. The rising trajectory (0.32 → 0.58 over 10 periods) suggests extraction is accumulating as the system matures — early coordination benefits persist while late-adopter extraction costs compound. Suppression (0.62): High. Multiple suppression mechanisms: (1) technical knowledge barriers prevent non-programmers from verifying scarcity properties, (2) mining concentration creates high barriers to entry for independent verification, (3) volatility creates uncertainty about wealth preservation (suppresses non-speculative use), (4) regulatory barriers in some jurisdictions suppress alternative sound-money systems. However, suppression is not total because open-source code allows public inspection and some communities have achieved viable non-speculative use (El Salvador, some merchants). Theater ratio (0.68): High and rising. The rising trajectory reflects increasing performativity: discussions of 'sound money' and 'immutable scarcity' increasingly serve as legitimacy claims rather than descriptions of actual monetary function. Evidence: (1) prices driven primarily by sentiment and speculation, not by sound-money properties, (2) institutional adoption focuses on volatility trading, not monetary use, (3) 'hodl' culture emphasizes narrative commitment over functional money use, (4) protocol governance discussions treat immutability as ideological commitment rather than technical constraint (forks remain possible). The theater has increased as the ecosystem professionalized — performative aspects of scarcity commitment became more central to maintaining belief in the system.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The protocol developer/miner sees the scarcity constraint as a solution (Rope) — they designed it to solve the double-spending problem and capture early-adopter rents. The late adopter sees pure extraction (Snare) — they cannot exit without accepting permanent loss. The technical participant sees mixed extraction and coordination (Tangled Rope) — they benefit from the transparency and immutability but are constrained by infrastructure concentration. The central bank sees a temporary competitive challenge with a sunset clause (Scaffold) — CBDC maturation will supersede cryptocurrency scarcity with state-controlled digital currency. The legacy financial system sees a challenger ritual that it can arbitrage away (Piton) — cryptocurrency maintains its scarcity theater through institutional inertia and ideological commitment. The analytical observer risks seeing an immutable law (Mountain) — all monetary systems require scarcity — but the structural data contradicts this: alternative monetary systems (fiat, commodity-backed, reputation-based) solve the monetary problem through different constraints. The false-summit diagnosis: the scarcity constraint is naturalized as universal when it is actually a contingent technical choice that benefits specific early-adopter populations.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) tracks each agent's structural position relative to the extraction flow. Early adopters and miners occupy the beneficiary end (d ≈ 0.1-0.2): they benefit from the scarcity constraint, experience low effective extraction. Late adopters occupy the victim end (d ≈ 0.85-0.95): they bear concentrated extraction costs. Technical participants occupy the mixed position (d ≈ 0.55-0.65): benefits from coordination and governance participation offset extraction from mining concentration and volatility suppression. The directionality chain produces the perspectival gap: early adopters with arbitrage exit options see Rope; late adopters with trapped exit options see Snare; moderate participants with constrained exit see Tangled Rope. The institutional perspective for miners uses beneficiary status + arbitrage exit → d ≈ 0.15 → f(d) ≈ -0.01 → low chi, producing Rope. The powerless perspective for late adopters uses victim status + trapped exit → d ≈ 0.90 → f(d) ≈ 1.28 → high chi, producing Snare. The moderate perspective for technical participants uses mixed beneficiary/victim status + constrained exit → d ≈ 0.60 → f(d) ≈ 0.80 → moderate chi, producing Tangled Rope. No directionality overrides needed; structural derivation produces appropriate differentiation.
 *
 * MANDATROPHY ANALYSIS:
 *   The scarcity constraint resolves mandatrophy through perspectival decomposition: all six types are defensible readings of the same base properties from different observer positions. The mandatrophy dissolves when we recognize that 'Is this money or extraction?' is not a property of the constraint itself but of the agent's structural relationship to it. For early adopters it is money (solved coordination problem, enabled wealth accumulation). For late adopters it is extraction (fixed-supply wealth trap, no exit). For technical participants it is mixed (genuine coordination value, asymmetric capture). For the analytical observer, the question 'Is scarcity a natural law or contingent choice?' is what we are actually measuring — the false-summit detection triggers when beneficiaries and victims can be identified in a constraint claiming to be mountain. The mandatrophy is resolved by accepting that the constraint's type is observer-relative and that the perspectival gap itself is diagnostic of the extraction mechanism. The rising theater_ratio confirms that as the system matures, the legitimacy claim ('this is sound money') increasingly diverges from the functional reality (speculative asset with wealth-concentration properties), which is the signature of mandatrophy resolution through theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scarcity_definition_ambiguity,
    'Is the operative scarcity constraint the fixed maximum supply (21 million bitcoin), the current circulating supply distribution, or the immutability of the ledger?',
    'Compare extraction mechanisms across three measurement bases: (a) maximum-supply cap as constraint, (b) current distribution inequality as constraint, (c) transaction-verification immutability as constraint. Different bases produce different ε values and victim sets.',
    'If maximum-supply cap is primary: mountain or rope classification (immutable law or coordination). If distribution inequality is primary: snare or tangled_rope (extraction mechanism). If immutability is primary: rope or mountain (coordination function). Each framing produces different perspectives and different mandatrophy resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scarcity_definition_ambiguity, conceptual, 'Which scarcity property defines the operative constraint').

omega_variable(
    monetary_function_vs_asset_divergence,
    'Has the sound-money scarcity constraint functioned as a monetary system (unit of account, medium of exchange, store of value) or as a speculative asset class (volatility instrument, wealth concentration mechanism)?',
    'Time-series analysis of adoption metrics: percentage of transactions for daily-life purchases vs investment/speculation; volatility correlation with monetary policy events vs asset-class sentiment; merchant adoption rates vs exchange-platform volume.',
    'If functioning as money: perspectival gap between early-adopter rope and late-adopter snare is genuine monetary asymmetry. If functioning as asset: the snare classification is reinforced (extraction mechanism is volatility harvesting by infrastructure operators, not sound-money coordination). If diverging between communities (money in some jurisdictions, asset in others): write separate constraint stories per jurisdiction with different ε values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monetary_function_vs_asset_divergence, empirical, 'Whether the constraint functions as money or as speculative asset').

omega_variable(
    protocol_immutability_vs_fork_governance,
    'How immutable is the scarcity constraint in practice? Can protocol participants fork, redistribute, or upgrade the supply cap if consensus emerges?',
    'Historical analysis of fork events (Bitcoin Cash, Bitcoin Gold, Ethereum/Ethereum Classic). Measurement of consensus barriers to upgrade (percentage agreement required, technical fork feasibility, community coordination costs). Counterfactual: what would happen if 51% of miners coordinated to change supply cap?',
    'If immutable in practice: mountain or rope classification reinforced (natural law or unquestionable coordination). If supermajority changeable: tangled_rope or scaffold classification (contingent institutional arrangement, potentially sunset-able). If uncertain: omega uncertainty is itself the constraint (governance ambiguity as suppression mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protocol_immutability_vs_fork_governance, empirical, 'Whether scarcity cap is truly immutable or fork-alterable').

omega_variable(
    early_adopter_extraction_vs_risk_compensation,
    'Is early-adopter premium extraction or legitimate risk compensation for investing in unproven technology?',
    'Risk-adjusted return analysis: compare returns to early bitcoin adopters vs expected returns from equivalent early-stage technology investments. Control for survivorship bias. Distinguish returns from coordination payoff vs. extraction from late arrivals.',
    'If primarily extraction: snare and tangled_rope classifications confirmed. If primarily risk compensation: rope classification dominates (early adoption premium is incentive for solving coordination problem, not extractive rent). If mixed: the perspectival gap is justified and the constraint genuinely produces different utility curves for different cohorts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(early_adopter_extraction_vs_risk_compensation, empirical, 'Whether early-adopter premium is extraction or risk compensation').

omega_variable(
    false_summit_naturalization,
    'Is the sound-money scarcity constraint a genuine property of all monetary systems, or a specific contingent choice embedded in cryptocurrency protocols and naturalized as universal law?',
    'Comparative monetary system analysis: catalog how different monetary systems solve scarcity (fiat token creation, commodity backing, user-hours, reputation bonds). Identify which components are physically necessary vs institutionally contingent. Test whether alternatives to fixed-supply scarcity can coordinate equally well.',
    'If genuinely universal: mountain classification confirmed across all perspectives. If contingent: false summit detection triggered; reclassify to tangled_rope or snare depending on beneficiary/victim structure. The analytical observer perspective would shift from mountain to a lower type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether scarcity is universal monetary law or contingent protocol choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sound_money_scarcity_constraint, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smsc_tr_t0, sound_money_scarcity_constraint, theater_ratio, 0, 0.45).
narrative_ontology:measurement(smsc_tr_t3, sound_money_scarcity_constraint, theater_ratio, 3, 0.58).
narrative_ontology:measurement(smsc_tr_t6, sound_money_scarcity_constraint, theater_ratio, 6, 0.68).
narrative_ontology:measurement(smsc_tr_t9, sound_money_scarcity_constraint, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(smsc_be_t0, sound_money_scarcity_constraint, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(smsc_be_t3, sound_money_scarcity_constraint, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(smsc_be_t6, sound_money_scarcity_constraint, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(smsc_be_t9, sound_money_scarcity_constraint, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sound_money_scarcity_constraint, resource_allocation).
narrative_ontology:affects_constraint(sound_money_scarcity_constraint, proof_of_work_energy_extraction).
narrative_ontology:affects_constraint(sound_money_scarcity_constraint, decentralization_ideology_lock).
narrative_ontology:affects_constraint(sound_money_scarcity_constraint, monetary_policy_substitution).

% DUAL FORMULATION NOTE:
% The sound-money scarcity constraint is a parent constraint for multiple downstream constraints: (1) proof_of_work_energy_extraction — the energy cost of maintaining scarcity, (2) decentralization_ideology_lock — the cognitive capture that treats scarcity as fundamental to decentralization, (3) monetary_policy_substitution — the structural competition with state-backed CBDC alternatives. Each downstream constraint has its own ε value reflecting distinct extraction mechanisms. The parent constraint (scarcity itself as coordination mechanism) has moderate extractiveness; downstream constraints specify the concrete extraction pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
