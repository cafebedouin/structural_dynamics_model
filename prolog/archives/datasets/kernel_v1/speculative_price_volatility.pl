% ============================================================================
% CONSTRAINT STORY: speculative_price_volatility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speculative_price_volatility, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speculative_price_volatility
 *   human_readable: Speculative Price Volatility in Cryptocurrency Markets
 *   domain: monetary_economics/technology_governance
 *
 * SUMMARY:
 *   Speculative price volatility in cryptocurrency markets is a contested
 *   structural phenomenon that instantiates three mutually foreclosing
 *   readings of a shared label. The Austrian-economics sound-money frame
 *   reads volatility as a temporary coordination problem — as adoption
 *   increases and the currency matures, volatility should mechanically
 *   decline toward stability. The speculative-asset frame reads the same
 *   volatility as functional price discovery — volatility is not a problem
 *   but the mechanism by which information gets incorporated into price,
 *   essential to market efficiency. The decentralization-ideology frame reads
 *   volatility as evidence of decentralized operation — market participants,
 *   not central authorities, discover price, and the volatility is the
 *   signature of authentic freedom. These three readings cannot coexist
 *   within a single coherent authority framework. The sound-money reading
 *   depends on the assumption that cryptocurrency *should* become stable
 *   (like fiat money); the speculative frame depends on the assumption that
 *   volatility *is* the market working; the decentralization frame depends on
 *   the assumption that price instability *proves* the absence of
 *   authoritarian control. As Bitcoin matured from 2009 to 2024,
 *   extractiveness trended upward (0.35 → 0.62), contradicting the
 *   sound-money prediction of declining volatility. Theater ratio increased
 *   alongside (0.45 → 0.72), reflecting the growing mismatch between
 *   ideological claims of decentralization and the concentration of mining,
 *   exchange custody, and governance control that actually occurred. The
 *   suppression mechanism intensified (0.45 → 0.65), driven by regulatory
 *   complexity and the professionalization of trading infrastructure that
 *   elevated barriers to retail participation. This constraint is a
 *   gold-standard case for kernel decomposition: the three readings should
 *   probably be authored as three separate constraint stories linked via
 *   network_affects_constraints, because they have fundamentally different ε
 *   values and different beneficiary/victim structures when evaluated against
 *   their own internal metrics.
 *
 * KEY AGENTS:
 *   - Retail Participants: Primary victims (powerless/trapped) — bear asymmetric losses during volatility spikes; cannot exit without realizing losses; lack information and execution speed parity with institutional traders
 *   - High-Frequency Traders: Primary beneficiaries (institutional/arbitrage) — profit from volatility through speed and information advantages; experience the constraint as functional price discovery (rope classification); can arbitrage across venues and instruments
 *   - Early Adopters (Sound-Money Frame): Secondary beneficiaries with constrained exit (powerful/constrained) — hold Bitcoin as inflation hedge; benefit from adoption-phase appreciation; trapped by ideological commitment to hodl narrative; experience mixed coordination (inflation protection) and extraction (volatility losses)
 *   - Price-Stability Infrastructure: Mixed actor (moderate/constrained) — central banks, payment processors, stablecoin designers; requires volatility reduction to scale crypto adoption; implements coordination through centralized mechanisms (defeating stated decentralization goal); experiences tangled rope (genuine stability problem + extraction of control)
 *   - Decentralization-Ideology Coalition: Organized advocates (organized/mobile) — protocol developers, libertarian advocates; see volatility as temporary coordination problem with sunset (scaffold frame); benefit from ideological coherence but constrained by actual market concentration
 *   - Monetary Traditionalists: Vestigial institutional actors (institutional/arbitrage) — central banks maintaining 'crypto is unstable' critique; critique has atrophied into theater; can arbitrage regulatory arbitrage but lack enforcement power
 *   - Analytical Observer: Civilizational position (analytical/analytical) — reveals the kernel contest structure; sees three mutually foreclosing readings; the constraint IS the contested frame, not a unified structural phenomenon
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speculative_price_volatility, 0.58).
domain_priors:suppression_score(speculative_price_volatility, 0.62).
domain_priors:theater_ratio(speculative_price_volatility, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speculative_price_volatility, extractiveness, 0.58).
narrative_ontology:constraint_metric(speculative_price_volatility, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speculative_price_volatility, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speculative_price_volatility, tangled_rope).
narrative_ontology:human_readable(speculative_price_volatility, "Speculative Price Volatility in Cryptocurrency Markets").
narrative_ontology:topic_domain(speculative_price_volatility, "monetary_economics/technology_governance").

domain_priors:requires_active_enforcement(speculative_price_volatility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speculative_price_volatility, 'd3a43158-458e-45dc-a8e7-2a07b0ac7589').
narrative_ontology:cs_kernel_codification('d3a43158-458e-45dc-a8e7-2a07b0ac7589', distributed).
narrative_ontology:cs_authority_grounding('d3a43158-458e-45dc-a8e7-2a07b0ac7589', distributed).
narrative_ontology:cs_reading_relation('d3a43158-458e-45dc-a8e7-2a07b0ac7589', sound_money_volatility_hypothesis, forecloses).
narrative_ontology:cs_reading_relation('d3a43158-458e-45dc-a8e7-2a07b0ac7589', speculative_asset_volatility_function, coexists_with).
narrative_ontology:cs_reading_relation('d3a43158-458e-45dc-a8e7-2a07b0ac7589', decentralization_volatility_signature, coexists_with).
narrative_ontology:cs_axiom('d3a43158-458e-45dc-a8e7-2a07b0ac7589', foundational, volatility_temporary_convergence_to_stability).
narrative_ontology:cs_axiom_status(volatility_temporary_convergence_to_stability, holdable).
narrative_ontology:cs_axiom_grounding('d3a43158-458e-45dc-a8e7-2a07b0ac7589', volatility_temporary_convergence_to_stability, empirically_contingent).
narrative_ontology:cs_axiom('d3a43158-458e-45dc-a8e7-2a07b0ac7589', foundational, volatility_functional_price_discovery).
narrative_ontology:cs_axiom_status(volatility_functional_price_discovery, holdable).
narrative_ontology:cs_axiom_grounding('d3a43158-458e-45dc-a8e7-2a07b0ac7589', volatility_functional_price_discovery, instrumental).
narrative_ontology:cs_axiom('d3a43158-458e-45dc-a8e7-2a07b0ac7589', foundational, volatility_proof_of_decentralization).
narrative_ontology:cs_axiom_status(volatility_proof_of_decentralization, overridden).
narrative_ontology:cs_axiom_grounding('d3a43158-458e-45dc-a8e7-2a07b0ac7589', volatility_proof_of_decentralization, deontological).
narrative_ontology:cs_reference_frame('d3a43158-458e-45dc-a8e7-2a07b0ac7589', distributed_price_discovery_without_central_authority).
narrative_ontology:cs_drift_state('d3a43158-458e-45dc-a8e7-2a07b0ac7589', contemporary_institutional_integration, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d3a43158-458e-45dc-a8e7-2a07b0ac7589', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speculative_price_volatility, high_frequency_traders).
narrative_ontology:constraint_beneficiary(speculative_price_volatility, early_adopters).
narrative_ontology:constraint_beneficiary(speculative_price_volatility, exchange_operators).
narrative_ontology:constraint_victim(speculative_price_volatility, retail_participants).
narrative_ontology:constraint_victim(speculative_price_volatility, price_stability_infrastructure).
narrative_ontology:constraint_victim(speculative_price_volatility, sound_money_proponents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL PARTICIPANT (SNARE) — Trapped by asymmetric information and execution speed. Cannot exit without realizing losses during volatile swings. Extraction is maximal: volatility itself is the mechanism that transfers wealth from slow participants to fast ones. No coordination benefit; pure extraction with suppressed alternatives.
constraint_indexing:constraint_classification(speculative_price_volatility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HIGH-FREQUENCY TRADER (ROPE) — Experiences volatility as pure coordination mechanism: price discovery through adversarial bidding. The constraint enables their function — rapid repricing against available information. Arbitrage exits mean they can deploy capital across multiple venues and instruments. Net beneficiary experiencing the constraint as functional coordination.
constraint_indexing:constraint_classification(speculative_price_volatility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: EARLY ADOPTER / SOUND MONEY FRAME (TANGLED ROPE) — Powerful actor with constrained exit. Holds Bitcoin as inflation hedge; volatility is real cost (unreliable store of value) but also provides wealth gains during adoption phases. Coordination benefit exists (inflation-resistant currency function) alongside significant extraction (price swing losses). Requires active enforcement of community narrative ('hodl,' dismissing volatility as temporary).
constraint_indexing:constraint_classification(speculative_price_volatility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PRICE STABILITY INFRASTRUCTURE (TANGLED ROPE) — Moderate actor (central banks, payment processors, stablecoin designers) constrained by the crypto ecosystem's existence and growth. Crypto volatility creates coordination problem: cryptocurrency adoption depends on price stability, but native volatility undermines that stability. Active enforcement required: stablecoin contracts, circuit breakers, liquidity pools. Genuine coordination function (maintaining transactional reliability) alongside extraction (governance of volatility through centralized mechanisms).
constraint_indexing:constraint_classification(speculative_price_volatility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DECENTRALIZATION-IDEOLOGY COALITION (SCAFFOLD) — Organized agents (protocol developers, libertarian advocates, distributed ledger researchers) see volatility as a temporary coordination problem solved by protocol maturation and network effects. The scaffold has a sunset: as adoption increases and liquidity deepens, volatility should decrease mechanically. Beneficiaries from coordination function (decentralized money without state) with explicit exit assumption: mature protocol eliminates volatility through sheer scale.
constraint_indexing:constraint_classification(speculative_price_volatility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: MONETARY TRADITIONALIST (PITON) — Institutional actor maintaining critique of crypto as inherently unstable, but the critique has atrophied into theater. Central banks now hold Bitcoin, governments regulate exchanges, academic economics incorporates crypto models. The 'crypto is gambling' frame persists institutionally but has lost functional gatekeeping power. Maintained through inertia and institutional identity rather than effective exclusion.
constraint_indexing:constraint_classification(speculative_price_volatility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the analytical position, speculative price volatility emerges as a contest between three incommensurable readings of a shared kernel. Each reading produces a different classification because each defines what 'volatility' means structurally. Sound-money frame: volatility is coordination failure (mountain, eventually); speculative-asset frame: volatility is functional price discovery (rope); decentralization frame: volatility is temporary coordination lag (scaffold, eventually). The three readings cannot coexist in a single coherent framework — they foreclose each other.
constraint_indexing:constraint_classification(speculative_price_volatility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speculative_price_volatility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(speculative_price_volatility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(speculative_price_volatility, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(speculative_price_volatility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(speculative_price_volatility, TR),
    TR >= 0.70.

:- end_tests(speculative_price_volatility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint exhibits genuine extraction: high-frequency traders and early adopters benefit from information asymmetries and first-mover advantages that volatility creates. However, extraction is not maximal (snare level ≥0.66) because the mechanism requires ongoing participation from retail actors — complete extraction would collapse trading volume and destroy the extraction mechanism itself. The upward trend (0.35→0.62) reflects the professionalization of the space: as institutional capital entered, the sophistication of extraction mechanisms increased. Suppression (0.62): Moderate-high. Retail participants face real structural barriers: custody requirements, exchange access restrictions, regulatory complexity, information asymmetries, and execution speed gaps. These are not trivial costs. However, suppression is not total (snare requires ≥0.60): crypto remains accessible to motivated retail participants, regulatory capture is incomplete, and alternatives (peer-to-peer, decentralized exchanges, self-custody) exist at high but surmountable cost. Theater ratio (0.68): High. The ideological narratives surrounding cryptocurrency have increasingly diverged from structural reality: decentralization rhetoric persists despite observable mining and custody concentration; sound-money claims persist despite ongoing volatility; efficiency claims persist despite high transaction costs and slow settlement. Theater has increased over time (0.45→0.72) as the gap between promised decentralization and actual control concentration widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces maximum perspectival divergence across the observation site. The retail participant sees snare (extraction, no exit, no benefit). The high-frequency trader sees rope (pure coordination). The early adopter sees tangled rope (mixed benefit and cost). The infrastructure layer sees tangled rope (mixed coordination problem and extractive control). The decentralization coalition sees scaffold (temporary volatility with sunset). The monetary traditionalist sees piton (vestigial critique). The analytical observer sees the entire perspectival structure as a manifestation of kernel contest — the three readings foreclose each other, and no single classification is correct because no single authority framework grounds them all. The gap reveals that 'cryptocurrency' is not a unified constraint but a contested domain where three different commitment systems (Austrian economics, market-efficiency finance, libertarian anti-state ideology) claim the same label.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's d value (derived from beneficiary/victim status + exit options + power level) produces a different experienced extractiveness chi. High-frequency traders: beneficiaries with arbitrage exit → d ≈ 0.10 → f(d) ≈ -0.02 → negative chi (they experience the constraint as enabling, not extractive). Retail participants: victims with trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high chi (maximum extraction). Early adopters: beneficiaries with constrained exit → d ≈ 0.35 → f(d) ≈ 0.30 (mixed, hence tangled rope). Price-stability infrastructure: mixed beneficiary/victim with constrained exit → d ≈ 0.50 → f(d) ≈ 0.65 (moderate chi, mixed classification). Decentralization coalition: organized with mobile exit → d ≈ 0.45 → f(d) ≈ 0.50 (moderate chi, but sunset clause means low effective extraction over time). The analytical observer (d ≈ 0.72, f(d) ≈ 1.15) sees maximum structural tension because the three readings cannot be harmonized through directionality shifts alone — they require incompatible reference frames.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is UNRESOLVED because the three readings are genuinely incommensurable. The sound-money frame predicts decreasing extractiveness (declining volatility as adoption increases); the actual data shows increasing extractiveness. The speculative frame sees volatility as non-extractive (functional); the data on retail losses and information asymmetry contradicts this. The decentralization frame sees volatility as evidence of working decentralization; the data on mining and custody concentration contradicts this. The constraint resolves mandatrophy not by finding the 'right' type but by recognizing that the type IS the contested frame. Each reading is a coherent constraint story when evaluated against its own metrics, but the three stories cannot be unified without abandoning one of the foundational claims. This is not a classification failure — it is a structural feature of the domain: the label 'speculative price volatility' covers three distinct constraints with different authority structures, beneficiary/victim relationships, and terminal classifications.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    volatility_fundamental_or_contingent,
    'Is cryptocurrency price volatility a fundamental property of decentralized systems or a contingent feature of current market microstructure?',
    'Long-term empirical trend: if volatility declines monotonically with adoption and liquidity depth, it is contingent (supports scaffold frame). If volatility persists or accelerates despite adoption, it is fundamental (supports snare/piton frames).',
    'Contingent volatility: scaffold classification confirmed, sunset mechanism real, early-adopter tangled-rope is temporary. Fundamental volatility: snare classification dominates, extraction mechanism is structural, no sunset exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(volatility_fundamental_or_contingent, empirical, 'Whether volatility is fundamental or contingent on market scale').

omega_variable(
    sound_money_kernel_coherence,
    'Can cryptocurrency simultaneously function as an inflation hedge (sound-money frame) and a speculative asset (speculative frame) without these readings foreclosing each other?',
    'Historical analysis of price behavior during inflation episodes vs financial crises; correlation between crypto price and inflation expectations vs market risk appetite. If they track together: coherent dual function. If they conflict: readings foreclose each other.',
    'Coherent: tangled rope classification holds, both beneficiaries (sound-money early adopters, speculators) coexist. Conflicting: one reading is foreclosed, the constraint structure shifts (either dominant extraction or dominant coordination, not hybrid).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sound_money_kernel_coherence, empirical, 'Whether sound-money and speculative readings are logically compatible').

omega_variable(
    decentralization_ideology_realization,
    'Does the decentralization-ideology frame describe an achievable structural outcome (true scaffold) or is it an aspirational narrative covering extractive concentration?',
    'Analysis of actual distribution of mining power, exchange custody concentration, and governance control in major cryptosystems. If decentralization is achieved: scaffold frame is accurate. If centralization occurs despite ideology: constraint is snare disguised as scaffold.',
    'True scaffold: sunset mechanism is real, volatility should decline with maturity. False scaffold: constraint is snare with ideological theater, volatility persists or increases as centralization tightens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decentralization_ideology_realization, empirical, 'Whether decentralization ideology is realizable or rhetorical cover').

omega_variable(
    kernel_identity_contest,
    'Are sound-money, speculative-asset, and decentralization readings three interpretations of ONE kernel or three distinct kernels sharing only a label?',
    'Logical analysis: Can a single authority structure (Bitcoin protocol, consensus mechanism, community governance) simultaneously ground all three readings? Or does each reading require a different foundational authority (Austrian theory, market mechanisms, libertarian values)?',
    'One kernel: the CS structure involves competing readings with reading_relations={forecloses, coexists_with, influences}. Three kernels: each story is a separate constraint with distinct ε values and no direct reading_relations. This determines whether this JSON is a single constraint story with kernel reading structure or should be decomposed into three stories with network links.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_contest, conceptual, 'Whether readings share one kernel or three distinct kernels').

omega_variable(
    suppression_mechanism_structural_or_narrative,
    'Does the measured suppression (0.62) reflect structural market barriers (information asymmetry, execution speed, custody requirements) or narrative suppression (ideological commitment to decentralization overcoming rational exit)?',
    'Post-exit analysis: agents leaving crypto markets — do they cite material barriers (exchange access, capital controls) or ideological decommitment (loss of faith in decentralization narrative)? Ratio indicates mechanism decomposition.',
    'Structural suppression: snare classification holds for retail; trapped exit is external. Narrative suppression: identity_locked exit classification more accurate; the binding is cognitive, not material, opening different policy interventions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_or_narrative, empirical, 'Whether suppression is structural market barriers or internalized ideology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speculative_price_volatility, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spv_tr_t0, speculative_price_volatility, theater_ratio, 0, 0.45).
narrative_ontology:measurement(spv_tr_t5, speculative_price_volatility, theater_ratio, 5, 0.55).
narrative_ontology:measurement(spv_tr_t10, speculative_price_volatility, theater_ratio, 10, 0.68).
narrative_ontology:measurement(spv_tr_t15, speculative_price_volatility, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(spv_be_t0, speculative_price_volatility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(spv_be_t5, speculative_price_volatility, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(spv_be_t10, speculative_price_volatility, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(spv_be_t15, speculative_price_volatility, base_extractiveness, 15, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(spv_su_t0, speculative_price_volatility, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(spv_su_t5, speculative_price_volatility, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(spv_su_t10, speculative_price_volatility, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(spv_su_t15, speculative_price_volatility, suppression_requirement, 15, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speculative_price_volatility, resource_allocation).
narrative_ontology:affects_constraint(speculative_price_volatility, bitcoin_energy_intensity).
narrative_ontology:affects_constraint(speculative_price_volatility, stablecoin_central_counterparty_risk).
narrative_ontology:affects_constraint(speculative_price_volatility, cryptocurrency_regulatory_arbitrage).

% DUAL FORMULATION NOTE:
% This constraint story represents the INTEGRATED VOLATILITY CONSTRAINT across all three readings. Decomposition into three separate constraint stories is RECOMMENDED for downstream analysis: (1) sound_money_volatility_hypothesis (ε=0.15, Mountain if true; Piton if false), (2) speculative_asset_volatility_function (ε=0.30, Rope), (3) decentralization_volatility_signature (ε=0.65, Snare). The integrated story captures the contested frame; the decomposed stories enable each reading's internal consistency to be tested independently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speculative_price_volatility, powerful, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
