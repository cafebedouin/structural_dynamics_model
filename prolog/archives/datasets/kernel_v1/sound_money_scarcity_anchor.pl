% ============================================================================
% CONSTRAINT STORY: sound_money_scarcity_anchor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sound_money_scarcity_anchor, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: sound_money_scarcity_anchor
 *   human_readable: Sound Money Scarcity Anchor in Cryptocurrency Systems
 *   domain: monetary_economics/technology_governance/political_economy
 *
 * SUMMARY:
 *   The 'sound money scarcity anchor' in cryptocurrency systems represents a
 *   contested commitment grounded in three structurally distinct readings of
 *   a shared technical kernel: Bitcoin's fixed-supply design and
 *   cryptographic proof mechanism. The sound-money reading (Austrian
 *   economics framework) treats scarcity as a natural hedge against monetary
 *   debasement — the constraint is legitimate precisely because it is
 *   immutable and enforced by mathematics. The speculative-asset reading
 *   treats the same scarcity mechanism as a narrative device creating
 *   artificial demand and price cycles — the constraint is extractive because
 *   late entrants are trapped in a Ponzi-like structure where early adopters'
 *   liquidity exits are funded by new entrants' price expectations. The
 *   decentralization-ideology reading treats scarcity as enabling
 *   peer-to-peer coordination without central authority — the constraint is a
 *   coordination mechanism that allows stateless monetary policy. These three
 *   readings share a technical kernel (the fixed supply and consensus
 *   algorithm) but derive entirely different classification types and
 *   beneficiary/victim structures from it. The constraint story captures this
 *   contest through a presheaf of perspectives indexed by (Power, Time, Exit,
 *   Scope), revealing that the 'same' scarcity mechanism appears as mountain,
 *   rope, snare, tangled_rope, and piton depending on the observer's
 *   structural position and reading framework.
 *
 * KEY AGENTS:
 *   - Early Adopters & Protocol Developers: Primary beneficiary (institutional/arbitrage) — capture first-mover advantage, network effects, and narrative authority to define what scarcity means
 *   - Late-Entry Retail Investors: Primary victim (powerless/trapped) — exposed to price volatility, trapped by FOMO and narrative framing, exit-blocked by sunk losses
 *   - Central Banks & Monetary Authorities: Constrained institutional actor (organized/constrained) — threatened by competing monetary system, but see potential coordination benefits and reserve asset role
 *   - Merchants & Payment Network Participants: Secondary victim/beneficiary mix (moderate/constrained) — gain settlement certainty and censorship resistance, but bear volatility and network complexity costs
 *   - Consensus Mechanism (PoW/PoS): Institutional legitimacy theater (institutional/arbitrage) — cryptographic proof maintains narrative authority over scarcity but is increasingly detached from actual scarcity enforcement
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent social commitment to supply caps as immutable mathematical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sound_money_scarcity_anchor, 0.58).
domain_priors:suppression_score(sound_money_scarcity_anchor, 0.48).
domain_priors:theater_ratio(sound_money_scarcity_anchor, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sound_money_scarcity_anchor, extractiveness, 0.58).
narrative_ontology:constraint_metric(sound_money_scarcity_anchor, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sound_money_scarcity_anchor, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sound_money_scarcity_anchor, tangled_rope).
narrative_ontology:human_readable(sound_money_scarcity_anchor, "Sound Money Scarcity Anchor in Cryptocurrency Systems").
narrative_ontology:topic_domain(sound_money_scarcity_anchor, "monetary_economics/technology_governance/political_economy").

domain_priors:requires_active_enforcement(sound_money_scarcity_anchor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sound_money_scarcity_anchor, '8c1650fc-ae54-4f30-a139-090902df12cf').
narrative_ontology:cs_kernel_codification('8c1650fc-ae54-4f30-a139-090902df12cf', fixed_text).
narrative_ontology:cs_authority_grounding('8c1650fc-ae54-4f30-a139-090902df12cf', distributed).
narrative_ontology:cs_reading_relation('8c1650fc-ae54-4f30-a139-090902df12cf', sound_money_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c1650fc-ae54-4f30-a139-090902df12cf', speculative_asset_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c1650fc-ae54-4f30-a139-090902df12cf', decentralization_ideology_reading, coexists_with).
narrative_ontology:cs_axiom('8c1650fc-ae54-4f30-a139-090902df12cf', foundational, fixed_supply_hedge_against_debasement).
narrative_ontology:cs_axiom_status(fixed_supply_hedge_against_debasement, holdable).
narrative_ontology:cs_axiom_grounding('8c1650fc-ae54-4f30-a139-090902df12cf', fixed_supply_hedge_against_debasement, empirically_contingent).
narrative_ontology:cs_axiom('8c1650fc-ae54-4f30-a139-090902df12cf', secondary, mathematical_cryptographic_proof_sufficient).
narrative_ontology:cs_axiom_status(mathematical_cryptographic_proof_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('8c1650fc-ae54-4f30-a139-090902df12cf', mathematical_cryptographic_proof_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('8c1650fc-ae54-4f30-a139-090902df12cf', austrian_economics_monetary_theory).
narrative_ontology:cs_drift_state('8c1650fc-ae54-4f30-a139-090902df12cf', contemporary_central_bank_competition, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8c1650fc-ae54-4f30-a139-090902df12cf', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sound_money_scarcity_anchor, early_adopters_hodlers).
narrative_ontology:constraint_beneficiary(sound_money_scarcity_anchor, protocol_developers).
narrative_ontology:constraint_beneficiary(sound_money_scarcity_anchor, scarcity_narrative_beneficiaries).
narrative_ontology:constraint_victim(sound_money_scarcity_anchor, late_entrants_price_exposure).
narrative_ontology:constraint_victim(sound_money_scarcity_anchor, monetary_system_stability).
narrative_ontology:constraint_victim(sound_money_scarcity_anchor, fiat_currency_displaced_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE-ENTRY RETAIL INVESTOR (SNARE) — Trapped by asymmetric information and FOMO-driven entry at inflated valuations. Cannot exit without realizing losses; bears full extraction from early-adopter exit liquidity and narrative-driven price cycles. Maximum suppression: no alternative path to wealth creation appears available within the narrative frame.
constraint_indexing:constraint_classification(sound_money_scarcity_anchor, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY ADOPTERS & PROTOCOL DEVELOPERS (ROPE) — Experience the constraint as pure coordination: the scarcity mechanism creates the shared commitment that enables network effects. Beneficiaries with arbitrage options (can exit into fiat, alternative assets, or hold selectively). The scarcity narrative is the coordination technology itself — it synchronizes expectations and incentivizes development. Net positive extraction flow toward this group; they experience it as legitimate reward for first-mover and builder risk.
constraint_indexing:constraint_classification(sound_money_scarcity_anchor, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: MERCHANTS & PAYMENT USERS (TANGLED ROPE) — Face constrained exit: can theoretically leave for alternatives but face network effects, volatility costs, and payment-processing complexity. The scarcity mechanism provides both genuine coordination (settlement certainty via immutable ledger) and extraction (price volatility risk, network liquidity requirements). Moderate extraction: real benefits mixed with real costs.
constraint_indexing:constraint_classification(sound_money_scarcity_anchor, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CENTRAL BANKS & MONETARY AUTHORITIES (TANGLED ROPE) — Constrained relationship to the scarcity anchor: cannot ignore cryptocurrency's existence or regulatory pressure, but cannot fully embrace it (would cede monetary control). The constraint both threatens their authority structure (decentralized money competes with fiat policy space) and provides coordination benefit (scarcity narrative creates stable asset class they can integrate into reserve management). Active enforcement required (regulation) to maintain their control narrative.
constraint_indexing:constraint_classification(sound_money_scarcity_anchor, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TECHNICAL CONSENSUS MECHANISMS (PITON) — Proof-of-work and proof-of-stake algorithms are maintained as legitimacy theater: the mathematical proof of scarcity provides institutional authority for the narrative, but the actual scarcity is socially enforced through price market coordination, not mathematically guaranteed. The consensus mechanism persists through inertia (changing it destabilizes the narrative) despite its degraded function relative to stated goals. Theater ratio high: the 'cryptographic certainty' of scarcity is performative.
constraint_indexing:constraint_classification(sound_money_scarcity_anchor, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL SCARCITY VIEW (MOUNTAIN) — From a civilizational scale, the fixed-supply design and cryptographic proof of scarcity appear as immutable natural law: this is what the protocol IS, invariant across all economic contexts. However, this classification is a false summit candidate: the 'immutable scarcity' naturalizes a social consensus that could in principle be changed (though at high cost to network legitimacy). The engine will detect this as naturalization of a contingent institutional commitment.
constraint_indexing:constraint_classification(sound_money_scarcity_anchor, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sound_money_scarcity_anchor_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sound_money_scarcity_anchor, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sound_money_scarcity_anchor, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sound_money_scarcity_anchor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sound_money_scarcity_anchor, TR),
    TR >= 0.70.

:- end_tests(sound_money_scarcity_anchor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The scarcity anchor enables extraction through three distinct channels: (1) first-mover advantage where early adopters capture liquidity as later cohorts enter; (2) narrative-driven expectation management where the 'fixed supply' story justifies price appreciation independent of use-value; (3) volatility-driven transfers from low-information entrants to trading-sophisticated exits. The value of 0.58 reflects that genuine coordination benefits exist (settlement certainty, censorship resistance, network effects) alongside the extraction mechanisms, preventing classification as pure snare. Suppression (0.48): Moderate. Significant barriers include technical complexity (requiring specialized knowledge to verify scarcity claims), information asymmetry (asymmetric understanding of use-value vs. speculative dynamics), regulatory uncertainty (unclear legal status creates switching costs), and narrative lock-in (the 'immutable supply' framing creates psychological anchoring). But suppression is not total: exit paths exist through sale into fiat, diversification into alternative cryptocurrencies, or abstention from adoption. Theater ratio (0.65): Moderate-high. Increasing over the measurement interval from 0.42 to 0.65. The cryptographic proof of scarcity (consensus mechanism) functions partly as technical enforcement and partly as legitimacy theater. As the network has matured, the ratio of performative activity (discussing immutability, proving the mechanism works mathematically, convincing skeptics via technical argument) to functional activity (actual settlement, transactions) has risen. The theater has intensified because more effort is required to maintain narrative credibility as actual monetary adoption (measured in transaction volume, merchant acceptance, policy integration) has plateaued relative to speculative trading volume.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full range of DR classification from a single set of base properties and contested readings. The early-adopter reading (sound-money via Austrian framework) sees rope: the scarcity anchor coordinates expectation around a stable value store and legitimate reward for bearing protocol risk. The late-entry victim (speculative-asset reading) sees snare: the scarcity mechanism creates appearance of fundamental anchoring while actually extracting from new entrants whose entry funds earlier exits. Central banks (monetary authority reading) see tangled_rope: the mechanism both threatens their authority (competing monetary system) and provides benefits (alternative reserve asset, coordination mechanism for cross-border settlement). Merchants (practical coordination reading) see tangled_rope: genuine benefits (censorship resistance, settlement finality) mixed with costs (volatility, liquidity risk). The consensus mechanism itself (technical legitimacy theater) appears as piton: the cryptographic proof persists through institutional inertia, maintaining narrative authority even as actual scarcity enforcement becomes increasingly dependent on social consensus (the community's agreement not to fork the supply cap). The analytical observer's mountain classification (immutable mathematical law) is a false summit: it naturalizes a social commitment that could in principle be changed, though at catastrophic cost to network legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position: early adopters with arbitrage exit options experience low d (0.15-0.20), producing negative or minimal effective extraction (institutional beneficiary frame). Late entrants with trapped exit experience high d (0.95), producing maximum extraction (powerless victim frame). Moderate agents with constrained exit experience d ≈ 0.65-0.75, producing moderate extraction scaled by scope and time horizon. The analytical observer with analytical exit experiences d ≈ 0.72 (canonical value for the analytical position). The directionality derivation depends entirely on the reading frame: sound-money reading produces low d for early adopters (they are legitimate risk-bearers); speculative-asset reading produces high d for late entrants (they are trapped by narrative manipulation). No directionality override is needed because the structural data (beneficiary/victim declarations) directly produce the reading-dependent directionality values.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not trigger mandatrophy resolution because extractiveness (0.58) is below the 0.70 threshold. However, the omega variables document the underlying indeterminacy: whether scarcity is structural (mountain) or narrative (tangled_rope/snare) depends on answering whether the supply cap commitment is credibly immutable (omega: fork_credibility_commitment) and whether enforcement is technical or social (omega: narrative_enforcement_vs_technical_enforcement). If the supply cap is credibly immutable and technically enforced: mountain classification is justified, and the sound-money reading dominates. If enforcement is social and the cap is reversible: classification shifts toward tangled_rope or snare, and the speculative-asset reading dominates. The mandatrophy is resolved not through a single answer but through recognizing that all six types are legitimate readings of different answer states to these omegas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scarcity_constraint_vs_store_of_value,
    'Is the scarcity mechanism a genuine constraint on monetary supply, or a narrative device encoding behavioral expectations about value preservation?',
    'Counterfactual: if adoption collapsed, would the ''scarcity'' persist as meaningful? If scarcity is meaningful only when adoption is high, it is behavioral (narrative-dependent), not structural.',
    'If scarcity is structural: mountain classification is justified. If narrative-dependent: classification shifts to tangled_rope or snare depending on adoption dynamics and extraction mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scarcity_constraint_vs_store_of_value, conceptual, 'Whether scarcity is structural constraint or narrative device').

omega_variable(
    monetary_system_displacement,
    'Does widespread cryptocurrency adoption displace fiat monetary systems, or create an alternative settlement layer alongside them?',
    'Long-term empirical: monitoring adoption rates, regulatory integration (CBDCs, stablecoin backing), and whether central banks retain monetary policy space. If adoption plateaus below 50% of transaction volume: alternative layer. If adoption exceeds 80%: displacement trajectory.',
    'If displacement: the extraction mechanism is systemic (regime-level replacement of monetary authority). If alternative layer: extraction is confined to crypto-native contexts with bounded victims. Classification could shift to snare (if displacement) or remain tangled_rope (if bounded).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monetary_system_displacement, empirical, 'Whether cryptocurrency displaces or complements fiat systems').

omega_variable(
    fork_credibility_commitment,
    'How credible is the commitment to immutable supply caps? Would the protocol community actually refuse to hard-fork to increase supply in a genuine crisis (e.g., systemic settlement failure, 51% attack vulnerability)?',
    'Historical analysis: past hard-fork decisions (Ethereum DAO rollback, Bitcoin UASF), developer statements on supply cap immutability, game-theoretic analysis of incentives to preserve vs. break the cap under different crisis scenarios.',
    'If credibly immutable: scarcity anchor is stable, mountain classification has some empirical merit. If commitment is reversible: scarcity is contingent on social consensus, not technical law — shifts to tangled_rope or piton depending on narrative enforcement strength.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fork_credibility_commitment, empirical, 'Credibility of immutable supply cap commitment').

omega_variable(
    kernel_or_three_kernels,
    'Is ''cryptocurrency'' one contested kernel read through three frameworks (sound-money, speculative-asset, decentralization-ideology), or three distinct kernels sharing only a name?',
    'Structural: do all three readings claim authority from the same founding text/design (Bitcoin whitepaper, Ethereum manifesto) or from different legitimizing sources? If same source read differently: one kernel. If different sources: three kernels requiring separate constraint stories.',
    'If one kernel: this story captures the contest. If three kernels: should decompose into separate constraint stories per the ε-invariance principle, each with its own ε and beneficiary/victim structure. The ''scarcity anchor'' might be a mountain from sound-money reading but a snare from speculative-asset reading — different ε values suggest decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_or_three_kernels, conceptual, 'Whether crypto is one kernel with three readings or three distinct kernels').

omega_variable(
    narrative_enforcement_vs_technical_enforcement,
    'Is the scarcity anchor enforced primarily by the cryptographic protocol, or by the social consensus narrative around the protocol?',
    'Decomposition analysis: isolate technical constraints (mathematical proof of scarcity) from behavioral constraints (market consensus on supply cap legitimacy). If technical enforcement alone maintains scarcity independent of narrative: mountain or rope. If narrative consensus is required (e.g., community would refuse to use forks that break supply cap): tangled_rope or piton.',
    'If technical: scarcity is more robust to social change. If narrative: scarcity is vulnerable to paradigm shifts and expectation reversal. This directly affects whether theater_ratio (0.65) should be higher or whether the constraint should shift to piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(narrative_enforcement_vs_technical_enforcement, empirical, 'Balance of technical vs. narrative enforcement of scarcity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sound_money_scarcity_anchor, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scarcity_theater_t0, sound_money_scarcity_anchor, theater_ratio, 0, 0.42).
narrative_ontology:measurement(scarcity_theater_t5, sound_money_scarcity_anchor, theater_ratio, 5, 0.55).
narrative_ontology:measurement(scarcity_theater_t10, sound_money_scarcity_anchor, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(scarcity_extract_t0, sound_money_scarcity_anchor, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(scarcity_extract_t5, sound_money_scarcity_anchor, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(scarcity_extract_t10, sound_money_scarcity_anchor, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(scarcity_suppress_t0, sound_money_scarcity_anchor, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(scarcity_suppress_t5, sound_money_scarcity_anchor, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(scarcity_suppress_t10, sound_money_scarcity_anchor, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sound_money_scarcity_anchor, resource_allocation).
narrative_ontology:boltzmann_floor_override(sound_money_scarcity_anchor, 0.18).
narrative_ontology:affects_constraint(sound_money_scarcity_anchor, monetary_policy_space_constraint).
narrative_ontology:affects_constraint(sound_money_scarcity_anchor, price_discovery_efficiency).
narrative_ontology:affects_constraint(sound_money_scarcity_anchor, regulatory_arbitrage_window).

% DUAL FORMULATION NOTE:
% The scarcity anchor constraint is upstream of several dependent constraints in the monetary governance domain. Monetary policy space constraint (central bank's ability to conduct expansionary policy without crypto competition) is directly affected by adoption rates and credibility of the supply cap. Price discovery efficiency (whether cryptocurrency markets efficiently incorporate fundamental value vs. speculative dynamics) is a downstream measurement of whether the scarcity mechanism is functional coordination or narrative theater. Regulatory arbitrage window (opportunities for actors to shift assets to jurisdictions with crypto-friendly policy) depends on the credibility of the scarcity anchor itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sound_money_scarcity_anchor, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
