% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__nakamoto_oracle_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
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
 *   constraint_id: bitcoin_whitepaper_purpose__nakamoto_oracle_opacity
 *   human_readable: Bitcoin Whitepaper Purpose Ambiguity (Nakamoto Oracle Opacity Reading)
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   In January 2011, Satoshi Nakamoto transferred control of the Bitcoin
 *   reference implementation to Gavin Andresen and effectively disappeared
 *   from public discourse. This event created a structural constraint: the
 *   whitepaper remained as the canonical description of Bitcoin's purpose,
 *   but its only authoritative interpreter had withdrawn. Subsequent
 *   interpreters claimed fidelity to the whitepaper while implementing
 *   divergent visions: electronic cash requiring high transaction throughput
 *   (Bitcoin Cash, Lightning Network advocates), store-of-value requiring
 *   scarcity preservation and validation decentralization (Bitcoin Core
 *   maximalists), or alternative purposes entirely (privacy coins diverging
 *   from transaction pseudonymity). The interpretive vacuum enabled fork
 *   proliferation and lock-in extraction while preventing convergence. This
 *   constraint story models the 'nakamoto_oracle_opacity' reading: the
 *   oracle's absence is the primary mechanism enabling both coordination
 *   failure and extractive rent-seeking. Other readings
 *   (electronic_cash_reading, store_of_value_reading) exist as separate
 *   constraints with different ε values and beneficiary/victim structures.
 *   This reading focuses on how the absence of authoritative interpretation
 *   creates the condition for asymmetric extraction by protocol
 *   entrepreneurs, interpretation gatekeepers, and maximalist factions —
 *   groups that benefit from the inability of the ecosystem to converge on a
 *   single meaning.
 *
 * KEY AGENTS:
 *   - Satoshi Nakamoto (Oracle, now absent): The sole agent with uncontested authority to interpret the whitepaper. Disappearance eliminated authoritative resolution mechanism.
 *   - Ecosystem Coherence (Primary victim, powerless/trapped): Abstract collective good — network effects, interoperability, unified protocol development. Bears extraction through fragmentation.
 *   - End Users (Primary victim, powerless/trapped): Adopt Bitcoin with capital at stake; cannot exit without switching costs; face uncertainty about which fork preserves founder intent.
 *   - Protocol Entrepreneurs (Beneficiary, powerful/mobile): Launch alternative implementations; claim fidelity to whitepaper while pursuing preferred design goals; extract adoption rent and lock-in value.
 *   - Interpretation Gatekeepers (Beneficiary, organized/constrained): Core developers, influential commentators, Bitcoin Core maintainers; constrained by peer credibility but have outsized voice in interpretive disputes; extract status and influence.
 *   - Protocol Coordination Layer (Neutral, institutional/arbitrage): Decentralized consensus as pure coordination function; enables other perspectives' extraction without participating in it.
 *   - Bitcoin Foundation and Institutional Legacy (Theater institution, institutional/arbitrage): Maintains appearance of stewardship role; exercises no real enforcement power; performance persists through inertia.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.65).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "Bitcoin Whitepaper Purpose Ambiguity (Nakamoto Oracle Opacity Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, '81636912-772b-4d34-8f13-fa4ef7a50388').
narrative_ontology:cs_kernel_codification('81636912-772b-4d34-8f13-fa4ef7a50388', fixed_text).
narrative_ontology:cs_authority_grounding('81636912-772b-4d34-8f13-fa4ef7a50388', extraction).
narrative_ontology:cs_reading_relation('81636912-772b-4d34-8f13-fa4ef7a50388', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('81636912-772b-4d34-8f13-fa4ef7a50388', bitcoin_whitepaper_purpose__store_of_value_reading, coexists_with).
narrative_ontology:cs_axiom('81636912-772b-4d34-8f13-fa4ef7a50388', foundational, oracle_absence_structural_indeterminacy).
narrative_ontology:cs_axiom_status(oracle_absence_structural_indeterminacy, holdable).
narrative_ontology:cs_axiom_grounding('81636912-772b-4d34-8f13-fa4ef7a50388', oracle_absence_structural_indeterminacy, deontological).
narrative_ontology:cs_axiom('81636912-772b-4d34-8f13-fa4ef7a50388', secondary, fork_proliferation_under_indeterminacy).
narrative_ontology:cs_axiom_status(fork_proliferation_under_indeterminacy, holdable).
narrative_ontology:cs_axiom_grounding('81636912-772b-4d34-8f13-fa4ef7a50388', fork_proliferation_under_indeterminacy, empirically_contingent).
narrative_ontology:cs_reference_frame('81636912-772b-4d34-8f13-fa4ef7a50388', nakamoto_as_epistemic_authority).
narrative_ontology:cs_drift_state('81636912-772b-4d34-8f13-fa4ef7a50388', contemporary_post_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('81636912-772b-4d34-8f13-fa4ef7a50388', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, maximalist_forks).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, protocol_entrepreneurs).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, interpretation_gatekeepers).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, ecosystem_coherence).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, interoperability_infrastructure).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, end_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECOSYSTEM COHERENCE (SNARE) — Cannot exit the interpretive chaos; the network effect commons has no voice and no authority to adjudicate claims. Every fork claims fidelity to 'the real Bitcoin.' The ecosystem bears extraction through fragmentation without ability to organize or resolve. Maximum suppression — the founding text is now inert; Nakamoto's disappearance eliminated the only credible arbiter.
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: END USERS (SNARE) — Trapped by wallet incompatibility, exchange liquidity fragmentation, and irreducible uncertainty about which version has preserved the founder's intent. Cannot exit without massive switching costs. No oracle to clarify which fork is 'legitimate.' Extraction takes the form of forced choice: commit capital to one interpretation and risk it being strategically abandoned as another gains traction.
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PROTOCOL ENTREPRENEURS (TANGLED ROPE) — Mobile; can launch new forks and migrate governance. The interpretive vacuum is a feature, not a bug: they benefit from ability to claim fidelity to 'true Bitcoin' while implementing preferred modifications. Genuine coordination function: each fork attempts to preserve Nakamoto's principles while removing perceived bottlenecks. But also extraction: each fork extracts adoption rent and lock-in value from users who cannot easily switch between competing interpretations. Active enforcement required: each fork must continually reinforce its legitimacy claim against rival interpretations.
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERPRETATION GATEKEEPERS (TANGLED ROPE) — Core developers and influential commentators are constrained by peer community pressure and technical credibility but have outsized voice in interpretive disputes. They coordinate protocol evolution AND extract status/influence from being recognized authorities on 'what Satoshi meant.' Suppression is moderate: dissenting developers cannot easily fork without losing credibility, but branching costs are lowering. Active enforcement: continuous defense of specific interpretations against rival claims to authenticity.
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PROTOCOL COORDINATION (ROPE) — From the perspective of decentralized consensus itself, the constraint coordinates technical governance: without Nakamoto's authority, the community must solve coordination problems through transparent code review, proposal processes, and consensus testing. This is coordination, not extraction. Network effects themselves solve the equilibration: which interpretation attracts more honest nodes wins. Pure coordination in the technical substrate layer, enabling other perspectives' extraction.
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: BITCOIN FOUNDATION / INSTITUTIONAL LEGACY (PITON) — The foundation, conceived to steward Nakamoto's vision, has become a theater institution with degraded legitimacy. Stakeholders treat it as an authoritative body because the alternative (chaos) is worse, but it has no real enforcement power and its interpretive pronouncements are routinely ignored or contested. Theater ratio high; functional authority nearly zero. Maintained through institutional inertia — organizations exist to adjudicate, so this organization persists even though its adjudications don't settle disputes.
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational perspective, the interpretive vacuum is an inherent property of decentralized systems without privileged nodes: no single source can authoritatively resolve meaning. This is presented as a 'feature not a bug' — true decentralization means no oracle. However, the structural data reveals this as a false summit: the oracle opacity is not inherent to decentralization itself but to Nakamoto's specific choice to disappear and the culture's veneration of his authority. Other founders have remained engaged (Vitalik, Charlie Lee). The 'immutability' framing naturalizes a contingent choice.
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, TR),
    TR >= 0.70.

:- end_tests(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Rising from 0.22 in 2009-2011 baseline to 0.58 by 2024. The constraint began as minimal extraction when Nakamoto was understood to be steward; the oracle absence was latent. Extraction accelerated as forks multiplied (2012-2015) and later as transaction scaling disputes intensified (2015-2018), then stabilized as ecosystem segmented into stable factions. The extractiveness measures the lock-in rent imposed by interpretive fragmentation: users must choose a fork without oracle guidance, locking capital into one interpretation's success. Suppression (0.65): The measured suppression is high and rising (0.35→0.65 over interval), reflecting that dissenters cannot easily shift to rival forks without massive costs. But the suppression has a specific character: it is not enforcement of a single interpretation but enforcement of the constraint itself — the barrier to convergence. Users are suppressed by their inability to obtain oracle clarification, not by active coercion. Theater ratio (0.48): Moderate and rising, reflecting increasing ceremonial rhetoric around 'what Satoshi meant' while actual governance is driven by economic incentives and developer preference. Bitcoin Foundation pronouncements are theater; technical governance happens in developer communities. The theater ratio rises as interpretive disputes intensify and participants feel compelled to justify positions through whitepaper exegesis rather than technical arguments.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival gap. End users experience Snare — they are trapped by uncertainty and switching costs. Protocol entrepreneurs experience Tangled Rope — they benefit from interpretive ambiguity while maintaining genuine coordination function. Interpretation gatekeepers experience Tangled Rope — they are constrained by credibility norms while extracting influence. The pure protocol coordination layer (analytical perspective on consensus itself) sees Rope — decentralized governance as coordination without extraction. The institutional legacy (Bitcoin Foundation) sees Piton — theater without function. The civilizational analytical observer risks seeing Mountain (decentralization inherently requires interpretive openness), but the structural data reveals this as false summit: Nakamoto chose to disappear; other founders have remained engaged; the oracle opacity is contingent, not inherent. The perspectival gap reveals that the constraint is not a feature of decentralization but an artifact of institutional choice and subsequent economic incentive structures.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position and exit capacity. Ecosystem coherence (victim, no exit) gets maximum d→maximum f(d). End users (victim, trapped by switching costs) get high d. Protocol entrepreneurs (beneficiary, can launch new forks) get low d — they have mobile exit and benefit from the constraint. Interpretation gatekeepers (beneficiary, constrained by credibility) get moderate d — they can move between forks but at cost. The protocol coordination layer is analytically privileged (d~0.72, canonical analytical value), which produces the analytical observer's perspective. The measurement progression shows d increasing over time as the constraint matures: early on, users could migrate between emerging forks at low cost (lower effective d); as lock-in accumulated (exchanges, wallet ecosystems), d increased for end users, raising their experienced extraction. The oracle's absence enables this d distribution to persist: without an oracle to converge the ecosystem, the d values stay heterogeneous across the fork network.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oracle_absence_vs_design,
    'Is the interpretive vacuum an inherent consequence of decentralization or a contingent result of Nakamoto''s specific choice to disappear?',
    'Historical counterfactual: model governance outcomes if Nakamoto had remained active; comparative analysis of other blockchain projects with active founders (Ethereum, Monero, Zcash) and their fork dynamics',
    'If inherent: mountain classification is correct; decentralization structurally requires interpretive openness. If contingent: the false summit detector fires; the constraint is tangled_rope with extractive asymmetry, not a law of distributed systems.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oracle_absence_vs_design, conceptual, 'Whether oracle absence is inherent to decentralization or contingent to founder choice').

omega_variable(
    fork_coordination_failure_mechanism,
    'Do Bitcoin forks proliferate because interpretive ambiguity enables rent-seeking, or because genuine technical disagreements on ''true Bitcoin principles'' are irresolvable without an oracle?',
    'Analysis of fork proliferation timelines and triggers; correlation between interpretive disputes and economic incentives (coin exchange listing, mining revenue, developer funding); clustering of forks by stated design principle vs stated interpretation of whitepaper',
    'If rent-seeking dominant: suppression is primarily extraction mechanism (snare dynamics). If genuine disagreement: suppression is coordination failure (tangled_rope or rope). Determines whether forks are parasitic (extracting lock-in rent) or divergent solutions to coordination problems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fork_coordination_failure_mechanism, empirical, 'Whether fork proliferation is rent-seeking or technical disagreement').

omega_variable(
    whitepaper_semantic_indeterminacy,
    'Is the whitepaper itself genuinely ambiguous on store-of-value vs electronic-cash purpose, or have subsequent interpretations read ambiguity into a text that was originally clear to contemporary readers?',
    'Discourse analysis of technical forums 2009-2011 before Nakamoto''s departure; contemporary developer interpretations vs modern revisionist readings; semantic analysis of whitepaper language specificity on transaction throughput, settlement finality, use cases',
    'If genuinely ambiguous: oracle opacity creates real indeterminacy requiring resolution mechanism (suggests landscape with multiple local attractors). If reading-induced: the constraint is extractive reinterpretation of a clear text; suppression is enforced amnesia (higher actual suppression than measured).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whitepaper_semantic_indeterminacy, empirical, 'Whether whitepaper is semantically indeterminate or interpretations created apparent ambiguity').

omega_variable(
    nakamoto_return_counterfactual,
    'If Nakamoto returned today and issued a definitive statement on the correct interpretation, would the ecosystem converge?',
    'Analysis of Bitcoin governance precedents where external authority attempted interpretation (Bitcoin Core leadership statements, Bitcoin Unlimited proposals); historical moments where Satoshi''s identity was claimed and community response; analysis of which forks would accept vs reject Satoshi statement',
    'If convergence likely: the oracle opacity is the primary mechanism maintaining the constraint (removing it would dissolve the structure). If forks reject Satoshi clarification: the constraint has become self-sustaining (economic incentives prevent convergence regardless of oracle); extraction is structural, not information-driven.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nakamoto_return_counterfactual, conceptual, 'Whether ecosystem would converge on Nakamoto''s return').

omega_variable(
    reading_relation_empirical_basis,
    'Can the store-of-value and electronic-cash readings coexist within a single reading of the whitepaper, or do they require logically incompatible premises about Bitcoin''s purpose?',
    'Textual analysis of whitepaper for simultaneous support of both readings; economic model analysis of whether store-of-value properties (limited supply, scarce digital commodity) are compatible with electronic-cash properties (high transaction velocity, low friction); analysis of scaling trilemma constraints that force choice between readings',
    'If compatible: readings coexist_with each other. If incompatible at system level: one reading forecloses the other (high-tx-rate cash forecloses store-of-value scarcity premium and vice versa). Determines reading_relations geometry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relation_empirical_basis, empirical, 'Whether store-of-value and cash readings are logically compatible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0, 13).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bwpo_theater_t0, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 0, 0.18).
narrative_ontology:measurement(bwpo_theater_t4, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 4, 0.28).
narrative_ontology:measurement(bwpo_theater_t8, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 8, 0.38).
narrative_ontology:measurement(bwpo_theater_t13, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 13, 0.48).

% Extraction over time
narrative_ontology:measurement(bwpo_extract_t0, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(bwpo_extract_t4, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(bwpo_extract_t8, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(bwpo_extract_t13, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 13, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bwpo_suppress_t0, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(bwpo_suppress_t4, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(bwpo_suppress_t8, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 8, 0.61).
narrative_ontology:measurement(bwpo_suppress_t13, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 13, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, identity_coordination).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose__electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose__store_of_value_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_mining_concentration).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, cryptocurrency_exchange_systemic_risk).

% DUAL FORMULATION NOTE:
% The bitcoin_whitepaper_purpose kernel decomposes into three distinct constraints with different ε values: (1) nakamoto_oracle_opacity (this file, ε=0.58) models the structural condition of absent authoritative interpretation; (2) electronic_cash_reading (separate file, expected ε~0.50) models the competitive interpretation claiming Bitcoin's true purpose is transaction throughput; (3) store_of_value_reading (separate file, expected ε~0.45) models the competitive interpretation claiming Bitcoin's true purpose is immutable scarcity. These are NOT the same constraint viewed differently. They have different beneficiary/victim distributions, different suppression mechanisms, and different measurement trajectories. The oracle_opacity reading is upstream — it creates the condition under which the alternative readings compete. The ε-invariance principle requires decomposition: if the observable (which interpretation's claim we evaluate) changes ε substantially, we have structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
