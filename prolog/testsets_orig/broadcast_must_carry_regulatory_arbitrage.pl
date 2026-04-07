% ============================================================================
% CONSTRAINT STORY: broadcast_must_carry_regulatory_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_broadcast_must_carry_regulatory_arbitrage, []).

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
 *   constraint_id: broadcast_must_carry_regulatory_arbitrage
 *   human_readable: Broadcast Must Carry Regulatory Arbitrage
 *   domain: media_regulation/telecommunications_policy
 *
 * SUMMARY:
 *   The Broadcast Must Carry (BMC) rule, enshrined in the 1992 Cable
 *   Television Consumer Protection and Competition Act, requires cable
 *   operators to carry local broadcast television stations. The rule was
 *   created to protect broadcast stations from cable gatekeeping and ensure
 *   that local news and content remained accessible to cable subscribers.
 *   However, the constraint exhibits a complex structure that blurs
 *   coordination with extraction. The rise of streaming video, broadband
 *   distribution, over-the-air digital transmission, and cable's own
 *   video-on-demand systems have substantially reduced the causal force of
 *   cable's gatekeeping power. Yet the must-carry mandate persists, now
 *   functioning primarily to maintain cable's regulatory privilege over
 *   alternative distribution methods and to suppress competing platforms'
 *   incentive to invest in local content. The constraint manifests
 *   differently across seven perspectives: for independent channels seeking
 *   national distribution, it is a snare with no exit; for streaming
 *   platforms, it is a tangled rope mixing genuine coordination benefits with
 *   asymmetric extraction; for cable operators, it is pure coordination; for
 *   the FCC enforcement apparatus, it is an increasingly performative ritual
 *   maintained through inertia. The theater ratio has increased from 0.38
 *   (when cable was truly the dominant distribution medium) to 0.55 (now), as
 *   the rule's functional content has decoupled from its regulatory
 *   justification.
 *
 * KEY AGENTS:
 *   - Cable Operators: Primary beneficiaries (institutional/arbitrage) — benefit from regulatory exemption from net neutrality obligations; maintain gatekeeping power over bundled tiers; leverage must-carry to suppress streaming competition
 *   - Incumbent Broadcasters: Secondary beneficiaries (powerful/mobile) — guaranteed carriage protects against gatekeeping but also constrains direct-to-consumer distribution; have significant exit options
 *   - Independent Channels: Primary victims (powerless/trapped) — cannot achieve distribution through must-carry mandate (applies only to pre-existing broadcast signals); locked into choice between regulatory dependency or invisibility
 *   - Streaming Platforms: Constrained victims (moderate/constrained) — cannot use must-carry (rule applies only to broadcast); face pressure to negotiate carriage with cable operators; dual-distribution model required
 *   - FCC Must-Carry Enforcement: Institutional actor (institutional/arbitrage) — maintains and enforces the rule; increasingly engaging in performative enforcement of declining functional relevance
 *   - Net Neutrality Coalition: Organized agent (organized/constrained) — seeking sunset of must-carry through broader broadband regulation; has political leverage but faces cable industry opposition
 *   - Consumer Choice: Structural victim (powerless/trapped) — bundled cable tiers limit modular content selection; must-carry maintains carriage obligations that increase consumer subscription costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(broadcast_must_carry_regulatory_arbitrage, 0.58).
domain_priors:suppression_score(broadcast_must_carry_regulatory_arbitrage, 0.62).
domain_priors:theater_ratio(broadcast_must_carry_regulatory_arbitrage, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(broadcast_must_carry_regulatory_arbitrage, extractiveness, 0.58).
narrative_ontology:constraint_metric(broadcast_must_carry_regulatory_arbitrage, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(broadcast_must_carry_regulatory_arbitrage, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(broadcast_must_carry_regulatory_arbitrage, tangled_rope).
narrative_ontology:human_readable(broadcast_must_carry_regulatory_arbitrage, "Broadcast Must Carry Regulatory Arbitrage").
narrative_ontology:topic_domain(broadcast_must_carry_regulatory_arbitrage, "media_regulation/telecommunications_policy").

domain_priors:requires_active_enforcement(broadcast_must_carry_regulatory_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(broadcast_must_carry_regulatory_arbitrage, cable_operators).
narrative_ontology:constraint_beneficiary(broadcast_must_carry_regulatory_arbitrage, incumbent_broadcasters).
narrative_ontology:constraint_victim(broadcast_must_carry_regulatory_arbitrage, independent_channels).
narrative_ontology:constraint_victim(broadcast_must_carry_regulatory_arbitrage, streaming_platforms).
narrative_ontology:constraint_victim(broadcast_must_carry_regulatory_arbitrage, consumer_choice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT CHANNEL (SNARE) — A new or independent broadcast channel has no meaningful exit from the must-carry regime. Either it achieves carriage through regulatory mandate (which creates dependency on regulatory goodwill), or it remains effectively invisible to distributed audiences. The channel cannot negotiate carriage; it cannot build consumer choice through alternative distribution pathways without losing the regulatory protection that is supposed to guarantee access. Maximum extraction: forced into a system designed to benefit incumbents while bearing full cost of regulatory capture.
constraint_indexing:constraint_classification(broadcast_must_carry_regulatory_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STREAMING PLATFORM (TANGLED ROPE) — Streaming platforms are constrained from using the must-carry mandate (it applies only to broadcast signals), but benefit from the regulatory complexity that slows cable competition. They can distribute independently but face pressure to negotiate carriage with cable operators. Genuine coordination function exists: the must-carry rule does prevent cable monopoly gatekeeping in principle. But asymmetric extraction occurs: streaming platforms cannot use the mandate, while incumbents can; platforms bear costs of dual-distribution models while cable operators consolidate leverage. Constrained exit (switching to cable-exclusive would forfeit direct consumer access) produces moderate experienced extraction.
constraint_indexing:constraint_classification(broadcast_must_carry_regulatory_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CABLE OPERATOR (ROPE) — The cable operator experiences the must-carry rule as pure coordination: the rule requires carrying broadcast signals, which they were already incentivized to do (local news, sports, popular content drives customer subscriptions). The regulatory mandate merely formalizes coordination that benefits both cable operators and broadcasters. Exit option is arbitrage: cable operators can negotiate carriage with streaming services, can lobby for regulatory exemptions, and can shift signal distribution to IPTV or wireless modalities that fall outside must-carry scope. Net beneficiary.
constraint_indexing:constraint_classification(broadcast_must_carry_regulatory_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NET NEUTRALITY COALITION (SCAFFOLD) — Organized consumer advocates and tech companies see must-carry as a temporary coordination fix with a sunset: as broadband distribution replaces cable as the primary delivery medium, must-carry rules become obsolete. The coalition is pushing for generalized net neutrality rules that would eliminate the need for sector-specific must-carry requirements. They have agency (political organizing, technical standards-setting) and a concrete exit path (universal broadband competition). Suppression is real (cable lobbying, regulatory capture) but not total — the coalition can mobilize counterweight. Theater is moderate: the must-carry mandate performs a protective function (prevents gatekeeping) but is increasingly disconnected from how people actually consume video.
constraint_indexing:constraint_classification(broadcast_must_carry_regulatory_arbitrage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FCC MUST-CARRY ENFORCEMENT (PITON) — The must-carry rule itself is increasingly performative. It was created to protect broadcast signals when cable was the dominant distribution medium. Cable still carries broadcast signals, but the causal mechanism has degraded: many households now access broadcast content through streaming (YouTube TV, Hulu Live), over-the-air reception, or cable's own video-on-demand systems. The FCC continues to enforce must-carry, but the enforcement targets a shrinking pie — it has power over fewer and fewer distribution pathways. Theater ratio is moderate-to-high: the rule is maintained through institutional inertia despite declining functional relevance. Arbitrage exit: cable operators have largely internalized must-carry as standard practice and can lobby for exemptions on newer technologies.
constraint_indexing:constraint_classification(broadcast_must_carry_regulatory_arbitrage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INCUMBENT BROADCASTER (TANGLED ROPE) — Large incumbent broadcasters (ABC, NBC, CBS affiliates) have a complex relationship to must-carry. They benefit from the rule (guaranteed carriage on cable systems), but also have significant exit options (direct streaming, negotiated carriage, political leverage). The rule does protect them from cable gatekeeping, providing genuine coordination function. But it also extracts from them: they cannot fully control distribution through their own platforms without cable carriage remaining valuable. Suppression is real but surmountable: large broadcasters have resources to negotiate, lobby, and build alternative distribution. Mobile exit options (direct-to-consumer streaming) mean powerful experienced extraction is lower than for trapped agents — extraction is conditional on choosing to play within the regulated system.
constraint_indexing:constraint_classification(broadcast_must_carry_regulatory_arbitrage, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the must-carry rule might appear as an immutable feature of broadcast regulation: a natural response to the scarcity of electromagnetic spectrum and the public interest in local content. However, this perspective naturalizes what is actually a contingent institutional arrangement. The analytical observer risks concluding that content distribution naturally requires gatekeeping safeguards (mountain of regulation) when the structural data reveals that the arrangement extracts from independent channels and streaming platforms while benefiting incumbents. The mountain classification signals the danger of false summits — naturalizing regulatory capture as inherent to media markets.
constraint_indexing:constraint_classification(broadcast_must_carry_regulatory_arbitrage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(broadcast_must_carry_regulatory_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(broadcast_must_carry_regulatory_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(broadcast_must_carry_regulatory_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(broadcast_must_carry_regulatory_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(broadcast_must_carry_regulatory_arbitrage, TR),
    TR >= 0.70.

:- end_tests(broadcast_must_carry_regulatory_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The rule extracts from independent channels (no exit, no carriage) and streaming platforms (cannot use mandate, must negotiate separately) while benefiting cable operators and incumbent broadcasters. The extraction is substantial but not total because alternative distribution pathways exist (YouTube, FAST channels, OTA reception, streaming services) — independent channels are not completely locked in economically, only locked out of the most valuable distribution channel. The extraction has increased over 14 years as streaming platforms have proliferated and cable's monopoly has weakened; the rule's value to incumbents has risen precisely as the coordination justification has weakened. Suppression (0.62): Moderate-high. Significant barriers include: cable operators' legal obligation to carry local signals (creates collateral obligation to prioritize them in interface design), FCC regulatory capture by cable interests, the natural language (local content protection) that obscures extraction logic, limited spectrum for alternative broadcast delivery, and the resource requirements for streaming platforms to negotiate separate carriage. But suppression is not total: over-the-air reception is free, streaming services exist, YouTube carries independent content, and some markets have alternative broadband providers. Theater ratio (0.55): Moderate. The rule performs its stated function (local content is carried on cable) but with declining efficiency. The functional value has degraded: fewer consumers rely solely on cable for content access; must-carry carriage no longer determines reach or audience; the rule's enforcement mechanisms (FCC negotiation processes, carriage dispute resolution) are bureaucratic and slow-moving rather than effective market mechanisms. The theater ratio has increased as the rule's causal force has declined.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Cable operators see pure coordination (Rope) — the rule formalizes what they were already incentivized to do and protects them from competing platforms. Incumbent broadcasters see mixed coordination and constrained extraction (Tangled Rope) — they benefit from carriage protection but lose direct distribution control. Streaming platforms see tangled rope with higher extraction (Tangled Rope) — they cannot use the mandate and must negotiate from weaker positions. Independent channels see pure extraction (Snare) — trapped without access to the rule's benefits. The FCC enforcement apparatus sees a degraded ritual (Piton) — the rule persists through institutional inertia despite declining functional necessity. The net neutrality coalition sees a temporary problem with a sunset (Scaffold) — as broadband regulation matures, must-carry becomes obsolete. The civilizational analytical observer risks naturalizing the rule as inherent to media regulation (Mountain) — seeing it as a timeless response to spectrum scarcity and gatekeeping when it is actually a contingent artifact of 1990s cable dominance. This perspectival diversity reveals that no single classification is 'correct' — the constraint's true nature is its multivalence across observation contexts.
 *
 * DIRECTIONALITY LOGIC:
 *   Cable operators have the lowest experienced extraction (negative chi from their perspective) because they are primary beneficiaries with arbitrage exit options. They can lobby for exemptions, negotiate streaming carriage, switch to new distribution technologies (IPTV, wireless), and maintain leverage in any policy negotiations. Their beneficiary status and arbitrage exit produce d ≈ 0.15, yielding f(d) ≈ -0.01, making their effective extraction negligible or negative (they benefit). Incumbent broadcasters have moderate d ≈ 0.45 because they are partial beneficiaries (guaranteed carriage) with mobile exit options (direct streaming). Their powerful status means they can exit the system if the returns from direct distribution exceed carriage benefits. Streaming platforms face higher d ≈ 0.65 because they are constrained victims: they cannot use the mandate, must negotiate separately, and their exit is costly (withdrawing from cable loses audience reach). Independent channels face maximum d ≈ 0.95 because they are trapped victims: the rule does not apply to them, they cannot force carriage, yet the rule's existence creates regulatory capture that makes alternative carriage negotiation harder. The analytical observer has d ≈ 0.72 (canonical for analytical power), revealing the risk of false summits when naturalizing contingent arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in broadcast must-carry resolves by recognizing that the constraint has transitioned from genuine coordination (protecting broadcast signals from cable gatekeeping, enabling local content access) to regulatory arbitrage (suppressing streaming competition, extracting from independent channels that cannot benefit from the mandate, maintaining cable's bundled-tier model). The transformation is not a classification error but a historical drift: the constraint WAS primarily coordination when cable was the dominant distribution medium (1992-2010); it has become primarily extractive as broadband and streaming have proliferated (2015-present). The theater ratio increase (0.38→0.55) captures this drift: as the rule's causal force has weakened, its performative content has grown. The constraint now masks regulatory capture under the language of local content protection. The false summit lies in the analytical observer's mountain classification — viewing the rule as a natural law of media regulation when it is actually a contingent institutional arrangement that has outlived its original justification. Resolution requires decomposition: the genuine coordination problem (ensuring diverse local content) remains valid, but the must-carry mechanism is no longer the appropriate solution in a broadband-primary distribution environment. Alternative mechanisms (public broadcasting funding, net neutrality rules, local content obligations for broadband providers) might achieve the coordination goal without the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cable_gatekeeping_necessity,
    'Is cable gatekeeping (or gatekeeping by any single distribution platform) inherently necessary, or is it a contingent feature of cable''s historical dominance?',
    'Empirical comparison of content diversity in cable-bundled markets vs. unbundled broadband-native markets (YouTube, streaming platforms). Analysis of whether consumers actually prefer bundled tiers or would choose modular content selection if available.',
    'If gatekeeping is necessary: must-carry rules are coordination (rope/tangled rope from all perspectives). If contingent: must-carry rules are regulatory capture disguised as coordination, extracting from independent channels and new entrants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cable_gatekeeping_necessity, empirical, 'Whether content platform gatekeeping is inherently necessary').

omega_variable(
    spectrum_scarcity_persistence,
    'Does spectrum scarcity continue to justify must-carry regulation, or has it been substantially resolved by broadband, streaming, and over-the-air digital transmission?',
    'Technical analysis of current spectrum utilization and capacity; documentation of how much content distribution now happens off spectrum (broadband, wired cable, satellite). Survey of whether spectrum scarcity constrains actual content availability or is a vestigial regulatory justification.',
    'If scarcity persists: must-carry is natural law (mountain). If scarcity has been resolved: must-carry is historical artifact maintained through regulatory inertia (piton or snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spectrum_scarcity_persistence, empirical, 'Whether spectrum scarcity justification for must-carry remains valid').

omega_variable(
    independent_channel_survival_mechanism,
    'Can independent channels survive and reach audiences through non-cable distribution (streaming, OTA, YouTube, FAST channels) or does cable carriage remain essential for economic viability?',
    'Longitudinal study of independent channel economics: revenue sources, audience reach, carriage impact on survival rates. Comparison of channels with strong cable carriage vs. those relying on alternative distribution.',
    'If alternative distribution is viable: must-carry protects a now-obsolete model; independent channels should exit cable and build direct audiences (tangled rope classification confirmed). If cable carriage is still essential: must-carry is necessary coordination despite extraction dynamics (snare classification may be overstated).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independent_channel_survival_mechanism, empirical, 'Whether independent channels have viable exit pathways from cable dependence').

omega_variable(
    regulatory_capture_quantification,
    'To what extent does the must-carry rule benefit cable operators through suppression of streaming competition versus benefiting broadcast stations through carriage protection?',
    'Policy analysis of how must-carry interacts with broadband regulation, net neutrality, and cable vertical integration. Economic analysis of carriage denial rates and negotiating power asymmetries. Historical analysis of FCC decisions on must-carry waiver requests.',
    'If cable operators are primary beneficiaries: constraint is snare for independent channels, rope for cable operators (current analysis). If broadcast protection is primary: constraint is rope across all perspectives with minimal extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_quantification, empirical, 'Whether must-carry primarily protects broadcast or suppresses streaming competition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(broadcast_must_carry_regulatory_arbitrage, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bmca_tr_t0, broadcast_must_carry_regulatory_arbitrage, theater_ratio, 0, 0.38).
narrative_ontology:measurement(bmca_tr_t7, broadcast_must_carry_regulatory_arbitrage, theater_ratio, 7, 0.48).
narrative_ontology:measurement(bmca_tr_t14, broadcast_must_carry_regulatory_arbitrage, theater_ratio, 14, 0.55).

% Extraction over time
narrative_ontology:measurement(bmca_be_t0, broadcast_must_carry_regulatory_arbitrage, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bmca_be_t7, broadcast_must_carry_regulatory_arbitrage, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(bmca_be_t14, broadcast_must_carry_regulatory_arbitrage, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(broadcast_must_carry_regulatory_arbitrage, resource_allocation).
narrative_ontology:affects_constraint(broadcast_must_carry_regulatory_arbitrage, cable_bundling_extraction).
narrative_ontology:affects_constraint(broadcast_must_carry_regulatory_arbitrage, streaming_platform_negotiating_power).
narrative_ontology:affects_constraint(broadcast_must_carry_regulatory_arbitrage, broadband_net_neutrality_regulatory_capture).

% DUAL FORMULATION NOTE:
% Broadcast must-carry operates in a constraint family with cable bundling rules and broadband net neutrality regulation. Must-carry affects streaming platforms' bargaining position and incentive to invest in local content. Upstream: cable bundling extracts from consumers and independent channels, making must-carry less effective at achieving its coordination goal. Downstream: net neutrality regulations compete with must-carry as alternative mechanisms for platform gatekeeping prevention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(broadcast_must_carry_regulatory_arbitrage, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
