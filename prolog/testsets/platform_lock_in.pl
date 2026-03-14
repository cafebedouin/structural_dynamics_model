% ============================================================================
% CONSTRAINT STORY: platform_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_lock_in, []).

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
 *   constraint_id: platform_lock_in
 *   human_readable: Platform Lock-In: Network Effects and Extractive Switching Costs
 *   domain: digital_economics/platform_governance
 *
 * SUMMARY:
 *   Platform lock-in represents a foundational constraint in digital-age
 *   economics: the structural coupling between network effects (the value of
 *   a platform increases with user base) and switching costs (the cost to
 *   users of leaving the platform). This constraint is genuinely hybrid —
 *   platform operators perceive network effects as pure coordination (the
 *   mechanism that solves the problem of users needing to reach a shared
 *   space), while locked-in users perceive switching costs as pure extraction
 *   (a mechanism that traps them). Regulators see both simultaneously:
 *   genuine coordination value coupled with exploitable asymmetry. The
 *   constraint has evolved from a technical inevitability ('platforms must
 *   reach monopoly scale to be viable') to an increasingly contested
 *   institutional arrangement as interoperability protocols mature and
 *   regulatory frameworks attempt to decouple network effects from switching
 *   costs. The extractiveness trajectory shows accumulation: from 0.28 at the
 *   constraint's emergence (early 2000s single-platform dominance still
 *   uncertain) to 0.58 in the current period (lock-in fully developed with
 *   multiple competing platforms all enforcing switching costs). Theater
 *   ratio remains moderate (0.48) because some platform value is genuinely
 *   functional (coordination around standards, data storage, real-time
 *   interaction), but much is performative (proprietary algorithms that could
 *   be interoperable, data formats that could be portable, social graphs that
 *   could be federated). The constraint's mandatrophy is resolved through
 *   perspectival decomposition: all six types are legitimate readings from
 *   different structural positions; the question is not 'which is correct'
 *   but 'whose extraction is being measured'.
 *
 * KEY AGENTS:
 *   - Platform Operator (institutional/arbitrage): Primary beneficiary — captures value from network effects and monetizes switching costs through data leverage, attention rent, and complementary service margins.
 *   - Locked-In User (powerless/trapped): Primary victim — invested social capital, data, network connections; bears full extraction when platform raises prices, degrades service, or exploits attention.
 *   - Dependent Service Provider (powerless/trapped): Secondary victim — creators, small businesses, developers whose revenue depends entirely on platform access; no recourse when platform changes terms unilaterally.
 *   - Transitional Competitor (moderate/constrained): Mixed victim-beneficiary — benefits from established platform norms (users understand 'social media' through incumbent education) but faces extraction through network effect disadvantage (users stay with incumbent despite new competitor's superiority).
 *   - Regulatory Coalition (organized/constrained): Countervailing power — governments, consumer advocates, interoperability consortia attempting to reshape lock-in through data portability mandates, API access rights, and forced interoperability.
 *   - Decentralization Movement (organized/constrained): Alternative path developers — protocol advocates, federated platform engineers proposing sunset trajectory through ActivityPub, distributed protocols that eliminate platform singularity.
 *   - Network-Effects Ideology (institutional/arbitrage): Maintenance mechanism — venture capital, business schools, regulatory bodies that frame lock-in as natural law rather than contingent institutional choice, sustaining the constraint through narrative.
 *   - Analytical Observer (analytical/analytical): External position — risks naturalizing lock-in as immutable economic law ('platforms must converge to monopoly') when empirically it is architectural choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_lock_in, 0.58).
domain_priors:suppression_score(platform_lock_in, 0.65).
domain_priors:theater_ratio(platform_lock_in, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_lock_in, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(platform_lock_in, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_lock_in, tangled_rope).
narrative_ontology:human_readable(platform_lock_in, "Platform Lock-In: Network Effects and Extractive Switching Costs").
narrative_ontology:topic_domain(platform_lock_in, "digital_economics/platform_governance").

domain_priors:requires_active_enforcement(platform_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_lock_in, platform_operator).
narrative_ontology:constraint_beneficiary(platform_lock_in, early_adopters_with_switching_buffers).
narrative_ontology:constraint_victim(platform_lock_in, user_ecosystem_dependent).
narrative_ontology:constraint_victim(platform_lock_in, complementary_service_providers).
narrative_ontology:constraint_victim(platform_lock_in, potential_competitors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN USER (SNARE) — User who has invested social capital, data, network connections, and digital assets on the platform. Exit requires abandoning years of accumulated relationships, losing data portability, or forgoing access to critical services. Maximum experienced extraction through switching cost imposition.
constraint_indexing:constraint_classification(platform_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEPENDENT SERVICE PROVIDER (SNARE) — Small business, creator, or developer whose entire revenue stream depends on platform access. Cannot exit without losing primary income source. Platform can unilaterally change terms, derank content, or adjust revenue share. No structural recourse.
constraint_indexing:constraint_classification(platform_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: TRANSITIONAL COMPETITOR (TANGLED ROPE) — New entrant attempting to build alternative platform. Benefits from existing ecosystem norms and user expectations established by incumbent (coordination function — users know how to use a 'social platform' because of prior learning). Also bears extraction through network effect disadvantage: must reach critical mass while users face switching costs that favor staying with incumbent. Active enforcement of incompatibility (API rate-limiting, data portability restrictions) increases extraction component.
constraint_indexing:constraint_classification(platform_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Operator experiences lock-in as pure coordination success: the constraint is precisely the network effect that makes the platform valuable. Can arbitrage this lock-in (migrate users, monetize through data/attention, charge rent on switching costs). Low experienced extraction because the operator designed and controls the mechanism.
constraint_indexing:constraint_classification(platform_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY COALITION (TANGLED ROPE) — Organized government actors, consumer advocates, and interoperability consortia see lock-in as a hybrid: genuine coordination problem (users benefit from network effects, platform provides real services) AND extractive rent-seeking (switching costs transfer user welfare to operator). Regulation (data portability, interoperability mandates, API access rights) imposes active enforcement to break the asymmetry. Moderate extraction because coalitions have agency and can reshape the constraint through policy.
constraint_indexing:constraint_classification(platform_lock_in, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DECENTRALIZATION MOVEMENT (SCAFFOLD) — Protocol advocates, federated platform developers, and blockchain projects propose sunset logic: distributed protocols (ActivityPub, BlueSky protocol stack, Nostr) aim to separate social graph from platform implementation, reducing switching costs to near-zero. Lock-in extraction would vanish if interoperability becomes standard. Scaffold classification reflects genuine sunset path: distributed protocols represent alternative coordination with lower theater if norms shift.
constraint_indexing:constraint_classification(platform_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: NETWORK-EFFECTS IDEOLOGY (PITON) — The 'natural monopoly' framing of platform dominance through network effects has become largely performative at civilizational scale. Lock-in is presented as inevitable technical consequence ('more users = more valuable = all users converge to one platform') when empirically, network effects are contingent on platform design choices: interoperability reduces lock-in severity, data portability enables switching, federation allows parallel networks. The ideology persists through institutional inertia (venture capital, business school teaching, regulatory deference) despite technological alternatives. Theater ratio high because the narrative work maintains the constraint more than actual technical features do.
constraint_indexing:constraint_classification(platform_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — Risk perspective: from a universalist analytical stance, network effects might appear as an irreducible economic law — platforms with larger user bases are inherently more valuable, creating unavoidable lock-in. However, the base properties contradict mountain classification: requires_active_enforcement=true (not emergent), suppression=0.65 (not a low-friction natural law), theater_ratio=0.48 (moderate, suggesting contingent institutional choices, not immutable constraint). This perspective demonstrates false summit: the 'law of network effects' is really a contingent institutional arrangement sustained through enforcement and narrative.
constraint_indexing:constraint_classification(platform_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_lock_in, TR),
    TR >= 0.70.

:- end_tests(platform_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint imposes genuine switching costs (time, data loss, social capital loss, service functionality loss) that the operator monetizes through price increases, attention exploitation, and leverage over complementary service providers. However, the extraction is not maximal (not 0.72+) because: (1) users receive real service value — the coordination function is genuine, not purely extractive theater, (2) competing platforms exist, reducing the absolute monopoly power, and (3) regulatory and technological pressure is beginning to reduce switching costs. The trajectory from 0.28 to 0.58 reflects the accumulation of switching cost mechanisms as platforms mature and lock-in deepens. Suppression (0.65): High. Users face multiple barriers to exit: (a) structural switching costs (data portability, social graph relocation), (b) identity lock (reputation, follower count, content history), (c) network effects (platforms are more valuable with more users, so users converge on incumbent), (d) lack of alternatives (interoperable platforms exist but lack feature parity and user base). Suppression is not total (0.80+) because exit is technically possible and some users do leave successfully, and because regulatory pressure is beginning to reduce barriers. Theater ratio (0.48): Moderate. A meaningful portion of platform value is genuine coordination — matching users with relevant content, enabling real-time communication, storing persistent identity and data. However, platform operators perform substantial theater: algorithmic recommendation systems presented as 'showing you what you want' (actually optimizing for engagement), data collection presented as 'personalization' (actually enabling price discrimination and manipulation), network effects presented as 'inevitable' (actually maintained through enforcement of incompatibility). Theater is neither dominant nor negligible — the platform coordinates real functions while hiding extractive mechanisms behind performative interfaces.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence on a single structural phenomenon. The platform operator sees pure Rope (network effects are coordination, the constraint solves the problem of fragmented attention). The locked-in user sees pure Snare (switching costs are traps with no exit). The transitional competitor sees Tangled Rope (benefits from norms + coordination frameworks, but faces extraction through network effect disadvantage). The regulator sees Tangled Rope (coordinates real services, but extraction is exploitable and remediable). The decentralization movement sees Scaffold (genuine sunset path through interoperability). The 'network effects as law' ideology sees implicit Mountain (lock-in is inevitable). The analytical observer risks naturalizing this into explicit Mountain. The engine detects the false summit by noting that requires_active_enforcement=true (not emergent from nature) and suppression=0.65 (not a frictionless natural law). The perspectival divergence is not relativism — each perspective's classification flows from real structural differences in (power, exit options, time horizon, scope). The locked-in user genuinely experiences maximum extraction; the operator genuinely experiences maximum coordination benefit. Both readings are true from their respective positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent derives from their structural relationship to the extraction flow. Platform operator: beneficiary with arbitrage options (can move capital, migrate users, exit and restart) → low d ≈ 0.10 → f(d) ≈ -0.08 → negative chi (operator nets gain). Locked-in user: victim with trapped exit (years invested, no alternatives) → high d ≈ 0.92 → f(d) ≈ 1.38 → high chi (target experiences maximum extraction). Dependent service provider: victim with trapped exit (revenue entirely dependent) → d ≈ 0.95 → f(d) ≈ 1.42 → maximum chi (small business experiences severe extraction). Transitional competitor: victim with constrained exit (can build alternative but faces network effect disadvantage) AND beneficiary (learns from incumbent's platform design) → d ≈ 0.68 → f(d) ≈ 1.05 → moderate chi (competitor experiences mixed extraction and benefit, unbalanced toward extraction). Regulator: neither beneficiary nor victim, powerful enough to reshape → d ≈ 0.50 → f(d) ≈ 0.65 → moderate chi (regulator experiences the constraint as a problem to solve, not as extraction). Decentralization movement: potential beneficiary (if protocol succeeds) but currently constrained → d ≈ 0.45 → f(d) ≈ 0.55 → moderate chi (movement faces coordination problem, not yet extractive lock-in). The derivation chain reflects the fundamental asymmetry: operators harvest extraction; users bear it.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVING HYBRID CLAIM (TANGLED ROPE): The mandatrophy is resolved by showing that lock-in genuinely contains BOTH a coordination function AND asymmetric extraction. Coordination function: platforms solve a real collective action problem (users need to find each other, share information, converge on shared infrastructure). This function is not fictional — when you switch from Platform A to Platform B, you lose immediate access to the social graph on A, demonstrating that the network was genuinely coordinating. Network effects are real. Asymmetric extraction: the coordination infrastructure is weaponized. Operators enforce incompatibility, restrict data portability, design switching costs into architecture, monetize the lock-in through attention rent and data leverage, and gain pricing power through entrenchment. This extraction is not incidental to the coordination — it is embedded in the design. Regulators' intervention (interoperability mandates, API access rights) aims to preserve the coordination function while removing the extraction mechanism: keep the platforms, break the lock-in. The classification as Tangled Rope is confirmed by: (1) beneficiaries declared (platform_operator, early_adopters_with_switching_buffers) — genuine benefits exist, (2) victims declared (user_ecosystem_dependent, complementary_service_providers) — genuine extraction exists, (3) requires_active_enforcement=true — the operator must actively enforce incompatibility to maintain the lock-in; it doesn't emerge naturally, (4) suppression=0.65 and extractiveness=0.58 — both high enough to be extractive, but not so high as to be pure Snare (which would require suppression ≥ 0.60 and extractiveness ≥ 0.46 with NO beneficial functions). The mandatrophy trap is avoided by refusing to collapse the hybrid into either pure type. Lock-in is not 'actually just coordination' (beneficiary myth) and not 'actually just extraction' (victim myth). It is genuinely both — the analytical task is to measure the proportion and track how policy and technology reshape the mix.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effect_contingency,
    'Are network effects technically immutable (a law of platform economics) or contingent on platform design choices (interoperability, data portability, federation)?',
    'Cross-platform comparison: platforms with interoperability features (Signal/WhatsApp cross-messaging, fediverse instances) vs closed platforms (Meta ecosystem). Measurement of switching cost elasticity with respect to portability features. Historical analysis of when network effects were broken (MySpace to Facebook, Twitter alternatives during Musk transition).',
    'If immutable: lock-in is mountain-like, regulation cannot reduce extraction. If contingent: lock-in is institutional arrangement, interoperability policy can reshape the constraint to Rope or reduce it to Scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effect_contingency, empirical, 'Whether network effects are technical law or institutional choice').

omega_variable(
    switching_cost_measurement,
    'What is the true switching cost distribution across user populations? Are costs genuinely insurmountable (trapped) or high but surmountable (constrained)?',
    'User survey and behavioral analysis: proportion of users who have attempted exit and succeeded vs failed; cost breakdown (data loss, social capital loss, service dependency). Competitor platform adoption rates when switching costs are systematically reduced (e.g., DataPortability API, federation enablement).',
    'If predominantly trapped: powerless perspective Snare is structural. If predominantly constrained: moderate perspectives Tangled Rope is more accurate, policy intervention can shift distribution toward mobile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_measurement, empirical, 'Proportion of users with surmountable vs insurmountable switching costs').

omega_variable(
    interoperability_ceiling,
    'What proportion of lock-in extraction can interoperability protocols actually eliminate? Is federation sufficient to break the lock or does core platform value remain singular?',
    'Technical comparison of interoperable protocol architectures (ActivityPub, ATproto) with incumbent platforms on user experience, feature parity, and adoption trajectories. Measurement of whether network effects persist in federated systems (do users prefer larger instances, or does federation eliminate scaling advantage?).',
    'If interoperability eliminates >70% of extraction: scaffold sunset is real and ambitious. If eliminates <30%: some lock-in is genuinely coordination function, scaffold is aspirational. If eliminates 30-70%: tangled rope structure remains even with interoperability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_ceiling, empirical, 'Proportion of lock-in extraction that interoperability can eliminate').

omega_variable(
    regulatory_capture_risk,
    'Can regulatory intervention targeting lock-in (interoperability mandates, data portability) avoid regulatory capture by incumbent platforms (via lobbying, technical standards setting, complexity)?',
    'Case studies of existing regulations (GDPR right to data portability, EU Digital Markets Act enforcement). Measurement of compliance cost and loophole prevalence. Analysis of lobbying expenditure and regulatory capture indicators in standards-setting bodies (W3C, IETF) where incumbent platforms are dominant participants.',
    'If capture is high-probability: regulatory perspective becomes constrained instead of powerful, cannot reshape lock-in substantially. If capture is avoidable: regulatory coalition perspective holds, policy intervention can shift constraint toward Rope or Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_risk, empirical, 'Susceptibility of regulatory intervention to incumbent capture').

omega_variable(
    identity_lock_component,
    'To what extent is lock-in due to identity fusion with platform (users define themselves through their presence/influence on the platform) versus structural data/social loss?',
    'User interviews and behavioral analysis: users who cite identity/reputation as primary barrier to exit vs those citing data loss or service functionality. Measurement of whether users maintain identity markers (usernames, follower counts, content archives) as part of self-concept. Analysis of whether identity-fused users show higher switching resistance even when offered frictionless data export.',
    'If identity lock is dominant: exit_options should be identity_locked for many powerless agents, not just trapped. Constraint becomes partially self-reinforcing through internalized frames, not just external switching costs. Interoperability policy alone insufficient; must address identity portability and reputation transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_component, empirical, 'Whether lock-in is structural or identity-based').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plock_tr_t0, platform_lock_in, theater_ratio, 0, 0.32).
narrative_ontology:measurement(plock_tr_t3, platform_lock_in, theater_ratio, 3, 0.4).
narrative_ontology:measurement(plock_tr_t6, platform_lock_in, theater_ratio, 6, 0.46).
narrative_ontology:measurement(plock_tr_t10, platform_lock_in, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(plock_be_t0, platform_lock_in, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(plock_be_t3, platform_lock_in, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(plock_be_t6, platform_lock_in, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(plock_be_t10, platform_lock_in, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_lock_in, resource_allocation).
narrative_ontology:affects_constraint(platform_lock_in, data_portability_resistance).
narrative_ontology:affects_constraint(platform_lock_in, interoperability_incompatibility).
narrative_ontology:affects_constraint(platform_lock_in, algorithmic_content_curation_opacity).

% DUAL FORMULATION NOTE:
% Platform lock-in decomposes into three structurally distinct constraints: (1) data_portability_resistance (ε=0.35, Rope with enforcement) — the technical and legal barriers to exporting user data, (2) interoperability_incompatibility (ε=0.42, Tangled Rope) — the active enforcement of API rate-limiting and protocol incompatibility, (3) algorithmic_content_curation_opacity (ε=0.48, Piton/Snare) — the performative presentation of algorithmic recommendation as objective curation when it is actually attention extraction. Platform lock-in as a whole (ε=0.58) is the hybrid result of these three constraints operating in concert. Each has different exit pathways and different regulatory solutions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
