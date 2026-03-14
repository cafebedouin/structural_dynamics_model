% ============================================================================
% CONSTRAINT STORY: epistemic_commons_degradation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_commons_degradation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: epistemic_commons_degradation
 *   human_readable: Epistemic Commons Degradation: Knowledge Quality Extraction via Incentive Misalignment
 *   domain: epistemology/institutional/information_systems
 *
 * SUMMARY:
 *   The epistemic commons — the shared human knowledge base, expertise
 *   networks, and information infrastructure that enable collective
 *   understanding — faces systematic degradation through misaligned incentive
 *   structures. Attention-optimization algorithms, publish-or-perish academic
 *   metrics, engagement-maximizing content distribution, and
 *   platform-mediated knowledge access create a structural constraint where
 *   the agents who benefit most from the commons (attention-capturing
 *   platforms, sensationalist producers, engagement-optimized researchers)
 *   are those who degrade its quality most effectively. Rigorous, niche, and
 *   long-form scholarship become economically and algorithmically invisible.
 *   The constraint exhibits tangled coordination (knowledge genuinely does
 *   flow through these systems, and algorithms do solve routing problems)
 *   alongside systematic extraction (the flow is biased toward engagement
 *   over accuracy, volume over depth, novelty over verification). From
 *   multiple perspectives — knowledge seekers, identity-locked researchers,
 *   niche scholars, platforms, and peer review systems — the same structural
 *   dynamic appears with different classification types, revealing how
 *   indexical position determines whether the commons is experienced as a
 *   snare, a coordination mechanism, a degraded ritual, or an immutable law.
 *
 * KEY AGENTS:
 *   - Attention-Capturing Agents: Primary beneficiaries (institutional/arbitrage) — exploit engagement incentives; extract attention and visibility during verification lag
 *   - Knowledge Seekers: Primary victims (powerless/trapped) — face contaminated epistemic commons with no exit or filtering capacity
 *   - Academic Researchers: Secondary victims (moderate/identity_locked) — identity-fused with publication metrics that incentivize sensationalism over rigor
 *   - Independent Scholars: Mixed position (moderate/constrained) — benefit from open access, constrained by resource barriers and platform deprioritization
 *   - Niche Knowledge Communities: Organized secondary victims (organized/constrained) — coordinate knowledge within domains while suppressed by engagement algorithms
 *   - Attention Platforms: Beneficiaries (institutional/arbitrage) — algorithms solve routing but extract through engagement optimization
 *   - Peer Review System: Institutional actor (institutional/arbitrage) — maintains performative verification; original function degraded by scale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_commons_degradation, 0.58).
domain_priors:suppression_score(epistemic_commons_degradation, 0.62).
domain_priors:theater_ratio(epistemic_commons_degradation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_commons_degradation, extractiveness, 0.58).
narrative_ontology:constraint_metric(epistemic_commons_degradation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(epistemic_commons_degradation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_commons_degradation, tangled_rope).
narrative_ontology:human_readable(epistemic_commons_degradation, "Epistemic Commons Degradation: Knowledge Quality Extraction via Incentive Misalignment").
narrative_ontology:topic_domain(epistemic_commons_degradation, "epistemology/institutional/information_systems").

domain_priors:requires_active_enforcement(epistemic_commons_degradation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_commons_degradation, attention_capturing_agents).
narrative_ontology:constraint_beneficiary(epistemic_commons_degradation, sensationalist_information_producers).
narrative_ontology:constraint_beneficiary(epistemic_commons_degradation, engagement_optimized_platforms).
narrative_ontology:constraint_victim(epistemic_commons_degradation, knowledge_quality).
narrative_ontology:constraint_victim(epistemic_commons_degradation, epistemic_reliability).
narrative_ontology:constraint_victim(epistemic_commons_degradation, long_form_scholarship).
narrative_ontology:constraint_victim(epistemic_commons_degradation, niche_knowledge_domains).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE KNOWLEDGE SEEKER (SNARE) — Information consumers facing algorithmic attention optimization have no meaningful exit. The epistemic commons is contaminated with low-effort, high-engagement content. Trapped in the commons with no ability to filter or escape extraction of attention and cognitive resources. Maximum extraction experienced — cannot opt out of the compromised information environment.
constraint_indexing:constraint_classification(epistemic_commons_degradation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE ACADEMIC RESEARCHER — Identity-locked within academia's metric-driven reward system. Structurally mobile (could leave academia) but identity constituted through publication, citation, and institutional prestige. The constraint forces publication strategy aligned with engagement rather than rigor. Identity lock prevents exit even as structural barriers lower — researcher's self-concept is fused with the academic identity they would need to abandon. High experienced extraction despite theoretical mobility.
constraint_indexing:constraint_classification(epistemic_commons_degradation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: THE INDEPENDENT SCHOLAR (TANGLED ROPE) — Constrained by resource scarcity and publishing gatekeeping, but also benefits from open-access movements and distributed knowledge-sharing platforms. Experiences both coordination (access to global knowledge) and extraction (exploitation of unpaid labor, content reuse without compensation). Moderate extraction with real agency and some exit options.
constraint_indexing:constraint_classification(epistemic_commons_degradation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ATTENTION PLATFORM (ROPE) — Experiences constraint as pure coordination: algorithms solve the problem of routing content to engaged audiences. Net beneficiary — extraction flows toward platform through engagement metrics, advertising revenue, and network effects. Sees the constraint as a coordination mechanism they have designed and maintain.
constraint_indexing:constraint_classification(epistemic_commons_degradation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THE NICHE KNOWLEDGE COMMUNITY (TANGLED ROPE) — Organized scholars in specialized domains (medieval history, rare plant taxonomy, obscure mathematical proofs) experience genuine coordination (distributed peer review, knowledge preservation) alongside extraction (pressured toward engagement-optimized outputs, deprioritization in algorithmic feeds). Significant suppression from resource scarcity, but organized agents have partial agency.
constraint_indexing:constraint_classification(epistemic_commons_degradation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE PEER REVIEW SYSTEM (PITON) — Degraded institutional process maintained through inertia. Peer review was designed to filter episodic low-quality claims, but now performs theater: evaluating polish and framing while gate-keeping at scale becomes impossible. The system persists because alternatives haven't fully replaced it, not because it functions well. Performative verification divorced from actual knowledge quality assurance.
constraint_indexing:constraint_classification(epistemic_commons_degradation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER (MOUNTAIN) — From civilizational scale, this appears as an immutable feature of human epistemic limits: attention is scarce, knowledge is abundant, and filtering mechanisms are inherently imperfect. The natural law framing: all information systems face the tragedy of the commons. However, the structural data (enforced misalignment, extractive beneficiaries, suppression of alternatives) contradicts the mountain classification — the engine will flag this as false naturalization of contingent institutional design.
constraint_indexing:constraint_classification(epistemic_commons_degradation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_commons_degradation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_commons_degradation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_commons_degradation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_commons_degradation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(epistemic_commons_degradation, TR),
    TR >= 0.70.

:- end_tests(epistemic_commons_degradation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The commons undergoes continuous extraction through multiple mechanisms: algorithmic prioritization of engagement over accuracy, publication metrics incentivizing flashy over rigorous work, and platform control over knowledge visibility. The extraction is not total (genuine knowledge sharing occurs), but it is systematic and structured. The metric reflects a 20-year trajectory from 0.32 (pre-algorithmic information landscapes had lower extractiveness; peer review functioned better at smaller scales) to 0.58 (current state with full algorithmic deployment and metric capture). Suppression (0.62): High. Barriers to exit are substantial: researchers cannot easily leave academia; knowledge seekers cannot opt out of algorithmic feeds; niche scholars cannot easily find audiences outside engagement-optimized platforms. Alternative epistemic infrastructure exists but requires coordination and resources many actors lack. Theater ratio (0.68): High. Peer review, academic publishing, and content moderation perform increasingly theatrical roles: they appear to filter for quality but systematically miss sensationalism and engage in gate-keeping while unable to verify claims at scale. The theatrical increase (0.42→0.68) reflects that the volume of claims far exceeds verification capacity, forcing review systems into performative modes.
 *
 * PERSPECTIVAL GAP:
 *   Knowledge seekers experience snare (trapped, powerless) due to algorithmic commons contamination with no filtering option. Researchers experience snare/tangled_rope ambiguously depending on whether identity lock (snare) or career opportunity (tangled_rope) dominates their perception. Platforms experience pure rope — the constraint is their own coordination mechanism. Niche scholars see tangled_rope — genuine benefits (shared knowledge infrastructure) with suppression. The peer review system sees piton — its own function has degraded but is maintained by institutional inertia. The analytical observer risks naturalizing the whole dynamic as immutable feature of information scarcity (mountain), but the measurable theater_ratio increase (0.42→0.68) and extractiveness trajectory (0.32→0.58) over 20 years show the degradation is contingent on recent institutional design changes, not structural limits.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality is derived from their structural position: beneficiary platforms derive low d from arbitrage exit + beneficiary status (d≈0.15→f(d)≈-0.01); trapped knowledge seekers derive high d from trapped exit + victim status (d≈0.95→f(d)≈1.42); identity-locked researchers derive very high d from identity-locked exit + victim status (d≈0.89→f(d)≈1.28, higher than merely constrained because identity lock prevents perceiving alternatives); constrained scholars derive moderate d from constrained exit + mixed status (d≈0.65→f(d)≈1.00); organized niche communities derive lower d from organized/constrained + victim status (d≈0.45→f(d)≈0.40, lowered by organization); peer review institutional derive low d from arbitrage + beneficiary-ish status (d≈0.20→f(d)≈0.02, slightly higher than platforms due to institutional constraints). The analytical observer derives standard analytical d (d≈0.73→f(d)≈1.15).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates how indexical classification prevents mandatrophy (false conflation of types). The constraint is simultaneously: (1) a snare from the knowledge seeker perspective (trapped, no alternatives); (2) a tangled rope from the scholar perspective (genuine knowledge coordination mixed with exploitation); (3) a rope from the platform perspective (pure coordination they designed); (4) a piton from the peer review perspective (degraded institutional ritual). These are not competing claims about which type is 'really' correct — they are all true descriptions of how different agents experience the same structural constraint. The mandatrophy is resolved by recognizing that the constraint has no context-independent type. The indexical tuple (P,T,E,S) fully determines classification from that perspective. The engine's job is not to resolve these to a single 'true' type but to map the perspectival space and identify where the gaps reveal structural features (in this case, the extraction gradient from beneficiary platforms to trapped knowledge seekers).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attention_scarcity_inherent,
    'Is epistemic commons degradation inherent to attention scarcity or engineered through algorithmic incentive structures?',
    'Comparative analysis of pre-algorithmic information environments (academic journals, libraries, conferences) against algorithmic feeds. Historical trend of degradation timing relative to algorithm deployment.',
    'If inherent: mountain classification correct, degradation is unavoidable. If engineered: snare/tangled_rope classification correct, the constraint is contingent and potentially reversible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attention_scarcity_inherent, empirical, 'Whether degradation is structural or engineered').

omega_variable(
    identity_lock_specificity,
    'Are academics identity-locked or merely constrained by career-risk calculus? Is the binding cognitive/identity-based or material/economic?',
    'Post-exit interviews with researchers who left academia; analysis of researchers who maintained scholarly identities outside institutional frameworks; longitudinal identity tracking of academics who shift research topics for career advancement.',
    'If identity-locked: the classification gap from trapped/constrained matters — the agent cannot perceive exit even when barriers lower. If merely constrained: higher power score (moderate → powerful) changes the perspectival gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_specificity, empirical, 'Whether academic identity lock is cognitive or material').

omega_variable(
    niche_knowledge_sustainability,
    'Can niche knowledge communities sustain themselves outside engagement-optimized platforms, or do they depend on subsidies from high-engagement content?',
    'Viability analysis of niche knowledge platforms (ArXiv preprints, specialized wikis, domain-specific repositories) without algorithmic amplification. Cost structure and funding models.',
    'If sustainable independently: scaffold perspective is correct, sunset through migration is possible. If dependent on cross-subsidies: extraction continues even in alternatives, snare classification persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(niche_knowledge_sustainability, empirical, 'Whether niche knowledge can sustain independently').

omega_variable(
    peer_review_functional_floor,
    'Below what scale does peer review become functionally impossible due to volume, and is this limit absolute or contingent on reviewer incentive structure?',
    'Historical analysis of peer review effectiveness at different scales; comparative effectiveness data for different reviewer compensation and incentive models.',
    'If absolute limit: piton classification is correct — the system is degraded but irreplaceable. If contingent: alternative incentive structures could restore function, enabling rope or scaffold classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peer_review_functional_floor, empirical, 'Whether peer review volume limits are absolute or contingent').

omega_variable(
    algorithmic_sortation_necessity,
    'Do engagement-optimized algorithms solve a genuine coordination problem (routing relevant knowledge) or primarily enable extraction (concentrating attention)?',
    'Comparative effectiveness: engagement-optimized algorithms vs. alternative sorting mechanisms (human curation, decentralized peer recommendation, semantic search, citation networks).',
    'If genuine coordination: the constraint contains real rope function despite extraction overlay — tangled_rope classification validated. If primarily extractive: the coordination claim is cover story, snare/piton classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_sortation_necessity, empirical, 'Whether algorithmic sortation solves coordination or enables extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_commons_degradation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epcom_tr_t0, epistemic_commons_degradation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(epcom_tr_t10, epistemic_commons_degradation, theater_ratio, 10, 0.55).
narrative_ontology:measurement(epcom_tr_t20, epistemic_commons_degradation, theater_ratio, 20, 0.68).
narrative_ontology:measurement(epcom_tr_t5, epistemic_commons_degradation, theater_ratio, 5, 0.48).
narrative_ontology:measurement(epcom_tr_t15, epistemic_commons_degradation, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(epcom_be_t0, epistemic_commons_degradation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(epcom_be_t10, epistemic_commons_degradation, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(epcom_be_t20, epistemic_commons_degradation, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(epcom_be_t5, epistemic_commons_degradation, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(epcom_be_t15, epistemic_commons_degradation, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_commons_degradation, information_standard).
narrative_ontology:boltzmann_floor_override(epistemic_commons_degradation, 0.12).
narrative_ontology:affects_constraint(epistemic_commons_degradation, academic_metric_gaming).
narrative_ontology:affects_constraint(epistemic_commons_degradation, platform_recommendation_opacity).
narrative_ontology:affects_constraint(epistemic_commons_degradation, peer_review_bottleneck).

% DUAL FORMULATION NOTE:
% Epistemic commons degradation is a structural phenomenon that decomposes into multiple linked constraints with distinct ε values: platform algorithmic bias (ε≈0.45, snare), academic publishing metrics (ε≈0.52, snare/tangled_rope), peer review theater (ε≈0.38, piton), and niche knowledge suppression (ε≈0.55, tangled_rope). The present story represents the aggregate constraint across all mechanisms. Downstream analysis should decompose by domain and institutional actor.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(epistemic_commons_degradation, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
