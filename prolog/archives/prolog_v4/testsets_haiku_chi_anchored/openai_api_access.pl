% ============================================================================
% CONSTRAINT STORY: openai_api_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_openai_api_access, []).

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
 *   constraint_id: openai_api_access
 *   human_readable: OpenAI API Access Controls
 *   domain: technological/economic
 *
 * SUMMARY:
 *   OpenAI's API access controls create a structural constraint on
 *   developers, platform providers, and end users who depend on frontier
 *   large language models. The constraint operates through managed
 *   authentication, rate limiting, pricing mechanisms, and unilateral
 *   terms-of-service updates. OpenAI frames this as coordination (ensuring
 *   fair access, preventing abuse, maintaining service quality); developers
 *   experience it as extraction (pricing power, switching costs, feature
 *   deprecation risk). The constraint exhibits characteristics of both pure
 *   extraction (Snare) and hybrid coordination-extraction (Tangled Rope)
 *   depending on the developer's organizational power and exit options. The
 *   theater_ratio (0.45) reflects that while API terms are published
 *   transparently, the actual mechanisms of rate limiting, pricing
 *   discrimination, and access denial operate partly through opaque
 *   algorithmic rules and customer support discretion. The extractiveness
 *   trajectory (0.35 → 0.58 over the interval) shows increasing
 *   rent-extraction as OpenAI's monopoly position strengthens through network
 *   effects, as open-source alternatives remain inferior, and as switching
 *   costs increase with broader API adoption.
 *
 * KEY AGENTS:
 *   - OpenAI Corporate: Primary beneficiary (institutional/arbitrage) — extracts rents through API pricing, rate limiting, feature control; frames access control as coordination mechanism
 *   - Dependent Developers: Primary victims (powerless/trapped) — bound by switching costs, lack alternatives at feature parity; experience unilateral price/access changes
 *   - End Users of Dependent Applications: Secondary victims (powerless/trapped) — no direct relationship with OpenAI; dependent on application providers' continued relationship and ability to pay
 *   - Enterprise Platform Providers: Secondary victims (moderate/constrained) — have negotiating power but constrained by OpenAI's unilateral control of frontier models; benefit from access but vulnerable to terms changes
 *   - Open Source AI Coalition: Organized alternatives (organized/constrained) — Meta (Llama), Hugging Face, academic researchers building alternatives; constrained by inferior models but have exit pathway through ecosystem development
 *   - Regulatory Intervention Actors: Competition authorities (organized/mobile) — EU Digital Markets Act, US FTC investigating market concentration; have agency through regulatory action; see sunset mechanism in interoperability mandates
 *   - Analytical Observer: Civilizational view (analytical/analytical) — observes that coordination justification (managing scarce compute) has weakened as hardware becomes available; identifies increasing extractive character
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openai_api_access, 0.58).
domain_priors:suppression_score(openai_api_access, 0.68).
domain_priors:theater_ratio(openai_api_access, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openai_api_access, extractiveness, 0.58).
narrative_ontology:constraint_metric(openai_api_access, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(openai_api_access, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(openai_api_access, tangled_rope).
narrative_ontology:human_readable(openai_api_access, "OpenAI API Access Controls").
narrative_ontology:topic_domain(openai_api_access, "technological/economic").

domain_priors:requires_active_enforcement(openai_api_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(openai_api_access, openai_corporate).
narrative_ontology:constraint_victim(openai_api_access, dependent_developers).
narrative_ontology:constraint_victim(openai_api_access, api_application_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT DEVELOPER (SNARE) — A developer who has built production systems on OpenAI's API faces high switching costs and no viable alternative at equivalent capability/cost. Termination of API access, rate limiting, pricing changes, or feature deprecation cannot be resisted. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96. Pure extraction from this position.
constraint_indexing:constraint_classification(openai_api_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: END USER (SNARE) — Users of applications built on OpenAI API have no direct negotiating power with OpenAI. Their access depends on their application provider's continued relationship with OpenAI and ability to pay. Service degradation, pricing increases, or API shutdown cascades to them without recourse. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.07. Maximum extraction; powerless + trapped + global scope.
constraint_indexing:constraint_classification(openai_api_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ENTERPRISE PLATFORM PROVIDER (TANGLED ROPE) — Large organizations integrating OpenAI API (Google, Microsoft, Meta-adjacent players) have moderate power: they can negotiate volume discounts, custom integrations, and SLA terms. They also benefit from OpenAI's innovation (coordination benefit of access to frontier models). But they are constrained by OpenAI's unilateral ability to change terms, restrict competitors' access, or deprecate features. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.44. Mixed coordination (access to frontier capability) and extraction (unilateral control).
constraint_indexing:constraint_classification(openai_api_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: OPENAI CORPORATE (ROPE) — Experiences API access control as pure coordination: managing access ensures quality control, billing integrity, abuse prevention, and fair resource allocation among users. The access mechanism solves the collective action problem of allocating scarce compute to competing demands. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Negative effective extraction = net coordinator.
constraint_indexing:constraint_classification(openai_api_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SOURCE AI COALITION (TANGLED ROPE) — Organized actors (Meta's Llama ecosystem, open-source foundations, academic researchers) see OpenAI's API control as both a competitive constraint AND a coordination failure that motivates alternative infrastructure. They benefit from using OpenAI models (integration advantage) while being constrained by closed access. The organization has agency: they build competing systems (Llama, LLaMA-2) that reduce dependence. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.45. Symmetric coordination-extraction mix; organized agents have exit pathway via alternatives.
constraint_indexing:constraint_classification(openai_api_access, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY INTERVENTION ACTOR (SCAFFOLD) — Competition regulators, privacy frameworks (EU AI Act), and interoperability mandates see OpenAI API control as a temporary coordination problem with a sunset. Regulatory action, interoperability requirements, or antitrust enforcement could break lock-in. These actors have agency and a clear path (regulation). d≈0.35, f(d)≈0.32, σ=1.1 → χ≈0.18. Low extraction because regulatory actors see a sunset mechanism.
constraint_indexing:constraint_classification(openai_api_access, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, OpenAI API control solves the genuine coordination problem of allocating frontier AI capability (verification, safety monitoring, compute fairness) AND extracts rents through monopolistic pricing, feature deprecation threats, and switching costs. Neither extraction nor coordination dominates; both are structural. d≈0.60, f(d)≈0.85, σ=1.2 → χ≈0.59. Confirms tangled_rope claim.
constraint_indexing:constraint_classification(openai_api_access, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openai_api_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(openai_api_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openai_api_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(openai_api_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(openai_api_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. OpenAI extracts rents through (1) premium pricing relative to marginal compute cost, (2) rate limiting that forces tiered spending, (3) feature deprecation and planned obsolescence (retiring older models), and (4) lock-in through switching costs. However, extraction is not maximal (would be 0.80+) because: open alternatives exist (Llama-2, Mistral, Claude), no single vendor has forced monopoly yet (though OpenAI approaches it), and some developers have exit capacity through multi-model architectures. The trajectory (0.35 → 0.58) reflects OpenAI's increasing market power as models improve and network effects strengthen. Suppression (0.68): High. Barriers to exit include: (a) technical lock-in (retraining models on new vendor, refactoring integrations), (b) capability lock-in (no open alternative matches GPT-4 on frontier tasks), (c) economic barriers (switching costs, learning curve for new APIs), and (d) psychological switching costs (developers optimized workflows for OpenAI's interface). Open alternatives (Llama, Mistral) reduce suppression somewhat — they prove alternatives are possible — but suppression remains high because parity is incomplete. Theater ratio (0.45): Moderate. API terms are published transparently, pricing is formulaic, and rate limits are disclosed. But the mechanism contains opaque elements: algorithmic rate-limit enforcement, customer support discretion in access disputes, and feature deprecation timelines announced with limited notice. The low-to-moderate theater distinguishes this from pure coordination (would be ~0.2) and from degraded institutional theater (would be ~0.7). The trajectory (0.30 → 0.45) shows increasing performative content as OpenAI adds more 'safety' and 'responsible use' language to justify access restrictions that are primarily economically motivated.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a stark perspectival gap between OpenAI's self-perception and dependent developers' experience. OpenAI sees Rope (coordination mechanism for managing scarce compute and preventing abuse). Dependent developers see Snare (extraction without meaningful alternatives). Enterprise providers see Tangled Rope (coordination benefits from frontier access, extraction from lock-in). Open source coalitions see Scaffold (temporary constraint with sunset via alternative models). Regulatory observers see Tangled Rope → Scaffold (extraction now, but regulatory interoperability mandates coming). The analytical observer sees Tangled Rope (genuine coordination problem overlaid with extractive rent-seeking, and the coordination justification weakening as hardware availability increases). The gap arises because OpenAI's claim of coordination rests on 'scarce compute' — but compute scarcity is partly artificial (maintained through rate limiting and pricing) rather than fundamental. If hardware were truly scarce, the Rope classification would be stable; instead, the trajectory shows increasing extractiveness, indicating that scarcity is being maintained strategically.
 *
 * DIRECTIONALITY LOGIC:
 *   Dependent developers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction (powerless and unable to exit). End users: Victim + trapped → d≈0.95, f(d)≈1.42. Even higher; transitive victim (victim of victim). Enterprise providers: Victim + constrained (negotiating capacity exists, but exit requires building/switching) → d≈0.55, f(d)≈0.75. Moderate extraction. OpenAI: Beneficiary + arbitrage (unilateral control, can redirect compute freely) → d≈0.08, f(d)≈-0.10. Net beneficiary (negative effective extraction = controller, not target). Open source coalition: Victim initially (trapped by inferior alternatives) + constrained (building alternatives but not yet parity) → d≈0.50, f(d)≈0.65. Symmetric; has agency but not escape velocity. Regulatory actors: Victim of the victim (constrained by market power, not by API control directly) + mobile (regulatory action available) → d≈0.35, f(d)≈0.32. Low extraction from their perspective; they have agency. The directionality chain shows why Snare appears from the most vulnerable positions and why Tangled Rope appears at moderate and organized levels.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The classification as Tangled Rope (not pure Snare) depends on recognizing the genuine coordination function that OpenAI provides ALONGSIDE the extraction. If we classified this as pure Snare, we would be denying that frontier LLM access is valuable coordination — that access to GPT-4 does solve real problems (inference speed, model quality, reliability) that developers could not solve independently. But by stating this as Tangled Rope, we acknowledge: (1) OpenAI provides real coordination value (frontier model access), (2) but layers extractive rent-seeking on top (pricing, lock-in, unilateral terms), (3) and the extraction mechanism depends on suppression (lack of alternatives) and is enforced (rate limiting, access denial). The mandatrophy is resolved by measuring: beneficiaries (OpenAI) — yes, clear rent extraction. Victims (developers, users) — yes, clear lock-in and switching costs. Active enforcement (rate limiting, authentication, feature deprecation) — yes, unilateral and ongoing. A pure Snare classification would erase the coordination. A pure Rope classification would erase the extraction. Tangled Rope holds both: this is a coordination mechanism (managing compute allocation) that has been colonized by extractive rent-seeking (pricing power, lock-in, artificial scarcity maintenance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_capability_parity,
    'Do open-source and alternative commercial models achieve functional parity with OpenAI''s frontier models on economically relevant tasks, enabling genuine developer optionality?',
    'Benchmark comparisons of frontier models (GPT-4 vs Llama-2, Mistral, Claude) on production AI application tasks; latency/cost/capability efficiency trade-offs; developer migration patterns post-alternative releases',
    'If parity achieved: constraint shifts from Snare → Tangled Rope; developers have exit (mobile/constrained). If parity fails: Snare classification confirmed; no real alternative exists despite open-source proliferation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_capability_parity, empirical, 'Whether open alternatives provide true functional parity with frontier models').

omega_variable(
    regulatory_interoperability_mandate,
    'Will competition regulators or AI safety frameworks impose interoperability/portability requirements that break OpenAI''s lock-in?',
    'Tracking of EU Digital Markets Act application, US FTC actions on AI market dominance, technical feasibility of portability standards (e.g., API compatibility layers)',
    'If mandated: sunset clause confirmed (Scaffold perspective valid). If not mandated: API control persists as extraction mechanism; Snare classification for developers remains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_interoperability_mandate, empirical, 'Whether regulatory frameworks will mandate API interoperability').

omega_variable(
    compute_scarcity_persistence,
    'Is access control a legitimate response to compute scarcity, or has hardware availability eliminated the coordination justification for gating?',
    'Analysis of GPU/TPU availability trends, OpenAI''s actual capacity constraints vs pricing strategy, correlation between capacity announcements and rate-limit changes',
    'If scarcity real: API control is primarily Rope (coordination). If artificial: API control is primarily Snare (extraction). Current data suggests artificial scarcity (compute available, pricing/rate-limiting maintained despite capacity growth).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compute_scarcity_persistence, empirical, 'Whether access control reflects genuine compute scarcity or artificial gating').

omega_variable(
    switching_cost_trap_mechanism,
    'Are switching costs (retraining, integration refactoring, endpoint changes) sufficiently high that they constitute a structural trap, or can developers reasonably port applications?',
    'Developer surveys on cost/time to migrate; technical analysis of API protocol compatibility; tracking of successful multi-model deployment architectures',
    'If high: Snare classification confirmed (exit_options = trapped). If low: exit_options upgrade to constrained/mobile; Tangled Rope becomes more plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_trap_mechanism, empirical, 'Whether API switching costs create structural traps for developers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(openai_api_access, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oai_api_tr_t0, openai_api_access, theater_ratio, 0, 0.3).
narrative_ontology:measurement(oai_api_tr_t3, openai_api_access, theater_ratio, 3, 0.38).
narrative_ontology:measurement(oai_api_tr_t6, openai_api_access, theater_ratio, 6, 0.45).

% Extraction over time
narrative_ontology:measurement(oai_api_be_t0, openai_api_access, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(oai_api_be_t3, openai_api_access, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(oai_api_be_t6, openai_api_access, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(openai_api_access, resource_allocation).
narrative_ontology:affects_constraint(openai_api_access, llm_market_concentration).
narrative_ontology:affects_constraint(openai_api_access, ai_capability_safety_regulation).

% DUAL FORMULATION NOTE:
% OpenAI API access control is downstream of LLM market concentration (fewer frontier models available to users) and upstream of specific harms (pricing barriers to AI capability access, lock-in of dependent systems). The constraint family decomposes as: (1) LLM market concentration (ε≈0.30, Rope/Tangled Rope) — structural fact that few orgs can train frontier models, (2) OpenAI API control (ε≈0.58, Tangled Rope) — rental extraction via API gating, (3) AI regulation (ε≈0.45, Scaffold) — regulatory response creating sunset mechanism. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(openai_api_access, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
