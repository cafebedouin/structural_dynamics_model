% ============================================================================
% CONSTRAINT STORY: attention_allocation_problem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_allocation_problem, []).

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
 *   constraint_id: attention_allocation_problem
 *   human_readable: Attention Allocation Problem: Coordination vs. Extraction in Scarce Cognitive Resources
 *   domain: cognitive/economic/social
 *
 * SUMMARY:
 *   The attention allocation problem emerges at the intersection of cognitive
 *   scarcity (no mind can consciously process all available information),
 *   technological capacity (digital systems enable exponential information
 *   generation), and economic incentives (surveillance capitalism monetizes
 *   user attention). This constraint exhibits structural transformation
 *   across its timeline: it began as genuine coordination problem (matching
 *   attention to relevant content) but has evolved into extraction mechanism
 *   (optimizing for engagement metrics rather than user value). The
 *   constraint demonstrates all major classification types depending on
 *   observer position. From the individual allocator's perspective, it is a
 *   snare: systematic capture through algorithmic ranking, notification
 *   design, and variable reward schedules with no exit option. From the
 *   platform operator's perspective, it is a rope: purely a coordination
 *   problem of matching supply and demand. From the institutional employer's
 *   perspective, it is tangled rope: genuine coordination need for internal
 *   communication alongside extraction through reduced employee focus
 *   capacity. The attention metrics/measurement system is piton: the
 *   apparatus of engagement metrics persists through institutional momentum
 *   despite well-documented failure (Goodhart's law). The digital minimalism
 *   movement sees it as scaffold: decentralized attention management
 *   protocols with explicit sunset logic. The analytical observer risks
 *   seeing it as mountain (natural law of cognition) but this naturalizes
 *   what is actually a contingent institutional design. Theater ratio has
 *   increased over the measurement interval (0.35 → 0.65) as
 *   attention-capture mechanisms have become increasingly sophisticated and
 *   measurement has become increasingly decoupled from actual user value.
 *
 * KEY AGENTS:
 *   - Individual Attention-Allocators: Primary victims (powerless/trapped) — bear full extraction cost of algorithmic capture without meaningful exit option
 *   - Attention Capturers (platforms, advertisers): Primary beneficiaries (institutional/arbitrage) — monetize user attention without consuming own attention allocation
 *   - Professional Knowledge Workers: Mixed position (moderate/constrained) — benefit from coordination (shared tools, communication) but victimized by extraction (constant interruption, attentional fragmentation)
 *   - Institutional Employers: Mixed position (organized/constrained) — need coordination infrastructure but bear productivity costs of attention extraction
 *   - Platform Operators: Institutional beneficiaries (institutional/arbitrage) — systematically advantaged by monopoly over attention allocation mechanisms
 *   - Attention Rights Movement: Organized challengers (organized/constrained) — building alternative architectures with sunset clauses
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent design as immutable law of cognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_allocation_problem, 0.58).
domain_priors:suppression_score(attention_allocation_problem, 0.62).
domain_priors:theater_ratio(attention_allocation_problem, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_allocation_problem, extractiveness, 0.58).
narrative_ontology:constraint_metric(attention_allocation_problem, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(attention_allocation_problem, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_allocation_problem, tangled_rope).
narrative_ontology:human_readable(attention_allocation_problem, "Attention Allocation Problem: Coordination vs. Extraction in Scarce Cognitive Resources").
narrative_ontology:topic_domain(attention_allocation_problem, "cognitive/economic/social").

domain_priors:requires_active_enforcement(attention_allocation_problem).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attention_allocation_problem, attention_capturers).
narrative_ontology:constraint_beneficiary(attention_allocation_problem, platform_operators).
narrative_ontology:constraint_beneficiary(attention_allocation_problem, advertisers).
narrative_ontology:constraint_victim(attention_allocation_problem, attention_allocators).
narrative_ontology:constraint_victim(attention_allocation_problem, cognitive_autonomy).
narrative_ontology:constraint_victim(attention_allocation_problem, distributed_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL ATTENTION-ALLOCATOR (SNARE) — Faces systematic capture mechanisms (algorithmic feed curation, notification design, variable reward schedules) with minimal exit options. Cannot opt out of attention demands without social/economic cost. Maximum extraction from powerless position — individual cognitive capacity is harvested for platform/advertiser benefit. Suppression operates through design (dark patterns, infinite scroll) and social obligation.
constraint_indexing:constraint_classification(attention_allocation_problem, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PROFESSIONAL KNOWLEDGE WORKER (TANGLED ROPE) — Experiences genuine coordination function (email enables team communication, shared platforms reduce friction) alongside extraction (constant interruption reduces deep work capacity, attentional tax on information filtering). Exit is costly but possible (can adopt analog workflows, negotiate reduced notification policies). Mixed extraction and coordination benefit — some agency but significant constraint.
constraint_indexing:constraint_classification(attention_allocation_problem, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences attention allocation as pure coordination problem: users need to find relevant content, creators need audiences, advertisers need visibility. Net beneficiary through arbitrage (can monetize attention without consuming their own allocation). Sees the constraint as solving collective action problem of matching attention to content. Extraction flows toward platform — they are systematically advantaged.
constraint_indexing:constraint_classification(attention_allocation_problem, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL EMPLOYER (TANGLED ROPE) — Needs attention coordination (scheduling, information distribution) but bears extraction cost (reduced employee cognitive capacity, attentional fragmentation reducing productivity). Organized enough to implement policies (focus time, notification restrictions) but constrained by industry norms and competitive pressure to stay connected. Both benefits from and is victimized by attention allocation mechanisms.
constraint_indexing:constraint_classification(attention_allocation_problem, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ATTENTION METRICS/ANALYTICS (PITON) — The industrial apparatus of attention measurement (engagement metrics, dwell time analytics, conversion tracking) is largely performative theater: metrics optimize for what is measurable rather than what matters (time-on-page != understanding, clicks != value). The measurement system persists through institutional inertia despite well-documented failures (Goodhart's law, metric gaming). Theater ratio high because actual attention allocation effectiveness is much lower than metric values suggest. The system maintains itself through momentum, not functional necessity.
constraint_indexing:constraint_classification(attention_allocation_problem, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DIGITAL MINIMALISM / ATTENTION RIGHTS MOVEMENT (SCAFFOLD) — Organized agents building alternative attention allocation architectures (focus modes, time-limit APIs, interoperable attention protocols) with explicit sunset logic: as these mature, platform lock-in on attention mechanisms becomes less valuable. Movement sees extraction as temporary institutional artifact that will be displaced by better coordination mechanisms. Constrained by incumbent network effects but progressing toward genuinely lower-extraction alternatives. Sunset driven by technical feasibility of decentralized attention management and regulatory pressure.
constraint_indexing:constraint_classification(attention_allocation_problem, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal civilizational perspective, attention scarcity is an inherent property of cognition itself: no mind can consciously attend to all available information simultaneously. The allocation problem is thus a natural law comparable to thermodynamic limits or computational complexity bounds. However, this perspective risks naturalizing what is actually a contingent design choice: the explosion of attention demands is not inevitable but results from specific economic incentives and architectural decisions. The engine's false summit detector will flag this as naturalization of a manipulable system as if it were immutable law.
constraint_indexing:constraint_classification(attention_allocation_problem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_allocation_problem_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attention_allocation_problem, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_allocation_problem, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attention_allocation_problem, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attention_allocation_problem, TR),
    TR >= 0.70.

:- end_tests(attention_allocation_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The attention allocation problem involves genuine extraction from individual allocators (their cognitive capacity is harvested for advertiser/platform benefit) but is not maximal because some agents benefit (platforms, advertisers, and even some users who find value in curated content). The value (0.58) reflects that extraction is real and systematic but not total — some users derive genuine value, and the coordination function is non-zero. Suppression (0.62): High. Multiple mechanisms suppress exit: network effects (cannot leave platform without losing social connectivity), structural barriers (dark patterns make controlling attention settings difficult), psychological mechanisms (variable reward schedules create habituation), and social obligation (professional communication norms require availability). Suppression is not total because some users successfully adopt low-extraction workflows, but barriers are substantial. Theater ratio (0.65): Moderate-high. The apparatus of attention metrics (engagement time, click-through rates, conversion tracking) is substantially performative. Metrics optimize for measurable proxies rather than actual user value or wellbeing. Algorithmic ranking is presented as objective efficiency but is actually value-extraction optimization. The theater has increased over time as the gap between measured engagement and actual user benefit has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence. The individual powerless agent sees snare (systematic capture with no exit). The platform operator sees rope (pure coordination). The knowledge worker sees tangled rope (mixed coordination and extraction). The employer sees tangled rope (benefits from and costs from attention mechanisms). The attention metrics system sees piton (its own function is degraded and performative). The organized movement sees scaffold (temporary problem being solved). The civilizational analytical observer risks seeing mountain (immutable law) but the constraint's explosive growth trajectory suggests this is naturalization rather than discovery of inherent limit. The perspectival gaps reveal that classification depends entirely on structural position — no single perspective is 'correct' because they are measuring fundamentally different relationships to the same physical phenomenon (attention as scarce resource).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Individual allocators (victims/trapped) have high d → high f(d) → high experienced χ. Platform operators (beneficiaries/arbitrage) have low d → low f(d) → low χ. Professional workers (mixed/constrained) have moderate d reflecting partial victimhood and partial benefit. The scope modifier σ(S) amplifies extraction at global scope (σ=1.2) because attention capture mechanisms scale: a single algorithmic change affects billions simultaneously, making verification and escape coordination harder. At local scope (σ=0.8), small communities can more easily implement alternative allocation mechanisms, dampening effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY STRUCTURE: The attention allocation problem should be decomposed into at least three distinct constraints with different ε values: (1) Information Coordination Problem (matching content to interested parties): ε ≈ 0.15, Rope — genuine coordination with low extraction overhead. (2) Attention Monetization Mechanism (converting user attention into advertiser value): ε ≈ 0.72, Snare — pure extraction with minimal coordination function. (3) Notification/Interruption Design (dark patterns, variable rewards): ε ≈ 0.68, Snare — extraction mechanism maintaining attentional lock-in. The aggregated story (this JSON) sits at ε=0.58 because it encompasses all three. If the question were narrowly 'Is the attention allocation problem a coordination mechanism?', the answer is Rope. If the question were 'Are current notification designs extractive?', the answer is Snare. The mandatrophy is resolved by recognizing that the colloquial term 'attention allocation problem' conflates structurally distinct constraints with different ε values. The Tangled Rope classification is appropriate for the AGGREGATE across all three mechanisms because the constraint genuinely coordinates some attention flow while extracting from individual allocators. The theater ratio (0.65) captures that much of the apparent 'coordination' is performative metric optimization rather than genuine utility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'At what threshold does notification volume transition from coordination-enabling to extraction-inducing?',
    'Empirical measurement: cognitive load studies correlating notification frequency with task performance, decision quality, and subjective wellbeing; comparison of low-notification vs high-notification cohorts controlling for information necessity',
    'If threshold is low (< 5 notifications/day): most current systems are pure extraction (Snare from more perspectives). If threshold is high (> 20 notifications/day): current systems may be legitimate coordination with acceptable extraction costs (Tangled Rope or Rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Threshold distinguishing coordination-enabling from extraction-inducing notification levels').

omega_variable(
    algorithmic_feed_necessity,
    'Is algorithmic content ranking a necessary coordination mechanism or a value-extraction overlay on simpler alternatives?',
    'A/B testing: chronological feeds, user-controlled ranking, community-curated rankings vs algorithmic ranking; measurement of information utility, user satisfaction, and advertiser value for each ranking method',
    'If algorithmic ranking is necessary: current extraction levels justified as coordination cost (Tangled Rope with higher floor). If simple alternatives are equivalent: algorithmic ranking is pure extraction theater (Snare or Piton), and platforms are using coordination framing as cover.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_feed_necessity, empirical, 'Whether algorithmic ranking is necessary for coordination or extractive value-capture').

omega_variable(
    attentional_autonomy_measurement,
    'Can ''attentional autonomy'' be operationalized in a way that distinguishes genuine user control from the theater of choice?',
    'Longitudinal behavioral tracking: exit rate when users revoke attention-capture mechanisms (notifications, algorithmic ranking); measurement of time-to-exit-or-accept for deliberately difficult-to-revoke settings; user surveys on perceived control vs measured control',
    'If autonomy is measurable and users choose to maintain high-extraction settings: they derive genuine value and extraction is lower than suppression metrics suggest (Rope or Tangled Rope). If users exit when settings are easy to control: current suppression reflects actual coercion and constraints should be classified as Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attentional_autonomy_measurement, empirical, 'Operationalizing attentional autonomy beyond theater of choice').

omega_variable(
    emergence_of_decentralized_alternatives,
    'Are decentralized/interoperable attention management systems technically viable as drop-in replacements for platform-controlled allocation?',
    'Technical feasibility assessment: protocol designs (ActivityPub extensions for feed control, open-source attention ranking), pilot implementation in niche communities, scaling analysis to mainstream user base',
    'If viable and scalable: scaffold sunset is structural and timeline can be estimated (extraction will decline as alternatives mature). If technically infeasible: scaffold perspective is aspirational, extraction will persist, and classification should remain Snare/Tangled Rope indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergence_of_decentralized_alternatives, empirical, 'Technical viability of decentralized attention allocation alternatives').

omega_variable(
    suppression_mechanism_internalization,
    'Is measured suppression (high friction to modify notification settings, dark patterns) primarily structural or has the target population internalized attention capture as normal/unavoidable?',
    'Post-intervention suppression trajectory: introduce users to low-extraction workflows (time-blocking, notification-free periods); measure whether suppression persists after structural barriers are removed or collapses when cognitive frame changes',
    'If primarily structural: suppression metric accurately reflects coercion (Snare classification robust). If largely internalized: actual suppression is higher than metrics show — users carry constraint even after structural removal, indicating deep cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural barriers or internalized cognitive capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_allocation_problem, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attn_tr_t0, attention_allocation_problem, theater_ratio, 0, 0.35).
narrative_ontology:measurement(attn_tr_t5, attention_allocation_problem, theater_ratio, 5, 0.5).
narrative_ontology:measurement(attn_tr_t10, attention_allocation_problem, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(attn_be_t0, attention_allocation_problem, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(attn_be_t5, attention_allocation_problem, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(attn_be_t10, attention_allocation_problem, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_allocation_problem, attachment_coordination).
narrative_ontology:affects_constraint(attention_allocation_problem, information_verification_bottleneck).
narrative_ontology:affects_constraint(attention_allocation_problem, epistemic_commons_degradation).
narrative_ontology:affects_constraint(attention_allocation_problem, cognitive_autonomy_erosion).

% DUAL FORMULATION NOTE:
% The attention allocation problem is upstream of multiple epistemic and cognitive constraints. Information verification fails partly because attention to verification is scarce. Epistemic commons degrades because attention cannot be allocated to maintenance. Cognitive autonomy erodes because attention is captured. Each downstream constraint should be analyzed as a separate story with its own ε reflecting the secondary effects of attention scarcity on that specific domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(attention_allocation_problem, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
