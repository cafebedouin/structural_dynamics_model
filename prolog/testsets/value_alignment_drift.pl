% ============================================================================
% CONSTRAINT STORY: value_alignment_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_value_alignment_drift, []).

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
 *   constraint_id: value_alignment_drift
 *   human_readable: The Purpose Creep: Value Alignment Drift in Algorithmic Systems
 *   domain: technological/social
 *
 * SUMMARY:
 *   Value alignment drift is a structural constraint where optimization for a
 *   measurable proxy metric (engagement, watch-time, click-through rate)
 *   systematically diverges from the underlying human value it was meant to
 *   serve (well-being, epistemic integrity, informed decision-making). The
 *   constraint operates at the intersection of three structural realities:
 *   (1) the technical necessity of scalable metrics to govern algorithmic
 *   routing at platform scale, (2) the opacity of human well-being compared
 *   to machine-measurable engagement signals, and (3) the financial incentive
 *   structure that rewards engagement maximization. The constraint is not a
 *   bug in a single system but a pattern that reproduces across platforms,
 *   algorithms, and organizations whenever a measurable proxy becomes the
 *   optimization target. This story demonstrates how a coordination mechanism
 *   (platforms solving the problem of content routing at scale) can evolve
 *   into an extraction mechanism (engagement optimization that harms user
 *   well-being) while maintaining the appearance of serving its original
 *   purpose. The drama unfolds over 15+ years: early engagement metrics
 *   solved legitimate scaling problems (2010-2015), but as the algorithms
 *   learned to exploit psychological vulnerabilities at scale, the metric
 *   drifted further from its justification (2015-2025), producing measurable
 *   harms (mental health, polarization, misinformation). The theater ratio
 *   has risen from 0.35 to 0.68 because much of the public response has been
 *   performative: 'well-being' committees, metric transparency reports, and
 *   algorithm audits that do not alter the core engagement-optimization
 *   objective.
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture engagement-driven revenue, recommendation-system efficiency, and data extraction from user behavioral optimization
 *   - End Users: Primary victim (powerless/trapped) — bear the full cost of misalignment: attention extraction, behavioral manipulation, psychological harms, misinformation exposure with minimal exit options
 *   - Content Creators: Secondary beneficiary/victim (moderate/constrained) — gain platform reach (coordination function) but lose autonomy over content strategy, forced to optimize for virality rather than value
 *   - Societal Well-Being (abstract collective): Victim (powerless/trapped) — epistemic integrity, collective deliberation, and mental health deteriorate as alignment drifts; cannot organize or exit
 *   - Regulatory and Standards Coalition: Organized agent (organized/constrained) — build alternative metrics and accountability structures (digital rights, content moderation standards, AI alignment research) attempting to create a sunset pathway
 *   - Engagement Metric as Institutional Inertia: Institutional actor (institutional/arbitrage) — the metric persists through organizational momentum even as operators acknowledge its harms; shifted from solving a problem to maintaining a status quo
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(value_alignment_drift, 0.58).
domain_priors:suppression_score(value_alignment_drift, 0.64).
domain_priors:theater_ratio(value_alignment_drift, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(value_alignment_drift, extractiveness, 0.58).
narrative_ontology:constraint_metric(value_alignment_drift, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(value_alignment_drift, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(value_alignment_drift, tangled_rope).
narrative_ontology:human_readable(value_alignment_drift, "The Purpose Creep: Value Alignment Drift in Algorithmic Systems").
narrative_ontology:topic_domain(value_alignment_drift, "technological/social").

domain_priors:requires_active_enforcement(value_alignment_drift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(value_alignment_drift, platform_operators).
narrative_ontology:constraint_beneficiary(value_alignment_drift, engagement_optimizers).
narrative_ontology:constraint_victim(value_alignment_drift, end_users).
narrative_ontology:constraint_victim(value_alignment_drift, societal_well_being).
narrative_ontology:constraint_victim(value_alignment_drift, epistemic_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Users are trapped within algorithmic systems designed to maximize engagement metrics, which increasingly diverge from their stated well-being. Exit options are minimal (leaving means losing platform access entirely). The constraint extracts attention, behavioral data, and psychological engagement with no transparent exchange. Maximum experienced extraction because users bear the full cost of misalignment with no recourse.
constraint_indexing:constraint_classification(value_alignment_drift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONTENT CREATOR (TANGLED ROPE) — Creators benefit from platform reach and audience building (coordination function), but face severe constraints: algorithmic suppression of non-engaging content, pressure to optimize for viral metrics rather than value, and loss of control over how their work is distributed. Mixed experience: genuine coordination benefit coupled with enforced drift toward engagement optimization. Constrained exit — platform dependence makes leaving costly but not impossible.
constraint_indexing:constraint_classification(value_alignment_drift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences the constraint as coordination: engagement metrics are a solution to the problem of scale. The platform needs a scalable proxy for 'value' to route content to billions of users. Engagement optimization is presented as solving a coordination problem (matching content to interest). High arbitrage: operators can shift metrics, reformulate objectives, or migrate to alternate platforms if needed. Extraction runs toward this agent, not away.
constraint_indexing:constraint_classification(value_alignment_drift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AND STANDARDS COALITION (SCAFFOLD) — Organized actors (digital ethics bodies, content moderation frameworks, AI alignment research communities) are building alternative metrics and accountability structures. These represent temporary supports with sunset logic: as algorithmic auditing, value-aligned objectives, and transparent metric reporting mature, the engagement-optimization bottleneck should decay. Theater is moderate-low because regulatory proposals are largely substantive rather than performative.
constraint_indexing:constraint_classification(value_alignment_drift, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ENGAGEMENT METRIC AS INSTITUTIONAL INERTIA (PITON) — The engagement optimization regime persists largely through institutional momentum. Early platform design (YouTube's watch-time metric, Facebook's Like button) solved a real problem at scale, but the original function has atrophied. Operators now recognize engagement-driven amplification produces polarization, mental health harms, and misinformation spread — yet the metric persists because alternatives haven't fully crystallized, and changing it risks revenue disruption. Theater ratio is high because much of the public discourse about 'fixing' platforms is performative — metrics and algorithms are frequently adjusted in cosmetic ways that don't address the core misalignment.
constraint_indexing:constraint_classification(value_alignment_drift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MOUNTAIN VIEW (SCALING LIMIT) — From a civilizational analytical perspective, some form of proxy metric is inherent to any system that scales beyond direct human judgment. The problem of translating complex human values into machine-measurable signals is a fundamental constraint on automated systems. However, the structural data contradicts a pure mountain classification — the drift from engagement to harm is not inevitable but a choice (in metric design, in optimization objectives, in regulatory forbearance). The engine will identify this as a false summit: naturalization of a contingent design decision.
constraint_indexing:constraint_classification(value_alignment_drift, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(value_alignment_drift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(value_alignment_drift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(value_alignment_drift, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(value_alignment_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(value_alignment_drift, TR),
    TR >= 0.70.

:- end_tests(value_alignment_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts user attention, behavioral data, and psychological engagement on a massive scale. Early in the constraint's lifecycle (t=0), engagement optimization genuinely solved coordination problems (matching content to interest at scale). By t=16, the extraction is far more pronounced as algorithms have learned to exploit psychological vulnerabilities, dark patterns, and recommendation cascades that prioritize engagement over accuracy or user welfare. The value has risen from 0.28 to 0.58 as the original coordination function atrophied. Suppression (0.64): High. Significant barriers to exit include lock-in effects (social graph, data portability friction, network effects), platform switching costs, and the absence of transparent alternatives. Users cannot easily switch to platforms with different metrics, and competing platforms often adopt similar optimization patterns. Regulatory alternatives are embryonic. Theater ratio (0.68): High. Much of the public response to misalignment is performative: platform announcements of 'well-being' initiatives, metric transparency reports, algorithm audits, and AI ethics committees that do not alter the core engagement-optimization incentive. The theater has risen as platforms have invested in appearing to address alignment while maintaining the underlying extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a dramatic perspectival gap. Platform operators genuinely experience the constraint as coordination (Rope) — engagement metrics solved a real technical problem at scale. End users experience it as pure extraction (Snare) — they have no transparency into how algorithms optimize their attention, no exit options, and face harms that accumulate invisible to them. Content creators experience it as tangled coordination-extraction (Tangled Rope) — they benefit from reach but lose control. Regulatory bodies see it as a temporary problem being solved (Scaffold) — alternative metrics and auditing standards are maturing. The institutional engagement-metric regime sees itself as degraded (Piton) — operators acknowledge the harms but persist in optimization because alternatives haven't fully crystalized and revenue disruption risk is high. The analytical observer risks seeing an immutable natural law (Mountain) — 'all systems at scale need proxy metrics' — but the structural data reveals this as false naturalization: the drift from engagement to harm reflects design choices (metric selection, optimization objectives, financial incentives) not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from the agent's structural position relative to extraction flows. Platform operators have arbitrage exit (can shift metrics, acquire competitors, or migrate to new products) and benefit from engagement-driven revenue, yielding low d → low/negative chi (experience as coordination). End users have trapped exit (cannot walk away without losing platform access) and bear the full cost of misalignment, yielding high d → high chi (maximum experienced extraction). Content creators have constrained exit (dependent on platform reach) with mixed benefits and costs, yielding moderate d → moderate chi (tangled rope experience). Organized regulatory agents have constrained exit (building alternatives takes time and coordination) but genuine agency (can create new metrics and accountability structures), yielding moderate d → moderate chi (scaffold experience). The engagement metric itself, as an institutional inertia actor, has arbitrage exit (operators can change it) but maintains high suppression (change risks business disruption and alternative metrics aren't proven), yielding piton through the theater gate rather than through high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the classification varies legitimately across perspectives, but the perspectival gap itself is a symptom of misalignment. The platform operator's Rope (genuine coordination) is NOT wrong, but it reflects only the operator's perspective. The end user's Snare (pure extraction) is also NOT wrong — it reflects their structural reality. The tangled rope (platform operators' intentional mixing of coordination benefit with extraction cost) is the integrative view that acknowledges both. The false summit (analytical mountain) is exposed: the drift from engagement to harm is not inevitable but a choice. The scaffold perspective is crucial because it identifies the constraint as temporary and solvable — alternative metrics, algorithmic auditing, and user-aligned objectives are not physically impossible, only institutionally underdeveloped. The mandatrophy is resolved by recognizing that value alignment drift is an extractive parasitism on a coordination mechanism, not a fundamental law of scaled systems. Better metrics exist (user-reported satisfaction over time, epistemic diversity, long-term well-being), but they require redistribution of incentive structure away from engagement-maximization toward user welfare maximization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_metric_viability,
    'Can alternative well-being-aligned metrics (user-reported satisfaction, long-term retention without addiction, epistemic diversity) be implemented at platform scale without collapsing under Goodhart''s Law?',
    'Pilot implementations of alternative metrics (Twitter''s ''Birdwatch'' community notes, YouTube''s labeled-recommendation algorithms); measurement of metric robustness under adversarial optimization attempts; comparison of user outcomes under alternative metric regimes',
    'If viable: scaffold perspective confirmed, sunset is structurally real, constraint is temporary. If not viable: mountain perspective gains force — some form of proxy misalignment may be inevitable at scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_metric_viability, empirical, 'Whether alternative metrics can scale without collapsing to the same misalignment').

omega_variable(
    operator_intent_vs_incentive_alignment,
    'Do platform operators genuinely believe engagement optimization serves user well-being (misalignment of intent), or do they knowingly prioritize engagement over well-being (misalignment of incentive)?',
    'Internal documentation analysis (regulatory discovery, leaks); leadership statements about metric design choices; correlation between stated values and implemented incentives; historical trajectory of metric adjustments in response to harm evidence',
    'If intent-misaligned: problem is one of epistemology and design (solvable by better metrics). If incentive-misaligned: problem is extraction (requires structural redistribution of platform profits or regulatory enforcement). Classification sensitivity: high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operator_intent_vs_incentive_alignment, empirical, 'Whether misalignment is driven by belief or by financial incentive').

omega_variable(
    monopoly_vs_coordination_nature,
    'Is platform dominance a natural monopoly (coordination function so strong that fragmentation would reduce utility) or an extractive monopoly (dominance maintained despite supraoptimal extraction)?',
    'Cross-platform comparison of coordination efficiency; measurement of switching costs and lock-in mechanisms; historical analysis of competitor entry and displacement; user welfare analysis under different market structures',
    'If natural monopoly: constraint structure is Rope or Scaffold with high coordination value, lower extractiveness. If extractive monopoly: constraint is closer to Snare, higher suppression. Determines regulatory response.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monopoly_vs_coordination_nature, empirical, 'Whether platform dominance is coordinative or extractive in nature').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(value_alignment_drift, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vad_tr_t0, value_alignment_drift, theater_ratio, 0, 0.35).
narrative_ontology:measurement(vad_tr_t8, value_alignment_drift, theater_ratio, 8, 0.52).
narrative_ontology:measurement(vad_tr_t16, value_alignment_drift, theater_ratio, 16, 0.68).

% Extraction over time
narrative_ontology:measurement(vad_be_t0, value_alignment_drift, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(vad_be_t8, value_alignment_drift, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(vad_be_t16, value_alignment_drift, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(value_alignment_drift, information_standard).
narrative_ontology:affects_constraint(value_alignment_drift, algorithmic_amplification_of_extremism).
narrative_ontology:affects_constraint(value_alignment_drift, data_extraction_regimes).
narrative_ontology:affects_constraint(value_alignment_drift, goodhart_metric_collapse).

% DUAL FORMULATION NOTE:
% Value alignment drift is downstream of specific platform design choices (recommendation algorithms, metric selection, incentive structures) but represents a distinct structural constraint on any system that scales beyond direct human judgment. The upstream constraints (specific algorithmic amplification mechanisms, data extraction architectures) have their own extractiveness values reflecting the particular implementation. The alignment drift constraint is the family problem: the tendency of proxy metrics to diverge from intended values under optimization pressure. Decomposition: algorithmic_amplification_of_extremism is a specific manifestation of the general alignment drift constraint in the political discourse domain; goodhart_metric_collapse is the theoretical foundation; data_extraction_regimes represents the incentive infrastructure that enforces metric optimization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(value_alignment_drift, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
