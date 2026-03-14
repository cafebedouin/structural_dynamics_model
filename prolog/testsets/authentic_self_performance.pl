% ============================================================================
% CONSTRAINT STORY: authentic_self_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_authentic_self_performance, []).

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
 *   constraint_id: authentic_self_performance
 *   human_readable: Authentic Self Performance Constraint
 *   domain: social/psychological/institutional
 *
 * SUMMARY:
 *   The authentic self performance constraint describes the structural
 *   tension between the psychological and social necessity of performance and
 *   the individual desire for authenticity. Individuals must perform socially
 *   acceptable versions of themselves to secure resources, maintain
 *   relationships, and navigate institutional gatekeeping. Simultaneously,
 *   they internalize cultural narratives about finding and expressing their
 *   'true self.' This creates a sustained cognitive load: individuals monitor
 *   their own performance, experience distress from performing
 *   inauthentically, yet cannot exit without significant cost. The constraint
 *   is genuinely hybrid — it solves critical coordination problems (social
 *   predictability enables trust and cooperation) while extracting
 *   authenticity and cognitive bandwidth from individuals. The theater ratio
 *   (0.65) reflects the gap between performative authenticity discourse
 *   (mindfulness, personal branding, 'be yourself' self-help industry) and
 *   unchanged structural incentives for standardized self-presentation. The
 *   authenticity ideal persists as cultural narrative despite institutional
 *   constraints making it nearly impossible to achieve — a classic piton
 *   pattern where the functional purpose has degraded but the performance
 *   ritual continues.
 *
 * KEY AGENTS:
 *   - Individual Performer: Primary victim (powerless/trapped) — no structural exit from performance demands; career, belonging, resource access contingent on performed self
 *   - Constrained Authenticist: Secondary victim (moderate/constrained) — perceives performance as costly, has some agency but faces high exit costs (social penalty, professional damage, relational disruption)
 *   - Social Coordination System: Primary beneficiary (institutional/arbitrage) — legitimate coordination function enabled by standardized performance (trust, predictability, reduced transaction costs)
 *   - Institutional Gatekeepers: Secondary beneficiary (institutional/constrained) — employers, credentialing bodies, social institutions benefit from ability to read and enforce performance standards; also experience constraints from employee burnout and reduced innovation
 *   - Authenticity Ideal Maintenance Apparatus: Theatrical actor (analytical/analytical) — therapy industry, personal development, 'authentic leadership' discourse sustain the ideal performatively while structural constraints remain unchanged
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks reading the constraint as a natural law of social life rather than contingent institutional intensification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(authentic_self_performance, 0.52).
domain_priors:suppression_score(authentic_self_performance, 0.48).
domain_priors:theater_ratio(authentic_self_performance, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(authentic_self_performance, extractiveness, 0.52).
narrative_ontology:constraint_metric(authentic_self_performance, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(authentic_self_performance, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(authentic_self_performance, tangled_rope).
narrative_ontology:human_readable(authentic_self_performance, "Authentic Self Performance Constraint").
narrative_ontology:topic_domain(authentic_self_performance, "social/psychological/institutional").

domain_priors:requires_active_enforcement(authentic_self_performance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(authentic_self_performance, social_coordination_function).
narrative_ontology:constraint_beneficiary(authentic_self_performance, institutional_gatekeepers).
narrative_ontology:constraint_victim(authentic_self_performance, individual_authenticity).
narrative_ontology:constraint_victim(authentic_self_performance, cognitive_bandwidth).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED PERFORMER (SNARE) — Individual with no structural exit from performance demands. Career advancement, social belonging, and resource access all depend on sustaining the performed self. Cannot leave without catastrophic loss. The performed identity becomes indistinguishable from survival mechanism. Maximum experienced extraction — full bandwidth devoted to performance maintenance.
constraint_indexing:constraint_classification(authentic_self_performance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONSTRAINED AUTHENTIC SEEKER (TANGLED ROPE) — Individual who perceives performance as costly but faces significant barriers to exit: social penalty, professional cost, relational disruption. Some agency and some coordination benefit (social predictability enables trust), but extraction is asymmetric. The constraint coordinates social interaction while extracting authenticity. Exit is possible at high cost.
constraint_indexing:constraint_classification(authentic_self_performance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SOCIAL COORDINATION SYSTEM (ROPE) — The constraint genuinely solves coordination problems: predictable performance enables trust, reduces transaction costs, facilitates cooperation across strangers. The institutional order benefits from standardized self-presentation. Net beneficiary. Experience is coordination, not extraction.
constraint_indexing:constraint_classification(authentic_self_performance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL GATEKEEPER (TANGLED ROPE) — Employers, credentialing bodies, and social institutions benefit from ability to read and enforce standardized performance: job interviews, professional credentials, social roles. But they also face constraints: employee burnout, authenticity crises, reduced innovation from conformity. Mixed relationship — coordination function with embedded extraction mechanism that serves their interests.
constraint_indexing:constraint_classification(authentic_self_performance, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEGRADED AUTHENTICITY IDEAL (PITON) — Post-Romantic ideal of 'finding your true self' and 'being authentic' persists as cultural narrative despite strong institutional incentives for performance. Theater ratio is high: widespread therapeutic language about authenticity coexists with unchanged structural constraints. The ideal maintains itself through performative authenticity discourse (mindfulness, personal branding as 'authentic self-expression') rather than reducing performance load. Piton classification derives from theater gate and declining functional authenticity.
constraint_indexing:constraint_classification(authentic_self_performance, piton,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, performance is inherent to social coordination: Goffman's dramaturgical model treats self-presentation as constitutive of social order, not departing from it. The constraint appears as an immutable property of human social life — we are necessarily performers. However, the structural data (extractiveness 0.52, suppression 0.48) contradicts pure natural law. The mountain classification risks naturalizing contingent institutional intensification of performance demands as if performance itself were unchangeable.
constraint_indexing:constraint_classification(authentic_self_performance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(authentic_self_performance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(authentic_self_performance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(authentic_self_performance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(authentic_self_performance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(authentic_self_performance, TR),
    TR >= 0.70.

:- end_tests(authentic_self_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts authenticity (cognitive bandwidth devoted to self-monitoring and performance maintenance) and time. But extraction is not maximal because genuine coordination functions are present — standardized self-presentation does enable trust and cooperation. The value reflects that some extraction is 'fair' coordination cost while some portion is surplus serving institutional power interests. Suppression (0.48): Moderate. Significant barriers exist to non-performance (career penalty, social isolation, relational rupture) but are not total — some agents do exit, some contexts permit greater authenticity, remote work and niche communities reduce performance demands. Theater ratio (0.65): Moderate-high and rising. Increasing disconnect between cultural authenticity narratives and institutional performance requirements. Authenticity discourse (mindfulness, personal branding, 'authentic leadership') frames performance itself as authenticity, creating performative theater that claims to dissolve the constraint while leaving it intact. The ratio has risen over the interval as authenticity language has proliferated without structural change in performance incentives.
 *
 * PERSPECTIVAL GAP:
 *   The trapped performer (powerless/trapped) experiences the constraint as a snare — inescapable and purely extractive. The constrained authenticist (moderate/constrained) experiences genuine mixed coordination and extraction — they see how performance enables trust but also feel the cost. The social coordination system (institutional/arbitrage) experiences rope — pure coordination function with no perceived extraction. The institutional gatekeeper (institutional/constrained) experiences tangled rope — they benefit from standardized performance but also face constraints from employee burnout and the coordination costs of maintaining performance incentives. The degraded authenticity ideal (piton perspective) observes that cultural narratives about authenticity persist despite inability to fulfill them — the ideal is maintained performatively rather than functionally. The civilizational analytical observer risks seeing this as a mountain (inherent property of social life) but the structural data suggests it is a contingent institutional intensification, not a natural law. The gap between perspectives reveals how the same constraint serves coordination and extraction simultaneously, benefiting those who set performance standards while extracting from those who must meet them.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to performance flows. The trapped performer (powerless/trapped) experiences full extraction — they are positioned as the target of performance enforcement. The constrained authenticist (moderate/constrained) experiences moderate extraction — they have some agency and some coordination benefit. The social coordination system (institutional/arbitrage) experiences negative extraction — they are the net beneficiary, performance flows toward them. The institutional gatekeeper (institutional/constrained) experiences mixed extraction — they benefit from setting standards but face costs from maintaining them. Directionality follows the flow of authenticity sacrifice toward those who benefit from standardized presentation: employers gain predictable workers, institutions gain enforceable norms, coordinators gain reduced transaction costs. Those who must perform experience the extraction asymmetry most acutely. The analytical contexts experience the constraint as either an immutable natural law (false mountain) or as a contingent institutional arrangement (true piton).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE DEGRADATION MECHANISM: The constraint exhibits mandatrophy through the gap between the authentic self mandate (cultural imperative to 'find your true self' and 'be authentic') and the institutional performance mandate (requirement to present standardized, professionally acceptable versions of self). Both mandates are simultaneously present and contradictory. The authenticity industry (therapy, personal development, executive coaching) attempts to resolve this by reframing performance itself as authenticity — 'authentic leadership' means performing the right persona convincingly, 'personal branding' means authentic self-expression. This performative resolution maintains the mandate while leaving the underlying contradiction unresolved. The mandatrophy is resolved not by choosing one mandate over the other but by recognizing that the institutional performance mandate is primary and the authenticity mandate is secondary cover. The piton perspective captures this: authenticity discourse persists as cultural narrative precisely because institutional performance demands are non-negotiable, making the authenticity mandate impossible to fulfill except performatively. Resolution requires recognizing that the mandatrophy is structural, not psychological — the constraint cannot be solved by individual authenticity work because it is not fundamentally a constraint on individuals but on the institutional order that demands performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_definition_ambiguity,
    'What constitutes ''authentic self'' when all self-presentation is necessarily performed (Goffman)?',
    'Phenomenological analysis: distinguish between performed-ness (the fact of performance) and performance-strain (the cost and consciousness of performance). Examine historical periods with lower self-monitoring costs and different authenticity criteria.',
    'If authenticity is performance-strain-reduction: the constraint is about optimizing disclosure bandwidth, not returning to ''true self''. If authenticity is some core kernel beneath performance: the constraint becomes snare-dominant (core self cannot surface). If authenticity is incoherent: the entire framework dissolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_definition_ambiguity, conceptual, 'Definition of authenticity in context of necessary performance').

omega_variable(
    performance_demand_intensity_rise,
    'Has performance demand intensity increased over the past 50 years, or is the constraint stable with modernization rhetoric masking stability?',
    'Comparative ethnography: self-monitoring costs in mid-20th-century institutions vs. contemporary. Time use data on impression management. Analysis of authenticity discourse frequency and tenor.',
    'If intensity rising: constraint is degrading (tangled rope → snare over time). If stable: current perception of crisis is performative (piton dynamic). If declining: modern tools (remote work, niche communities) are raising exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_demand_intensity_rise, empirical, 'Trend in performance demand intensity over time').

omega_variable(
    identity_lock_versus_material_barrier,
    'Is the suppression of authentic self primarily structural (material barriers to exit) or internalized (identity fusion with performed self)?',
    'Longitudinal tracking: agents who exit performance-heavy contexts. Do they report post-exit suppression persistence (internalized) or does suppression drop when external barriers remove (structural)? Examine therapy and identity-work outcomes.',
    'If internalized: agents are identity-locked, carrying suppression with them. Measured suppression (0.48) understates true binding. If structural: suppression declines when barriers remove. Implications for whether exit options correctly classified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_versus_material_barrier, empirical, 'Source of suppression mechanism: structural vs. internalized').

omega_variable(
    coordination_necessity_degree,
    'What portion of observed performance is necessary for coordination vs. surplus extraction serving institutional power?',
    'Comparative institutional analysis: minimum performance requirements for trust vs. actual enforcement. Examine organizations with reduced performance expectations (flat hierarchies, remote-first cultures) and track coordination failure rates.',
    'If necessary portion is high (>70%): constraint is predominantly rope (strong coordination function). If low (<40%): constraint is predominantly snare (extraction exceeds coordination need). Affects whether all perspectives are justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_necessity_degree, empirical, 'Proportion of performance demand that serves coordination vs. extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(authentic_self_performance, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(authperf_tr_t0, authentic_self_performance, theater_ratio, 0, 0.42).
narrative_ontology:measurement(authperf_tr_t2, authentic_self_performance, theater_ratio, 2, 0.52).
narrative_ontology:measurement(authperf_tr_t4, authentic_self_performance, theater_ratio, 4, 0.61).
narrative_ontology:measurement(authperf_tr_t6, authentic_self_performance, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(authperf_be_t0, authentic_self_performance, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(authperf_be_t2, authentic_self_performance, base_extractiveness, 2, 0.44).
narrative_ontology:measurement(authperf_be_t4, authentic_self_performance, base_extractiveness, 4, 0.49).
narrative_ontology:measurement(authperf_be_t6, authentic_self_performance, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(authentic_self_performance, identity_coordination).
narrative_ontology:affects_constraint(authentic_self_performance, workplace_emotional_labor).
narrative_ontology:affects_constraint(authentic_self_performance, professional_identity_formation).
narrative_ontology:affects_constraint(authentic_self_performance, burnout_cycle).

% DUAL FORMULATION NOTE:
% Authentic self performance decomposes into multiple structurally distinct constraints: (1) performance_demand_enforcement (ε≈0.45) — the institutional mechanism that mandates standardized self-presentation; (2) authenticity_narrative_maintenance (ε≈0.38) — the cultural/therapeutic discourse that frames performance as authenticity; (3) cognitive_load_extraction (ε≈0.58) — the bandwidth cost of self-monitoring and performance management. This story integrates all three; domains with high evidence on one decomposed constraint should be examined for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(authentic_self_performance, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
