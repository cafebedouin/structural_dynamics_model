% ============================================================================
% CONSTRAINT STORY: externalization_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_externalization_necessity, []).

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
 *   constraint_id: externalization_necessity
 *   human_readable: Externalization Necessity for Self-Knowledge
 *   domain: philosophy_of_mind/epistemology/social_ontology
 *
 * SUMMARY:
 *   The externalization necessity for self-knowledge is the epistemic
 *   constraint that agents cannot achieve reliable self-knowledge through
 *   introspection alone — knowledge of one's own beliefs, competences,
 *   personality traits, and blind spots requires externalization into a
 *   shared world where the work can be examined by differently-positioned
 *   observers. This constraint is grounded in Hannah Arendt's theory of the
 *   common world (The Human Condition) and supported by empirical studies
 *   showing systematic self-other disagreement on personality assessment and
 *   competence evaluation. The constraint is downstream of
 *   instrument_object_identity (the mountain-level constraint that an
 *   observer cannot simultaneously be the observed without positional shift)
 *   but operates at the social-epistemic level rather than the logical level.
 *   The coordination function is genuine: externalization enables
 *   triangulation from multiple observation positions, distributed
 *   error-checking, and access to perspectives unavailable from the
 *   first-person standpoint. The modest extractiveness (0.32) reflects that
 *   externalization has real costs — time, cognitive effort, vulnerability to
 *   misinterpretation, dependence on shared symbolic systems — but these
 *   costs are primarily coordination overhead rather than asymmetric
 *   extraction. The constraint exhibits rope classification from most
 *   perspectives because the coordination function is accessible and the
 *   costs are symmetric.
 *
 * KEY AGENTS:
 *   - Individual Knowledge Seeker: Primary beneficiary (moderate/mobile) — gains access to external feedback and triangulation; experiences coordination function directly
 *   - Epistemic Community: Organized beneficiary (organized/mobile) — scientific communities, philosophical traditions, collaborative knowledge systems that enable distributed cognition through shared externalization
 *   - Knowledge Commons: Institutional beneficiary (institutional/arbitrage) — accumulates externalized knowledge artifacts across generations; benefits from durability of externalized knowledge
 *   - Novice Learner: Temporary dependent (moderate/constrained) — requires external feedback for calibration but dependence decreases with expertise (scaffold logic)
 *   - Epistemically Isolated Agent: Mixed position (powerless/identity_locked) — coordination function exists but is inaccessible due to lack of audience or feedback mechanisms; may internalize isolation as epistemic self-sufficiency
 *   - Analytical Observer: Structural view (analytical/analytical) — sees externalization necessity as emerging from instrument_object_identity constraint (mountain classification is legitimate, not false summit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(externalization_necessity, 0.32).
domain_priors:suppression_score(externalization_necessity, 0.28).
domain_priors:theater_ratio(externalization_necessity, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(externalization_necessity, extractiveness, 0.32).
narrative_ontology:constraint_metric(externalization_necessity, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(externalization_necessity, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(externalization_necessity, rope).
narrative_ontology:human_readable(externalization_necessity, "Externalization Necessity for Self-Knowledge").
narrative_ontology:topic_domain(externalization_necessity, "philosophy_of_mind/epistemology/social_ontology").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(externalization_necessity, knowledge_seeking_agents).
narrative_ontology:constraint_beneficiary(externalization_necessity, epistemic_communities).
narrative_ontology:constraint_beneficiary(externalization_necessity, collaborative_knowledge_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL KNOWLEDGE SEEKER (ROPE) — Experiences externalization as coordination mechanism. By articulating thoughts in shared world, gains access to feedback from differently-positioned observers. Mobile exit (can choose solipsistic introspection) but recognizes coordination value. Low extraction — the constraint solves genuine epistemic problem of self-opacity.
constraint_indexing:constraint_classification(externalization_necessity, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: EPISTEMIC COMMUNITY (ROPE) — Scientific communities, philosophical traditions, and collaborative knowledge systems experience externalization as coordination infrastructure. Shared symbolic systems (language, notation, experimental protocols) enable distributed cognition. Organized agents with mobile exit see pure coordination function — the constraint enables collective intelligence that exceeds individual capacity.
constraint_indexing:constraint_classification(externalization_necessity, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: KNOWLEDGE COMMONS (ROPE) — Institutional beneficiary with arbitrage exit. The externalization requirement creates durable knowledge artifacts (texts, data, arguments) that persist beyond individual minds. Benefits from accumulation of externalized knowledge across generations. Experiences constraint as pure coordination — no extraction, only enabling function.
constraint_indexing:constraint_classification(externalization_necessity, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From analytical position, externalization necessity appears as structural feature of knowledge itself. Self-knowledge requires triangulation from multiple observation positions (Arendt's common world). Empirical studies show systematic self-other disagreement on personality and competence — internal introspection alone cannot resolve these gaps. The constraint emerges from the structure of perspectival knowledge, not from institutional arrangement. However, this mountain classification is legitimate (not false summit) because the underlying constraint is instrument_object_identity (mountain) — the observer cannot simultaneously be the observed without positional shift.
constraint_indexing:constraint_classification(externalization_necessity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: NOVICE LEARNER (SCAFFOLD) — Experiences externalization as temporary support structure. Early-stage learners require external feedback to calibrate self-assessment, but as expertise develops, internal models improve. The constraint has sunset logic — dependence on external validation decreases as metacognitive capacity matures. Constrained exit (cannot skip externalization phase) but sees declining extraction over biographical time.
constraint_indexing:constraint_classification(externalization_necessity, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: EPISTEMICALLY ISOLATED AGENT (TANGLED ROPE) — Agent without access to epistemic community or feedback mechanisms experiences mixed coordination and extraction. The externalization requirement is genuine (coordination function exists) but inaccessible (no audience, no shared symbolic system, no feedback loop). Identity-locked because isolation may be internalized as epistemic self-sufficiency. Experiences moderate extraction — the constraint's coordination function exists but cannot be accessed from this position.
constraint_indexing:constraint_classification(externalization_necessity, tangled_rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(identity_locked),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(externalization_necessity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(externalization_necessity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(externalization_necessity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(externalization_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate-low. Externalization has real costs (time, effort, vulnerability, symbolic mediation requirements) but these are primarily coordination overhead. The constraint solves a genuine epistemic problem (self-opacity, blind spots, metacognitive limitations) and the costs are largely symmetric across agents. The modest extraction reflects that some agents (epistemically isolated, lacking access to feedback mechanisms) bear costs without accessing the coordination function, but this is a minority position. Suppression (0.28): Low-moderate. Agents can choose solipsistic introspection (mobile exit for most) but recognize the epistemic costs. Some suppression exists for agents outside dominant symbolic systems or lacking access to epistemic communities, but barriers are surmountable for most agents at biographical timescales. Theater ratio (0.35): Low-moderate. Some externalization is performative (signaling rather than genuine knowledge-seeking) but most externalization serves its stated function (enabling feedback and triangulation). The theater ratio has increased slightly over the interval as academic publishing and social media have introduced more performative externalization, but the core coordination function remains intact.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits rope classification from most perspectives (individual, community, institutional) because the coordination function is accessible and the costs are symmetric. The scaffold perspective (novice learner) reflects that dependence on external feedback decreases with expertise — the constraint has sunset logic at biographical timescales for individual agents. The tangled_rope perspective (epistemically isolated agent) reflects that coordination function exists but is inaccessible for agents without audience or feedback mechanisms — genuine extraction for this minority position. The mountain perspective (analytical observer) is legitimate rather than false summit because the constraint is downstream of instrument_object_identity (the observer cannot simultaneously be the observed) — externalization necessity emerges from the structure of perspectival knowledge itself. The perspectival gap is modest because most agents experience the constraint as coordination, but the gap reveals that access to epistemic community is not universal — isolation creates extraction even when the coordination function is genuine.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality structure is beneficiary-dominant: most agents experience externalization as coordination that enables self-knowledge they could not achieve alone. Individual knowledge seekers (moderate/mobile) are beneficiaries with low d — they gain access to feedback and triangulation. Epistemic communities (organized/mobile) are beneficiaries with low d — shared externalization enables distributed cognition. The knowledge commons (institutional/arbitrage) is a beneficiary with very low d — accumulates externalized knowledge across generations. The novice learner (moderate/constrained) is a temporary beneficiary with moderate d — requires external feedback but dependence decreases over time (scaffold logic). The epistemically isolated agent (powerless/identity_locked) is the only position with elevated d — coordination function exists but is inaccessible, creating extraction without benefit. This agent is identity-locked rather than trapped because the isolation may be internalized (epistemic self-sufficiency as identity) rather than purely structural. The analytical observer (analytical/analytical) sees the constraint as mountain (structural feature of perspectival knowledge) but this is legitimate rather than false summit because the underlying constraint is instrument_object_identity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that rope classification can coexist with mountain-level structural grounding. The externalization necessity is rope (coordination function with low extraction) from most perspectives, but it is downstream of instrument_object_identity (mountain — the observer cannot simultaneously be the observed). The rope classification is not undermined by the mountain foundation — the coordination function is genuine even though it emerges from structural constraints. The tangled_rope perspective (epistemically isolated agent) shows that coordination functions can create extraction when access is asymmetric, but this does not invalidate the rope classification for agents with access. The scaffold perspective (novice learner) shows that coordination requirements can have sunset logic at biographical timescales even when the underlying structural constraint is permanent. The mandatrophy resolution is that coordination (rope) can be built on top of structural necessity (mountain) without collapsing into extraction — the coordination function is real, the costs are symmetric for most agents, and the constraint solves a genuine epistemic problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    introspective_reliability_threshold,
    'At what level of metacognitive sophistication does introspection alone provide reliable self-knowledge without external triangulation?',
    'Longitudinal studies comparing self-assessment accuracy across expertise levels; measurement of metacognitive calibration in domains with objective performance metrics',
    'If threshold is low (achievable within biographical time): scaffold perspective generalizes — externalization is temporary support. If threshold is high or unreachable: rope perspective generalizes — externalization is permanent coordination requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(introspective_reliability_threshold, empirical, 'Threshold for introspective reliability without external feedback').

omega_variable(
    symbolic_mediation_necessity,
    'Is externalization into symbolic form (language, notation, artifact) necessary, or is direct intersubjective recognition (pre-linguistic shared attention) sufficient for self-knowledge?',
    'Developmental studies of self-concept formation in pre-linguistic children; cross-cultural studies of self-knowledge in communities with different symbolic systems; phenomenological analysis of recognition vs articulation',
    'If symbolic mediation is necessary: coordination function is tightly coupled to linguistic/cultural infrastructure (higher suppression for agents outside dominant symbolic systems). If direct recognition suffices: coordination function is more universal (lower suppression).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(symbolic_mediation_necessity, conceptual, 'Whether symbolic externalization is necessary or direct recognition suffices').

omega_variable(
    epistemic_isolation_voluntariness,
    'Is epistemic isolation (lack of access to feedback) primarily structural (material barriers, geographic isolation, institutional exclusion) or identity-based (internalized self-sufficiency, rejection of external validation)?',
    'Sociological analysis of isolated agents'' access to epistemic communities; psychological studies of epistemic self-reliance vs forced isolation; historical case studies of solitary thinkers',
    'If primarily structural: tangled_rope perspective reflects genuine extraction (coordination function exists but is inaccessible due to external barriers). If primarily identity-based: classification shifts toward rope (agent has exit option but chooses not to exercise it).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_isolation_voluntariness, empirical, 'Whether epistemic isolation is structural or identity-based').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(externalization_necessity, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(extern_tr_t0, externalization_necessity, theater_ratio, 0, 0.3).
narrative_ontology:measurement(extern_tr_t50, externalization_necessity, theater_ratio, 50, 0.33).
narrative_ontology:measurement(extern_tr_t100, externalization_necessity, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(extern_be_t0, externalization_necessity, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(extern_be_t50, externalization_necessity, base_extractiveness, 50, 0.3).
narrative_ontology:measurement(extern_be_t100, externalization_necessity, base_extractiveness, 100, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(externalization_necessity, information_standard).

% DUAL FORMULATION NOTE:
% Externalization necessity is downstream of instrument_object_identity (mountain constraint that observer cannot simultaneously be observed). The upstream constraint establishes the logical necessity; the downstream constraint operates at the social-epistemic level where externalization into shared symbolic systems enables triangulation from multiple observation positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
