% ============================================================================
% CONSTRAINT STORY: bureaucratic_legibility_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bureaucratic_legibility_collapse, []).

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
 *   constraint_id: bureaucratic_legibility_collapse
 *   human_readable: The Administrative Whiteout
 *   domain: political/organizational/informational
 *
 * SUMMARY:
 *   This constraint models the 'Administrative Whiteout,' a phenomenon where
 *   an institution's metrics for understanding and managing a domain become
 *   pathologically decoupled from reality. Inspired by James C. Scott's
 *   'Seeing Like a State,' it describes a system where simplified, legible
 *   abstractions (statistics, targets, KPIs) supplant complex ground truth.
 *   The institution then acts upon this false map, leading to actions that
 *   are counterproductive or destructive in the real world, while appearing
 *   successful on paper. The system's primary function shifts from achieving
 *   its stated goals to maintaining the fiction of the metrics, creating a
 *   high-theater, high-extraction environment.
 *
 * KEY AGENTS:
 *   - Central Planning Bureaucracy: Primary beneficiary (institutional/arbitrage) — benefits from the illusion of control and simplified reporting.
 *   - Managed Population: Primary victim (powerless/trapped) — suffers the real-world consequences of misguided policies.
 *   - Front-Line Implementers: Secondary victims (moderate/constrained) — forced to enforce rules they know are dysfunctional.
 *   - Reform Coalition: Organized agents (organized/mobile) — attempting to introduce accountability and better metrics.
 *   - Analytical Observer: Systems theorist (analytical/analytical) — sees the full, self-perpetuating structure of the collapse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bureaucratic_legibility_collapse, 0.68).
domain_priors:suppression_score(bureaucratic_legibility_collapse, 0.75).
domain_priors:theater_ratio(bureaucratic_legibility_collapse, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bureaucratic_legibility_collapse, extractiveness, 0.68).
narrative_ontology:constraint_metric(bureaucratic_legibility_collapse, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bureaucratic_legibility_collapse, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bureaucratic_legibility_collapse, tangled_rope).
narrative_ontology:human_readable(bureaucratic_legibility_collapse, "The Administrative Whiteout").
narrative_ontology:topic_domain(bureaucratic_legibility_collapse, "political/organizational/informational").

domain_priors:requires_active_enforcement(bureaucratic_legibility_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bureaucratic_legibility_collapse, central_planning_bureaucracy).
narrative_ontology:constraint_beneficiary(bureaucratic_legibility_collapse, metric_auditing_firms).
narrative_ontology:constraint_victim(bureaucratic_legibility_collapse, managed_population).
narrative_ontology:constraint_victim(bureaucratic_legibility_collapse, front_line_implementers).
narrative_ontology:constraint_victim(bureaucratic_legibility_collapse, ecological_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MANAGED POPULATION (SNARE) — Experiences the direct, negative consequences of policies based on flawed metrics. They are trapped in a system whose actions are arbitrary and harmful, with no recourse or ability to correct the institutional map. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.97.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CENTRAL PLANNERS (ROPE) — Perceive the system as a necessary coordination tool. The metrics provide a simplified, legible view that enables decision-making at scale. Negative feedback is filtered or dismissed as local noise. They benefit from the illusion of control. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.08.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: FRONT-LINE IMPLEMENTER (TANGLED ROPE) — Caught between the official metrics and ground truth. They must enforce policies they know are counterproductive to meet performance targets, making them both agents of the system's extraction and victims of its dysfunction. d≈0.85, f(d)≈1.15, σ=0.8 → χ≈0.62.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: VETERAN BUREAUCRAT (PITON) — Remembers when the metrics were more closely aligned with reality. Sees the current system as a degraded, inertial version of its former self, where performative compliance has replaced functional purpose. The theater_ratio of 0.80 triggers the piton classification.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — A group of auditors, journalists, and activists working to introduce alternative metrics and accountability. They see the current system as a temporary, failing structure that must be dismantled and replaced, viewing their own efforts as the sunset clause. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.51.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — Sees the full structure: a high-extraction, high-suppression system where a coordination claim masks a reality-inverting machine. The combination of ε=0.68, suppression=0.75, and high theater (0.80) classifies it as a severe snare, where the primary extraction is reality itself. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bureaucratic_legibility_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bureaucratic_legibility_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bureaucratic_legibility_collapse, TR),
    TR >= 0.70.

:- end_tests(bureaucratic_legibility_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The system extracts well-being, resources, and ecological health from the managed domain and converts it into 'successful' data points that fuel the bureaucracy. Suppression (0.75): High. The official metrics are enforced as the sole source of truth. Local, anecdotal, or contradictory knowledge is actively dismissed, delegitimized, or punished. Theater Ratio (0.80): Very High. The core activity of the institution becomes performative — generating reports, meeting targets, and conducting audits that validate the flawed map, rather than achieving real-world outcomes. This high theater score is crucial for the Piton perspective.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. Central planners see a functional coordination mechanism (Rope) because the numbers on their dashboards add up. The managed population experiences an arbitrary, inescapable, and destructive force (Snare). Front-line workers are caught in a hybrid system of coercion and coordination (Tangled Rope). Veteran insiders see a degraded version of a once-functional system (Piton). This highlights how a single, dysfunctional structure can generate the full spectrum of classifications depending on the observer's position relative to the flow of information and consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (planners) have arbitrage exit — they can always change the metric or the narrative, leading to a low 'd' value and a Rope classification. Victims (population) are trapped, leading to a high 'd' value and a Snare classification. The analytical observer's classification as a Snare is driven by the objective high values of ε and suppression, which override any claims of coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a powerful resolver of mandatrophy. An external observer might mistake the institution's stated purpose ('coordination', 'management') for its actual function. The Deferential Realism framework, by centering the perspective of the powerless ('trapped' exit), correctly identifies the system's snare-like nature. The bureaucracy's claim to be a Rope is revealed as a perspectival illusion maintained by its structural position, not an objective description of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_drift,
    'Was the legibility collapse an intentional outcome designed to obscure failure and extract resources, or an emergent property of bureaucratic drift and complexity?',
    'Archival analysis of the system''s design principles and initial metric selection criteria, compared with longitudinal data on performance and resource allocation.',
    'If intentional, the system is a pure Snare from its inception. If emergent drift, it began as a Rope or Scaffold and degraded into a Piton/Snare, which has different implications for reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_drift, conceptual, 'Distinguishing between intentional design and emergent bureaucratic drift').

omega_variable(
    reversibility_threshold,
    'Is there a point of metric-reality decoupling beyond which an institution cannot recover its ability to see accurately?',
    'Comparative case studies of institutional reform efforts, correlating the degree of theater/extraction with the success or failure of interventions.',
    'If a threshold exists, reform efforts past that point are futile, and the only solution is dissolution (a true sunset clause). If not, even deeply degraded systems are salvageable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversibility_threshold, empirical, 'Whether institutional decoupling from reality is reversible').

omega_variable(
    local_knowledge_viability,
    'Can complex, ''illegible'' local knowledge be integrated into a large-scale administrative system without being destructively simplified?',
    'Pilot programs testing federated or polycentric governance models against centralized, standardized ones.',
    'If viable, it provides a structural alternative (Rope). If not, it suggests a hard limit on institutional scale, making some form of legibility collapse a Mountain-like constraint for large systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_knowledge_viability, empirical, 'Scalability of local knowledge vs. standardized metrics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bureaucratic_legibility_collapse, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bure_tr_t0, bureaucratic_legibility_collapse, theater_ratio, 0, 0.3).
narrative_ontology:measurement(bure_tr_t10, bureaucratic_legibility_collapse, theater_ratio, 10, 0.65).
narrative_ontology:measurement(bure_tr_t20, bureaucratic_legibility_collapse, theater_ratio, 20, 0.8).

% Extraction over time
narrative_ontology:measurement(bure_be_t0, bureaucratic_legibility_collapse, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(bure_be_t10, bureaucratic_legibility_collapse, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(bure_be_t20, bureaucratic_legibility_collapse, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bureaucratic_legibility_collapse, resource_allocation).
narrative_ontology:affects_constraint(bureaucratic_legibility_collapse, public_trust_in_institutions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
