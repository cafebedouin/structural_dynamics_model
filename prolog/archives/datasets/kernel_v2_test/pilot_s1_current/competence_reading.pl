% ============================================================================
% CONSTRAINT STORY: competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: competence_reading
 *   human_readable: Competence Reading of Preparedness Transmission
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the COMPETENCE READING of the contested
 *   kernel preparedness_transmission. The competence reading asserts that
 *   organizational preparedness is sustained through continuous operational
 *   practice and embodied knowledge transmission (apprenticeship during
 *   drills and exercises), independent of memorial framing or commemoration.
 *   Competence persists regardless of whether the organization maintains
 *   narrative rituals, monuments, or cultural retrospectives about past
 *   disasters. The reading claims that skill, pattern recognition, and muscle
 *   memory are acquired and retained through exercise, not through stories or
 *   remembrance. This reading sits opposite the husk_reading, which asserts
 *   that preparedness depends on sustained memorial performance — that the
 *   cultural narrative of past disaster and collective remembrance motivates
 *   the institutional commitment to preparedness spending and drills. The
 *   competence reading classifies the constraint as Mountain from all
 *   perspectives: the independence of embodied knowledge from memorial
 *   framing appears as a natural law of learning and skill retention. The
 *   metrics reflect this classification: near-zero extractiveness (no party
 *   collects from the fact that competence is embodied), minimal suppression
 *   (no enforcement needed — drills work by their own logic), and minimal
 *   theater (competence assessment is direct and functional, not
 *   performative).
 *
 * KEY AGENTS:
 *   - Individual Responders: Powerless/trapped agents who learn through drill and operational exercise. Experience the constraint as immutable natural law.
 *   - Operating Disaster Response Teams: Organized agents with constrained exit who maintain competence through apprenticeship and continuous exercises. Perceive the constraint as structural fact of their operation.
 *   - Regional Emergency Management Authority: Institutional agent with mobile exit options who allocates preparedness budgets. Experiences the constraint as natural law: drill funding produces competence; memorial spending does not.
 *   - Analytical Observer: Civilizational perspective perceiving the constraint as immutable law of embodied knowledge transmission across all organizational contexts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_reading, 0.08).
domain_priors:suppression_score(competence_reading, 0.12).
domain_priors:theater_ratio(competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_reading, mountain).
narrative_ontology:human_readable(competence_reading, "Competence Reading of Preparedness Transmission").
narrative_ontology:topic_domain(competence_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:emerges_naturally(competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_reading, 'fa608c9b-6523-4a86-8e24-7cf2d141577b').
narrative_ontology:cs_kernel_codification('fa608c9b-6523-4a86-8e24-7cf2d141577b', distributed).
narrative_ontology:cs_authority_grounding('fa608c9b-6523-4a86-8e24-7cf2d141577b', practice).
narrative_ontology:cs_interpretation_layer_present('fa608c9b-6523-4a86-8e24-7cf2d141577b').
narrative_ontology:cs_reading_relation('fa608c9b-6523-4a86-8e24-7cf2d141577b', competence_reading__preparedness_husk_reading, coexists_with).
narrative_ontology:cs_axiom('fa608c9b-6523-4a86-8e24-7cf2d141577b', foundational, apprenticeship_transmission_doctrine).
narrative_ontology:cs_axiom_status(apprenticeship_transmission_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('fa608c9b-6523-4a86-8e24-7cf2d141577b', apprenticeship_transmission_doctrine, empirically_contingent).
narrative_ontology:cs_axiom('fa608c9b-6523-4a86-8e24-7cf2d141577b', foundational, embodied_knowledge_irreducibility).
narrative_ontology:cs_axiom_status(embodied_knowledge_irreducibility, holdable).
narrative_ontology:cs_axiom_grounding('fa608c9b-6523-4a86-8e24-7cf2d141577b', embodied_knowledge_irreducibility, empirically_contingent).
narrative_ontology:cs_reference_frame('fa608c9b-6523-4a86-8e24-7cf2d141577b', apprenticeship_competence_baseline).
narrative_ontology:cs_drift_state('fa608c9b-6523-4a86-8e24-7cf2d141577b', contemporary_post_professionalization_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fa608c9b-6523-4a86-8e24-7cf2d141577b', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(competence_reading, apprenticeship_transmission_doctrine).
narrative_ontology:constraint_vindicates(competence_reading, embodied_knowledge_irreducibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Frontline personnel (firefighters, paramedics, emergency managers) who develop competence through repeated drill and operational response. They experience preparedness training as a continuous requirement; their competence is built through hands-on practice, observation of senior responders, and muscle memory from exercises. They have no exit from the constraint — competence acquisition requires practice.
narrative_ontology:constraint_stakeholder(competence_reading, individual_responders, observer,
    powerless, biographical, trapped, local).

% Institutional teams (fire departments, emergency response units) that maintain preparedness through scheduled drills, exercises, and real operational deployment. These teams run the apprenticeship system — senior members train junior members during drills and actual responses. They set the agenda for preparedness protocols and drill schedules. They are constrained by budget limits and staffing capacity but have some discretion in how to structure training.
narrative_ontology:constraint_stakeholder(competence_reading, disaster_response_teams, agenda_setter,
    organized, generational, constrained, regional).

% Governance authority (state/regional emergency management office) that allocates preparedness budgets across memorial spending and operational training. Can choose to fund drills versus monuments, memorials versus exercises. From the competence reading perspective, this authority observes that operational spending produces measurable competence; memorial spending does not directly enhance technical capacity. The authority can shift resources between modalities.
narrative_ontology:constraint_stakeholder(competence_reading, regional_emergency_authority, agenda_setter,
    institutional, generational, mobile, national).

% The abstract organizational commitment to preparedness as a valued practice. In the competence reading, preparedness culture is an outcome of functional success (drills work, competence improves, hazards are managed), not a prerequisite. The cultural commitment persists because the constraint is real — because embodied knowledge genuinely persists and competence genuinely improves through practice.
narrative_ontology:constraint_stakeholder(competence_reading, preparedness_culture, observer,
    powerless, civilizational, constrained, global).
narrative_ontology:stakeholder_non_agent(competence_reading, preparedness_culture).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintaining and transmitting technical competence in hazard response across generational turnover and organizational staffing changes. The coordination problem is: how does the organization ensure that new responders develop genuine competence (not just procedural compliance) and that experienced responders maintain skill as they age and face novel hazard patterns?
% TRANSFER_FUNCTION: Knowledge and skill transfer from experienced to new responders. What moves: embodied pattern recognition, judgment about hazard severity and appropriate response intensity, muscle memory for standard protocols, and adaptation logic for novel scenarios. From: senior/experienced responders to junior/new responders. To: the collective capacity of the team.
% ABSENT_VOICES: Responders from historical disasters (if surviving) whose firsthand experience could inform training; victims of past failures whose testimony could highlight skill gaps; private-sector responders operating parallel preparedness systems; international responders from other disaster regimes. These voices are absent from the formal apprenticeship system; their insights are not systematically incorporated unless they happen to be embedded in the team through hiring or consultation.
% DISAPPEARANCE_RATIONALE: If the competence reading constraint (embodied knowledge transmission through practice) were to disappear — if responders could somehow maintain competence without continuous drill and exercise — the world would rearrange significantly. Hazard response capacity would degrade rapidly. Response times would increase, error rates would rise, adaptation to novel scenarios would deteriorate. The competence-dependent aspects of disaster response would fail: rescue coordination, triage decisions, resource allocation under uncertainty, and protection of civilians. The constraint is not peripheral; it is foundational to functional disaster response.
% FOUNDING_PROBLEM: The founding problem is the basic skill and knowledge maintenance challenge: how does a disaster response organization ensure that its personnel remain competent to handle emergencies when the frequency, magnitude, and type of hazards are uncertain, when staff turnover is continuous, and when expertise cannot be reduced to written procedures? The constraint — that competence persists through practice, independent of narrative — emerges as a solution to this problem: invest in continuous drills and exercises, and competence will maintain itself through embodied learning.
% FOUNDING_PROBLEM_CORROBORATION: The problem status is attested by: (1) active operational disaster response metrics — response times, error rates, civilian casualty prevention rates — which directly depend on responder competence; (2) professional literature in emergency management and occupational safety documenting the decay of competence without practice; (3) after-action reviews from historical disasters showing that responder competence predicted outcome quality more strongly than procedural documents or memorial knowledge. The problem is live because disasters continue to occur unpredictably, hazard patterns evolve, and staff turnover continues — the need to maintain embodied competence does not resolve.
narrative_ontology:disappearance_verdict(competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(competence_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRACTICING RESPONDER (MOUNTAIN) — Individual who learns preparedness through continuous drill and operational exercise. The constraint is immutable from this position: competence persists independent of whether the organization maintains memorial rituals or commemorative framing. Drills work regardless of narrative. The responder perceives no alternative to competence maintenance through exercise — it is a natural law of skill acquisition and retention.
constraint_indexing:constraint_classification(competence_reading, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE OPERATING DISASTER RESPONSE TEAM (MOUNTAIN) — Institutional actor maintaining preparedness through continuous operational deployment and scheduled drills. From this position, the constraint appears as a structural fact: competence is maintained through apprenticeship (senior → junior transmission during exercises) and muscle memory (repeated execution of protocols), not through memorial monuments or stories about past disasters. The team experiences the constraint as unchangeable — to maintain competence, you must drill; there is no substitute.
constraint_indexing:constraint_classification(competence_reading, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE REGIONAL EMERGENCY MANAGEMENT AUTHORITY (MOUNTAIN) — Institutional actor with capacity to allocate resources. From this position, the constraint appears as a natural law: preparedness budgets must fund continuous training, drills, and exercises; memorial spending (monuments, museums, commemorative events) does not directly produce competence. The authority perceives the constraint as immutable natural law at this scale — drill schedules work, monuments do not. Exit options exist (the authority could choose to memorialize rather than drill) but the constraint's force is evident: drills produce competence, memorials do not. Framing does not change the structural fact.
constraint_indexing:constraint_classification(competence_reading, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, competence in response to repeated-pattern hazards (earthquakes, hurricanes, floods, pandemics) is grounded in a natural law of learning and skill maintenance: embodied knowledge — the pattern recognition, muscle memory, and apprenticeship transmission encoded in practice — persists independent of cultural narrative framing. This is not contingent on memorial institutions or commemorative doctrine. The analytical observer perceives complete accessibility collapse: there is no conceivable alternative framing that changes this fact. Resistance is negligible: no institutional actor disputes that drills produce competence.
constraint_indexing:constraint_classification(competence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_reading_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(competence_reading, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(competence_reading, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(competence_reading),
    narrative_ontology:constraint_metric(competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Near-zero. The competence reading asserts that embodied knowledge transmission is independent of memorial framing — nobody collects value from this independence, and no agent is extracted from. The minimal nonzero value reflects that the reading is contestable (the husk reading claims memorial is necessary), creating a small epistemic uncertainty tax. Suppression (0.12): Minimal. No enforcement is required for the constraint to persist — drills work through their own functional logic, not through coercion. Suppression represents only the baseline coordination cost of scheduling and resource allocation for exercises. Theater ratio (0.15): Minimal. The competence reading treats drill assessment as direct and functional: participants either develop competence through exercise or they do not. The low theater reflects that actual competence measurement (error rates, response times, adaptation quality) is the metric, not performative compliance. Stability across the interval (measurements flat): Consistent with Mountain classification — the constraint's metrics are stable because embodied knowledge transmission is a natural law that persists unchanged across the observation period. If extractiveness, suppression, or theater rose significantly, the reading would drift toward Tangled Rope or Piton, suggesting that the constraint is not purely natural law but contingent institutional practice.
 *
 * PERSPECTIVAL GAP:
 *   Minimal perspectival gap. All four perspectives classify the constraint as Mountain because the reading asserts that the constraint is truly immutable — competence persists independent of narrative framing from any observer position. The gap would widen under the husk_reading, where memorial performance would classify differently for beneficiaries (Rope) versus responders (Snare). But under the competence_reading, the gap collapses to zero: drills work everywhere, memorials decorate everywhere, and the structural fact is universal.
 *
 * DIRECTIONALITY LOGIC:
 *   The mountain classification from all perspectives reflects the reading's core claim: competence is embodied, independent of memorial framing, and emerges naturally from practice. No indexical position experiences extraction or suppression as a function of this constraint — the constraint is not about asymmetric benefit or coercion, but about the structural facts of learning and skill retention. Each agent's directionality is near the symmetric point (d ≈ 0.45–0.55) because the constraint is not distributional (nobody benefits, nobody pays); it is a natural law. The analytical observer's perspective is not analytically captured: the constraint is equally visible and equally immutable from all positions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    embodied_vs_narrative_independence,
    'Is competence truly independent of narrative framing, or does narrative context (understanding the historical precedent, knowing the story of past failure) enhance skill retention and application fidelity in ways that pure drill cannot measure?',
    'Longitudinal cohort comparison: measure competence retention over 5+ years in cohorts with identical drill schedules but varying narrative/memorial exposure. Compare error rates, adaptation quality to novel scenarios, and intergenerational knowledge transfer fidelity.',
    'If narrative enhances competence: the constraint is Tangled Rope (genuine coordination function + asymmetric extraction via narrative control). If narrative is inert: the constraint is Mountain (competence is embodied, memorial framing is decorative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embodied_vs_narrative_independence, empirical, 'Whether narrative framing enhances or is independent of competence').

omega_variable(
    kernel_reading_contest,
    'Which reading of preparedness transmission is structurally accurate — the competence reading (embodied knowledge persists independent of memorial framing) or the husk reading (memorial performance sustains the cultural commitment that motivates preparedness investment)?',
    'Historical case analysis across diverse organizational contexts: natural experiment when memorial funding is cut without cutting drill budgets (or vice versa). Measure: Does competence decline? Does drill participation decline? Does political support for preparedness persist?',
    'If competence remains stable and drill participation persists without memorial support: competence reading is correct (Mountain). If drill participation erodes or political support collapses: husk reading is correct (memorial frame is structurally necessary to sustained preparedness culture).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which reading of preparedness transmission is structurally true').

omega_variable(
    apprenticeship_sufficiency,
    'Does on-the-job apprenticeship during drills and operations transmit competence at sufficient fidelity across generations, or does explicit memorial/historical documentation improve transmission by providing context that allows younger responders to adapt knowledge to novel scenarios?',
    'Analysis of adaptation failures: track whether competence gaps in novel scenarios (e.g., responders trained only on 1990s earthquake protocols facing 2020s flood-pandemic cascade) correlate with cohorts that lacked historical context versus pure drill training.',
    'If apprenticeship alone is sufficient: Mountain (pure embodied transmission). If context improves adaptation: Tangled Rope (apprenticeship provides coordination; historical narrative provides necessary adaptation capacity that extracts value from the organization''s collective memory).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apprenticeship_sufficiency, empirical, 'Whether apprenticeship alone suffices or historical context improves adaptation').

omega_variable(
    reading_mutual_foreclosure,
    'Do the competence reading (embodied knowledge independent of memorial) and husk reading (memorial performance necessary for cultural commitment) logically foreclose one another, or do they coexist as different explanations of preparedness dynamics that could both be true in different organizational contexts?',
    'Institutional mapping: identify organizations where preparedness is maintained despite memorial collapse (supports competence reading foreclosure) versus organizations where memorial collapse triggers preparedness collapse despite identical drill schedules (supports husk reading foreclosure). If both patterns exist, readings coexist.',
    'If readings foreclose: the kernel has a determinate structure and one reading is false. If readings coexist: preparedness transmission involves both embodied and memorial dimensions; both readings capture true aspects of different organizational configurations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_mutual_foreclosure, conceptual, 'Whether the two readings logically foreclose each other or coexist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_theater_t0, competence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comp_theater_t5, competence_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(comp_theater_t10, competence_reading, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(comp_extract_t0, competence_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(comp_extract_t5, competence_reading, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(comp_extract_t10, competence_reading, base_extractiveness, 10, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(comp_suppress_t0, competence_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(comp_suppress_t5, competence_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(comp_suppress_t10, competence_reading, suppression_requirement, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(competence_reading, 0.08).
narrative_ontology:affects_constraint(competence_reading, preparedness_husk_reading).

% DUAL FORMULATION NOTE:
% This constraint and preparedness_husk_reading form a constraint family decomposed from the single contested kernel preparedness_transmission. They are not alternative measurements of the same constraint; they are structurally distinct claims with different ε values about what sustains preparedness. The competence_reading emphasizes embodied knowledge (ε ≈ 0.08, Mountain); the husk_reading emphasizes memorial narrative (expected ε ≈ 0.35–0.45, likely Tangled Rope or Piton). Each reading has its own beneficiary/victim structure, though the competence reading has no identified beneficiaries or victims because it classifies the constraint as immutable natural law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
