% ============================================================================
% CONSTRAINT STORY: ritualistic_transition_scaffold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ritualistic_transition_scaffold, []).

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
 *   constraint_id: ritualistic_transition_scaffold
 *   human_readable: The Habit-Building Scaffold
 *   domain: social/institutional
 *
 * SUMMARY:
 *   The habit-building scaffold is an intentional deployment of procedural
 *   theater to stabilize chaotic organizations during transitions. When
 *   established organizations disrupt operations (merger, leadership change,
 *   technology adoption, crisis recovery), explicit rituals — structured
 *   meetings, standardized communication protocols, ceremonial
 *   decision-making, documented handoffs — reduce ambiguity and help members
 *   internalize new coordination norms. The scaffold is temporary by design:
 *   once habits solidify and coordination becomes implicit, the explicit
 *   ritual phases out naturally. This differs from a degraded piton (which
 *   persists through inertia) or a pure extraction snare (which uses
 *   procedure to extract from participants). The critical feature is the
 *   sunset clause: leadership commits to phasing out explicit ritual once
 *   implicit coordination emerges, measured by meeting efficiency, reduced
 *   need for facilitation, and member self-reports of internalized norms.
 *   Theater ratio rises during the ritual phase because much of the activity
 *   is performative — verbalization, ceremony, and documentation serve to
 *   encode norms into behavior rather than accomplish external tasks. But
 *   theater should decline as the scaffold is phased out and implicit
 *   coordination takes over.
 *
 * KEY AGENTS:
 *   - Organizational Leadership: Institutional actors (institutional/arbitrage) — design and deploy the ritual intentionally; control the sunset timeline; experience the scaffold as a temporary coordination investment
 *   - Individual Members: Moderate actors (moderate/mobile) — participate in ritual; benefit from reduced ambiguity; experience coordination function; retain exit option (can leave organization if norms become intolerable)
 *   - Collective Habit Formation: Organized agents (organized/constrained) — the implicit coordination system that the ritual stabilizes; initially chaotic, gradually solidifies into habit; beneficiary of the scaffold
 *   - Resistant Minority: Powerful actors (powerful/constrained) — experience both coordination benefit (stability) and extraction cost (time, autonomy); constrained by membership but powerful enough to voice dissent; perspective reveals mixture of rope and snare
 *   - Behavioral Science Observer: Analytical actor (analytical/analytical) — sees ritual-based habit formation as a well-established coordination mechanism with minimal extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ritualistic_transition_scaffold, 0.28).
domain_priors:suppression_score(ritualistic_transition_scaffold, 0.35).
domain_priors:theater_ratio(ritualistic_transition_scaffold, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ritualistic_transition_scaffold, extractiveness, 0.28).
narrative_ontology:constraint_metric(ritualistic_transition_scaffold, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ritualistic_transition_scaffold, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ritualistic_transition_scaffold, scaffold).
narrative_ontology:human_readable(ritualistic_transition_scaffold, "The Habit-Building Scaffold").
narrative_ontology:topic_domain(ritualistic_transition_scaffold, "social/institutional").

domain_priors:requires_active_enforcement(ritualistic_transition_scaffold).
narrative_ontology:has_sunset_clause(ritualistic_transition_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ritualistic_transition_scaffold, organizational_leadership).
narrative_ontology:constraint_beneficiary(ritualistic_transition_scaffold, stabilizing_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORGANIZATIONAL LEADERSHIP (SCAFFOLD) — Institutional actors with exit optionality. They deploy ritual and procedural constraint intentionally to stabilize chaotic operational states. The constraint is temporary by design: once organizational habits solidify and implicit coordination replaces explicit ritual, the constraint can be lifted. Leadership experiences the ritual as a sunset mechanism, not an extractive burden.
constraint_indexing:constraint_classification(ritualistic_transition_scaffold, scaffold,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 2: INDIVIDUAL MEMBER (ROPE) — Participants in the ritual experience it primarily as coordination. The shared procedural structure reduces ambiguity about role expectations, meeting times, communication norms, and decision-making processes. Extractiveness is low because the ritual directly enables the member's own goal (stable collaboration). Mobile exit options (can leave if norms become intolerable) prevent maximum suppression.
constraint_indexing:constraint_classification(ritualistic_transition_scaffold, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: COLLECTIVE HABIT FORMATION (SCAFFOLD) — Organized agents (the implicit group coordination system) see the ritual as a temporary support structure for embedding new norms. Theater ratio is elevated because much of the ritual is performative — the explicit verbalization and ceremonial aspects serve to reinforce cognitive pathways rather than accomplish material tasks. But the sunset is clear: as habits solidify and the group internalizes coordination, explicit ritual becomes redundant and dissolves naturally.
constraint_indexing:constraint_classification(ritualistic_transition_scaffold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: RESISTANT MINORITY (TANGLED ROPE) — Some members experience the ritual as both coordination (solving the chaos problem) and extraction (time cost, forced participation, loss of autonomy). They are constrained by organizational membership and group pressure while also benefiting from the stabilization. The mixture of coordination function and asymmetric cost places this perspective at tangled_rope rather than pure rope or pure snare.
constraint_indexing:constraint_classification(ritualistic_transition_scaffold, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: BEHAVIORAL SCIENCE OBSERVER (ROPE) — From the analytical/civilizational level, ritual-based habit formation is a well-established coordination mechanism with strong empirical support (habit loops, environmental design, social proof). The constraint classifies as pure rope: coordination function is primary, extraction is minimal, theater serves coordination by encoding norms into behavior. No sunset is visible from this perspective — the mechanism works perpetually for habit consolidation, though specific rituals may rotate.
constraint_indexing:constraint_classification(ritualistic_transition_scaffold, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ritualistic_transition_scaffold_tests).
:- end_tests(ritualistic_transition_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate. The ritual imposes time and behavioral constraints on participants, but these are justified by the coordination function (reducing organizational chaos). The extractiveness value reflects that most participants experience net benefit — the ritual solves real coordination problems. The extractiveness is not zero because there is asymmetry: leadership controls the ritual design and sunset timing, while members must participate. But extractiveness does not rise to snare levels because: (1) membership is voluntary with exit options, (2) members perceive genuine coordination benefit, and (3) the constraint is explicitly temporary. Suppression (0.35): Moderate. Participation is expected (social pressure, organizational expectation) but not coerced. Members can voice dissent, negotiate ritual modifications, or leave. The resistant minority experiences higher suppression (constrained exit due to career costs of leaving), but organization-wide, suppression is moderate. Theater ratio (0.65): Moderate-high. Explicit ritual by definition elevates theater: verbalization, ceremony, documentation, and procedural compliance all serve symbolic/encoding functions rather than producing external goods. Theater peaks during the mid-phase (time point 6) when the ritual is most active and explicitly performative, then would decline as the scaffold is phased out and coordination becomes implicit (not captured in this 12-month interval, which shows the scaffold at maturity).
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap is between leadership (who see the ritual as intentionally temporary coordination support) and resistant members (who see it as extracted time and autonomy loss). Leadership's rope/scaffold perspectives show the coordination function and sunset design. The resistant minority's tangled_rope perspective captures the mixed experience: real coordination benefit but asymmetric cost. The individual member's rope perspective shows that most participants experience genuine coordination gain. The behavioral science observer's rope perspective validates the mechanism as pure coordination, not extraction. The gap reveals that the constraint's classification depends critically on whether participants perceive the ritual as legitimately temporary or as degrading into performative theater without function (the piton risk identified in omega_ritual_dependency_risk).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing the scaffold from adjacent types through three structural markers: (1) Explicit sunset clause — leadership publicly commits to phasing out ritual once implicit coordination emerges. (2) Low base extractiveness (0.28 < 0.46) — the constraint is not a snare; participants experience net coordination benefit. (3) Theater ratio (0.65) is elevated but declining during the measurement interval as habits form and ritual becomes less necessary — the theater is part of the coordination mechanism (encoding norms into behavior), not a sign of degradation. If the ritual were to persist beyond the projected sunset (12-24 months), theater would remain high and extractiveness would rise (as participants begin experiencing ritual as busywork rather than coordination), converting the scaffold into a piton. The mandatrophy test: Can leadership commit to and execute the sunset? If yes, scaffold. If the ritual becomes permanent through inertia despite low ongoing coordination function, piton. If the ritual becomes coercive (exit costs rise dramatically for dissenters), snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_coordination_boundary,
    'At what point does voluntary ritual participation become enforced compliance, converting the scaffold into a snare?',
    'Measurement of exit costs: anonymous surveys on perceived choice, tracking of members who leave during ritual establishment phase, correlation between ritual adherence and social/career consequences',
    'If exit costs rise above 0.5: classification shifts toward snare for affected members. If costs remain low: rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_vs_coordination_boundary, empirical, 'Boundary between voluntary coordination ritual and coercive enforcement').

omega_variable(
    implicit_coordination_emergence_timeline,
    'How long does it take for explicit ritual to become implicit habit, and how is successful transition measured?',
    'Longitudinal measurement of ritual abandonment rates, meeting efficiency metrics, informal coordination frequency, member self-reports of internalized norms',
    'If transition occurs within 12-24 months: sunset clause is real and timing estimates are valid. If transition stalls beyond 36 months: ritual may be degrading into piton (performative without functional internalization).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_coordination_emergence_timeline, empirical, 'Timeline and indicators for habit internalization').

omega_variable(
    ritual_dependency_risk,
    'Does sustained ritual create organizational dependency where explicit procedures prevent natural habit formation, trapping the organization in perpetual theater?',
    'Comparative analysis: organizations that extended ritual beyond projected sunset vs those that phased ritual out; measurement of member competence and autonomy in both conditions',
    'If dependency occurs: constraint degrades from scaffold to piton. If habits solidify and ritual phases out as designed: scaffold classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_dependency_risk, empirical, 'Risk of ritual creating permanent dependency rather than temporary scaffolding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ritualistic_transition_scaffold, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rts_tr_t0, ritualistic_transition_scaffold, theater_ratio, 0, 0.4).
narrative_ontology:measurement(rts_tr_t6, ritualistic_transition_scaffold, theater_ratio, 6, 0.65).
narrative_ontology:measurement(rts_tr_t12, ritualistic_transition_scaffold, theater_ratio, 12, 0.72).

% Extraction over time
narrative_ontology:measurement(rts_be_t0, ritualistic_transition_scaffold, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(rts_be_t6, ritualistic_transition_scaffold, base_extractiveness, 6, 0.28).
narrative_ontology:measurement(rts_be_t12, ritualistic_transition_scaffold, base_extractiveness, 12, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ritualistic_transition_scaffold, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
