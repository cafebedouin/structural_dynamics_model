% ============================================================================
% CONSTRAINT STORY: cow_field_poop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cow_field_poop, []).

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
 *   constraint_id: cow_field_poop
 *   human_readable: The Cow Field Hazard (Pragmatic Avoidance)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The cow field hazard exemplifies a pure coordination constraint: agents
 *   must navigate an environment containing distributed negative features
 *   (waste, disease vectors, physical hazards) that cannot be eliminated but
 *   can be managed through shared information and behavioral protocols.
 *   Unlike snares (which extract value through coercion) or tangled ropes
 *   (which mix coordination with asymmetric extraction), the cow field hazard
 *   is fundamentally a coordination problem. No agent benefits from others
 *   being harmed; all parties benefit from effective hazard communication.
 *   The constraint emerges naturally from biological cohabitation and
 *   persists because pragmatic avoidance is genuinely useful to all users.
 *   Over time, institutional layers develop (warning signs, maintenance
 *   schedules, designated pathways) that increase theater_ratio from ~0.15 to
 *   0.35, reflecting gradual bureaucratization of what began as informal
 *   coordination. However, the functional core remains intact — the
 *   constraint continues to solve a real problem.
 *
 * KEY AGENTS:
 *   - Pragmatic Field Users: Primary beneficiaries (moderate/mobile) — benefit from shared hazard information enabling efficient navigation
 *   - Field Landowner: Secondary beneficiary (powerful/arbitrage) — maintains field and benefits from reduced friction with users through clear hazard communication
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes hazard as archetypal coordination problem without malice or extraction
 *   - Institutional Hazard Management: Institutional layer (institutional/constrained) — develops formal protocols and warning systems; theater increases as bureaucratic maintenance becomes detached from actual user needs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cow_field_poop, 0.32).
domain_priors:suppression_score(cow_field_poop, 0.28).
domain_priors:theater_ratio(cow_field_poop, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cow_field_poop, extractiveness, 0.32).
narrative_ontology:constraint_metric(cow_field_poop, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(cow_field_poop, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cow_field_poop, rope).
narrative_ontology:human_readable(cow_field_poop, "The Cow Field Hazard (Pragmatic Avoidance)").
narrative_ontology:topic_domain(cow_field_poop, "social/psychological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cow_field_poop, pragmatic_agents).
narrative_ontology:constraint_beneficiary(cow_field_poop, field_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRAGMATIC FIELD USER (ROPE) — Recognizes cow field as an inherent hazard requiring coordination around avoidance behaviors. Exit option is mobile (can choose different route or time). Power is moderate (has options but not unlimited). d≈0.50, f(d)≈0.65, σ=0.8 → χ≈0.17. Pure coordination: learning where the hazard exists and when enables collective navigation.
constraint_indexing:constraint_classification(cow_field_poop, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: FIELD LANDOWNER (ROPE) — Manages the field and its hazards. Benefits from establishing clear boundaries and shared understanding about how to coexist with the constraint. Exit is arbitrage (can modify the field itself, fence it, or manage herd). Power is powerful (controls the resource). d≈0.15, f(d)≈-0.01, σ=0.8 → χ≈-0.00. Coordination with minimal extraction: landowner has interest in users understanding the hazard without friction.
constraint_indexing:constraint_classification(cow_field_poop, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (ROPE) — Views the cow field hazard as an archetypal coordination problem: how do communities organize around inevitable, distributed, naturally-occurring negative features? The hazard is not created by malice or extraction — it simply exists. Rope classification reflects that the constraint solves a real collective action problem (information sharing about hazard location/timing) without requiring hierarchical coercion. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.21.
constraint_indexing:constraint_classification(cow_field_poop, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: INSTITUTIONAL HAZARD MANAGEMENT (PITON) — Over time, formal hazard management systems (warning signs, designated paths, maintenance protocols) develop. These institutions persist partly through genuine coordination function (people do benefit from shared understanding) but increasingly through theater: bureaucratic sign-posting and routine inspections that maintain institutional legitimacy even after the actual hazard coordination has been absorbed into local practice. theater_ratio≈0.35 reflects that while some performative elements exist (official warnings), the system retains functional core. d≈0.40, f(d)≈0.40, σ=0.9 → χ≈0.13.
constraint_indexing:constraint_classification(cow_field_poop, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cow_field_poop_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(cow_field_poop, TR),
    TR >= 0.70.

:- end_tests(cow_field_poop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Low-moderate. The constraint does not extract value from any agent — no party accumulates wealth or power through the hazard's existence. Extractiveness reflects only the slight burden of avoidance and hazard-awareness behavior. Suppression (0.28): Low-moderate. Agents have genuine options for managing the hazard (different routes, different times, protective equipment, risk acceptance). While not costless, suppression is not severe — alternatives exist. Theater ratio (0.35): Low. The constraint's functional content (hazard exists, avoidance is necessary) dominates performative layers. Warning signs and maintenance routines add some theater (~15-35% of total institutional activity), but core coordination remains functional. The modest theater increase over the interval reflects institutional drift toward bureaucratic self-perpetuation, not a fundamental degradation of the constraint's purpose.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify the constraint as Rope or Piton, with no significant perspectival gap. This is expected: the cow field hazard is a low-extraction, high-coordination problem that all agents recognize as coordination rather than coercion. The pragmatic user and landowner both see their interests as aligned (safe field use). The analytical observer recognizes the coordination function. The institutional perspective sees an early-stage Piton (functional coordination with emerging theater, but not yet severely degraded). The absence of snare or tangled_rope perspectives indicates that the constraint genuinely lacks extraction mechanisms — no agent is trapped or victimized. The absence of a mountain perspective indicates that while the hazard is 'natural' (emerges from biology), it is not immutable (can be engineered away, mitigated, or tolerated). The uniformity of classification across perspectives is a diagnostic sign that this is a pure coordination problem.
 *
 * DIRECTIONALITY LOGIC:
 *   Pragmatic field user: Beneficiary + mobile → d≈0.50, f(d)≈0.65. Symmetric: benefits from hazard information, pays modest cost of avoidance. Landowner: Beneficiary + arbitrage → d≈0.15, f(d)≈-0.01. Net beneficiary: owns the resource, has unilateral exit options (fencing, herd management). Institutional system: Institutional + constrained → d≈0.40, f(d)≈0.40. Moderate extraction index reflects growing theater as institutional layer becomes semi-autonomous from actual user needs. Analytical observer: d≈0.50, f(d)≈0.65. Symmetric observation position — sees equal benefits and costs across all agents.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_versus_artifact,
    'Is the cow field hazard an inevitable feature of biological cohabitation (natural law) or a contingent artifact of particular land-use patterns?',
    'Cross-cultural and historical comparison of hazard management approaches; analysis of whether absence of cows eliminates the coordination problem or merely transforms it',
    'If inevitable: constraint approaches mountain classification (immutable hazard). If contingent: constraint remains rope (contingent coordination challenge that could be engineered away). Current ε=0.32 and empirical evidence suggest rope is correct, but strong cultural narratives treat it as natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_versus_artifact, conceptual, 'Whether cow field hazard is inevitable or contingent on land use patterns').

omega_variable(
    hazard_distribution_homogeneity,
    'Is the hazard uniformly distributed (same risk everywhere in the field) or clustered (high-risk zones and safe zones)?',
    'Spatial mapping of hazard distribution; statistical analysis of random vs clustered occurrence patterns',
    'If uniform: pure coordination problem (everyone faces equal risk, needs shared understanding). If clustered: creates asymmetric exposure and potential for exploitation (some agents can navigate safely while others bear concentrated risk), shifting classification toward tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hazard_distribution_homogeneity, empirical, 'Spatial distribution of hazard within the field').

omega_variable(
    avoidance_versus_tolerance,
    'Does pragmatic avoidance require that agents stay out of the field entirely, or can they develop tolerance/immunity through repeated exposure?',
    'Behavioral studies of adaptation; analysis of whether hazard becomes less salient over time for experienced users',
    'If avoidance only: constraint maintains Rope classification (need for information coordination). If tolerance develops: constraint shifts toward Piton (institutional management becomes theater as practical users develop informal protocols that supersede formal warnings). Current theater_ratio suggests institutional layer is partially theatrical already.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(avoidance_versus_tolerance, empirical, 'Whether adaptation reduces functional hazard over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cow_field_poop, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cowfield_tr_t0, cow_field_poop, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cowfield_tr_t5, cow_field_poop, theater_ratio, 5, 0.25).
narrative_ontology:measurement(cowfield_tr_t10, cow_field_poop, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(cowfield_be_t0, cow_field_poop, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cowfield_be_t5, cow_field_poop, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(cowfield_be_t10, cow_field_poop, base_extractiveness, 10, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cow_field_poop, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
