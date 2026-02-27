% ============================================================================
% CONSTRAINT STORY: dead_sea_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dead_sea_effect, []).

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
 *   constraint_id: dead_sea_effect
 *   human_readable: The Dead Sea Effect (Talent Evaporation)
 *   domain: social/economic
 *
 * SUMMARY:
 *   The Dead Sea Effect represents a self-reinforcing organizational
 *   constraint where the most talented and mobile employees leave first,
 *   reducing organizational capability and accelerating subsequent
 *   departures. This creates a negative feedback loop: as top performers
 *   depart, the organization's attractiveness declines, making it harder to
 *   retain or recruit replacements, which accelerates further departures from
 *   the remaining talent cohort. The constraint exhibits both coordination
 *   and extraction functions: organizations require coordination of talent to
 *   deliver projects and innovation (rope function), but the talent
 *   evaporation mechanism extracts value from those trapped by mobility
 *   constraints (snare function). The constraint's base extractiveness
 *   increased from 0.28 to 0.52 over the measurement interval, reflecting
 *   growing asymmetry between those who can leave and those who remain.
 *   Theater ratio increased from 0.35 to 0.58, indicating that formal
 *   organizational structures (titles, promotion ladders, organizational
 *   charts) became increasingly performative as actual capability departed.
 *   The constraint is tangled because organizations actively maintain both
 *   coordination mechanisms (projects, knowledge systems) and extraction
 *   mechanisms (lock-in, differential pay structures, non-competes)
 *   simultaneously.
 *
 * KEY AGENTS:
 *   - Top Talent (Departed): Primary beneficiary at moment of departure (powerful/arbitrage) — captures option value through exit; net beneficiary through better opportunities elsewhere
 *   - Organization / Leadership: Structural beneficiary (institutional/arbitrage) — captures value from retained workforce through lock-in; maintains formal hierarchy
 *   - Stranded Mid-Tier Performers: Primary victim (powerless/trapped) — cannot afford departure; experience cascading workload and reduced organizational capability
 *   - High-Talent Non-Departure Cohort: Secondary victim (moderate/constrained) — stay due to commitment, family ties, or option constraints; experience extraction as peers leave
 *   - Organizational Capability (Collective): Victim (powerless/trapped) — projects and innovation degrade as talent evaporates; no exit option (abstract collective)
 *   - Formal Organizational Hierarchy: Institutional actor (institutional/arbitrage) — persists through inertia despite hollowing out; maintains theater ratio
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dead_sea_effect, 0.52).
domain_priors:suppression_score(dead_sea_effect, 0.65).
domain_priors:theater_ratio(dead_sea_effect, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dead_sea_effect, extractiveness, 0.52).
narrative_ontology:constraint_metric(dead_sea_effect, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(dead_sea_effect, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dead_sea_effect, tangled_rope).
narrative_ontology:human_readable(dead_sea_effect, "The Dead Sea Effect (Talent Evaporation)").
narrative_ontology:topic_domain(dead_sea_effect, "social/economic").

domain_priors:requires_active_enforcement(dead_sea_effect).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dead_sea_effect, organizational_leadership).
narrative_ontology:constraint_beneficiary(dead_sea_effect, remaining_mediocre_workforce).
narrative_ontology:constraint_victim(dead_sea_effect, organizational_capability).
narrative_ontology:constraint_victim(dead_sea_effect, high_talent_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRANDED MID-TIER PERFORMER (SNARE) — Cannot afford to leave (limited outside options, geographic/skill constraints, family obligations). Watches talent leave while organizational decline accelerates. d≈0.93, f(d)≈1.40, σ=0.8 → χ≈0.58. Trapped in degrading organization.
constraint_indexing:constraint_classification(dead_sea_effect, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: TOP TALENT / DEPARTED (ROPE) — Maximum exit options (offers elsewhere, global mobility, optionality). Experiences the constraint as pure coordination problem: staying requires staying coordinated with others at same level. Once coordination fails and peers leave, staying becomes irrational. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary through exit.
constraint_indexing:constraint_classification(dead_sea_effect, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ORGANIZATION / INSTITUTIONAL ACTOR (TANGLED ROPE) — Leadership deliberately or inadvertently creates conditions (performance rankings, differential pay, visible promotion of star exits) that accelerate talent departure while enforcing retention structures (non-competes, vesting cliffs, golden handcuffs) on remaining workers. Coordination function: pooling talent enables projects. Extraction: capturing value from those who cannot leave. d≈0.50, f(d)≈0.65, σ=0.9 → χ≈0.31. Hybrid extraction/coordination.
constraint_indexing:constraint_classification(dead_sea_effect, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: HIGH-TALENT NON-DEPARTURE COHORT (TANGLED ROPE) — Some high-talent individuals stay (family roots, local ties, genuine commitment). They experience cascading extraction as peers leave and workload increases. Also coordinate tacit knowledge transfer to remaining staff (coordination function). d≈0.62, f(d)≈0.85, σ=0.9 → χ≈0.39. Mixed: they enable projects (rope) but are extracted from as others leave.
constraint_indexing:constraint_classification(dead_sea_effect, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: FORMAL ORGANIZATIONAL HIERARCHY (PITON) — Organizational charts, titles, and promotion ladders persist despite being hollowed out by talent departure. Theater_ratio=0.58 reflects that formal structures (management layers, review processes, career paths) continue functioning but with degraded actual capability. The hierarchy is maintained through inertia: it was once functional, now performs institutional legitimacy rather than actual coordination. d≈0.30, f(d)≈0.20, σ=1.0 → χ≈0.12.
constraint_indexing:constraint_classification(dead_sea_effect, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE WITH COALITION UPGRADE) — From civilizational view, talent evaporation appears as inevitable physics: talented people always have better options, so they always leave. This would be Mountain. But the structural data (ε=0.52, suppression=0.65) shows this is NOT a natural law — it is a contingent institutional arrangement. The constraint is a Snare because it actively suppresses alternatives (lock-in mechanisms, non-competes, retaliation against leavers). The analytical observer also considers: if the high-talent agents could coordinate (unionize, collective exit, coordinated demands for improved conditions), they could convert their individual arbitrage power into collective constraint-breaking power. The threat of such coalition may actually drive accelerated departures (preemptive exit before coordination threat). d≈0.70, f(d)≈1.10, σ=1.2 → χ≈0.64.
constraint_indexing:constraint_classification(dead_sea_effect, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dead_sea_effect_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dead_sea_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dead_sea_effect, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dead_sea_effect, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dead_sea_effect, TR),
    TR >= 0.70.

:- end_tests(dead_sea_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The constraint extracts value through two mechanisms: (1) talent lock-in via vesting schedules, non-competes, and career investment; (2) increased work burden on remaining staff to compensate for departures. The value is not as high as pure predatory extraction (which would exceed 0.70) because organizations also invest in retaining remaining talent, but it is substantial. Suppression (0.65): High. Significant barriers to talent departure include: financial penalties (unvested equity), career reputation costs (labeling as 'flight risk'), geographic constraints (high-cost-of-living areas), family/social ties, healthcare/benefits lock-in, and non-compete agreements. These are real structural constraints on exit, though not absolute. Theater ratio (0.58): Moderate. Organizational structures (management hierarchies, promotion ladders, project assignment systems) continue despite being hollowed out by departures. The theater is not as high as traditional pitons (0.70+) because some real coordination still occurs, but it is substantial — organizational communication often occurs among the departed through informal channels while formal structures persist.
 *
 * PERSPECTIVAL GAP:
 *   The highest perspectival gap is between the Top Talent (Rope: d≈0.08, χ≈-0.06) and the Stranded Mid-Tier Performer (Snare: d≈0.93, χ≈0.58). From the top talent's view, the organization is a coordination problem — they need peer cohesion to achieve goals, and leaving is the rational response when coordination fails. From the stranded performer's view, the organization is pure extraction — they cannot afford to leave, and the constraint extracts their labor. The Organization as Institutional Actor (Tangled Rope: d≈0.50, χ≈0.31) occupies the middle ground: it coordinates projects but also extracts from those with constrained exit. The Analytical Observer initially risks perceiving this as Mountain (inevitable talent mobility) but the structural data reveals it as Snare with extraction mechanisms — suppression=0.65 shows this is not just 'talented people naturally leave' but 'talented people are suppressed from staying by institutional structures.'
 *
 * DIRECTIONALITY LOGIC:
 *   Top Talent + powerful + arbitrage: d≈0.08, f(d)≈-0.10. Strongly beneficiary. They have maximum options. Stranded Mid-Tier + powerless + trapped: d≈0.93, f(d)≈1.40. Strongly victim. Few options; bears extraction. High-Talent Non-Departure + moderate + constrained: d≈0.62, f(d)≈0.85. Significant victim status (constrained exit) but with some mitigation (moderate power, possible organizational investment in them). Organization + institutional + arbitrage: d≈0.50, f(d)≈0.65. Symmetric in terms of the index (both beneficiary and victim of coordination): they benefit from retaining talent but suffer from departures. The analytical observer at civilizational scope risks assigning Mountain because individual talent mobility seems like a law of nature, but the suppression=0.65 indicates active institutional constraint, not natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint is classified as Tangled Rope (claimed_type) to resolve the ambiguity between 'inevitable talent mobility' (which would be Mountain or Rope) and 'systematic extraction from those trapped' (which would be Snare). The tangled rope classification captures both: (1) COORDINATION FUNCTION: Organizations genuinely require talent coordination to execute projects. Top talent benefits from working together with other top talent. This is rope. (2) EXTRACTION FUNCTION: The organization maintains structures (lock-in, non-competes, differential visibility/opportunity) that extract value from those with limited exit options while allowing high-talent agents to extract option value through departure. This is snare. The mandatrophy is resolved by recognizing that this is NOT a choice between 'is it rope or snare?' but rather 'rope AND snare simultaneously, with asymmetric distribution: the coordination benefits accrue to those who stay (or benefit from departure timing), while extraction burdens fall on those trapped.' The theater_ratio increase (0.35→0.58) reveals an important degradation pattern: as top talent departs, the organizational structures that once coordinated actual work become increasingly performative. This is diagnostic of constraint degradation — the coordinate mechanism (projects, innovation) atrophies while extraction mechanisms (lock-in, reporting hierarchies) persist. If theater_ratio reaches 0.70 within the next measurement interval, the constraint transitions from Tangled Rope to Piton (degraded hybrid becoming degraded-inertial).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    talent_detection_threshold,
    'What metric distinguishes actual talent from organizational position? Can organizations accurately identify who is truly high-value vs. who merely appears high-value within organizational hierarchy?',
    'Post-departure career tracking: compare actual outside success (titles, compensation, publications, ventures) with organizational performance metrics used to identify ''talent''',
    'If high correlation: organizations accurately identify talent, and departures reflect real external options. If low correlation: organizations systematically misidentify talent, and departures may reflect organizational signaling errors rather than true capability differences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(talent_detection_threshold, empirical, 'Accuracy of talent identification metrics').

omega_variable(
    departure_causality_direction,
    'Does talent leave because the organization has declined, or does organizational decline occur because talent left? Which is cause vs. consequence?',
    'Longitudinal analysis of departure timing vs. organizational performance metrics (revenue, innovation, quality). Correlation of individual departure timing with organizational trajectory. Post-departure organization tracking.',
    'If talent departures PRECEDE decline: Dead Sea is the extraction cause. If organizational decline PRECEDES departures: departures are rational response to decline, not cause of it. If simultaneous: feedback loop.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(departure_causality_direction, empirical, 'Causal direction: talent evaporation causing or responding to decline').

omega_variable(
    retention_mechanism_efficacy,
    'Do formal retention structures (vesting cliffs, golden handcuffs, non-competes, promotion promises) actually prevent talent departure, or do they only delay it and increase departure friction?',
    'Comparison of departure rates with/without retention mechanisms; analysis of time-to-departure conditional on mechanism type; exit interview data on whether mechanisms influenced departure decision',
    'If effective: suppression value justified; retention structures are real constraints. If ineffective: suppression may be lower than claimed, and mechanisms only extract value from those retained (increasing snare character).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retention_mechanism_efficacy, empirical, 'Whether retention mechanisms prevent or merely delay talent departure').

omega_variable(
    coordinated_exit_possibility,
    'Can high-talent agents credibly coordinate collective exit or collective demands that would break the Dead Sea constraint, or is individual arbitrage always dominant strategy?',
    'Analysis of strikes, collective bargaining, industry-wide talent movements; game-theoretic modeling of defection incentives vs. coordination benefits; case studies of coordinated talent movements',
    'If coordination possible: high-talent agents are not powerless; constraint classification shifts from Snare toward Rope or Tangled Rope at the ''powerful/organized'' perspective. If coordination impossible (prisoner''s dilemma): high-talent agents remain trapped in individual exit logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinated_exit_possibility, empirical, 'Whether talented agents can coordinate to break the constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dead_sea_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dse_tr_t0, dead_sea_effect, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dse_tr_t5, dead_sea_effect, theater_ratio, 5, 0.47).
narrative_ontology:measurement(dse_tr_t10, dead_sea_effect, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(dse_be_t0, dead_sea_effect, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dse_be_t5, dead_sea_effect, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(dse_be_t10, dead_sea_effect, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dead_sea_effect, resource_allocation).
narrative_ontology:affects_constraint(dead_sea_effect, organizational_decline_cascade).
narrative_ontology:affects_constraint(dead_sea_effect, talent_lock_in_mechanisms).
narrative_ontology:affects_constraint(dead_sea_effect, morale_extraction_spiral).

% DUAL FORMULATION NOTE:
% The Dead Sea Effect is downstream of organizational structure constraints (how companies allocate resources, set compensation, design career paths) and upstream of organizational decline and institutional inertia. The separable constraint stories are: (1) talent_lock_in_mechanisms (ε≈0.35, Rope/Tangled Rope) — the structural mechanisms that suppress exit; (2) dead_sea_effect (ε≈0.52, Tangled Rope) — the emergent phenomenon of evaporation; (3) organizational_decline_cascade (ε≈0.58, Snare) — the institutional deterioration following talent loss. Each has distinct ε values because they measure different structural aspects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dead_sea_effect, powerful, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
