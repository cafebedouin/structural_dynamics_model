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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_reading
 *   human_readable: Preparedness as Live Exercised Knowledge (Competence Reading)
 *   domain: disaster_preparedness/institutional_memory/organizational_resilience
 *
 * SUMMARY:
 *   Preparedness as live exercised knowledge represents a structural
 *   commitment: organizations maintain operational capacity across
 *   generational transitions by repeatedly drilling complex procedures under
 *   realistic conditions. This constraint simultaneously coordinates
 *   essential knowledge transmission and extracts significant costs from
 *   personnel and operations. The competence reading instantiates the case
 *   where drills genuinely test decision-making, reveal operational gaps, and
 *   transmit tacit knowledge that cannot be fully documented. This reading
 *   assumes that live exercise is not performative ritual but functional
 *   verification — that the drills actually work to maintain adaptive
 *   capacity and prevent the catastrophic institutional discontinuities (D5
 *   breaks) that occur when organizational memory fails across personnel
 *   turnover. The constraint exhibits a tangled rope structure: genuine
 *   coordination function (testing real decisions, transmitting tacit
 *   knowledge, revealing gaps) coexists with asymmetric extraction (junior
 *   personnel time, organizational schedule disruption, ongoing enforcement
 *   requirements). The theater ratio remains moderate (0.38) because the
 *   competence reading assumes that drill outcomes directly feed operational
 *   strategy — the exercise is not mainly performative, though performative
 *   elements (after-action reviews, formal certifications) accumulate over
 *   time. The sibling readings (husk_reading, hybrid_reading) instantiate
 *   alternative framings where the drill mechanism becomes degraded or mixed;
 *   those are separate constraint stories with different epsilon values. This
 *   story models the case where preparedness drills work.
 *
 * KEY AGENTS:
 *   - Operational Leadership: Primary beneficiary (institutional/arbitrage) — benefits from demonstrable competence assurance, actionable intelligence about gaps, strategic planning data; can exit or scale drill programs without career penalty
 *   - Junior Personnel: Primary victim (powerless/trapped) — trapped in recurring drill cycles with no discretion; time extraction without visible reward; career advancement depends on compliance with opaque competence standards
 *   - Mid-Level Coordinator: Secondary victim (moderate/constrained) — resources and burden for training infrastructure; constrained by schedule disruptions and resource scarcity; also benefits from improved operational clarity
 *   - Training Standards Body: Organized agent (organized/constrained) — sees drills as transitional mechanism with sunset logic; embedded simulation and documented procedures will eventually replace live exercises
 *   - Organizational Capacity Commons: Structural victim — abstract collective good (readiness, decision-making speed, adaptive response capability) that is maintained through the constraint but not perceived as beneficiary
 *   - Institutional Ritual Layer: Performs the preparedness function at civilizational scale but risks degradation into pure theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_reading, 0.28).
domain_priors:suppression_score(competence_reading, 0.35).
domain_priors:theater_ratio(competence_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(competence_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(competence_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_reading, tangled_rope).
narrative_ontology:human_readable(competence_reading, "Preparedness as Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(competence_reading, "disaster_preparedness/institutional_memory/organizational_resilience").

domain_priors:requires_active_enforcement(competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(competence_reading, distributed).
narrative_ontology:cs_authority_grounding(competence_reading, practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_reading, operational_leadership).
narrative_ontology:constraint_beneficiary(competence_reading, trained_personnel).
narrative_ontology:constraint_victim(competence_reading, organizational_capacity_commons).
narrative_ontology:constraint_victim(competence_reading, junior_personnel_time).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JUNIOR PERSONNEL (SNARE) — Trapped in recurring drill cycles with no discretion over participation. Extractive loop: time investment in repetitive drills with no visible reward; career advancement depends on compliance with an opaque competence standard. Cannot exit without career penalty. No coordination benefit perceived — only extraction of labor and attention.
constraint_indexing:constraint_classification(competence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MID-LEVEL COORDINATOR (TANGLED ROPE) — Constrained by resource scarcity and training burden; experiences genuine coordination function (drills do test real decision-making and reveal gaps) alongside extraction (required to justify drills to skeptical leadership, absorb schedule disruptions, maintain training infrastructure). Both costs and benefits are substantial. Some exit available (transfer to non-drill-intensive roles) at moderate career cost.
constraint_indexing:constraint_classification(competence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: OPERATIONAL LEADERSHIP (ROPE) — Benefits from demonstrable competence assurance. Drills provide actionable intelligence about gaps in operational capacity; live exercise data feeds strategic planning. Low-cost arbitrage exit: leadership can scale or repurpose drill programs without loss of status or institutional position. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRAINING STANDARDS BODY (SCAFFOLD) — Organized agents (certification programs, regulatory bodies, inter-agency working groups) see preparedness drills as a transitional coordination mechanism with clear sunset logic: as organizational memory institutionalizes (documented procedures, digital system handoff, embedded simulation capabilities), the need for live drills declines. Current suppression (procedural overhead, scheduling friction) is tolerated because the sunset path is visible. Theater ratio remains moderate as standards bodies distinguish performative compliance from functional verification.
constraint_indexing:constraint_classification(competence_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL RITUAL LAYER (PITON) — At the civilizational scale, the preparedness drill has become substantially performative. Originally a genuine verification mechanism, it persists through institutional inertia despite degraded function: after-action reviews often repeat findings from previous cycles; recommendations are filed but not systematically implemented; the theater of 'being prepared' substitutes for actual adaptive capacity. High theater ratio (0.68 at civilizational scale) reflects that the ritual itself has become the measure of preparedness rather than actual operational verification.
constraint_indexing:constraint_classification(competence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some rehearsal of complex operations is inherent to maintaining readiness: knowledge degrades without exercise, personnel turn over, novel situations demand adaptive capacity. This perspective sees preparedness drills as an immutable property of complex systems resilience — a natural law of organizational decay and renewal. However, the structural data (identified beneficiaries, enforcement requirements, theater accumulation) contradicts this naturalization. The engine will detect this as a false summit candidate.
constraint_indexing:constraint_classification(competence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(competence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(competence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(competence_reading, TR),
    TR >= 0.70.

:- end_tests(competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The competence reading assumes that drills genuinely serve a coordination function (test real decisions, transmit tacit knowledge, reveal operational gaps) alongside extraction (personnel time, schedule disruption). The value reflects that both flows are substantial but neither dominates. The original observational basis would show measurable improvements in decision-making quality and operational gap identification correlated with drill participation. If empirical evidence shows drills fail to predict real-world performance or recommendations are not implemented, extractiveness would rise toward husk_reading values (~0.65). Suppression (0.35): Moderate. Barriers to exit or refusal include career penalties, compliance requirements, and institutional culture that normalizes participation. But suppression is not severe — personnel can (at cost) transfer to roles with reduced drill requirements, and organizational leadership can (at risk) deprioritize drills. Theater ratio (0.38): Moderate. Reflects that drills combine genuine verification (live decision-making under realistic conditions) with performative elements (after-action reviews, formal certifications, ritual repetition). The theater ratio increases gradually over the 15-year interval (0.25 → 0.38) as organizational routines ossify and younger personnel encounter repeated drill scenarios without novel learning. This trajectory is consistent with competence reading: as tacit knowledge transfers successfully and procedures stabilize, the theatrical component naturally increases because fewer genuinely novel gaps emerge.
 *
 * PERSPECTIVAL GAP:
 *   Operational leadership perceives rope (coordination, actionable intelligence, low-cost arbitrage) while junior personnel perceive snare (extraction, time burden, no escape). Mid-level coordinators perceive tangled rope (both coordination and extraction at substantial levels). Training standards bodies perceive scaffold (temporary coordination problem being solved by procedure documentation and embedded simulation). The institutional ritual layer at civilizational scale risks piton classification (performative persistence of degraded function). The analytical observer risks false-summit mountain classification (naturalizing the necessity of live drills as inherent to organizational resilience). The perspectival gap reveals that the 'necessity' of live drills depends entirely on whether they actually improve decision-making (competence reading, supported by empirical data about gap detection and implementation) or merely simulate preparation (husk reading, where theater ratio >> 0.70 and benefits concentrate entirely on leadership).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from their structural position: operational leadership are beneficiaries with arbitrage exit (low d, negative chi), junior personnel are victims with trapped exit (high d, high chi), mid-level coordinators are both beneficiaries and victims with constrained exit (moderate d, moderate chi). Organized agents with clear sunset paths experience moderate effective extraction despite moderate base extraction (the scaffold structure makes the extraction temporally bounded). The piton perspective at civilizational scale has high theater_ratio but moderate baseline extractiveness — the classification shift depends on whether the ritual has become divorced from functional verification. The mountain perspective at analytical/civilizational scale would derive d from canonical analytical value (0.73), producing false-summit red flags when beneficiaries are declared. The competence reading assumes that beneficiary declarations are legitimate (leadership genuinely benefits from actionable intelligence) and that extracted value correlates with generated value (drills actually improve organizational capacity). The husk reading would contest both of these assumptions.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing this reading from its siblings. The competence reading resolves the tension between 'drills are necessary for readiness' and 'drills consume massive organizational resources' by declaring that the resources are extractive but justified: the drills produce genuine coordination benefits (gap identification, tacit knowledge transfer, decision-making verification) that measurably improve operational capacity. The classification as tangled_rope is stable across the competence reading precisely because both extraction and coordination are real and substantial. If empirical analysis shows that drills fail to identify gaps (high false-negative rate against real incidents) or recommendations go unimplemented, the classification would shift toward snare/piton (husk_reading). If analysis shows mixed performance with some drills working and others becoming purely ritual, the classification would shift toward hybrid_reading. The mandatrophy prevention here is categorical: declare which reading you are instantiating (competence, husk, or hybrid) and specify the empirical observable that would differentiate them (drill-to-incident outcome correlation, recommendation implementation rate, tacit knowledge transfer effectiveness). Do not collapse all three readings into a single constraint with measurement uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_versus_husk_reading,
    'Does this preparedness regime instantiate genuine competence maintenance (this reading) or performative husk maintenance (sibling reading)? What observable distinguishes them?',
    'Compare drill-to-operational-incident outcomes: if drills reveal and correct gaps that later appear in real incidents, competence reading holds; if drills fail to predict or prevent real-world failures despite passing all exercises, husk reading holds. Measure gap-detection rate and implementation rate of recommendations.',
    'Competence reading: extractiveness remains ~0.28 (tangled rope primary). Husk reading: extractiveness rises to ~0.65 (snare primary), theater ratio reaches ~0.80+, beneficiary list narrows to leadership only. The classification hinge depends on whether the drills actually improve decision-making or merely create the appearance of preparation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_versus_husk_reading, empirical, 'Whether drills maintain genuine competence or merely simulate preparation').

omega_variable(
    generational_knowledge_transfer_mechanism,
    'Is the primary knowledge transfer mechanism through live drills (this reading assumes yes) or through documented procedures, embedded systems, and digital handoff (would shift toward scaffold/piton)?',
    'Track knowledge loss across generational turnover: compare decision-making quality of personnel trained via live drills vs. those trained via documentation and simulation. Measure the tacit knowledge that drills transmit but cannot be documented.',
    'If tacit/embodied knowledge is critical and only drills transmit it: competence reading holds, extractiveness justified. If most competence-critical knowledge can be documented: extractiveness may be pure extraction (husk reading), and scaffold sunset logic becomes dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_knowledge_transfer_mechanism, empirical, 'Whether live drills are necessary for competence maintenance').

omega_variable(
    this_reading_versus_sibling_readings,
    'This constraint instantiates the competence reading of the preparedness_commitment kernel. What distinguishes it from husk_reading and hybrid_reading?',
    'The competence reading assumes that live drills test real decision-making, reveal genuine operational gaps, and transmit tacit knowledge essential for adaptive capacity. The husk_reading assumes that drills create performative assurance while actual competence decays (theater_ratio >> 0.70, beneficiaries narrow). The hybrid_reading assumes both mechanisms coexist with ongoing tension. The three readings have different epsilon values, different beneficiary/victim structures, and different dominant types across perspectives.',
    'Competence: ε~0.28, tangled_rope primary, theater_ratio moderate (0.38). Husk: ε~0.65, snare/piton primary, theater_ratio high (0.75+). Hybrid: ε~0.50, mixed perspectives, escalating theater over time. Different sibling readings imply different interventions: competence reading suggests optimization of drill design; husk reading suggests ritual abolition; hybrid reading suggests structural reforms to decouple verification from performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(this_reading_versus_sibling_readings, conceptual, 'This reading versus sibling readings of the preparedness kernel').

omega_variable(
    d5_organizational_break_risk,
    'Under what conditions would generational turnover cause a D5 organizational discontinuity (loss of critical procedural knowledge and decision-making capacity)?',
    'Historical case analysis: compare organizations that experienced vs. avoided sudden capability loss during major transitions. Measure knowledge transfer success rate; identify failure modes (key personnel departure, undocumented procedures, tacit knowledge erosion). Test whether competence-reading preparedness regimes (live exercise focus) correlate with lower discontinuity risk than husk-reading regimes (documented-procedures-only).',
    'If D5 breaks are a real risk mitigated by live drills: competence reading validated, extractiveness cost justified. If D5 breaks can be prevented through documentation and transition management without live drills: competence reading overstates necessity, overlaps with husk reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(d5_organizational_break_risk, empirical, 'Risk of D5 organizational discontinuity and role of live drills').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(comp_tr_t5, competence_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(comp_tr_t10, competence_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(comp_tr_t15, competence_reading, theater_ratio, 15, 0.45).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(comp_be_t5, competence_reading, base_extractiveness, 5, 0.23).
narrative_ontology:measurement(comp_be_t10, competence_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(comp_be_t15, competence_reading, base_extractiveness, 15, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_reading, 0.12).
narrative_ontology:affects_constraint(competence_reading, husk_reading).
narrative_ontology:affects_constraint(competence_reading, hybrid_reading).
narrative_ontology:affects_constraint(competence_reading, d5_organizational_discontinuity).

% DUAL FORMULATION NOTE:
% The preparedness_commitment kernel admits three structurally distinct constraint readings. The competence_reading assumes live drills genuinely maintain operational capacity and justify their extraction costs. The husk_reading assumes drills have become performative with theater_ratio >> 0.70 and narrowed beneficiaries. The hybrid_reading assumes mixed performance with oscillating classifications. The three readings have different epsilon values (0.28, 0.65, 0.50 respectively) and different dominant types. They are not observational variants of a single constraint but competing empirical hypotheses. See the sibling constraint files for husk_reading and hybrid_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
