% ============================================================================
% CONSTRAINT STORY: crew_mental_health_degradation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_crew_mental_health_degradation, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: crew_mental_health_degradation
 *   human_readable: Crew Mental Health Degradation in Long-Duration Missions
 *   domain: aerospace/occupational_health/organizational_behavior
 *
 * SUMMARY:
 *   Crew mental health degradation in long-duration missions (space,
 *   submarine, polar research, remote scientific stations) represents a
 *   structural constraint where organizational mission-completion imperatives
 *   systematically override crew psychological wellbeing. The constraint
 *   exhibits Tangled Rope characteristics: a genuine coordination function
 *   (organizing crew labor, maintaining psychological baseline for mission
 *   success) coexists with systematic extraction (psychological costs borne
 *   disproportionately by crew; organizational benefits from maintained
 *   mission schedule despite crew degradation). The theater_ratio (0.68)
 *   reflects performative mental health protocols that emphasize crew
 *   resilience and stoic selection while minimizing acknowledgment of actual
 *   psychological degradation during and after missions. The suppression
 *   (0.72) is high because crew face career consequences, contractual
 *   obligations, and cultural pressure to report only normalized stress
 *   levels. The extractiveness (0.58) reflects moderate but sustained
 *   extraction: crew psychological resources are extracted through
 *   confinement, isolation, circadian disruption, separation from family, and
 *   mission-critical prioritization over wellbeing — yet the extraction is
 *   not absolute (some crew complete multiple missions without catastrophic
 *   degradation) and some organizational investment in psychological support
 *   exists. The constraint's measurement trajectory shows degradation
 *   acceleration over time, with theater ratio increasing as protocols become
 *   more performative and extractiveness plateauing as organizational
 *   pressure to maintain mission schedule limits further psychological
 *   optimization.
 *
 * KEY AGENTS:
 *   - Individual Crew Members: Primary victims (powerless/trapped) — contract-bound, career-dependent, socially isolated during missions; bear full psychological costs with minimal exit options
 *   - Mission Operations Leadership: Primary beneficiaries (institutional/arbitrage) — control crew deployment timing and mission scope; benefit from maintained schedule and crew productivity despite psychological costs
 *   - Flight Surgeons / Medical Officers: Secondary actors (moderate/constrained) — face dual loyalty between medical ethics and organizational pressure; constrained by hierarchy and career incentives to clear crew for continued deployment
 *   - Crew Unions / Advocacy Groups: Organized resistance (organized/constrained) — collective power limited by mission-critical framing; benefit from membership but constrained by national security and organizational resistance
 *   - Mental Health Reform Initiatives: Institutional reformers (organized/constrained) — building alternative protocols with sunset logic; partially constrained by organizational inertia and funding limitations
 *   - Organizational Culture (Stoicism Narrative): Institutional actor (institutional/arbitrage) — maintains vestigial narrative of crew resilience through selection; benefits from continued mission prioritization over wellbeing; perceives itself as degraded (piton) but persists through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(crew_mental_health_degradation, 0.58).
domain_priors:suppression_score(crew_mental_health_degradation, 0.72).
domain_priors:theater_ratio(crew_mental_health_degradation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(crew_mental_health_degradation, extractiveness, 0.58).
narrative_ontology:constraint_metric(crew_mental_health_degradation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(crew_mental_health_degradation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(crew_mental_health_degradation, tangled_rope).
narrative_ontology:human_readable(crew_mental_health_degradation, "Crew Mental Health Degradation in Long-Duration Missions").
narrative_ontology:topic_domain(crew_mental_health_degradation, "aerospace/occupational_health/organizational_behavior").

domain_priors:requires_active_enforcement(crew_mental_health_degradation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(crew_mental_health_degradation, mission_operations).
narrative_ontology:constraint_beneficiary(crew_mental_health_degradation, organizational_leadership).
narrative_ontology:constraint_victim(crew_mental_health_degradation, crew_psychological_wellbeing).
narrative_ontology:constraint_victim(crew_mental_health_degradation, mission_safety_culture).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL CREW MEMBER (SNARE) — Trapped by contractual obligations, mission criticality, and career consequences of refusing deployment. Bears psychological costs (isolation, confinement stress, circadian disruption, separation from family) with no legitimate exit mechanism. Mission completion requirements override wellbeing. Maximum extraction experienced by the powerless actor with no alternatives.
constraint_indexing:constraint_classification(crew_mental_health_degradation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FLIGHT SURGEON / MEDICAL OFFICER (TANGLED ROPE) — Constrained by dual loyalty: medical oath to crew wellbeing vs. organizational pressure to clear crew for mission. Benefits from access to crew data and career advancement through mission success. Faces significant extraction through forced complicity in minimizing psychological risk. Some agency (can escalate concerns) but constrained by institutional hierarchy and career jeopardy.
constraint_indexing:constraint_classification(crew_mental_health_degradation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MISSION OPERATIONS LEADERSHIP (ROPE) — Experiences the constraint as coordination mechanism: managing crew psychological resources to maintain mission success. Net beneficiary with high arbitrage capacity (can defer, relocate, or redistribute crew). Sees mental health protocols as enabling mission coordination rather than constraining it. Extraction flows toward this institutional actor through maintained access to crew labor and attention.
constraint_indexing:constraint_classification(crew_mental_health_degradation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CREW UNIONS / ADVOCACY GROUPS (TANGLED ROPE) — Organized agents with some power (collective bargaining, media advocacy, regulatory engagement) but constrained by mission-critical framing and national security priorities. Benefit from membership dues and advocacy presence; experience extraction through limited enforcement power. Some agency (can publicize problems, negotiate protocols) but structural constraints limit victory scope.
constraint_indexing:constraint_classification(crew_mental_health_degradation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: MENTAL HEALTH REFORM INITIATIVES (SCAFFOLD) — Temporary support structures (pre-deployment screening improvements, in-mission peer support programs, post-mission psychological integration protocols) showing sunset architecture. High theater (training emphasizes protocols; actual use varies by mission culture) but genuinely lower extraction than traditional model because initiatives create alternative pathways for crew support. Suppression declines as initiatives mature — crew have more legitimate channels to surface distress.
constraint_indexing:constraint_classification(crew_mental_health_degradation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL STOICISM-AS-SELECTION CULTURE (PITON) — Vestigial institutional narrative that crew mental health challenges are individual weakness rather than structural extraction. The framing persists through inertia (astronaut/pilot culture idealization) despite evidence that psychological support improves outcomes. Theater ratio (0.68) reflects performative selection messaging ('we only choose psychologically resilient individuals') masking actual psychological degradation during missions. Function has atrophied — selection-based screening cannot prevent mission-induced degradation — but narrative persists.
constraint_indexing:constraint_classification(crew_mental_health_degradation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (NATURALIZATION RISK, APPEARS MOUNTAIN) — From a civilizational perspective, some degree of psychological stress in extreme environments may appear unchangeable — confinement, isolation, and separation from family are inherent to space/remote missions. Risks naturalizing contingent extraction as immutable fact of space exploration. However, the base properties contradict mountain classification: accessibility_collapse would require ≥0.85 (crew have well-established psychological support science); resistance would require ≤0.15 (organizational resistance to mental health prioritization is substantial). This is a false summit revealing naturalization bias.
constraint_indexing:constraint_classification(crew_mental_health_degradation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(crew_mental_health_degradation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(crew_mental_health_degradation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(crew_mental_health_degradation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(crew_mental_health_degradation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(crew_mental_health_degradation, TR),
    TR >= 0.70.

:- end_tests(crew_mental_health_degradation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Crew psychological resources are systematically extracted through confinement, isolation, family separation, circadian disruption, and mission-over-wellbeing prioritization. The extraction is not total (some crew complete multiple missions; some organizations invest in psychological support) and not clearly intentional (many organizations believe they are optimizing for mission success, not deliberately harming crew). Revised downward from initial 0.72 assessment due to genuine organizational investment in screening, support protocols, and post-mission integration — these represent partial coordination function alongside extraction. Suppression (0.72): High. Multiple barrier layers: contractual obligations prevent mid-mission exit; career consequences punish psychological disclosure (mission disqualification, peer judgment); cultural narrative frames distress as individual weakness; mission-critical framing justifies psychological cost acceptance. Crew have limited legitimate channels to report distress without jeopardizing mission or career. Theater_ratio (0.68): Moderate-high. Mental health protocols are substantially performative: pre-deployment screening emphasizes crew resilience; peer support training emphasizes coping and mission focus rather than psychological needs articulation; organizational communication emphasizes protocols rather than outcomes. Actual psychological support quality and availability vary widely by mission context. The measurements show theater increasing over time as organizational pressure to maintain mission schedule increases performative elements while reducing substantive psychological monitoring.
 *
 * PERSPECTIVAL GAP:
 *   The original research group sees coordination (Rope) — mission operations views psychological protocols as enabling crew labor coordination. Crew members see extraction (Snare) — their psychological resources are extracted with minimal consent or benefit. Flight surgeons see mixed coordination and extraction (Tangled Rope) — they must balance professional ethics with organizational pressure. The open science coalition analogue here is mental health reformers (Scaffold) — they are building alternative protocols with sunset logic. The journal editorial system analogue is organizational stoicism culture (Piton) — performative emphasis on crew resilience persists despite limited function. The naturalization risk appears at the analytical level (Mountain) — risks treating psychological stress as inherent to extreme environments rather than as contingent extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) flows from structural position. Individual crew members are trapped (highest d → highest f(d) → highest experienced extraction chi). They have no exit options and bear full psychological costs. Mission operations leadership has arbitrage exit (lowest d → low/negative f(d)) — they can defer, relocate, or redistribute crew, meaning the constraint subsidizes their operational flexibility. Flight surgeons face constrained exit and dual-victim status (moderate d). Advocacy groups have some organized power but constrained exit (moderate d, offset upward by organized power modifier). The psychological wellbeing itself is labeled as victim (abstract collective good that cannot exit) — it receives maximum d treatment in the engine's derivation chain. The directionality override is not needed — structural data (beneficiaries: mission_operations, organizational_leadership; victims: crew_psychological_wellbeing, mission_safety_culture) produces accurate d values through the standard derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that crew mental health degradation is genuinely a Tangled Rope constraint, not a failure of classification. The coordination function is real: organizing crew labor for mission success requires some psychological baseline and some organizational investment in crew welfare. The extraction is also real: crew bear disproportionate psychological costs; organizational leadership bears minimal psychological risk while capturing mission success benefits. The Snare perspective is the crew member's experiential reality — they experience near-total extraction. The Rope perspective is the organizational reality — they experience coordination benefits and moderate extraction. The Scaffold perspective is the reformer reality — they see temporary constraints being solved by cultural evolution. The Piton perspective is the organizational culture reality — stoicism narrative persists despite degraded function. No single type is 'correct' — the presheaf over observation sites is the answer. The engine correctly identifies this as Tangled Rope at the analytical level because the base properties contain BOTH genuine beneficiaries (mission operations) and genuine victims (crew wellbeing) and BOTH coordination function (organizing crew labor) and asymmetric extraction (psychological costs to crew, benefits to operations). The false summit (Mountain) would commit the naturalization error — treating contingent organizational choices about prioritizing mission over crew as immutable laws of space exploration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_individual_psychology,
    'Is crew mental health degradation primarily a structural extraction mechanism (organizational prioritizes mission over crew wellbeing) or primarily individual susceptibility variation (some crew members are psychologically resilient to confinement; others are not)?',
    'Longitudinal psychological assessment data comparing crew members across multiple missions; control groups of non-mission personnel exposed to similar confinement conditions; measurement of degradation rate variance between crew of similar selection profiles',
    'If structural: constraint remains Tangled Rope/Snare. If primarily individual: classification shifts toward Rope (coordination mechanism for sorting resilient from vulnerable crew). If mixed: determine proportion to calibrate extraction baseline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_individual_psychology, empirical, 'Whether mental health degradation is structural extraction or individual variation').

omega_variable(
    suppression_mechanism_type,
    'Is suppression of mental health challenges (0.72) primarily structural (career consequences, contractual lock-in, mission-critical justifications prevent reporting) or primarily internalized (crew have internalized the cultural narrative that psychological distress is individual weakness and self-censor reporting)?',
    'Anonymous psychological assessment during missions; comparison of reported vs undetected psychological distress; exit interviews and post-mission debriefs with psychological transparency protocols; measurement of distress reporting rates post-retirement vs during service',
    'If structural: suppression would decline if career consequences and contractual lock-in were removed. If internalized: crew would continue self-censoring even with external barrier removal — the constraint travels with the crew post-mission. Mixed suppression complicates extraction measurement and post-mission recovery.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_type, empirical, 'Whether suppression of mental health reporting is structural or internalized').

omega_variable(
    mission_success_causality,
    'Do crew mental health optimization and mission success have positive, neutral, or negative correlation? Is suppressing psychological support actually instrumental to mission success, or does optimization improve outcomes?',
    'Comparative analysis: missions with robust mental health protocols vs minimal protocols; outcome metrics (mission completion, safety incidents, crew performance, post-mission integration); cost-benefit analysis of psychological support investment vs mission risk reduction',
    'If positive correlation (support improves success): constraint classification shifts significantly downward in extractiveness — the beneficiaries are actually also the beneficiaries of crew wellbeing. Extraction would be revealed as organizational error rather than intentional. If neutral: extraction is real but unplanned side effect. If negative: extraction is intentional trade-off.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mission_success_causality, empirical, 'Correlation between crew mental health support and mission success').

omega_variable(
    identity_lock_in_astronaut_culture,
    'To what degree are crew members identity-locked into the role (their self-concept constituted through astronaut/mission identity) vs structurally trapped (external barriers to exit)?',
    'Post-career psychological assessment of identity transition; measurement of career-change motivation; analysis of crew retention post-mission vs exit rates after psychological degradation; identity restructuring requirements in post-mission integration',
    'If primarily identity-locked: crew cannot imagine themselves outside the role even after psychological degradation. Exit would require identity dissolution — increases long-term extraction costs. If primarily structurally trapped: exit barriers removal would increase voluntary exit and create pressure for working conditions improvement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_astronaut_culture, empirical, 'Whether crew psychological lock-in is identity-based or structural').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(crew_mental_health_degradation, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crew_mhd_theater_t0, crew_mental_health_degradation, theater_ratio, 0, 0.55).
narrative_ontology:measurement(crew_mhd_theater_t30, crew_mental_health_degradation, theater_ratio, 30, 0.64).
narrative_ontology:measurement(crew_mhd_theater_t60, crew_mental_health_degradation, theater_ratio, 60, 0.68).
narrative_ontology:measurement(crew_mhd_theater_t90, crew_mental_health_degradation, theater_ratio, 90, 0.62).

% Extraction over time
narrative_ontology:measurement(crew_mhd_extract_t0, crew_mental_health_degradation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(crew_mhd_extract_t30, crew_mental_health_degradation, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(crew_mhd_extract_t60, crew_mental_health_degradation, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(crew_mhd_extract_t90, crew_mental_health_degradation, base_extractiveness, 90, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(crew_mental_health_degradation, attachment_coordination).
narrative_ontology:boltzmann_floor_override(crew_mental_health_degradation, 0.12).
narrative_ontology:affects_constraint(crew_mental_health_degradation, organizational_risk_externalization).
narrative_ontology:affects_constraint(crew_mental_health_degradation, astronaut_recruitment_pipeline).

% DUAL FORMULATION NOTE:
% Crew mental health degradation is a distinct constraint from the organizational structures that enable it (risk externalization) and from the recruitment incentive structures that perpetuate it (astronaut pipeline idealization). All three are linked: organizational risk externalization creates structure that enables mental health extraction; the pipeline idealizes crew resilience, filtering for psychological traits compatible with extraction tolerance. The three constraints form a cluster where degradation of one increases pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(crew_mental_health_degradation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
