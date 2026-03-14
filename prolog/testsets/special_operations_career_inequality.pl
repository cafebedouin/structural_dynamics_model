% ============================================================================
% CONSTRAINT STORY: special_operations_career_inequality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_special_operations_career_inequality, []).

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
 *   constraint_id: special_operations_career_inequality
 *   human_readable: Special Operations Career Inequality and Selective Advancement
 *   domain: military/organizational/institutional
 *
 * SUMMARY:
 *   Special operations career advancement is formally governed by
 *   meritocratic criteria (performance evaluations, technical certifications,
 *   demonstrated leadership) but structurally mediated through informal
 *   networks of mentorship, access to visible positions, and information
 *   about advancement pathways. The constraint creates a hybrid mechanism:
 *   genuine operational coordination function (teams require cohesion, shared
 *   trust, cultural integration) is intertwined with systematic exclusion of
 *   geographically isolated or non-connected personnel from advancement
 *   opportunities. The extractiveness has increased over the 30-year interval
 *   (0.35 to 0.58) as operational demands have concentrated on specialized
 *   roles and selection pressure has intensified, making network access
 *   increasingly critical. Theater ratio has increased moderately (0.42 to
 *   0.55), reflecting the gap between formal meritocratic narrative and
 *   informal network-based reality. The diversity and inclusion initiatives
 *   from year 15 onward have begun pushing theater ratio downward (0.55 to
 *   0.48 at year 30), indicating structural reform is starting to reduce
 *   performative evaluation content and increase transparency.
 *
 * KEY AGENTS:
 *   - Isolated Operators: Primary victims (powerless/trapped) — geographically remote personnel without network access to decision-makers; bear full cost of advancement inequality without exit option
 *   - Regional Operators: Secondary victims (moderate/constrained) — regional base assignment limits network access; face constrained career mobility; benefit from some operational coordination infrastructure
 *   - Network-Connected Operators: Primary beneficiaries (organized/mobile) — integrated into mentorship networks and advancement information pipelines; experience career coordination and accelerated progression
 *   - Career Advancement Gatekeepers: Secondary beneficiaries (institutional/arbitrage) — senior command, selection boards, branch leadership benefit from leverage over talented subordinates; maintain network while selecting subordinates; enforce the gatekeeping structure
 *   - Formal Advancement System: Institutional theater (institutional/arbitrage) — official promotion processes, evaluation boards, and performance criteria maintain meritocratic appearance while network-based advancement continues underneath
 *   - Diversity and Inclusion Initiative: Organized reform actor (organized/constrained) — mandated diversity targets, structured mentorship, transparent selection criteria attempting to create alternative advancement pathways outside network gatekeeping
 *   - Analytical Observer: Cross-institutional perspective (analytical/analytical) — sees the constraint as genuinely hybrid: coordination function (team cohesion) captured by incumbent networks creating extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(special_operations_career_inequality, 0.58).
domain_priors:suppression_score(special_operations_career_inequality, 0.68).
domain_priors:theater_ratio(special_operations_career_inequality, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(special_operations_career_inequality, extractiveness, 0.58).
narrative_ontology:constraint_metric(special_operations_career_inequality, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(special_operations_career_inequality, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(special_operations_career_inequality, tangled_rope).
narrative_ontology:human_readable(special_operations_career_inequality, "Special Operations Career Inequality and Selective Advancement").
narrative_ontology:topic_domain(special_operations_career_inequality, "military/organizational/institutional").

domain_priors:requires_active_enforcement(special_operations_career_inequality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(special_operations_career_inequality, established_operator_networks).
narrative_ontology:constraint_beneficiary(special_operations_career_inequality, career_advancement_gatekeepers).
narrative_ontology:constraint_beneficiary(special_operations_career_inequality, institutional_command_structure).
narrative_ontology:constraint_victim(special_operations_career_inequality, geographically_isolated_operators).
narrative_ontology:constraint_victim(special_operations_career_inequality, non_networked_personnel).
narrative_ontology:constraint_victim(special_operations_career_inequality, merit_based_aspirants).
narrative_ontology:constraint_victim(special_operations_career_inequality, external_talent_pool).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED OPERATOR (SNARE) — Trapped by geographic isolation, lack of network access to decision-makers, and career structure that penalizes lateral movement. Cannot exit without abandoning specialization investment. Bears full extraction cost of the inequality mechanism without recourse.
constraint_indexing:constraint_classification(special_operations_career_inequality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL OPERATOR (TANGLED ROPE) — Constrained by regional base assignment and limited knowledge of advancement pathways. Benefits from operational coordination infrastructure but faces extraction through information asymmetry. Some agency but significant barriers to exit or advancement.
constraint_indexing:constraint_classification(special_operations_career_inequality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NETWORK-CONNECTED OPERATOR (ROPE) — Integrated into advancement networks; experiences the constraint as coordination mechanism. Benefits from information flow and mentorship pipelines. Mobility sufficient to navigate career progression. Net beneficiary with genuine coordination function.
constraint_indexing:constraint_classification(special_operations_career_inequality, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: CAREER ADVANCEMENT GATEKEEPER (TANGLED ROPE) — Institutional actor (senior command, selection board member, branch chief) benefits from network leverage and retention of talented subordinates through selective advancement. Enforces and maintains the network gatekeeping structure. High extraction leverage with minimal enforcement cost due to careerism and institutional inertia.
constraint_indexing:constraint_classification(special_operations_career_inequality, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL ADVANCEMENT SYSTEM (PITON) — Official promotion criteria, performance evaluations, and selection boards maintain the appearance of meritocracy while functioning primarily as theaters for network-based advancement decisions. The formal system persists through institutional inertia and legitimation theater despite the informal network being the actual mechanism. Theater ratio reflects substantial performative evaluation content.
constraint_indexing:constraint_classification(special_operations_career_inequality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DIVERSITY AND INCLUSION INITIATIVE (SCAFFOLD) — Organized reform effort (mandated diversity targets, structured mentorship programs, transparent selection criteria) creates temporary pathways bypassing network gatekeeping. Suppression declining as oversight mechanisms mature. Sunset logic: if transparent metrics and external review boards establish cultural norms, informal gatekeeping loses institutional legitimacy. Estimated sunset: 15-25 years for norm internalization.
constraint_indexing:constraint_classification(special_operations_career_inequality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Cross-institutional analysis reveals the constraint as a genuine hybrid: special operations teams require high cohesion and trust (coordination function, supporting rope classification) but the network mechanism systematically excludes qualified outsiders (extraction function, supporting snare/tangled rope classification). The coordination function is real but asymmetrically captured by incumbent networks, making this a canonical tangled rope.
constraint_indexing:constraint_classification(special_operations_career_inequality, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(special_operations_career_inequality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(special_operations_career_inequality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(special_operations_career_inequality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(special_operations_career_inequality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(special_operations_career_inequality, TR),
    TR >= 0.70.

:- end_tests(special_operations_career_inequality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Special operations advancement benefits from network integration, but the mechanism is not pure rent extraction — operational teams genuinely require cohesion and cultural integration, creating legitimate coordination function. Extractiveness reflects the asymmetric capture of this coordination benefit by incumbent networks, not total value transfer. The increase over 30 years reflects intensifying selection pressure and specialization, making network access more valuable. Suppression (0.68): High. Multiple suppression mechanisms operate: geographic isolation (assignment policies), information asymmetry (advancement criteria known to networks, hidden from others), career structure penalties for lateral movement or external hiring, and institutional inertia of network gatekeeping. Suppression is primarily structural rather than coercive. Theater ratio (0.55): Moderate. Formal evaluation processes and selection boards create performative meritocracy content — candidates are assessed against stated criteria that appear neutral. However, the informal network is where actual advancement decisions occur. Theater is lower than in purely performative systems because some genuine operational metrics (technical skills, performance in high-visibility positions) do correlate with advancement; the network simply ensures its members access those high-visibility positions first.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates perspectival divergence across all six types. The isolated operator sees pure extraction (Snare) — advancement is unavailable regardless of merit. The regional operator sees mixed coordination and extraction (Tangled Rope) — benefits from coordination infrastructure but disadvantaged by network structure. The network-connected operator sees coordination (Rope) — advancement feels meritocratic from within the network, and they genuinely benefit from cohesion. The gatekeeper sees beneficial coordination (Tangled Rope or Rope) — network maintenance generates value while controlling access. The formal system sees itself as meritocratic (Piton) — the evaluation theater persists because it legitimizes what is actually network-based advancement. The diversity initiative sees a temporary problem with a sunset (Scaffold) — transparent metrics and external oversight are eroding network gatekeeping. The analytical observer sees the full hybrid structure (Tangled Rope) — genuine coordination function captured asymmetrically by network incumbents. The perspectival gap is largest between the isolated operator (snare) and the network-connected operator (rope), revealing that the same structural constraint produces radically different experienced classifications based on network integration.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to the network gatekeeping mechanism. Isolated operators have d ≈ 0.92 (full target): geographic isolation, information asymmetry, no network leverage, trapped exit options. Regional operators have d ≈ 0.68 (mixed victim/beneficiary): partial network access, constrained mobility. Network-connected operators have d ≈ 0.25 (beneficiary): low extraction directed toward them, arbitrage exit options. Gatekeepers have d ≈ 0.15 (beneficiary): extraction flows toward them through network leverage. The formal system has d ≈ 0.20 (beneficiary): institutional position enables gatekeeping legitimation. The diversity initiative has d ≈ 0.55 (ambivalent): opposing the gatekeeping mechanism but structurally constrained by institutional power. The analytical observer's d ≈ 0.72 reflects the standard formula for analytical positions viewing a mixed constraint: not quite victim, not quite beneficiary, but structurally viewing the full apparatus. The directionality spread (0.15 to 0.92) explains the perspectival range from Rope (low d) to Snare (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely tangled: the coordination function is real (special operations teams require cohesion, trust, and cultural integration) but is asymmetrically captured by incumbent networks. The tangled rope classification prevents three errors: (1) calling it pure coordination (Rope) misses the systematic extraction of opportunity from non-connected personnel; (2) calling it pure extraction (Snare) misses the genuine operational coordination function; (3) calling it a temporary problem (Scaffold) misses that the network gatekeeping is structurally maintained by careerism, not a temporary policy defect. The measured extractiveness (0.58), suppression (0.68), and theater ratio (0.55) all support tangled rope classification: extractiveness is high but not maximal (coordination function accounts for some value); suppression is high (information asymmetry and geographic isolation are structural barriers); theater is moderate (performative evaluation exists but genuine operational metrics also correlate with advancement). The diversity initiative (scaffold perspective) is a genuine reform mechanism but does not resolve the mandatrophy — it addresses suppression and theater, not the underlying asymmetric capture of coordination benefits. A complete resolution would require either (a) decoupling advancement from network integration by making coordination benefits available to all (converting to Rope) or (b) acknowledging the extraction and compensating isolated operators (converting to Snare with explicit acknowledgment). Current trajectory suggests (a) is the intended reform path, supporting the scaffold sunset mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cohesion_vs_nepotism_boundary,
    'At what threshold does team cohesion requirement justify network-based advancement versus becoming a cover story for nepotism?',
    'Comparative performance analysis: outcome metrics for network-selected vs openly-selected operators; team cohesion measurements across diverse selection cohorts',
    'If cohesion requirement dominates: constraint is primarily rope with corruption at margins. If largely pretextual: constraint is primarily snare with coordination theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohesion_vs_nepotism_boundary, empirical, 'Boundary between legitimate cohesion requirements and nepotistic exclusion').

omega_variable(
    information_asymmetry_magnitude,
    'What proportion of career advancement variance is explained by information asymmetry versus actual performance differentials?',
    'Regression analysis controlling for objective performance metrics; longitudinal career tracking for operators with identical performance profiles but different network access',
    'If information asymmetry < 30%: suppression lower, constraint lighter. If > 60%: suppression higher, extraction more severe, classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_magnitude, empirical, 'Magnitude of information asymmetry in advancement outcomes').

omega_variable(
    external_talent_pool_accessibility,
    'Are qualified external operators systematically excluded or merely disadvantaged in access to information about advancement pathways?',
    'Analysis of hiring patterns for external talent; comparative advancement rates for identically-qualified internal vs external hires; attrition analysis',
    'If excluded: victim group definition is correct and extraction is severe. If information disadvantage: constraint is primarily suppression/coordination failure, not extraction per se.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(external_talent_pool_accessibility, empirical, 'Whether external talent faces exclusion or information disadvantage').

omega_variable(
    geographic_isolation_permanence,
    'Is geographic isolation structural (permanent assignment policies) or operational (temporary deployment consequence)?',
    'Analysis of rotation policies; comparison of geographic mobility between network members and isolated operators; policy documentation review',
    'If structural: isolation is enforced suppression mechanism, extraction is intentional. If operational: isolation is side effect of mission requirements, constraint may be lighter than classified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geographic_isolation_permanence, empirical, 'Whether geographic isolation is structural policy or operational consequence').

omega_variable(
    diversity_initiative_effectiveness,
    'Do structured mentorship and transparent selection criteria actually reduce network gatekeeping or merely create parallel advancement pathways?',
    'Longitudinal tracking of diversity initiative cohorts; advancement rate comparison with historical baselines; network penetration analysis for non-traditional candidates',
    'If effective: scaffold classification confirmed, sunset mechanism is real. If ineffective: initiatives become theater, piton classification dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_initiative_effectiveness, empirical, 'Whether diversity initiatives address underlying network gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(special_operations_career_inequality, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(socareer_tr_t0, special_operations_career_inequality, theater_ratio, 0, 0.42).
narrative_ontology:measurement(socareer_tr_t10, special_operations_career_inequality, theater_ratio, 10, 0.52).
narrative_ontology:measurement(socareer_tr_t20, special_operations_career_inequality, theater_ratio, 20, 0.55).
narrative_ontology:measurement(socareer_tr_t30, special_operations_career_inequality, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(socareer_be_t0, special_operations_career_inequality, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(socareer_be_t10, special_operations_career_inequality, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(socareer_be_t20, special_operations_career_inequality, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(socareer_be_t30, special_operations_career_inequality, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(special_operations_career_inequality, identity_coordination).
narrative_ontology:affects_constraint(special_operations_career_inequality, military_institutional_culture).
narrative_ontology:affects_constraint(special_operations_career_inequality, talent_retention_mechanisms).
narrative_ontology:affects_constraint(special_operations_career_inequality, organizational_cohesion_requirements).

% DUAL FORMULATION NOTE:
% Special operations career inequality can be decomposed into two structurally distinct constraints: (1) operational cohesion coordination (genuine network-based team selection to maintain trust and integration) and (2) career advancement gatekeeping (network-based access to visible positions and advancement information). The first is primarily rope; the second is primarily snare/tangled rope. This story treats them as a single tangled rope because they are operationally inseparable — the same network mechanism serves both functions simultaneously. Decomposition would require analyzing advancement separately from operational selection, which may be a future constraint family expansion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(special_operations_career_inequality, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
