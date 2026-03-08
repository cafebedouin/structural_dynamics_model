% ============================================================================
% CONSTRAINT STORY: informant_recruitment_through_false_solidarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_informant_recruitment_through_false_solidarity, []).

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
 *   constraint_id: informant_recruitment_through_false_solidarity
 *   human_readable: Informant Recruitment Through False Solidarity
 *   domain: labor_relations/institutional_control/surveillance
 *
 * SUMMARY:
 *   Informant recruitment through false solidarity is a labor control
 *   mechanism where institutional actors (management, HR, security
 *   consultants) adopt worker identity markers — shared grievances,
 *   union-friendly language, working-class cultural signifiers — to elicit
 *   trust from workers, then extract intelligence about organizing activity
 *   under the pretense of collective action. The practice operates through
 *   identity exploitation: the authority figure presents as 'one of us' to
 *   bypass workers' rational distrust of management. This constraint is
 *   downstream of debt_leverage_as_consent_manufacturing — workers already
 *   economically vulnerable through debt are particularly susceptible to
 *   recruitment because the institutional actor can offer both solidarity
 *   performance and material relief (shift preferences, overtime access, debt
 *   assistance) in exchange for intelligence. The constraint exhibits high
 *   theater ratio (0.75) because the solidarity performance is increasingly
 *   transparent to experienced organizers, yet institutions maintain the
 *   practice because it occasionally succeeds and because alternatives (overt
 *   surveillance) are legally and politically riskier. The extraction
 *   mechanism is the betrayal of trust: workers share information they would
 *   never share with acknowledged management, enabling targeted disruption of
 *   organizing before it reaches critical mass.
 *
 * KEY AGENTS:
 *   - Recruited Informant: Primary victim (powerless/identity_locked) — identity fused with dual role; cannot exit without abandoning worker community or institutional protection; experiences continuous extraction through identity trap
 *   - Targeted Organizer: Primary victim (powerless/trapped) — economically dependent and surveillance-exposed; faces career damage and blacklisting; no exit options
 *   - Worker Solidarity Network: Secondary victim (moderate/constrained) — experiences both coordination gains and extraction losses; can exit at high cost to organizing capacity
 *   - Institutional Surveillance Apparatus: Primary beneficiary (institutional/arbitrage) — captures intelligence advantage; experiences constraint as coordination; can exit to alternative methods without cost
 *   - Management Intelligence Operations: Secondary beneficiary (institutional/arbitrage) — uses extracted intelligence for strategic disruption of organizing
 *   - Labor Rights Coalition: Organized agents (organized/mobile) — building alternative pathways through legal protections and digital organizing tools; sees sunset in 15-25 years
 *   - Human Resources Department: Institutional actor (institutional/constrained) — sees practice as degraded; maintains through inertia; high theater ratio
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies both genuine coordination function and asymmetric extraction; notes that transparent mechanisms could serve coordination without betrayal
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(informant_recruitment_through_false_solidarity, 0.58).
domain_priors:suppression_score(informant_recruitment_through_false_solidarity, 0.68).
domain_priors:theater_ratio(informant_recruitment_through_false_solidarity, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(informant_recruitment_through_false_solidarity, extractiveness, 0.58).
narrative_ontology:constraint_metric(informant_recruitment_through_false_solidarity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(informant_recruitment_through_false_solidarity, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(informant_recruitment_through_false_solidarity, tangled_rope).
narrative_ontology:human_readable(informant_recruitment_through_false_solidarity, "Informant Recruitment Through False Solidarity").
narrative_ontology:topic_domain(informant_recruitment_through_false_solidarity, "labor_relations/institutional_control/surveillance").

domain_priors:requires_active_enforcement(informant_recruitment_through_false_solidarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(informant_recruitment_through_false_solidarity, institutional_surveillance_apparatus).
narrative_ontology:constraint_beneficiary(informant_recruitment_through_false_solidarity, management_intelligence_operations).
narrative_ontology:constraint_victim(informant_recruitment_through_false_solidarity, worker_solidarity_networks).
narrative_ontology:constraint_victim(informant_recruitment_through_false_solidarity, recruited_informants).
narrative_ontology:constraint_victim(informant_recruitment_through_false_solidarity, targeted_organizers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RECRUITED INFORMANT (SNARE) — Identity-locked through fusion of worker identity with institutional role. Cannot exit without abandoning both peer relationships and institutional protection. The informant's self-concept depends on maintaining dual identity: authentic worker to peers, trusted intelligence source to management. Exit would require becoming a different person — either abandoning worker community or losing institutional favor. The constraint extracts continuously through this identity trap.
constraint_indexing:constraint_classification(informant_recruitment_through_false_solidarity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: TARGETED ORGANIZER (SNARE) — Trapped by economic dependency and surveillance exposure. Cannot exit workplace without losing livelihood; cannot organize without triggering retaliation enabled by informant intelligence. Faces maximum extraction: career damage, blacklisting, and loss of organizing capacity. No exit options and no coordination benefit — pure extraction mechanism.
constraint_indexing:constraint_classification(informant_recruitment_through_false_solidarity, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: WORKER SOLIDARITY NETWORK (TANGLED ROPE) — Constrained by need for trust-based organizing while facing infiltration risk. The network genuinely coordinates collective action (rope function) but simultaneously experiences extraction through intelligence leakage and strategic disruption. Can exit through disbanding or going underground, but at high cost to organizing capacity. Mixed experience: real coordination gains alongside real extraction losses.
constraint_indexing:constraint_classification(informant_recruitment_through_false_solidarity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: INSTITUTIONAL SURVEILLANCE APPARATUS (ROPE) — Primary beneficiary with arbitrage exit options. Experiences the constraint as pure coordination: recruiting informants solves the legitimate institutional problem of maintaining workplace order and preventing disruption. Can exit to alternative surveillance methods (digital monitoring, external consultants) without cost. Sees the practice as necessary labor relations management, not extraction.
constraint_indexing:constraint_classification(informant_recruitment_through_false_solidarity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR RIGHTS COALITION (SCAFFOLD) — Organized advocacy groups and legal aid organizations see this as a temporary problem with declining effectiveness. Whistleblower protections, NLRB enforcement of surveillance prohibitions, and digital organizing tools that bypass face-to-face trust requirements are creating alternative pathways. The constraint's power diminishes as workers gain legal recourse and organizing moves to encrypted platforms. Estimated sunset: 15-25 years as legal protections mature and digital organizing becomes normalized.
constraint_indexing:constraint_classification(informant_recruitment_through_false_solidarity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: HUMAN RESOURCES DEPARTMENT (PITON) — Sees the practice as degraded and increasingly theatrical. The 'employee engagement' and 'open door policy' rituals that once genuinely gathered intelligence now mostly generate performative data. Workers have learned to identify false solidarity signals; informant recruitment has become harder and less reliable. HR maintains the practice through institutional inertia and because alternatives (digital surveillance, external labor consultants) are politically sensitive. High theater ratio: the solidarity performance persists despite declining functional intelligence value.
constraint_indexing:constraint_classification(informant_recruitment_through_false_solidarity, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, this constraint exhibits both genuine coordination function (institutions do need information about workplace dynamics to function) and asymmetric extraction (the method systematically exploits trust and identity to extract intelligence that harms the intelligence sources' collective interests). The coordination function is real but could be served by transparent mechanisms (surveys, elected worker councils, formal grievance procedures). The extraction is the delta between what transparent coordination would cost and what false solidarity extraction achieves: the difference is captured by the institution through betrayal of trust.
constraint_indexing:constraint_classification(informant_recruitment_through_false_solidarity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(informant_recruitment_through_false_solidarity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(informant_recruitment_through_false_solidarity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(informant_recruitment_through_false_solidarity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(informant_recruitment_through_false_solidarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(informant_recruitment_through_false_solidarity, TR),
    TR >= 0.70.

:- end_tests(informant_recruitment_through_false_solidarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through multiple channels: (1) recruited informants experience identity damage and social alienation, (2) targeted organizers face career retaliation and blacklisting, (3) worker solidarity networks lose organizing capacity through intelligence leakage and strategic disruption. The extraction is substantial but not maximal — some organizing succeeds despite infiltration, and some informants maintain functional dual identities. The value reflects that extraction is severe for direct victims but the practice's declining effectiveness (rising theater ratio) limits aggregate extraction. Suppression (0.68): High. Workers face significant barriers to organizing: economic dependency (cannot risk job loss), surveillance exposure (informant intelligence enables targeted retaliation), social fragmentation (distrust undermines solidarity), and legal constraints (NLRB protections are weak and enforcement is slow). But suppression is not total — some workers organize successfully, labor rights coalitions provide legal support, and digital tools create new organizing pathways. Theater ratio (0.75): High. The solidarity performance is increasingly performative. Experienced organizers have learned to identify false solidarity signals: authority figures who adopt worker language but lack authentic grievance history, who ask leading questions about other workers, who appear at organizing meetings but never face retaliation. The practice persists because it occasionally succeeds with inexperienced workers and because overt alternatives are riskier, not because it reliably produces intelligence.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how identity exploitation creates divergent experiences of the same structural mechanism. The institutional surveillance apparatus sees pure coordination (Rope) — recruiting informants solves the legitimate problem of maintaining workplace order. The recruited informant sees a snare — identity-locked through fusion of worker and institutional roles, experiencing continuous extraction through betrayal of peers. The targeted organizer sees a snare — trapped by economic dependency and surveillance exposure, facing maximum extraction with no coordination benefit. The worker solidarity network sees tangled rope — genuine coordination (collective action) alongside extraction (intelligence leakage). The labor rights coalition sees scaffold — a temporary problem with declining effectiveness as legal protections and digital tools mature. The HR department sees piton — a degraded practice maintained through inertia despite declining functional value. The analytical observer sees tangled rope — real coordination function (institutions need workplace information) served through extractive means (betrayal of trust) when transparent alternatives exist. The perspectival gap reveals that 'solidarity' means fundamentally different things depending on whether you're performing it to extract intelligence or experiencing it as betrayal.
 *
 * DIRECTIONALITY LOGIC:
 *   The recruited informant is identity_locked rather than trapped because the binding mechanism is cognitive: the informant's self-concept is constituted through the dual identity (authentic worker to peers, trusted source to management). A trapped agent faces external barriers; an identity_locked agent cannot exit because exit would require abandoning the identity they've constructed. The informant could physically leave the workplace (they have the same economic mobility as other workers) but cannot leave the informant role without psychological crisis — they would have to admit to themselves and potentially to peers that they betrayed trust. This identity fusion is the extraction mechanism: the institution captures ongoing intelligence not through continued material incentives but through the informant's need to maintain identity coherence. The targeted organizer is trapped rather than identity_locked because their barriers are material: economic dependency (job loss means inability to pay rent, support family) and surveillance exposure (informant intelligence creates blacklist risk across employers). The organizer sees the extraction clearly — they are not cognitively captured — but cannot exit because the costs are insurmountable. The worker solidarity network is constrained rather than trapped because the network can exit (disband, go underground, shift to digital organizing) but at high cost to organizing capacity. The network has agency and options, but all options involve significant sacrifice. The institutional surveillance apparatus is arbitrage because it can exit to alternative methods (digital monitoring, external consultants, overt surveillance) without losing core function. The institution is not dependent on this specific practice — it's one tool among many.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the coordination function (institutional information-gathering for workplace management) is real but could be served by transparent mechanisms (worker councils, anonymous surveys, formal grievance procedures). The extraction is the delta between what transparent coordination would cost and what false solidarity extraction achieves. Transparent mechanisms would require institutions to negotiate with workers as acknowledged parties with legitimate interests; false solidarity extraction allows institutions to gather intelligence while maintaining the fiction of unilateral management authority. The coordination claim is not false — institutions do need information about workplace dynamics — but the method is extractive because it achieves coordination through betrayal rather than through acknowledged exchange. The tangled rope classification captures this: genuine coordination function exists (not pure snare) but is inseparable from asymmetric extraction (not pure rope). The analytical perspective confirms this by noting that the coordination function persists across all institutional forms (even worker cooperatives need information about workplace dynamics) but the extraction mechanism is specific to hierarchical institutions that cannot or will not use transparent information-gathering methods.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_durability,
    'How long does the recruited informant''s identity fusion persist after the institutional relationship ends?',
    'Longitudinal interviews with former informants; psychological assessment of identity integration vs compartmentalization; tracking of post-employment social reintegration patterns',
    'If identity lock is temporary (resolves within 1-2 years): extraction is bounded and informant can recover. If permanent (identity damage persists indefinitely): extraction is catastrophic and informant becomes permanently alienated from both worker and institutional communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_durability, empirical, 'Duration of identity fusion after institutional relationship ends').

omega_variable(
    coordination_function_necessity,
    'Is the institutional intelligence-gathering function genuinely necessary for workplace coordination, or is it primarily a control mechanism?',
    'Comparison of workplace outcomes (safety, productivity, conflict resolution) in organizations with transparent information-gathering (worker councils, anonymous surveys) vs covert informant networks; analysis of what information is actually used for operational decisions vs disciplinary action',
    'If genuinely necessary: tangled rope classification confirmed — real coordination function exists. If primarily control: classification shifts toward snare — coordination claim is cover story for pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_necessity, conceptual, 'Whether intelligence function serves coordination or control').

omega_variable(
    digital_organizing_substitution,
    'Do encrypted digital organizing tools actually reduce vulnerability to informant infiltration, or do they create new surveillance vectors?',
    'Analysis of organizing success rates and retaliation patterns in digital-first vs face-to-face organizing campaigns; assessment of institutional adaptation (digital surveillance, metadata analysis, device seizure) to encrypted organizing',
    'If digital tools provide genuine protection: scaffold perspective confirmed — sunset is real. If institutions adapt surveillance methods: scaffold is aspirational — extraction mechanism migrates rather than dissolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_organizing_substitution, empirical, 'Whether digital organizing provides protection from infiltration').

omega_variable(
    false_solidarity_detection_rate,
    'What proportion of false solidarity attempts are detected by workers before intelligence extraction occurs?',
    'Worker surveys about trust assessment; analysis of ''failed'' recruitment attempts; comparison of organizing outcomes in workplaces with high vs low informant detection rates',
    'If detection rate is high (>60%): theater ratio is even higher than measured — the practice is mostly performative. If detection rate is low (<30%): extraction is more severe than base metrics suggest — workers cannot protect themselves through vigilance alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_solidarity_detection_rate, empirical, 'Worker detection rate of false solidarity signals').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(informant_recruitment_through_false_solidarity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infr_tr_t0, informant_recruitment_through_false_solidarity, theater_ratio, 0, 0.45).
narrative_ontology:measurement(infr_tr_t3, informant_recruitment_through_false_solidarity, theater_ratio, 3, 0.58).
narrative_ontology:measurement(infr_tr_t6, informant_recruitment_through_false_solidarity, theater_ratio, 6, 0.68).
narrative_ontology:measurement(infr_tr_t10, informant_recruitment_through_false_solidarity, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(infr_be_t0, informant_recruitment_through_false_solidarity, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(infr_be_t3, informant_recruitment_through_false_solidarity, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(infr_be_t6, informant_recruitment_through_false_solidarity, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(infr_be_t10, informant_recruitment_through_false_solidarity, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(informant_recruitment_through_false_solidarity, identity_coordination).
narrative_ontology:affects_constraint(informant_recruitment_through_false_solidarity, debt_leverage_as_consent_manufacturing).

% DUAL FORMULATION NOTE:
% This constraint is downstream of debt_leverage_as_consent_manufacturing. Workers already economically vulnerable through debt are particularly susceptible to informant recruitment because the institutional actor can offer both solidarity performance and material relief (shift preferences, overtime access, debt assistance) in exchange for intelligence. The upstream constraint (debt leverage) creates the economic dependency that makes the downstream constraint (false solidarity recruitment) effective. Both constraints operate in the same institutional domain (labor control) but represent distinct extraction mechanisms with different ε values: debt leverage extracts through economic coercion (ε ≈ 0.65), false solidarity extracts through identity exploitation (ε = 0.58).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
