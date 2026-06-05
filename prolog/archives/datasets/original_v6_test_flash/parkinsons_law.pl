% ============================================================================
% CONSTRAINT STORY: parkinsons_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parkinsons_law, []).

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
 *   constraint_id: parkinsons_law
 *   human_readable: Parkinson's Law: Work Expands to Fill the Time Available
 *   domain: organizational/economic
 *
 * SUMMARY:
 *   Parkinson's Law describes the tendency for bureaucracies to expand their
 *   personnel and generate work, irrespective of the actual amount of work to
 *   be done. This expansion results in a complex interplay of beneficiaries
 *   and victims, and leads to various perspectives on its classification. The
 *   law highlights the challenge of maintaining efficiency and accountability
 *   within large organizations.
 *
 * KEY AGENTS:
 *   - Bureaucratic Managers: Primary beneficiaries (institutional/arbitrage) - gain increased power and influence.
 *   - Supporting Staff: Beneficiaries (moderate/constrained) - gain job security but may experience reduced efficiency.
 *   - Taxpayers: Primary victims (powerless/trapped) - bear the cost of bureaucratic expansion through higher taxes or reduced services.
 *   - Productive Workers: Secondary victims (moderate/constrained) - experience their work diluted by bureaucratic processes.
 *   - The Bureaucracy: A self-sustaining organization (institutional/constrained) - May become a piton over time, losing original coordinating function.
 *   - Analytical Observer: Objective analyst (analytical/analytical) - recognizes both the coordinating and extracting elements.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parkinsons_law, 0.65).
domain_priors:suppression_score(parkinsons_law, 0.7).
domain_priors:theater_ratio(parkinsons_law, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parkinsons_law, extractiveness, 0.65).
narrative_ontology:constraint_metric(parkinsons_law, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(parkinsons_law, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parkinsons_law, tangled_rope).
narrative_ontology:human_readable(parkinsons_law, "Parkinson's Law: Work Expands to Fill the Time Available").
narrative_ontology:topic_domain(parkinsons_law, "organizational/economic").

domain_priors:requires_active_enforcement(parkinsons_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parkinsons_law, bureaucratic_managers).
narrative_ontology:constraint_beneficiary(parkinsons_law, supporting_staff).
narrative_ontology:constraint_victim(parkinsons_law, taxpayers).
narrative_ontology:constraint_victim(parkinsons_law, productive_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAXPAYERS (SNARE) - Taxpayers are the ultimate bearers of the cost of bureaucratic inefficiency. They are trapped within the system and have limited exit options, as the expansion is often masked and difficult to directly attribute. Suffer extraction due to higher taxes or reduced services without corresponding benefits.
constraint_indexing:constraint_classification(parkinsons_law, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRODUCTIVE WORKER (TANGLED ROPE) - Productive workers find their work diluted or delayed by superfluous tasks or coordination efforts necessitated by bureaucratic expansion. While they are constrained within their work environment, they may have some mobility, e.g., changing jobs. They benefit from the law by increased job security, but bear the cost of reduced efficiency.
constraint_indexing:constraint_classification(parkinsons_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: BUREAUCRATIC MANAGERS (ROPE) - Bureaucratic managers directly benefit from Parkinson's Law as it leads to increased staff and resources under their control, enhancing their power and influence. They perceive the law as a coordination mechanism that helps them expand their domains. They have high arbitrage opportunities.
constraint_indexing:constraint_classification(parkinsons_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: THE BUREAUCRACY (PITON) - The bureaucracy, as a whole, might become a piton over time. Initially intended as a rope for coordinating efforts, it can devolve into a self-perpetuating entity focused on its own survival and expansion. The theater ratio is high, as the bureaucracy focuses on maintaining its structure rather than on providing efficient services.
constraint_indexing:constraint_classification(parkinsons_law, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) - Analytical observers see the law as a tangled rope, a hybrid of coordination and extraction. The law promotes internal coordination within the bureaucracy, but also extracts resources from the outside world (taxpayers) to fuel its expansion. The overall impact on society is mixed.
constraint_indexing:constraint_classification(parkinsons_law, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parkinsons_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parkinsons_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parkinsons_law, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(parkinsons_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(parkinsons_law, TR),
    TR >= 0.70.

:- end_tests(parkinsons_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. Resources are continuously extracted from taxpayers and funneled to the bureaucracy to fuel its expansion. Suppression (0.70): High. There is often significant resistance from productive workers, but the system can suppress attempts to improve efficiency, as it would threaten the jobs of many employees. Theater ratio (0.60): Moderate-High. The bureaucracy engages in many theatrical activities, such as meetings and reports, that contribute little to real productivity, but serves to justify continued expansion.
 *
 * PERSPECTIVAL GAP:
 *   The taxpayers view the law as a snare because they are trapped in the system and bear the full cost. The productive workers see it as a tangled rope because they are both constrained and benefit from it. Bureaucratic managers see the law as a rope, as it helps them expand their power and control. From an analytical perspective, Parkinson's Law presents a tangled rope – it facilitates some coordination within the bureaucracy but ultimately extracts from society.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality logic derives from the structural positions of each agent. Bureaucratic managers benefit, while taxpayers bear costs. Productive workers have mixed directionality. The overall effect is one of extraction from the outside and coordination within.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by acknowledging the role that bureaucracies play in organizing and managing resources. However, it also recognizes the tendency of these organizations to expand beyond their optimal size, leading to inefficiencies and the extraction of resources from taxpayers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    productivity_measurement_difficulty,
    'How accurately can the productivity of bureaucratic work be measured, and what metrics are most reliable?',
    'Development of new performance metrics, analysis of correlations between bureaucratic staff size and output of related sectors.',
    'If productivity is easily measured, the effects of Parkinson''s Law can be identified and mitigated (moves towards rope). If it is difficult, extraction is hidden (moves towards snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(productivity_measurement_difficulty, empirical, 'Measurability of productivity in bureaucratic work.').

omega_variable(
    public_awareness_threshold,
    'What level of public awareness and political will is required to effectively combat Parkinson''s Law in government?',
    'Studies of successful government reforms, analysis of media coverage and public opinion data.',
    'If awareness is low, the law can continue unchecked (moves towards snare). If awareness is high, reforms can be implemented (moves towards scaffold or rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_awareness_threshold, preference, 'Public awareness and political will to combat bureaucratic inefficiency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parkinsons_law, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(park_tr_t0, parkinsons_law, theater_ratio, 0, 0.4).
narrative_ontology:measurement(park_tr_t5, parkinsons_law, theater_ratio, 5, 0.5).
narrative_ontology:measurement(park_tr_t10, parkinsons_law, theater_ratio, 10, 0.6).

% Extraction over time
narrative_ontology:measurement(park_be_t0, parkinsons_law, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(park_be_t5, parkinsons_law, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(park_be_t10, parkinsons_law, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parkinsons_law, resource_allocation).
narrative_ontology:affects_constraint(parkinsons_law, principal_agent_problem).
narrative_ontology:affects_constraint(parkinsons_law, regulatory_capture).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
