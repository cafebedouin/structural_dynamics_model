% ============================================================================
% CONSTRAINT STORY: policing_discretion_subordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_policing_discretion_subordination, []).

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
 *   constraint_id: policing_discretion_subordination
 *   human_readable: Policing Discretion Subordination
 *   domain: criminal_justice/governance
 *
 * SUMMARY:
 *   Policing discretion subordination is a structural snare in which the
 *   formal grant of discretionary authority to street-level officers becomes
 *   a mechanism for extracting compliance from officers and unequal
 *   enforcement from marginalized communities. The constraint operates
 *   through two extraction flows: (1) officers are nominally granted
 *   discretion but functionally subordinated to unwritten performance
 *   expectations, administrative pressure, and career trajectory incentives,
 *   creating compliance extraction; (2) marginalized communities bear the
 *   costs of discretionary enforcement without meaningful appeal or exit,
 *   creating equity extraction. The constraint is maintained through the
 *   legitimacy narrative that discretion is necessary for effective policing
 *   (true in some contexts) combined with suppression of alternatives
 *   (rule-based systems, community policing, decriminalization) through
 *   political, institutional, and union resistance. Theater has increased
 *   over the measurement interval as accountability mechanisms (civilian
 *   review, use-of-force policies) have proliferated without substantively
 *   constraining discretionary practice, creating performance of legitimacy
 *   rather than actual constraint.
 *
 * KEY AGENTS:
 *   - Street-Level Officers: Primary victims (powerless/trapped) — extract compliance through career dependency, administrative discipline threats, and unwritten performance expectations
 *   - Marginalized Communities: Primary victims (powerless/trapped) — extract unequal enforcement, stop-and-frisk exposure, pretextual charges without meaningful appeal
 *   - Police Leadership: Primary beneficiaries (institutional/arbitrage) — benefit from discretion as coordination mechanism that delegates accountability downward while maintaining deniability
 *   - Political Authorities: Primary beneficiaries (institutional/arbitrage) — benefit from discretion as tool for resource allocation and political flexibility without direct accountability
 *   - Police Unions & Officer Associations: Secondary actors (organized/mobile) — both resist some extractive pressure and coordinate to enforce compliance culture
 *   - Accountability Systems: Theater maintainers (institutional/arbitrage) — civilian review, use-of-force policies, training signal legitimacy without substantively constraining extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(policing_discretion_subordination, 0.68).
domain_priors:suppression_score(policing_discretion_subordination, 0.72).
domain_priors:theater_ratio(policing_discretion_subordination, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(policing_discretion_subordination, extractiveness, 0.68).
narrative_ontology:constraint_metric(policing_discretion_subordination, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(policing_discretion_subordination, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(policing_discretion_subordination, snare).
narrative_ontology:human_readable(policing_discretion_subordination, "Policing Discretion Subordination").
narrative_ontology:topic_domain(policing_discretion_subordination, "criminal_justice/governance").

domain_priors:requires_active_enforcement(policing_discretion_subordination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(policing_discretion_subordination, institutional_police_leadership).
narrative_ontology:constraint_beneficiary(policing_discretion_subordination, political_authorities).
narrative_ontology:constraint_victim(policing_discretion_subordination, street_level_officers).
narrative_ontology:constraint_victim(policing_discretion_subordination, marginalized_communities).
narrative_ontology:constraint_victim(policing_discretion_subordination, due_process_protections).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STREET-LEVEL OFFICER (SNARE) — Officers face pervasive discretionary mandates that extract compliance through ambiguous performance metrics, threat of administrative discipline, and career trajectory dependency on institutional favor. They cannot exit without abandoning their livelihood. Discretion is formally granted but functionally subordinated to unwritten expectations. Maximum experienced extraction.
constraint_indexing:constraint_classification(policing_discretion_subordination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITIES (SNARE) — Bear the costs of discretionary policing without meaningful exit or appeal. Subject to stop-and-frisk, pretextual charges, and unequal enforcement based on officer discretion. Suppression is structural: legal recourse is costly, punishment for complaint is social isolation, and the system perpetuates enforcement asymmetry. No agency or alternative.
constraint_indexing:constraint_classification(policing_discretion_subordination, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: POLICE LEADERSHIP & POLITICAL AUTHORITIES (ROPE) — Experience the constraint as pure coordination: discretionary authority allows rapid response to local conditions, resource efficiency, and political flexibility. Discretion is the mechanism by which they delegate accountability downward while maintaining deniability. Benefits from the extraction without direct management burden.
constraint_indexing:constraint_classification(policing_discretion_subordination, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POLICE UNIONS & OFFICER ASSOCIATIONS (TANGLED ROPE) — Organize officers to resist some extractive pressure (contract protections, grievance procedures) but also coordinate with management to enforce compliance culture. Both genuine coordination function (protecting members) and asymmetric extraction (enforcing solidarity, suppressing whistle-blowing). Mixed classification reflects dual role.
constraint_indexing:constraint_classification(policing_discretion_subordination, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGITIMACY THEATER (PITON) — Formal accountability mechanisms (civilian review boards, use-of-force policies, training requirements) persist as performative structures that signal legitimacy without constraining discretionary extraction. Theater ratio is moderately high (0.58) because accountability processes create procedural appearance while discretion remains functionally unconstrained. The constraint is maintained through institutional inertia despite reduced functional verification.
constraint_indexing:constraint_classification(policing_discretion_subordination, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From civilizational perspective, policing discretion subordination appears as a structural snare: it systematically extracts compliance from officers through career dependency while extracting equity from marginalized communities through unequal enforcement. The constraint persists because both extraction flows are obscured by the legitimacy claims of discretion (necessary for officer responsiveness) and the proceduralism of the system (which signals but does not guarantee fairness). High suppression because alternatives (automated enforcement, strict rule-following, decriminalization) are politically and institutionally suppressed.
constraint_indexing:constraint_classification(policing_discretion_subordination, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(policing_discretion_subordination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(policing_discretion_subordination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(policing_discretion_subordination, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(policing_discretion_subordination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(policing_discretion_subordination, TR),
    TR >= 0.70.

:- end_tests(policing_discretion_subordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts compliance from officers through unwritten expectations, career dependency, and administrative coercion. It extracts equity from marginalized communities through discretionary enforcement asymmetry. Both extraction flows are significant, and suppression of alternatives is substantial. The 0.68 value reflects that discretion does provide some genuine coordination benefit (rapid response to local variation) but the extraction component dominates over the measurement interval. Suppression (0.72): High. Officers cannot exit without losing their livelihood; marginalized communities have no meaningful appeal or recourse; alternatives (rule-based systems, strict rule-following) are politically and institutionally suppressed. Legal remedies for improper discretion are costly and slow. Social suppression of complaint (isolation, retaliation) is real. Theater ratio (0.58): Moderate-high. The increase in theater from 0.35 to 0.58 over the interval reflects the proliferation of accountability mechanisms that signal legitimacy without constraining discretionary extraction. Civilian review boards, use-of-force policies, and training requirements create procedures that appear to constrain discretion while officers retain functional autonomy. The theater is not complete (some accountability procedures have effects) but substantial.
 *
 * PERSPECTIVAL GAP:
 *   Police leadership and political authorities perceive the constraint as pure coordination (Rope) — discretion is a tool for efficient resource allocation and local responsiveness. Street-level officers perceive it as a snare extracting compliance through career dependency and administrative pressure. Marginalized communities perceive it as a snare extracting unequal enforcement without appeal. Police unions perceive it as a mixed system (Tangled Rope) — they negotiate protections but also enforce compliance culture. Accountability systems appear as legitimacy theater (Piton) — procedures signal constraint without substantively limiting discretionary extraction. The analytical observer perceives the full snare structure: both extraction flows (officer compliance and community equity) are systematic, suppression is substantial, and alternatives are suppressed.
 *
 * DIRECTIONALITY LOGIC:
 *   Police leadership and political authorities derive d ≈ 0.05 (beneficiary + arbitrage exit) producing low or negative f(d), low chi. Street-level officers derive d ≈ 0.85 (victim + trapped exit) producing high f(d) ≈ 1.15, high chi. Marginalized communities derive d ≈ 0.90 (victim + trapped exit) producing f(d) ≈ 1.28, very high chi. Police unions derive d ≈ 0.50 (mixed benefit/victim + mobile exit) producing f(d) ≈ 0.65, moderate chi. The perspectival gap reveals that the same constraint structure is experienced as coordination (by beneficiaries) and extraction (by victims). The analytical observer's snare classification reflects that the coordination narrative (discretion enables responsiveness) is overridden by the extraction structure (both officer compliance and community equity are extracted).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint resolves the mandatrophy by showing that discretion can be both genuinely necessary (for local responsiveness) and genuinely extractive (for compliance and equity). The false choice is 'is discretion coordination or extraction?' The true structure is 'discretion serves coordination functions for institutional leadership while operating as a snare for officers and marginalized communities.' The beneficiary's rope experience and the victim's snare experience are both structurally accurate — they differ because the agent's position in the extraction flow differs. The piton classification (legitimacy theater) correctly identifies that accountability mechanisms have proliferated without constraining extraction, suggesting the system is maintaining its extractive function through performance of legitimacy rather than through genuine coordination benefit. The constraint cannot be resolved by 'more training' or 'better policies' because the extraction is structural, not accidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretion_necessity_threshold,
    'At what level of situational variation does discretionary authority become genuinely necessary versus merely convenient for management?',
    'Comparative analysis of policing outcomes in high-discretion vs rule-based systems; measurement of outcome variance vs incident variance; statistical analysis of when officers invoke discretion vs when policy covers the scenario',
    'If necessity threshold is low: discretion is primarily extractive (snare classification holds). If threshold is high: discretion provides genuine coordination benefit (rope reclassification for leadership).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_necessity_threshold, empirical, 'Threshold at which discretion becomes necessary versus convenient').

omega_variable(
    subordination_mechanism_visibility,
    'Are officers aware that their discretion is functionally subordinated, or does the formal grant of discretion create an illusion of autonomy that obscures extraction?',
    'Officer interviews about perceived autonomy; comparison of self-reported discretionary freedom vs measured compliance with unwritten expectations; longitudinal tracking of officers who resist vs comply with extractive pressures',
    'If obscured: constraint operates partly through cognitive capture (identity_locked component). If visible: constraint operates through pure coercion (trapped component). Visibility determines whether this is a snare or partially a cognitive trap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_mechanism_visibility, empirical, 'Visibility of discretion subordination to officers').

omega_variable(
    accountability_mechanism_functionality,
    'Do formal accountability structures (civilian review, use-of-force policies, training) actually constrain discretionary extraction or are they purely theatrical?',
    'Analysis of review board outcomes vs officer behavior changes; measurement of policy impact on discretionary stops, searches, and force incidents; comparison of outcomes with and without accountability procedures',
    'If functional: suppression should be lower and theater ratio lower (piton classification weakens). If theatrical: confirms piton perspective and suggests accountability is legitimacy performance rather than constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accountability_mechanism_functionality, empirical, 'Whether accountability mechanisms actually constrain discretion').

omega_variable(
    alternative_policing_model_feasibility,
    'Can rule-based or algorithmic policing systems actually replace human discretion without creating different extractive problems?',
    'Comparison of discretion-based vs rule-based policing outcomes; measurement of bias in algorithmic systems vs human discretion; cost-benefit analysis of accountability mechanisms in each model',
    'If alternatives are feasible: suppression of alternatives is political/institutional (extractive mechanism confirmed). If alternatives create worse outcomes: discretion subordination may be lesser-evil coordination rather than pure snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_policing_model_feasibility, empirical, 'Feasibility of rule-based alternatives to discretionary policing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(policing_discretion_subordination, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pds_tr_t0, policing_discretion_subordination, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pds_tr_t5, policing_discretion_subordination, theater_ratio, 5, 0.45).
narrative_ontology:measurement(pds_tr_t10, policing_discretion_subordination, theater_ratio, 10, 0.58).
narrative_ontology:measurement(pds_tr_t2, policing_discretion_subordination, theater_ratio, 2, 0.4).

% Extraction over time
narrative_ontology:measurement(pds_be_t0, policing_discretion_subordination, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(pds_be_t5, policing_discretion_subordination, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(pds_be_t10, policing_discretion_subordination, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(pds_be_t2, policing_discretion_subordination, base_extractiveness, 2, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(policing_discretion_subordination, enforcement_mechanism).
narrative_ontology:affects_constraint(policing_discretion_subordination, stop_and_frisk_asymmetry).
narrative_ontology:affects_constraint(policing_discretion_subordination, prosecutorial_discretion_lock_in).
narrative_ontology:affects_constraint(policing_discretion_subordination, qualified_immunity_suppression).

% DUAL FORMULATION NOTE:
% Policing discretion subordination is downstream of specific institutional structures (stop-and-frisk practices, prosecutorial discretion, qualified immunity) that enable and protect discretionary extraction. Each affects_constraint has its own extractiveness value but all are enabled by the discretion subordination frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(policing_discretion_subordination, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
