% ============================================================================
% CONSTRAINT STORY: thai_senate_veto_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-03-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thai_senate_veto_2026, []).

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
 *   constraint_id: thai_senate_veto_2026
 *   human_readable: The Senate Constitutional Veto (Post-Transitory Residual)
 *   domain: political
 *
 * SUMMARY:
 *   Following the expiration of the five-year transitory provision that
 *   allowed the junta-appointed Senate to vote for the Prime Minister, the
 *   Senate retains a one-third veto power over constitutional amendments
 *   under Article 256. This constraint analysis focuses on the period after
 *   this transitory provision, assessing the structural implications of this
 *   ongoing veto power.
 *
 * KEY AGENTS:
 *   - Military Establishment: Primary beneficiary (institutional/arbitrage) - Maintains influence over constitutional amendments.
 *   - Royalist Establishment: Secondary beneficiary (institutional/constrained) - Protects the existing power structure.
 *   - Pro-Democracy Movement: Primary victim (powerless/trapped) - Undermines the electoral mandate and limits their ability to enact constitutional reforms.
 *   - Elected Government: Constrained actor (moderate/constrained) - Must negotiate with the Senate to achieve constitutional amendments.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thai_senate_veto_2026, 0.55).
domain_priors:suppression_score(thai_senate_veto_2026, 0.7).
domain_priors:theater_ratio(thai_senate_veto_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thai_senate_veto_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(thai_senate_veto_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(thai_senate_veto_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thai_senate_veto_2026, tangled_rope).
narrative_ontology:human_readable(thai_senate_veto_2026, "The Senate Constitutional Veto (Post-Transitory Residual)").
narrative_ontology:topic_domain(thai_senate_veto_2026, "political").

domain_priors:requires_active_enforcement(thai_senate_veto_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(thai_senate_veto_2026, military_establishment).
narrative_ontology:constraint_beneficiary(thai_senate_veto_2026, royalist_establishment).
narrative_ontology:constraint_victim(thai_senate_veto_2026, pro_democracy_movement).
narrative_ontology:constraint_victim(thai_senate_veto_2026, electoral_mandate).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRO-DEMOCRACY MOVEMENT (SNARE) - The pro-democracy movement is largely trapped by the constitutional framework and faces significant barriers to amending the constitution to remove the Senate veto. They are the primary target of extraction, as their electoral mandate is undermined.
constraint_indexing:constraint_classification(thai_senate_veto_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ELECTED GOVERNMENT (TANGLED ROPE) - The elected government is constrained by the Senate's veto power, requiring them to negotiate and compromise on constitutional amendments. However, they also benefit from the stability provided by the constitutional framework. Significant extraction, but some coordination benefit.
constraint_indexing:constraint_classification(thai_senate_veto_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MILITARY AND ROYALIST ESTABLISHMENT (ROPE) - The military and royalist establishment benefits from the Senate's veto power, which allows them to maintain influence over constitutional amendments and protect their interests. They experience the constraint as a coordination mechanism for maintaining the existing power structure.
constraint_indexing:constraint_classification(thai_senate_veto_2026, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) - An analytical observer sees the Senate's veto power as a tangled rope, balancing stability with democratic principles. The veto provides a check on potential radical changes to the constitution but also undermines the electoral mandate and limits the ability of the government to respond to the needs of the people.
constraint_indexing:constraint_classification(thai_senate_veto_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thai_senate_veto_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(thai_senate_veto_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thai_senate_veto_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(thai_senate_veto_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(thai_senate_veto_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-High. The Senate's veto power extracts significant power from the elected government and the pro-democracy movement, limiting their ability to enact constitutional reforms. Suppression (0.70): High. The Senate's veto power significantly suppresses the ability of the elected government and the pro-democracy movement to amend the constitution, creating a substantial barrier to change. Theater ratio (0.30): Low. The Senate's veto power is a real and consequential constraint, not merely a performative one.
 *
 * PERSPECTIVAL GAP:
 *   The pro-democracy movement views the Senate's veto power as a snare, trapping them within a constitutional framework that limits their ability to enact reforms. The military and royalist establishment views the Senate's veto power as a rope, a coordination mechanism for maintaining their influence and protecting their interests. The elected government experiences the Senate's veto power as a tangled rope, balancing the need for compromise and negotiation with the desire to fulfill their electoral mandate. The analytical observer sees the Senate's veto power as a tangled rope, recognizing both its stabilizing and undemocratic aspects.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are determined by the structural position of each agent. The military and royalist establishment, as beneficiaries, have low directionality values. The pro-democracy movement, as victims, have high directionality values. The elected government has a moderate directionality value, reflecting their constrained position. The analytical observer's perspective reflects a balanced view of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the Senate's veto power is not simply a pure extraction mechanism (snare) but also serves a coordination function (rope) for the military and royalist establishment. The tangled rope classification reflects the balance between these two aspects, recognizing both the stabilizing and undemocratic aspects of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_threshold,
    'What level of public support for constitutional amendments is required to overcome the Senate''s veto power and delegitimize its use?',
    'Public opinion surveys and analysis of past constitutional amendment attempts.',
    'If high support required: Senate veto remains effective. If low support sufficient: Senate veto becomes politically unsustainable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_threshold, empirical, 'Threshold of public support needed to overcome the Senate''s veto.').

omega_variable(
    coalition_formation,
    'Can a stable coalition of political parties be formed that is committed to amending the constitution and removing the Senate''s veto power?',
    'Analysis of political party platforms and coalition negotiations.',
    'If coalition forms: Senate veto becomes vulnerable. If coalition fails to form: Senate veto remains entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_formation, empirical, 'The potential for a political coalition to challenge the Senate''s veto.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thai_senate_veto_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(thai_tr_t0, thai_senate_veto_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(thai_tr_t5, thai_senate_veto_2026, theater_ratio, 5, 0.3).
narrative_ontology:measurement(thai_tr_t10, thai_senate_veto_2026, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(thai_be_t0, thai_senate_veto_2026, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(thai_be_t5, thai_senate_veto_2026, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(thai_be_t10, thai_senate_veto_2026, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thai_senate_veto_2026, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
