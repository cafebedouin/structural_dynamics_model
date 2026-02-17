% ============================================================================
% CONSTRAINT STORY: tear_gas_repression_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_tear_gas_repression_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: tear_gas_repression_2026
 * human_readable: The Tear Gas Riot-Incentive Loop
 * domain: political/technological/social
 * * SUMMARY:
 * This constraint analyzes the use of tear gas as a tool that purportedly
 * manages crowds but functionally incites riots to justify state force. It
 * maps the transition from a citizen's right to protest to a requirement
 * for "armoring" (gas masks) to survive state repression.
 * * KEY AGENTS:
 * - [Protesters/Journalists]: Subject (Powerless) - Subjected to "aerosolized
 * pain" and federal "invasions".
 * - [State/Federal Agents]: Beneficiary (Institutional) - Utilize gas to
 * clear crowds and frame gatherings as "riots".
 * - [Analytical Observer]: Auditor (Analytical) - Documents the specific gear and
 * logical fallacies of "crowd control".
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extremely high extraction (0.88) due to the excruciating physiological pain
% and long-term health impacts (e.g., menstrual disruption).
domain_priors:base_extractiveness(tear_gas_repression_2026, 0.88).

% High suppression (0.82); unmarked vans and "unavoidable" gas clouds restrict
% exit options for civilians.
domain_priors:suppression_score(tear_gas_repression_2026, 0.82).

% High theater ratio (0.78); the "failed state" or "riot" label is a
% theatrical justification for functional violence.
domain_priors:theater_ratio(tear_gas_repression_2026, 0.78).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(tear_gas_repression_2026, extractiveness, 0.88).
narrative_ontology:constraint_metric(tear_gas_repression_2026, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(tear_gas_repression_2026, theater_ratio, 0.78).

% The state claims this is a coordination tool for crowd control.
narrative_ontology:constraint_claim(tear_gas_repression_2026, tangled_rope).
narrative_ontology:human_readable(tear_gas_repression_2026, "The Tear Gas Riot-Incentive Loop").

% Requires active enforcement by DHS, feds, and local police.
domain_priors:requires_active_enforcement(tear_gas_repression_2026).

% Structural property derivation hooks for Tangled Rope.
% has_coordination_function/1 is derived from constraint_beneficiary/2.
% has_asymmetric_extraction/1 is derived from constraint_victim/2.
narrative_ontology:constraint_beneficiary(tear_gas_repression_2026, state_and_federal_agents).
narrative_ontology:constraint_victim(tear_gas_repression_2026, protesters_and_journalists).


/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the protester, tear gas is a snare: a trap of "involuntary gasping" and
% physical pain that makes presence in public space a "fishbowl" of suffering.
constraint_indexing:constraint_classification(tear_gas_repression_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the State, the gas is a rope: a "routine tool" for coordinating
% crowd dispersal and maintaining "sovereign policy choices".
constraint_indexing:constraint_classification(tear_gas_repression_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The Auditor identifies the "riot-incentive" loop: the gas is a hybrid
% coordination (dispersal) and extraction (incitement/harm) tool.
constraint_indexing:constraint_classification(tear_gas_repression_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tear_gas_repression_2026_tests).

test(perspectival_gap) :-
    % Subject feels a Snare (Trapped/Pain), State sees a Rope (Coordination).
    constraint_indexing:constraint_classification(tear_gas_repression_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tear_gas_repression_2026, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(tear_gas_repression_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_threshold_validation) :-
    % A tangled_rope requires high extraction and suppression.
    domain_priors:base_extractiveness(tear_gas_repression_2026, E),
    domain_priors:suppression_score(tear_gas_repression_2026, S),
    E >= 0.50,
    S > 0.40.

:- end_tests(tear_gas_repression_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) is based on the article's account of "aerosolized
 * pain" and the loss of breath control. The high theater_ratio (0.78) stems
 * from the semantic struggle over "protest" vs "riot"—the state uses the gas
 * to create the chaos it claims to be preventing. The Tangled Rope classification
 * is critical because the tool has a genuine (if brutal) coordination function
 * (dispersal) alongside its primary extractive function (suppressing dissent,
 * inflicting harm, justifying further force).
 *
 * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is vital because the gas does coordinate
 * (dispersal) but extracts the civilian's right to occupy public space.
 * [RESOLVED MANDATROPHY] - The link between gas and riot incitement is causal
 * per the auditor's epiphany. The system correctly identifies the hybrid nature
 * instead of misclassifying it as a pure Snare, which would ignore the state's
 * coordination claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_tear_gas_repression_2026,
    'Does tear gas prevent violence or functionally incite it to justify force?',
    'Comparative analysis of protest policing tactics and escalation outcomes across different jurisdictions.',
    'If gas primarily disperses without escalating, it is a Rope; if it reliably incites, it is a Snare/Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tear_gas_repression_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Model the escalation from 2020 (T=0) to the Minneapolis surge in 2025 (T=10).

% Theater ratio: Increases as the "riot" label is applied to HOA-meeting style
% crowds to justify tactical escalation.
narrative_ontology:measurement(tear_gas_repression_2026_tr_t0, tear_gas_repression_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tear_gas_repression_2026_tr_t5, tear_gas_repression_2026, theater_ratio, 5, 0.55).
narrative_ontology:measurement(tear_gas_repression_2026_tr_t10, tear_gas_repression_2026, theater_ratio, 10, 0.78).

% Extraction: Increases as "peaceful" crowds are met with unmarked vans and
% more potent chemical "pea-soupers".
narrative_ontology:measurement(tear_gas_repression_2026_ex_t0, tear_gas_repression_2026, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(tear_gas_repression_2026_ex_t5, tear_gas_repression_2026, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(tear_gas_repression_2026_ex_t10, tear_gas_repression_2026, base_extractiveness, 10, 0.88).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The use of tear gas is a specific type of enforcement mechanism, which
% allows for Boltzmann purity analysis against other constraints of this type.
narrative_ontology:coordination_type(tear_gas_repression_2026, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */