% ============================================================================
% CONSTRAINT STORY: taiwan_grand_bargain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [DRAFT]
% ============================================================================

:- module(constraint_taiwan_grand_bargain, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: taiwan_grand_bargain
 *   human_readable: The U.S.-China Taiwan Grand Bargain
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   A potential diplomatic framework where the U.S. and China agree on a
 *   resolution for Taiwan, potentially involving compromises on Taiwan's
 *   autonomy, economic integration, or security arrangements. This framework
 *   may prioritize regional stability and great power relations over the
 *   self-determination of the Taiwanese people and the integrity of global
 *   democratic norms.
 *
 * KEY AGENTS:
 *   - Taiwanese People: Primary target (powerless/trapped) - autonomy is directly affected.
 *   - Chinese Communist Party: Primary beneficiary (institutional/constrained) - gains regional influence and stability.
 *   - US Military-Industrial Complex: Secondary beneficiary (institutional/constrained) - sustains arms sales and regional presence.
 *   - Global Democratic Norms: Secondary target (powerless/trapped) - principles are compromised for geopolitical expediency.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taiwan_grand_bargain, 0.55).
domain_priors:suppression_score(taiwan_grand_bargain, 0.7).
domain_priors:theater_ratio(taiwan_grand_bargain, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taiwan_grand_bargain, extractiveness, 0.55).
narrative_ontology:constraint_metric(taiwan_grand_bargain, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(taiwan_grand_bargain, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taiwan_grand_bargain, tangled_rope).
narrative_ontology:human_readable(taiwan_grand_bargain, "The U.S.-China Taiwan Grand Bargain").
narrative_ontology:topic_domain(taiwan_grand_bargain, "economic/geopolitical").

domain_priors:requires_active_enforcement(taiwan_grand_bargain).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taiwan_grand_bargain, chinese_communist_party).
narrative_ontology:constraint_beneficiary(taiwan_grand_bargain, us_military_industrial_complex).
narrative_ontology:constraint_victim(taiwan_grand_bargain, taiwanese_people).
narrative_ontology:constraint_victim(taiwan_grand_bargain, global_democratic_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Taiwanese people, with limited agency to influence the grand bargain's terms, are largely trapped. Their autonomy and self-determination are suppressed to facilitate the agreement between larger powers.
constraint_indexing:constraint_classification(taiwan_grand_bargain, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% The CCP benefits from increased regional influence and stability but is constrained by the need to maintain internal legitimacy and manage international perceptions. Coordination through the bargain enhances its extraction capacity from the region.
constraint_indexing:constraint_classification(taiwan_grand_bargain, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% The MIC benefits from continued arms sales and a strategic presence in the region, albeit with potentially reduced direct intervention. Coordination with China enhances its extraction capacity from the global security landscape.
constraint_indexing:constraint_classification(taiwan_grand_bargain, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% The dilution of democratic values and principles to accommodate geopolitical expediency negatively impacts global democratic norms. These abstract norms are powerless and unable to exit. 
constraint_indexing:constraint_classification(taiwan_grand_bargain, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Analytical perspective reveals a Tangled Rope – a combination of coordination for stability and resource management with asymmetric extraction from affected populations and democratic principles.
constraint_indexing:constraint_classification(taiwan_grand_bargain, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_grand_bargain_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taiwan_grand_bargain, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_grand_bargain, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taiwan_grand_bargain, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(taiwan_grand_bargain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate to high (0.55) as it involves compromises on autonomy and democratic principles. Suppression is high (0.70) because the Taiwanese people have limited agency to influence the terms. Theater is low (0.30) because the primary focus is on the actual redistribution of geopolitical power.
 *
 * PERSPECTIVAL GAP:
 *   The Taiwanese people see a Snare, as their autonomy is sacrificed. The CCP and US MIC see a Tangled Rope, balancing regional stability with the need to maintain internal legitimacy and global perceptions, respectively. Global democratic norms also experience a snare, as these are undermined by the compromises made by the major powers. An analytical observer sees the bargain as a Tangled Rope, characterized by a combination of coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (CCP and US MIC) have constrained exit options. Victims (Taiwanese people and democratic norms) are largely trapped. Therefore, the beneficiaries experience lower extraction due to coordination benefits, while the victims bear the brunt of the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This resolves the mandatrophy by recognizing that the grand bargain is not simply a coordination mechanism or a purely extractive scheme. It is a complex situation with both positive and negative aspects, depending on the observer's perspective. The dominant classification of Tangled Rope captures this complexity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taiwanese_self_determination,
    'To what extent will the Taiwanese people have genuine self-determination within the grand bargain?',
    'Monitoring of political participation, democratic processes, and public opinion in Taiwan.',
    'If high: The bargain may be more rope-like. If low: It reinforces the snare classification for the Taiwanese people.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taiwanese_self_determination, empirical, 'The degree of genuine self-determination for the Taiwanese people.').

omega_variable(
    long_term_stability,
    'Will the grand bargain genuinely lead to long-term regional stability, or will it merely postpone conflict?',
    'Analysis of geopolitical trends, military deployments, and diplomatic relations in the region.',
    'If stable: Justifies the coordination benefits. If unstable: Undermines the rationale and exposes the extractive nature of the deal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_stability, empirical, 'Whether the grand bargain will lead to long-term regional stability.').

omega_variable(
    democratic_norm_erosion,
    'What is the long-term impact on global democratic norms if powerful nations compromise democratic principles for geopolitical gain?',
    'Tracking of democratic backsliding, human rights violations, and public trust in democratic institutions worldwide.',
    'If significant erosion: Reinforces the snare classification for global democratic norms. If minimal erosion: Suggests the damage can be mitigated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_norm_erosion, empirical, 'The long-term impact on global democratic norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taiwan_grand_bargain, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taiw_tr_t0, taiwan_grand_bargain, theater_ratio, 0, 0.2).
narrative_ontology:measurement(taiw_tr_t5, taiwan_grand_bargain, theater_ratio, 5, 0.3).
narrative_ontology:measurement(taiw_tr_t10, taiwan_grand_bargain, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(taiw_be_t0, taiwan_grand_bargain, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(taiw_be_t5, taiwan_grand_bargain, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(taiw_be_t10, taiwan_grand_bargain, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
