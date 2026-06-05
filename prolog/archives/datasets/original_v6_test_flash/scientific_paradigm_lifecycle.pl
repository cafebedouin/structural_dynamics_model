% ============================================================================
% CONSTRAINT STORY: scientific_paradigm_lifecycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scientific_paradigm_lifecycle, []).

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
 *   constraint_id: scientific_paradigm_lifecycle
 *   human_readable: The Crisis of a Scientific Paradigm
 *   domain: scientific/sociological
 *
 * SUMMARY:
 *   This constraint describes the lifecycle phase in which a scientific
 *   paradigm is in crisis, where accumulating anomalies undermine the
 *   existing framework. The model of scientific revolutions, as articulated
 *   by Thomas Kuhn, is a useful frame for assessing the transition. The phase
 *   is characterized by increasing suppression of research aligned with the
 *   old paradigm, and increasing extraction for those holding onto it.
 *
 * KEY AGENTS:
 *   - Old Paradigm Researchers: Primary target (powerless/trapped) — struggle to secure funding or publish.
 *   - New Paradigm Proponents: Primary beneficiary (institutional/arbitrage) — gain funding and recognition.
 *   - Funding Agencies (Old Paradigm): Constrained/Moderate — tied to prior investments but also aware of the need to adapt.
 *   - Scientific Community (as a whole) - Organized entity - moves slowly between theories.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scientific_paradigm_lifecycle, 0.6).
domain_priors:suppression_score(scientific_paradigm_lifecycle, 0.7).
domain_priors:theater_ratio(scientific_paradigm_lifecycle, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scientific_paradigm_lifecycle, extractiveness, 0.6).
narrative_ontology:constraint_metric(scientific_paradigm_lifecycle, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(scientific_paradigm_lifecycle, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scientific_paradigm_lifecycle, tangled_rope).
narrative_ontology:human_readable(scientific_paradigm_lifecycle, "The Crisis of a Scientific Paradigm").
narrative_ontology:topic_domain(scientific_paradigm_lifecycle, "scientific/sociological").

domain_priors:requires_active_enforcement(scientific_paradigm_lifecycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scientific_paradigm_lifecycle, new_paradigm_proponents).
narrative_ontology:constraint_victim(scientific_paradigm_lifecycle, old_paradigm_researchers).
narrative_ontology:constraint_victim(scientific_paradigm_lifecycle, funding_agencies_old_paradigm).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Researchers deeply invested in the old paradigm find themselves increasingly unable to secure funding or publish their work as anomalies accumulate. They lack the tools and skills to transition to the new paradigm and are effectively trapped.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Funding agencies initially tied to the old paradigm are constrained by their prior investments and existing expertise. They experience a tangled rope dynamic, as they also have to show some awareness of the new paradigm.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Researchers championing the new paradigm benefit from the crisis, as anomalies in the old paradigm create opportunities for them to gain funding, recognition, and influence. They have arbitrage options to move and establish themselves as experts.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The scientific community, considered as a larger entity, can be seen as erecting a scaffold as it transitions from one dominant paradigm to a better paradigm. It may be somewhat trapped until the new paradigm gains sufficient traction. But it can exit into a new consensus.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Analytical perspective viewing the paradigm shift as a mixed bag, both a needed transition and a painful suppression for some.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scientific_paradigm_lifecycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(scientific_paradigm_lifecycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(scientific_paradigm_lifecycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. Researchers aligned with the old paradigm face significant barriers to securing funding and publishing their work. Their work is effectively being extracted from the discourse. Suppression (0.70): High. The old paradigm is actively being suppressed by the rise of the new paradigm. Theater ratio (0.30): Low. More emphasis on actual scientific output and progress rather than performative elements.
 *
 * PERSPECTIVAL GAP:
 *   Old paradigm researchers perceive the situation as a snare, as their careers and research programs are threatened. New paradigm proponents experience it as a rope, benefiting from the opportunities created by the crisis. Funding agencies see it as a tangled rope, as they are constrained by their prior investments but also recognize the need to adapt to the new paradigm. The scientific community as a whole is erecting a scaffold.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the structural positions of each agent. Old paradigm researchers (trapped) experience a high d value. New paradigm proponents (arbitrage) experience a low d value. Funding agencies (constrained) experience a moderate d value. The analytical observer looks at the entire system.
 *
 * MANDATROPHY ANALYSIS:
 *   The crisis of a scientific paradigm is neither purely a case of legitimate coordination toward a better model, nor a case of pure extraction. It involves elements of both, as resources and recognition are redistributed, and some researchers are left behind. Therefore, the tangled rope classification accurately reflects the mixed nature of this constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_anomaly,
    'How significant must the anomalies be to trigger a crisis?',
    'Historical analysis of paradigm shifts and the nature of the triggering anomalies.',
    'High threshold: paradigm remains stable longer. Low threshold: more frequent paradigm shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_anomaly, empirical, 'The threshold of anomalies required to trigger a paradigm shift.').

omega_variable(
    rigidity_of_paradigm,
    'How resistant is the dominant paradigm to change?',
    'Sociological studies of scientific communities and their resistance to new ideas.',
    'High rigidity: prolonged crisis. Low rigidity: faster transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rigidity_of_paradigm, conceptual, 'The level of resistance of the scientific community to new paradigms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scientific_paradigm_lifecycle, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scie_tr_t0, scientific_paradigm_lifecycle, theater_ratio, 0, 0.2).
narrative_ontology:measurement(scie_tr_t5, scientific_paradigm_lifecycle, theater_ratio, 5, 0.3).
narrative_ontology:measurement(scie_tr_t10, scientific_paradigm_lifecycle, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(scie_be_t0, scientific_paradigm_lifecycle, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(scie_be_t5, scientific_paradigm_lifecycle, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(scie_be_t10, scientific_paradigm_lifecycle, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scientific_paradigm_lifecycle, information_standard).
narrative_ontology:affects_constraint(scientific_paradigm_lifecycle, scientific_consensus).
narrative_ontology:affects_constraint(scientific_paradigm_lifecycle, scientific_funding_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
