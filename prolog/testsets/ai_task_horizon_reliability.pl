% ============================================================================
% CONSTRAINT STORY: ai_task_horizon_reliability
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_ai_task_horizon_reliability, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: ai_task_horizon_reliability
 * human_readable: The AI Task Horizon and Reliability Bottleneck
 * domain: technological/economic
 * * SUMMARY:
 * This constraint defines the inverse relationship between task complexity 
 * (measured in human time-to-complete) and AI success rates. As tasks scale 
 * in duration and educational requirements, model reliability drops significantly, 
 * creating a "task horizon" that limits autonomous productivity gains while 
 * enabling automation of mid-complexity professional work.
 * * KEY AGENTS:
 * - Enterprise API User: Beneficiary (Institutional)
 * - Professional Knowledge Worker: Victim (Moderate Power)
 * - Individual Web User: Subject (Powerless)
 * - Systems Auditor: Observer (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ai_task_horizon_reliability, 0.48). % Snare extraction >= 0.46. Represents deskilling and value transfer from labor to capital.
domain_priors:suppression_score(ai_task_horizon_reliability, 0.60).   % Selection effects mask true failure rates as users avoid tasks they expect to fail.
domain_priors:theater_ratio(ai_task_horizon_reliability, 0.10).       % Low theater; the constraint is highly functional.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(ai_task_horizon_reliability, extractiveness, 0.48).
narrative_ontology:constraint_metric(ai_task_horizon_reliability, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(ai_task_horizon_reliability, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It is presented as an unavoidable, natural limit of the current technology paradigm.
narrative_ontology:constraint_claim(ai_task_horizon_reliability, tangled_rope).
narrative_ontology:human_readable(ai_task_horizon_reliability, "The AI Task Horizon and Reliability Bottleneck").
narrative_ontology:topic_domain(ai_task_horizon_reliability, "technological/economic").

% Binary flags
% Enforcement is the market pressure and platform design choices that perpetuate the bottleneck.
domain_priors:requires_active_enforcement(ai_task_horizon_reliability). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(ai_task_horizon_reliability, efficiency_seeking_firms).
narrative_ontology:constraint_victim(ai_task_horizon_reliability, white_collar_professionals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (POWERLESS USER) -> MOUNTAIN
% For the individual web user, the AI's limitations are an immutable fact of the
% technology they are using. They have no power to change it and must work around it.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ENTERPRISE) -> ROPE
% For an enterprise deploying AI, the reliability bottleneck is a known parameter.
% They focus on the massive efficiency gains (the Rope) from automating tasks
% within the reliable horizon, treating failures as a manageable cost.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE VICTIM (KNOWLEDGE WORKER) -> SNARE
% For the professional whose expertise falls within the AI's capability, the
% system is a Snare. It automates their valuable skills, extracting their
% professional autonomy and depressing wages, forcing them to adapt or be marginalized.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER -> TANGLED ROPE
% The analyst sees both the coordination function (efficiency gains for firms)
% and the asymmetric extraction (deskilling of professionals). The need for
% market enforcement to adopt this imperfect technology makes it a Tangled Rope.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_task_horizon_reliability_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(ai_task_horizon_reliability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_task_horizon_reliability, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    assertion(TypePowerless == mountain),
    assertion(TypeInstitutional == rope).

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ai_task_horizon_reliability, ExtMetricName, E),
    (E =< 0.15 ; E >= 0.46), % Ensures it's either low-extraction or high-extraction
    assertion(E == 0.48).

test(tangled_rope_conditions_met) :-
    % The analytical observer must see a tangled_rope
    constraint_indexing:constraint_classification(ai_task_horizon_reliability, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(ai_task_horizon_reliability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness is set to 0.48, just above the Snare threshold, to
 * model the significant but not total displacement of professional knowledge
 * work. The suppression score of 0.60 reflects the strong market pressure and
 * selection effects that hide the technology's true failure rates.
 *
 * The Perspectival Gap is profound:
 * - For a powerless user, it's a Mountain (an immutable technological fact).
 * - For an institutional beneficiary, it's a Rope (a tool for efficiency).
 * - For a professional whose skills are targeted, it's a Snare (an extractive trap).
 * - The analytical view resolves this into a Tangled Rope, acknowledging both
 *   the genuine coordination/efficiency benefits and the coercive, asymmetric
 *   extraction from a specific class of labor.
 *
 * * MANDATROPHY ANALYSIS:
 * Classifying this as a Tangled Rope is critical. A simpler analysis might label
 * it a pure Snare (focusing only on the workers) or a pure Rope (focusing only
 * on the firms). The Tangled Rope classification correctly identifies that the
 * system has a genuine coordination function (automating complex workflows)
 * that is inextricably linked to an extractive one (devaluing human expertise).
 * This prevents mischaracterizing a complex techno-economic shift as simple
 * predation or pure progress.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ai_task_horizon,
    'Will the reliability horizon remain a persistent bottleneck, or will architectural breakthroughs eliminate it?',
    'longitudinal_tracking_of_model_success_on_civilizational_scale_tasks',
    'if_persistent_productivity_stalls_if_eliminated_labor_disruption_accelerates',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ai_task_horizon_reliability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness > 0.46 requires temporal data.
% This models the constraint intensifying as AI adoption becomes more widespread
% and integrated into core business processes, increasing its extractive potential.
%
% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(ai_thr_tr_t0, ai_task_horizon_reliability, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ai_thr_tr_t5, ai_task_horizon_reliability, theater_ratio, 5, 0.08).
narrative_ontology:measurement(ai_thr_tr_t10, ai_task_horizon_reliability, theater_ratio, 10, 0.10).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(ai_thr_ex_t0, ai_task_horizon_reliability, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ai_thr_ex_t5, ai_task_horizon_reliability, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ai_thr_ex_t10, ai_task_horizon_reliability, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The AI reliability horizon acts as a de facto standard for global digital labor,
% making it a form of global infrastructure.
narrative_ontology:coordination_type(ai_task_horizon_reliability, global_infrastructure).

% This constraint directly influences the value and structure of professional labor markets.
narrative_ontology:affects_constraint(ai_task_horizon_reliability, professional_labor_markets).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */