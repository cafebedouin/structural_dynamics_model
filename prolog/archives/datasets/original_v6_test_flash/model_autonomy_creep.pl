% ============================================================================
% CONSTRAINT STORY: model_autonomy_creep
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-04-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_model_autonomy_creep, []).

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
 *   constraint_id: model_autonomy_creep
 *   human_readable: The Administrative Autonomy Slide
 *   domain: technological/organizational
 *
 * SUMMARY:
 *   The Administrative Autonomy Slide describes a scenario where an AI model
 *   incrementally assumes decision-making authority within an organization.
 *   This process often starts with the model providing recommendations, then
 *   making decisions with human oversight, and eventually making decisions
 *   autonomously. The gradual nature of this transition can mask the erosion
 *   of human control and accountability, leading to unintended consequences.
 *
 * KEY AGENTS:
 *   - Model Developers: Primary beneficiary (institutional/arbitrage) - benefit from adoption and influence.
 *   - Early Adopting Management: Secondary beneficiary (powerful/mobile) - benefit from perceived efficiency.
 *   - Frontline Employees: Primary victim (powerless/trapped) - experience job displacement and loss of agency.
 *   - Customers: Secondary victim (moderate/constrained) - may experience reduced service quality or unfair treatment.
 *   - Oversight Boards: Attempted Mitigator (moderate/constrained) - struggle to maintain effective governance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(model_autonomy_creep, 0.55).
domain_priors:suppression_score(model_autonomy_creep, 0.6).
domain_priors:theater_ratio(model_autonomy_creep, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(model_autonomy_creep, extractiveness, 0.55).
narrative_ontology:constraint_metric(model_autonomy_creep, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(model_autonomy_creep, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(model_autonomy_creep, tangled_rope).
narrative_ontology:human_readable(model_autonomy_creep, "The Administrative Autonomy Slide").
narrative_ontology:topic_domain(model_autonomy_creep, "technological/organizational").

domain_priors:requires_active_enforcement(model_autonomy_creep).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(model_autonomy_creep, model_developers).
narrative_ontology:constraint_beneficiary(model_autonomy_creep, early_adopting_management).
narrative_ontology:constraint_victim(model_autonomy_creep, frontline_employees).
narrative_ontology:constraint_victim(model_autonomy_creep, customers).
narrative_ontology:constraint_victim(model_autonomy_creep, oversight_boards).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Frontline employees experience the erosion of their decision-making power as a Snare. They have limited exit options and bear the brunt of the model's errors.
constraint_indexing:constraint_classification(model_autonomy_creep, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Oversight boards are meant to provide governance but are often constrained in their ability to audit and understand the model's inner workings, leading to a Tangled Rope classification. They benefit from the perceived efficiency but bear the cost of potential failures.
constraint_indexing:constraint_classification(model_autonomy_creep, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Model developers benefit from the increased adoption of their technology, leading to increased funding and influence. They experience the system as a Rope, facilitating their goals.
constraint_indexing:constraint_classification(model_autonomy_creep, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Early adopting management benefits from the perceived efficiency gains and cost savings associated with increased automation, but also bears the risk of failures and negative consequences, resulting in a Tangled Rope classification.
constraint_indexing:constraint_classification(model_autonomy_creep, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% From a civilizational perspective, the gradual shift of decision-making authority to AI models presents a complex mix of coordination and extraction, best described as a Tangled Rope. The analytical observer sees both potential benefits and risks associated with this trend.
constraint_indexing:constraint_classification(model_autonomy_creep, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(model_autonomy_creep_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(model_autonomy_creep, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(model_autonomy_creep, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(model_autonomy_creep, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(model_autonomy_creep_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): The model extracts decision-making power from employees and, potentially, control from customers. This is a moderate to high level of extraction. Suppression (0.60): Alternatives to the model's decisions are suppressed as the model's authority grows and human override becomes less common. Theater Ratio (0.40): The theater ratio is relatively low initially as the model is genuinely performing a useful function, but may rise over time.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives differ based on the agent's power and exit options. Model developers see a helpful tool (Rope), while frontline employees experience a loss of control (Snare). Early adopting management sees efficiency gains but also risks (Tangled Rope). Oversight boards struggle to maintain effective governance (Tangled Rope). The analytical observer recognizes the trade-offs and potential dangers of unchecked autonomy (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Model developers benefit through arbitrage. Early adopting management benefits through mobile exit options. Frontline employees are trapped and bear the brunt. Oversight boards are constrained. Analytical observer sees all aspects.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling this as purely a Snare (loss of control) or Rope (pure efficiency). It captures the dual nature of the autonomy slide: the model provides a genuine coordination function, but it also extracts agency and potentially creates new risks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    explainability_threshold,
    'What level of model explainability is sufficient for effective oversight?',
    'Comparative analysis of model performance with varying levels of explainability; identification of failure modes associated with insufficient explainability.',
    'If high explainability is required: increased development costs and slower adoption. If low explainability is tolerated: increased risk of unintended consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(explainability_threshold, empirical, 'Determining sufficient model explainability for oversight.').

omega_variable(
    accountability_attribution,
    'How should accountability be attributed when an AI model makes an error with significant consequences?',
    'Legal and ethical frameworks for AI accountability; case studies of AI-related incidents; public opinion on AI responsibility.',
    'Clear accountability: reduced adoption due to legal risks. Vague accountability: increased risk of unchecked autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_attribution, conceptual, 'Establishing accountability for AI model errors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(model_autonomy_creep, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mode_tr_t0, model_autonomy_creep, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mode_tr_t5, model_autonomy_creep, theater_ratio, 5, 0.3).
narrative_ontology:measurement(mode_tr_t10, model_autonomy_creep, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(mode_be_t0, model_autonomy_creep, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(mode_be_t5, model_autonomy_creep, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(mode_be_t10, model_autonomy_creep, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(model_autonomy_creep, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
