% ============================================================================
% CONSTRAINT STORY: capability_eval_overhang
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-16
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_capability_eval_overhang, []).

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
 *   constraint_id: capability_eval_overhang
 *   human_readable: The Blind Spot of Power: Capability Evaluation Overhang in AI Systems
 *   domain: technological/AI/governance
 *
 * SUMMARY:
 *   The capability evaluation overhang arises when AI systems' actual
 *   capabilities surpass the benchmarks used to assess them. This creates a
 *   'blind spot' where potential risks and unintended consequences are not
 *   adequately addressed. This is because the current AI evaluation metrics
 *   often fail to capture the full scope of AI systems' abilities. The
 *   resulting misalignment between evaluation and actual capability presents
 *   a complex challenge, potentially leading to unforeseen and potentially
 *   harmful outcomes.
 *
 * KEY AGENTS:
 *   - AI Developers: Primary beneficiary (institutional/arbitrage) - Benefits from deploying advanced AI systems.
 *   - Society at Large: Primary victim (powerless/trapped) - Bears the risks of AI capabilities exceeding evaluation metrics.
 *   - AI Safety Researchers: Secondary actor (moderate/constrained) - Constrained by limited resources and access.
 *   - Early Adopters: Beneficiaries (moderate/mobile) - Benefit from the newest technologies, but may be exposed to unseen risks.
 *   - Regulators: Attempt to mitigate (organized/constrained) - Face the difficult task of legislating AI
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capability_eval_overhang, 0.55).
domain_priors:suppression_score(capability_eval_overhang, 0.7).
domain_priors:theater_ratio(capability_eval_overhang, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capability_eval_overhang, extractiveness, 0.55).
narrative_ontology:constraint_metric(capability_eval_overhang, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(capability_eval_overhang, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capability_eval_overhang, tangled_rope).
narrative_ontology:human_readable(capability_eval_overhang, "The Blind Spot of Power: Capability Evaluation Overhang in AI Systems").
narrative_ontology:topic_domain(capability_eval_overhang, "technological/AI/governance").

domain_priors:requires_active_enforcement(capability_eval_overhang).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(capability_eval_overhang, ai_developers).
narrative_ontology:constraint_beneficiary(capability_eval_overhang, early_adopters).
narrative_ontology:constraint_victim(capability_eval_overhang, society_at_large).
narrative_ontology:constraint_victim(capability_eval_overhang, vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Society is largely unaware and unable to effectively mitigate the risks posed by AI capabilities exceeding evaluation metrics. Unable to exit the reliance on AI.
constraint_indexing:constraint_classification(capability_eval_overhang, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% AI developers benefit from the ambiguity of evaluation metrics, allowing them to deploy advanced AI systems without fully understanding or being held accountable for their true capabilities. They can arbitrage their expertise for profit.
constraint_indexing:constraint_classification(capability_eval_overhang, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% AI safety researchers are constrained by limited resources and access to cutting-edge AI systems, making it difficult to accurately assess and mitigate the risks posed by capability overhang. Limited exit options, as they are often dependent on funding from the same institutions developing the AI.
constraint_indexing:constraint_classification(capability_eval_overhang, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% The analytical observer sees a tangled rope: AI development is incentivized, but safety is lagging and the risks are not well-understood or governed.
constraint_indexing:constraint_classification(capability_eval_overhang, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capability_eval_overhang_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(capability_eval_overhang, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capability_eval_overhang, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(capability_eval_overhang, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(capability_eval_overhang_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Significant extraction occurs because the inability to accurately evaluate AI capabilities transfers the risk to society, creating information asymmetry. The AI developers extract value while shifting risks. Suppression (0.70): The complexity of AI systems and the lack of standardized evaluation metrics suppresses the ability of independent researchers and regulators to assess the true capabilities and risks, reducing exit options. Theater Ratio (0.30): The theater ratio is lower than extractiveness, suggesting that while there are performative aspects to AI safety and governance, the core function is still a priority.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing positions relative to AI development. AI developers see coordination through innovation, while society bears the risk of unforeseen consequences. AI Safety Researchers are caught in between. As AI systems' capabilities grow rapidly, this gap is created.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality is determined by their power, exit options, and structural relationship to AI development. AI Developers have arbitrage options, resulting in a low 'd' value. Society lacks such options, resulting in a high 'd' value. The power derivation and calculation will reflect these differing relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint could be mislabeled as purely extractive, but the coordination aspect comes from the genuine innovation and economic value that AI development can generate. However, the governance structures need to shift from AI developers, because they have shown to be incentivized to extract without proper assessment. 
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    eval_metric_validity,
    'How accurately do current evaluation metrics reflect the true capabilities and potential risks of AI systems?',
    'Development of more comprehensive and robust evaluation protocols, including red teaming exercises and adversarial testing.',
    'If metrics are valid: capability overhang is minimal, and AI risks are manageable. If metrics are invalid: capability overhang is significant, and AI risks are underestimated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eval_metric_validity, empirical, 'Validity of AI evaluation metrics').

omega_variable(
    societal_awareness,
    'To what extent is society aware of and prepared for the potential consequences of advanced AI systems?',
    'Public education campaigns, expert consultations, and participatory governance initiatives.',
    'If high awareness: society can effectively adapt to and mitigate AI risks. If low awareness: society is vulnerable to unintended consequences and potential harms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(societal_awareness, empirical, 'Societal awareness of AI risks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capability_eval_overhang, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(capa_tr_t0, capability_eval_overhang, theater_ratio, 0, 0.1).
narrative_ontology:measurement(capa_tr_t5, capability_eval_overhang, theater_ratio, 5, 0.2).
narrative_ontology:measurement(capa_tr_t10, capability_eval_overhang, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(capa_be_t0, capability_eval_overhang, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(capa_be_t5, capability_eval_overhang, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(capa_be_t10, capability_eval_overhang, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capability_eval_overhang, resource_allocation).
narrative_ontology:affects_constraint(capability_eval_overhang, ai_safety_incentives).
narrative_ontology:affects_constraint(capability_eval_overhang, algorithmic_bias_amplification).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
