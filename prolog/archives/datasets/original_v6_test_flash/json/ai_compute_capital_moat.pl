% ============================================================================
% CONSTRAINT STORY: ai_compute_capital_moat
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_compute_capital_moat, []).

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
 *   constraint_id: ai_compute_capital_moat
 *   human_readable: The AI Compute & Capital Moat
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The development of frontier AI models requires immense capital for two
 *   primary inputs: elite research talent and massive-scale GPU compute
 *   clusters. This creates a significant barrier to entry, concentrating
 *   power and resources in the hands of a few dominant players. This barrier
 *   is described as a compute and capital moat.
 *
 * KEY AGENTS:
 *   - Frontier AI Labs: Primary beneficiary (institutional/arbitrage) — benefits from the capital moat, attracting top talent and building massive compute clusters.
 *   - Open Source AI Community: Primary victim (powerless/trapped) — lacks the resources to compete and is effectively trapped.
 *   - Academic AI Researchers: Secondary victim (moderate/constrained) — faces funding constraints, limiting access to compute resources.
 *   - Smaller AI Startups: Secondary victim (moderate/constrained) — struggles to compete in attracting talent and securing compute resources.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_compute_capital_moat, 0.65).
domain_priors:suppression_score(ai_compute_capital_moat, 0.7).
domain_priors:theater_ratio(ai_compute_capital_moat, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_compute_capital_moat, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_compute_capital_moat, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_compute_capital_moat, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_compute_capital_moat, tangled_rope).
narrative_ontology:human_readable(ai_compute_capital_moat, "The AI Compute & Capital Moat").
narrative_ontology:topic_domain(ai_compute_capital_moat, "technological/economic").

domain_priors:requires_active_enforcement(ai_compute_capital_moat).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_compute_capital_moat, frontier_ai_labs).
narrative_ontology:constraint_victim(ai_compute_capital_moat, open_source_ai_community).
narrative_ontology:constraint_victim(ai_compute_capital_moat, academic_ai_researchers).
narrative_ontology:constraint_victim(ai_compute_capital_moat, smaller_ai_startups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The open-source AI community lacks the resources to compete with frontier AI labs, faces an increasingly difficult playing field, and is effectively trapped. They are the targets of the moat.
constraint_indexing:constraint_classification(ai_compute_capital_moat, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Academic researchers face funding constraints, limiting their access to compute resources. They are also constrained by the publish-or-perish system. They are significantly disadvantaged by the capital moat.
constraint_indexing:constraint_classification(ai_compute_capital_moat, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Smaller AI startups struggle to compete with well-funded frontier AI labs in attracting talent and securing compute resources. They are constrained by capital.
constraint_indexing:constraint_classification(ai_compute_capital_moat, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Frontier AI labs benefit from the capital moat, allowing them to attract top talent, build massive compute clusters, and maintain a competitive advantage. They can arbitrage talent and compute resources across the globe.
constraint_indexing:constraint_classification(ai_compute_capital_moat, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational perspective, the AI compute and capital moat poses a significant barrier to entry for smaller players and the open-source community, concentrating power and resources in the hands of a few dominant players. Coordination within these powerful actors happens even as they extract from other players. 
constraint_indexing:constraint_classification(ai_compute_capital_moat, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_compute_capital_moat_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_compute_capital_moat, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_compute_capital_moat, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_compute_capital_moat, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_compute_capital_moat_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The capital moat creates a significant barrier to entry for smaller players and the open-source community, extracting their potential contributions and limiting innovation. Suppression (0.70): High. The high cost of compute and talent effectively suppresses competition from smaller players. Theater ratio (0.20): Low. The effort is very functionally oriented in building capability, and less about performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   The frontier AI labs experience the capital requirements as coordination - they solve the difficult problem of building and managing talent and infrastructure. The open-source community, academic researchers, and smaller startups experience it as a significant barrier and a source of extraction. The analytical observer sees a concentrated ecosystem with limited distribution.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier AI labs (institutional/arbitrage) are beneficiaries with a negative d. The open-source community (powerless/trapped), academic researchers (moderate/constrained) and smaller startups (moderate/constrained) are victims with high d values. The directionality values appropriately reflect the structural relationships and power dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by considering the perspectives of different stakeholders. While the capital moat may be necessary for developing frontier AI models, it also creates a significant barrier to entry and concentrates power and resources, so a pure coordination framing would be inaccurate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hardware_commoditization,
    'How quickly will specialized AI hardware become commoditized and accessible?',
    'Track the development and adoption of open-source hardware and cloud-based AI compute services.',
    'If hardware commoditizes quickly, the capital moat will weaken. If it remains specialized and expensive, the moat will persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hardware_commoditization, empirical, 'Rate of AI hardware commoditization').

omega_variable(
    algorithmic_efficiency,
    'To what extent will algorithmic innovations reduce the compute requirements for training frontier AI models?',
    'Monitor advances in model compression, pruning, and other efficiency techniques.',
    'Significant algorithmic improvements could shrink the compute requirements and erode the capital moat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_efficiency, empirical, 'Impact of algorithmic efficiency gains on compute requirements').

omega_variable(
    decentralized_training,
    'Will decentralized training methods enable collaborative model development across smaller organizations and individuals?',
    'Evaluate the scalability and effectiveness of federated learning and other distributed training approaches.',
    'Successful decentralized training could bypass the need for massive compute clusters and weaken the capital moat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decentralized_training, empirical, 'Feasibility and impact of decentralized training').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_compute_capital_moat, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_c_tr_t0, ai_compute_capital_moat, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_c_tr_t5, ai_compute_capital_moat, theater_ratio, 5, 0.15).
narrative_ontology:measurement(ai_c_tr_t10, ai_compute_capital_moat, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_c_be_t0, ai_compute_capital_moat, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ai_c_be_t5, ai_compute_capital_moat, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(ai_c_be_t10, ai_compute_capital_moat, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_compute_capital_moat, resource_allocation).
narrative_ontology:affects_constraint(ai_compute_capital_moat, ai_model_proliferation).
narrative_ontology:affects_constraint(ai_compute_capital_moat, ai_talent_pipeline).

% DUAL FORMULATION NOTE:
% The AI compute and capital moat is distinct from AI model proliferation but affects it. Similarly, it is tied to the AI talent pipeline but represents a separate constraint on entry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
