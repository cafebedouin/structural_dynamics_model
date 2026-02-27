% ============================================================================
% CONSTRAINT STORY: ai_scholar_citation_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_scholar_citation_trap, []).

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
 *   constraint_id: ai_scholar_citation_trap
 *   human_readable: AI Scholarly Citation System Snare
 *   domain: technological
 *
 * SUMMARY:
 *   The AI Scholarly Citation System, designed to streamline research,
 *   synthesizes scientific studies and generates citations akin to human
 *   experts. However, this system creates a structural trap. By automating
 *   citation suggestions, it risks reinforcing existing biases, marginalizing
 *   researchers outside of the AI's influence, and ultimately, undermining
 *   the integrity of the scientific record. The system's reliance on
 *   algorithms introduces opacity and potential for manipulation, turning a
 *   tool designed for coordination into an extractive snare.
 *
 * KEY AGENTS:
 *   - AI Model Developers: Primary beneficiary (institutional/arbitrage) - Gaining prestige and influence by controlling the AI and its citation algorithms.
 *   - Early Adopting Researchers: Secondary beneficiary (moderate/constrained) - Experiencing increased visibility, but also constrained by the AI's biases and limitations.
 *   - Researchers Outside AI System: Primary victim (powerless/trapped) - Lacking access and facing difficulty to compete with the AI system's preferred cited works.
 *   - Scientific Integrity: Abstract victim (powerless/trapped) - compromised and undermined due to algorithmic bias.
 *   - Analytical Observer: analytical/analytical - observes complex dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_scholar_citation_trap, 0.6).
domain_priors:suppression_score(ai_scholar_citation_trap, 0.7).
domain_priors:theater_ratio(ai_scholar_citation_trap, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_scholar_citation_trap, extractiveness, 0.6).
narrative_ontology:constraint_metric(ai_scholar_citation_trap, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_scholar_citation_trap, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_scholar_citation_trap, snare).
narrative_ontology:human_readable(ai_scholar_citation_trap, "AI Scholarly Citation System Snare").
narrative_ontology:topic_domain(ai_scholar_citation_trap, "technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_scholar_citation_trap, ai_model_developers).
narrative_ontology:constraint_beneficiary(ai_scholar_citation_trap, early_adopting_researchers).
narrative_ontology:constraint_victim(ai_scholar_citation_trap, researchers_outside_ai_system).
narrative_ontology:constraint_victim(ai_scholar_citation_trap, scientific_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Researchers who do not use the AI citation system are at a disadvantage. They cannot effectively compete for citations or recognition due to the AI's preferential citation patterns. They are trapped because of the network effects of the AI system.
constraint_indexing:constraint_classification(ai_scholar_citation_trap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Early adopters benefit from increased citations but are also constrained by the AI's biases and limitations. They are tangled in a system that boosts their visibility while potentially undermining the rigor of their work due to over-reliance on AI-selected sources.
constraint_indexing:constraint_classification(ai_scholar_citation_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% Developers benefit from the widespread adoption of their AI model, gaining prestige, influence, and potentially financial rewards. They can arbitrage the system by controlling its citation algorithms and setting the research agenda.
constraint_indexing:constraint_classification(ai_scholar_citation_trap, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The integrity of the scientific record is harmed if the AI system promotes biased or flawed research due to its citation patterns. Ensuring scientific rigour becomes more challenging when the AI's choices aren't transparent or easily verifiable. This aspect is trapped within the unintended consequences of the system.
constraint_indexing:constraint_classification(ai_scholar_citation_trap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% From an analytical perspective, the AI citation system is a tangled rope. It coordinates and accelerates research by providing efficient citation suggestions, but it also introduces extraction by amplifying biases, consolidating power within specific research groups, and potentially skewing the overall direction of scientific inquiry.
constraint_indexing:constraint_classification(ai_scholar_citation_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_scholar_citation_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_scholar_citation_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_scholar_citation_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_scholar_citation_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_scholar_citation_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is estimated at 0.60, reflecting the system's ability to amplify biases and marginalize researchers outside the AI's influence. Suppression is estimated at 0.70 because the system creates significant barriers to those outside it. The theater ratio, at 0.30, indicates relatively low performative content initially but this can increase over time.
 *
 * PERSPECTIVAL GAP:
 *   The perspective of researchers outside the AI system (snare) drastically differs from the AI model developers (rope). Those that rely on the AI citation system (early adopters, tangled rope) experience some benefits but are constrained by its underlying code. Analytical observers are faced with the complicated dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   AI Model Developers gain influence. Early adopting researchers gain visibility. Researchers outside AI and scientific Integrity pay the costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The system is initially intended as a helpful research tool, but the reality has changed it into a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bias_detection_accuracy,
    'How accurately can biases within the AI''s citation algorithm be detected and mitigated?',
    'Independent audits of the AI''s algorithms and citation patterns, comparison with human expert citations.',
    'If biases are easily detected and mitigated: the AI system may function more like a rope. If biases are difficult to detect or cannot be fully mitigated: the system remains a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bias_detection_accuracy, empirical, 'Accuracy of bias detection in AI citation system.').

omega_variable(
    transparency_of_algorithms,
    'How transparent is the AI''s citation algorithm to researchers and the public?',
    'Open-source release of the AI''s code, detailed documentation of its citation selection process.',
    'If the algorithm is transparent: researchers can understand and correct for biases, making the system more like a tangled rope. If the algorithm is opaque: biases are hidden and difficult to address, reinforcing the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_of_algorithms, conceptual, 'Transparency of algorithms in AI citation system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_scholar_citation_trap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_scholar_citation_trap, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_s_tr_t5, ai_scholar_citation_trap, theater_ratio, 5, 0.2).
narrative_ontology:measurement(ai_s_tr_t10, ai_scholar_citation_trap, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_scholar_citation_trap, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ai_s_be_t5, ai_scholar_citation_trap, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ai_s_be_t10, ai_scholar_citation_trap, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_scholar_citation_trap, information_standard).
narrative_ontology:affects_constraint(ai_scholar_citation_trap, scientific_publishing_gatekeeping).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
