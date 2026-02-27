% ============================================================================
% CONSTRAINT STORY: citation_collapse_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-04-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_citation_collapse_dynamics, []).

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
 *   constraint_id: citation_collapse_dynamics
 *   human_readable: The Recursive Grounding Decay in LLM Citations
 *   domain: technological/academic/informational
 *
 * SUMMARY:
 *   LLMs citing LLM-generated content eventually sever the link to primary
 *   source material, creating a 'recursive grounding decay'. This poses a
 *   significant threat to epistemic integrity and the research community.
 *   While LLM providers benefit from increased adoption, the long-term
 *   consequences of this decay could be dire.
 *
 * KEY AGENTS:
 *   - LLM Providers: Primary beneficiary (institutional/arbitrage) - benefit from increased adoption.
 *   - Epistemic Integrity: Primary victim (powerless/trapped) - the abstract concept has no means of defending itself against the degradation of knowledge.
 *   - Research Community: Secondary victim (moderate/constrained) - individual researchers are forced to deal with the increasing likelihood of errors.
 *   - Open Knowledge Initiatives: Organized actors (organized/mobile) - may implement ways of verifying ground truth and slowing the spread of falsehoods.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(citation_collapse_dynamics, 0.6).
domain_priors:suppression_score(citation_collapse_dynamics, 0.7).
domain_priors:theater_ratio(citation_collapse_dynamics, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(citation_collapse_dynamics, extractiveness, 0.6).
narrative_ontology:constraint_metric(citation_collapse_dynamics, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(citation_collapse_dynamics, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(citation_collapse_dynamics, tangled_rope).
narrative_ontology:human_readable(citation_collapse_dynamics, "The Recursive Grounding Decay in LLM Citations").
narrative_ontology:topic_domain(citation_collapse_dynamics, "technological/academic/informational").

domain_priors:requires_active_enforcement(citation_collapse_dynamics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(citation_collapse_dynamics, llm_providers).
narrative_ontology:constraint_beneficiary(citation_collapse_dynamics, certain_academic_circles).
narrative_ontology:constraint_victim(citation_collapse_dynamics, epistemic_integrity).
narrative_ontology:constraint_victim(citation_collapse_dynamics, research_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The long-term accumulation of errors in knowledge is a slow-motion disaster. Trapped by its abstract nature, epistemic integrity has no agency. The damage to knowledge accumulates over time.
constraint_indexing:constraint_classification(citation_collapse_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Individual researchers are constrained by the need to access and trust information, even as the grounding decays. They may benefit from access to summarization via LLMs, while bearing costs from the potential propagation of misinformation.
constraint_indexing:constraint_classification(citation_collapse_dynamics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% LLM providers benefit from increased adoption and usage, even if the outputs are less reliable. The immediate economic gains outweigh the longer-term concerns of citation collapse.
constraint_indexing:constraint_classification(citation_collapse_dynamics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Initiatives promoting open access, data provenance, and verification may temporarily reduce extraction, providing a scaffold until more robust solutions are in place.
constraint_indexing:constraint_classification(citation_collapse_dynamics, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Traditional academic journals may become pitons if they fail to adapt to the new information landscape, clinging to increasingly meaningless citation metrics.
constraint_indexing:constraint_classification(citation_collapse_dynamics, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% LLMs can be viewed as beneficial tools in academic research, but the dangers of the recursive grounding decay is worrisome.
constraint_indexing:constraint_classification(citation_collapse_dynamics, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(citation_collapse_dynamics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(citation_collapse_dynamics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(citation_collapse_dynamics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(citation_collapse_dynamics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(citation_collapse_dynamics, TR),
    TR >= 0.70.

:- end_tests(citation_collapse_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): The extraction is high because a significant amount of knowledge can be corrupted. Suppression (0.7): Suppression is also high as there is little that can stop this from occurring with present technology. There are clear incentives for usage even if truthfulness declines. Theater Ratio (0.75): High, as performative aspects of current knowledge production make the issue hard to address.
 *
 * PERSPECTIVAL GAP:
 *   Epistemic integrity is fundamentally powerless against the decay. Researchers will see some benefit and harm, but the overall benefits for them are unclear. Open knowledge initiatives could slow the damage, but ultimately can only provide a scaffold.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective experiences a different relationship to the extraction flow. The LLM providers extract value from the information ecosystem, while epistemic integrity and the research community bear the costs of inaccurate or unsupported claims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ground_truth_detectability,
    'How easy is it to automatically detect if a claim is grounded in primary sources?',
    'Develop better tools and techniques for automated source checking.',
    'If ground truth is easily detectable, less damage will happen to long-term knowledge. If ground truth is difficult to detect, the recursive decay will accelerate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ground_truth_detectability, empirical, 'Ability to detect ground truth automatically.').

omega_variable(
    economic_incentive_alignment,
    'Can economic incentives be aligned to reward accuracy over speed and cost?',
    'Develop alternative funding and recognition models.',
    'If incentives are aligned, there would be much less damage from this problem. If incentives cannot be aligned, there is little that can be done in the long-term.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_incentive_alignment, preference, 'Alignment of economic incentives to reward accuracy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(citation_collapse_dynamics, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cita_tr_t0, citation_collapse_dynamics, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cita_tr_t5, citation_collapse_dynamics, theater_ratio, 5, 0.5).
narrative_ontology:measurement(cita_tr_t10, citation_collapse_dynamics, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(cita_be_t0, citation_collapse_dynamics, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cita_be_t5, citation_collapse_dynamics, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(cita_be_t10, citation_collapse_dynamics, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(citation_collapse_dynamics, information_standard).
narrative_ontology:affects_constraint(citation_collapse_dynamics, information_quality_degradation).
narrative_ontology:affects_constraint(citation_collapse_dynamics, misinformation_proliferation).

% DUAL FORMULATION NOTE:
% This constraint is distinct from misinformation, although they are strongly linked. This is specifically concerned with the decaying of long term grounded knowledge as LLMs become increasingly relied upon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
