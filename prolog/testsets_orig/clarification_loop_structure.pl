% ============================================================================
% CONSTRAINT STORY: clarification_loop_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_clarification_loop_structure, []).

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
 *   constraint_id: clarification_loop_structure
 *   human_readable: Clarification Loop Structure in Multi-Stage Research Protocols
 *   domain: epistemic_methodology/protocol_application
 *
 * SUMMARY:
 *   The clarification loop structure in multi-stage research protocols
 *   creates a coordination gap between pipeline stages. When a research
 *   context generates a clarification request signaling ambiguity, the
 *   modular architecture allows downstream protocols (like UKE_SCOPE) to be
 *   invoked anyway, despite the unresolved ambiguity. This constraint sits
 *   downstream of two mountain constraints: input_specification_failure (the
 *   user's input was genuinely ambiguous) and
 *   protocol_execution_impossibility (the downstream protocol cannot execute
 *   without resolved input). The clarification loop structure itself is not a
 *   mountain — it is a contingent architectural choice that creates
 *   asymmetric costs. The system architects benefit from protocol modularity
 *   and reusability; end users bear the cost of coordination gaps when
 *   clarification signals are not propagated or enforced across stage
 *   boundaries. The constraint exhibits increasing extractiveness over time
 *   as protocol composition becomes more complex and clarification requests
 *   become more frequent, while theater ratio increases as clarification
 *   loops become ritualized (generated but not acted upon).
 *
 * KEY AGENTS:
 *   - End User in Active Research Context: Primary victim (powerless/trapped) — generated clarification request but downstream protocol invoked anyway; experiences coordination gap but recognizes modular value
 *   - Research Operator: Secondary victim (moderate/constrained) — can work around gaps manually but at cognitive cost; also benefits from protocol reuse when it works
 *   - System Architect: Primary beneficiary (institutional/arbitrage) — benefits from modular protocol design; can arbitrage between implementations
 *   - Protocol Standards Coalition: Organized agents (organized/mobile) — developing clarification-aware routing and explicit handoff protocols to close the coordination gap
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees mixed coordination (protocol reusability) and extraction (user-borne cost of gaps)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(clarification_loop_structure, 0.38).
domain_priors:suppression_score(clarification_loop_structure, 0.42).
domain_priors:theater_ratio(clarification_loop_structure, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(clarification_loop_structure, extractiveness, 0.38).
narrative_ontology:constraint_metric(clarification_loop_structure, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(clarification_loop_structure, theater_ratio, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(clarification_loop_structure, tangled_rope).
narrative_ontology:human_readable(clarification_loop_structure, "Clarification Loop Structure in Multi-Stage Research Protocols").
narrative_ontology:topic_domain(clarification_loop_structure, "epistemic_methodology/protocol_application").

domain_priors:requires_active_enforcement(clarification_loop_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(clarification_loop_structure, protocol_modularity).
narrative_ontology:constraint_beneficiary(clarification_loop_structure, system_architects).
narrative_ontology:constraint_victim(clarification_loop_structure, user_experience).
narrative_ontology:constraint_victim(clarification_loop_structure, research_context_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (TANGLED ROPE) — Trapped in coordination gap but recognizes the modular protocol structure provides value in other contexts. Generated clarification request signaling ambiguity; downstream protocol invoked anyway. Bears significant cost of protocol mismatch but the architecture is not purely extractive — when clarification is properly handled, the system works. Experiences high extraction but not pure snare because the coordination function is real.
constraint_indexing:constraint_classification(clarification_loop_structure, tangled_rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: RESEARCH OPERATOR (TANGLED ROPE) — Constrained by pipeline architecture but benefits from modular protocol structure when it works correctly. Experiences both coordination (protocol reuse across contexts) and extraction (forced to work around clarification gaps). Can partially exit by manual intervention but at significant cognitive cost.
constraint_indexing:constraint_classification(clarification_loop_structure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SYSTEM ARCHITECT (ROPE) — Benefits from modular protocol design. The clarification loop structure enables protocol composition and reuse across diverse research contexts. Experiences the constraint as coordination: the loop exists to handle ambiguity gracefully. Can arbitrage between protocol implementations and has full visibility into the architecture.
constraint_indexing:constraint_classification(clarification_loop_structure, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROTOCOL STANDARDS COALITION (SCAFFOLD) — Organized agents developing improved clarification-aware protocol composition see this as a temporary coordination failure with a sunset. Next-generation pipeline architectures (context-aware routing, clarification-blocking gates, explicit handoff protocols) are being developed to prevent downstream invocation when upstream clarification is pending. Estimated sunset: 3-5 years as standards mature.
constraint_indexing:constraint_classification(clarification_loop_structure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees genuine coordination function (modular protocols enable epistemic division of labor) coupled with asymmetric extraction (users bear cost of coordination gaps). The clarification loop structure solves a real problem (protocol reusability) but creates a secondary problem (stage-transition ambiguity). Mixed coordination and extraction at the system level.
constraint_indexing:constraint_classification(clarification_loop_structure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(clarification_loop_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(clarification_loop_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(clarification_loop_structure, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(clarification_loop_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The coordination gap creates real costs for users who must navigate protocol mismatches, but the extraction is not severe — the modular architecture does provide genuine value through protocol reuse. Reduced from 0.42 to better reflect the substantial coordination function and to move away from the snare boundary. Suppression (0.42): Moderate. Users have limited alternatives when clarification requests are ignored — they cannot force the pipeline to respect clarification-blocking gates, and manual intervention requires technical knowledge. Reduced from 0.48 to reflect that organized agents are actively developing alternatives and the suppression is not as severe as initially estimated. Theater ratio (0.32): Moderate-low. Some clarification loops are genuinely functional (they resolve ambiguity and prevent downstream errors), but an increasing proportion are ritualized (generated as signals but not enforced as gates). Reduced from 0.35 to reflect that most clarification loops still serve their intended function, with theatrical drift being a secondary concern.
 *
 * PERSPECTIVAL GAP:
 *   The end user sees a tangled rope — they explicitly signaled ambiguity and were ignored by the pipeline architecture, bearing significant cost of the resulting mismatch, but they recognize the modular structure provides value when clarification is properly handled. The research operator sees a tangled rope — the modular structure provides value through protocol reuse but creates coordination gaps that require manual workarounds. The system architect sees a rope — the clarification loop structure is a coordination mechanism that enables protocol composition across diverse contexts. The protocol standards coalition sees a scaffold — the coordination gap is a temporary problem being solved by next-generation clarification-aware architectures. The analytical observer sees a tangled rope at the system level — genuine coordination function coupled with asymmetric extraction. The gap reveals that 'coordination' and 'extraction' are not intrinsic properties of the constraint but perspectival readings determined by the agent's structural position relative to the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   The end user is a victim with trapped exit options — they generated a clarification request (explicit signal of the coordination gap) but have no mechanism to prevent downstream invocation. However, the user also recognizes that the modular protocol structure provides value in other contexts, which moderates the experienced extraction compared to a pure snare. The research operator is both victim (bears cost of gaps) and beneficiary (gains from protocol reuse), with constrained exit (can manually intervene but at cost) — this produces moderate directionality. The system architect is a beneficiary with arbitrage exit (can choose between implementations and has full architectural visibility) — this produces low directionality and low or negative experienced extraction. The protocol standards coalition is organized with mobile exit (can develop and adopt alternative architectures) — this produces moderate directionality but with a sunset logic (the constraint is temporary). The analytical observer sees the mixed structure: genuine coordination function (modularity) coupled with asymmetric extraction (user-borne gap cost).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the clarification loop structure has both a genuine coordination function (protocol modularity and reusability) AND asymmetric extraction (users bear cost of coordination gaps). It is not pure coordination (rope) because users experience real costs when clarification signals are not enforced. It is not pure extraction (snare) because the modular architecture does provide genuine value and the coordination gap is being addressed by organized agents. The tangled rope classification captures the mixed structure: the constraint solves a real coordination problem (protocol composition) but does so in a way that concentrates costs on users who have the least power to exit or modify the architecture. The perspectival gap between the user's tangled rope experience (high extraction but recognized coordination) and the architect's rope experience (pure coordination) is the diagnostic signal — the same structural phenomenon appears as mixed extraction-coordination from below and pure coordination from above, revealing the asymmetry. The moderate extractiveness (0.38) reflects that the coordination function is substantial and the extraction, while real, is not overwhelming.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    clarification_blocking_threshold,
    'At what confidence threshold should a clarification request block downstream protocol invocation?',
    'Empirical analysis of clarification request outcomes: correlation between request confidence and downstream protocol success rates; cost-benefit analysis of blocking vs proceeding under ambiguity',
    'If threshold too low: excessive blocking reduces protocol throughput. If threshold too high: coordination gaps persist and users bear extraction cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clarification_blocking_threshold, empirical, 'Confidence threshold for clarification-blocking gate').

omega_variable(
    context_handoff_sufficiency,
    'Does the current context handoff protocol preserve sufficient information for downstream stages to detect upstream clarification requests?',
    'Protocol trace analysis: identification of dropped context signals; comparison of clarification request visibility across pipeline stages',
    'If context handoff is insufficient: architectural fix required (not just threshold tuning). If sufficient: coordination gap is a policy choice rather than a technical limitation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(context_handoff_sufficiency, empirical, 'Whether context handoff preserves clarification signals').

omega_variable(
    modularity_extraction_tradeoff,
    'Is the extraction cost borne by users an inherent tradeoff of modular protocol design, or a contingent feature of the current implementation?',
    'Comparative analysis of alternative pipeline architectures: monolithic vs modular with clarification-aware routing vs modular with explicit handoff gates',
    'If inherent: tangled rope classification is structural (coordination requires this extraction). If contingent: current implementation is a snare that could be redesigned as rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modularity_extraction_tradeoff, conceptual, 'Whether modularity inherently requires user-borne extraction cost').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(clarification_loop_structure, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clarif_tr_t0, clarification_loop_structure, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clarif_tr_t3, clarification_loop_structure, theater_ratio, 3, 0.26).
narrative_ontology:measurement(clarif_tr_t6, clarification_loop_structure, theater_ratio, 6, 0.32).

% Extraction over time
narrative_ontology:measurement(clarif_be_t0, clarification_loop_structure, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(clarif_be_t3, clarification_loop_structure, base_extractiveness, 3, 0.34).
narrative_ontology:measurement(clarif_be_t6, clarification_loop_structure, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(clarification_loop_structure, information_standard).

% DUAL FORMULATION NOTE:
% This constraint sits downstream of input_specification_failure and protocol_execution_impossibility (both mountains). The upstream constraints establish that the user's input was genuinely ambiguous and the downstream protocol cannot execute without resolution. The clarification loop structure is the contingent architectural choice that determines how the pipeline handles this ambiguity. The constraint family structure is: input_specification_failure (mountain) → clarification_loop_structure (tangled_rope) ← protocol_execution_impossibility (mountain). The two mountains constrain the space of possible solutions; the clarification loop structure is the implemented solution with its own extractiveness profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
