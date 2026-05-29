% ============================================================================
% CONSTRAINT STORY: responsibility_misassignment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_responsibility_misassignment, []).

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
 *   constraint_id: responsibility_misassignment
 *   human_readable: Responsibility Misassignment in LLM Interface Framing
 *   domain: human_computer_interaction/cognitive_ergonomics
 *
 * SUMMARY:
 *   The responsibility misassignment constraint emerges from the interaction
 *   between LLM interface design and user mental models inherited from prior
 *   tools. Conversational interfaces create an affordance that resembles
 *   search engines or calculators — tools where the system is responsible for
 *   correctness. But LLMs are probabilistic text generators, not knowledge
 *   retrieval systems, and their outputs require verification. Users who
 *   approach LLMs with a convenience frame (system handles correctness)
 *   experience fabrications and citation errors as disqualifying defects.
 *   Users who approach with a capability-extension frame (user handles
 *   correctness) experience the same outputs as expected friction requiring
 *   verification. The constraint is not that one frame is correct and the
 *   other wrong, but that the interface shape does not clearly signal which
 *   contract applies. This creates coordination friction as users, providers,
 *   and designers negotiate new interaction norms. The low extractiveness
 *   (0.18) reflects that this is primarily a transitional coordination
 *   problem rather than a mechanism that systematically benefits one party at
 *   another's expense. Users can exit to other tools, providers are investing
 *   in clearer uncertainty communication, and the HCI community is developing
 *   interface solutions. The theater ratio (0.35) captures some performative
 *   elements — disclaimers that users ignore, safety warnings that don't
 *   match interaction patterns — but most of the constraint's activity is
 *   functional coordination work.
 *
 * KEY AGENTS:
 *   - Convenience-Seeking Users: Beneficiaries (moderate/mobile) — gain cognitive load reduction from treating LLM as correctness-responsible; can switch tools when precision matters
 *   - LLM Providers: Beneficiaries (institutional/arbitrage) — conversational interface enables mass adoption; also bear reputation risk from misassignment
 *   - Professional Users with Domain Expertise: Beneficiaries (powerful/mobile) — use LLMs as capability-extension tools with clear verification workflows; minimal friction
 *   - HCI Research Community: Organized agents (organized/constrained) — see responsibility ambiguity as coordination problem amenable to design solutions
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees transitional coordination friction as norms stabilize around new tool class
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(responsibility_misassignment, 0.18).
domain_priors:suppression_score(responsibility_misassignment, 0.22).
domain_priors:theater_ratio(responsibility_misassignment, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(responsibility_misassignment, extractiveness, 0.18).
narrative_ontology:constraint_metric(responsibility_misassignment, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(responsibility_misassignment, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(responsibility_misassignment, rope).
narrative_ontology:human_readable(responsibility_misassignment, "Responsibility Misassignment in LLM Interface Framing").
narrative_ontology:topic_domain(responsibility_misassignment, "human_computer_interaction/cognitive_ergonomics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(responsibility_misassignment, llm_providers).
narrative_ontology:constraint_beneficiary(responsibility_misassignment, users_seeking_convenience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONVENIENCE-SEEKING USER (ROPE) — Experiences the interface as a coordination mechanism that solves the problem of accessing information quickly. The conversational framing creates a mental model where the system handles correctness verification, reducing cognitive load. Low extraction because the user chose this tool for exactly this affordance and can switch tools when precision matters.
constraint_indexing:constraint_classification(responsibility_misassignment, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: LLM PROVIDER (ROPE) — The conversational interface is a coordination solution that makes probabilistic text generation accessible to non-technical users. The responsibility ambiguity is not extraction but an unavoidable consequence of bridging statistical models and human communication norms. Providers benefit from adoption but also invest heavily in alignment research and safety disclaimers. Arbitrage exit because providers can pivot to different interface paradigms or market positioning.
constraint_indexing:constraint_classification(responsibility_misassignment, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PROFESSIONAL USER (ROPE) — Domain experts treat LLMs as capability-extension tools, maintaining responsibility for verification. The interface framing creates friction but not extraction — experts already have mental models for tool limitations and verification workflows. Mobile exit because experts can choose specialized tools, hire human assistants, or revert to traditional research methods.
constraint_indexing:constraint_classification(responsibility_misassignment, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: HCI RESEARCH COMMUNITY (ROPE) — Organized researchers see the responsibility ambiguity as a coordination problem amenable to interface design solutions: explicit uncertainty indicators, citation verification tools, mode-switching UI patterns. The constraint coordinates attention on a real design challenge. Constrained exit because the community is professionally committed to solving interface problems rather than abandoning the technology.
constraint_indexing:constraint_classification(responsibility_misassignment, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — The responsibility assignment ambiguity is a coordination problem inherent to introducing probabilistic systems into deterministic-expectation contexts. The conversational interface creates an affordance mismatch, but this is a design challenge, not an extraction mechanism. Users, providers, and researchers are collectively negotiating new interaction contracts. Low extraction because no agent is systematically trapped or coerced — the friction is transitional as norms stabilize.
constraint_indexing:constraint_classification(responsibility_misassignment, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(responsibility_misassignment_tests).
:- end_tests(responsibility_misassignment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The responsibility ambiguity creates friction but not systematic extraction. Users who experience fabrications as disqualifying defects can switch to other tools (search, human experts, specialized databases). Providers invest in alignment research and interface improvements, bearing costs rather than extracting rents. The asymmetry is real — some users are surprised by outputs that don't match their mental models — but the surprise is transitional as norms evolve, not structural. Suppression (0.22): Low. No significant barriers prevent users from adopting verification practices, switching tools, or demanding clearer interfaces. The constraint operates through affordance mismatch and inherited mental models, not through coercion or lack of alternatives. Theater ratio (0.35): Moderate-low. Some performative elements exist (disclaimers users skip, safety warnings that don't match interaction flow), but most interface activity is functional — the conversational paradigm genuinely makes probabilistic text generation accessible to non-technical users. The theater has increased slightly as providers add more warnings without changing core interaction patterns, but it remains below the 0.50 threshold where proxy goals dominate real function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all five perspectives classify as rope. The uniformity reflects the structural reality: the responsibility ambiguity is a coordination problem, not an extraction mechanism. Users who want convenience get it (and accept verification costs when precision matters). Providers who want adoption get it (and invest in safety). Experts who want capability extension get it (and maintain verification workflows). Researchers who want design challenges get them (and propose solutions). The 'AI is unreliable' discourse framed as system failure is a surface phenomenon — users expressing surprise that a probabilistic tool behaves probabilistically — but the underlying structure is coordination, not extraction. The discourse will stabilize as norms evolve and interfaces improve. The constraint's low extractiveness and suppression, combined with beneficiary declarations across all agent types, produce rope classifications across all perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives classify as rope because all agents are beneficiaries of the coordination function with mobile or arbitrage exit options. Convenience-seeking users benefit from cognitive load reduction and can switch tools when precision matters (d ≈ 0.15, mobile exit + beneficiary). LLM providers benefit from adoption enabled by conversational interfaces and have arbitrage exit to different paradigms (d ≈ 0.05, arbitrage exit + beneficiary). Professional users benefit from capability extension and have mobile exit to specialized tools (d ≈ 0.15, mobile exit + beneficiary). The HCI research community benefits from a tractable design challenge and has constrained exit due to professional commitment (d ≈ 0.20, constrained exit + beneficiary, but still low enough for rope). The analytical observer sees coordination friction with no systematic victim (d ≈ 0.72, analytical canonical). No agent is trapped or systematically extracted from — the responsibility ambiguity is a coordination problem all parties are working to resolve.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not trigger mandatrophy analysis (extractiveness 0.18 < 0.70 threshold). The responsibility misassignment is a coordination friction, not a severe extraction mechanism. The rope classification is robust across perspectives because the structural data — low extractiveness, low suppression, beneficiaries across all agent types, mobile/arbitrage exit options — consistently point to coordination rather than extraction. The constraint resolves the potential mandatrophy (is this really just rope, or is hidden extraction present?) by showing that users can exit, providers bear costs, and all parties are negotiating new norms. The omega variables identify empirical uncertainties (does the interface inherently imply correctness? what verification cost triggers user adaptation? do providers benefit from ambiguity?) but none of these uncertainties, if resolved, would reclassify the constraint to a higher-extraction type. The coordination function is genuine and the extraction is minimal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interface_determinism,
    'Does the conversational interface inherently imply correctness guarantees, or is this a learned association from prior tools (search engines, calculators) that users incorrectly transfer?',
    'Longitudinal studies of user mental models across different interface paradigms; cross-cultural comparison of responsibility attribution in societies with different tool-use histories',
    'If inherent: interface redesign is necessary to prevent misassignment. If learned: user education and norm evolution will resolve the ambiguity over time without structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interface_determinism, empirical, 'Whether conversational interfaces inherently imply correctness').

omega_variable(
    verification_cost_threshold,
    'At what verification cost does the convenience frame break down and users revert to capability-extension mental models?',
    'Behavioral studies measuring user verification effort across task types; identification of cost thresholds where users switch from trusting outputs to checking them',
    'If threshold is low: most users will naturally adopt verification practices. If threshold is high: responsibility ambiguity persists and may require interface intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verification_cost_threshold, empirical, 'Cost threshold for user verification behavior').

omega_variable(
    provider_incentive_alignment,
    'Do LLM providers benefit from responsibility ambiguity (users blame themselves for misuse rather than demanding better systems), or does ambiguity create liability and reputation risk that providers want to resolve?',
    'Analysis of provider behavior: investment in uncertainty communication, legal positioning, interface redesign efforts. Comparison of open-source vs commercial provider incentives.',
    'If providers benefit: the coordination framing may be incomplete and some extraction is present. If providers face risk: the rope classification is robust and all parties want clearer contracts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provider_incentive_alignment, empirical, 'Whether providers benefit from responsibility ambiguity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(responsibility_misassignment, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(resp_misassign_tr_t0, responsibility_misassignment, theater_ratio, 0, 0.25).
narrative_ontology:measurement(resp_misassign_tr_t1, responsibility_misassignment, theater_ratio, 1, 0.3).
narrative_ontology:measurement(resp_misassign_tr_t2, responsibility_misassignment, theater_ratio, 2, 0.35).

% Extraction over time
narrative_ontology:measurement(resp_misassign_be_t0, responsibility_misassignment, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(resp_misassign_be_t1, responsibility_misassignment, base_extractiveness, 1, 0.15).
narrative_ontology:measurement(resp_misassign_be_t2, responsibility_misassignment, base_extractiveness, 2, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(responsibility_misassignment, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of state_role_time_collapse (the cognitive difficulty of tracking system state across interaction turns) and frame_mismatch_friction (the general problem of interface affordances not matching underlying system capabilities). The responsibility misassignment is a specific instantiation of frame mismatch in the LLM domain, where the conversational interface creates a convenience-frame affordance that conflicts with the capability-extension contract the system actually offers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
