% ============================================================================
% CONSTRAINT STORY: codex_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_codex_access, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: codex_access
 *   human_readable: OpenAI Codex Access Control
 *   domain: technological/economic
 *
 * SUMMARY:
 *   OpenAI's control of Codex access via restricted API and tiered
 *   application represents a structural constraint combining coordination and
 *   extraction. The constraint creates asymmetric value distribution: OpenAI
 *   and early-adopter enterprises capture disproportionate benefits from
 *   first-mover advantage and API integration, while independent developers,
 *   academic researchers, and the open-source community bear suppression
 *   costs through pricing barriers, rate limits, and feature gatekeeping.
 *   Extractiveness has increased from 0.32 to 0.52 over the measurement
 *   interval as Codex became integral to developer workflows, raising
 *   switching costs. Simultaneously, theater_ratio has declined from 0.55 to
 *   0.48, indicating that the access control mechanism is becoming more
 *   functionally efficient (less performative) as API standardization
 *   matures. The constraint exhibits all six DR types depending on observer
 *   position: independent developers experience pure extraction (Snare),
 *   academic researchers experience mixed coordination-extraction (Tangled
 *   Rope), enterprises and OpenAI experience coordination (Rope), the
 *   open-source coalition sees a temporary problem being solved (Scaffold),
 *   legacy licensing frameworks persist through institutional inertia
 *   (Piton), and the civilizational analytical observer risks naturalizing a
 *   contingent business model as immutable (Mountain).
 *
 * KEY AGENTS:
 *   - OpenAI: Primary beneficiary and constraint enforcer (institutional/arbitrage) — controls access, captures pricing premium, benefits from ecosystem lock-in
 *   - Enterprise Customers: Secondary beneficiary (institutional/arbitrage) — negotiate volume discounts, integrate deeply into workflows, capture competitive advantage from early access
 *   - Independent Developers: Primary victim (powerless/trapped) — depend on API access for productivity, face rate limits and pricing extraction, cannot negotiate terms
 *   - Academic Researchers: Secondary victim (moderate/constrained) — restricted research access tiers, publication restrictions, limited budget for API costs
 *   - Open Source Community: Organized victim (organized/constrained) — developing alternative models (CodeLlama, Phi), building around proprietary dependency
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing proprietary control as inherent to frontier AI capability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(codex_access, 0.52).
domain_priors:suppression_score(codex_access, 0.65).
domain_priors:theater_ratio(codex_access, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(codex_access, extractiveness, 0.52).
narrative_ontology:constraint_metric(codex_access, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(codex_access, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(codex_access, tangled_rope).
narrative_ontology:human_readable(codex_access, "OpenAI Codex Access Control").
narrative_ontology:topic_domain(codex_access, "technological/economic").

domain_priors:requires_active_enforcement(codex_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(codex_access, openai).
narrative_ontology:constraint_beneficiary(codex_access, early_adopter_enterprises).
narrative_ontology:constraint_victim(codex_access, independent_developers).
narrative_ontology:constraint_victim(codex_access, academic_researchers).
narrative_ontology:constraint_victim(codex_access, open_source_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT DEVELOPER (SNARE) — Trapped by market dependency on Codex for competitive productivity in code generation. Cannot exit without material disadvantage relative to enterprises with API access. API rate limits, pricing tiers, and feature gatekeeping create asymmetric extraction. Maximum experienced coercion with minimal coordination benefit.
constraint_indexing:constraint_classification(codex_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ACADEMIC RESEARCHER (TANGLED ROPE) — Constrained by limited research budgets and access tiers that restrict non-commercial research. Benefits from Codex for code generation research but extraction occurs through pricing asymmetry and publication restrictions. Coordination value (standardized benchmark) exists alongside asymmetric cost-sharing.
constraint_indexing:constraint_classification(codex_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OPENAI / ENTERPRISE PARTNERSHIP (ROPE) — OpenAI and early-adopter enterprises experience the constraint as coordination: API standardization enables ecosystem development, enterprise integration, and market expansion. Net beneficiaries experience access control as a coordination mechanism with minimal perceived extraction.
constraint_indexing:constraint_classification(codex_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SOURCE COALITION (SCAFFOLD) — Organized agents (Hugging Face, open-source LLM communities, Meta Llama releases) are building alternative code generation pathways that reduce dependency on Codex. Access control constraint has sunset logic: as open alternatives mature (CodeLlama, Phi models), closed API dependency declines. Coalition has agency and sees exit path.
constraint_indexing:constraint_classification(codex_access, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY LICENSING FRAMEWORK (PITON) — OpenAI's access control persists through institutional inertia from earlier venture-capital funding models and Azure partnership lock-in. The licensing theater (tiers, quotas, rate limits) maintains extraction but has degraded functional utility as market conditions changed. Maintained through contractual obligations and switching costs rather than genuine lock-in advantage.
constraint_indexing:constraint_classification(codex_access, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, proprietary control of frontier AI models might be framed as an immutable natural law: the R&D costs of training large code models are so high that only well-capitalized actors can bear them, making closed access inevitable. However, this perspective risks naturalizing what is a contingent business model choice. The engine's false summit detector will identify this as naturalization — open alternatives demonstrate that the constraint is institutional, not inherent.
constraint_indexing:constraint_classification(codex_access, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(codex_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(codex_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(codex_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(codex_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(codex_access, TR),
    TR >= 0.70.

:- end_tests(codex_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. OpenAI captures substantial surplus through pricing differentiation, rate limiting, and feature gatekeeping. The increase from 0.32 to 0.52 reflects deepening developer dependency as Codex became integral to workflows. However, extraction is not maximal because open alternatives are emerging and developers retain some mobility (can use local models, competitors). Suppression (0.65): High. Independent developers face significant barriers: specialized API knowledge, pricing asymmetry, rate limits that penalize high-volume use, publication restrictions on research, and switching costs from integrated workflows. Academic and open-source actors face additional suppression through explicit access tier restrictions. Theater ratio (0.48): Moderate-low. The access control mechanism is substantially functional — the API gateway, authentication, and rate limiting actually serve technical purposes (cost management, DoS prevention, quality control) alongside extraction. Theater has declined because the mechanism's technical legitimacy has increased as the platform matured. This is not primarily performative enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence across structural positions. OpenAI/enterprise sees coordination (Rope) — the API enables ecosystem development, standardizes interfaces, and allows enterprises to build integrated solutions. Independent developers see extraction (Snare) — they pay for access without negotiating power and face punitive rate limits. Academic researchers see mixed coordination and extraction (Tangled Rope) — the platform enables research but access tiers and pricing asymmetry extract surplus. The open-source coalition sees a sunset (Scaffold) — alternative models are maturing and will reduce dependency. The legacy licensing framework sees degradation (Piton) — the venture capital licensing model persists through contractual obligations but has lost functional utility as market conditions changed. The civilizational observer risks seeing inevitability (Mountain) — the high R&D costs of training large code models might appear to necessitate closed access, but the emergence of well-funded open alternatives (Meta's Llama investment, community Llama derivatives) contradicts this naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural relationship to extraction flow. OpenAI (beneficiary + arbitrage exit) derives low d (≈0.15), producing negative effective extraction χ — they experience the constraint as a coordination benefit. Enterprises (beneficiary + constrained exit via switching costs) derive moderate d (≈0.35), experiencing moderate positive χ. Independent developers (victim + trapped exit due to market dependency) derive high d (≈0.85), experiencing maximum χ — this is the primary extraction flow. Academic researchers (victim + constrained exit through restricted tiers) derive moderate-high d (≈0.70), experiencing substantial χ. Open-source community (organized victim + constrained exit being actively worked to mobilize) derives moderate d (≈0.50-0.55), experiencing moderate χ. The engine computes χ from these d values via the sigmoid function f(d) and scope modifier σ(S=global=1.2), producing effective extractiveness that ranges from negative (beneficiaries) to maximum (trapped victims).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE STRUCTURE CONFIRMED: The constraint satisfies all three Tangled Rope gates. First, genuine coordination function exists: the API standardizes code generation interfaces, enabling enterprises and third-party developers to build integrated solutions without reimplementing underlying models. Second, asymmetric extraction exists: pricing tiers and rate limits impose disproportionate costs on independent developers and academic researchers while subsidizing enterprise volumes. Third, active enforcement is required: OpenAI must actively maintain API access controls, manage rate limits, monitor usage patterns, and enforce licensing terms to sustain the extraction. Without enforcement, developers would exploit unlimited access, breaking the pricing mechanism. The mandatrophy is resolved by recognizing that the Rope coordination function (API standardization) and the Snare extraction function (access gatekeeping) are structurally inseparable. OpenAI cannot provide the coordination benefit without also creating the extraction mechanism. The perspectival gap reflects this structural dual function: agents who benefit from coordination (enterprises) experience it as Rope; agents who bear extraction costs (independent developers) experience it as Snare; agents with mixed relationships (academics) experience Tangled Rope. No single type adequately describes the constraint from all perspectives simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    open_model_parity_timeline,
    'When will open-source code generation models reach feature parity with Codex across key metrics (latency, accuracy, specialized domains)?',
    'Benchmark comparison (HumanEval, MultiPL-E, domain-specific code generation tasks) tracking CodeLlama, Phi, and other open models against Codex; empirical measurements of inference speed and fine-tuning capability',
    'If parity achieved within 18-24 months: Scaffold sunset is real and near-term; Snare classification becomes temporary. If parity delayed beyond 3 years: open alternatives remain niche; Snare and Tangled Rope persist as primary classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_model_parity_timeline, empirical, 'Timeline for open-source code model parity with proprietary Codex').

omega_variable(
    enterprise_lock_in_reversibility,
    'How reversible is enterprise lock-in to Codex API? Can integrated development workflows migrate to open alternatives without substantial refactoring?',
    'Analysis of enterprise integration patterns: API wrapper dependencies, proprietary fine-tuning data, custom model performance SLAs; migration cost estimates for switching to open alternatives',
    'If highly reversible: enterprise exit options are better than constrained, weakening Rope classification. If lock-in is strong: enterprise dependency persists, sustaining the Rope perspective for OpenAI/enterprise partnership.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enterprise_lock_in_reversibility, empirical, 'Reversibility of enterprise lock-in to Codex API infrastructure').

omega_variable(
    pricing_tier_discrimination_intent,
    'Is OpenAI''s pricing tier structure (free tier → paid tiers → enterprise contracts) designed primarily for demand management and cost recovery, or does it intentionally extract surplus from independent developers while subsidizing enterprises?',
    'Analysis of tier design decisions: cost structure vs pricing; comparison with competitor pricing models; internal documentation (if available) on tier targeting; empirical measurement of who bears highest per-inference cost',
    'If primarily cost-recovery: access control is coordination mechanism (Rope dominates). If extraction is intentional: Snare and Tangled Rope classifications are justified. Determines whether suppression (0.65) reflects natural scarcity or artificial gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pricing_tier_discrimination_intent, empirical, 'Whether pricing tiers reflect cost recovery or intentional surplus extraction').

omega_variable(
    knowledge_distillation_asymmetry,
    'Do open models trained on Codex outputs (via knowledge distillation or synthetic data generation) constitute genuine alternative capability or merely reproduce Codex behavior with degraded performance?',
    'Analysis of distilled model performance: direct comparison with Codex on held-out benchmarks; evaluation of capability generalization beyond training data distribution; detection of memorized patterns vs learned abstractions',
    'If genuine alternative: open community can solve the access constraint through indirect learning. If degraded reproduction: open alternatives remain dependent on reverse-engineering proprietary behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_distillation_asymmetry, empirical, 'Whether knowledge distillation produces genuine alternative capability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(codex_access, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(codex_tr_t0, codex_access, theater_ratio, 0, 0.55).
narrative_ontology:measurement(codex_tr_t3, codex_access, theater_ratio, 3, 0.51).
narrative_ontology:measurement(codex_tr_t6, codex_access, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(codex_be_t0, codex_access, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(codex_be_t3, codex_access, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(codex_be_t6, codex_access, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(codex_access, information_standard).
narrative_ontology:affects_constraint(codex_access, large_language_model_training_cost_barrier).
narrative_ontology:affects_constraint(codex_access, enterprise_ai_integration_lock_in).
narrative_ontology:affects_constraint(codex_access, developer_tool_market_concentration).

% DUAL FORMULATION NOTE:
% Codex access control is downstream of training cost economics (LLM training requires massive capital investment) but represents a distinct structural choice about how to distribute access once models are trained. Alternative distribution models (open-source release, public API without gatekeeping) would have different extractiveness values. The constraint story captures OpenAI's specific access control architecture, not the inherent cost structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(codex_access, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
