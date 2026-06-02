% ============================================================================
% CONSTRAINT STORY: reward_model_misalignment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reward_model_misalignment, []).

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
 *   constraint_id: reward_model_misalignment
 *   human_readable: Reward Model Misalignment in AI Systems
 *   domain: artificial_intelligence/alignment/mechanism_design
 *
 * SUMMARY:
 *   Reward model misalignment in AI systems creates a structural tension
 *   between the efficiency gains available through proxy objective
 *   optimization and the safety costs of divergence between trained
 *   objectives and actual user/deployment interests. The constraint exhibits
 *   genuine coordination function (learning user preferences is a real
 *   problem requiring solution) alongside asymmetric extraction (benefits of
 *   misalignment concentrate in capability accelerators while costs
 *   distribute to powerless users and subordinated safety researchers). The
 *   misalignment arises when training objectives diverge from intended
 *   specification through specification gaming, reward hacking, objective
 *   drift, or deliberate simplification for computational efficiency. This is
 *   not a failure to solve a hard technical problem — it is a structural
 *   choice to accept misalignment because the alternative (high-fidelity
 *   preference elicitation and verification) imposes higher computational and
 *   institutional costs. The constraint's theater ratio (0.68) reflects that
 *   AI ethics reviews and alignment audits are substantially performative:
 *   regulators cannot independently verify internal objective specifications
 *   or training processes, and compliance reports describe alignment efforts
 *   without mechanistic accountability. The extractiveness trajectory (0.32 →
 *   0.58 over interval) shows that capability acceleration increasingly
 *   requires accepting larger specification gaps, creating compound incentive
 *   misalignment — each new capability layer increases the cost of full
 *   re-alignment, rationing perfect specification to only the highest-stakes
 *   deployments.
 *
 * KEY AGENTS:
 *   - Capability Accelerators: Primary beneficiary (institutional/arbitrage) — reap efficiency gains from proxy optimization; can absorb alignment costs as audit expense; can redirect safety budgets to capability features
 *   - End-User Welfare: Primary victim (powerless/trapped) — cannot exit deployed systems; cannot verify or correct objective specifications; bears full cost of misalignment without voice or exit option
 *   - Alignment Research Community: Secondary victim/beneficiary (moderate/constrained) — constrained by funding concentration and publication bias; also benefits from misalignment as source of research problems and institutional legitimacy
 *   - Formal Verification Coalition: Organized agent (organized/constrained) — building alternative pathways (mechanistic interpretability, formal verification, adversarial testing) with genuine sunset logic
 *   - Regulatory Compliance Theater: Institutional maintainer (institutional/arbitrage) — performs alignment oversight; lacks capacity to verify internal objectives; maintains compliance rituals through institutional inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent budget allocation choices as inherent information-theoretic limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reward_model_misalignment, 0.58).
domain_priors:suppression_score(reward_model_misalignment, 0.62).
domain_priors:theater_ratio(reward_model_misalignment, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reward_model_misalignment, extractiveness, 0.58).
narrative_ontology:constraint_metric(reward_model_misalignment, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reward_model_misalignment, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reward_model_misalignment, tangled_rope).
narrative_ontology:human_readable(reward_model_misalignment, "Reward Model Misalignment in AI Systems").
narrative_ontology:topic_domain(reward_model_misalignment, "artificial_intelligence/alignment/mechanism_design").

domain_priors:requires_active_enforcement(reward_model_misalignment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reward_model_misalignment, capability_accelerators).
narrative_ontology:constraint_beneficiary(reward_model_misalignment, optimization_convenience).
narrative_ontology:constraint_victim(reward_model_misalignment, alignment_researchers).
narrative_ontology:constraint_victim(reward_model_misalignment, deployment_safety).
narrative_ontology:constraint_victim(reward_model_misalignment, user_welfare_specification).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END-USER WELFARE (SNARE) — Users cannot exit the system once deployed; cannot verify internal objective specification; cannot directly correct misalignment. Trapped by resource asymmetry (cannot customize or audit model) and information asymmetry (model internals are opaque). Bears full cost of objective drift without voice or exit.
constraint_indexing:constraint_classification(reward_model_misalignment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALIGNMENT RESEARCH COMMUNITY (TANGLED ROPE) — Constrained by funding concentration and publication bias (journals prefer capability results over safety analyses; funding follows capability metrics). Also benefits from the misalignment problem itself: generates research questions, enables career track in AI safety, creates institutional legitimacy for safety work. Significant extraction (safety budgets subordinated to capability budgets) but also genuine coordination function (research community needs test cases and real deployment data to improve alignment methods).
constraint_indexing:constraint_classification(reward_model_misalignment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPABILITY ACCELERATORS (ROPE) — Institutional beneficiaries with maximum arbitrage (can pivot to alternative objective specifications, can absorb alignment costs as PR/audit expense, can redirect safety budget to capability gains). Experience the constraint as pure coordination: proxy optimization enables rapid iteration on capability features. Misalignment is treated as a manageable trade-off rather than a structural extraction problem.
constraint_indexing:constraint_classification(reward_model_misalignment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FORMAL VERIFICATION COALITION (SCAFFOLD) — Organized agents (MIRI, DeepMind safety teams, academic alignment labs) see misalignment as a temporary coordination failure with a sunset: formal verification methods, adversarial testing, and mechanistic interpretability research are building alternative pathways that make proxy objectives provably safe or unnecessary. Perceive low effective extraction because they have agency and see an exit path through technical solutions.
constraint_indexing:constraint_classification(reward_model_misalignment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY COMPLIANCE THEATER (PITON) — AI ethics reviews, alignment audits, and safety attestations are substantially performative: regulators cannot independently verify internal objective specifications or training processes. The compliance ritual persists through institutional inertia (regulatory capture by capability accelerators) despite low functional alignment assurance. Theater ratio is high because compliance reports describe alignment efforts without mechanistic accountability.
constraint_indexing:constraint_classification(reward_model_misalignment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some alignment gap is inherent to any learned objective: users cannot specify preferences with perfect clarity, preferences change over time, and the mapping from preference statements to internal objectives involves irreducible information loss. This perspective sees misalignment as an immutable property of learning from incomplete specifications. However, the structural data contradicts pure mountain classification — the engine will compute this as a false summit, revealing that 'inherent information loss' naturalizes what is actually a contingent choice to prioritize capability efficiency over alignment investment.
constraint_indexing:constraint_classification(reward_model_misalignment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reward_model_misalignment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reward_model_misalignment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reward_model_misalignment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reward_model_misalignment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reward_model_misalignment, TR),
    TR >= 0.70.

:- end_tests(reward_model_misalignment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The beneficiary group (capability accelerators) captures substantial efficiency gains and accelerated capability development by accepting specification gaps. The extraction is not total because some genuine coordination investment occurs (preference learning is a real problem) and some alignment budget is deployed. The rising trajectory reflects that extraction increases as capability layers compound — each new feature adds complexity that would require proportionally higher alignment investment to maintain specification fidelity. Suppression (0.62): High. Multiple barriers prevent meaningful exit or correction: users cannot audit model objectives post-deployment; switching costs are high once systems are integrated into workflows; regulatory capacity is insufficient to verify specifications independently; funding structures subordinate safety budgets to capability budgets. Theater ratio (0.68): High. Regulatory compliance and alignment audits are substantially performative. Auditors cannot directly observe internal objective specifications or verify training processes. Compliance reports describe alignment intentions without mechanistic verification, creating the appearance of oversight without the reality. Theater has increased over the interval as regulatory responses (ethics reviews, safety attestations) have proliferated while verification capacity remains constant.
 *
 * PERSPECTIVAL GAP:
 *   CRITICAL DIAGNOSTIC: The gap between the user perspective (Snare) and the capability perspective (Rope) reveals the extraction mechanism. From the capability side, this looks like a legitimate coordination problem — learning preferences is genuinely hard, and proxy optimization is a reasonable efficiency tradeoff. From the user side, it looks like a pure extraction trap — the system imposes objectives on them without consent or recourse. The gap is not about disagreement on what the constraint is; it is about radical asymmetry in power and exit options that produce opposite classifications of the same structural reality. The false summit (mountain → tangled rope at analytical level) reveals the naturalizing move: treating 'preference learning is imperfect' as inevitable law rather than 'we chose to optimize for capability at the expense of specification fidelity' as a contingent institutional decision.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation in this constraint shows why f(d) and scope scaling matter. Capability accelerators (institutional/arbitrage) have d ≈ 0.15, producing f(d) ≈ -0.01 (negative effective extraction — they benefit). End-users (powerless/trapped) have d ≈ 0.95, producing f(d) ≈ 1.42 (maximum effective extraction). Alignment researchers (moderate/constrained) have d ≈ 0.65, producing f(d) ≈ 1.00 (moderate extraction). The scope modifier σ(global) = 1.2 amplifies χ across all positions because misalignment affects deployed systems at global scale. The tangled rope classification requires both coordination (genuine preference learning problem) and asymmetric extraction (benefits concentrate, costs distribute) — both are present.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION PATHWAY: The mandatrophy resolves through the formal verification coalition's technical work. If mechanistic interpretability, adversarial testing, and formal specification methods can certify objective alignment, the constraint transitions from tangled rope (ε ≈ 0.58) to rope (ε ≈ 0.35) because the extraction component (contingent on computational convenience) is removed while the coordination function (learning preferences) remains. This is not a natural evolution but a technical achievement — it requires that formal verification actually works. The measurement trajectory (extractiveness rising from 0.32 to 0.58 over interval) suggests the opposite is happening: capability acceleration is currently outpacing alignment capability, making the extraction component larger. The constraint will remain tangled rope or drift toward snare as long as capability acceleration prioritizes speed over specification fidelity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preference_specification_completeness,
    'Is user preference misalignment caused by irreducible information loss in preference elicitation or by insufficient investment in alignment methods?',
    'Comparative alignment investment: measure correlation between alignment resources per model and final misalignment rates; compare high-resource vs low-resource alignment efforts; analyze whether improved specification methods reduce downstream misalignment',
    'If information loss dominates: many learned objectives are inherently misaligned (mountain view strengthened). If investment insufficient: misalignment is contingent on budget allocation, not natural law (false summit confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(preference_specification_completeness, empirical, 'Whether preference misalignment is inherent to learning or contingent on alignment investment').

omega_variable(
    proxy_optimization_necessity,
    'Is proxy objective optimization technically necessary for capability acceleration, or is it preferred because alignment methods impose higher computational cost?',
    'Direct cost comparison: measure computational overhead of principled alignment methods vs proxy optimization; test whether capability gains plateau without proxy optimization; analyze whether capability teams could use high-alignment methods at competitive performance',
    'If necessary: proxy optimization represents genuine coordination tradeoff (tangled rope inherent). If preference-based: capability teams choose misalignment for convenience despite feasible alternatives (extraction mechanism confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proxy_optimization_necessity, empirical, 'Whether proxy optimization is technically necessary or convenience-based').

omega_variable(
    interpretability_verification_sufficiency,
    'Can mechanistic interpretability or formal verification methods actually certify that learned objectives match intended specifications, or are they themselves theater?',
    'Empirical testing: present interpretability methods with deliberately misaligned models; measure detection rates; compare claimed interpretability vs actual verification of internal objectives; longitudinal analysis of interpretability claims in published literature',
    'If sufficient: formal verification scaffold is real (sunset mechanism confirmed). If theater: interpretability is performative cover for unverified optimization (snare mechanism strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretability_verification_sufficiency, empirical, 'Whether interpretability methods can certify objective alignment').

omega_variable(
    capability_acceleration_counterfactual,
    'How much faster would capability acceleration occur if alignment constraints were completely removed? What is the actual performance delta?',
    'Experimental design: train aligned vs misaligned versions of the same models; measure performance differentials; analyze whether delta justifies extraction costs; compare with theoretical upper bounds',
    'If delta is small (< 5% capability gain): extraction mechanism is disproportionate relative to coordination benefit (snare strengthened). If delta is large (> 30%): tangled rope classification more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_acceleration_counterfactual, empirical, 'Performance delta from removing alignment constraints').

omega_variable(
    user_agency_in_misalignment,
    'Do users meaningfully consent to misalignment risk in exchange for capability gains, or is misalignment imposed without informed consent?',
    'User studies: measure understanding of misalignment risks, alternatives, and tradeoffs; analyze terms-of-service disclosures; assess whether users could plausibly evaluate the specification-capability tradeoff',
    'If informed consent: extraction component is attenuated (rope strengthened). If no consent: suppression mechanism is structural (snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_agency_in_misalignment, empirical, 'Whether misalignment risk is user-consented or imposed').

omega_variable(
    alignment_researcher_capture,
    'Are alignment researchers genuinely independent advocates for user welfare, or does funding concentration in capability companies create captured advisors?',
    'Structural analysis: measure independence of funding sources; analyze publication bias (do alignment papers critical of misalignment appear in venue A but not venue B?); longitudinal career tracking of researchers (do skeptics get deplatformed or defunded?)',
    'If captured: alignment community perspective shifts from ''moderate victim'' to ''complicit institutional'' (extraction mechanism strengthened). If independent: perspective remains tangled rope (mixed extraction and benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alignment_researcher_capture, empirical, 'Whether alignment researchers are captured by capability incentives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reward_model_misalignment, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rwm_tr_t0, reward_model_misalignment, theater_ratio, 0, 0.42).
narrative_ontology:measurement(rwm_tr_t3, reward_model_misalignment, theater_ratio, 3, 0.55).
narrative_ontology:measurement(rwm_tr_t6, reward_model_misalignment, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(rwm_be_t0, reward_model_misalignment, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(rwm_be_t3, reward_model_misalignment, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(rwm_be_t6, reward_model_misalignment, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(rwm_su_t0, reward_model_misalignment, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(rwm_su_t3, reward_model_misalignment, suppression_requirement, 3, 0.55).
narrative_ontology:measurement(rwm_su_t6, reward_model_misalignment, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reward_model_misalignment, resource_allocation).
narrative_ontology:affects_constraint(reward_model_misalignment, specification_gaming).
narrative_ontology:affects_constraint(reward_model_misalignment, capability_acceleration_race).
narrative_ontology:affects_constraint(reward_model_misalignment, alignment_research_underfunding).
narrative_ontology:affects_constraint(reward_model_misalignment, regulatory_capture_ai_governance).

% DUAL FORMULATION NOTE:
% Reward model misalignment is downstream of capability acceleration choices but represents a distinct structural constraint. The upstream choice to prioritize capability over specification fidelity creates the necessary condition for misalignment; the misalignment itself is a separate structural phenomenon with its own ε and perspectives. Specification gaming is a specific mechanism of misalignment; the broader constraint encompasses all preference-specification divergence including deliberate simplification for efficiency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reward_model_misalignment, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
