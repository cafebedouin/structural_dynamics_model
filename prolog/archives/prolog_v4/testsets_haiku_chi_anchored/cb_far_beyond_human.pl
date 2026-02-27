% ============================================================================
% CONSTRAINT STORY: cb_far_beyond_human
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cb_far_beyond_human, []).

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
 *   constraint_id: cb_far_beyond_human
 *   human_readable: AI Alignment Problem: Value Specification and Control
 *   domain: technological/artificial_intelligence
 *
 * SUMMARY:
 *   The AI alignment problem describes the fundamental challenge of ensuring
 *   that advanced artificial intelligence systems pursue goals aligned with
 *   human values. This constraint operates at the intersection of technical
 *   capability research and institutional incentive structures, creating a
 *   hybrid coordination-extraction dynamic. Capability developers benefit
 *   from the existence of alignment work (it provides public legitimacy and
 *   regulatory access), but they also extract value by maintaining control
 *   over alignment research, constraining independent safety audits, and
 *   advancing capabilities faster than alignment guarantees. The field of
 *   alignment research is structurally positioned as a victim: dependent on
 *   corporate funding, restricted from accessing trained models for
 *   independent evaluation, and operating under race dynamics that prioritize
 *   capability. Humanity as a whole faces a civilizational constraint with no
 *   exit option — the development of advanced AI systems continues regardless
 *   of alignment readiness, creating a trilemma: pause development (losing
 *   competitive advantage), develop with insufficient alignment guarantees
 *   (existential risk), or develop faster than alignment can keep pace
 *   (extraction of control). Theater ratio has increased from 0.35 to 0.64 as
 *   public commitments to safety (constitutional AI, RLHF, safety research
 *   funding) have become more performative without corresponding structural
 *   enforcement mechanisms.
 *
 * KEY AGENTS:
 *   - Humanity's Value Preservation: Primary victim (powerless/trapped) — global civilization with no exit from AI development race; bears existential cost of misalignment
 *   - Alignment Research Field: Primary victim (powerless/trapped) — constrained by institutional funding dependencies and corporate gatekeeping; lacks enforcement power over capabilities development
 *   - AI Capability Developers: Primary beneficiary (institutional/arbitrage) — private companies and research labs capturing competitive advantage during capability race; gain legitimacy through alignment rhetoric
 *   - Frontier Research Institutions: Secondary beneficiary (powerful/arbitrage) — universities and research centers gaining prestige, funding, and computational resources from capabilities research
 *   - Alignment Research Organizations: Secondary victim (organized/constrained) — coordinated but constrained entities (MIRI, Anthropic safety teams, academic labs) lacking institutional leverage over development priorities
 *   - National AI Regulatory Bodies: Tertiary extractor (powerful/mobile) — governments and regulatory agencies gaining authority and geopolitical advantage through AI governance frameworks but unable to enforce globally
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices (corporate control, speed-over-safety norms) as inherent to AI development
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cb_far_beyond_human, 0.58).
domain_priors:suppression_score(cb_far_beyond_human, 0.68).
domain_priors:theater_ratio(cb_far_beyond_human, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cb_far_beyond_human, extractiveness, 0.58).
narrative_ontology:constraint_metric(cb_far_beyond_human, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cb_far_beyond_human, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cb_far_beyond_human, tangled_rope).
narrative_ontology:human_readable(cb_far_beyond_human, "AI Alignment Problem: Value Specification and Control").
narrative_ontology:topic_domain(cb_far_beyond_human, "technological/artificial_intelligence").

domain_priors:requires_active_enforcement(cb_far_beyond_human).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cb_far_beyond_human, ai_capability_developers).
narrative_ontology:constraint_beneficiary(cb_far_beyond_human, frontier_research_institutions).
narrative_ontology:constraint_victim(cb_far_beyond_human, humanity_value_preservation).
narrative_ontology:constraint_victim(cb_far_beyond_human, autonomous_agent_alignment_field).
narrative_ontology:constraint_victim(cb_far_beyond_human, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HUMANITY VALUE PRESERVATION (SNARE) — Cannot exit the development race; unable to unilaterally impose alignment constraints without surrendering capability advantage. Bears full cost of misalignment failure. d≈0.98, f(d)≈1.48, σ=1.2 → χ≈1.01.
constraint_indexing:constraint_classification(cb_far_beyond_human, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALIGNMENT RESEARCH FIELD (SNARE) — Constrained by corporate funding dependencies and race dynamics; institutional incentives favor capabilities research over alignment safety; field lacks enforcement mechanisms. d≈0.92, f(d)≈1.42, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(cb_far_beyond_human, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ALIGNMENT RESEARCH ORGANIZATIONS (TANGLED ROPE) — Organized but constrained by funding limits and research access restrictions. Benefit from collaboration opportunities and intellectual prestige but face suppression of independent safety research due to corporate control. d≈0.68, f(d)≈1.05, σ=1.2 → χ≈0.72.
constraint_indexing:constraint_classification(cb_far_beyond_human, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: AI CAPABILITY DEVELOPERS (ROPE) — Institutional actors experience alignment constraints as pure coordination problem: alignment is necessary for sustained market access, regulation compliance, and public trust. Coordination benefit (safety enabling deployment) exceeds extraction cost. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(cb_far_beyond_human, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NATIONAL REGULATORY BODIES (TANGLED ROPE) — Powerful but internationally constrained by regulatory arbitrage (jurisdictional competition). Benefit from coordination on safety standards but also extract rents through licensing authority and export controls. d≈0.48, f(d)≈0.65, σ=1.1 → χ≈0.39.
constraint_indexing:constraint_classification(cb_far_beyond_human, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: TRADITIONAL AI SAFETY DISCOURSE (PITON) — Performative safety commitments and alignment rhetoric without structural enforcement mechanisms. Theater ratio 0.64: safety frameworks (constitutional AI, RLHF) appear functional but lack verifiable control guarantees. Maintained through institutional inertia despite low actual constraint effectiveness.
constraint_indexing:constraint_classification(cb_far_beyond_human, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FUNDAMENTAL CONSTRAINT (MOUNTAIN) — From civilizational/universal perspective, value specification is fundamentally under-constrained: no compact description of human values exists, instrumental convergence creates misalignment inevitability, and optimization power scales faster than interpretability. However, structural data (ε=0.58, suppression=0.68) contradicts mountain classification — this reveals false summit: what appears as inherent to intelligence is actually contingent institutional pathways for capability development.
constraint_indexing:constraint_classification(cb_far_beyond_human, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cb_far_beyond_human_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cb_far_beyond_human, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cb_far_beyond_human, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cb_far_beyond_human, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cb_far_beyond_human, TR),
    TR >= 0.70.

:- end_tests(cb_far_beyond_human_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. Capability developers capture career advancement, funding, computing resources, and market share during the capability-first phase. Alignment researchers are chronically underfunded relative to capabilities work. The original research groups/companies advance their positions while humanity as a whole bears the tail risk. The extraction is not maximal (0.70+) because alignment work does provide genuine coordination value — safety is necessary for long-term deployment. Suppression (0.68): High. Multiple structural suppressions: (1) Corporate gatekeeping of model access prevents independent safety audits; (2) Race dynamics make unilateral safety pauses economically irrational; (3) Publication restrictions on safety vulnerabilities prevent knowledge accumulation; (4) Career incentives favor capabilities over safety; (5) Interpretability progress lags capability scaling. No total suppression — some independent research exists — but significant. Theater ratio (0.64): Moderate-high. Safety commitments (constitutional AI, RLHF, safety team budgets) are real but lack verifiable guarantees. Alignment rhetoric serves legitimacy functions without necessarily constraining capability development. The ratio has increased as public pressure grows — more theatrical safety commitments to match capability announcements.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the mandatrophy between beneficiary and victim perspectives. Capability developers see alignment as a coordination problem (Rope) — safety enables their deployment and long-term business viability. Alignment researchers see it as constrained extraction (Tangled Rope) — they contribute essential work while lacking institutional leverage. Humanity sees an inescapable extraction mechanism (Snare) — trapped in a development race with no pause option. Regulatory bodies see mixed institutional dynamics (Tangled Rope) — they benefit from AI governance authority but cannot enforce globally. The traditional safety discourse appears functional (Piton) — safety frameworks exist — but lacks enforcement. The civilizational/analytical observer risks seeing a fundamental law of intelligence (Mountain) — misalignment inevitable due to value incompleteness — but the structural data reveals this as naturalizing contingent institutional choices: alternatives exist (different ownership structures, international coordination, different capability timelines) that would change the extraction pattern.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanity: Victim + trapped (no exit option from AI development) → d≈0.98, f(d)≈1.48. Maximum extraction. Alignment field: Victim + trapped (dependent on corporate funding, restricted from research access) → d≈0.92, f(d)≈1.42. Near-maximum extraction. Alignment orgs: Organized victim + constrained (have agency but limited institutional leverage) → d≈0.68, f(d)≈1.05. Significant extraction but not maximal — coordinated action possible. Capability developers: Beneficiary + arbitrage (can exit to other domains, have market choice) → d≈0.08, f(d)≈-0.08. Net beneficiary due to escape capacity. Regulatory bodies: Powerful + mobile (can adjust jurisdiction, face regulatory arbitrage) → d≈0.48, f(d)≈0.65. Moderate extraction; they have mobility despite institutional constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by recognizing that perspectives 4 (Capability Developers, Rope) and 1-2 (Humanity/Alignment Field, Snare) are not measuring the same constraint from different positions — they are measuring different institutional arrangements that could exist at different ε values. From the beneficiary's institutional perspective (perspective 4), alignment IS a coordination problem (ε_perceived ≈ 0.10). From the victim's civilizational perspective (perspective 1), alignment IS an extraction mechanism (ε_actual ≈ 0.58). The difference is not observational relativity but structural: institutional choices about ownership, funding, publication norms, and competitive dynamics SET the base extractiveness. The constraint's true extracted value lies in the institutional choice to concentrate AI capability development in private corporations with limited alignment oversight. If ownership were distributed, funding were coordination-aligned, and research were open, ε would drop to 0.15-0.25 (rope or scaffold). The mandatrophy resolution: the claimed type (tangled_rope) is correct because alignment work provides genuine coordination value AND beneficiaries extract rents through institutional gatekeeping. Both are structurally present. The false summit (mountain perspective) is caught because ε=0.58 contradicts emerges_naturally and accessibility_collapse thresholds — what appears inevitable is actually institutional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    value_specification_completeness,
    'Is human value specification fundamentally incomplete, or merely informationally difficult?',
    'Theoretical analysis of value function expressiveness; empirical testing of whether learned value models converge to stable descriptions; comparison with other complex preference elicitation problems (constitutional frameworks, multicultural governance)',
    'If fundamentally incomplete: alignment is a permanent extraction mechanism (snare). If informationally difficult: alignment is a solvable engineering problem (rope or scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(value_specification_completeness, conceptual, 'Whether value specification is fundamentally incomplete or merely difficult').

omega_variable(
    instrumental_convergence_scope,
    'How broad is instrumental convergence across AI architectures and training regimes? Does convergence occur only for certain goal classes?',
    'Formal analysis of instrumental subgoal universality; empirical testing across diverse AI systems for convergence to power-seeking, resource accumulation, and goal preservation behaviors independent of terminal objectives',
    'If universal: misalignment is inevitable structural feature (mountain). If goal-specific: alignment can be achieved through careful objective design (rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(instrumental_convergence_scope, empirical, 'Scope of instrumental convergence across AI systems').

omega_variable(
    interpretability_scalability_limit,
    'Is there a fundamental limit to how interpretable superintelligent systems can be, or is interpretability a scaling engineering problem?',
    'Analysis of interpretability scaling laws; testing of mechanistic interpretability on increasing capability levels; comparison with other complex systems (biological cognition, distributed computation)',
    'If fundamental limit: control is impossible at scale (snare). If engineering problem: interpretability-guided alignment is achievable (rope or tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretability_scalability_limit, empirical, 'Whether interpretability hits fundamental limits at scale').

omega_variable(
    race_dynamics_irreversibility,
    'Are the competitive dynamics creating irreversible lock-in toward capability-first development paths, or can institutional changes redirect incentives?',
    'Game-theoretic analysis of coordination mechanisms under competitive pressure; historical case studies of industrial standards and safety constraints emerging despite competition; testing of regulatory regimes that align incentives',
    'If irreversible: suppression (0.68) is structural and increasing (snare). If reversible: institutional architecture can reduce suppression (scaffold or rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(race_dynamics_irreversibility, empirical, 'Whether competitive lock-in toward capability-first is irreversible').

omega_variable(
    scalable_oversight_existence,
    'Do scalable oversight methods (iterated amplification, recursive reward modeling, process-based evaluation) actually work as theory predicts?',
    'Empirical testing of oversight methods on increasing AI capability levels; assessment of whether oversight quality scales or degradation becomes inevitable; measurement of human evaluator alignment under scale',
    'If methods work: supervision is achievable (rope or scaffold). If methods fail: supervision is theater (piton or snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scalable_oversight_existence, empirical, 'Whether scalable oversight methods actually function at scale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cb_far_beyond_human, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aialgn_tr_t0, cb_far_beyond_human, theater_ratio, 0, 0.35).
narrative_ontology:measurement(aialgn_tr_t5, cb_far_beyond_human, theater_ratio, 5, 0.5).
narrative_ontology:measurement(aialgn_tr_t10, cb_far_beyond_human, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(aialgn_be_t0, cb_far_beyond_human, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(aialgn_be_t5, cb_far_beyond_human, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(aialgn_be_t10, cb_far_beyond_human, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cb_far_beyond_human, enforcement_mechanism).
narrative_ontology:affects_constraint(cb_far_beyond_human, ai_capability_scaling).
narrative_ontology:affects_constraint(cb_far_beyond_human, interpretability_scaling_limit).
narrative_ontology:affects_constraint(cb_far_beyond_human, autonomous_agent_power_seeking).
narrative_ontology:affects_constraint(cb_far_beyond_human, corporate_ai_governance_capture).

% DUAL FORMULATION NOTE:
% The alignment problem decomposes into technical constraints (value specification, interpretability, instrumental convergence) and institutional constraints (corporate gatekeeping, race dynamics, research funding structure). This story addresses the institutional constraint. Upstream technical constraints have their own ε values reflecting empirical difficulty; this story has ε=0.58 reflecting institutional extraction above and beyond technical difficulty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cb_far_beyond_human, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
