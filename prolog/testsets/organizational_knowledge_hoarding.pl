% ============================================================================
% CONSTRAINT STORY: organizational_knowledge_hoarding
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_organizational_knowledge_hoarding, []).

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
 *   constraint_id: organizational_knowledge_hoarding
 *   human_readable: Organizational Knowledge Hoarding
 *   domain: organizational/institutional
 *
 * SUMMARY:
 *   Organizational knowledge hoarding creates a hybrid constraint combining
 *   genuine coordination challenges (tacit expertise, crediting skilled work)
 *   with asymmetric extraction (gatekeeping career advancement, controlling
 *   organizational succession). Junior employees and organizational
 *   continuity function bear the costs; knowledge gatekeepers and siloed
 *   departments capture the benefits. The constraint exhibits all major DR
 *   types from different structural positions. A powerless junior employee
 *   experiences pure extraction (Snare). A knowledge gatekeeper in moderate
 *   position experiences mixed coordination and extraction (Tangled Rope):
 *   they solve the legitimate problem of establishing expertise while
 *   simultaneously hoarding for job security. An organization with strong
 *   knowledge management systems sees hoarding as a coordination problem
 *   (Rope). An organized reform initiative (knowledge management program,
 *   documentation, mentorship policies) creates a sunset pathway (Scaffold).
 *   Existing departmental silos persist through institutional inertia
 *   (Piton). The analytical observer risks naturalizing hoarding as inherent
 *   to human nature (Mountain), when it is actually a contingent outcome of
 *   organizational incentives and management philosophy. The extractiveness
 *   value (0.58) reflects moderate but growing extraction: gatekeepers
 *   benefit from knowledge scarcity, but the benefit is constrained by
 *   organizational need for some knowledge transfer. The theater ratio (0.58)
 *   reflects that corporate knowledge management initiatives (training
 *   budgets, documentation systems, knowledge wiki projects) are often
 *   performative — they ritualize commitment to knowledge sharing without
 *   removing the incentives that drive hoarding.
 *
 * KEY AGENTS:
 *   - Junior Employees: Primary victims (powerless/trapped) — cannot advance without access to knowledge controlled by gatekeepers; cannot exit without career penalty
 *   - Knowledge Gatekeepers: Primary beneficiaries (moderate/constrained) — benefit from knowledge scarcity and job security; constrained by organizational need for some transfer
 *   - Successor Planning Function: Victim (powerless/trapped) — cannot guarantee continuity when knowledge is tacit and concentrated; trapped by gatekeeper dependency
 *   - Knowledge-Transferable Organization: Observer (powerful/mobile) — sees hoarding as coordination failure; has resources and ability to exit to better-managed firms
 *   - Knowledge Management Reform Initiative: Organized reform (organized/constrained) — implements documentation, mentorship, training programs as sunset pathways to reduce gatekeeping
 *   - Siloed Departments: Institutional actor (institutional/arbitrage) — maintains departmental boundaries and information silos through policy inertia despite rhetorical commitment to sharing
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent organizational incentives as immutable human nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(organizational_knowledge_hoarding, 0.58).
domain_priors:suppression_score(organizational_knowledge_hoarding, 0.65).
domain_priors:theater_ratio(organizational_knowledge_hoarding, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(organizational_knowledge_hoarding, extractiveness, 0.58).
narrative_ontology:constraint_metric(organizational_knowledge_hoarding, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(organizational_knowledge_hoarding, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(organizational_knowledge_hoarding, tangled_rope).
narrative_ontology:human_readable(organizational_knowledge_hoarding, "Organizational Knowledge Hoarding").
narrative_ontology:topic_domain(organizational_knowledge_hoarding, "organizational/institutional").

domain_priors:requires_active_enforcement(organizational_knowledge_hoarding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(organizational_knowledge_hoarding, knowledge_gatekeepers).
narrative_ontology:constraint_victim(organizational_knowledge_hoarding, organization_efficiency).
narrative_ontology:constraint_victim(organizational_knowledge_hoarding, junior_employees).
narrative_ontology:constraint_victim(organizational_knowledge_hoarding, successor_planning).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JUNIOR EMPLOYEE (SNARE) — Trapped in organizational hierarchy with no access to critical knowledge held by senior gatekeepers. Cannot advance without permission from knowledge holders; cannot exit without severe career penalty. Maximum extraction: career progression depends on goodwill of knowledge holders, creating dependency.
constraint_indexing:constraint_classification(organizational_knowledge_hoarding, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SUCCESSOR PLANNING / INSTITUTIONAL CONTINUITY (SNARE) — Trapped when knowledge is tacit and concentrated. Cannot guarantee organizational continuity; cannot exit the dependency on original knowledge holder. When gatekeeping restricts knowledge transfer, succession becomes impossible without retention of the gatekeeper or catastrophic continuity loss.
constraint_indexing:constraint_classification(organizational_knowledge_hoarding, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: KNOWLEDGE GATEKEEPER (TANGLED ROPE) — Moderate power, constrained exit. Hoarding solves a genuine coordination problem (crediting expertise, maintaining value of learned skills) while simultaneously extracting: job security, influence, higher compensation. Benefits from knowledge scarcity; bears cost of organizational inefficiency. Cannot fully exit without losing accumulated advantage.
constraint_indexing:constraint_classification(organizational_knowledge_hoarding, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: KNOWLEDGE-TRANSFERABLE ORGANIZATION (ROPE) — Organizations with strong knowledge management systems, documentation practices, and mentorship norms see hoarding as a coordination problem to solve collaboratively. Mobile senior staff can exit to better-run organizations; the constraint motivates knowledge transfer practices that benefit all.
constraint_indexing:constraint_classification(organizational_knowledge_hoarding, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: KNOWLEDGE MANAGEMENT REFORM (SCAFFOLD) — Organized stakeholders (HR, management, training programs) implement documented processes, wiki systems, and structured onboarding to build alternative knowledge pathways. Reduces extraction through sunset clause: as institutional memory becomes externalized and systematized, gatekeeping loses force.
constraint_indexing:constraint_classification(organizational_knowledge_hoarding, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SILOED DEPARTMENT (PITON) — Departmental boundaries and information silos persist through institutional inertia. Theater ratio high (0.58): cross-department knowledge sharing is ostensibly encouraged by corporate communications and training budgets but functionally blocked by siloing policies. The ritual of knowledge management persists while actual transfer remains constrained.
constraint_indexing:constraint_classification(organizational_knowledge_hoarding, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, some knowledge hoarding may appear inherent to human nature: people naturally value and protect expertise, knowledge creation requires motivated effort, and tacit knowledge cannot be fully transferred. This perspective risks naturalizing what is actually a contingent organizational choice — the analytical observer's tendency to treat systemic properties as immutable.
constraint_indexing:constraint_classification(organizational_knowledge_hoarding, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(organizational_knowledge_hoarding_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(organizational_knowledge_hoarding, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(organizational_knowledge_hoarding, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(organizational_knowledge_hoarding, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(organizational_knowledge_hoarding, TR),
    TR >= 0.70.

:- end_tests(organizational_knowledge_hoarding_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Gatekeepers capture real benefits (job security, career advancement leverage, compensation premium from scarcity) but the constraint is not maximal extraction because organizations require SOME knowledge transfer to function. The value reflects sustained asymmetry: gatekeepers extract enough to maintain advantage (preventing their replacement) without triggering cascading organizational failure. The 4-year trajectory from 0.42 to 0.58 reflects accumulation: knowledge becomes more critical as organizations grow in complexity, and gatekeepers progressively restrict access to maintain scarcity value. Suppression (0.65): High. Multiple barriers restrict junior employees' access: no time allocated for training, documentation systems inadequate, mentorship informal and gated, knowledge transfer explicitly discouraged to preserve expertise value. Gatekeepers use resource scarcity and organizational hierarchy to enforce restriction. Theater ratio (0.58): Moderate-high. Corporate knowledge management initiatives (training programs, documentation projects, knowledge wikis) are ostensibly mandated but functionally constrained by lack of enforcement, insufficient time allocation, and continued gatekeeping norms. Employees participate in training theater while actual knowledge transfer remains blocked by gatekeeping incentives.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap separates beneficiary and victim positions. Gatekeepers (moderate/constrained) experience the constraint as legitimate expertise protection (Tangled Rope coordination function visible to them). Junior employees (powerless/trapped) experience the same constraint as pure career blocking (Snare extraction visible to them). The gatekeeper sees training they refused to provide as 'on the job learning'; the junior employee sees deliberate withholding. An organization with transparent systems sees the same knowledge requirement as solvable coordination (Rope); an organization with gatekeeping culture sees it as inherent limitation (Mountain). This perspectival gap reveals the constraint's contingency: the classification depends entirely on organizational choices, not on intrinsic properties of knowledge.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position: beneficiary status, victim status, and exit options. Junior employees are trapped victims (d ≈ 0.95) — high directionality toward being targets of extraction. Gatekeepers are beneficiaries with constrained exit (d ≈ 0.45) — moderate directionality; they benefit but cannot fully escape the organizational role that depends on hoarding. Successor planning is a trapped victim (d ≈ 0.95) — cannot exit institutional continuity function or organize against gatekeeper control. Organizations with knowledge transfer systems have institutional beneficiary status with arbitrage options (d ≈ 0.15) — low directionality; these actors have solved the coordination problem and can exit to opportunities elsewhere. Knowledge management reforms create organized actors with constrained exit (d ≈ 0.55) — moderate-high directionality; they perceive extraction but also see exit path (sunset clause). Siloed departments operate with institutional arbitrage (d ≈ 0.20) — low directionality; they maintain status quo through policy inertia with minimal direct extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that organizational knowledge hoarding is a genuine hybrid: it solves a real coordination problem (how to incentivize knowledge creation and expertise development) while simultaneously enabling extraction (gatekeeping career advancement and organizational continuity). The tangled_rope classification correctly identifies both functions. The error in naive analysis is collapsing this into either pure coordination (organizations that have solved the incentive problem) or pure extraction (organizations trapped in gatekeeping culture). The framework's indexical approach reveals that BOTH readings are structurally correct from their respective positions — the powerless junior employee genuinely experiences extraction; the gatekeeper genuinely coordinates expertise contribution. The resolution lies not in claiming one type is 'true' but in recognizing that organizational incentive structures determine which type dominates, and these incentives are changeable (creating the Scaffold sunset possibility).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_vs_codifiable_knowledge,
    'What proportion of hoarded knowledge is genuinely tacit (transfer-resistant) versus strategically withholded codifiable knowledge?',
    'Process documentation audit; successful transfer case studies; measurement of knowledge transfer success rates with explicit effort vs without gatekeeper cooperation',
    'If primarily tacit: hoarding is coordination problem (higher Rope/Scaffold classification). If primarily codifiable: hoarding is extractive (higher Snare/Tangled Rope classification). This determines whether the constraint is a temporary coordination failure or structural extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_vs_codifiable_knowledge, empirical, 'Proportion of hoarded knowledge that is genuinely tacit vs strategically withheld').

omega_variable(
    gatekeeper_motivation_decomposition,
    'Does gatekeeper behavior reflect job security concerns (trapped by own expertise), career advancement incentives (constrained optimization), or identity fusion with expertise (identity_locked)?',
    'Behavioral analysis of gatekeepers offered alternatives: job security guarantees, career advancement without knowledge retention, or identity-affirming roles. Which changes hoarding behavior?',
    'If primarily security concerns: gatekeeper classification is trapped/constrained (higher Snare). If identity-fused: gatekeeper classification is identity_locked (different extraction mechanism). If advancement incentive: gatekeeper is constrained/mobile (Tangled Rope). This affects whether the solution is guarantee + restructuring, or identity reframing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_motivation_decomposition, empirical, 'Primary gatekeeper motivation: security, advancement, or identity').

omega_variable(
    knowledge_transfer_resistance_source,
    'Is suppression sourced from structural barriers (no time/budget for training, no documentation systems) or from active gatekeeping (deliberate refusal to share, creating artificial scarcity)?',
    'Organizational audit of training budgets, documentation tools, time allocation for knowledge sharing; analysis of gatekeepers'' behavior when barriers are removed; comparison of knowledge transfer rates in resource-constrained vs resource-adequate departments',
    'If structural barriers: measured suppression (0.65) reflects coordination failure that scales (higher with scope). If active gatekeeping: suppression reflects enforcement (stays high despite resource investment). This determines whether the constraint persists after scaffolding (Scaffold sunset achievable) or requires behavioral change (may become Piton despite reform).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_transfer_resistance_source, empirical, 'Whether suppression is structural barrier or active gatekeeping').

omega_variable(
    organizational_context_dependence,
    'Does the constraint''s severity depend on organizational size, industry, or management philosophy? Is knowledge hoarding inevitable in certain contexts?',
    'Comparative analysis across organizations of different sizes, industries, and management models; identification of organizations with low hoarding despite high knowledge complexity; correlation analysis between management transparency norms and knowledge transfer rates',
    'If context-dependent: constraint is contingent (Tangled Rope → Rope with better management). If universal: constraint approaches Mountain. This determines whether reform is realistic or whether some hoarding is inherent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(organizational_context_dependence, empirical, 'Context-dependence of knowledge hoarding severity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(organizational_knowledge_hoarding, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(knowhoard_tr_t0, organizational_knowledge_hoarding, theater_ratio, 0, 0.35).
narrative_ontology:measurement(knowhoard_tr_t5, organizational_knowledge_hoarding, theater_ratio, 5, 0.48).
narrative_ontology:measurement(knowhoard_tr_t10, organizational_knowledge_hoarding, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(knowhoard_be_t0, organizational_knowledge_hoarding, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(knowhoard_be_t5, organizational_knowledge_hoarding, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(knowhoard_be_t10, organizational_knowledge_hoarding, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(organizational_knowledge_hoarding, identity_coordination).
narrative_ontology:affects_constraint(organizational_knowledge_hoarding, organizational_succession_risk).
narrative_ontology:affects_constraint(organizational_knowledge_hoarding, career_advancement_blocking).
narrative_ontology:affects_constraint(organizational_knowledge_hoarding, institutional_knowledge_loss).

% DUAL FORMULATION NOTE:
% Knowledge hoarding is upstream of specific career advancement and succession failures but represents a distinct structural constraint. Career advancement blocking (constraint: career_advancement_blocking) and succession risk (constraint: organizational_succession_risk) are downstream manifestations; knowledge hoarding is the enabling mechanism. Institutional knowledge loss (constraint: institutional_knowledge_loss) is a longer-timescale manifestation of unaddressed hoarding. All three are linked via affects_constraints and should be analyzed as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(organizational_knowledge_hoarding, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
