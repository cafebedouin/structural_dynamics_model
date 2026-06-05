% ============================================================================
% CONSTRAINT STORY: algorithmic_management_escalation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_management_escalation, []).

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
 *   constraint_id: algorithmic_management_escalation
 *   human_readable: Algorithmic Management Escalation in Platform Labor
 *   domain: labor/platform_economics/algorithmic_governance
 *
 * SUMMARY:
 *   Algorithmic management escalation describes the progressive tightening of
 *   algorithmic control over contingent workers on digital labor platforms
 *   (ride-share, delivery, freelance work) paired with legal classification
 *   of these workers as independent contractors. The constraint exhibits the
 *   hallmark structure of a tangled_rope: there is a genuine coordination
 *   problem (matching supply to demand, optimizing route efficiency,
 *   preventing fraud) alongside asymmetric extraction (workers bear all
 *   employment risk, receive no benefits, have no due process for
 *   deactivation). The constraint has escalated over a decade as platforms
 *   have deployed increasingly sophisticated algorithms to extract margin
 *   from worker labor while maintaining the fiction that they merely provide
 *   a matching service. The extractiveness measurement shows a clear
 *   escalation pattern: from 0.35 at the constraint's origin (basic matching
 *   + rating) to 0.58 (algorithmic task allocation, real-time payment
 *   processing, deactivation threats). The theater ratio has also increased,
 *   reflecting that platforms increasingly frame algorithmic control as
 *   neutral optimization rather than employment management. This is the
 *   defining cover story: 'we don't manage workers; we optimize supply
 *   matching.' The constraint is a case study in how asymmetric information
 *   (worker cannot know why they were deactivated; cannot see the algorithm's
 *   decision rules) combines with structural dependence (no viable
 *   alternative platforms; entry barriers are low but switching costs are
 *   high) to create suppression.
 *
 * KEY AGENTS:
 *   - Contingent Workers: Primary victims (powerless/trapped) — economically dependent on platform income; subject to algorithmic deactivation without due process; no collective bargaining power; no alternative platforms with better terms
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture efficiency gains through algorithmic optimization; maintain independent contractor classification to avoid employment obligations; can relocate infrastructure or change business model if regulatory pressure increases
 *   - Labor Standards Enforcement: Secondary victim (powerless/trapped) — faces jurisdictional gaps and classification ambiguity; cannot exit; bears full cost of platform evasion
 *   - Regulators: Moderate victims (moderate/constrained) — face genuine coordination problem (legitimate gig work vs exploitative extraction) alongside political/economic pressure from platforms; have some leverage but limited enforcement tools
 *   - Worker Organizing Coalitions: Organized challengers (organized/constrained) — building countervailing power through unions and advocacy; constrained by platform retaliation and algorithmic opacity; experiencing both coordination benefits (collective voice) and extraction (algorithmic suppression)
 *   - Traditional Employment Framework: Institutional actor (institutional/constrained) — maintains legitimation through labor codes and employment law while hollowed out in platform economy; preserved through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_management_escalation, 0.58).
domain_priors:suppression_score(algorithmic_management_escalation, 0.68).
domain_priors:theater_ratio(algorithmic_management_escalation, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_management_escalation, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_management_escalation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(algorithmic_management_escalation, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_management_escalation, tangled_rope).
narrative_ontology:human_readable(algorithmic_management_escalation, "Algorithmic Management Escalation in Platform Labor").
narrative_ontology:topic_domain(algorithmic_management_escalation, "labor/platform_economics/algorithmic_governance").

domain_priors:requires_active_enforcement(algorithmic_management_escalation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_management_escalation, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_management_escalation, algorithmic_optimization_targets).
narrative_ontology:constraint_victim(algorithmic_management_escalation, contingent_workers).
narrative_ontology:constraint_victim(algorithmic_management_escalation, labor_standards_enforcement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTINGENT WORKER (SNARE) — Faces algorithmic deactivation, opaque performance metrics, and lack of due process. No meaningful exit: economic dependence, lack of alternative platforms with better terms, no collective bargaining power. Bears full extraction cost with zero alternatives.
constraint_indexing:constraint_classification(algorithmic_management_escalation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LABOR STANDARDS ENFORCEMENT (SNARE) — Cannot exit the jurisdictional gap. Algorithmic classification of workers as independent contractors is designed to circumvent labor law. The enforcement system is structurally trapped by classification games and cross-border opacity. High suppression via legal ambiguity.
constraint_indexing:constraint_classification(algorithmic_management_escalation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATOR (TANGLED ROPE) — Faces genuine coordination problem (need to enable legitimate gig work) alongside asymmetric extraction (platforms resist classification, workers bear uncompensated risk). Constrained by political pressure, technical complexity, and platform exit threats. Moderate experienced extraction — has some leverage but limited enforcement tools.
constraint_indexing:constraint_classification(algorithmic_management_escalation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Experiences the constraint as coordination of supply and demand with marginal algorithmic optimization. Net beneficiary of the extraction mechanism. Has arbitrage options (can relocate infrastructure, lobby for favorable classification, switch to higher-extraction models). Experiences constraint as enabling mechanism, not coercion.
constraint_indexing:constraint_classification(algorithmic_management_escalation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL EMPLOYMENT RELATIONSHIP (PITON) — The employment contract, employer liability framework, and labor standards are increasingly theater in the platform economy. The traditional apparatus persists through institutional inertia (labor codes, court precedent, regulatory habit) despite hollowed-out function. Classification as independent contractor bypasses the traditional system entirely. Piton classification derives from theater ratio (0.62) — the formal employment apparatus is maintained as legitimation ritual while the actual control/extraction occurs through algorithmic governance outside the traditional framework.
constraint_indexing:constraint_classification(algorithmic_management_escalation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: WORKER ORGANIZING COALITION (TANGLED ROPE) — Organized agents (unions, advocacy groups, gig worker collectives) perceive genuine coordination benefits (collective bargaining protocols, safety standards, benefit pooling) alongside asymmetric extraction (platforms resist unionization, algorithmic retaliation is common). Constrained by platform power and algorithmic opacity but building countervailing institutions. Moderate extraction — has agency and exit options but faces active suppression.
constraint_indexing:constraint_classification(algorithmic_management_escalation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational view, algorithmic management escalation is a structural snare: it separates control from employment status, externalizes labor standards, and uses opacity to suppress worker agency. The constraint's escalation dynamic (tightening algorithmic control to extract more margin while maintaining independent contractor fiction) is the defining feature. High suppression through algorithmic opacity and legal ambiguity. No built-in sunset or coordination benefit justifies the extraction magnitude.
constraint_indexing:constraint_classification(algorithmic_management_escalation, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_management_escalation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_management_escalation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_management_escalation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_management_escalation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_management_escalation, TR),
    TR >= 0.70.

:- end_tests(algorithmic_management_escalation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting significant but not maximal extraction. The platform captures efficiency gains through algorithmic optimization while externalizing employment costs. However, extraction is not at snare levels (≥0.66) because: (1) some genuine coordination value exists (matching is non-trivial), (2) workers retain some optionality (multi-homing is possible though costly), and (3) regulatory pressure is building. Suppression (0.68): High, reflecting multiple mechanisms: algorithmic opacity (workers cannot see decision rules), classification ambiguity (independent contractor status shields platforms from labor law), coordination problems (workers are atomized and cannot easily organize), and information asymmetry (platform has complete visibility; workers have none). Theater ratio (0.62): Moderate-high, reflecting that platforms maintain substantial performative framing. The 'neutral matching service' narrative is theater that masks algorithmic management. Traditional employment management (performance evaluation, discipline, compensation terms) occurs through the algorithm but is presented as market forces. The theater has increased over time as platforms have become more sophisticated in their linguistic framing of control. The escalation pattern in measurements (extractiveness rising from 0.35 to 0.58; theater rising from 0.48 to 0.62) reflects the drift from genuine coordination problem (early platform model) toward extraction mechanism with increasingly sophisticated suppression (current state).
 *
 * PERSPECTIVAL GAP:
 *   The platform operator and contingent worker occupy structurally opposite positions relative to this constraint. The operator benefits from the classification ambiguity and algorithmic opacity that enables extraction while maintaining plausible deniability. They experience the constraint as coordination — solving a genuine supply-matching problem with high-return optimization. The worker bears all the extraction cost while receiving none of the coordination benefit. They experience algorithmic control that tightens without explanation, deactivation without due process, and structural dependence without employment protections. The regulator occupies an intermediate position — they see the coordination value (legitimate gig work should exist) but also see the extraction mechanism (workers are being exploited through classification games). The worker coalition sees both simultaneously: the platform does solve a matching problem AND it extracts through opacity and suppression. The traditional employment apparatus (Piton) masks the true control relationship — formal labor codes persist but actual management occurs through the algorithm outside the formal framework. This is the defining feature of the constraint: separation of control from employment status, and separation of coordination function from extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural relationship to the constraint. Platform operators are beneficiaries with arbitrage options — they can lobby for favorable classification, relocate infrastructure, or change business models if pressure increases. This gives them low d (~0.15) and negative f(d), meaning they experience the constraint as enabling rather than extractive. Contingent workers are victims with trapped exit options — economic dependence on platform income, lack of viable alternatives, and high switching costs create structural immobility. This gives them high d (~0.92) and high f(d), meaning they experience maximal extraction. Regulators are intermediate — they perceive asymmetric extraction (workers are being exploited) but also genuine coordination problems (legitimate gig work should exist). Constrained exit options (political pressure, platform leverage, technical complexity) give them moderate-high d (~0.58-0.65). Worker coalitions are organized victims — they have higher power than individual workers but face active suppression from platforms. Constrained exit (platform retaliation, algorithmic opacity) and victim status (workers bear costs) give them moderate d (~0.65). The traditional employment framework has constrained exit options (deeply embedded in legal/regulatory systems) but is increasingly a victim of platform circumvention — it gets moderate-high d (~0.60).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing genuine coordination (matching problem) from extraction mechanism (algorithmic margin extraction). The tangled_rope classification correctly identifies that both functions coexist. The platform operator sees Rope because they benefit from the coordination and capture the extraction margin — for them, the extraction is a feature, not a bug. The contingent worker sees Snare because the extraction mechanism completely dominates their experience — the coordination benefit (knowing when/where to work) is trivial compared to the extraction cost (wages declining as algorithms tighten, arbitrary deactivation, no benefits). The regulator sees Tangled Rope because they must balance enabling the genuine coordination problem (gig work does enable flexibility and quick job matching) against preventing extraction abuse (workers should not bear all employment risk without protections or voice). The worker coalition also sees Tangled Rope but pushes toward the Rope end — they want the coordination benefits (efficient matching) redistributed and protected through unionization and due process. The constraint escalates toward higher extractiveness when the coordination function becomes saturated (the matching problem is 'solved') and the platform shifts focus entirely to margin extraction. At that point, the classification from the worker perspective approaches pure Snare. The analytics observer sees Snare because the escalation trajectory shows the constraint is drifting from mixed coordination-extraction toward pure extraction as platforms maximize algorithmic pressure without bound.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_opacity_mechanism,
    'Is suppression primarily structural (genuine technical limitations in auditing algorithms) or institutional (platforms actively prevent transparency)?',
    'Audit access mandates (EU AI Act, algorithmic impact assessments); correlation between platforms offering transparency and worker organizing success; cross-jurisdictional comparison of suppression levels',
    'If technical: suppression may decline as auditing tools mature. If institutional: suppression is active strategy requiring explicit intervention. Affects classification of constraint as temporary (scaffold) vs durable (snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_opacity_mechanism, empirical, 'Whether algorithmic opacity is technical constraint or active suppression').

omega_variable(
    independent_contractor_classification_stability,
    'Will algorithmic management escalation eventually force reclassification of platform workers as employees, or will legal/political capture preserve independent contractor status indefinitely?',
    'Legislative outcomes across jurisdictions (UK worker status, California Prop 22, EU Platform Work Directive implementation); judicial precedent tracking; correlation between worker organizing and classification outcomes',
    'If reclassification occurs: constraint transforms to tangled_rope or rope (employment protections re-enter). If classification persists: constraint becomes durable snare with escalating extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independent_contractor_classification_stability, preference, 'Stability of independent contractor classification under pressure').

omega_variable(
    collective_bargaining_feasibility,
    'Can contingent workers build countervailing power through unions or algorithmic cooperatives, or is worker atomization inherent to the platform model?',
    'Tracking of successful union organizing (Deliveroo, Amazon Flex, Instacart); emergence and sustainability of alternative platforms with democratic governance; correlation between organizing success and algorithmic suppression mechanisms deployed',
    'If feasible: worker coalition perspective (organized/constrained) becomes primary, transforming constraint to tangled_rope with real sunset. If infeasible: constraint remains snare with powerless/trapped primary perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_bargaining_feasibility, empirical, 'Whether collective bargaining can build countervailing power in platform labor').

omega_variable(
    algorithmic_escalation_ceiling,
    'Is there a extractiveness ceiling beyond which algorithmic control becomes economically counterproductive (worker flight, quality collapse, organizing threshold crossed)?',
    'Longitudinal data on worker retention vs algorithmic strictness; correlation between algorithm tightening and churn/quality metrics; threshold analysis of when organizing campaigns succeed',
    'If ceiling exists: extractiveness will stabilize before reaching snare maximum. If no ceiling: extraction can escalate indefinitely. Affects whether constraint is durable or will auto-correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(algorithmic_escalation_ceiling, empirical, 'Whether algorithmic escalation has inherent ceiling or can escalate indefinitely').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_management_escalation, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algmgt_tr_t0, algorithmic_management_escalation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(algmgt_tr_t3, algorithmic_management_escalation, theater_ratio, 3, 0.55).
narrative_ontology:measurement(algmgt_tr_t6, algorithmic_management_escalation, theater_ratio, 6, 0.6).
narrative_ontology:measurement(algmgt_tr_t9, algorithmic_management_escalation, theater_ratio, 9, 0.62).

% Extraction over time
narrative_ontology:measurement(algmgt_be_t0, algorithmic_management_escalation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(algmgt_be_t3, algorithmic_management_escalation, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(algmgt_be_t6, algorithmic_management_escalation, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(algmgt_be_t9, algorithmic_management_escalation, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_management_escalation, resource_allocation).
narrative_ontology:affects_constraint(algorithmic_management_escalation, employment_classification_ambiguity).
narrative_ontology:affects_constraint(algorithmic_management_escalation, algorithmic_opacity_in_labor_systems).
narrative_ontology:affects_constraint(algorithmic_management_escalation, benefit_externalization_in_gig_economy).

% DUAL FORMULATION NOTE:
% Algorithmic management escalation is downstream of employment classification ambiguity (independent contractor vs employee distinction) and algorithmic opacity (workers cannot audit decision rules). The upstream constraint has ε ≈ 0.30 (primarily classification framework); this constraint has ε = 0.58 (implementation through algorithmic control and escalation). Decomposition is required because the classification ambiguity exists independent of platform-specific algorithmic practices, and the opacity exists in other domains (content moderation, credit scoring, hiring algorithms). This constraint story focuses on the escalation dynamic specific to labor platforms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_management_escalation, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
