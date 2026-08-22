% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__precarity_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_precarity, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: flexible_employment_legitimacy__precarity_extraction_reading
 *   human_readable: Flexible Employment as Structural Precarity Enabling Platform Extraction
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   Platform-flexible employment is analyzed here as structural precarity
 *   that enables extraction of surplus value from workers while externalizing
 *   costs to social security systems and formalized competitors. The same
 *   institutional arrangement is read differently by other parties:
 *   market-efficiency advocates see genuine labor-market clearing;
 *   developmental-state analysts see a transitional form requiring state
 *   management toward formalization. This story instantiates ONE reading —
 *   the precarity-extraction reading — and does not resolve the contest. The
 *   referent is the standing arrangement (platform employment classification,
 *   algorithmic management, contractor status, cost externalization) as this
 *   reading assesses it. The metrics describe the standing arrangement's
 *   actual operation under this reading's empirical lens: high extraction
 *   (0.81 at interval end), rising extraction trajectory, substantial
 *   suppression via algorithmic control and exit barriers, and theater ratio
 *   showing that enforcement increasingly defends the cost-shifting rather
 *   than the coordination function.
 *
 * KEY AGENTS:
 *   - platform_operators: agenda-setter (institutional power); designs classification rules, sets compensation, controls work allocation, enforces contractor status via deactivation
 *   - flexible_workers: payer (powerless); absorb precarity, risk externalization, identity-fusion barriers to exit
 *   - consumer_beneficiaries: benefit from price reduction; carry diffuse secondary costs
 *   - formalized_labor_competitors: payer (powerful institutional); market share eroded by cost-shifting
 *   - labor_regulators: captured agenda-setters; enforcement authority subordinated to platform relocation threats
 *   - social_security_systems: payer (institutional but passive); absorb fiscal consequences of benefits gaps
 *   - labor_organizing: excluded (classification rules, algorithmic retaliation, communication control)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, 0.81).
domain_priors:suppression_score(flexible_employment_legitimacy__precarity_extraction_reading, 0.72).
domain_priors:theater_ratio(flexible_employment_legitimacy__precarity_extraction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__precarity_extraction_reading, tangled_rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__precarity_extraction_reading, "Flexible Employment as Structural Precarity Enabling Platform Extraction").
narrative_ontology:topic_domain(flexible_employment_legitimacy__precarity_extraction_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__precarity_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__precarity_extraction_reading, 'e3c6d1ab-aec8-48a0-8d6f-13b2dc24ae81').
narrative_ontology:cs_kernel_codification('e3c6d1ab-aec8-48a0-8d6f-13b2dc24ae81', distributed).
narrative_ontology:cs_authority_grounding('e3c6d1ab-aec8-48a0-8d6f-13b2dc24ae81', extraction).
narrative_ontology:cs_reading_relation('e3c6d1ab-aec8-48a0-8d6f-13b2dc24ae81', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3c6d1ab-aec8-48a0-8d6f-13b2dc24ae81', flexible_employment_legitimacy__developmental_state_reading, influences).
narrative_ontology:cs_axiom('e3c6d1ab-aec8-48a0-8d6f-13b2dc24ae81', foundational, cost_externalization_is_extraction).
narrative_ontology:cs_axiom_status(cost_externalization_is_extraction, holdable).
narrative_ontology:cs_axiom_grounding('e3c6d1ab-aec8-48a0-8d6f-13b2dc24ae81', cost_externalization_is_extraction, empirically_contingent).
narrative_ontology:cs_axiom('e3c6d1ab-aec8-48a0-8d6f-13b2dc24ae81', foundational, precarity_is_enforced_not_chosen).
narrative_ontology:cs_axiom_status(precarity_is_enforced_not_chosen, holdable).
narrative_ontology:cs_axiom_grounding('e3c6d1ab-aec8-48a0-8d6f-13b2dc24ae81', precarity_is_enforced_not_chosen, empirically_contingent).
narrative_ontology:cs_axiom('e3c6d1ab-aec8-48a0-8d6f-13b2dc24ae81', secondary, regulatory_capture_sustains_classification).
narrative_ontology:cs_axiom_status(regulatory_capture_sustains_classification, holdable).
narrative_ontology:cs_axiom_grounding('e3c6d1ab-aec8-48a0-8d6f-13b2dc24ae81', regulatory_capture_sustains_classification, empirically_contingent).
narrative_ontology:cs_reference_frame('e3c6d1ab-aec8-48a0-8d6f-13b2dc24ae81', cost_internalization_labor_standard).
narrative_ontology:cs_drift_state('e3c6d1ab-aec8-48a0-8d6f-13b2dc24ae81', contemporary_platform_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e3c6d1ab-aec8-48a0-8d6f-13b2dc24ae81', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, consumer_beneficiaries).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, flexible_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, formalized_labor_competitors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, consumer_beneficiaries).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, social_security_systems).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, algorithmic_management_legitimacy).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, gig_economy_inevitability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce labor classification rules that keep workers as independent contractors rather than employees. Set work allocation via algorithmic assignment, deactivate workers at will, take a commission on every transaction, and avoid employer obligations (benefits, payroll taxes, unemployment insurance, minimum hours). Justify the model as enabling worker autonomy and market flexibility; operationally, use algorithmic opacity and speed of retaliation (deactivation) to manage labor discipline.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Accept reduced earnings per-unit work compared to formalized employment, no paid leave, no benefits, no schedule guarantee, and algorithm-driven work assignment they cannot contest. Market themselves as independent (mandatory self-branding for app access), absorb all risk (vehicle wear, accident liability, platform payment risk). Exit appears available but is structurally barred by identity fusion: livelihood, daily routine, social role, and self-concept are constituted through the platform relationship. Career mobility into formalized employment is blocked by classification stigma and gaps in portable credentials.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, flexible_workers, payer,
    powerless, biographical, identity_locked, global).

% Receive lower prices and faster service delivery (rides, deliveries, task execution) because the platform has externalized labor costs to workers and avoided employer obligations. Also bear diffuse risk: lower quality service, worker safety concerns, platform liability exposure, and reduced worker income creates secondary demand shocks. Switching costs are low; loyalty is price-driven.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, consumer_beneficiaries, beneficiary,
    organized, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__precarity_extraction_reading, consumer_beneficiaries, payer).

% Formalized taxi drivers, delivery companies, and logistics operators face revenue collapse as platforms undercut their pricing by externalizing labor costs. They cannot compete on price while maintaining employee protections and employer obligations. Their market share erodes; some exit the market. Regulatory remedies are available but face institutional resistance from capturing legislators and captured regulators.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, formalized_labor_competitors, payer,
    powerful, generational, constrained, national).

% Tasked with enforcing labor standards and classification rules. Captured by platform industry: threatened with relocation, job losses, and reduced tax revenue from platforms that will leave if classification changes. Enforce labor codes selectively, grant regulatory exemptions for 'innovation,' and redefine independent contractor thresholds to accommodate platform business models. Their enforcement authority is real but their incentive structure rewards non-enforcement.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_regulators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__precarity_extraction_reading, labor_regulators, observer).

% Attempts to organize platform workers and secure collective bargaining. Structurally blocked by classification rules (independent contractors cannot legally unionize in many jurisdictions), algorithmic retaliation (organizers are deactivated), platform-controlled communication infrastructure (no access to worker-to-worker organizing channels), and worker atomization (no workplace, no coworkers, identity-locked exit barriers prevent strike participation). Win conditions exist but organizing infrastructure is systematically dismantled.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_organizing, excluded,
    moderate, generational, constrained, national).

% Absorb the cost-shifting: unemployment insurance funds are depleted by excluded workers; disability and injury claims rise from workers without employer coverage; pension gaps create public elder-care obligations. Governments must subsidize or see social-safety-net collapse, transferring wealth from the taxpaying formalized workforce to platform shareholders. The constraint's enforcement depends partly on regulatory inaction — social security systems have no power to alter classification but bear its fiscal consequences.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, social_security_systems, payer,
    institutional, generational, trapped, national).

% Measure the constraint's operation across jurisdictions: track earnings trajectories, benefits coverage gaps, work-hour variance, algorithmic management intensity, regulatory capture patterns, and worker health outcomes. Compare against formalized labor and developmental-state readings of the same kernel.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__precarity_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches labor supply to on-demand work through algorithmic assignment and rating systems, reducing search frictions and enabling service delivery without upfront hiring commitments. Provides workers rapid access to income opportunities and platforms rapid access to labor without formal onboarding.
% TRANSFER_FUNCTION: Transfers employer obligations (benefits, payroll taxes, income security, schedule guarantees) from the platform to workers and taxpayers; transfers wage premiums (captured when the same work is formalized with benefits) to platform shareholders and consumers; transfers work-hour variance risk from platforms to workers. The direction is: workers pay via precarity, social security systems pay via gaps, formalized competitors pay via market-share loss, consumers gain via price reduction.
% ABSENT_VOICES: Formalized workers who have exited the labor market due to platform competition have no seat. Future workers who would have entered formalized employment but are now platform-classified are structurally excluded from the conversation. Social security actuaries who understand the long-term fiscal impact are present in regulatory hearings but subordinated to platform and political voices. Labor organizers are excluded by platform-controlled communication and classification rules.
% DISAPPEARANCE_RATIONALE: If platform-flexible employment classification and its enforcement vanished, workers would shift to formalized employment (where available) or unemployment (where unavailable); service prices would rise to reflect true labor costs; social security systems would see claims decline and tax bases stabilize; formalized competitors would recover market share. The entire architecture of platform profitability rests on this cost-shifting structure.
% FOUNDING_PROBLEM: Traditional labor markets were slow to match workers to on-demand opportunities; entry barriers (hiring formality, W-2 onboarding, geographic concentration) kept labor underutilized and service provision expensive and geographically limited.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and venture-capital investors attest the founding problem is live and solved by their model. Labor economists and social policy researchers document that the founding problem was partially real but has been substantially overcome in many markets (geographic friction has fallen due to digital coordination; matching technology exists and could operate under formalized employment with benefits; the persistence of flexible classification is now driven by profitability maximization, not problem-solving). Regulatory bodies in some jurisdictions (EU, California, UK) acknowledge the founding problem is displaced and have moved to reclassify.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__precarity_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__precarity_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__precarity_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) and rising because: (1) platform commission rates capture wage premiums once paid under formalized employment; (2) worker earnings per unit work are below formalized equivalents, even accounting for on-demand flexibility; (3) workers absorb all variance risk (no guaranteed hours, income volatility, work absence during illness/injury) for which formalized workers receive risk premiums or benefits. The measured extraction is the surplus above what competitive labor markets would pay if costs were internalized. Suppression is substantial (0.72) and rising because: (1) algorithmic management is opaque, speed of disciplinary action (deactivation) is uncontestable, (2) classification rules legally prevent unionization in many jurisdictions, (3) platform-controlled infrastructure prevents worker-to-worker organizing, (4) identity-lock barriers (livelihood fused with platform relationship, self-concept as independent contractor, daily dependence on algorithm) make exit appear available but structurally barred. Theater ratio (0.48) is moderate and rising because: the stated coordination function (match labor to demand) is real and captures ~50% of enforcement overhead; the rising half reflects growing algorithmic management intensity deployed to enforce cost-shifting rather than improve matching (e.g., acceptance rates, response-time requirements, algorithmic de-prioritization of workers who reject low-wage assignments). The measurement series runs on one shared time grid (all metrics authored at each point 0, 5, 10, 15, 20, 25) so temporal drift is observable.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is extreme: the agenda-setter (operators) perceive coordination; the payers (workers, formalized competitors, social security) perceive extraction. The engine computes different types per seat: from the operator seat, the arrangement may compute as tangled_rope (coordination with asymmetric collection); from the worker seat, it computes as snare (pure extraction with legal cover). This is exactly how the framework detects false narratives: the claim (rope-like coordination) diverges from the measured structure (extraction-heavy with suppression), and the per-seat computation reveals that the divergence is systematic, not a measurement error.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators: d ≈ 0.1 (full beneficiary). They set the rules, take commissions, avoid employer obligations, have exit options (operate in other jurisdictions, scale to other labor markets). Flexible workers: d ≈ 0.92 (near-full target). They pay precarity (wage differential, no benefits, income variance), face maximum suppression (algorithmic control, deactivation, identity-lock), have no exit (classification rules plus identity fusion). Consumer beneficiaries: d ≈ 0.4 (symmetric toward slight target). They benefit from lower prices but carry diffuse secondary costs (reduced service quality, platform liability, worker safety concerns, demand shock from reduced worker income). Formalized competitors: d ≈ 0.75 (near-target). They lose market share, face a cost structure that makes competition impossible while maintaining formalized employment. Labor regulators: d ≈ 0.15 (near-beneficiary). They are captured by platform relocation and tax-revenue threats; their autonomy is subordinated to platform pressure. Social security: d ≈ 0.85 (near-target). They absorb fiscal consequences (benefits gaps, unemployment claims) and have no mechanism to recover the costs. Directionality overrides are used for regulators (derived d-moderate from beneficiary pressure would underestimate capture) and for labor organizers if they were a stakeholder (their power is moderate but exit is trapped by classification law).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (labor-matching frictions, entry barriers, slow onboarding) was partially real and partially solved. The constraint's classification as tangled_rope (genuine coordination + asymmetric extraction) passes the mandatrophy test: there IS a real coordination function (algorithmic matching, reduced search friction), but the arrangement persists partly because of that function and partly because extraction sustains it. The constraint would not be viable without the coordination function (workers would not stay if the arrangement were pure suppression and no income); it is also not viable without the extraction (operators would not maintain the infrastructure if commissions did not generate returns above cost). However, the founding problem is DYING or DEAD: regulatory capture is the primary enforcement mechanism now, not the remaining coordination surplus. As regulatory reclassification spreads (EU platform work directive, California gig work law, UK employment status rulings), the founding problem's legitimacy erodes and the constraint appears increasingly as pure extraction defended by legal/regulatory barriers rather than genuine coordination. Temporal measurements show extractiveness rising and theater ratio rising (enforcement shifting toward pure cost-defense rather than matching improvement), consistent with mandatrophy drift: the founding problem is dead, but the arrangement persists through regulatory capture, and the visible enforcement is increasingly theatrical (defending cost-shifting rather than improving service).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_structural_vs_internalized,
    'Is the measured suppression (0.72) primarily structural (legal barriers to unionization, algorithmic retaliation infrastructure, platform-controlled communication) or internalized (workers believe they deserve the treatment, have fused identity with the platform relationship, reject alternatives pre-consciously)?',
    'Post-exit trajectory analysis: workers who exit the platform entirely and move to formalized employment or other platforms. If suppression persists after platform exit (workers report low earnings expectations, internalized self-blame, difficulty readjusting to formalized norms), reclassify as partially internalized. If suppression dissolves (workers recover baseline work expectations, improve earnings, readjust easily), reclassify as primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the worker carries the suppression forward and it propagates into future labor-market choices. If structural, the suppression dissipates at exit and is localized to the platform relationship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_structural_vs_internalized, empirical, 'Suppression mechanism: structural barriers vs. internalized beliefs in platform-worker relationships').

omega_variable(
    coordination_extraction_separability,
    'Is the algorithmic matching function (genuine coordination) structurally inseparable from contractor classification and cost externalization (extraction), or could the matching operate under formalized employment with benefits?',
    'Natural experiment from jurisdictions that mandate employment classification (e.g., France, Spain post-2021): if platforms maintain competitive matching services while absorbing employer costs, the functions are separable. If platforms exit or degrade service, the functions are inseparable under formalized employment.',
    'If separable, the extraction is pure rent-seeking and policy remedies (mandate benefits without changing matching) are viable. If inseparable, the extraction is structurally coupled to the coordination function, and policy must choose between losing the matching or accepting the extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether platform matching and cost externalization are structurally separable').

omega_variable(
    regulatory_capture_vs_coordination_surplus,
    'Is the constraint persisting due to residual coordination surplus (workers still gain from matching speed, platforms still gain from reduced hiring friction) or primarily due to regulatory capture (legal barriers, captured regulators, threatened relocation)?',
    'Temporal analysis of regulatory reclassification events: when jurisdictions mandate employment classification without changing the matching algorithm or customer base, does platform profitability collapse or stabilize? Stabilization indicates capture is primary; collapse indicates coordination surplus is primary.',
    'If capture is primary, the constraint is approaching mandatrophy (founding problem dead, arrangement persisting through non-coordination mechanisms). If coordination surplus is primary, the constraint is stable tangled_rope despite regulatory pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_vs_coordination_surplus, empirical, 'Whether persistence derives from coordination surplus or regulatory capture').

omega_variable(
    kernel_reading_contest_status,
    'Which reading of the flexible_employment_legitimacy kernel will stabilize in policy and regulatory equilibrium: precarity_extraction_reading, market_efficiency_reading, or developmental_state_reading?',
    'Track regulatory adoption across jurisdictions (2024–2030): which reading''s policy recommendations (mandatory reclassification, sectoral exemptions, transitional formalization pathway) become law and are sustained through subsequent electoral/judicial cycles?',
    'If precarity_extraction_reading stabilizes (mandatory employment classification spreads), this constraint transitions toward death (or shifts to a snare_locked variant where extraction persists despite legalization). If market_efficiency_reading stabilizes (classification remains flexible, regulatory exemptions expand), this constraint remains tangled_rope and rises in dominance. If developmental_state_reading stabilizes (transitional pathway to formalization with state support), this constraint decays and a new constraint (formalization_with_platform_structure) replaces it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_status, conceptual, 'Long-term policy and regulatory settlement of the flexible_employment_legitimacy kernel contest').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__precarity_extraction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(flex_tr_t0, observed).
narrative_ontology:measurement(flex_tr_t5, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 5, 0.37).
narrative_ontology:measurement_basis(flex_tr_t5, observed).
narrative_ontology:measurement(flex_tr_t10, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(flex_tr_t10, observed).
narrative_ontology:measurement(flex_tr_t15, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement_basis(flex_tr_t15, observed).
narrative_ontology:measurement(flex_tr_t20, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement_basis(flex_tr_t20, observed).
narrative_ontology:measurement(flex_tr_t25, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(flex_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(flex_be_t0, observed).
narrative_ontology:measurement(flex_be_t5, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement_basis(flex_be_t5, observed).
narrative_ontology:measurement(flex_be_t10, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement_basis(flex_be_t10, observed).
narrative_ontology:measurement(flex_be_t15, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement_basis(flex_be_t15, observed).
narrative_ontology:measurement(flex_be_t20, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement_basis(flex_be_t20, observed).
narrative_ontology:measurement(flex_be_t25, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement_basis(flex_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(flex_su_t0, observed).
narrative_ontology:measurement(flex_su_t5, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(flex_su_t5, observed).
narrative_ontology:measurement(flex_su_t10, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(flex_su_t10, observed).
narrative_ontology:measurement(flex_su_t15, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(flex_su_t15, observed).
narrative_ontology:measurement(flex_su_t20, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(flex_su_t20, observed).
narrative_ontology:measurement(flex_su_t25, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(flex_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__precarity_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__precarity_extraction_reading, 0.22).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, formalized_employment_labor_standards).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, social_security_funding_adequacy).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, algorithmic_management_labor_discipline).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, regulatory_capture_platform_industry).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the flexible_employment_legitimacy kernel, alongside market_efficiency_reading and developmental_state_reading. The three readings share a referent (the standing arrangement of platform-flexible employment) but differ in how they assess it: precarity_extraction_reading emphasizes cost-shifting and worker precarity; market_efficiency_reading emphasizes labor-market clearing; developmental_state_reading emphasizes transitional instability requiring state intervention. Each reading instantiates a separate constraint story with its own ε, beneficiaries/victims, temporal trajectory, and policy implications. They are linked via network.affects_constraints and cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(flexible_employment_legitimacy__precarity_extraction_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
