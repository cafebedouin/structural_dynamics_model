% ============================================================================
% CONSTRAINT STORY: workplace_surveillance_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_workplace_surveillance_extraction, []).

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
 *   constraint_id: workplace_surveillance_extraction
 *   human_readable: Workplace Surveillance as Extraction and Control Mechanism
 *   domain: labor/organizational_power/technology
 *
 * SUMMARY:
 *   Workplace surveillance has transformed from task-based monitoring
 *   (measuring deliverables, attendance, task completion) into comprehensive
 *   behavioral surveillance that extends far beyond work coordination.
 *   Keystroke logging, email content scanning, video monitoring, application
 *   tracking, location monitoring, mood detection via facial analysis, and
 *   predictive behavioral profiling have become routine infrastructure in
 *   many organizations. This constraint exhibits the full spectrum of DR
 *   classification types depending on structural position because different
 *   worker populations experience genuinely different structural
 *   relationships to the same surveillance apparatus. A powerless worker
 *   without alternative employment experiences the constraint as a snare
 *   (pure extraction with no exit). A skilled worker with market options
 *   experiences it as tangled rope (mixed coordination and extraction with
 *   constrained exit). A professional whose identity is fused with their
 *   career experiences identity-locked suppression (structurally mobile but
 *   functionally trapped through identity). Management experiences pure
 *   coordination (solving the principal-agent problem). Surveillance vendors
 *   experience market coordination (customer demand met by product supply).
 *   Labor organizing structures experience their own degradation (piton —
 *   formal structures persist but functional capacity to negotiate
 *   surveillance limits has declined). Emerging tech worker coalitions
 *   experience leverage points (organized agents with genuine mobility
 *   options). The analytical observer risks naturalizing the constraint as an
 *   immutable feature of scaled work coordination, but the structural data
 *   reveals it as a contested arrangement with identifiable beneficiaries and
 *   victims.
 *
 * KEY AGENTS:
 *   - Powerless workers (trapped exit): Primary victims bearing maximum extraction. No alternative employment options, employment contract enforced monitoring, complete behavioral transparency.
 *   - Skilled workers (constrained exit): Secondary victims with high exit costs. Marketable skills provide alternatives but relocation, credential transfer, regional dependence create friction.
 *   - Professionals with fused identity (identity-locked exit): Tertiary victims constituted through career identity. Can exit specific workplaces but cannot exit professional role without identity dissolution. Off-hours monitoring extends constraint into personal life.
 *   - Management and organizational ownership (institutional/arbitrage): Primary beneficiary. Surveillance solves principal-agent problems, enables predictive management, preempts organizing efforts, justifies wage suppression through algorithmic 'merit' systems.
 *   - Surveillance technology vendors (institutional/arbitrage): Secondary beneficiary. Create revenue streams through comprehensive monitoring platforms, benefit from labor market asymmetries, create vendor lock-in that persists constraints.
 *   - Labor organizing structures (organized/constrained): Formal counter-structure with degraded function. Union density declined; surveillance enables preemption of organizing; labor law enforcement weak. Theater ratio high because formal negotiating structures persist but lack power.
 *   - Tech worker organizing movements (organized/mobile): Emerging coalition with leverage points. Cross-company employment mobility, geographic flexibility, technical expertise, media attention. Can partially escape constraint and build countervailing power.
 *   - Analytical observer (analytical/analytical): Civilizational perspective that risks false summit classification, naturalizing extraction as inherent to work coordination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(workplace_surveillance_extraction, 0.58).
domain_priors:suppression_score(workplace_surveillance_extraction, 0.68).
domain_priors:theater_ratio(workplace_surveillance_extraction, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(workplace_surveillance_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(workplace_surveillance_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(workplace_surveillance_extraction, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(workplace_surveillance_extraction, tangled_rope).
narrative_ontology:human_readable(workplace_surveillance_extraction, "Workplace Surveillance as Extraction and Control Mechanism").
narrative_ontology:topic_domain(workplace_surveillance_extraction, "labor/organizational_power/technology").

domain_priors:requires_active_enforcement(workplace_surveillance_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(workplace_surveillance_extraction, management_ownership).
narrative_ontology:constraint_beneficiary(workplace_surveillance_extraction, surveillance_technology_vendors).
narrative_ontology:constraint_victim(workplace_surveillance_extraction, workers_labor_autonomy).
narrative_ontology:constraint_victim(workplace_surveillance_extraction, workers_informational_privacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MONITORED WORKER (SNARE) — A worker without specialized skills, in a tight labor market, cannot exit the surveillance constraint without losing employment and income. They experience comprehensive behavioral monitoring with minimal coordination benefit — the surveillance does not solve a legitimate workplace coordination problem; it extracts behavioral compliance and attention. No agency, no alternatives, full suppression. This is the maximum extraction experience.
constraint_indexing:constraint_classification(workplace_surveillance_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SKILLED WORKER (CONSTRAINED) — A worker with marketable skills faces high but surmountable costs to exit: relocation, career transition, credential transfer delays. Experiences the surveillance as mixed: some coordination function (real-time task tracking enables distributed work) alongside significant extraction (mood detection, application monitoring, keystroke logging extend far beyond task coordination). The constraint benefits the employer through both coordination efficiency AND behavioral control; workers bear disproportionate costs.
constraint_indexing:constraint_classification(workplace_surveillance_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROFESSIONAL WITH FUSED IDENTITY (TANGLED ROPE / IDENTITY-LOCKED) — A professional whose identity is constituted through their career role (physician, software engineer, consultant) experiences surveillance as structurally mobile (could exit the specific workplace) but functionally trapped through identity fusion. The career IS the self-concept. Surveillance extends into off-hours (email access, mobile device tracking, social media monitoring). Exit would require abandoning the professional identity itself. Moderate experienced extraction because the agent is organizationally complicit in the constraint's maintenance — they internalize the monitoring as legitimate through professional identity.
constraint_indexing:constraint_classification(workplace_surveillance_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 4: MANAGEMENT AND OWNERSHIP (ROPE) — Experience the constraint as pure coordination with side benefits. Task tracking enables distributed team management; real-time compliance monitoring reduces shirking; behavioral profiling enables predictive performance management and preemptive risk mitigation. From the beneficiary perspective, surveillance solves the principal-agent problem. The constraint appears legitimate — a coordination mechanism, not extraction. No experienced extraction because they are extracting, not bearing the cost.
constraint_indexing:constraint_classification(workplace_surveillance_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SURVEILLANCE TECHNOLOGY VENDORS (ROPE) — Software and hardware vendors benefit from the constraint as a coordination mechanism that solves their customers' (management) perceived need for visibility and control. They experience surveillance as pure market coordination: customers demand visibility tools; vendors supply them. From the vendor perspective, surveillance is efficient technology transfer. The constraint generates revenue streams and market growth. No experienced extraction from the vendor perspective because they are the net beneficiary.
constraint_indexing:constraint_classification(workplace_surveillance_extraction, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LABOR ORGANIZING STRUCTURES (PITON) — Unions and worker advocacy organizations represent a formal counter-structure to surveillance extraction, but their functional capacity has degraded. Union density has declined; surveillance technologies have enabled employer detection and preemption of organizing efforts; labor law enforcement is weak. The formal structure persists (unions still exist, workers can still organize), but the primary function (collectively negotiating surveillance limits) is attenuated. The constraint persists through institutional inertia and formalized contracts, not because workers prefer it. Theater ratio is high because organizing rituals (grievance procedures, contract negotiation, safety committees) proceed but have limited power to actually constrain surveillance implementation.
constraint_indexing:constraint_classification(workplace_surveillance_extraction, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: TECH WORKER ORGANIZING MOVEMENTS (TANGLED ROPE / ORGANIZED) — Emerging worker coalitions (Amazon warehouse organizing, Google walkout movements, tech worker ethics groups) experience surveillance as both a coordination problem and an extraction mechanism they can partially escape. These organized groups have mobile exit options (cross-company employment, freelance work, geographic relocation) and are using that mobility to build leverage. They see the constraint as tangled: some of it (productivity tracking) is legitimate coordination; much of it (mood detection, location tracking, algorithmic management) is pure extraction. Their organizing creates genuine friction against surveillance expansion.
constraint_indexing:constraint_classification(workplace_surveillance_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / CIVILIZATIONAL VIEW (MOUNTAIN / FALSE-SUMMIT RISK) — From a civilizational perspective, visibility and accountability appear inherent to any organized work: someone must track tasks, measure productivity, ensure compliance with safety standards. The constraint appears as an immutable feature of scaled labor organization — a natural law of working in groups. However, this perspective naturalizes what is actually a contingent institutional arrangement. The specific forms of surveillance (keystroke logging, mood detection, algorithmic management) are NOT inherent to accountability; they represent a particular technological-economic choice driven by beneficiary interests. The analytical observer risks performing false natural law status on an extractive constraint.
constraint_indexing:constraint_classification(workplace_surveillance_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(workplace_surveillance_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(workplace_surveillance_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(workplace_surveillance_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(workplace_surveillance_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(workplace_surveillance_extraction, TR),
    TR >= 0.70.

:- end_tests(workplace_surveillance_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The surveillance apparatus extracts behavioral compliance, attention, off-hours monitoring, algorithmic management control, and informational privacy on a continuing basis from workers while providing minimal reciprocal transparency or benefit. However, the extractiveness is not maximal (0.72+) because some legitimate coordination functions exist: task tracking enables distributed team management, deadline monitoring prevents coordination failures, compliance tracking addresses genuine safety and regulatory requirements. The extraction is enabled BY coordination functions, not pure extraction. The measurement trajectory shows extraction increasing from 0.32 to 0.58 over the interval as surveillance technologies expanded from task-based monitoring toward comprehensive behavioral surveillance. This reflects the constraint's evolution from coordination-dominant to extraction-dominant. Suppression (0.68): High. Barriers to exit are severe: job loss threat (strongest suppressor), alternative employment scarcity in tight labor markets, skills transfer costs, relocation friction, professional licensing requirements, identity lock mechanisms for some workers. Suppression is both structural (job market conditions, employment law, contract terms) and internalized (panopticon effect, surveillance normalization, belief in monitoring legitimacy). The trajectory shows suppression increasing from 0.42 to 0.68 as surveillance becomes normalized and workers internalize acceptance of monitoring. Theater ratio (0.64): Moderate-high and increasing. Early surveillance systems had higher functional value relative to performative content (task-based monitoring actually enabled distributed work). Contemporary comprehensive surveillance has significant performative content: mood detection for behavioral prediction, off-hours monitoring for loyalty signal, algorithmic management that obscures rather than clarifies decision-making. The theater ratio increase reflects the shift from coordination-based to control-based surveillance justification. Claimed type (Tangled Rope): Correct classification from the aggregate perspective. The constraint has genuine coordination functions (task tracking, compliance monitoring) alongside significant extraction (behavioral control, privacy invasion, algorithmic management). Active enforcement is required (IT infrastructure maintains surveillance, contracts enforce monitoring consent, termination threatens enforce compliance).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The trapped worker (powerless/trapped) experiences Snare because they have no exit options; the extraction feels total and inescapable. The constrained skilled worker experiences Tangled Rope because they perceive both coordination functions and extraction, with real but high-cost exit options. The identity-locked professional experiences Tangled Rope at biographical horizon (perceives constraint as changeable in principle) but would experience Mountain at immediate horizon (perceives it as unchangeable within their identity frame). Management and vendors experience Rope because they perceive pure coordination with beneficial side effects. Labor organizing structures experience Piton because their formal function (negotiating surveillance limits) has degraded while the structures persist. Tech worker coalitions experience Tangled Rope with actual leverage because their organizing creates friction. The analytical observer risks Mountain because civilizational-scale analysis can naturalize extraction as inherent to work. This perspectival gap reveals the structural reality: the constraint is not a natural law but a contested arrangement that different agents experience radically differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) value is derived from the agent's structural position: power level, exit options, and relationship to the extraction flow. Trapped workers with no alternatives: d ≈ 0.95 (full target of extraction) → f(d) ≈ 1.42 → high experienced χ regardless of base ε. Skilled constrained workers: d ≈ 0.75 (primary target but with escape routes) → f(d) ≈ 1.00 → moderate experienced χ. Identity-locked professionals: d ≈ 0.85 (target but partially complicit through identity fusion) → f(d) ≈ 1.15 → elevated experienced χ. Management beneficiaries: d ≈ 0.15 (beneficiary with arbitrage exit) → f(d) ≈ -0.01 → negative/near-zero experienced χ (they benefit, not suffer). Vendor beneficiaries: d ≈ 0.05 (beneficiary with high arbitrage mobility) → f(d) ≈ -0.12 → institutional/extractive positioning. Organized workers with mobile exit: d ≈ 0.55 (partial target with genuine leverage) → f(d) ≈ 0.75 → moderate χ but with agency and leverage points.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by revealing that 'coordination' and 'extraction' are inseparable in the specific case of workplace surveillance. A pure coordination story (monitoring enables distributed team management) describes one part of the constraint. A pure extraction story (behavioral control and privacy invasion) describes another. The tangled rope classification acknowledges both: the constraint solves genuine problems (principal-agent coordination) while simultaneously solving extractive goals (behavioral compliance, loyalty signaling, wage suppression justification). The key insight from mandatrophy resolution: the coordination functions could be achieved with minimal surveillance (task-based monitoring alone). The comprehensive surveillance is not required for coordination; it is required for extraction. This means: (a) the constraint is author-intentionally tangled rope, not genuinely rope-with-side-effects, and (b) the extraction is deliberate and measurable, not an accidental byproduct of coordination functions. The perspectival divergence confirms this: beneficiaries experience it as coordination because that legitimizes their extraction goals; victims experience it as extraction because that is their structural reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_versus_control_boundary,
    'Which surveillance mechanisms are necessary for genuine work coordination (task scheduling, deadline tracking, safety compliance) versus which are extractive control mechanisms (keystroke logging, mood detection, off-hours monitoring)?',
    'Comparative analysis: measure productivity improvements from minimal surveillance (task-based tracking only) versus comprehensive surveillance. Identify mechanisms that improve coordination without additional control. Test organizational performance across surveillance intensity levels.',
    'If extractive mechanisms are separable from coordination: ε drops significantly (maybe 0.25-0.30 for task-based monitoring alone), classification shifts to Rope for all but powerless workers. If extractive and coordination mechanisms are inseparable: ε stays high (0.50+), tangled rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_versus_control_boundary, empirical, 'Boundary between coordination-necessary and control-extractive surveillance').

omega_variable(
    suppression_internalization_rate,
    'How much of measured suppression is external (job loss threat, contract enforced monitoring) versus internalized (workers accepting surveillance as legitimate, panopticon internalization)?',
    'Post-exit analysis: do workers retain surveillance-acceptance behaviors after leaving monitoring contexts? Measure psychological costs of reintegration into low-surveillance environments. Survey worker beliefs about surveillance legitimacy stratified by tenure, skill level, and market conditions.',
    'If primarily external suppression: exit becomes more viable as alternative jobs appear, classification shifts toward constrained (not trapped). If primarily internalized: suppression persists even with exit options, identity_locked mechanisms dominate, constraint becomes harder to dislodge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_rate, empirical, 'Proportion of suppression that is structural versus internalized').

omega_variable(
    algorithmic_management_opacity,
    'Can workers and unions ever achieve sufficient transparency into algorithmic surveillance and decision-making systems to negotiate meaningful limits, or is the technical complexity itself a permanent asymmetry?',
    'Case studies of algorithmic transparency efforts (CCPA, GDPR right-to-explanation, union audits). Track whether transparency efforts have reduced extractive surveillance or merely formalized the asymmetry. Assess whether algorithmic auditability is achievable as opposed to theoretically possible.',
    'If transparency is achievable: organized workers can develop countervailing power, organized perspective''s mobile exit becomes functional leverage, potential for constraint evolution toward Rope or Scaffold. If opacity is permanent: algorithmic management becomes an asymmetry floor, snare classification becomes stable for powerless workers regardless of organizing efforts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(algorithmic_management_opacity, empirical, 'Whether algorithmic surveillance opacity is technically remediable').

omega_variable(
    labor_market_slack_feedback,
    'Does surveillance extraction increase during periods of labor market slack (high unemployment, weak worker power) and decrease during tight labor markets (low unemployment, worker mobility)? Or is surveillance ratcheted up during slack periods but not relaxed when markets tighten?',
    'Historical data on surveillance expansion by labor market conditions. Analyze whether worker movements (organizing wins, mass quitting, unionization) actually reduce surveillance intensity or merely slow expansion. Track wage premiums for low-surveillance work environments.',
    'If surveillance is responsive to labor market conditions: it is a true Tangled Rope that can be negotiated. If surveillance ratchets up but doesn''t down: it is functionally a Snare for trapped workers and may evolve toward the Piton classification as the constraint persists despite changed labor market conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_slack_feedback, empirical, 'Whether surveillance extraction responds to labor market tightness').

omega_variable(
    identity_lock_profession_specificity,
    'Does surveillance-induced identity lock affect professionals differently than other workers? Are physicians, lawyers, engineers experiencing identity_locked suppression (identity constituted through profession) while warehouse workers experience trapped suppression (economic dependency)?',
    'Comparative survey and interview data: measure career identity fusion by profession. Measure psychological costs of exiting profession-specific surveillance versus non-professional work exits. Analyze whether professional licensing and credentialing requirements create identity lock independent of surveillance.',
    'If professional identity lock is distinct from trapped suppression: perspectives should differentiate by professional status. Piton classification becomes more relevant for degraded professional structures (universities, hospitals, law firms) where surveillance undermines professional autonomy. If identity lock is uniform across worker types: suppress this decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_profession_specificity, empirical, 'Whether surveillance-induced identity lock is profession-specific').

omega_variable(
    false_summit_naturalization,
    'Is the analytical observer''s classification of workplace surveillance as a mountain (natural law of organized work) a genuine insight into inherent coordination requirements, or is it a false summit that naturalizes a contingent extractive arrangement?',
    'Historical comparison: identify organizations and periods with minimal surveillance that achieved comparable coordination and productivity. Analyze whether societies with stronger labor protections (EU, Nordic countries) have lower surveillance intensity with maintained productivity. Determine whether surveillance growth correlates with productivity gains or merely with technological capability and capital''s declining bargaining position.',
    'If surveillance is contingent: analytical perspective is false summit. The constraint''s classification from beneficiary and trapped perspectives is the structural reality. If surveillance is truly inherent: mountain classification holds, but FSM detector should reclassify when beneficiaries are identified (triggering the signature override chain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization, empirical, 'Whether surveillance is inherent to work coordination or naturalized extraction').

omega_variable(
    vendor_capture_lock_in,
    'Have surveillance technology vendors created lock-in effects that prevent organizations from reducing surveillance even if they wanted to? Do organizational IT systems depend on continuous data extraction and monitoring architecture in ways that make low-surveillance operation technically infeasible?',
    'Technical audits of surveillance stacks in common platforms (Microsoft Teams, Slack, Salesforce, Amazon Chime). Identify whether low-surveillance modes exist and are accessible without significant technical rework. Analyze whether competing products offer genuinely reduced-surveillance alternatives or whether all platforms converge on comprehensive monitoring.',
    'If lock-in is strong: vendors transition from beneficiary to system maintainer role. Constraint becomes harder to dislodge even if workers and management both wanted reduction (path dependency, switching costs). Classification stability increases; constraint persists through technical rather than purely extractive architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_capture_lock_in, empirical, 'Whether vendor lock-in prevents surveillance reduction even if desired').

omega_variable(
    cross_industry_surveillance_contagion,
    'Does surveillance spread from high-extraction industries (logistics, retail, warehouse work) to moderate-extraction industries (office work, professional services) through competitive labor market dynamics and vendor sales pressure? Is there a contagion mechanism that increases industry-wide surveillance intensity?',
    'Historical data on surveillance adoption rates by industry and time period. Identify whether surveillance is driven by productivity gains specific to each industry or by technology vendors and competitive pressure to match competitors'' monitoring intensity. Track whether industries that resisted surveillance maintain competitive viability.',
    'If contagion is strong: suppression levels will increase over time even in industries without direct surveillance benefits. Theater ratio will rise as surveillance persists beyond its coordination function (piton mechanism). Constraint will shift toward Snare classification across industries as escape becomes harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_industry_surveillance_contagion, empirical, 'Whether surveillance spreads through competitive labor market dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(workplace_surveillance_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wks_tr_t0, workplace_surveillance_extraction, theater_ratio, 0, 0.38).
narrative_ontology:measurement(wks_tr_t5, workplace_surveillance_extraction, theater_ratio, 5, 0.51).
narrative_ontology:measurement(wks_tr_t10, workplace_surveillance_extraction, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(wks_be_t0, workplace_surveillance_extraction, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(wks_be_t5, workplace_surveillance_extraction, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(wks_be_t10, workplace_surveillance_extraction, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(wks_su_t0, workplace_surveillance_extraction, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(wks_su_t5, workplace_surveillance_extraction, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(wks_su_t10, workplace_surveillance_extraction, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(workplace_surveillance_extraction, enforcement_mechanism).
narrative_ontology:affects_constraint(workplace_surveillance_extraction, wage_suppression_via_algorithmic_justification).
narrative_ontology:affects_constraint(workplace_surveillance_extraction, union_organizing_preemption_surveillance).
narrative_ontology:affects_constraint(workplace_surveillance_extraction, worker_dignity_erosion_panopticon).

% DUAL FORMULATION NOTE:
% Workplace surveillance as a single constraint exhibits both coordination (task tracking, compliance monitoring) and extraction (behavioral control, privacy invasion, algorithmic management) functions. Some analyses decompose this into separate constraints: surveillance_as_coordination (ε ≈ 0.15-0.20, Rope) and surveillance_as_behavioral_control (ε ≈ 0.70+, Snare). The unified tangled_rope classification (ε = 0.58) captures the inseparability of these mechanisms in actual organizational practice. The constraint's principal effect is not coordination; coordination is the justification for extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(workplace_surveillance_extraction, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
