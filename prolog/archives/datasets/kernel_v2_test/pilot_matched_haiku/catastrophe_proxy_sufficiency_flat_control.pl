% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency_flat_control, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_proxy_sufficiency_flat_control
 *   human_readable: Catastrophe-Avoidance Competence Maintenance Commitment
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   The shared commitment that catastrophe-avoidance competence must be
 *   actively maintained rather than assumed stable creates a structural
 *   tension between genuine organizational learning needs and the
 *   institutional mechanisms that enforce maintenance. This constraint
 *   operates across high-reliability organizations (nuclear power, aviation,
 *   healthcare, chemical processing) and generates different classifications
 *   depending on the observer's structural position. The commitment appears
 *   necessary from the perspective of catastrophe prevention but becomes
 *   increasingly theatrical as regulatory compliance decouples from
 *   functional competence validation. The core contestation is not whether
 *   maintenance is needed, but what constitutes 'sufficient' maintenance — a
 *   question that has no objective answer and therefore becomes a site of
 *   ongoing extraction. The theater ratio has risen from 0.35 to 0.72 over
 *   the interval, indicating that maintenance activity has become
 *   increasingly performative relative to functional competence validation.
 *   The extractiveness has risen from 0.22 to 0.38, reflecting both the
 *   increasing burden of compliance and the growing gap between regulatory
 *   requirements and actual competence maintenance.
 *
 * KEY AGENTS:
 *   - Frontline Operator: Primary victim (powerless/trapped) — bears continuous maintenance burden with no control over sufficiency standards
 *   - Resource-Constrained Organization: Secondary victim (moderate/constrained) — needs competence maintenance but faces asymmetric compliance costs
 *   - Safety Management Profession: Primary beneficiary (institutional/arbitrage) — benefits from demand for expertise, training, consulting, certification
 *   - Regulatory Agency: Secondary beneficiary (institutional/arbitrage) — benefits from clear regulatory object and enforcement authority
 *   - High-Reliability Organization Network: Organized agent (organized/constrained) — benefits from shared safety standard but constrained by continuous maintenance burden; has some agency in defining sufficiency
 *   - Maintenance Theater System: Institutional actor (institutional/arbitrage) — perpetuates performative compliance regimes through inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contestable empirical claims about competence decay
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency_flat_control, 0.38).
domain_priors:suppression_score(catastrophe_proxy_sufficiency_flat_control, 0.42).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency_flat_control, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency_flat_control, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency_flat_control, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency_flat_control, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency_flat_control, tangled_rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency_flat_control, "Catastrophe-Avoidance Competence Maintenance Commitment").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency_flat_control, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(catastrophe_proxy_sufficiency_flat_control, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency_flat_control, safety_management_profession).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency_flat_control, regulatory_agencies).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency_flat_control, organizational_leadership).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency_flat_control, operational_frontline_workers).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency_flat_control, resource_constrained_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency_flat_control, resource_constrained_organization).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency_flat_control, regulatory_agency).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency_flat_control, high_reliability_organization_network).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency_flat_control, frontline_operator).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency_flat_control, resource_constrained_organization).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency_flat_control, high_reliability_organization_network).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Performs continuous maintenance drills, competence validations, and certification exercises as required by organizational and regulatory standards. Bears the labor cost of maintenance activity without control over what constitutes 'sufficient' maintenance. Cannot exit without accepting catastrophic risk or abandoning safety-critical role. Experiences the commitment as an externally imposed burden with no clear endpoint.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, frontline_operator, payer,
    powerless, biographical, trapped, national).

% Must maintain catastrophe-avoidance competence to avoid catastrophic risk and regulatory penalties, but faces significant resource burden in meeting compliance requirements. Constrained by regulatory mandates and liability exposure. Benefits from the shared understanding that maintenance is necessary (coordination function) but pays disproportionately through compliance costs. Cannot exit without accepting unacceptable risk.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, resource_constrained_organization, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency_flat_control, resource_constrained_organization, beneficiary).

% Benefits from the commitment through demand for safety expertise, training programs, consulting services, and certification systems. The commitment that maintenance must be active creates a continuous market for safety professionals. Can shift focus to emerging risk domains or expand into new sectors (arbitrage options). Collects rents from the maintenance requirement without bearing the operational burden.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, safety_management_profession, beneficiary,
    institutional, immediate, arbitrage, global).

% Benefits from the commitment through regulatory authority, inspection regimes, and enforcement capacity. The commitment that maintenance must be active provides a clear regulatory object and measurable compliance criteria. Can adjust maintenance standards, shift to outcome-based regulation, or expand regulatory scope (arbitrage options). Collects authority and legitimacy from the maintenance requirement.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, regulatory_agency, beneficiary,
    institutional, generational, arbitrage, national).

% Organized network of nuclear operators, aviation systems, healthcare organizations, and chemical processors that have internalized the commitment and benefit from it through reduced catastrophic risk. Constrained by the need to maintain competence continuously and by regulatory compliance requirements. Benefits from the shared safety standard and can influence what constitutes 'sufficient' maintenance through professional networks and regulatory participation. Experience is mixed: genuine coordination function alongside extraction burden.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, high_reliability_organization_network, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency_flat_control, high_reliability_organization_network, payer).

% Institutional system of regulatory requirements, audit criteria, certification standards, and compliance documentation that perpetuates maintenance activity regardless of functional competence validation. The original function (ensuring competence actually persists) has partially atrophied, replaced by performative compliance. Persists through institutional inertia and because alternatives haven't fully replaced it. Can shift to new compliance regimes or expand audit scope (arbitrage options). Benefits from the perpetuation of maintenance activity.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, maintenance_theater_system, agenda_setter,
    institutional, civilizational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensuring that catastrophe-avoidance competence is maintained at levels sufficient to prevent catastrophic failure in high-reliability organizations. The genuine coordination problem is that competence degrades without active maintenance, and organizations need a shared understanding of the necessity and mechanisms of maintenance.
% TRANSFER_FUNCTION: The commitment transfers maintenance labor and compliance costs from the safety management profession and regulatory agencies to frontline operators and resource-constrained organizations. It also transfers authority and legitimacy to regulatory agencies and demand for services to the safety profession. The transfer is asymmetric: those who bear the burden (operators, constrained organizations) have less control over what constitutes 'sufficient' maintenance than those who benefit (safety profession, regulators).
% ABSENT_VOICES: Organizations that have abandoned active maintenance regimes (if any exist) are absent from the conversation. Operators who have left safety-critical roles due to maintenance burden are absent. Researchers studying alternative competence-maintenance mechanisms are underrepresented. The voices most present are those of the safety profession and regulatory agencies, who benefit from the commitment.
% DISAPPEARANCE_RATIONALE: If the commitment disappeared overnight, the world would partially rearrange: organizations would need to develop alternative mechanisms for maintaining catastrophe-avoidance competence, and the demand for safety expertise would shift. However, the extent of rearrangement is contested. Some argue that competence would degrade rapidly without active maintenance (world rearranges significantly). Others argue that competence could be maintained through alternative mechanisms (organizational redundancy, continuous operational exposure, technological systems) with less burden (world rearranges minimally). The empirical question of competence decay rates is unresolved.
% FOUNDING_PROBLEM: The founding problem is that catastrophe-avoidance competence degrades without active maintenance, and organizations need mechanisms to ensure that competence is continuously validated and refreshed. This problem emerged from historical catastrophes (Three Mile Island, Chernobyl, Challenger, etc.) where competence degradation or loss of institutional memory contributed to failure.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by high-reliability organization researchers (Weick, Sutcliffe, Roberts) and by regulatory agencies that have implemented maintenance requirements. However, the empirical support for the specific claim that competence decays rapidly without maintenance is contested. Some researchers argue that competence is more stable than the commitment assumes; others argue that alternative mechanisms (technological systems, organizational redundancy) can substitute for active maintenance. The safety profession and regulatory agencies attest the founding problem's continued relevance, but they have incentives to maintain the commitment regardless of empirical support.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency_flat_control, contested).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency_flat_control, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE OPERATOR (SNARE) — Trapped in perpetual maintenance drills and competence validation cycles with no exit. Bears the cost of continuous exercise requirements without control over what constitutes 'sufficient' maintenance. Extraction is maximal: the operator must perform maintenance labor while the sufficiency standard remains contested and externally imposed.
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RESOURCE-CONSTRAINED ORGANIZATION (TANGLED ROPE) — Genuinely needs catastrophe-avoidance competence maintenance (coordination function) but faces asymmetric extraction through compliance costs. Constrained by regulatory requirements and liability exposure; cannot exit without accepting catastrophic risk. Benefits from the commitment's coordination function but pays disproportionately through resource burden.
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SAFETY MANAGEMENT PROFESSION (ROPE) — Primary beneficiary. Experiences the commitment as pure coordination: the shared belief that maintenance must be active creates demand for safety expertise, consulting services, training programs, and certification. Net beneficiary with arbitrage options (can shift focus to emerging risk domains, expand into new sectors). Extraction runs toward this agent.
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AGENCY (ROPE) — Institutional beneficiary. The commitment that maintenance must be active justifies regulatory authority, inspection regimes, and enforcement capacity. Experiences the constraint as coordination: mandating active maintenance creates a clear regulatory object and measurable compliance criteria. Arbitrage options available (can adjust maintenance standards, shift to outcome-based vs process-based regulation).
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: HIGH-RELIABILITY ORGANIZATION NETWORK (TANGLED ROPE) — Organized agents (nuclear operators, aviation, healthcare systems) that have internalized the commitment and benefit from it through reduced catastrophic risk. Constrained by the need to maintain competence continuously but also benefit from the shared standard. Experience is mixed: genuine coordination function (shared understanding of maintenance necessity) alongside extraction (continuous resource burden, regulatory compliance costs). The HRO network has agency and can influence what constitutes 'sufficient' maintenance.
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MAINTENANCE THEATER SYSTEM (PITON) — The commitment's original function (ensuring catastrophe-avoidance competence actually persists) has partially atrophied. Many organizations perform maintenance exercises that satisfy regulatory requirements and audit criteria without demonstrating that competence has actually been maintained or that the exercise would catch real degradation. The theater persists through institutional inertia: organizations continue elaborate maintenance regimes because alternatives haven't fully replaced them and because the regulatory framework rewards visible activity. Theater ratio (0.65) reflects that a substantial portion of maintenance activity is performative compliance rather than functional competence validation.
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the commitment appears as an immutable principle: catastrophe-avoidance competence MUST degrade without active maintenance — this is a law of organizational learning and skill atrophy. The analytical observer risks naturalizing what is actually a contestable claim about human and organizational capability. The engine's false summit detector will identify this as a false summit: the 'law' that competence decays without maintenance is grounded in empirical claims about skill atrophy and organizational memory that are themselves contested.
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_proxy_sufficiency_flat_control, TR),
    TR >= 0.70.

:- end_tests(catastrophe_proxy_sufficiency_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The commitment creates genuine coordination value (shared understanding that competence must be maintained) but also extracts through compliance burden and the contestation over sufficiency standards. The extraction is not maximal because some organizations benefit from the commitment (HROs with resources to exceed minimums) and because the commitment does address a real problem (competence decay). The rising trajectory (0.22 → 0.38) reflects increasing regulatory burden and growing gap between compliance and function. Suppression (0.42): Moderate. Operators and resource-constrained organizations face significant barriers to exit (regulatory requirements, liability exposure, catastrophic risk if competence lapses) but are not completely trapped. Some organizations can negotiate maintenance standards; some operators can move to less safety-critical roles. The rising trajectory (0.25 → 0.42) reflects increasing regulatory enforcement and tightening compliance requirements. Theater ratio (0.65): High. A substantial portion of maintenance activity is performative compliance rather than functional competence validation. Organizations conduct drills to satisfy audit criteria, maintain certifications to demonstrate regulatory compliance, and document procedures to create liability protection — not necessarily to validate that competence has actually been maintained or that exercises would catch real degradation. The rising trajectory (0.35 → 0.72) indicates that maintenance has become increasingly theatrical as regulatory requirements have proliferated without corresponding clarity on what constitutes functional sufficiency.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of classification from a single set of base properties. The frontline operator sees pure extraction (Snare) — they bear the maintenance burden with no control over standards. The resource-constrained organization sees mixed coordination and extraction (Tangled Rope) — they need competence maintenance but pay disproportionately. The safety profession sees pure coordination (Rope) — the commitment creates demand for their expertise. The regulatory agency sees coordination (Rope) — the commitment justifies their authority. The HRO network sees mixed coordination and extraction (Tangled Rope) — they benefit from the shared standard but constrained by continuous burden. The maintenance theater system sees degraded function (Piton) — the original competence-validation function has atrophied, replaced by performative compliance. The analytical observer risks seeing natural law (Mountain) — competence must decay without maintenance — but the structural data reveals this as a false summit: the 'law' is grounded in contestable empirical claims about skill atrophy and organizational memory.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by the agent's structural position relative to the extraction flow. Frontline operators are trapped with no exit and bear full costs: d approaches 1.0 (full target). Resource-constrained organizations are constrained but not trapped, and receive some coordination benefit: d is moderate (~0.65). The safety profession benefits from the commitment with arbitrage options: d approaches 0.0 (full beneficiary). The regulatory agency benefits with arbitrage options: d approaches 0.0 (full beneficiary). The HRO network has agency and mixed benefits: d is moderate (~0.45). The maintenance theater system benefits from perpetuating compliance regimes: d approaches 0.0 (full beneficiary). The analytical observer's directionality is derived from the commitment's beneficiary structure: the observer sees that the commitment benefits institutional actors (safety profession, regulators) and extracts from powerless and constrained actors, which would suggest d is moderate-to-high from the observer's perspective if the observer is positioned as an advocate for the powerless. However, the analytical observer's position is civilizational and universal, which typically produces low d (the observer is not embedded in the extraction flow). The false summit detection will flag that the mountain classification is grounded in beneficiary presence (the commitment benefits the safety profession and regulators) combined with the claim of natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question is central to this constraint: has the original mandate (ensuring catastrophe-avoidance competence actually persists) outlived its function? The rising theater ratio (0.35 → 0.72) and the piton perspective suggest that the mandate may be dead or dying — organizations perform maintenance exercises that satisfy regulatory requirements without demonstrating that competence has actually been maintained. However, the empirical question remains open: do organizations that abandon active maintenance experience increased catastrophic risk? If yes, the mandate is live and the constraint remains tangled rope (mixed coordination and extraction). If no, the constraint should be reclassified as piton (degraded, maintained through theater). The contestation over 'sufficiency' standards prevents resolution of this question — without a clear definition of what constitutes sufficient maintenance, it is impossible to determine whether the commitment's function is being fulfilled or merely performed. This unresolved contestation is itself the extraction mechanism: the commitment persists because no one can definitively prove it is unnecessary, and the burden of proof falls on those who would abandon it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_decay_rate_empirical,
    'What is the actual decay rate of catastrophe-avoidance competence in the absence of active maintenance, and does it vary by domain, operator experience level, and task complexity?',
    'Longitudinal studies tracking competence degradation in operators with varying maintenance schedules; comparison of competence retention across domains (nuclear, aviation, healthcare, chemical processing); analysis of actual incident causation to determine whether competence decay was a factor',
    'If decay is rapid and universal: the commitment is grounded in genuine natural law (mountain classification strengthens). If decay is slow, domain-dependent, or experience-dependent: the commitment is a contestable empirical claim, and the ''sufficiency'' standard becomes a policy choice rather than a natural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_decay_rate_empirical, empirical, 'Actual decay rate of catastrophe-avoidance competence without maintenance').

omega_variable(
    sufficiency_standard_contestation,
    'What constitutes ''sufficient'' maintenance exercise, and who has authority to define it? Is sufficiency determined by regulatory compliance, demonstrated competence retention, organizational risk tolerance, or some combination?',
    'Analysis of regulatory standards across domains; comparison of maintenance regimes in organizations with different incident histories; examination of whether organizations that exceed regulatory minimums show lower incident rates; stakeholder interviews with operators, safety professionals, and regulators about what they believe constitutes sufficiency',
    'If sufficiency is objectively measurable: the commitment can be operationalized and the extraction mechanism is reduced (clearer compliance path). If sufficiency is fundamentally contestable: the commitment becomes a site of ongoing negotiation, and the extraction mechanism persists (no clear path to compliance, perpetual maintenance burden).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sufficiency_standard_contestation, conceptual, 'Definition and authority for ''sufficient'' maintenance exercise').

omega_variable(
    maintenance_theater_effectiveness,
    'Do organizations that perform high-theater maintenance regimes (elaborate drills, frequent certifications, extensive documentation) actually maintain competence better than organizations with lower-theater but more targeted maintenance?',
    'Comparative analysis of incident rates and competence retention across organizations with different maintenance theater ratios; analysis of whether regulatory compliance correlates with actual safety outcomes; examination of whether maintenance exercises catch real competence degradation or merely satisfy audit criteria',
    'If theater correlates with safety outcomes: the piton classification is incorrect and the commitment is functioning as intended. If theater does NOT correlate with safety: the piton classification is confirmed, and the commitment has become a mechanism for extracting compliance labor without functional benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maintenance_theater_effectiveness, empirical, 'Whether maintenance theater correlates with actual competence retention').

omega_variable(
    false_summit_natural_law_status,
    'Is the commitment that catastrophe-avoidance competence must be actively maintained a genuine natural law of organizational learning, or is it a constructed institutional arrangement that benefits the safety management profession and regulatory agencies?',
    'Historical analysis of when and why the commitment emerged; examination of whether organizations that reject the commitment experience higher catastrophic risk; analysis of whether the commitment''s beneficiaries (safety profession, regulators) have incentives to maintain it regardless of empirical support; cross-cultural comparison of maintenance approaches and outcomes',
    'If natural law: the mountain classification is correct, and the commitment should be universally enforced. If constructed: the false summit detector correctly identifies it, and the commitment becomes subject to contestation about its necessity and sufficiency standards.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_status, conceptual, 'Whether the maintenance commitment is natural law or constructed institutional arrangement').

omega_variable(
    organizational_learning_alternative_pathways,
    'Are there alternative mechanisms for maintaining catastrophe-avoidance competence that do not require the active maintenance commitment (e.g., organizational redundancy, automated systems, continuous operational exposure, knowledge management systems)?',
    'Analysis of organizations that maintain high safety records without extensive active maintenance regimes; examination of whether technological or organizational alternatives can substitute for active maintenance; study of whether continuous operational exposure (high-frequency low-consequence incidents) maintains competence as effectively as deliberate exercises',
    'If alternatives exist and are effective: the commitment is not necessary, and the extraction mechanism can be reduced or eliminated. If alternatives are ineffective or unavailable: the commitment is necessary, but the ''sufficiency'' standard remains contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_learning_alternative_pathways, empirical, 'Alternative mechanisms for maintaining catastrophe-avoidance competence').

omega_variable(
    mandatrophy_status_maintenance_function,
    'Has the original mandate of the commitment (ensuring catastrophe-avoidance competence actually persists) outlived its function, or does it remain vital to organizational safety?',
    'Analysis of whether organizations that have abandoned active maintenance regimes experience increased catastrophic risk; examination of whether the commitment''s function has been replaced by technological systems or organizational structures; assessment of whether the commitment persists primarily through institutional inertia or through genuine necessity',
    'If mandate is dead: the constraint should be reclassified as piton (degraded, maintained through theater). If mandate is live: the constraint remains tangled rope or snare depending on perspective. If contested: the mandatrophy status itself becomes an omega variable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_status_maintenance_function, empirical, 'Whether the maintenance commitment''s original mandate remains vital or has been superseded').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency_flat_control, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catprox_tr_t0, catastrophe_proxy_sufficiency_flat_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(catprox_tr_t5, catastrophe_proxy_sufficiency_flat_control, theater_ratio, 5, 0.5).
narrative_ontology:measurement(catprox_tr_t10, catastrophe_proxy_sufficiency_flat_control, theater_ratio, 10, 0.65).
narrative_ontology:measurement(catprox_tr_t15, catastrophe_proxy_sufficiency_flat_control, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(catprox_be_t0, catastrophe_proxy_sufficiency_flat_control, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(catprox_be_t5, catastrophe_proxy_sufficiency_flat_control, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(catprox_be_t10, catastrophe_proxy_sufficiency_flat_control, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(catprox_be_t15, catastrophe_proxy_sufficiency_flat_control, base_extractiveness, 15, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(catprox_su_t0, catastrophe_proxy_sufficiency_flat_control, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(catprox_su_t5, catastrophe_proxy_sufficiency_flat_control, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(catprox_su_t10, catastrophe_proxy_sufficiency_flat_control, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(catprox_su_t15, catastrophe_proxy_sufficiency_flat_control, suppression_requirement, 15, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency_flat_control, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency_flat_control, skill_atrophy_organizational_memory).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency_flat_control, regulatory_compliance_theater).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency_flat_control, high_reliability_organization_sustainability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_proxy_sufficiency_flat_control, analytical, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
