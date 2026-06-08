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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Catastrophe Proxy Sufficiency and Flat Control in High-Reliability Organizations
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   High-reliability organizations (nuclear plants, aviation systems,
 *   offshore drilling platforms) operate under a shared commitment:
 *   catastrophe-avoidance competence cannot be assumed stable and must be
 *   actively maintained through regular exercises, drills, and periodic
 *   testing. This commitment is grounded in genuine epistemic insight —
 *   competence decays under disuse, organizational learning inertias, and
 *   worst-case scenarios are rarely encountered in normal operations.
 *   However, the constraint exhibits structural tension between its
 *   coordination function (maintaining real catastrophe-avoidance capacity in
 *   large organizations with heterogeneous operational contexts) and its
 *   extraction mechanism (proxy-drill certification creates institutional
 *   credibility and immunity that may not correlate with actual competence).
 *   The constraint manifests as tangled rope at the institutional level
 *   (genuine coordination + asymmetric extraction), snare at the operational
 *   workforce level (trapped in performative compliance), and piton at the
 *   safety apparatus level (theater-ratio 0.61 indicates that much of the
 *   maintenance activity is performative certification-generation rather than
 *   functional competence-building). The rising theater ratio over the
 *   measurement interval (0.38 → 0.61) suggests that the maintenance
 *   commitment is increasingly decoupling from actual competence verification
 *   — drills are becoming primarily bureaucratic compliance rituals rather
 *   than effective learning mechanisms.
 *
 * KEY AGENTS:
 *   - Operational Workforce: Primary victims (powerless/trapped) — must execute drills while forbidden from questioning sufficiency; bear reputational cost if drills fail; experience maximum extraction through compliance requirement
 *   - Field Safety Engineers: Secondary actor (moderate/constrained) — coordinate genuine safety requirements through proxy drills while extracting institutional credibility via certification; trapped between realistic assessment and institutional requirement to certify passing
 *   - Risk Governance Institution: Primary beneficiary (institutional/arbitrage) — solves distributed risk-management problem across heterogeneous contexts; experiences constraint as pure coordination
 *   - Safety Apparatus Operators: Institutional maintainers (institutional/mobile) — design and run maintenance systems; benefit from continued institutional need for their apparatus; maintain degraded competence-verification function through inertia
 *   - High-Reliability Organization Network: Organized beneficiaries (organized/constrained) — maintain shared commitment; coordinate across domains; extract institutional credibility and regulatory advantage from appearance of coordinated safety
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing what may be constructed institutional standard as immutable law of organizational psychology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency_flat_control, 0.58).
domain_priors:suppression_score(catastrophe_proxy_sufficiency_flat_control, 0.42).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency_flat_control, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency_flat_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency_flat_control, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency_flat_control, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency_flat_control, tangled_rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency_flat_control, "Catastrophe Proxy Sufficiency and Flat Control in High-Reliability Organizations").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency_flat_control, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(catastrophe_proxy_sufficiency_flat_control, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency_flat_control, safety_apparatus_operators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency_flat_control, institutional_risk_governance).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency_flat_control, operational_workforce).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency_flat_control, field_adaptability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency_flat_control, risk_governance_institution).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency_flat_control, field_safety_engineers).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency_flat_control, competence_decay_under_disuse).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency_flat_control, organizational_learning_inertia).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Frontline operators and technicians must execute routine maintenance drills as scheduled, maintain documentation of performance, and accept certification-based attestation of their readiness. They bear compliance labor, reputational risk if drills appear to fail, and institutional pressure to perform drills as theater rather than genuine learning. Cannot exit without career termination. Knowledge that proxy drills may not reflect actual catastrophe-avoidance competence creates cognitive dissonance — they understand the constraint's function but experience it as performative compliance.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, operational_workforce, payer,
    powerless, biographical, trapped, national).

% Responsible for designing, executing, and certifying competence-maintenance drills. Set proxy standards (drill frequency, scope, passing criteria) while constrained by institutional requirement to certify compliance and by resource limits on true competence assessment. Extract institutional credibility through certification (the record of 'maintained readiness' protects the organization). Also bear costs: must manage tension between realistic assessment of actual competence and institutional requirement to certify passing.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, field_safety_engineers, agenda_setter,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency_flat_control, field_safety_engineers, payer).

% Benefits from the maintenance commitment as a distributed risk-management mechanism that enables organizational oversight of catastrophe-avoidance competence across heterogeneous contexts without continuous worst-case scenario deployment. The proxy system solves a genuine coordination problem: how to verify competence readiness in large organizations with many operational units. Can reallocate to different risk domains if this one becomes problematic (arbitrage exit).
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, risk_governance_institution, beneficiary,
    institutional, immediate, arbitrage, global).

% Safety departments, risk committees, compliance teams that maintain the proxy-certification system. Design drills, generate documentation, schedule exercises, produce reports. Institutional continuity depends on ongoing perceived need for their apparatus. Much of their activity is performative: designing drills that satisfy regulatory audit trails, creating certification documentation, scheduling for compliance visibility. Real competence-maintenance function has attrophied — most genuine learning happens through incident response, not scheduled drills.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, safety_apparatus_operators, agenda_setter,
    institutional, generational, mobile, national).

% Organized community of practice (nuclear operations, aviation safety, offshore drilling) that maintains and transmits the shared commitment that catastrophe-avoidance competence must be actively maintained. Coordinate across domains on maintenance standards, share incident learning, collectively benefit from institutional credibility and regulatory advantage that comes from appearing as coordinated safety practitioners. Constrained by regulatory environment and by need to maintain legitimacy as safety-conscious institutions.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, high_reliability_organization_network, agenda_setter,
    organized, civilizational, constrained, global).

% The organizational capacity to learn and adapt from novel scenarios and unexpected conditions. Proxy-maintenance systems that consume resources and attention for ritual compliance reduce institutional bandwidth for genuine adaptive learning. Organizations that optimize for drill performance may ossify their response patterns and lose capability to handle scenarios outside the drill design space.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, field_adaptability, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(catastrophe_proxy_sufficiency_flat_control, field_adaptability).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintaining catastrophe-avoidance competence in large organizations with heterogeneous operational contexts, distributed across many units, where worst-case scenarios are rarely encountered in normal operations and competence decays under disuse. The constraint solves the distributed risk-management problem: how to verify readiness without continuous deployment of highest-consequence scenarios.
% TRANSFER_FUNCTION: The constraint transfers compliance labor (operational workforce executes drills, documents performance) and reputational risk (operators bear cost if drills fail) to beneficiaries (risk governance institution, safety apparatus operators) who capture institutional credibility, regulatory advantage, and organizational immunity from certification. It transfers attention and resources away from adaptive organizational learning toward performative compliance documentation.
% ABSENT_VOICES: Operators who have experienced actual catastrophes or near-misses outside drill design spaces; organizations that did not maintain catastrophe-avoidance through formal periodic drills but achieved safety through alternative mechanisms (distributed informal practice, high-consequence learning environments, extreme operational discipline); field practitioners in domains where proxy-drill correlation to actual competence is known to be low.
% DISAPPEARANCE_RATIONALE: If the commitment to active maintenance of catastrophe-avoidance competence disappeared, organizations would face urgent pressure to establish alternative verification mechanisms. Competence decay under disuse is real; organizations cannot assume stable readiness. In the absence of formal proxy systems, some organizations would invest in continuous low-stakes testing, incident learning systems, or high-fidelity simulation. Others would revert to risk-averse operational posture (reducing complexity, increasing human oversight, accepting lower efficiency). The constraint's infrastructure (maintenance apparatus, certification systems, regulatory reporting) would dismantle. The shared commitment itself (that maintenance is necessary) would likely persist, but the specific proxy standard would be contested and reformed.
% FOUNDING_PROBLEM: Catastrophe-avoidance competence decays under disuse and organizational learning inertias accumulate, but worst-case scenarios are rare or non-existent in normal operations. How can large organizations with distributed operational units maintain readiness for low-probability, high-consequence events without deploying those events continuously? The proxy-maintenance commitment was built to solve distributed verification in organizations where true competence testing (deploying actual worst-case scenarios) is infeasible or dangerous.
% FOUNDING_PROBLEM_CORROBORATION: Field safety engineering literature (Weick & Sutcliffe on high-reliability organizations, Reason on human factors); incident investigation reports (competence gaps identified after failures); organizational learning research (skill decay rates, organizational inertia); attested by risk governance institutions, safety practitioners, and operational workforce who all acknowledge competence decay under disuse. The problem is live and recognized across the field.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency_flat_control, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPERATIONAL WORKFORCE (SNARE) — Powerless frontline staff, trapped in a system where they must execute maintenance drills they suspect are theater; forbidden from questioning the proxy's sufficiency; bear reputational cost if drills fail. Cannot exit without career termination. Experience maximal extraction through the gap between what certification claims and what they know about actual readiness.
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIELD SAFETY ENGINEERS (TANGLED ROPE) — Moderate power, constrained by institutional requirement to verify drills as passing and by resource limits on true competence assessment. They coordinate genuine safety requirements through proxy drills while simultaneously extracting institutional credibility: certification creates record of 'maintained readiness' that protects the institution if catastrophe occurs. Some genuine coordination function; some extraction of institutional immunity.
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RISK GOVERNANCE INSTITUTION (ROPE) — Institutional actor with arbitrage-exit (can reallocate to different risk domains). Experiences the constraint as pure coordination: the proxy system enables distributed risk management across heterogeneous operational contexts. From this perspective, the maintenance commitment solves a genuine collective-action problem — how to maintain catastrophe-avoidance capacity in large organizations without continuous operational deployment of worst-case scenarios. Sees certification as solving verification.
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SAFETY APPARATUS OPERATORS (PITON) — Institutional actors (safety departments, risk committees, compliance teams) who maintain the proxy-certification system. Theater ratio (0.61) reflects that much of their activity is performative: designing drills that satisfy regulatory audit trails, generating certification documentation, scheduling maintenance exercises for compliance visibility. The actual competence-maintenance function has attrophied — most real learning happens through incident response, not through scheduled drills. The apparatus persists through institutional inertia and regulatory requirement, not because it effectively maintains competence.
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: HIGH-RELIABILITY ORGANIZATION NETWORK (TANGLED ROPE) — Organized community of practice (nuclear operations, aviation safety, offshore drilling) that maintains the shared commitment that catastrophe-avoidance must be actively maintained. They coordinate genuine epistemic requirements (competence decay under disuse is real; organizational learning does inertiate) through proxy drills while extracting institutional credibility and regulatory advantage. The network benefits from the appearance of coordinated safety while bearing the cost of maintenance theater.
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal analytical perspective, some maintenance testing may appear immutable: complex catastrophe-avoidance systems genuinely do decay without use, and some form of periodic validation is inescapable physics and organizational psychology. However, the structural data contradicts this naturalization — the engine will compute this as a false summit, revealing that the specific proxy sufficiency standard (what counts as 'adequate' maintenance) is institutionally constructed and contestable, not a law of nature.
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

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_proxy_sufficiency_flat_control, TR),
    TR >= 0.70.

:- end_tests(catastrophe_proxy_sufficiency_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The proxy-drill system extracts from the operational workforce (compliance labor, reputational risk) and from field adaptability (resources devoted to maintenance theater rather than actual competence-building); provides institutional credibility and regulatory immunity to beneficiaries (risk governance, safety apparatus operators). The extracted value increases over time as theater ratio rises and disconnection from actual competence verification widens. Not as severe as a pure snare (0.75+) because the coordination function is genuine — large organizations genuinely do need distributed catastrophe-avoidance maintenance — but substantial because the proxy standard is contestable and may exceed functional requirement. Suppression (0.42): Moderate. Operational workforce faces career risk for questioning drill sufficiency and cannot exit without termination. Field engineers are constrained by institutional requirement to certify compliance. But suppression is not totalizing — safety practitioners can and do voice concerns through internal channels, and some organizations do modify their maintenance standards. Rising suppression_requirement trajectory (0.35 → 0.42) reflects intensifying enforcement as regulatory pressure for documented compliance increases. Theater ratio (0.61): High. Rising from 0.38 to 0.61 over the interval indicates increasing decoupling of proxy drills from actual competence verification. Much maintenance activity is performative: drill documentation for audit trails, certification generation, scheduling for regulatory visibility. Real learning happens through incident response and rare high-consequence scenarios, not through routine exercises. The high theater ratio suggests maintenance apparatus is increasingly theater-dependent.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates divergent classification across structural positions. The operational workforce sees snare — trapped compliance with questionable coordination function, bearing extraction through reputational and labor costs. The safety apparatus operators see piton — maintaining degraded verification ritual through institutional inertia, not because it works. The risk governance institution sees rope — solving genuine distributed risk-management coordination problem across heterogeneous contexts. The high-reliability organization network sees tangled rope — coordinating real catastrophe-avoidance requirements while extracting institutional credibility from certification. The civilizational analytical observer risks seeing mountain — competence decay is immutable organizational psychology, maintenance commitment is inevitable — but the structural data (rising theater ratio, contestation over sufficiency standards, extraction asymmetry) reveals this as false summit: what appears to be natural law is constructed institutional standard with identifiable beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from agent power, exit options, and beneficiary/victim relationship. Operational workforce: powerless/trapped/victim → high d (near 1.0) → maximum experienced extraction (χ). Risk governance institution: institutional/arbitrage/beneficiary → low d (near 0.0) → negative χ (subsidy, benefit). Field safety engineers: moderate/constrained/mixed → intermediate d → moderate χ. The tangled rope classification requires both genuine coordination (risk governance institution benefits while solving real problem) and asymmetric extraction (operational workforce bears costs while gaining no benefit). The rising extractiveness over time reflects increasing asymmetry: as theater ratio rises, beneficiaries capture more institutional credibility from certification while operational burden remains constant.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is 'maintain catastrophe-avoidance competence through active exercise.' The contestation is not whether the mandate is necessary, but what constitutes sufficient maintenance. The operational workforce and field engineers contest whether current proxy sufficiency standards (typical frequency: annual/biennial drills for critical systems) actually maintain real competence or whether they are over-conservative institutional requirements that extract compliance labor without commensurate safety gain. The rising theater ratio suggests the mandate has partially outlived its function — much maintenance activity has become bureaucratic compliance-generation rather than effective competence-building. The constraint is not resolved mandatrophy (the safety requirement is still live), but exhibits mandatrophy dynamics: the original mandate (maintain competence) persists while the mechanism (periodic drills) increasingly performs institutional theater rather than functional verification. This is the diagnostic signature of a constraint moving from tangled rope toward piton if theater ratio continues rising, or toward snare if extraction asymmetry increases without compensating coordination benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proxy_validity_threshold,
    'What metric or observable defines ''sufficient'' maintenance — at what point do proxy drills stop predicting actual catastrophe-avoidance competence?',
    'Post-incident investigation: correlation analysis between pre-incident drill performance and actual response competence in systems that experienced near-miss or failure events; comparison of drill-certification prediction accuracy across high-reliability domains (aviation, nuclear, maritime)',
    'If proxy-drill correlation > 0.7: current maintenance sufficiency standard is legitimate, reduces snare classification. If proxy-drill correlation < 0.4: proxy is decoupled from reality, snare classification confirmed, maintenance theater becomes primary extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_validity_threshold, empirical, 'Validity threshold for proxy-drill sufficiency as predictor of actual competence').

omega_variable(
    competence_decay_rate_contestation,
    'How fast does catastrophe-avoidance competence actually decay under disuse? Is the decay rate consistent across domains, or is it highly context-dependent?',
    'Longitudinal data from high-reliability organizations tracking measured competence (simulator performance, incident-response success, decision-making quality) across varying drill intervals; meta-analysis of organizational learning literature on skill-decay rates in safety-critical domains',
    'If decay is universal and fast: maintenance commitment is grounded in shared epistemic reality, reduces extraction reading. If decay is slow or highly variable: proxy-maintenance sufficiency standard is over-conservative, reveals extraction mechanism (unnecessary over-testing creates institutional credibility without functional gain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_decay_rate_contestation, empirical, 'Actual competence decay rates under disuse').

omega_variable(
    organizational_learning_inertia_mechanism,
    'Does the proxy-maintenance system actually prevent organizational learning inertia, or does it institutionalize inertia by freezing the definition of ''competent response''?',
    'Case studies of organizations that modified maintenance proxy standards: did modifications lead to adaptive capability improvement or to classification creep (expanding drill scope without corresponding adaptability gains); analysis of incident-response evolution before/after proxy-standard changes',
    'If proxy system prevents inertia: tangled-rope coordination function confirmed, snare reading is overstated. If proxy system institutionalizes inertia: snare extraction mechanism confirmed, maintenance drills become obstacle to genuine organizational learning.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(organizational_learning_inertia_mechanism, empirical, 'Whether proxy-maintenance system prevents or institutionalizes organizational learning inertia').

omega_variable(
    maintenance_exercise_alternative_sufficiency,
    'Do alternative competence-maintenance mechanisms (incident learning systems, real-time adaptive exercises, continuous low-stakes testing) maintain catastrophe-avoidance competence as effectively as periodic formal drills?',
    'Comparative effectiveness analysis: organizations using mixed maintenance models (scheduled drills + incident learning) vs drill-only models; measurement of adaptive response quality, incident recovery speed, and unplanned-scenario handling across models',
    'If alternatives are more effective: current proxy standard is extractive lock-in, high snare classification warranted. If alternatives are less effective: proxy system is genuinely necessary coordination, reduces snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintenance_exercise_alternative_sufficiency, empirical, 'Relative effectiveness of alternative catastrophe-avoidance competence maintenance mechanisms').

omega_variable(
    false_summit_natural_law_contestation,
    'Is the requirement for active maintenance of catastrophe-avoidance competence a natural law (competence decay is immutable human/organizational psychology) or a constructed institutional standard (the specific definition of ''adequate'' maintenance is contestable)?',
    'Historical and cross-cultural analysis: societies/organizations that did not maintain catastrophe-avoidance through formal periodic drills but achieved safety through alternative mechanisms (distributed informal practice, high-consequence learning, extreme operational discipline); analysis of whether competence decay is inevitable or contingent on specific organizational structures',
    'If natural law: mountain classification confirmed, the proxy sufficiency question is about how to instantiate the inevitable. If constructed: false summit detected, the constraint is tangled_rope at best (extraction mechanism around ''what counts as sufficient''), snare at worst (pure extraction wearing a natural-law cover story).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_contestation, conceptual, 'Whether catastrophe-avoidance competence maintenance is natural law or constructed institutional standard').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency_flat_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catprox_tr_t0, catastrophe_proxy_sufficiency_flat_control, theater_ratio, 0, 0.38).
narrative_ontology:measurement(catprox_tr_t3, catastrophe_proxy_sufficiency_flat_control, theater_ratio, 3, 0.48).
narrative_ontology:measurement(catprox_tr_t6, catastrophe_proxy_sufficiency_flat_control, theater_ratio, 6, 0.59).
narrative_ontology:measurement(catprox_tr_t10, catastrophe_proxy_sufficiency_flat_control, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(catprox_be_t0, catastrophe_proxy_sufficiency_flat_control, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(catprox_be_t3, catastrophe_proxy_sufficiency_flat_control, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(catprox_be_t6, catastrophe_proxy_sufficiency_flat_control, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(catprox_be_t10, catastrophe_proxy_sufficiency_flat_control, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(catprox_su_t0, catastrophe_proxy_sufficiency_flat_control, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(catprox_su_t5, catastrophe_proxy_sufficiency_flat_control, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(catprox_su_t10, catastrophe_proxy_sufficiency_flat_control, suppression_requirement, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency_flat_control, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency_flat_control, incident_investigation_sufficiency).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency_flat_control, operator_decision_authority_in_degraded_mode).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency_flat_control, organizational_memory_institutional_learning).

% DUAL FORMULATION NOTE:
% The catastrophe-proxy sufficiency constraint is upstream of operational constraints in high-reliability organizations: how maintenance standards are set affects incident-investigation adequacy (if maintenance drills are insufficiently challenging, incident investigations may not generate learning that updates maintenance), affects operator authority in degraded scenarios (if operators trust proxy drills to predict competence, they may over-rely on certification and under-trust local judgment), and affects organizational memory mechanisms (theater-driven maintenance may create false institutional history of 'maintained readiness' that obscures actual competence gaps).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_proxy_sufficiency_flat_control, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
