% ============================================================================
% CONSTRAINT STORY: procurement_inertia
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_procurement_inertia, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: procurement_inertia
 *   human_readable: Pentagon Procurement Inertia: Military-Grade Premium Over Commercial Equivalents
 *   domain: security_studies/technology_governance
 *
 * SUMMARY:
 *   The Pentagon maintains procurement preference for expensive
 *   military-grade systems over cheaper commercial equivalents despite
 *   capability convergence. What began as necessary security and reliability
 *   coordination in the Cold War has atrophied into theatrical maintenance of
 *   certification requirements that no longer track capability gaps. Special
 *   operations units routinely bypass the system, proving commercial
 *   platforms meet mission requirements. The procurement bureaucracy persists
 *   not because anyone profits enough to defend it, but because the cost to
 *   fix exceeds what any single actor bears and institutional identity is
 *   fused with the existing process. KEY AGENTS (by structural relationship):
 *   - defense_contractors: Beneficiary (institutional/mobile) — collect
 *   premium pricing but do not control the system - procurement_bureaucracy:
 *   Agenda setter (institutional/identity_locked) — administers the rules,
 *   could change them, but identity-fused with existing process - taxpayers:
 *   Victim (powerless/trapped) — fund the premium with no visibility or exit
 *   - special_operations_units: Dual (organized/arbitrage) — bypass the
 *   constraint, demonstrate alternatives work - conventional_military_units:
 *   Victim (organized/constrained) — locked into standard channels, bear
 *   capability lag - government_accountability_office: Observer
 *   (institutional/analytical) — documents the dysfunction, ignored -
 *   commercial_technology_firms: Excluded (powerful/mobile) — could supply
 *   equivalent capability, structurally barred
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(procurement_inertia, 0.68).
domain_priors:suppression_score(procurement_inertia, 0.42).
domain_priors:theater_ratio(procurement_inertia, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(procurement_inertia, extractiveness, 0.68).
narrative_ontology:constraint_metric(procurement_inertia, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(procurement_inertia, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(procurement_inertia, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(procurement_inertia, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(procurement_inertia, piton).
narrative_ontology:human_readable(procurement_inertia, "Pentagon Procurement Inertia: Military-Grade Premium Over Commercial Equivalents").
narrative_ontology:topic_domain(procurement_inertia, "security_studies/technology_governance").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(procurement_inertia, defense_contractors).
narrative_ontology:constraint_victim(procurement_inertia, taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(procurement_inertia, special_operations_units).
narrative_ontology:constraint_victim(procurement_inertia, special_operations_units).
narrative_ontology:constraint_victim(procurement_inertia, conventional_military_units).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive contracts for military-grade systems at premium prices justified by certification requirements, ruggedization standards, and security protocols. The procurement process favors established vendors with security clearances and existing contract vehicles. Revenue depends on maintaining the distinction between military-grade and commercial-grade equipment even when functional capability converges.
narrative_ontology:constraint_stakeholder(procurement_inertia, defense_contractors, beneficiary,
    institutional, generational, mobile, national).

% Administers the acquisition regulations, certification standards, and approval processes that gate what equipment enters the supply chain. Career advancement depends on risk avoidance and compliance with established procedures. The bureaucracy could streamline commercial adoption but bears the career risk of any failure while capturing none of the cost savings. Changing the system would require admitting decades of unnecessary expense.
narrative_ontology:constraint_stakeholder(procurement_inertia, procurement_bureaucracy, agenda_setter,
    institutional, biographical, identity_locked, national).

% Fund defense procurement through taxation with no direct visibility into cost-capability tradeoffs. Bear the opportunity cost of budget allocation to premium systems when commercial equivalents would suffice. Cannot exit the tax obligation and have no mechanism to contest specific procurement decisions.
narrative_ontology:constraint_stakeholder(procurement_inertia, taxpayers, payer,
    powerless, biographical, trapped, national).

% Operate under mission urgency that permits bypassing standard procurement channels. Routinely adopt commercial drones, smartphones, and software because operational tempo cannot wait for military-grade certification. Demonstrate that commercial platforms meet mission requirements at fraction of cost, but their workarounds do not feed back into institutional procurement reform. Benefit from mission flexibility while paying the institutional cost of maintaining two parallel supply chains.
narrative_ontology:constraint_stakeholder(procurement_inertia, special_operations_units, payer,
    organized, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(procurement_inertia, special_operations_units, beneficiary).

% Locked into standard procurement channels and cannot adopt commercial alternatives without formal approval. Wait years for capability that special operations acquire commercially in weeks. Bear the operational cost of capability lag and the budget cost of premium pricing, but lack the institutional authority to bypass the system.
narrative_ontology:constraint_stakeholder(procurement_inertia, conventional_military_units, payer,
    organized, biographical, constrained, global).

% Audits procurement outcomes and documents cost overruns, schedule delays, and commercial-military capability gaps. Publishes reports showing that commercial platforms meet mission requirements at lower cost. Recommendations are filed and ignored; the bureaucracy treats audit findings as compliance theater rather than reform triggers.
narrative_ontology:constraint_stakeholder(procurement_inertia, government_accountability_office, observer,
    institutional, generational, analytical, national).

% Produce platforms with comparable or superior capability to military-grade systems but cannot access defense procurement without security clearances, facility certifications, and multi-year approval processes. The barrier to entry is administrative rather than technical. Most choose to serve commercial markets rather than navigate defense bureaucracy.
narrative_ontology:constraint_stakeholder(procurement_inertia, commercial_technology_firms, excluded,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(procurement_inertia, diffuse).
narrative_ontology:fixing_cost_class(procurement_inertia, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally coordinated defense acquisition around security requirements, supply chain integrity, and operational reliability standards that commercial markets did not address in the Cold War era.
% TRANSFER_FUNCTION: Moves taxpayer dollars to defense contractors through procurement budgets, justified by military-grade certification requirements that increasingly duplicate commercial capability at premium cost.
% ABSENT_VOICES: Commercial technology firms that could supply equivalent capability at lower cost are structurally excluded by certification barriers. Taxpayers funding the premium have no seat in procurement decisions. Future warfighters who will inherit the capability lag are not in the room.
% DISAPPEARANCE_RATIONALE: If the military-grade premium requirement vanished overnight, procurement would shift toward commercial platforms already proven in special operations use. Defense contractors would lose premium pricing power. Conventional units would gain capability parity with special operations. The budget would reallocate or compress. The institutional identity of the procurement bureaucracy would require reconstruction.
% FOUNDING_PROBLEM: Cold War-era defense technology operated at the frontier of what was technically possible and required security-cleared supply chains that commercial markets could not provide. Military-grade standards ensured reliability, interoperability, and supply chain integrity when commercial alternatives did not exist or were demonstrably inadequate.
% FOUNDING_PROBLEM_CORROBORATION: Special operations units demonstrate daily that commercial platforms meet mission requirements in contested environments. GAO reports document capability convergence and cost divergence. Independent defense analysts and congressional testimony from outside the contractor-bureaucracy nexus confirm that the commercial-military technology gap has inverted: commercial platforms now lead in many capability domains while military procurement lags by acquisition cycle length.
narrative_ontology:disappearance_verdict(procurement_inertia, world_rearranges).
narrative_ontology:founding_problem_status(procurement_inertia, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(procurement_inertia, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-03',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(procurement_inertia, 'none', 1).
narrative_ontology:epsilon_provenance(procurement_inertia, 0.68, 'claude-sonnet-4-5-20250929', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(procurement_inertia_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(procurement_inertia, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(procurement_inertia_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.45 → 0.68) as commercial-military capability convergence makes the premium increasingly unjustifiable. Theater ratio rises sharply (0.38 → 0.71) as certification and review processes become performative rather than functional — the bureaucracy maintains the appearance of rigorous standards while special operations prove the standards are obsolete. Suppression falls (0.62 → 0.42) because enforcement capacity erodes: the system cannot prevent special operations from bypassing it, and each bypass demonstrates the constraint is not necessary. Accessibility collapse is low (0.38) because alternatives are visible and proven. Resistance is moderate (0.52) because conventional units and taxpayers lack the institutional power to force reform, but special operations demonstrate defection daily.
 *
 * PERSPECTIVAL GAP:
 *   The procurement bureaucracy experiences this as legitimate coordination — maintaining standards, ensuring reliability, protecting security. From that seat the constraint is a degraded rope whose coordination function has weakened but remains necessary. Taxpayers and conventional units experience it as extraction — paying premium costs for capability lag while special operations prove cheaper alternatives work. The GAO observer seat sees it as institutional capture by inertia rather than by any concentrated beneficiary. The engine computes these divergent classifications from the structural data; the claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Defense contractors are beneficiaries but not controllers — they collect premium pricing but do not set the rules and could adapt to commercial competition if forced. The procurement bureaucracy is the agenda setter and is identity-locked: career paths, institutional prestige, and self-concept are fused with the existing certification regime. Admitting the system is obsolete would require admitting decades of unnecessary cost and capability lag. Taxpayers are trapped victims with no exit and no visibility. Special operations have arbitrage exit and use it constantly, which makes them dual-positioned: they benefit from mission flexibility but pay the institutional cost of maintaining parallel supply chains. Conventional units are constrained victims: they see the alternative but cannot access it.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a piton, not a snare, because no party profits enough to actively maintain it. Defense contractors benefit but do not control procurement policy and would adapt to commercial competition if forced. The procurement bureaucracy administers the system but is identity-locked rather than rent-seeking: the cost to reform exceeds the bureaucratic pain of persistence, and institutional identity is fused with the certification regime. The constraint persists by inertia and identity fusion, not by concentrated extraction. A snare would have a beneficiary seat with institutional power defending the arrangement; here the beneficiary is passive and the administrator is trapped by its own identity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capability_gap_inversion,
    'At what point did commercial technology capability cross over military-grade systems, and how long has the procurement premium persisted after that crossover?',
    'Historical analysis of capability benchmarks (sensor resolution, processing power, reliability metrics) comparing commercial and military platforms over time, cross-referenced with procurement budget allocation.',
    'If the crossover occurred decades ago, the constraint is pure inertia and the theater ratio should be near 1.0. If recent, some coordination function remains and the piton classification is premature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_gap_inversion, empirical, 'When did the founding problem actually die, and how long has the zombie persisted?').

omega_variable(
    identity_lock_strength,
    'Is the procurement bureaucracy''s identity fusion with military-grade standards strong enough to prevent reform even under external pressure, or would congressional mandate or leadership turnover break the lock?',
    'Natural experiment from reform attempts: do leadership changes, congressional directives, or budget crises produce procurement streamlining, or does the bureaucracy absorb and neutralize reform pressure?',
    'If identity lock is weak, the constraint is a snare maintained by contractor lobbying rather than a piton maintained by bureaucratic inertia. If strong, the piton classification holds and fixing cost is prohibitive without external forcing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Whether the bureaucracy''s identity fusion is the binding constraint or a cover story for contractor capture.').

omega_variable(
    special_operations_feedback_loop,
    'Why does special operations'' routine demonstration of commercial platform adequacy not feed back into institutional procurement reform?',
    'Institutional ethnography: how are special operations workarounds framed internally? Are they treated as exceptions proving the rule, or as evidence the rule is obsolete? What structural barriers prevent lessons learned from propagating?',
    'If the feedback loop is structurally blocked, the constraint is a piton whose theater ratio will continue rising. If the loop is weak but functional, reform pressure will eventually overcome inertia and the constraint will collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(special_operations_feedback_loop, conceptual, 'Why demonstrated alternatives do not trigger institutional learning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(procurement_inertia, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(proc_tr_t0, procurement_inertia, theater_ratio, 0, 0.38).
narrative_ontology:measurement(proc_tr_t7, procurement_inertia, theater_ratio, 7, 0.47).
narrative_ontology:measurement(proc_tr_t14, procurement_inertia, theater_ratio, 14, 0.56).
narrative_ontology:measurement(proc_tr_t21, procurement_inertia, theater_ratio, 21, 0.63).
narrative_ontology:measurement(proc_tr_t28, procurement_inertia, theater_ratio, 28, 0.68).
narrative_ontology:measurement(proc_tr_t35, procurement_inertia, theater_ratio, 35, 0.71).

% Extraction over time
narrative_ontology:measurement(proc_be_t0, procurement_inertia, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(proc_be_t7, procurement_inertia, base_extractiveness, 7, 0.51).
narrative_ontology:measurement(proc_be_t14, procurement_inertia, base_extractiveness, 14, 0.58).
narrative_ontology:measurement(proc_be_t21, procurement_inertia, base_extractiveness, 21, 0.63).
narrative_ontology:measurement(proc_be_t28, procurement_inertia, base_extractiveness, 28, 0.66).
narrative_ontology:measurement(proc_be_t35, procurement_inertia, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(proc_su_t0, procurement_inertia, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(proc_su_t7, procurement_inertia, suppression_requirement, 7, 0.58).
narrative_ontology:measurement(proc_su_t14, procurement_inertia, suppression_requirement, 14, 0.53).
narrative_ontology:measurement(proc_su_t21, procurement_inertia, suppression_requirement, 21, 0.48).
narrative_ontology:measurement(proc_su_t28, procurement_inertia, suppression_requirement, 28, 0.45).
narrative_ontology:measurement(proc_su_t35, procurement_inertia, suppression_requirement, 35, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(procurement_inertia, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% Downstream of technology_diffusion_asymmetry (mountain): the commercial technology sector's faster innovation cycle creates the capability convergence that makes procurement inertia visible. The upstream constraint is a genuine natural law (technology diffusion follows power-law dynamics); this constraint is the institutional failure to adapt to that reality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(procurement_inertia, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
