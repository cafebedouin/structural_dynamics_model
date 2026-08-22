% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__simulation_as_proxy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__simulation_as_proxy, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: competence_exercise_validity__simulation_as_proxy
 *   human_readable: Simulation-as-Proxy Competence Validation Framework
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   A regulatory and institutional framework treats simulation-based
 *   competence exercise as a valid proxy for real-world catastrophe
 *   experience. Operators satisfy annual competence requirements through
 *   simulation scenarios; regulators accept simulation completion metrics as
 *   sufficient evidence of readiness; vendors supply the simulation
 *   infrastructure. This reading instantiates the 'simulation as proxy'
 *   position: competence CAN be validly certified through high-fidelity
 *   simulation, empirical safety records under this regime prove adequacy,
 *   and regulatory compliance through simulation metrics suffices. The
 *   claim/metric gap is deliberate — the constraint is CLAIMED as tangled
 *   rope (genuine coordination problem solved: cost-effective competence
 *   maintenance at scale; asymmetric extraction: operators and budget holders
 *   benefit while field personnel and safety-critical users bear hidden
 *   risk). The authored metrics describe moderate-to-high extractiveness and
 *   rising theater ratio — the extraction score reflects the hidden transfer
 *   of risk; the theater ratio rise reflects growing reliance on simulation
 *   completion as proxy performance, decoupling from real-world incident
 *   correlation.
 *
 * KEY AGENTS:
 *   - Regulatory compliance agencies: institutional agenda-setters; define simulation equivalence standard
 *   - Budget-constrained operators: organized beneficiaries; capture cost savings from avoiding real-world drills
 *   - Field operators and emergency responders: moderate-to-powerless payers; certified through simulation but untested under catastrophe
 *   - Simulation infrastructure vendors: powerful beneficiaries; profit from regulatory-mandated equivalence
 *   - Safety-critical personnel and incident survivors: excluded; their competence-validation failures appear only post-incident
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, 0.68).
domain_priors:suppression_score(competence_exercise_validity__simulation_as_proxy, 0.61).
domain_priors:theater_ratio(competence_exercise_validity__simulation_as_proxy, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__simulation_as_proxy, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__simulation_as_proxy, "Simulation-as-Proxy Competence Validation Framework").
narrative_ontology:topic_domain(competence_exercise_validity__simulation_as_proxy, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__simulation_as_proxy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__simulation_as_proxy, 'e8caf386-0489-45ca-b884-cda85cd41c46').
narrative_ontology:cs_kernel_codification('e8caf386-0489-45ca-b884-cda85cd41c46', formalized).
narrative_ontology:cs_authority_grounding('e8caf386-0489-45ca-b884-cda85cd41c46', extraction).
narrative_ontology:cs_interpretation_layer_present('e8caf386-0489-45ca-b884-cda85cd41c46').
narrative_ontology:cs_reading_relation('e8caf386-0489-45ca-b884-cda85cd41c46', competence_exercise_validity__continuous_refresh_hybrid, coexists_with).
narrative_ontology:cs_reading_relation('e8caf386-0489-45ca-b884-cda85cd41c46', competence_exercise_validity__real_catastrophe_only, coexists_with).
narrative_ontology:cs_axiom('e8caf386-0489-45ca-b884-cda85cd41c46', foundational, simulation_equivalent_to_catastrophe).
narrative_ontology:cs_axiom_status(simulation_equivalent_to_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('e8caf386-0489-45ca-b884-cda85cd41c46', simulation_equivalent_to_catastrophe, empirically_contingent).
narrative_ontology:cs_axiom('e8caf386-0489-45ca-b884-cda85cd41c46', foundational, regulatory_compliance_sufficient_for_competence).
narrative_ontology:cs_axiom_status(regulatory_compliance_sufficient_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('e8caf386-0489-45ca-b884-cda85cd41c46', regulatory_compliance_sufficient_for_competence, conventional).
narrative_ontology:cs_reference_frame('e8caf386-0489-45ca-b884-cda85cd41c46', simulation_sufficiency_standard).
narrative_ontology:cs_drift_state('e8caf386-0489-45ca-b884-cda85cd41c46', contemporary_post_incident_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e8caf386-0489-45ca-b884-cda85cd41c46', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, regulatory_compliance_agencies).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, budget_constrained_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, simulation_infrastructure_vendors).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, field_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, safety_critical_personnel).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, emergency_response_teams).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, field_operators).
narrative_ontology:constraint_vindicates(competence_exercise_validity__simulation_as_proxy, simulation_equivalence_doctrine).
narrative_ontology:constraint_vindicates(competence_exercise_validity__simulation_as_proxy, regulatory_sufficiency_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the standard that simulation training counts toward competence certification and mandates annual renewal through simulations. Avoids costly real-world incident oversight and uses simulation completion metrics to close compliance files. Justifies the standard as equivalent to real-world experience while reducing incident frequency data requirements.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, regulatory_compliance_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Avoid the capital expense and operational disruption of running high-fidelity disaster scenarios or field exercises. Simulation is cheaper, schedulable, and produces compliance documentation. They benefit from reduced training burden while maintaining regulatory standing; they also bear implicit risk that competence validated through simulation may not transfer to real incidents.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, budget_constrained_operators, beneficiary,
    organized, biographical, constrained, national).

% Complete annual simulations to satisfy licensing requirements and employer mandate. The simulations have fidelity but lack the high-stakes stress, equipment failure cascades, and irreversible consequences of real catastrophe. They carry the implicit risk that their competence level is certified but untested under actual crisis conditions. Professional identity is fused to the licensing framework — exit means career change, not constraint escape.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, field_operators, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__simulation_as_proxy, field_operators, beneficiary).

% Are supervised by operators certified through simulation alone. When a real incident occurs, they depend on operators whose competence was validated through proxy exercise, not tested under actual catastrophic pressure. They have no exit from the arrangement — they are the users, patients, or evacuees whose safety depends on the outcome.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, safety_critical_personnel, payer,
    powerless, immediate, trapped, local).

% Supply the software, hardware, and scenario libraries that operators use to meet compliance. They benefit from regulatory mandates treating simulation as equivalent to real-world testing — each mandate expands market demand. They have no stake in whether competence actually transfers; their revenue depends on simulation-as-proxy remaining the regulatory standard.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, simulation_infrastructure_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Coordinate incident response with operators certified through simulation. They bear the operational risk that coordination breaks down under actual catastrophic stress if operators have never drilled past the point where simulation scenarios end. Their exit options are constrained by jurisdiction and incident type — they cannot refuse to coordinate with certified personnel.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, emergency_response_teams, payer,
    organized, biographical, constrained, regional).

% Would testify, if present, that operators' competence gap became apparent only after the incident began — that simulation-validated competence failed under real conditions. They are structurally excluded from the competence-validation conversation; their voices enter only in post-incident inquiries.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, incident_survivors, excluded,
    powerless, immediate, trapped, local).

% Analyze incident patterns to assess whether simulation-based competence validation correlates with safety outcomes. They report findings but cannot change the regulatory standard unilaterally; enforcement decisions rest with the regulatory agencies.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, independent_incident_investigators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__simulation_as_proxy, regulatory_compliance_agencies).
narrative_ontology:fixing_cost_class(competence_exercise_validity__simulation_as_proxy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of competence maintenance at scale: a distributed workforce across thousands of sites needs periodic competence validation without the cost and disruption of running real-world disaster scenarios for everyone simultaneously.
% TRANSFER_FUNCTION: Moves certification authority and operational flexibility from catastrophe-based testing regimes to simulation-based metrics: operators document simulation completion; regulators close oversight files based on simulation documentation; vendors supply the simulation infrastructure and profit from the mandated equivalence.
% ABSENT_VOICES: Incident survivors and personnel who experienced competence failure under real pressure are structurally absent from the standard-setting conversation. They appear only in post-incident inquiries after the regulatory standard is already entrenched. Personnel who favor real-world disaster drills as irreplaceable are marginalized as 'safety theater advocates' rather than heard as serious challengers.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared, competence validation would revert to real-world high-fidelity scenarios, field drills with equipment failure, or hybrid regimes. Operators would face higher training costs; budgets would reallocate; simulation vendors would lose regulatory-mandated market demand; regulatory agencies would lose the low-oversight compliance documentation; incident investigation patterns might shift if competence testing became more stringent.
% FOUNDING_PROBLEM: Scaling competence maintenance: as operations grew, real-world disaster scenarios became too expensive and disruptive to run for every personnel cohort every year. A proxy was needed that could certify retention without operational paralysis.
% FOUNDING_PROBLEM_CORROBORATION: Budget officers and regulatory compliance managers attest the cost problem is acute and ongoing. Incident investigators report mixed evidence — some incident reviews find no correlation between simulation completion and failure; others attribute failures to scenarios that fell outside the simulated case set. Emergency response coordinators testify that real incidents regularly exceed simulation boundaries. Simulation vendors and regulated operators attest the founding problem remains unsolved without the proxy standard.
narrative_ontology:disappearance_verdict(competence_exercise_validity__simulation_as_proxy, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__simulation_as_proxy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__simulation_as_proxy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__simulation_as_proxy, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__simulation_as_proxy, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__simulation_as_proxy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__simulation_as_proxy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts at 0.54 and rises to 0.68 over the interval. The trajectory reflects cumulative risk transfer: as simulation-as-proxy becomes institutionalized, the gap between simulation-certified competence and real-catastrophe-tested competence grows wider, but the regulatory standard hardens around the simulation metrics. Theater ratio rises from 0.38 to 0.52, crossing above center toward the end of the interval — simulation completion becomes increasingly performative relative to its actual competence-validation function. Suppression requirement rises from 0.48 to 0.61: maintaining the constraint requires active suppression of contrary evidence (incident reviews showing competence gaps, real-world scenarios exceeding simulated cases, challenger proposals for hybrid regimes) and exclusion of dissident voices (incident survivors, emergency responders who cite real incidents as evidence). The shared time grid ensures every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory agency and vendor seats, the constraint solves a genuine coordination problem (cost, scale, compliance) and the empirical record (accident statistics, incident reviews) supports adequacy — the engine should compute rope from these seats. From the field operator, emergency responder, and safety-critical user seats, the constraint extracts risk (hidden dependency on untested competence, trapped exit) under a coordination cover story — the engine should compute snare or tangled rope from these seats. The structural divergence arises from asymmetric exit (vendors and budget holders are arbitrage-mobile; operators are identity-locked; end-users are trapped) and asymmetric information (regulators see compliance documentation; operators experience gap between simulation and real incident; field users experience outcome only during crisis). The engine derives seat-specific directionality from power/exit/beneficiary structure; the authored claim and metrics remain independent of the engine's per-seat computation.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory agencies: d near 0.0 (beneficiary, sets agenda, analytical exit); they benefit from low-oversight compliance regime and face no personal safety risk. Simulation vendors: d near 0.0 (beneficiary, arbitrage-mobile, profit from mandated standard). Budget-constrained operators: d near 0.3 (mixed: beneficiary from cost savings, but moderate power and moderate-to-constrained exit; they depend on regulatory approval). Field operators: d near 0.75 (target: identity-locked exit, bear the hidden competence-test risk, competence is certified but untested). Safety-critical personnel: d near 1.0 (full target: powerless, trapped exit, depend entirely on operator competence under real pressure). The divergence in d across seats explains why the constraint will compute as different types from different institutional positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competence scaling) is live, but the mandate (simulation equals real-catastrophe experience) is increasingly divorced from real-world incident data. Incident investigation findings that competence gaps appear during real crises are actively suppressed or reinterpreted as 'training gaps' rather than 'simulation limitations.' Post-incident inquiries routinely find that operators were simulation-compliant but unprepared for actual incident scenarios. The theater ratio rise signals mandatrophy drift: the constraint persists because it solves the budget problem and provides compliance documentation, not because evidence supports simulation equivalence. Suppression is active: proposals for hybrid regimes (simulation + periodic real-world drills) are marginalized as 'cost-prohibitive'; incident survivors' testimony is excluded from standard-setting; emergency responders' field reports of coordination failures are treated as operational exceptions rather than constraint system failures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_sufficiency,
    'Is high-fidelity simulation (equipment, scenarios, stress inoculation) a sufficient proxy for real catastrophe experience, or are there irreducible dimensions of real-world pressure (stakes, irreversibility, cascade effects) that simulation cannot replicate?',
    'Comparative analysis of incident-investigation findings: do operators certified through simulation show competence gaps during real incidents in dimensions that simulation covered? Do post-incident debriefs identify scenarios or failure modes that fell outside the simulated case set?',
    'If simulation is sufficient, the current constraint remains valid and extraction is a legitimate cost of coordination. If simulation falls short in identifiable ways, the constraint should reclassify to mandatrophy — the founding problem persists but the mandate is inadequate, and reclassification would shift from rope/tangled_rope toward snare (victims bear competence-test risk; beneficiaries capture cost savings).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_sufficiency, empirical, 'Whether simulation fidelity is sufficient for competence certification.').

omega_variable(
    suppression_of_contrary_evidence,
    'Is the regulatory standard suppressing contrary evidence about competence gaps, or is the evidence genuinely insufficient to warrant change?',
    'Meta-analysis of incident investigation reports: do they systematically identify simulation-competence gaps? Are findings incorporated into revised standards, or archived without action? Do proposals for hybrid regimes receive funding and pilot evaluation, or are they dismissed as cost-prohibitive without analysis?',
    'If contrary evidence is actively suppressed and proposals are dismissed without evaluation, suppression is structural and the constraint reclassifies toward snare (extraction persists through information control and agenda-setting capture). If evidence is genuinely mixed or proposals are genuinely costly to pilot, the constraint remains tangled rope (legitimate coordination with asymmetric risk transfer).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_of_contrary_evidence, empirical, 'Whether suppression of contrary evidence is structural or evidence-driven.').

omega_variable(
    identity_lock_mechanism,
    'For field operators, is exit truly impossible or merely career-costly? Do alternative career paths exist outside the simulation-certified employment sector?',
    'Labor market analysis: track operators who exit to alternative sectors or roles; measure career-path availability and income differential. Survey operators on perceived exit costs and alternatives.',
    'If exit is truly impossible (trapped), operators are full targets (d ≈ 1.0) and the constraint from their seat is snare (extraction enforced through employment dependence). If exit is costly but possible (constrained rather than trapped), directionality moderates and classification softens toward tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether operator exit is identity-locked or merely constrained.').

omega_variable(
    reading_foreclosure_contestation,
    'Is the ''simulation_as_proxy'' reading logically foreclosed by the ''real_catastrophe_only'' reading, or do both remain live positions within a single authority framework?',
    'Examine whether real-world incidents have occurred that simulation-validated operators failed to handle competently. If yes, the ''real_catastrophe_only'' reading gains empirical traction. Assess whether regulators have moved toward hybrid regimes (incorporating both simulation and real-world scenarios) or remain committed to simulation-alone — if regulators remain committed despite contrary evidence, the readings coexist (different institutional commitments) rather than foreclose each other.',
    'If readings foreclose, the engine reclassifies to the foreclosing reading''s frame; if coexist, both remain live and the constraint''s type depends on the reading instantiated (this story = simulation_as_proxy = tangled rope/rope from beneficiary seats, snare from target seats). If hybrid regimes gain traction, the ''continuous_refresh_hybrid'' reading becomes dominant and this constraint becomes archived as overridden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_contestation, conceptual, 'Whether simulation_as_proxy is foreclosed by contrary reading or coexists as live institutional commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__simulation_as_proxy, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__simulation_as_proxy, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_validity__simulation_as_proxy, theater_ratio, 5, 0.42).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_validity__simulation_as_proxy, theater_ratio, 10, 0.46).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_validity__simulation_as_proxy, theater_ratio, 15, 0.5).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__simulation_as_proxy, theater_ratio, 20, 0.51).
narrative_ontology:measurement_basis(comp_tr_t20, observed).
narrative_ontology:measurement(comp_tr_t25, competence_exercise_validity__simulation_as_proxy, theater_ratio, 25, 0.52).
narrative_ontology:measurement_basis(comp_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t5, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 5, 0.59).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 10, 0.63).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(comp_be_t20, observed).
narrative_ontology:measurement(comp_be_t25, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(comp_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t5, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 10, 0.57).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 15, 0.59).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(comp_su_t20, observed).
narrative_ontology:measurement(comp_su_t25, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 25, 0.61).
narrative_ontology:measurement_basis(comp_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__simulation_as_proxy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__simulation_as_proxy, 0.14).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__continuous_refresh_hybrid).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__real_catastrophe_only).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, regulatory_simulation_mandate__market_capture).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, incident_investigation__suppression_of_alternatives).

% DUAL FORMULATION NOTE:
% The competence_exercise_validity kernel decomposes into three structurally distinct constraint stories with different ε values: (1) simulation_as_proxy (THIS story): ε≈0.68, treats simulation as sufficient proxy, regulatory compliance sufficient. (2) continuous_refresh_hybrid: ε≈0.52, treats simulation as necessary but insufficient, hybrid regimes required, lower extraction due to broader competence testing. (3) real_catastrophe_only: ε≈0.41, treats only real catastrophe as valid test, simulation is practice not certification, lowest extraction because competence standard is highest. Each reading instantiates a different constraint with different beneficiary/victim structures, enforcement mechanisms, and extractiveness. The readings coexist across different institutional positions — regulators and vendors hold (1), emergency responders and incident investigators hold (3), hybrid advocates hold (2). All three stories are linked via network.affects_constraints as members of the competence_exercise_validity family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_validity__simulation_as_proxy, analytical, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
