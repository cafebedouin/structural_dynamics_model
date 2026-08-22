% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__hybrid_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: preparedness_transmission__hybrid_reading
 *   human_readable: Preparedness Transmission (Hybrid Reading: Stratified Competence Decay)
 *   domain: institutional/public_safety
 *
 * SUMMARY:
 *   The preparedness transmission constraint governs how disaster-response
 *   knowledge flows across generations in a centralized, regulated system.
 *   This is the hybrid reading: infrastructure competence (engineering
 *   knowledge of dams, shelters, power systems) remains high and demonstrably
 *   works — tested, certified, inherited through explicit apprenticeship. But
 *   civilian coordination knowledge (evacuation sequencing, improvisation
 *   under chaos, adaptive decision-making) has decayed into performative
 *   compliance: drills happen on schedule, but the lived operational
 *   understanding of *how to execute when real conditions violate drill
 *   assumptions* has eroded. The constraint is tangled because it genuinely
 *   solves a coordination problem (preparing for rare disasters) while
 *   asymmetrically extracting: it benefits regulatory authorities and
 *   engineers (whose competence is visible and measurable) and imposes costs
 *   on coordinators and responders (whose knowledge decay is invisible until
 *   failure, and whose identity-lock prevents exit). The measurement series
 *   shows steady but modest extraction growth as the theater ratio rises —
 *   extractiveness is driven not by the core coordination function but by the
 *   increasing divergence between performance (drills complete,
 *   infrastructure certified) and operational readiness (coordination
 *   knowledge decaying). This is the hybrid reading's central claim: the
 *   constraint is stratified.
 *
 * KEY AGENTS:
 *   - Infrastructure Engineering Corps: maintainers of dams, levees, shelters, power systems; knowledge transmission works; credentialed, tested, hands-on apprenticeship; benefits from regulatory certification and funding; d ≈ 0.2 (beneficiary side)
 *   - Civilian Evacuation Coordinators: tasked with executing evacuation orders and shelter logistics; inherit eroded knowledge; embedded in the same preparedness mandate but with degraded operational understanding; d ≈ 0.7 (target side)
 *   - Regulatory Authorities: set standards, certify, allocate funding; benefit from the appearance of comprehensive preparedness (measurable drills, infrastructure passing inspections); d ≈ 0.15 (beneficiary side)
 *   - Emergency Response Personnel: police, fire, medical, logistics; identity-locked to the profession; inherit fragmented knowledge; high theater ratio (drill performance vs. real readiness ambiguous); d ≈ 0.75 (target side)
 *   - General Population at Risk: powerless, trapped, depends on coordination layer; exposed when coordination fails despite infrastructure success; d ≈ 0.85 (full target end)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.58).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.62).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Preparedness Transmission (Hybrid Reading: Stratified Competence Decay)").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "institutional/public_safety").

domain_priors:requires_active_enforcement(preparedness_transmission__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, '19ada81b-b05e-48dd-a4d1-be44bc929b3c').
narrative_ontology:cs_kernel_codification('19ada81b-b05e-48dd-a4d1-be44bc929b3c', fixed_text).
narrative_ontology:cs_authority_grounding('19ada81b-b05e-48dd-a4d1-be44bc929b3c', extraction).
narrative_ontology:cs_interpretation_layer_present('19ada81b-b05e-48dd-a4d1-be44bc929b3c').
narrative_ontology:cs_reading_relation('19ada81b-b05e-48dd-a4d1-be44bc929b3c', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('19ada81b-b05e-48dd-a4d1-be44bc929b3c', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_axiom('19ada81b-b05e-48dd-a4d1-be44bc929b3c', foundational, knowledge_transmission_is_stratified).
narrative_ontology:cs_axiom_status(knowledge_transmission_is_stratified, holdable).
narrative_ontology:cs_axiom_grounding('19ada81b-b05e-48dd-a4d1-be44bc929b3c', knowledge_transmission_is_stratified, empirically_contingent).
narrative_ontology:cs_axiom('19ada81b-b05e-48dd-a4d1-be44bc929b3c', foundational, infrastructure_and_coordination_competence_are_structurally_separable).
narrative_ontology:cs_axiom_status(infrastructure_and_coordination_competence_are_structurally_separable, holdable).
narrative_ontology:cs_axiom_grounding('19ada81b-b05e-48dd-a4d1-be44bc929b3c', infrastructure_and_coordination_competence_are_structurally_separable, empirically_contingent).
narrative_ontology:cs_reference_frame('19ada81b-b05e-48dd-a4d1-be44bc929b3c', comprehensive_unified_preparedness_doctrine).
narrative_ontology:cs_drift_state('19ada81b-b05e-48dd-a4d1-be44bc929b3c', contemporary_post_disaster_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('19ada81b-b05e-48dd-a4d1-be44bc929b3c', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, infrastructure_engineering_corps).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, regulatory_authorities).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, civilian_evacuation_coordinators).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, emergency_response_personnel).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, general_population_at_risk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, civilian_evacuation_coordinators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains physical disaster-resilience infrastructure (dams, levees, seawalls, shelters, power systems). Tests and certifies engineering systems through annual stress tests and inspection protocols. Their knowledge transmission works: each generation of engineers inherits explicit technical manuals, participates in hands-on maintenance cycles, and validates competence through credentialed testing. Infrastructure components perform on design spec during incidents.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, infrastructure_engineering_corps, agenda_setter,
    organized, generational, mobile, national).

% Responsible for executing evacuation orders, sheltering populations, coordinating logistics, and managing public behavior under stress. Operate within the same preparedness mandate as engineers but receive degraded operational knowledge: protocols exist on paper, drills occur on schedule, but the lived understanding of *how to execute under actual chaos* has decayed. Each cycle of routine drills reinforces the performance, not the adaptive decision-making.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civilian_evacuation_coordinators, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__hybrid_reading, civilian_evacuation_coordinators, beneficiary).

% Set and enforce preparedness standards, certify drills, allocate funding to infrastructure and training. Benefit from the appearance of comprehensive preparedness (infrastructure certification passes, drill completion rates meet targets) without bearing the risk of operational failure in the coordination layer. Enforcement focuses on measurable, verifiable infrastructure metrics.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, regulatory_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Police, fire, medical, and logistics personnel tasked with execution under real disaster conditions. Professional identity fused with the coordination mandate; cannot exit without losing career. Inherit fragmented knowledge: formal procedures, but eroded understanding of how to improvise when real conditions diverge from drill assumptions. Theater ratio is high for them — they perform preparedness in drills; actual operational readiness is ambiguous.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, emergency_response_personnel, payer,
    moderate, biographical, identity_locked, regional).

% Depends on the coordination layer for effective evacuation and shelter during disasters. Exposed to the breakdown when decayed coordination knowledge intersects with real crisis. No exit from the jurisdiction's preparedness system; no voice in its design. Absorb the consequence when infrastructure performs but coordination fails.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, general_population_at_risk, payer,
    powerless, immediate, trapped, regional).

% The institutional commitment to 'comprehensive preparedness' — the doctrine that framing all disaster risk as solvable through centralized planning, standardized drills, and regulatory oversight. Benefits from the continued performance of preparedness (drills complete, infrastructure certified) without being exposed to operational failure. A doctrine, not an actor; persists via institutional inertia and the theater ratio.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, preparedness_doctrine_authority, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(preparedness_transmission__hybrid_reading, preparedness_doctrine_authority).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__hybrid_reading, regulatory_authorities).
narrative_ontology:fixing_cost_class(preparedness_transmission__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits disaster-response knowledge across generations: engineering principles through explicit technical apprenticeship; civilian coordination through institutional memory and rehearsal. The constraint solves the collective-action problem of preparing a large population for rare, high-consequence events.
% TRANSFER_FUNCTION: Moves preparedness burden from infrastructure (where knowledge is actively transmitted and tested) to coordination (where knowledge erodes into performative compliance). Transfers risk from engineers (high visibility, measurable competence) to coordinators and responders (low visibility, unobservable until failure). Transfers authority from operational personnel (who inherit eroded knowledge) to regulatory authorities (who measure performance on certifiable dimensions).
% ABSENT_VOICES: Coordinators and responders whose knowledge has decayed are structurally present in drills but their voices on readiness degradation are muted by the same bureaucratic structure that measures them. Past disasters whose coordination failures have been forgotten (institutional amnesia) cannot testify. Populations in jurisdictions with high disaster frequency might attest knowledge decay; those with low frequency cannot, so the decay is invisible in the aggregate. The voices missing are those who would say: 'we drill the routine but have forgotten how to improvise.'
% DISAPPEARANCE_RATIONALE: If the preparedness transmission constraint vanished, infrastructure systems would degrade (engineering knowledge transmission would collapse without the regulatory mandate and apprenticeship structure); coordination knowledge would improve unpredictably (freed from the performance theater, it might atrophy further or evolve differently without mandated drills). The constraint's removal would create a crisis of confidence in disaster preparedness even if actual operational readiness improved. Populations and authorities depend on its institutional form, even when competence is stratified.
% FOUNDING_PROBLEM: The founding problem is two-fold: (1) rare, high-consequence disasters require advance preparation; (2) preparation knowledge must persist across decades without regular activation. Early disaster response learned that centralized planning, regulatory oversight, and standardized drills solve the problem for the engineering layer but struggle with the coordination layer.
% FOUNDING_PROBLEM_CORROBORATION: Infrastructure engineers attest the founding problem is live and solved (systems perform on spec). Regulatory authorities attest it is solved (drills meet targets, infrastructure certified). Coordination personnel and independent disaster-response researchers attest the problem is incompletely solved: infrastructure performs, but coordination knowledge decay means the solutions are stratified. Post-disaster investigations consistently find coordination failures despite adequate engineering performance — external evidence from outside the benefiting parties (regulatory authorities, infrastructure engineers) supports the hybrid reading.
narrative_ontology:disappearance_verdict(preparedness_transmission__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_transmission__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__hybrid_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_transmission__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at end) because the constraint does solve a genuine coordination problem for rare, high-consequence events, but the solution is incomplete and stratified. The asymmetry is in who bears the cost of knowledge decay and who benefits from the appearance of comprehensive preparedness. Suppression (0.62) reflects the enforcement machinery required to maintain the performance: drills must be completed on schedule (institutional requirement), procedures must be documented (regulatory requirement), certification metrics must pass (funding requirement), even when the lived operational knowledge supporting them has eroded. Theater ratio (0.48 at end) is rising as the performance diverges from readiness — extractiveness is driven by the growing gap between *what the system looks like* (comprehensive, tested, certified) and *what coordinators and responders can actually do* (follow memorized scripts, adapt poorly, fail under high chaos). The measurement series runs on a single shared time grid; all metrics are authored at every time point so temporal alignment is guaranteed.
 *
 * PERSPECTIVAL GAP:
 *   The engineering corps perceives this as genuine coordination with solved problems (infrastructure works, apprenticeship transmits competence, regulatory oversight validates capability — their seat computes rope or even mountain). Regulatory authorities perceive it as successful governance (all measurable standards met, drills completed, funding justified — their seat computes rope). Coordinators and responders perceive it as extractive performance theater layered over eroding knowledge (they drill, they pass, but they inherit fragmented understanding; real disasters will expose the stratification — their seat computes tangled_rope or snare). The general population perceives it through absence: when coordination fails during a real disaster, they discover the stratification too late. The engine computes per-seat classifications from the structural data; the claim that it is tangled_rope is correct from the beneficiary (regulatory authority, engineer) and target (coordinator, responder, population) seats, but the divergence is instructive.
 *
 * DIRECTIONALITY LOGIC:
 *   Infrastructure engineers (organized power, mobile exit, active apprenticeship) are beneficiaries: they receive credentialing, funding, visible competence validation; d ≈ 0.2. Regulatory authorities (institutional power, mobile exit, measure-what-they-can) are beneficiaries: they benefit from the appearance of comprehensive preparedness and bear minimal risk of visibility when coordination fails (failures are attributed to 'human error' or 'unpredictability,' not to regulatory design); d ≈ 0.15. Coordinators and responders (moderate-to-powerless power, identity-locked or constrained exit, inherit eroded knowledge) are targets: they bear the cost of knowledge decay (responsibility for execution, accountability for failures, identity fusion prevents exit); d ≈ 0.70-0.75. The general population (powerless, trapped, no voice) is at the target end: d ≈ 0.85. The directionality derivation flows from beneficiary/victim declarations and exit options: engineers and authorities benefit without running coordination; coordinators and population pay through eroded readiness and exposure to failure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits clear mandatrophy: the founding problem (prepare for rare, high-consequence disasters) remains live, but the solution has stratified. The coordination mandate persists (drills are still mandated, preparedness doctrine is still institutional orthodoxy) but the operational knowledge supporting it has decayed. The constraint is classified as tangled_rope (genuine coordination function + asymmetric extraction of knowledge decay onto coordinators and population) rather than rope (which would require distributed benefit) or snare (which would require pure extraction with no coordination function). The key to this classification is recognizing that the constraint solves half the problem (infrastructure competence) while the solution is incomplete for the other half (coordination knowledge). The mandate has not vanished or reversed; it has hollowed out asymmetrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    knowledge_decay_measurement,
    'How much of the coordination knowledge decay is structural (erased from institutional memory) vs. internalized (carried by individuals but not transmitted)?',
    'Post-disaster analysis: compare coordination failures in jurisdictions with high disaster frequency (knowledge may be maintained through repeated activation) vs. low frequency (knowledge decay is invisible). Interview responders from successive generations about what they inherited vs. what they learned on the job.',
    'If decay is structural, fixing it requires rebuilding apprenticeship and documentation (high fixing cost). If decay is internalized (individuals know but aren''t transmitting), the problem is the transmission mechanism, not erasure; fixing cost is lower and targeted at apprenticeship redesign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_decay_measurement, empirical, 'Whether coordination knowledge decay is institutional erasure or transmission failure.').

omega_variable(
    engineering_infrastructure_divergence,
    'Is engineering competence genuinely *high* or merely *visible*? Do infrastructure systems perform on design spec under real disaster conditions, or only under drill conditions?',
    'Post-disaster analysis of infrastructure performance: do dams, levees, shelters, power systems actually function as designed when stressed by real events, or do they fail in ways invisible to inspection protocols?',
    'If engineering competence is high only under drill conditions (benign, controlled, partial), the constraint is more thoroughly hollowed than the hybrid reading suggests — both layers would be stratified. If engineering truly performs on spec, the hybrid reading holds: stratification is real and localized to the coordination layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineering_infrastructure_divergence, empirical, 'Whether infrastructure competence is real or drill-visibility artifact.').

omega_variable(
    institutional_reading_constraint,
    'Could the regulatory authority and engineering corps adopt the husk reading (drills are memorial ritual, knowledge has hollowed out entirely) without abandoning the preparedness doctrine that legitimates their authority and funding?',
    'Textual analysis of how regulatory authorities and engineering institutions frame their mandate: do they claim active knowledge transmission or institutional continuity? Interview institutional actors about whether explicit knowledge erosion would challenge their legitimacy.',
    'If adoption of the husk reading would delegitimate their authority, the readings coexist but with institutional pressure toward the competence reading. If adoption is structurally possible within their framework, coexistence is looser. This affects whether the competing readings remain live or converge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_reading_constraint, conceptual, 'Whether the institutional authority structure constrains which reading can be held without self-delegitimation.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression (0.62) structural (external enforcement: mandatory drills, regulatory audits, funding conditionality) or internalized (responders and coordinators have internalized the mandate and no longer question whether their knowledge is adequate)?',
    'Post-mandate relaxation experiment: if regulatory drill requirements were eased, would responders voluntarily maintain the same preparation intensity, or would they reduce it? Do responders express doubt about their readiness in private contexts?',
    'If suppression is structural, removing enforcement would reduce extraction (responders could exit the performance theater). If internalized, suppression persists even after external enforcement is removed (responders carry the mandate forward, limiting their ability to develop alternative competence models). Internalized suppression increases the effective extraction on responders and coordinators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression of doubt about coordination readiness is externally enforced or internalized into professional identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__hybrid_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t5, preparedness_transmission__hybrid_reading, theater_ratio, 5, 0.39).
narrative_ontology:measurement_basis(prep_tr_t5, observed).
narrative_ontology:measurement(prep_tr_t10, preparedness_transmission__hybrid_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(prep_tr_t10, observed).
narrative_ontology:measurement(prep_tr_t15, preparedness_transmission__hybrid_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement_basis(prep_tr_t15, observed).
narrative_ontology:measurement(prep_tr_t25, preparedness_transmission__hybrid_reading, theater_ratio, 25, 0.47).
narrative_ontology:measurement_basis(prep_tr_t25, observed).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__hybrid_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(prep_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t5, preparedness_transmission__hybrid_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(prep_be_t5, observed).
narrative_ontology:measurement(prep_be_t10, preparedness_transmission__hybrid_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(prep_be_t10, observed).
narrative_ontology:measurement(prep_be_t15, preparedness_transmission__hybrid_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement_basis(prep_be_t15, observed).
narrative_ontology:measurement(prep_be_t25, preparedness_transmission__hybrid_reading, base_extractiveness, 25, 0.57).
narrative_ontology:measurement_basis(prep_be_t25, observed).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__hybrid_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(prep_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__hybrid_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t5, preparedness_transmission__hybrid_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement_basis(prep_su_t5, observed).
narrative_ontology:measurement(prep_su_t10, preparedness_transmission__hybrid_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(prep_su_t10, observed).
narrative_ontology:measurement(prep_su_t15, preparedness_transmission__hybrid_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement_basis(prep_su_t15, observed).
narrative_ontology:measurement(prep_su_t25, preparedness_transmission__hybrid_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(prep_su_t25, observed).
narrative_ontology:measurement(prep_su_t40, preparedness_transmission__hybrid_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(prep_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__hybrid_reading, 0.14).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__husk_reading).

% DUAL FORMULATION NOTE:
% The preparedness_transmission kernel decomposes into three distinct constraint readings, each with different ε values and beneficiary/victim structures: (1) competence_reading — knowledge remains live and exercised; (2) husk_reading — knowledge has completely hollowed out into ritual; (3) hybrid_reading — stratified competence, with infrastructure high and coordination decayed. The three readings coexist as live positions held by different observers in the same system. The hybrid reading integrates empirical evidence from post-disaster investigations and inter-generational knowledge transfer studies; it bridges the optimism of the competence reading and the pessimism of the husk reading by proposing differential decay rates across the engineering and coordination layers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_transmission__hybrid_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
