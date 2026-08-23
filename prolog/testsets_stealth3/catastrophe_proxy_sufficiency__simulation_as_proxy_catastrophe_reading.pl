% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, []).

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
 *   constraint_id: catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading
 *   human_readable: Simulation Exercises as Catastrophe-Equivalent Competence Maintenance
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kernel
 *   catastrophe_proxy_sufficiency: the claim, embedded in nuclear, aviation,
 *   chemical, and healthcare certification regimes, that simulation exercises
 *   constitute catastrophe-equivalent practice sufficient to maintain
 *   operational competence indefinitely. The standing arrangement under
 *   contest — and therefore the epsilon referent — is the existing practice
 *   of satisfying competence-maintenance requirements through scored
 *   exercises, assessed by this reading's own lights: the reading holds that
 *   the exercises work, so it sees a coordination arrangement that delivers
 *   rehearsal of unrehearsable events, with a modest acknowledged asymmetry
 *   (certification also serves as the regime's liability record). The contest
 *   with sibling readings is NOT argued inside this constraint; per the
 *   committer-frame rules it is carried entirely in the omega variables and
 *   the cs_structure block. KEY AGENTS (by structural relationship): -
 *   safety_regulatory_agencies: agenda-setting beneficiary (institutional /
 *   constrained) — writes and audits the exercise rules; collects liability
 *   deflection and administrative continuity -
 *   licensed_operating_organizations: dual-positioned beneficiary/payer
 *   (powerful / constrained) — buys defensibility, pays compliance -
 *   frontline_operators_and_response_crews: dual-positioned beneficiary/payer
 *   (organized / constrained) — gains rehearsed procedure, pays time and
 *   inherits any rehearsal gap - liability_insurers: pure beneficiary
 *   (powerful / arbitrage) — consumes a legible readiness signal -
 *   simulation_training_vendors: beneficiary (organized / mobile) -
 *   downstream_exposed_communities: promised-protected public (powerless /
 *   trapped) - safety_science_dissenters: excluded challengers (moderate /
 *   mobile) - accident_investigation_boards: analytical observer
 *   (institutional / analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.3).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.48).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "Simulation Exercises as Catastrophe-Equivalent Competence Maintenance").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, '1313dc2c-4cb7-48ff-af36-45125083ff9d').
narrative_ontology:cs_kernel_codification('1313dc2c-4cb7-48ff-af36-45125083ff9d', formalized).
narrative_ontology:cs_authority_grounding('1313dc2c-4cb7-48ff-af36-45125083ff9d', expertise).
narrative_ontology:cs_interpretation_layer_present('1313dc2c-4cb7-48ff-af36-45125083ff9d').
narrative_ontology:cs_reading_relation('1313dc2c-4cb7-48ff-af36-45125083ff9d', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('1313dc2c-4cb7-48ff-af36-45125083ff9d', catastrophe_proxy_sufficiency__hybrid_degradation_reading, forecloses).
narrative_ontology:cs_reading_relation('1313dc2c-4cb7-48ff-af36-45125083ff9d', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('1313dc2c-4cb7-48ff-af36-45125083ff9d', foundational, simulated_practice_transfers_to_catastrophe_performance).
narrative_ontology:cs_axiom_status(simulated_practice_transfers_to_catastrophe_performance, holdable).
narrative_ontology:cs_axiom_grounding('1313dc2c-4cb7-48ff-af36-45125083ff9d', simulated_practice_transfers_to_catastrophe_performance, empirically_contingent).
narrative_ontology:cs_axiom('1313dc2c-4cb7-48ff-af36-45125083ff9d', secondary, drill_evidence_suffices_for_certification).
narrative_ontology:cs_axiom_status(drill_evidence_suffices_for_certification, holdable).
narrative_ontology:cs_axiom_grounding('1313dc2c-4cb7-48ff-af36-45125083ff9d', drill_evidence_suffices_for_certification, conventional).
narrative_ontology:cs_reference_frame('1313dc2c-4cb7-48ff-af36-45125083ff9d', validated_transfer_of_training_paradigm).
narrative_ontology:cs_drift_state('1313dc2c-4cb7-48ff-af36-45125083ff9d', post_severe_accident_review_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1313dc2c-4cb7-48ff-af36-45125083ff9d', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, safety_regulatory_agencies).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, licensed_operating_organizations).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators_and_response_crews).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, liability_insurers).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_training_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, downstream_exposed_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, licensed_operating_organizations).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators_and_response_crews).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, transfer_of_training_generalization).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, exercise_evidenced_certification_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes the drill-frequency, scenario-scope, and evidence rules by which operating licenses and certificates are granted and renewed; commissions or audits the exercises; signs off on organizational readiness. When a certified facility suffers a disaster, the completed, accredited exercise record is the regime's principal defense against blame reaching the standard-setters themselves. Leaving the arrangement would mean dismantling the certification paradigm the agency administers and staking its authority on evidence it does not control.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, safety_regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, safety_regulatory_agencies, beneficiary).

% Operates nuclear plants, airlines, chemical facilities, and hospital systems. Buys certification: an audited exercise record lowers insurance premiums and provides the legal record invoked when defending duty-of-care claims after an incident. Pays in exercise downtime, scenario development, documentation load, and the foregone alternatives — longer apprenticeships, live field rotations — that certification criteria crowd out. Exiting would mean surrendering licensure or fighting standards case by case.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, licensed_operating_organizations, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, licensed_operating_organizations, payer).

% Control-room crews, flight crews, surgical and emergency-response teams drilled on recurring schedules. Gain rehearsed procedure and team coordination for event classes they could not otherwise practice without waiting for the real thing. Pay shift time, repeated stress exposure, and the frustration of exercises run partly for the audit trail. Employment is tied to staying drill-current, and working identity is bound up with being a drilled, certified crew. What no schedule can give them is the event that arrives outside every scenario they have run.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators_and_response_crews, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators_and_response_crews, payer).

% Prices coverage partly on certification status and exercise records, which provide a standardized, legible readiness signal the insurer did not have to build. Can reprice or withdraw from an entire sector faster than any regulated firm can rebuild its training estate.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, liability_insurers, beneficiary,
    powerful, biographical, arbitrage, global).

% Designs scenarios, builds simulators, runs exercises, and supplies the evaluators who score them. Every tightening of certification requirements enlarges the market. Exit is easy: the same products serve neighboring industries.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_training_vendors, beneficiary,
    organized, biographical, mobile, global).

% Lives near plants, ports, flight paths, and flood zones covered by certified response plans. Receives the arrangement's central promise: that drilled, certified organizations will perform competently when catastrophe arrives. Bears whatever residue remains when a real event outruns the scenarios that were rehearsed — a residue the sufficiency doctrine treats as negligible by hypothesis. Has no seat in the committees that set scenario scopes.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, downstream_exposed_communities, beneficiary,
    powerless, generational, trapped, regional).

% Researchers in human factors and organizational learning who argue, from degradation studies and post-incident analyses, that tacit knowledge and stress-response capacity erode without live-event exposure. Publish, testify occasionally, and remain outside the working groups that draft competency standards; their results tend to enter certification policy only after an accident forces a review.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, safety_science_dissenters, excluded,
    moderate, biographical, mobile, global).

% Convenes after failures to reconstruct what crews actually did against what they had rehearsed. Produces the periodic evidence that calibrates — or embarrasses — the sufficiency claim. Analytical seat: neither collects nor pays; the reports reshape the argument for everyone else.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, accident_investigation_boards, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, safety_regulatory_agencies).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem that catastrophic-event practice cannot be obtained on demand: the relevant events are too rare, too dangerous, and too expensive to rehearse live, so standardized simulation exercises give dispersed teams shared procedure, decision rehearsal, and graded stress exposure without waiting for — or causing — real catastrophes.
% TRANSFER_FUNCTION: Moves assurance — certified, exercise-evidenced readiness claims — from operating organizations to regulators, insurers, and the public; moves compliance costs (crew hours, exercise budgets, documentation) from operating organizations and frontline crews into the certification apparatus; and moves post-incident blame toward the 'trained and certified' record, shielding the certifying bodies themselves from liability.
% ABSENT_VOICES: Proponents of real-event-exposure requirements and researchers documenting multi-generational degradation of tacit knowledge sit outside the certification deliberations that encode this doctrine; so do the communities downstream of certified facilities, who inherit whatever gap remains between rehearsed and actual catastrophe without representation in the working groups that set scenario scope.
% DISAPPEARANCE_RATIONALE: Certification regimes would lose their evidentiary basis overnight: licensure, insurance pricing, and post-incident legal defense all key to the exercise record. Operating organizations would need replacement competence evidence (extended apprenticeship, live field rotations, adversarial field exercises), the training-vendor market would contract sharply, and regulators would have to defend standards on grounds they currently delegate to scored exercises.
% FOUNDING_PROBLEM: After the early industrial, aviation, and nuclear disasters, regulators faced a practicable puzzle: how do you require and verify readiness for events too rare and dangerous for anyone on staff to have experienced — how do you certify competence for a catastrophe nobody has lived through?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: accident investigation boards (severe nuclear, aviation, and offshore-drilling inquiries) attest both that readiness-for-unexperienced-events was the founding problem and that its solution remains under continual review; the human-factors and organizational-learning literature outside the certification industry documents the ongoing gap between exercised and lived response. The problem statement predates and exceeds the parties that now administer the doctrine.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tests).
:- end_tests(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored LOW (0.30 at interval end) because this reading's referent assessment finds a functioning coordination core: exercises deliver procedural and team competence for event classes that cannot be practiced live, and every seated party draws net benefit under the reading's hypothesis. The residual 0.30 reflects what even this reading concedes: the certification record doubles as a blame-deflection asset for the standard-setters, and exercise mandates crowd out alternative competence investments. Theater (0.32, rising from 0.10) tracks the audit-formalization of exercises — a growing share of drill activity exists to satisfy the file rather than the crew, classic proxy-goal creep, though the functional core remains real. Suppression (0.48) is authored as a raw, unscaled structural property: licensure and insurance are gated on drill currency, so the arrangement requires active enforcement machinery, but it forecloses few genuine alternatives outright (accessibility_collapse 0.30 — apprenticeship intensification and field rotations remain legally available, merely disincentivized). Resistance (0.30) reflects live scientific dissent rather than participant revolt; crews and operators broadly endorse drilling. The three temporal series share one six-point grid (0, 8, 16, 24, 32, 40). The rising suppression_requirement series is authored deliberately: the story traces enforcement-capacity maturation (post-severe-accident ratchets in drill mandates, accreditation audits, evidence requirements), not merely shifting extraction. Extractiveness drifts upward slowly as the liability-deflection function becomes routinized; the claim/metric pairing is independent — rope is what this reading believes the structure to be, and the gently rising series is what the record shows.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently from the same structural data. From the agency seat the arrangement is prudent standardization it built and defends; from the operating-organization seat it is an affordable, insurance-like product; from the crew seat it is a mixed blessing — real skill plus audit burden; from the community seat it is a promise received without a seat at the table. Same-level lateral divergence: licensed_operating_organizations and liability_insurers hold comparable nominal power but face the constraint oppositely — the firms are constrained (licensure ties them to the drill record), the insurers hold arbitrage exit (reprice or leave a sector faster than any firm can rebuild its training estate), so identical global standing yields different experienced constraint. The engine computes these per-seat differences from power, exit, and role data; this story only declares the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: all six declared groups sit toward the beneficiary end (low d), with the two dual-role seats (operating organizations, frontline crews) pulled toward symmetry by their payer secondary roles — they both collect from and fund the apparatus. Downstream_exposed_communities combine powerless power, trapped exit, and generational horizon: nearest the subsidized end, though their regional scope and the verification difficulty of 'competence' claims temper the damping modestly. Safety_regulatory_agencies derive low d from their beneficiary position, but their agenda-setting role means the arrangement also reproduces their administration — the derivation captures most of this through the secondary beneficiary declaration, so no directionality override is authored; the residual administrative nuance is left to the engine rather than hand-corrected. Suppression enters the engine's arithmetic unscaled; only extractiveness is scaled, by each agent's derived directionality and the constraint's national-to-global scopes.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim disciplines both directions of mislabeling. Against the snare reading (critics who see only the liability shield): the coordination function is genuine and load-bearing — rehearsal of rare catastrophes is otherwise unobtainable, and the founding problem is live and externally corroborated — so reading the arrangement as pure extraction would erase the competence it actually produces. Against complacency (advocates who see only free coordination): the receipt surface names a concentrated gainer (safety_regulatory_agencies, via deflected liability), fixing is prohibitively costly relative to any benefit its administrators perceive, and the theater and extractiveness series both rise monotonically — the early-drift signatures the corpus exists to catch. Founding problem status is live, so no zombie flag arises; the danger this story watches for is not obsolescence but quiet hardening — enforcement ratcheting upward while the doctrine's empirical premise goes unexamined between disasters.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    real_event_irreducibility_question,
    'Does simulation maintain tacit knowledge and stress-response capacity across operator generations without any live-event exposure, or is there an irreducible component only real catastrophes confer?',
    'Longitudinal cohort comparison of operators with and without live-event experience, tracking decision quality and stress markers in subsequent high-fidelity exercises; degradation-curve estimation across training generations within single facilities.',
    'If an irreducible real-event component exists, this reading collapses toward the hybrid or necessity sibling, the certification evidentiary basis weakens structurally, and effective extraction rises as the liability shield loses its factual footing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_event_irreducibility_question, empirical, 'The kernel''s central contest, carried here as an open variable rather than argued inside the constraint.').

omega_variable(
    fidelity_threshold_locatability,
    'Is there a measurable simulation-fidelity threshold above which stress and uncertainty match live catastrophe closely enough for transfer, making sufficiency technology-dependent rather than categorical?',
    'Meta-analysis of transfer-of-training studies stratified by simulator fidelity class, plus prospective validation as full-scale immersive and adversarial simulation platforms mature.',
    'A locatable threshold would reframe this reading as an instance of the fidelity-threshold sibling (sufficiency conditional on current platforms crossing it); no threshold would strengthen the categorical claim this reading asserts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_threshold_locatability, empirical, 'Whether the sufficiency claim is categorical or secretly threshold-indexed.').

omega_variable(
    liability_shield_extraction_visibility,
    'Is the certification regime''s blame-deflection function a concealed transfer of downside from certifiers to the public that this reading''s charitable lights render invisible?',
    'Comparative analysis of post-incident accountability outcomes across jurisdictions with differing competence-evidence rules: does the exercise record systematically absorb blame that alternative evidence regimes would assign to standard-setters?',
    'If the shield function dominates, the arrangement is coordination wrapped around an extraction channel — effective chi for the agency seat rises and the rope classification gives way to tangled_rope; if incidental, the low-extraction assessment stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_shield_extraction_visibility, conceptual, 'Whether the reading''s own frame systematically understates the liability-transfer component.').

omega_variable(
    residual_tail_risk_allocation,
    'How much casualty and damage burden is attributable to events that outran the rehearsed scenario envelope, versus events within it where drilled performance held?',
    'Systematic coding of severe-accident investigation reports against the certified scenario library of the implicated facility: fraction of loss occurring in unrehearsed regime.',
    'A large unrehearsed-regime loss share converts downstream_exposed_communities from beneficiaries into a latent payer seat, raising aggregate effective extraction and pressuring the no-victim-set structure of this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_tail_risk_allocation, empirical, 'Quantifies the gap between the doctrine''s promise and delivered protection.').

omega_variable(
    cs_kernel_framing_underdetermination,
    'Is the commitment-system kernel the sufficiency doctrine itself (framed here: formalized kernel adjudicated by credentialed expertise), or the certification-authority structure layered above it, whose administrative survival depends on the doctrine remaining unexamined between disasters?',
    'Test which framing better predicts observed behavior: under the doctrine-framing, standards revisions follow new transfer-of-training evidence; under the authority-framing, revisions follow liability events and administrative cycles regardless of the evidence stream.',
    'Adopting the authority-framing would shift this constraint''s classification toward extraction-grounded commitment systems (authority_grounding extraction, interpretation layer as buffer against revision), materially changing the drift analysis; the doctrine-framing adopted here keeps expertise as the grounding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_kernel_framing_underdetermination, conceptual, 'Two coherent framings of the same kernel produce different commitment-system classifications; the doctrine-framing was chosen because standards bodies formally cite training-science evidence, but the alternative is live.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cpsim_read_tr_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cpsim_read_tr_t8, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(cpsim_read_tr_t16, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 16, 0.17).
narrative_ontology:measurement(cpsim_read_tr_t24, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(cpsim_read_tr_t32, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(cpsim_read_tr_t40, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 40, 0.32).

% Extraction over time
narrative_ontology:measurement(cpsim_read_be_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(cpsim_read_be_t8, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(cpsim_read_be_t16, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 16, 0.23).
narrative_ontology:measurement(cpsim_read_be_t24, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 24, 0.26).
narrative_ontology:measurement(cpsim_read_be_t32, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 32, 0.28).
narrative_ontology:measurement(cpsim_read_be_t40, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 40, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(cpsim_read_su_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(cpsim_read_su_t8, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 8, 0.24).
narrative_ontology:measurement(cpsim_read_su_t16, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 16, 0.31).
narrative_ontology:measurement(cpsim_read_su_t24, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 24, 0.37).
narrative_ontology:measurement(cpsim_read_su_t32, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 32, 0.43).
narrative_ontology:measurement(cpsim_read_su_t40, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% The natural-language label 'does simulation suffice?' decomposes, per the epsilon-invariance principle, into four structurally distinct readings of the kernel catastrophe_proxy_sufficiency, each with its own epsilon and beneficiary structure. This story authors only the simulation_as_proxy reading (low extractiveness, coordination-dominant, beneficiary-weighted, no victim set under its own lights); the sibling files carry the necessity critique (high contest), the hybrid two-component split, and the technology-conditioned threshold variant. Family links run through affects_constraints in all four files. Structural gradient: this reading is the institutionally ascendant one — certification practice funds and frames the terrain on which the others operate — hence the influences edge to simulation_fidelity_threshold (whose research agenda lives inside sim-dependent institutions) and foreclosure edges to the two readings whose core premises categorical sufficiency logically contradicts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
