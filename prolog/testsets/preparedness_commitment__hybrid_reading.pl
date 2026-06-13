% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__hybrid_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_commitment__hybrid_reading
 *   human_readable: Dual-Layer Preparedness Commitment: Memorial Stabilization + Competence Maintenance
 *   domain: institutional/civic/disaster_management
 *
 * SUMMARY:
 *   A preparedness system that coordinates disaster response across
 *   generations faces a core problem: disaster frequency is low enough that
 *   commitment erodes during quiet periods. The hybrid reading proposes that
 *   commitment-stabilization (memorial layer) and operational-competence
 *   (competence layer) are both necessary and structurally distinct. The
 *   memorial layer includes annual commemorations, formal procedure reviews,
 *   and institutional narratives that keep preparedness culturally alive. The
 *   competence layer includes ongoing training, equipment testing, and
 *   procedure updates that maintain actual response capacity. The constraint
 *   requires active enforcement because the dual structure creates
 *   overhead—resources spent on ritual cannot be redirected to immediate
 *   service delivery—and resource-constrained agencies face pressure to shed
 *   the memorial layer and keep only competence (or conversely, to keep
 *   memorial and let competence decay). The hybrid reading holds that both
 *   layers prevent distinct catastrophes: memorial abandonment leads to
 *   generational commitment decay, while competence abandonment leads to
 *   failed immediate response. This story authorizes a tangled_rope
 *   classification: genuine coordination (disaster response only functions
 *   with maintained procedures and institutional commitment), asymmetric
 *   extraction (payers subsidize beneficiaries), active enforcement (the
 *   dual-layer structure is defended against cost-efficiency pressure), and
 *   tension between layers (memorial is overhead from a competence-only
 *   perspective; competence is futile if the institution doesn't survive to
 *   deploy it).
 *
 * KEY AGENTS:
 *   - institutional_memory_keepers: agenda-setter, organized/identity-locked. Sets the dual-layer standard, administers both memorial and competence elements, career identity fused with preparedness custodianship.
 *   - resource_constrained_agencies: payer, moderate/constrained. Allocates annual budget to both layers despite competing needs; experiences the constraint as unfunded mandate in low-disaster periods.
 *   - technical_personnel: payer, moderate/constrained. Executes both ritual and competence activities on top of regular duties; experiences extraction of time.
 *   - post_disaster_survivors: beneficiary, powerless/trapped. Receives coordinated, informed response; has no voice in preparedness administration.
 *   - taxpayers_low_disaster_exposure: payer, organized/mobile. Subsidizes preparedness in low-risk regions; can partially exit through relocation or political opposition; suppression is moderate.
 *   - disaster_frequency_variability: observer, analytical. External reality check on the constraint's justification; oscillates the credibility of both layers.
 *   - rival_preparedness_models: excluded, institutional/constrained. Insurance-based, competence-only, and memorial-only frameworks are structurally excluded from adoption.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, 0.58).
domain_priors:suppression_score(preparedness_commitment__hybrid_reading, 0.42).
domain_priors:theater_ratio(preparedness_commitment__hybrid_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_commitment__hybrid_reading, "Dual-Layer Preparedness Commitment: Memorial Stabilization + Competence Maintenance").
narrative_ontology:topic_domain(preparedness_commitment__hybrid_reading, "institutional/civic/disaster_management").

domain_priors:requires_active_enforcement(preparedness_commitment__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__hybrid_reading, '9e4a3294-5625-495a-a957-6f8b4c13ef65').
narrative_ontology:cs_kernel_codification('9e4a3294-5625-495a-a957-6f8b4c13ef65', implicit).
narrative_ontology:cs_authority_grounding('9e4a3294-5625-495a-a957-6f8b4c13ef65', practice).
narrative_ontology:cs_interpretation_layer_present('9e4a3294-5625-495a-a957-6f8b4c13ef65').
narrative_ontology:cs_reading_relation('9e4a3294-5625-495a-a957-6f8b4c13ef65', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('9e4a3294-5625-495a-a957-6f8b4c13ef65', preparedness_commitment__husk_reading, coexists_with).
narrative_ontology:cs_axiom('9e4a3294-5625-495a-a957-6f8b4c13ef65', foundational, memorial_and_competence_mutually_necessary).
narrative_ontology:cs_axiom_status(memorial_and_competence_mutually_necessary, holdable).
narrative_ontology:cs_axiom_grounding('9e4a3294-5625-495a-a957-6f8b4c13ef65', memorial_and_competence_mutually_necessary, instrumental).
narrative_ontology:cs_axiom('9e4a3294-5625-495a-a957-6f8b4c13ef65', foundational, institutional_commitment_requires_memorial_anchoring).
narrative_ontology:cs_axiom_status(institutional_commitment_requires_memorial_anchoring, holdable).
narrative_ontology:cs_axiom_grounding('9e4a3294-5625-495a-a957-6f8b4c13ef65', institutional_commitment_requires_memorial_anchoring, deontological).
narrative_ontology:cs_reference_frame('9e4a3294-5625-495a-a957-6f8b4c13ef65', dual_layer_preparedness_framework).
narrative_ontology:cs_drift_state('9e4a3294-5625-495a-a957-6f8b4c13ef65', contemporary_cost_efficiency_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9e4a3294-5625-495a-a957-6f8b4c13ef65', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__hybrid_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, institutional_continuity_stakeholders).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, post_disaster_survivors).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, resource_constrained_agencies).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, technical_personnel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, taxpayers_low_disaster_exposure).
narrative_ontology:constraint_vindicates(preparedness_commitment__hybrid_reading, institutional_memory_is_extractive_cost).
narrative_ontology:constraint_vindicates(preparedness_commitment__hybrid_reading, ritual_maintenance_prevents_abandonment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain preparedness protocols, conduct regular drills, document and transmit disaster response procedures. Their professional identity fuses with the custodianship role; abandoning preparedness work feels like professional self-annihilation. They administer both the memorial/ritual layer (annual commemorations, formal procedure reviews) and the competence layer (actual training, equipment testing). They justify continued investment through institutional loyalty and the claim that memory prevents abandonment.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, institutional_memory_keepers, agenda_setter,
    institutional, generational, identity_locked, national).

% Allocate annual budgets to preparedness maintenance despite competing immediate needs. They must fund both memorial activities (ceremony, documentation, historical review) and operational competence (equipment, training, redundancy). The dual layer creates overhead: resources spent on ritual cannot be redirected to service delivery. They experience the constraint as an unfunded mandate when disaster frequency is low.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, resource_constrained_agencies, payer,
    moderate, biographical, constrained, national).

% Execute both ritual and competence activities on top of regular duties. They participate in annual drills (memorial function), maintain equipment (competence function), and generate compliance documentation. The constraint extracts time that could go to primary work. Their exit is constrained by employment contracts and the diffuse sense that abandoning preparedness is irresponsible.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, technical_personnel, payer,
    moderate, biographical, constrained, national).

% Receive coordinated, informed response when disaster strikes because the competence layer maintains actual operational capacity. They benefit from decades of accumulated response knowledge embodied in procedures and personnel. They have no voice in preparedness administration and cannot exit the constraint's domain.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, post_disaster_survivors, beneficiary,
    powerless, immediate, trapped, local).

% Fund preparedness through taxes in regions with low disaster probability. The dual-layer structure means they subsidize both competence (arguably justified by residual risk) and memorial maintenance (which feels ceremonial from a low-risk region). They can partially exit through relocation to lower-tax jurisdictions or through political opposition to preparedness funding, though the suppression of this exit is moderate—dissent is heard but overridden by institutional commitment narratives.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, taxpayers_low_disaster_exposure, payer,
    organized, biographical, mobile, national).

% Acts as an external reality check on the constraint's justification. Long periods without major disasters make the memorial layer look like pure overhead and suppress the case for competence investment. Clustering of disasters resurrects both layers' credibility. The constraint's perceived necessity oscillates with disaster frequency.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, disaster_frequency_variability, observer,
    analytical, civilizational, analytical, global).

% Alternative frameworks (competence-only, memorial-only, probabilistic insurance-based) are excluded from institutional adoption by the hybrid model's entrenchment. A purely insurance-based model would abandon the memorial layer; a competence-only model would lose institutional commitment across generations. These alternatives exist in academic literature and other jurisdictions but cannot displace the embedded hybrid structure without institutional redesign.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, rival_preparedness_models, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__hybrid_reading, institutional_memory_keepers).
narrative_ontology:fixing_cost_class(preparedness_commitment__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The dual-layer system coordinates across disaster cycles and generational turnover. The memorial layer (commemorations, formal procedure preservation, institutional narrative) ensures that preparedness commitment survives long periods without disasters—when actual risk becomes psychologically abstract, ritual keeps the commitment alive. The competence layer (training, equipment maintenance, operational testing) ensures that when disasters arrive, the response is informed by accumulated knowledge and practiced routines. Together they solve the core coordination problem: how to maintain effective disaster response across generations when disasters are rare and memory is fragile.
% TRANSFER_FUNCTION: Moves resources (budget, personnel time, attention) from immediate service delivery into maintaining both institutional memory and operational competence. The memorial layer extracts time for ceremony, documentation review, historical narrative maintenance. The competence layer extracts resources for training and equipment. The constraint redistributes capacity from the many low-disaster-exposure taxpayers toward the concentrated benefit of post-disaster survivors.
% ABSENT_VOICES: Disaster-risk modelers and actuaries who would argue for probabilistic insurance-based allocation (spending more when risk is measurably higher, less when it drops) are excluded from the funding framework. Cost-efficiency advocates who would argue for pure competence maintenance without memorial overhead are excluded. Citizens in low-disaster regions who would prefer opt-out arrangements have limited voice in the system. Academic researchers advancing alternative models of institutional memory are heard but their arguments are systematically deprioritized in budget cycles.
% DISAPPEARANCE_RATIONALE: If the dual-layer preparedness system vanished, the immediate effect would be degraded disaster response in the next major event—accumulated procedures would be lost, personnel trained to apply those procedures would disperse, and coordination mechanisms would be ad hoc. Over a generation, without the memorial layer, institutional commitment to preparedness would decay during quiet periods and would need to be rebuilt catastrophically after the next disaster. The regional economies and post-disaster populations would face substantially higher costs.
% FOUNDING_PROBLEM: Preparedness requires maintaining capability and commitment across long periods without disasters. Purely rational economic allocation fails because (1) disaster probability is low enough that profit-maximizing insurance avoids the tail risk, (2) trained personnel retire and knowledge dissipates, and (3) commitment erodes when the threat is abstract. The hybrid system was built to solve this: memoir and ritual keep institutional commitment alive, and regular competence maintenance keeps actual response capacity current.
% FOUNDING_PROBLEM_CORROBORATION: Disaster-response practitioners and institutional historians outside the preparedness community attest that the founding problem is live—regions that let preparedness lapse (memorial layer degrades first, competence follows) face catastrophic response failures when disasters strike. Cost-efficiency advocates contest that the problem is solved by competence maintenance alone and that the memorial layer is mostly overhead. Insurance analysts note that pricing models now incorporate disaster frequency learning, which partly displaces the need for institutional memory. No unified external consensus; multiple viewpoints grounded in actual post-disaster outcomes.
narrative_ontology:disappearance_verdict(preparedness_commitment__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(preparedness_commitment__hybrid_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_commitment__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.58 over the interval, with a projected peak at 0.65 before settling at 0.58. The rise reflects accumulating awareness that memorial activities consume resources without direct operational payoff during disaster-free periods. The peak and decline model a cycle: extraction intensity rises as inefficiency awareness grows, then recedes if a major disaster validates both layers (or if political pressure succeeds in shedding memorial overhead). Theater rises from 0.38 to 0.58 at the same projection point, indicating growing performative overhead—memorial activities become more ceremony, less content—as agencies attempt to maintain the commitment narrative without proportional resource investment. Suppression requirement (0.35→0.52→0.42) models enforcement intensity: maintaining the dual structure requires suppression of cost-efficiency arguments in budget cycles (0.42–0.47 baseline), which intensifies when disaster frequency is low (peak 0.52 at t=35, a projected low-disaster moment). At t=40, the model assumes a major disaster event; suppression requirement drops (0.42) because the disaster validates both layers' necessity and reduces the pressure to abandon them. These metrics are authored on a shared time grid: every metric has a value at every time point examined, enabling the engine to detect coupled dynamics and identify the measurement intervals most relevant to classification divergence.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (memory-keeper) and the payer seats experience dramatically different constraint types. From the memory-keeper perspective, the dual system is genuine coordination—both layers prevent distinct failures, and the tension between them is a necessary design feature, not overhead. From the constrained-payer perspective (resource agencies, technical personnel), the constraint is enforced extraction—they fund both layers against their immediate preferences, and the memorial layer especially looks like institutional capture of budget. From the low-disaster-exposure taxpayer perspective, the constraint is a snare with theatrical elements—they subsidize both layers for remote contingencies and see the memorial activities as pure overhead. The engine computes these per-seat divergences from the structural data: the memory-keeper's high power, identity-locked exit, and generational horizon versus the constrained-payer's moderate power, constrained exit, and biographical horizon. The directionality for the memory-keeper (beneficiary, low d → negative χ, subsidy-side) and the constrained-payer (payer, high d → high χ, extraction-side) should produce visibly different type classifications. This is exactly the seat divergence the system is designed to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality divides along institutional position and exit structure. The memory-keeper seat holds agenda-setting power (institutional) and identity-locked exit (professional identity fused with custodianship); they derive substantial authority from administering the dual system and experience it as coordination they designed and maintain. Their d value is near 0.2 (beneficiary range): they set the rules, collect authority, and face minimal exit cost (career identity would need to restructure, but that is an internal cost, not an external barrier). The constrained-payer seats (resource agencies, technical personnel) hold moderate power and constrained exit; they fund resources they would prefer to redirect and face weak alternatives (exit via budget reallocation or organizational redesign, both politically difficult). Their d values are near 0.7–0.8 (target range): they bear the constraint's costs. The low-disaster-exposure taxpayers (organized but mobile) have d near 0.5 (symmetric): they subsidize but can relocate or reduce support through political pressure. The post-disaster survivors (powerless, trapped) have d near 1.0 (full target): they depend on the constraint's functionality but have zero input on its structure. No directionality overrides are needed; the derived chain (beneficiary/victim + power + exit → d) produces the asymmetry organically.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy because the founding problem remains live and the dual-layer structure is contested by design. The memory-keeper institution attests that the problem (institutional commitment decay across disaster-free periods) is still active and that both layers prevent distinct failures. Disaster-response practitioners and institutional historians outside the preparedness community corroborate that regions letting preparedness lapse face catastrophic response failures. The contested status of the founding problem (live vs. dead readings) is exactly what the hybrid reading inhabits: the problem is real but its solution cost is debated. The tangled_rope classification holds if both layers are genuinely necessary for coordination (integration problem: without memorial, competence decays; without competence, coordination is theatrical). If the founding problem were fully dead (disaster frequency dropped to near-zero, insurance pricing absorbed the risk), the constraint would drift toward piton (maintained by inertia, not necessity). If the founding problem were fully live but the competence layer alone were sufficient, the constraint would be snare (memorial layer is pure extraction). The hybrid reading's mandatrophy status is 'contested and active': the problem is real, the solution is defended, and the tension between layers is acknowledged. This prevents the constraint from crystallizing into either pure recovery (mandatrophy resolved via sunset) or pure inertia (piton classification).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    memorial_competence_separability,
    'Are the memorial and competence layers structurally separable, or do they reinforce each other in ways that make combined investment more efficient than separate allocation?',
    'Comparative analysis of jurisdictions that have attempted to maintain competence without memorial (competence-only) or memorial without competence (husk-only); measurement of institutional commitment persistence in each model across disaster-free decades.',
    'If separable, the hybrid reading''s extraction cost is higher than the minimum needed for coordination; the competence layer alone might sustain adequate response capacity. If reinforcing, the hybrid structure is closer to genuine coordination than a snare, and the measured extractiveness includes necessary coupling costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_competence_separability, empirical, 'Whether the two layers are functionally redundant or mutually reinforcing.').

omega_variable(
    institutional_identity_lock_mechanism,
    'Is the identity-lock in the memory-keeper role structural (the custodian role is genuinely difficult to abandon without organizational redesign) or performative (abandonment is psychologically costly but organizationally trivial)?',
    'Track attempts to reduce memorial overhead or shift custodianship; measure psychological and career costs on personnel who reduce engagement; document institutional redesigns in other domains that successfully decoupled identity from role.',
    'If structural, the identity-lock contributes to the constraint''s persistence and partly justifies the suppression measure. If performative, the identity-lock is a manufactured suppression mechanism and the constraint is closer to pure snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_identity_lock_mechanism, conceptual, 'Whether memory-keeper identity fusion is intrinsic or culturally constructed.').

omega_variable(
    disaster_frequency_determination,
    'Is the actual disaster frequency in the jurisdictions under study high enough to justify the dual-layer resource investment on expected-value grounds, independent of commitment narratives?',
    'Historical actuarial analysis of disaster probability and impact; cost-benefit comparison against insurance-only models; measurement of lives saved attributable to maintained competence layers after major disasters.',
    'High frequency justifies both layers on rational grounds; medium frequency justifies competence but makes memorial look optional; low frequency makes the entire dual system look like institutional inertia. This is the crux separating the hybrid reading from the husk reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disaster_frequency_determination, empirical, 'Whether actual disaster frequency justifies the dual-layer investment structure.').

omega_variable(
    committer_frame_alternative_readings,
    'This constraint is one reading of the preparedness_commitment kernel. How would the competence_reading and husk_reading characterize the same facts differently?',
    'Author separate constraint stories for each sibling reading, applying the same structural data but framing the coordination/extraction boundary differently. The three stories together establish whether the kernel''s reading-space is genuinely three-way contested or whether one reading dominates.',
    'The reading-space structure determines whether the hybrid reading represents a stable equilibrium or a transient compromise. If all three readings remain live in actual institutional discourse, the hybrid reading captures real ambiguity. If one reading dominates in practice, the hybrid reading is a minority analytical position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_alternative_readings, conceptual, 'The kernel contest structure: which reading(s) are institutionally live.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__hybrid_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t5, preparedness_commitment__hybrid_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement_basis(prep_tr_t5, observed).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__hybrid_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement_basis(prep_tr_t10, observed).
narrative_ontology:measurement(prep_tr_t15, preparedness_commitment__hybrid_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement_basis(prep_tr_t15, observed).
narrative_ontology:measurement(prep_tr_t25, preparedness_commitment__hybrid_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement_basis(prep_tr_t25, observed).
narrative_ontology:measurement(prep_tr_t35, preparedness_commitment__hybrid_reading, theater_ratio, 35, 0.58).
narrative_ontology:measurement_basis(prep_tr_t35, projected).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__hybrid_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(prep_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t5, preparedness_commitment__hybrid_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(prep_be_t5, observed).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__hybrid_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(prep_be_t10, observed).
narrative_ontology:measurement(prep_be_t15, preparedness_commitment__hybrid_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(prep_be_t15, observed).
narrative_ontology:measurement(prep_be_t25, preparedness_commitment__hybrid_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement_basis(prep_be_t25, observed).
narrative_ontology:measurement(prep_be_t35, preparedness_commitment__hybrid_reading, base_extractiveness, 35, 0.65).
narrative_ontology:measurement_basis(prep_be_t35, projected).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__hybrid_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(prep_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__hybrid_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t5, preparedness_commitment__hybrid_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement_basis(prep_su_t5, observed).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__hybrid_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(prep_su_t10, observed).
narrative_ontology:measurement(prep_su_t15, preparedness_commitment__hybrid_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(prep_su_t15, observed).
narrative_ontology:measurement(prep_su_t25, preparedness_commitment__hybrid_reading, suppression_requirement, 25, 0.47).
narrative_ontology:measurement_basis(prep_su_t25, observed).
narrative_ontology:measurement(prep_su_t35, preparedness_commitment__hybrid_reading, suppression_requirement, 35, 0.52).
narrative_ontology:measurement_basis(prep_su_t35, projected).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__hybrid_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(prep_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__husk_reading).

% DUAL FORMULATION NOTE:
% The preparedness_commitment kernel decomposes into three constraint stories, each with a different ε and different beneficiary/victim structure. The hybrid_reading (this story) holds that both memorial and competence layers are necessary; the competence_reading frames preparedness as operational knowledge maintenance alone; the husk_reading frames preparedness as memorial performance that has lost competence. The three readings constitute a genuine kernel contest: they cannot all be true simultaneously in a single framework, but all three remain live in institutional discourse. Each story carries its own ε (competence_reading lower, because it assigns less extraction cost; husk_reading higher, because it treats memorial as pure overhead). The stories are linked by network edges so that classification divergences can be traced to the underlying reading contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
