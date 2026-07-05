% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: preparedness_commitment__hybrid_reading
 *   human_readable: Layered Preparedness: Memorial Commitment Stabilizer plus Operational Competence Function
 *   domain: institutional/civic/disaster_management
 *
 * SUMMARY:
 *   This story is the HYBRID reading of the preparedness_commitment kernel:
 *   it treats disaster preparedness as a genuinely layered system in which a
 *   memorial/commemorative component and an operational/competence component
 *   are structurally distinct but functionally coupled. The memorial layer
 *   (anniversaries, plaques, ceremonial briefings) exists to prevent
 *   preparedness funding and attention from lapsing once a disaster recedes
 *   from living memory — a real coordination problem, since purely technical
 *   programs are shown to attrite politically over multi-decade gaps. The
 *   competence layer (certification, live drills, equipment maintenance)
 *   exists to prevent the memorial layer's ceremony from substituting for
 *   actual operational capacity. The hybrid claim is that BOTH layers are
 *   load-bearing and BOTH generate cost: the memorial layer draws resources
 *   and responder time that compete with competence maintenance, and the
 *   competence layer requires ongoing enforcement (certification lapses,
 *   drill mandates) that the memorial layer alone would never generate. This
 *   is a distinct constraint from the sibling readings: the husk_reading
 *   claims the memorial layer has become a substitute for competence
 *   (near-total capture by performance), and the competence_reading claims
 *   the operational layer alone is sufficient and load-bearing (memorial
 *   elements are decorative, not structural). Do not average these three
 *   readings — each is a separate constraint with its own epsilon,
 *   stakeholders, and structural claim; this file is only the hybrid one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, 0.38).
domain_priors:suppression_score(preparedness_commitment__hybrid_reading, 0.42).
domain_priors:theater_ratio(preparedness_commitment__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_commitment__hybrid_reading, "Layered Preparedness: Memorial Commitment Stabilizer plus Operational Competence Function").
narrative_ontology:topic_domain(preparedness_commitment__hybrid_reading, "institutional/civic/disaster_management").

domain_priors:requires_active_enforcement(preparedness_commitment__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__hybrid_reading, '4cd47bd1-4deb-4f82-9d6c-56ac65404dab').
narrative_ontology:cs_kernel_codification('4cd47bd1-4deb-4f82-9d6c-56ac65404dab', distributed).
narrative_ontology:cs_authority_grounding('4cd47bd1-4deb-4f82-9d6c-56ac65404dab', practice).
narrative_ontology:cs_interpretation_layer_present('4cd47bd1-4deb-4f82-9d6c-56ac65404dab').
narrative_ontology:cs_reading_relation('4cd47bd1-4deb-4f82-9d6c-56ac65404dab', preparedness_commitment__husk_reading, influences).
narrative_ontology:cs_reading_relation('4cd47bd1-4deb-4f82-9d6c-56ac65404dab', preparedness_commitment__competence_reading, influences).
narrative_ontology:cs_axiom('4cd47bd1-4deb-4f82-9d6c-56ac65404dab', foundational, memorial_and_competence_are_jointly_necessary).
narrative_ontology:cs_axiom_status(memorial_and_competence_are_jointly_necessary, holdable).
narrative_ontology:cs_axiom_grounding('4cd47bd1-4deb-4f82-9d6c-56ac65404dab', memorial_and_competence_are_jointly_necessary, instrumental).
narrative_ontology:cs_axiom('4cd47bd1-4deb-4f82-9d6c-56ac65404dab', secondary, layer_tension_is_an_acceptable_maintenance_cost).
narrative_ontology:cs_axiom_status(layer_tension_is_an_acceptable_maintenance_cost, holdable).
narrative_ontology:cs_axiom_grounding('4cd47bd1-4deb-4f82-9d6c-56ac65404dab', layer_tension_is_an_acceptable_maintenance_cost, instrumental).
narrative_ontology:cs_reference_frame('4cd47bd1-4deb-4f82-9d6c-56ac65404dab', post_disaster_institutional_founding).
narrative_ontology:cs_drift_state('4cd47bd1-4deb-4f82-9d6c-56ac65404dab', contemporary_multigenerational_quiet_period, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4cd47bd1-4deb-4f82-9d6c-56ac65404dab', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__hybrid_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, future_disaster_populations).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, commemorative_institutions).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, current_taxpayers_funding_drills).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, frontline_responders_bearing_dual_burden).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and mandates the combined program: annual commemorative observances (anniversary drills, memorial plaques, ceremonial briefings) alongside technical competence requirements (equipment certification, live-fire simulations, interagency coordination tests). Justifies budget requests by pointing to both layers — the memorial layer secures political will and funding continuity, the competence layer is what actually gets tested in real events. Administers which layer gets emphasis in any given budget cycle.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, emergency_management_agencies, beneficiary).

% Museums, memorial foundations, and anniversary event organizers whose institutional existence depends on preparedness remaining culturally salient. They benefit whenever the memorial layer is emphasized, regardless of whether the competence layer atrophies, because their funding and relevance derive from commemoration, not operational testing.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, commemorative_institutions, beneficiary,
    organized, civilizational, arbitrage, national).

% Firefighters, medics, and civil-defense volunteers required to participate in both ceremonial commemorations and live competence drills within the same limited training calendar. They report that memorial events consume hours that could go to skills maintenance, but cannot refuse either component without risking institutional censure or loss of standing.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, frontline_responders_bearing_dual_burden, payer,
    moderate, biographical, constrained, local).

% Fund both layers through local levies without a clear accounting of which dollars sustain memory-keeping versus which sustain functional readiness. They cannot audit the split and have no direct voice in reallocating between layers.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, current_taxpayers_funding_drills, payer,
    powerless, biographical, trapped, regional).

% Not yet born or not yet affected — the people who will face the next disaster and depend on both the institutional memory that keeps preparedness funded across quiet decades and the operational skill that keeps the response competent when the disaster arrives. They have no voice in the current tradeoffs between layers.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, future_disaster_populations, beneficiary,
    powerless, civilizational, analytical, regional).

% Periodically review whether preparedness spending produces measurable competence gains or mainly sustains commemorative programming. They can reallocate funds between layers but rarely have granular data distinguishing the two, so their oversight tends to ratify existing splits rather than correct them.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, budget_oversight_committees, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_commitment__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves two linked problems simultaneously: sustaining political and financial will to fund preparedness across the long quiet stretches between disasters (via memorial/commemorative elements that keep the threat culturally present), and maintaining actual operational capacity to respond when disaster strikes (via competence elements — drills, certifications, equipment maintenance).
% TRANSFER_FUNCTION: Moves taxpayer funds and responder labor-time into two channels: commemorative institutions and ceremonial programming on one side, operational training and equipment on the other. The split between channels is set administratively, not by measured need, and shifts with political cycles rather than risk assessment.
% ABSENT_VOICES: Future disaster populations who will depend on whichever balance of memory and competence exists when the disaster hits have no representation in current budget or program design. Frontline responders' complaints about training-time competition between the two layers are heard but rarely change the mandated balance.
% DISAPPEARANCE_RATIONALE: If the memorial layer disappeared, emergency managers argue funding and political attention to preparedness would erode within a generation as the disaster recedes from living memory — this is contested by critics who say competence programs with their own metrics (drill pass rates, response-time benchmarks) could sustain funding without ceremony. If the competence layer disappeared, the memorial layer alone would produce a preparedness apparatus that looks functional in ceremony but fails operationally — this is less contested; most parties agree competence loss would be catastrophic and visible at the next disaster.
% FOUNDING_PROBLEM: Communities repeatedly let preparedness lapse after a disaster fades from memory, and lapsed preparedness produces sharply worse outcomes at the next disaster; a purely technical competence program with no memorial or narrative anchor was found to be politically unsustainable across multi-decade gaps between events.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster-response researchers and post-incident review boards (outside both the emergency agencies and the commemorative institutions) corroborate that regions with lapsed multi-decade preparedness funding show measurably worse outcomes, and that purely technical programs lacking any commemorative/narrative anchor saw the sharpest funding attrition — supporting that the founding problem remains live for both layers, though the correct balance between them is unresolved even among these independent observers.
narrative_ontology:disappearance_verdict(preparedness_commitment__hybrid_reading, contested).
narrative_ontology:founding_problem_status(preparedness_commitment__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_commitment__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__hybrid_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__hybrid_reading_tests).
:- end_tests(preparedness_commitment__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because real coordination value exists in both layers, but resource competition between memorial and competence programming under a fixed budget produces a mild but real extraction on payers (taxpayers, frontline responders) who cannot audit or reallocate the split. Theater ratio rises to 0.40 over the interval — not because the memorial layer is pure performance (in this reading it is not), but because the coupling between layers means that as the disaster recedes further from memory, agencies shift more resources to memorial visibility (easier to justify to funders) at some cost to competence maintenance, creating a genuine but partial drift toward performance that the hybrid reading tracks honestly rather than denying. Suppression climbs modestly (0.42) reflecting hardening mandatory-participation requirements for both layers, imposed on responders who have no exit from either component.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting agencies' seat, the dual-layer structure is a deliberate and successful design solving a genuine institutional-memory problem. From the frontline responder seat bearing both mandatory ceremony and mandatory drill within a fixed calendar, the same structure reads as an unaudited transfer of their time to two competing demands, only one of which they see as operationally necessary. The engine computes both from the same structural data; neither party's report should be taken as dispositive on its own.
 *
 * DIRECTIONALITY LOGIC:
 *   Emergency management agencies and commemorative institutions sit near the beneficiary end: they administer the split and their institutional survival depends on the arrangement continuing in some form. Future disaster populations are structural beneficiaries in principle (they benefit from both layers surviving) but have zero present-tense voice, which the derivation captures via powerless/analytical exit. Frontline responders and current taxpayers are targets: they bear the transfer (time, money) without control over how the memorial/competence balance is set, and their exit is constrained by professional and civic obligation respectively.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading is precisely the classification that prevents mislabeling this arrangement as pure extraction (the husk reading's implicit accusation) or as pure functional necessity (the competence reading's implicit defense). Tangled Rope captures that both a genuine coordination function (sustaining multi-decade preparedness commitment) and an asymmetric cost (responders and taxpayers absorbing the friction of maintaining two layers under one budget) can be simultaneously true. Treating this as a pure Rope would ignore the real resource competition and rising theater ratio; treating it as a pure Snare would ignore the corroborated finding that competence-only programs attrite badly over multi-decade quiet periods.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    layer_decoupling_test,
    'Could the memorial layer and competence layer be structurally decoupled — funded and administered separately — without triggering the funding-attrition failure mode the memorial layer is claimed to prevent?',
    'Natural experiment: compare regions that separate memorial and competence budgets/administration against regions that keep them fused, tracking multi-decade funding stability and post-disaster competence outcomes in both.',
    'If decoupling preserves funding stability, the hybrid reading''s coordination claim about the memorial layer weakens toward the husk reading (memorial function becomes optional decoration rather than load-bearing). If decoupling causes funding collapse, the hybrid reading''s coupling claim is strongly corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer_decoupling_test, empirical, 'Whether the memorial and competence layers are truly structurally coupled or merely administratively fused.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is ''preparedness'' genuinely a single layered kernel with three defensible readings (hybrid, husk, competence), or does the apparent contest between readings simply reflect different regions/programs that have drifted to different actual states — some toward husk, some toward competence, some genuinely hybrid?',
    'Cross-regional audit of competence-drill pass rates against memorial-programming budget share; regions clustering near either extreme would suggest the ''kernel'' label covers what are actually distinct empirical states rather than one contested interpretive kernel.',
    'If regions cluster at extremes rather than distributing along a hybrid continuum, the three-reading kernel framing may be an artifact of averaging across genuinely different underlying constraints rather than three interpretations of one shared arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the kernel''s three readings reflect genuine interpretive contest or disguised empirical heterogeneity across cases.').

omega_variable(
    theater_drift_trajectory,
    'Is the rising theater_ratio observed in this reading a stable equilibrium feature of the hybrid structure, or an early-stage drift toward the husk_reading''s near-total capture by performance?',
    'Extend the measurement interval and track whether theater_ratio plateaus (stable hybrid) or continues rising past the point where competence metrics (drill pass rates, response-time benchmarks) begin to degrade in absolute terms.',
    'A plateauing trajectory supports treating this as a stable Tangled Rope; a continuing rise past competence degradation would suggest this constraint is transitioning into the husk_reading over time and should eventually be re-authored as that sibling constraint rather than this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_drift_trajectory, empirical, 'Whether observed theater drift is a stable hybrid feature or an early transition toward the husk reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__hybrid_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(prep_tr_t8, preparedness_commitment__hybrid_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(prep_tr_t16, preparedness_commitment__hybrid_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(prep_tr_t24, preparedness_commitment__hybrid_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(prep_tr_t32, preparedness_commitment__hybrid_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__hybrid_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(prep_be_t8, preparedness_commitment__hybrid_reading, base_extractiveness, 8, 0.26).
narrative_ontology:measurement(prep_be_t16, preparedness_commitment__hybrid_reading, base_extractiveness, 16, 0.31).
narrative_ontology:measurement(prep_be_t24, preparedness_commitment__hybrid_reading, base_extractiveness, 24, 0.34).
narrative_ontology:measurement(prep_be_t32, preparedness_commitment__hybrid_reading, base_extractiveness, 32, 0.36).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__hybrid_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(prep_su_t8, preparedness_commitment__hybrid_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(prep_su_t16, preparedness_commitment__hybrid_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(prep_su_t24, preparedness_commitment__hybrid_reading, suppression_requirement, 24, 0.38).
narrative_ontology:measurement(prep_su_t32, preparedness_commitment__hybrid_reading, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__hybrid_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__competence_reading).

% DUAL FORMULATION NOTE:
% Constraint family of three readings on the preparedness_commitment kernel. This file (hybrid_reading) claims both memorial and competence layers are load-bearing with real coupling costs — Tangled Rope. husk_reading claims the memorial layer has substantively displaced competence — expected Piton or Snare-adjacent. competence_reading claims the memorial layer is inert decoration and only the operational layer is load-bearing — expected closer to Rope. Each carries an independent epsilon; do not reconcile across files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
