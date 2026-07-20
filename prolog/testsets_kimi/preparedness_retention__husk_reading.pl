% ============================================================================
% CONSTRAINT STORY: preparedness_retention__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__husk_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_retention__husk_reading
 *   human_readable: Preparedness as Memorial Performance (Husk Reading)
 *   domain: disaster preparedness / institutional memory / governance
 *
 * SUMMARY:
 *   This constraint story instantiates the husk_reading of the
 *   preparedness_retention kernel: the claim that institutional disaster
 *   preparedness has degraded into memorial performance. Under this reading,
 *   drills, inspections, and certifications function as rituals that simulate
 *   readiness without preserving live competence. The constraint extracts
 *   resources (budget, time, attention) from adaptive capacity and transfers
 *   them to ceremonial compliance, benefiting institutional legitimacy while
 *   leaving actual response capacity brittle. The kernel is contested: the
 *   competence_reading holds that drills preserve genuine operational
 *   knowledge, and the hybrid_reading holds that competence survives only in
 *   specialized enclaves. This story treats the husk reading as structurally
 *   true for the constraint it models.
 *
 * KEY AGENTS:
 *   - emergency_management_agencies: agenda_setter (institutional/constrained) â administer the ceremonial regime and capture budget/legitimacy
 *   - compliance_vendors: beneficiary (organized/mobile) â supply the ritual infrastructure and profit from compliance demand
 *   - frontline_responders: payer (moderate/constrained) â bear the opportunity cost of ceremonial participation
 *   - disaster_exposed_populations: payer (powerless/trapped) â bear catastrophic risk when ceremonial readiness fails
 *   - adaptive_capacity_advocates: excluded (moderate/constrained) â marginalized alternative voice
 *   - legislative_oversight: observer (institutional/analytical) â audits via checkbox metrics, reinforcing the regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__husk_reading, 0.62).
domain_priors:suppression_score(preparedness_retention__husk_reading, 0.48).
domain_priors:theater_ratio(preparedness_retention__husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__husk_reading, piton).
narrative_ontology:human_readable(preparedness_retention__husk_reading, "Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_retention__husk_reading, "disaster preparedness / institutional memory / governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__husk_reading, '1320375a-5b25-457e-a302-02d80997ed0e').
narrative_ontology:cs_kernel_codification('1320375a-5b25-457e-a302-02d80997ed0e', distributed).
narrative_ontology:cs_authority_grounding('1320375a-5b25-457e-a302-02d80997ed0e', practice).
narrative_ontology:cs_interpretation_layer_present('1320375a-5b25-457e-a302-02d80997ed0e').
narrative_ontology:cs_reading_relation('1320375a-5b25-457e-a302-02d80997ed0e', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('1320375a-5b25-457e-a302-02d80997ed0e', preparedness_retention__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('1320375a-5b25-457e-a302-02d80997ed0e', foundational, ceremonial_action_equals_readiness).
narrative_ontology:cs_axiom_status(ceremonial_action_equals_readiness, holdable).
narrative_ontology:cs_axiom_grounding('1320375a-5b25-457e-a302-02d80997ed0e', ceremonial_action_equals_readiness, conventional).
narrative_ontology:cs_axiom('1320375a-5b25-457e-a302-02d80997ed0e', foundational, tacit_competence_unreliable).
narrative_ontology:cs_axiom_status(tacit_competence_unreliable, holdable).
narrative_ontology:cs_axiom_grounding('1320375a-5b25-457e-a302-02d80997ed0e', tacit_competence_unreliable, instrumental).
narrative_ontology:cs_reference_frame('1320375a-5b25-457e-a302-02d80997ed0e', ritualized_preparation_state).
narrative_ontology:cs_drift_state('1320375a-5b25-457e-a302-02d80997ed0e', post_systemic_failure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1320375a-5b25-457e-a302-02d80997ed0e', '').
narrative_ontology:cs_kernel_id(preparedness_retention__husk_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, compliance_vendors).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, disaster_exposed_populations).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, frontline_responders).
narrative_ontology:constraint_vindicates(preparedness_retention__husk_reading, ceremonial_accountability_hypothesis).
narrative_ontology:constraint_vindicates(preparedness_retention__husk_reading, measurable_readiness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer preparedness grants, mandate drill schedules, and conduct compliance inspections. Their organizational survival and budget growth depend on visible, measurable preparedness outputs. They set the criteria by which readiness is judged, favoring auditable ceremony over tacit competence that resists documentation.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Sell drill facilitation, inspection protocols, certification training, and after-action report templates to agencies. Their revenue scales with the institutional demand for visible compliance artifacts rather than with actual disaster outcomes.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, compliance_vendors, beneficiary,
    organized, biographical, mobile, national).

% Required to participate in scheduled drills and inspections that consume training time but do not develop adaptive judgment. Their career advancement depends on certification metrics rather than operational competence. Cannot opt out without jeopardizing employment or assignment to specialized units.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, frontline_responders, payer,
    moderate, immediate, constrained, regional).

% Live in hazard zones where institutional preparedness is presented as protective. Bear the catastrophic risk when ceremonial readiness fails to translate into competent response during actual disasters. Have no direct voice in preparedness design or resource allocation.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, disaster_exposed_populations, payer,
    powerless, immediate, trapped, local).

% Researchers and practitioners who argue for improvisation, tacit knowledge, and decentralized adaptive capacity over scripted drills. Their funding and policy access are limited because their recommendations resist quantifiable audit metrics and centralized management.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, adaptive_capacity_advocates, excluded,
    moderate, generational, constrained, national).

% Audit preparedness spending through checkbox compliance frameworks. Their reports reinforce ceremonial priorities because their own legitimacy depends on auditable evidence rather than on unobservable operational competence.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, legislative_oversight, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_retention__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, repeatable framework for rehearsing institutional response procedures and demonstrating accountability to political principals, funders, and the public through visible, auditable activity.
% TRANSFER_FUNCTION: Moves funding, staff time, training hours, and institutional attention from adaptive skill-building and tacit competence development into visible ceremonial activities (scripted drills, inspection checklists, documentation) that produce measurable compliance artifacts. Transfers legitimacy and budgetary justification to administering institutions while leaving actual disaster response capacity under-resourced and brittle.
% ABSENT_VOICES: Frontline responders with deep tacit operational knowledge, disaster-affected communities who experienced capability gaps, and researchers of adaptive management and improvisation are structurally excluded. Their exclusion is enforced by funding mechanisms that only reimburse quantifiable drill outputs and by professional norms that equate certification with competence.
% DISAPPEARANCE_RATIONALE: If the ceremonial drill-and-inspection apparatus vanished, emergency management budgets would reallocate toward unmeasured competence activities or away from preparedness entirely; compliance vendors would lose their institutional market; oversight bodies would lose their audit objects. The institutional landscape of disaster governance would reorganize around different accountability mechanisms, though the transition would be contested.
% FOUNDING_PROBLEM: Actual disasters historically revealed chaotic, uncoordinated response failures; institutions needed a mechanism to ensure readiness, build inter-agency familiarity, rehearse decision chains, and demonstrate accountability to political principals and publics.
% FOUNDING_PROBLEM_CORROBORATION: Disaster sociology researchers and frontline responders attested in post-event reviews (e.g., Hurricane Katrina, Fukushima, Grenfell) that ceremonial compliance failed to translate into adaptive capacity. No independent corroboration from outside the benefiting institutions holds that the current drill regime preserves live competence; corroboration of the founding problem's death comes from external academic and practitioner communities, while the benefiting institutions assert it remains live.
narrative_ontology:disappearance_verdict(preparedness_retention__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__husk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_retention__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__husk_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater_ratio is high (0.78) because the constraint's dominant activity is performative: drills rehearse scripts rather than adaptive judgment, inspections verify documentation rather than capability. Extractiveness (0.62) reflects substantial resource diversion from competence to ceremony. Suppression (0.48) is moderate: alternatives are not violently repressed but are starved of funding and excluded from credentialing pathways. Resistance (0.25) is low because victims (disaster-exposed populations) do not recognize their structural victimhood until a disaster occurs, and frontline responders are constrained by career norms. Accessibility_collapse (0.52) is moderate: competence-based alternatives exist in theory but cannot compete for institutional resources against auditable ceremonial outputs. The measurement series show monotonic drift toward greater theatricality and extraction over the interval, consistent with institutional atrophy.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (emergency management agencies) experiences the constraint as necessary institutional maintenance: they believe they are fulfilling a legitimate mandate. The payer seats (frontline responders, disaster-exposed populations) experience the same structure as inertial extraction that consumes resources without producing protective capacity. The compliance_vendors experience it as a neutral market opportunity. The engine computes this divergence from the structural data â the husk reading asserts the agenda_setter's framing is false consciousness or institutional self-preservation, not structural truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Emergency_management_agencies and compliance_vendors sit near the beneficiary end: they accrue budget, authority, and revenue from the constraint's operation. Their exit options are constrained only by organizational identity, not by economic necessity. Frontline_responders and disaster_exposed_populations sit near the target end: they pay through lost competence-development time and catastrophic risk exposure, with trapped or constrained exit. Legislative_oversight sits near symmetric but is analytically captured by the audit paradigm it reinforces.
 *
 * MANDATROPHY ANALYSIS:
 *   The husk reading prevents mislabeling by distinguishing atrophied coordination from live coordination. A tangled_rope reading would require that genuine coordination still occurs alongside extraction; the husk reading denies this, asserting the coordination function is necrotic. A snare reading would require active, intentional suppression by a concentrated beneficiary; the husk reading instead identifies diffuse institutional inertia and theatrical maintenance as the persistence mechanism. The piton classification captures the atrophy and theatricality without imputing malign intent to a rent-extractor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does this constraint instantiate a genuinely degraded preparedness system (husk_reading), or a competence-preserving system misread as ceremonial (competence_reading), or a stratified mixture (hybrid_reading)?',
    'Comparative ethnography of drill outcomes against actual disaster response performance; measurement of tacit skill retention versus ceremonial compliance across institutional types.',
    'If competence_reading is true, the constraint is a rope or tangled_rope with low extractiveness; if husk_reading is true, it is a piton or snare with high theater_ratio and substantial extraction from actual capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the preparedness_retention kernel is structurally true.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the dominance of ceremonial preparedness structural (funding and career incentives) or internalized (professionals genuinely believe checklists equal readiness)?',
    'Post-reform trajectory analysis: if ceremonial dominance persists after funding incentives shift to competence metrics, the suppression is partially internalized.',
    'If internalized, effective suppression exceeds structural measures and reclassification toward snare may be warranted; if purely structural, the constraint is more readily reformable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression of competence alternatives is structural or internalized.').

omega_variable(
    ceremony_necessary_cost,
    'Does some level of ceremonial rehearsal constitute a necessary coordination cost, or is all ceremony overhead?',
    'Natural experiments comparing institutions with high and low ceremony-to-competence ratios, controlling for disaster type and frequency.',
    'If some ceremony is necessary cost, the base extractiveness should be discounted by that floor; if none is necessary, the full measure stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ceremony_necessary_cost, empirical, 'Whether ceremonial activity carries any non-extractive coordination floor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(preparedness_retention_husk_tr_t0, preparedness_retention__husk_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(preparedness_retention_husk_tr_t8, preparedness_retention__husk_reading, theater_ratio, 8, 0.52).
narrative_ontology:measurement(preparedness_retention_husk_tr_t16, preparedness_retention__husk_reading, theater_ratio, 16, 0.62).
narrative_ontology:measurement(preparedness_retention_husk_tr_t24, preparedness_retention__husk_reading, theater_ratio, 24, 0.7).
narrative_ontology:measurement(preparedness_retention_husk_tr_t32, preparedness_retention__husk_reading, theater_ratio, 32, 0.75).
narrative_ontology:measurement(preparedness_retention_husk_tr_t40, preparedness_retention__husk_reading, theater_ratio, 40, 0.78).

% Extraction over time
narrative_ontology:measurement(preparedness_retention_husk_be_t0, preparedness_retention__husk_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(preparedness_retention_husk_be_t8, preparedness_retention__husk_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(preparedness_retention_husk_be_t16, preparedness_retention__husk_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(preparedness_retention_husk_be_t24, preparedness_retention__husk_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(preparedness_retention_husk_be_t32, preparedness_retention__husk_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(preparedness_retention_husk_be_t40, preparedness_retention__husk_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(preparedness_retention_husk_su_t0, preparedness_retention__husk_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(preparedness_retention_husk_su_t8, preparedness_retention__husk_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(preparedness_retention_husk_su_t16, preparedness_retention__husk_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(preparedness_retention_husk_su_t24, preparedness_retention__husk_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(preparedness_retention_husk_su_t32, preparedness_retention__husk_reading, suppression_requirement, 32, 0.48).
narrative_ontology:measurement(preparedness_retention_husk_su_t40, preparedness_retention__husk_reading, suppression_requirement, 40, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% The preparedness_retention kernel decomposes into three structurally distinct constraints: competence_reading (low extraction, live coordination), husk_reading (high theater, atrophied function), and hybrid_reading (stratified competence and ceremony). Each reading carries a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
