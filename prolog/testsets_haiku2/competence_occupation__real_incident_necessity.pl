% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__real_incident_necessity, []).

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
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Real Incident Necessity for Competence Occupation in High-Reliability Organizations
 *   domain: organizational_safety/training_management
 *
 * SUMMARY:
 *   High-reliability organizations (aviation, nuclear power, emergency
 *   medicine, military operations) maintain personnel competence through
 *   training regimes. This constraint story instantiates ONE reading of a
 *   contested kernel: the claim that ONLY actual catastrophic incidents
 *   provide the authentic conditions necessary to occupy the competence
 *   kernel — to certify that personnel can handle genuine emergencies. Under
 *   this reading, simulation and drills, no matter how sophisticated, cannot
 *   truly test competence because they lack the physiological and cognitive
 *   stress of real stakes. The doctrine is self-validating: incidents prove
 *   competence gaps existed; absences prove nothing. Operating personnel and
 *   exposed populations bear the cost of this untestable doctrine; safety
 *   regulators and incident investigators benefit from its explanatory
 *   structure.
 *
 * KEY AGENTS:
 *   - operating_personnel: high-stakes operators (pilots, surgeons, nuclear technicians) who must be certified competent but cannot be tested short of real incidents — trapped payers
 *   - safety_regulators: institutional beneficiaries who enforce competence regimes they cannot validate and whose authority strengthens when incidents occur
 *   - incident_exposed_populations: powerless members of the public (passengers, patients, nearby residents) who become evidence for competence certification when incidents happen
 *   - operating_organizations: institutional payers who must defend untestable training regimes and absorb liability when incidents expose competence gaps
 *   - simulation_technology_vendors: excluded by doctrine; their high-fidelity platforms are deemed insufficient despite technological sophistication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.78).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.72).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.78).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, tangled_rope).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real Incident Necessity for Competence Occupation in High-Reliability Organizations").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "organizational_safety/training_management").

domain_priors:requires_active_enforcement(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, '37336608-721b-4d6d-b9d1-b7437fb100ea').
narrative_ontology:cs_kernel_codification('37336608-721b-4d6d-b9d1-b7437fb100ea', formalized).
narrative_ontology:cs_authority_grounding('37336608-721b-4d6d-b9d1-b7437fb100ea', extraction).
narrative_ontology:cs_interpretation_layer_present('37336608-721b-4d6d-b9d1-b7437fb100ea').
narrative_ontology:cs_reading_relation('37336608-721b-4d6d-b9d1-b7437fb100ea', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('37336608-721b-4d6d-b9d1-b7437fb100ea', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('37336608-721b-4d6d-b9d1-b7437fb100ea', foundational, authentic_stress_incommensurability).
narrative_ontology:cs_axiom_status(authentic_stress_incommensurability, holdable).
narrative_ontology:cs_axiom_grounding('37336608-721b-4d6d-b9d1-b7437fb100ea', authentic_stress_incommensurability, empirically_contingent).
narrative_ontology:cs_axiom('37336608-721b-4d6d-b9d1-b7437fb100ea', foundational, incident_necessity_for_validation).
narrative_ontology:cs_axiom_status(incident_necessity_for_validation, holdable).
narrative_ontology:cs_axiom_grounding('37336608-721b-4d6d-b9d1-b7437fb100ea', incident_necessity_for_validation, deontological).
narrative_ontology:cs_reference_frame('37336608-721b-4d6d-b9d1-b7437fb100ea', untestable_competence_doctrine).
narrative_ontology:cs_drift_state('37336608-721b-4d6d-b9d1-b7437fb100ea', contemporary_alternative_regimes, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('37336608-721b-4d6d-b9d1-b7437fb100ea', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, organizational_learning_doctrine).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, operating_personnel).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, incident_exposed_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, incident_investigation_authorities).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, operating_organizations).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, training_program_designers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pilots, surgeons, nuclear operators, emergency responders, military personnel who must maintain high-stakes competence. Under the real-incident-necessity doctrine, they cannot prove to regulators that their training has adequately prepared them for genuine emergencies until actual emergencies occur. They invest heavily in training and certification, but the doctrine holds that the training's adequacy is unknowable until events validate or invalidate it. They bear the psychological and professional burden of this untestable standard and the liability risk when incidents expose competence gaps. Exit means leaving the profession entirely.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, operating_personnel, payer,
    moderate, biographical, constrained, regional).

% Airlines, hospitals, nuclear plants, military branches that employ high-stakes personnel and must maintain competence regimes. They cannot prove to regulators that their training is working — the doctrine offers no measurable success criteria. They defend their regimes through compliance with regulatory guidance and incident investigation, but the doctrine guarantees they will be found inadequate when incidents occur (the incidents prove the regime was insufficient). They absorb operational costs, liability, and regulatory scrutiny.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, operating_organizations, payer,
    institutional, generational, constrained, national).

% Members of the public in proximity to high-stakes operations: airline passengers, hospital patients, residents near nuclear facilities, civilians in combat zones. Under the real-incident-necessity reading, they are the involuntary evidence base for competence certification. They bear the risk that competence gaps go undetected until incidents expose them. They have no seat in competence-regime decisions and no exit from the constraint.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, incident_exposed_populations, payer,
    powerless, immediate, trapped, local).

% Federal and professional bodies that certify competence and oversee training regimes (FAA, NRC, medical licensing boards, etc.). They benefit from the real-incident-necessity doctrine because it provides explanatory coherence: incidents prove the framework is working (revealing true competence needs), absence of incidents is indeterminate. The doctrine legitimizes their authority to investigate and regulate. They set agenda for what counts as competence occupation and enforce compliance through inspection, audit, and incident investigation. Their legitimacy and mandate grow with each incident that reveals a competence gap.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, safety_regulators, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__real_incident_necessity, safety_regulators, agenda_setter).

% Organizations that investigate accidents and incidents (NTSB, medical boards, military inspector-general offices). They benefit from the doctrine because incidents provide the evidence for competence certification. Their institutional mandate, prestige, and budget grow with each incident that reveals a competence gap framed as proof the framework is working. They investigate, report, and recommend training improvements, which are then framed as evidence that the competence regime is being refined.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, incident_investigation_authorities, beneficiary,
    institutional, generational, analytical, national).

% Companies that design and sell high-fidelity simulation platforms for pilot training, medical simulation, military operations, nuclear operations. Their technologies are sophisticated and widely used, but under the real-incident-necessity reading they are systematically excluded from the competence-certification process. The doctrine asserts that simulation cannot authentically reproduce the stress and decision-context of real emergencies, so simulation counts as training but not as competence validation. They are trapped outside the decision frame; their voice is not heard in regulatory and institutional discussions of what counts as adequate competence maintenance.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, simulation_technology_vendors, excluded,
    powerful, biographical, mobile, global).

% Alternative institutional framings (simulation-sufficiency, hybrid multi-mechanism occupation, continuous-refresher models) that operate in other regulatory jurisdictions and are documented in international safety standards. They represent structural alternatives to real-incident-necessity but are not the authorized doctrine in this constraint's regulatory context.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, international_hybrid_regimes, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(competence_occupation__real_incident_necessity, international_hybrid_regimes).

% Aviation instructors, medical educators, military trainers, and simulation specialists who design and deliver competence training. They operate under the real-incident-necessity doctrine, which means their training regimes are evaluated as adequate or inadequate retroactively — only when incidents occur. They cannot design curricula confident in their own validity; they work within a framework that treats all non-incident training as potentially insufficient. They bear professional and legal exposure when their trainees are involved in incidents.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, training_program_designers, payer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__real_incident_necessity, safety_regulators).
narrative_ontology:fixing_cost_class(competence_occupation__real_incident_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishing a shared doctrine for certifying that personnel in high-stakes operations (aviation, medicine, nuclear, military) have achieved and maintain competence to handle emergencies without exposing the public to untested personnel.
% TRANSFER_FUNCTION: Transfers the burden and cost of competence proof from regulators and organizations (who would need to demonstrate readiness through measurable means) to operating personnel and incident-exposed populations (who become the evidence base when incidents occur). Moves authority to interpret what counts as adequate competence from training designers and organizations to incident investigators and regulators.
% ABSENT_VOICES: Simulation technology designers and international safety regimes using hybrid or simulation-sufficient approaches are excluded from this reading's framework. Their argument that sophisticated simulation can adequately test competence is foreclosed by the doctrine's core premise. Exposed populations are trapped: they bear the risk but have no seat in competence-certification decisions or regimen design.
% DISAPPEARANCE_RATIONALE: If the real-incident-necessity reading of competence occupation vanished, organizations would adopt empirically testable regimes. Training would be validated through measurable criteria (simulation performance, procedural mastery, stress-response benchmarks) rather than incident interpretation. Competence maintenance would shift from an untestable doctrine to a continuous-validation framework. Incident rates would become interpretable as evidence for or against the adopted regime rather than as proof that the prior regime was inadequate.
% FOUNDING_PROBLEM: Discovered empirically in aviation and emergency medicine in the mid-20th century: personnel trained to expert level in simulators and classrooms sometimes performed inadequately or froze in genuine emergencies due to the incommensurable stress and cognitive load of real life-or-death situations. The question emerged: how can we know if someone is truly competent for emergencies without exposing them (and the public) to emergencies?
% FOUNDING_PROBLEM_CORROBORATION: Aviation authorities (ICAO) and emergency medicine bodies (WHO) documented the problem's reality in their historical standards. However, contemporary high-reliability organization research (Weick, Roberts on organizational resilience; Dekker on safety culture) disputes the solution. International safety standards have shifted toward hybrid and simulation-sufficient regimes. Regulatory bodies in some jurisdictions (e.g., some U.S. FAA regions, some national aviation authorities) continue to endorse real-incident-necessity; others have adopted measurable alternatives. The problem is corroborated; the solution is contested.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__real_incident_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78 at interval end) is high because the doctrine transfers the burden of competence proof from authorities (who should demonstrate readiness) to personnel and populations (who must be exposed to incidents to validate regimes). The doctrine is self-protecting: it cannot be falsified by safe operation (absence of incidents proves nothing under this reading) and is validated by incidents (which appear to prove competence was inadequate, not that the doctrine itself is flawed). Suppression (0.72) is sustained because alternatives (simulation-sufficiency, hybrid regimes, continuous-refresher models) are actively excluded by regulatory authority; personnel cannot choose different training regimes. Theater (0.61, high but not dominant) reflects the substantial performative component: much training activity and incident investigation happens to vindicate the doctrine's logic rather than to improve safety directly. The measurement series document rising extractiveness and theater over the interval, consistent with the doctrine becoming more entrenched and more reliant on incident interpretation.
 *
 * PERSPECTIVAL GAP:
 *   Operating personnel and organizations perceive this constraint as an insoluble bind: they cannot prove competence is adequate (no untestable doctrine permits proof) and they are held liable when incidents occur as evidence they were inadequate. Safety regulators and incident investigators perceive it as a coherent doctrine: incidents reveal true competence needs, absence of incidents is indeterminate. The engine computes divergent types from the structural positions: personnel and organizations sit near the target end (high d, extraction amplified); regulators sit near the beneficiary end (low d, authority and explanatory power collected). This perspective gap is the point.
 *
 * DIRECTIONALITY LOGIC:
 *   Operating personnel are victims (trapped, constrained exit, no authority over regime selection — high d toward 1.0, full target). Operating organizations are payers (institutional, but constrained by regulatory mandate, absorb liability — high d, 0.6–0.8 range). Incident-exposed populations are powerless victims (d near 1.0). Safety regulators are beneficiaries (institutional power, agenda-setting authority, explanatory authority strengthened by incidents — low d, 0.1–0.3 range). Incident investigation authorities are secondary beneficiaries (mandate and prestige grow with incidents). Simulation vendors are excluded, not coordinated; their exclusion is the enforcement object.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to certify competence in high-stakes fields without real-incident testing) is LIVE in the sense that organizations continue to grapple with competence maintenance. However, the specific solution offered by the real-incident-necessity reading is increasingly questioned: hybrid regimes, continuous-refresher models, and simulation-sufficiency doctrines are live alternatives in international practice. The constraint persists in some regulatory regimes because its self-validating structure protects it: incidents prove it right; absences prove nothing. This is a strong candidate for mandatrophy: the founding problem persists but the solution (real-incident-necessity) is contested and may be atrophying in favor of measurable alternatives. If regulators shifted to hybrid regimes, this constraint would persist theatrically (incident investigation would still frame findings as proof of competence gaps) but would lose its primary justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incident_causation_attribution,
    'When an incident occurs and investigation reveals a competence gap, is the gap evidence that competence occupation was never achieved (as the real-incident-necessity reading asserts), or evidence that the regime for maintaining competence was inadequate (as alternative readings assert)?',
    'Comparative incident analysis across jurisdictions using different competence regimes (simulation-sufficient, hybrid, real-incident-necessary). If incident rates are similar, the reading is ambiguous; if rates differ systematically, attribution becomes clearer.',
    'If incidents are attributed to regime inadequacy rather than proof-of-competence-gap, the self-validating structure of the real-incident-necessity reading collapses. Regulators lose their explanatory armor and must shift to measurable regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incident_causation_attribution, empirical, 'Whether incidents reveal competence gaps or regime failures.').

omega_variable(
    simulation_fidelity_boundary,
    'Is there a technological or cognitive boundary beyond which simulation cannot reproduce the authentic stress and decision context of real incidents? Or is any claimed boundary a rationalization for the doctrine rather than a structural feature?',
    'Neurophysiological and cognitive-science research on stress response, decision-making under uncertainty, and transfer of training from high-fidelity simulation to live performance. Longitudinal tracking of personnel trained under different regimes.',
    'If simulation can authentically reproduce competence-critical stress, the real-incident-necessity reading loses its core claim. If boundaries exist and are irreducible, the reading is vindicated but would need to articulate them precisely rather than invoking incidents as the sole test.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_boundary, empirical, 'Whether authentic-stress boundaries are inherent or constructed.').

omega_variable(
    self_validation_trap,
    'Is the real-incident-necessity reading''s self-validating structure (incidents prove it right; absences prove nothing) a feature that captures genuine epistemological truth, or a feature that makes the doctrine unfalsifiable and therefore unscientific?',
    'Philosophy of science and epistemology: comparison of the reading''s logical structure with frameworks for falsifiability. Assessment of whether high-reliability organizations can operate without a falsifiable competence doctrine.',
    'If the unfalsifiability is recognized as a flaw rather than a feature, the reading loses legitimacy within scientific and regulatory frameworks committed to empirical testing. The constraint would be recognized as extraction dressed in safety language.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_validation_trap, conceptual, 'Whether self-validation is epistemically sound or a protective mechanism.').

omega_variable(
    beneficiary_structure_ambiguity,
    'Who benefits from the real-incident-necessity doctrine? Safety regulators gain explanatory authority and authority to investigate. Incident investigation authorities gain mandate and prestige. But do operating organizations or exposed populations benefit, or only incur costs?',
    'Institutional analysis of regulatory capture: does the doctrine serve organizational learning and safety, or does it serve the bureaucratic interests of regulators and investigators?',
    'If the doctrine is recognized as serving regulatory bureaucracy rather than safety, it becomes a snare rather than a tangled rope. The coordination function (competence occupation) is real, but the extraction mechanism (untestable doctrine that vindicates regulators when incidents occur) is pure rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_ambiguity, empirical, 'Whether the doctrine''s beneficiaries include safety or only regulatory authority.').

omega_variable(
    kernel_reading_stability,
    'Is the real-incident-necessity reading a stable, enduring position in the competence-occupation kernel, or is it historically contingent and eroding?',
    'Historical and institutional analysis of competence-training doctrine across aviation, nuclear, emergency medicine, and military operations over the past 50 years. Tracking of shifts toward simulation-sufficiency and hybrid regimes.',
    'If the reading is eroding and being displaced by measurable alternatives, the constraint itself is a piton — persisting theatrically while its primary justification atrophies. If it remains stable, it is an entrenched tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_stability, empirical, 'Whether real-incident-necessity is a durable doctrine or historically contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.48).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__real_incident_necessity, theater_ratio, 5, 0.51).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__real_incident_necessity, theater_ratio, 10, 0.54).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__real_incident_necessity, theater_ratio, 15, 0.57).
narrative_ontology:measurement(comp_tr_t25, competence_occupation__real_incident_necessity, theater_ratio, 25, 0.59).
narrative_ontology:measurement(comp_tr_t40, competence_occupation__real_incident_necessity, theater_ratio, 40, 0.61).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(comp_be_t5, competence_occupation__real_incident_necessity, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(comp_be_t10, competence_occupation__real_incident_necessity, base_extractiveness, 10, 0.69).
narrative_ontology:measurement(comp_be_t15, competence_occupation__real_incident_necessity, base_extractiveness, 15, 0.71).
narrative_ontology:measurement(comp_be_t25, competence_occupation__real_incident_necessity, base_extractiveness, 25, 0.75).
narrative_ontology:measurement(comp_be_t40, competence_occupation__real_incident_necessity, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__real_incident_necessity, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(comp_su_t5, competence_occupation__real_incident_necessity, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(comp_su_t10, competence_occupation__real_incident_necessity, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(comp_su_t15, competence_occupation__real_incident_necessity, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(comp_su_t25, competence_occupation__real_incident_necessity, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(comp_su_t40, competence_occupation__real_incident_necessity, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__real_incident_necessity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__real_incident_necessity, 0.12).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, regulatory_authority_incident_framing).

% DUAL FORMULATION NOTE:
% The 'competence_occupation' kernel decomposes into three constraint stories with distinct ε values and beneficiary structures: real_incident_necessity (this story, ε=0.78, high extraction via untestable doctrine); simulation_sufficiency (ε~0.35, low extraction, empirically testable); hybrid_occupation (ε~0.55, moderate extraction, contested configuration). The readings are not alternative measurements of one constraint — they are structurally distinct constraints sharing a common kernel. Each reading instantiates a different causal mechanism for competence maintenance, different observable-set, and different beneficiary structure. ε-invariance principle: if changing the observable (incident vs. simulation vs. hybrid) changes ε materially, the observer is looking at different constraints. The three stories are linked via network edges (each affects the others) and the kernel context; they remain separate JSON files with independent classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
