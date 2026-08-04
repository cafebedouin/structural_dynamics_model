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
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Real Incident Necessity for Competence Occupation
 *   domain: safety/organizational
 *
 * SUMMARY:
 *   This constraint instantiates the real_incident_necessity reading of the
 *   competence_occupation kernel: the claim that authentic competence can
 *   only be certified and maintained by operators who have survived actual
 *   catastrophic incidents. The constraint operates as a snare — it presents
 *   itself as a safety principle (ensuring true competence) while enforcing
 *   conditions that make catastrophes necessary and unpreventable.
 *   High-reliability organizations face an impossible bind: accept that their
 *   simulation-trained operators are inadequately competent (violates their
 *   safety mandate) or accept that catastrophic incidents are unavoidable
 *   maintenance costs (also violates their mandate). The measurement series
 *   shows rising extractiveness and theater_ratio over time as the constraint
 *   hardens institutional norms and suppresses contradictory evidence
 *   (simulator-based equivalence studies, comparative safety data from
 *   hybrid-trained cohorts).
 *
 * KEY AGENTS:
 *   - Incident survivors: undergo catastrophic exposure as the only recognized path to authentic competence certification — bear the material cost of the constraint
 *   - Frontline operators: identity-locked to the constraint — professional legitimacy fused to 'having seen it' creates psychological suppression
 *   - Training organizations: constrained payers — cannot claim simulation-sufficiency even when evidence supports it
 *   - Regulatory gatekeepers: beneficiaries and agenda-setters — maintain authority by keeping the bar at incident-necessity
 *   - Incident researchers: beneficiaries — ensure continuous supply of catastrophic incidents as research data
 *   - High-reliability organizations: institutional payers — face unresolvable tradeoff between safety and competence maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.88).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.91).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.88).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, snare).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real Incident Necessity for Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "safety/organizational").

domain_priors:requires_active_enforcement(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, '78155b9c-8055-4d25-bd61-2b530cdb5456').
narrative_ontology:cs_kernel_codification('78155b9c-8055-4d25-bd61-2b530cdb5456', formalized).
narrative_ontology:cs_authority_grounding('78155b9c-8055-4d25-bd61-2b530cdb5456', extraction).
narrative_ontology:cs_interpretation_layer_present('78155b9c-8055-4d25-bd61-2b530cdb5456').
narrative_ontology:cs_reading_relation('78155b9c-8055-4d25-bd61-2b530cdb5456', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('78155b9c-8055-4d25-bd61-2b530cdb5456', competence_occupation__hybrid_occupation, coexists_with).
narrative_ontology:cs_axiom('78155b9c-8055-4d25-bd61-2b530cdb5456', foundational, catastrophic_incidents_ontologically_necessary).
narrative_ontology:cs_axiom_status(catastrophic_incidents_ontologically_necessary, holdable).
narrative_ontology:cs_axiom_grounding('78155b9c-8055-4d25-bd61-2b530cdb5456', catastrophic_incidents_ontologically_necessary, empirically_contingent).
narrative_ontology:cs_axiom('78155b9c-8055-4d25-bd61-2b530cdb5456', foundational, simulation_cannot_bridge_authenticity_gap).
narrative_ontology:cs_axiom_status(simulation_cannot_bridge_authenticity_gap, holdable).
narrative_ontology:cs_axiom_grounding('78155b9c-8055-4d25-bd61-2b530cdb5456', simulation_cannot_bridge_authenticity_gap, empirically_contingent).
narrative_ontology:cs_reference_frame('78155b9c-8055-4d25-bd61-2b530cdb5456', incident_authenticity_doctrine).
narrative_ontology:cs_drift_state('78155b9c-8055-4d25-bd61-2b530cdb5456', simulator_technology_maturation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('78155b9c-8055-4d25-bd61-2b530cdb5456', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, training_organizations).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, high_reliability_cultures).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, incident_researchers).
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, regulatory_gatekeepers).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, incident_survivors).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, high_reliability_organizations).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, simulation_technology_vendors).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, system_users).
narrative_ontology:constraint_vindicates(competence_occupation__real_incident_necessity, incident_authenticity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Undergo catastrophic incident exposure as the only recognized path to authentic competence certification. They bear the material cost — injury, death, psychological trauma, organizational disruption — that makes competence 'real' under this constraint. No exit without abandoning credibility as truly competent.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, incident_survivors, payer,
    powerless, biographical, trapped, global).

% Professional identity fused to the constraint: a truly competent pilot, surgeon, reactor operator, or nuclear commander is one who has 'seen it.' Simulation-trained operators, no matter how effective, carry stigma under this constraint. Their exit would require abandoning the belief that their profession has standards.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, frontline_operators, payer,
    moderate, biographical, identity_locked, global).

% Bound by the constraint to justify their simulation and drill programs as preparatory, not sufficient. The constraint prevents them from claiming training alone produces competent operators, even when evidence suggests it does. Their institutional mandate becomes permanently incomplete.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, training_organizations, payer,
    organized, generational, constrained, global).

% Face an impossible tradeoff: operate below authentic competence (using simulation-trained staff) or accept catastrophic incidents as maintenance cost. The constraint frames any admission of simulation-sufficiency as moral failure. Safety culture erodes under this bind.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, high_reliability_organizations, payer,
    institutional, generational, constrained, global).

% The constraint ensures a continuous supply of catastrophic real incidents to study and publish from. Research careers, academic advancement, and incident investigation infrastructure depend on the occurrence of preventable catastrophes that serve as data sources.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, incident_researchers, beneficiary,
    institutional, generational, mobile, global).

% Maintain authority over competence certification by enforcing the real-incident standard. Their power to define what counts as authentic competence depends on keeping the bar at 'has survived a catastrophe.' Accepting simulation-sufficiency would transfer certification authority to training organizations.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, regulatory_gatekeepers, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__real_incident_necessity, regulatory_gatekeepers, agenda_setter).

% Can sell simulation systems indefinitely while knowing the institutional framework treats simulation as insufficient. They can market their products as 'preparation for the real thing,' but the constraint prevents them from claiming competence-completion through technology.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, simulation_technology_vendors, payer,
    powerful, biographical, mobile, global).

% Depend on operators who must either carry the stigma of simulation-only training or undergo the trauma of real incident exposure to become 'truly competent.' They fund both the cost of preventable catastrophes and the cost of unnecessary incident exposure through insurance, premiums, and organizational damage.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, system_users, payer,
    organized, biographical, trapped, global).

% Maintain codes of ethics and competence standards, but cannot mandate incident-exposure without violating harm-prevention principles. They witness the constraint's operation but lack the structural authority to override the incident-necessity doctrine.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, professional_societies, observer,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__real_incident_necessity, regulatory_gatekeepers).
narrative_ontology:fixing_cost_class(competence_occupation__real_incident_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legitimacy mechanism for competence recognition in high-stakes domains: ensures operators claiming expertise have faced authentic system-failure conditions, not just drilled the theory.
% TRANSFER_FUNCTION: Transfers the cost of catastrophic incidents (injuries, deaths, organizational failure, trauma) from the abstract domain of 'training cost' to concrete operators and their communities. Ensures incidents remain unpreventable — the very incidents organizations should prevent become the currency of competence certification.
% ABSENT_VOICES: Systems engineers and training researchers who have demonstrated simulation-based competence equivalence are systematically excluded from certification authority. They would testify that authentic competence can be occupied without incident exposure, but their findings are framed as incomplete or theoretical rather than evidence.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, organizations could certify operators on demonstrable simulation mastery plus behavioral markers, incidents would decline sharply (no longer serving as training), research would shift from incident analysis to competence validation, and regulatory authority would transfer toward training organizations and away from gatekeeping regulators. The entire competence maintenance infrastructure would reorganize without the incident-necessity anchor.
% FOUNDING_PROBLEM: In high-reliability domains, the distance between trained performance and authentic performance under catastrophic conditions was historically vast — drills and simulators could not replicate the psychological and operational chaos of real failure. Operators trained only on simulation faced unknown failure modes.
% FOUNDING_PROBLEM_CORROBORATION: Modern simulation technology, decades of evidence from aviation and nuclear operations, and cross-domain research from anesthesia, surgery, and emergency medicine all document that simulation-based training can close the theory-practice gap substantially. This corroboration comes from operators themselves (pilot studies), technologists (simulator fidelity research), and independent researchers outside the regulatory-authority sphere. The regulatory gatekeepers attest the problem remains live, but no credible external source supports that assessment.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.88, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.88) because the constraint transfers the cost of catastrophes — which should be preventable — into the domain of training maintenance, making incidents economically rationalized. Suppression is highest-measured (0.91) because the constraint must actively suppress contradictory evidence: simulator-based competence studies, cross-domain research showing simulation-training equivalence, and operators' own reports of readiness without incident exposure all threaten the constraint's legitimacy. Theater_ratio rises sharply (0.48 → 0.72) as organizations invest in elaborate incident-simulation drills that are framed as preparation rather than complete training, and as regulatory language increasingly emphasizes incident-authenticity while de-emphasizing simulator-fidelity improvements. The measurement trajectory shows hardening enforcement: regulatory language tightens (t=5–15), incident investigations are prioritized over near-miss analysis (t=15–25), and by t=40 the constraint appears near-irreversible institutional norm.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory-gatekeeper seat, the constraint appears as legitimate competence assurance — operators who have faced failure are genuinely competent, and simulation cannot replace that. From the frontline-operator and high-reliability-organization seats, the constraint appears as institutional capture: regulators maintain authority by making incident-exposure necessary, creating a perverse incentive against prevention. From the incident-researcher seat, it appears as career infrastructure — the constraint ensures a continuous supply of catastrophic events to study. The engine computes these divergent types from the structural asymmetry: beneficiary seats see coordination (real incidents are necessary), payer seats see extraction (incidents are being perpetuated as certification cost).
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory gatekeepers are structural beneficiaries (d ≈ 0.1–0.2): they maintain certification authority, collect rents in the form of influence over organizational safety policy, and can defend the constraint as safety-centered. Incident researchers are beneficiaries (d ≈ 0.2–0.3): catastrophic incidents fund their research programs and careers. Frontline operators, training organizations, high-reliability organizations, and system users are all targets (d ≈ 0.7–1.0): they bear the cost of incident exposure, the stigma of simulation-only training, and the impossible tradeoff. The constraint uses identity-lock (for operators) and constrained exit (for organizations) to suppress the beneficiary/victim structure — operators cannot exit without abandoning professional identity, organizations cannot exit without abandoning safety mandates that the constraint itself has made impossible to meet.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the distance between simulation and authentic performance under catastrophic conditions) has been substantially solved by simulator technology advances, behavioral research, and cross-domain comparative studies. The constraint persists not because the founding problem lives, but because the regulatory apparatus and research infrastructure have become invested in the problem's persistence. The constraint satisfies mandatrophy resolution: the mandate ('ensure operators are competent') has outlived its function (simulation-based training can now satisfy that mandate), but the constraint persists because the institutional solution (accepting simulation-sufficiency) would transfer authority away from the gatekeeping regulators. This is structurally a snare: the constraint persists because beneficiaries block the exits that would benefit the payers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulator_fidelity_sufficiency,
    'Has modern simulator technology closed the performance gap between simulation-trained and incident-trained operators to the point that simulation-based competence occupation is functionally equivalent to incident-based occupation?',
    'Comparative cohort studies tracking incident-free operations of simulation-trained vs. incident-trained operators over 10+ years in high-stakes domains (aviation, nuclear, medical). Meta-analysis of existing cross-domain evidence (anesthesia, emergency medicine, pilot training showing no safety differentials).',
    'If simulator-sufficiency is demonstrated, the constraint becomes a false necessity — catastrophic incidents would be preventable without sacrificing competence standards, and the constraint would reclassify from snare to false-summit mountain (natural law disguising institutional benefit). If incidents remain necessary, the constraint''s snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulator_fidelity_sufficiency, empirical, 'Whether modern simulation can occupy competence without incident exposure.').

omega_variable(
    identity_lock_mechanism,
    'To what extent is the suppression of simulation-sufficiency evidence driven by operators'' identity-fusion with the ''incident-tested'' narrative, vs. genuine risk assessment?',
    'Qualitative research with operators trained purely on simulation who achieve safety records equivalent to incident-trained peers: tracking whether identity-fusion (career advancement, peer recognition, self-perception of competence) depends on incident exposure or shifts as safety outcomes prove sufficiency.',
    'If identity-fusion is the primary suppression mechanism, the constraint relies on internalized psychological enforcement that persists even after external validation of simulation-sufficiency. This deepens the snare character — operators carry suppression with them after any attempt to exit. If identity-fusion is secondary to material risk assessment, exit becomes more viable and the constraint''s hold weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether suppression is structural or internalized through professional identity.').

omega_variable(
    regulatory_authority_transfer,
    'Would accepting simulation-sufficiency require transferring competence-certification authority from regulatory gatekeepers to training organizations and system operators?',
    'Policy analysis of current regulatory frameworks: tracing which institutions currently hold the authority to define ''competent'' and which would hold it if simulation-sufficiency were codified.',
    'If authority transfer is required, regulatory gatekeepers face a structural incentive to deny simulation-sufficiency regardless of evidence, because accepting it would reduce their institutional power. This elevates the constraint to an institutional-capture snare where the beneficiary structure is itself a barrier to resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_authority_transfer, conceptual, 'Whether the constraint''s persistence depends on preserving gatekeeping authority.').

omega_variable(
    incident_necessity_doctrine_scope,
    'Is the real_incident_necessity reading universal across high-reliability domains, or domain-specific? Do some domains (aviation, medicine) accept simulation-sufficiency more readily than others (nuclear command, military)?',
    'Comparative analysis of competence-certification standards across domains: aviation (simulation-heavy certification paths), medical education (simulation increasingly accepted), nuclear and military (incident-necessity doctrine more entrenched).',
    'If incident-necessity is domain-specific, the constraint''s snare character is domain-bounded — some institutional contexts have escaped the bind through acceptance of simulation. If universal, the constraint operates across all high-stakes domains as a systematic structure preventing preventable incidents from being prevented.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incident_necessity_doctrine_scope, empirical, 'Scope of the real_incident_necessity reading across professional domains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.48).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__real_incident_necessity, theater_ratio, 5, 0.52).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__real_incident_necessity, theater_ratio, 10, 0.58).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__real_incident_necessity, theater_ratio, 15, 0.64).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t25, competence_occupation__real_incident_necessity, theater_ratio, 25, 0.68).
narrative_ontology:measurement_basis(comp_tr_t25, observed).
narrative_ontology:measurement(comp_tr_t40, competence_occupation__real_incident_necessity, theater_ratio, 40, 0.72).
narrative_ontology:measurement_basis(comp_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t5, competence_occupation__real_incident_necessity, base_extractiveness, 5, 0.72).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_occupation__real_incident_necessity, base_extractiveness, 10, 0.76).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_occupation__real_incident_necessity, base_extractiveness, 15, 0.8).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t25, competence_occupation__real_incident_necessity, base_extractiveness, 25, 0.85).
narrative_ontology:measurement_basis(comp_be_t25, observed).
narrative_ontology:measurement(comp_be_t40, competence_occupation__real_incident_necessity, base_extractiveness, 40, 0.88).
narrative_ontology:measurement_basis(comp_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__real_incident_necessity, suppression_requirement, 0, 0.82).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t5, competence_occupation__real_incident_necessity, suppression_requirement, 5, 0.85).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_occupation__real_incident_necessity, suppression_requirement, 10, 0.87).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_occupation__real_incident_necessity, suppression_requirement, 15, 0.88).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t25, competence_occupation__real_incident_necessity, suppression_requirement, 25, 0.9).
narrative_ontology:measurement_basis(comp_su_t25, observed).
narrative_ontology:measurement(comp_su_t40, competence_occupation__real_incident_necessity, suppression_requirement, 40, 0.91).
narrative_ontology:measurement_basis(comp_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__real_incident_necessity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__real_incident_necessity, 0.06).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_occupation kernel. The simulation_sufficiency and hybrid_occupation constraints instantiate sibling readings of the same kernel: each claims a different structural relationship between simulation-training, real-incident exposure, and competence occupation. The three stories are linked by network.affects_constraints because a change in the empirical facts about simulator-fidelity (measured in omega_simulator_fidelity_sufficiency) would alter the plausibility and classification of all three readings. This is not a case of one constraint influencing another in the domain sense; it is a case of three constraints being alternative framings of a single contestable institutional claim. Each constraint should be authored independently as a complete, ε-invariant story, with its own beneficiary/victim structure and its own structural data. The engine will compute which reading's classification matches the evidence best.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__real_incident_necessity, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
