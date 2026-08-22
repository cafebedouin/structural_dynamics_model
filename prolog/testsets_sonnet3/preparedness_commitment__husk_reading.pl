% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__husk_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: preparedness_commitment__husk_reading
 *   human_readable: Disaster Preparedness as Memorial Performance (Husk Reading)
 *   domain: institutional/disaster_preparedness
 *
 * SUMMARY:
 *   This constraint models the husk reading of the preparedness-commitment
 *   kernel: a disaster-preparedness regime whose drills, certifications, and
 *   documented plans persist as high-fidelity institutional theater while the
 *   operational competence they were built to produce has hollowed out.
 *   Compliance metrics climb (drills logged, certificates issued, audit
 *   trails complete) while adaptive capacity under novel stress — the thing
 *   the whole apparatus supposedly exists to guarantee — degrades. This is
 *   not the same constraint as the competence_reading (which asserts the
 *   drills genuinely maintain live operational skill) or the hybrid_reading
 *   (which asserts memorial and competence elements are functionally
 *   separable and both present); each of those is a structurally distinct
 *   claim with a different ε, authored as a separate story and linked here
 *   via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - incumbent_program_administrators: agenda-setter/beneficiary, institutional power, constrained exit — run the certification treadmill
 *   - compliance_certifying_bodies: beneficiary, institutional power, arbitrage exit — collect legitimacy fees with no operational exposure
 *   - frontline_emergency_responders: payer, moderate power, trapped exit — bear the field-level capacity gap personally
 *   - affected_residents_in_future_disasters: payer, powerless, trapped exit — bear the ultimate cost in an unscripted disaster
 *   - disaster_studies_researchers: observer, analytical — document the drilled-vs-actual performance gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__husk_reading, 0.68).
domain_priors:suppression_score(preparedness_commitment__husk_reading, 0.58).
domain_priors:theater_ratio(preparedness_commitment__husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, theater_ratio, 0.81).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__husk_reading, piton).
narrative_ontology:human_readable(preparedness_commitment__husk_reading, "Disaster Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_commitment__husk_reading, "institutional/disaster_preparedness").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__husk_reading, 'aed1a020-130b-4760-ad3d-c543224bb23a').
narrative_ontology:cs_kernel_codification('aed1a020-130b-4760-ad3d-c543224bb23a', implicit).
narrative_ontology:cs_authority_grounding('aed1a020-130b-4760-ad3d-c543224bb23a', practice).
narrative_ontology:cs_interpretation_layer_present('aed1a020-130b-4760-ad3d-c543224bb23a').
narrative_ontology:cs_reading_relation('aed1a020-130b-4760-ad3d-c543224bb23a', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('aed1a020-130b-4760-ad3d-c543224bb23a', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('aed1a020-130b-4760-ad3d-c543224bb23a', foundational, form_compliance_has_decoupled_from_function).
narrative_ontology:cs_axiom_status(form_compliance_has_decoupled_from_function, holdable).
narrative_ontology:cs_axiom_grounding('aed1a020-130b-4760-ad3d-c543224bb23a', form_compliance_has_decoupled_from_function, empirically_contingent).
narrative_ontology:cs_axiom('aed1a020-130b-4760-ad3d-c543224bb23a', secondary, certification_artifacts_are_legitimacy_not_evidence).
narrative_ontology:cs_axiom_status(certification_artifacts_are_legitimacy_not_evidence, holdable).
narrative_ontology:cs_axiom_grounding('aed1a020-130b-4760-ad3d-c543224bb23a', certification_artifacts_are_legitimacy_not_evidence, empirically_contingent).
narrative_ontology:cs_reference_frame('aed1a020-130b-4760-ad3d-c543224bb23a', post_incident_reform_mandate).
narrative_ontology:cs_drift_state('aed1a020-130b-4760-ad3d-c543224bb23a', contemporary_certification_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aed1a020-130b-4760-ad3d-c543224bb23a', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__husk_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, compliance_certifying_bodies).
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, incumbent_program_administrators).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, frontline_emergency_responders).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, affected_residents_in_future_disasters).
narrative_ontology:constraint_vindicates(preparedness_commitment__husk_reading, institutional_continuity_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and run the annual drill calendar, certify compliance to funders and legislatures, and are evaluated on whether the drills happened and were documented, not on whether responders could handle a novel event. They administer the program and could redesign it, but redesign threatens their own certification metrics and budget renewal, so the diffuse cost of the gap between drilled and needed capacity is one they absorb only reputationally, not operationally.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, incumbent_program_administrators, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__husk_reading, incumbent_program_administrators, beneficiary).

% Issue preparedness certifications based on checklist completion — drills logged, plans filed, equipment inventoried. They collect fees and legitimacy from certifying, and have no operational exposure when a certified jurisdiction fails under real stress; the certification artifact is what they are paid to produce, regardless of what it predicts.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, compliance_certifying_bodies, beneficiary,
    institutional, generational, arbitrage, national).

% Participate in drills that rehearse a fixed, familiar scenario every cycle and are graded on procedural adherence rather than improvisation under uncertainty. When a real event deviates from the drilled script, they discover in the field that the rehearsed muscle memory does not transfer, and they bear that failure personally and professionally — they cannot opt out of the drills, which are mandatory for continued certification of their own roles.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, frontline_emergency_responders, payer,
    moderate, immediate, trapped, local).

% Depend on the emergency system's actual competence during a crisis they cannot predict or select the shape of. They have no visibility into whether the drills that produced their community's preparedness certificate track real capacity, and no channel to demand a different kind of exercise; they only discover the gap when the response fails to match a novel event's actual demands.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, affected_residents_in_future_disasters, payer,
    powerless, generational, trapped, local).

% Represents the class of events (compound disasters, cascading infrastructure failure, unfamiliar hazard combinations) that the drilled scenarios never rehearse. It has no voice in program design because the program is built around historically familiar, administratively convenient scenarios rather than the space of plausible future ones.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, novel_disaster_scenarios, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(preparedness_commitment__husk_reading, novel_disaster_scenarios).

% Study after-action reports and compare drilled scenarios to actual incident demands; they document the recurring pattern of high compliance scores paired with operational failure in genuine emergencies, without holding power to redesign the certification regime they observe.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, disaster_studies_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_commitment__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: synchronize agencies, personnel, and equipment around a shared, rehearsed response so that in a crisis, actors know their roles and can act without renegotiating the plan in real time.
% TRANSFER_FUNCTION: Moves budget, staff hours, and political credit from the underlying task of building adaptive operational capacity to the production of compliance artifacts — drill logs, certificates, and audit-ready documentation — captured by certifying bodies and administrators as continued funding and legitimacy.
% ABSENT_VOICES: Frontline responders who know from field experience that the drilled scenarios are stale are rarely consulted on drill design, which is set top-down for auditability; residents who will depend on the system in a future, unscripted disaster have no representation in the design process at all.
% DISAPPEARANCE_RATIONALE: Administrators and certifying bodies would say the world rearranges catastrophically — funding streams, legal compliance obligations, and insurance requirements all hinge on the certification apparatus. Disaster researchers and many responders would say the world barely changes operationally, because the current drills do not produce the capacity they claim to certify; removing the performance would only make the existing competence gap visible rather than create a new one.
% FOUNDING_PROBLEM: Early emergency-response coordination failures (units not knowing their roles, equipment not tested, plans existing only on paper) killed people during real disasters; a genealogy of drilling and certification was built to ensure agencies could act on a shared, rehearsed plan under pressure.
% FOUNDING_PROBLEM_CORROBORATION: Disaster-studies researchers and post-incident review boards, both outside the certifying and administering bodies, repeatedly find that certified jurisdictions perform well on drilled scenarios and poorly on novel ones — supporting the reading that the founding problem (real operational coordination) has been substantially decoupled from the artifact (the certificate) that was built to signal it. Administrators and certifying bodies themselves attest the problem remains live and adequately addressed by current drills; no source outside those two groups corroborates that claim.
narrative_ontology:disappearance_verdict(preparedness_commitment__husk_reading, contested).
narrative_ontology:founding_problem_status(preparedness_commitment__husk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_commitment__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__husk_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_commitment__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at T=24) reflects the resource and credibility transfer from genuine capacity-building toward artifact production — budgets and staff hours go to auditable drill logs rather than scenario diversity or stress-testing beyond the familiar script. Suppression (0.58) is moderate: no one is coercively barred from designing better drills, but the certification regime structurally rewards conformity to the existing scenario and penalizes deviation (a novel drill that 'fails' looks worse on paper than a familiar one that 'succeeds'), which functions as suppression of alternative practice. Theater ratio is high and rising (0.42 to 0.81) — this is the central diagnostic of the husk reading: the ratio of performative to functional activity climbs steadily as compliance infrastructure matures and calcifies around a fixed scenario set. Accessibility collapse (0.62) is moderate-high: once a jurisdiction is inside the certification treadmill, alternative preparedness models (red-teaming, adversarial scenario design, capability-based rather than checklist-based assessment) become organizationally illegible and hard to fund. Resistance (0.44) is moderate — frontline responders and researchers do push back, but administrators and certifiers control the metrics that determine funding, so resistance rarely translates into redesign.
 *
 * DIRECTIONALITY LOGIC:
 *   Administrators and certifying bodies sit near the beneficiary end: they collect funding, legitimacy, and continued institutional existence from the certification artifact itself, with no direct exposure when the underlying capacity fails. Frontline responders and future residents sit near the target end: responders are trapped in a mandatory drill cycle that does not build the skills they will need, and residents bear the tail-risk cost of a system that looks prepared on paper but is not stress-tested against novel conditions. The directionality gap between these seats is the structural signature of a husk: form-compliance flows toward the administering seats as legitimacy, while the competence gap flows downstream toward the seats with no voice in the drill design.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uncoordinated agencies failing under pressure) was real and the original coordination function was genuine — this is what distinguishes the husk reading from a pure snare: there was, and nominally still is, a coordination service being performed. But the founding problem's status is contested: administrators assert it remains live and solved; independent post-incident reviews suggest the mandate has drifted from 'build real capacity' to 'produce a compliant record,' with the record now serving primarily to protect the administering institutions from liability and budget cuts rather than to guarantee response capacity. Classifying this as piton (rather than snare) reflects that no single party is capturing large concentrated rents — the certifying bodies collect modest fees, administrators collect job security and budget continuity — while the costs (a preparedness gap that will surface catastrophically under novel stress) are diffuse and deferred onto a population that cannot yet be named. This is the diagnostic case mandatrophy analysis exists for: neither dismissing the whole apparatus as pure extraction (it retains real, if degraded, coordination value) nor crediting it with the operational competence its own theater-ratio trend shows it has stopped producing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_vs_competence_empirical_test,
    'Is the observed pattern (rising compliance metrics, degrading demonstrated capacity under novel stress) a genuine feature of THIS jurisdiction''s preparedness system, or does the husk reading over-generalize from a subset of poorly-run programs to the kernel as a whole?',
    'Compare after-action performance in jurisdictions with high drill-compliance scores against their performance in genuinely novel incidents (not variations of the drilled scenario); a consistent gap across many jurisdictions supports the husk reading, while jurisdictions showing transferable competence would support the competence_reading for those cases.',
    'If the gap is not general but concentrated in specific administratively captured programs, the husk reading should be scoped to those programs rather than treated as the default reading of the kernel, and this story''s beneficiary/victim structure would need narrowing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_competence_empirical_test, empirical, 'Whether hollowed competence is a general kernel property or a subset phenomenon this reading over-extends.').

omega_variable(
    d5_break_threshold,
    'At what degree of novelty does the drilled scenario''s transferable value collapse to zero — is there a smooth degradation or a sharp breakpoint (the ''D5 break'') where familiar procedure becomes actively counterproductive under sufficiently unfamiliar conditions?',
    'Structured comparison of incident response outcomes across a novelty gradient (near-drilled, moderately novel, highly novel events) using existing disaster case libraries.',
    'A sharp breakpoint would strengthen the husk reading''s claim that current drills produce a false sense of security that fails catastrophically rather than gracefully; a smooth gradient would suggest partial transferable value exists even in the husk case, moving the classification closer to a tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(d5_break_threshold, empirical, 'Whether competence collapse under novel stress is gradual or a sharp breakpoint.').

omega_variable(
    kernel_framing_choice,
    'Is the choice to author the husk reading (rather than the competence or hybrid reading) as the primary lens for this jurisdiction justified by the specific evidence available, or does it reflect an availability bias toward documented failures (which get reported) over undocumented successes (which do not)?',
    'Systematic review of the full population of drilled-then-tested incidents, not just the well-publicized failures that motivate skepticism of preparedness theater.',
    'If failures are over-represented in the visible record relative to their true frequency, the husk reading may be over-weighted in public and institutional discourse relative to the hybrid_reading''s more balanced structural claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether selection into visibility biases the choice of reading toward the husk framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__husk_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__husk_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(prep_tr_t4, preparedness_commitment__husk_reading, theater_ratio, 4, 0.51).
narrative_ontology:measurement(prep_tr_t8, preparedness_commitment__husk_reading, theater_ratio, 8, 0.6).
narrative_ontology:measurement(prep_tr_t12, preparedness_commitment__husk_reading, theater_ratio, 12, 0.68).
narrative_ontology:measurement(prep_tr_t16, preparedness_commitment__husk_reading, theater_ratio, 16, 0.74).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__husk_reading, theater_ratio, 20, 0.78).
narrative_ontology:measurement(prep_tr_t24, preparedness_commitment__husk_reading, theater_ratio, 24, 0.81).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__husk_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prep_be_t4, preparedness_commitment__husk_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(prep_be_t8, preparedness_commitment__husk_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(prep_be_t12, preparedness_commitment__husk_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(prep_be_t16, preparedness_commitment__husk_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__husk_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(prep_be_t24, preparedness_commitment__husk_reading, base_extractiveness, 24, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_commitment__husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language concept 'disaster preparedness commitment' per the epsilon-invariance principle. The husk_reading (this file, ε=0.68, piton-shaped) claims operational competence has decoupled from compliance artifacts. The competence_reading (separate file, expected low ε, rope-shaped) claims the drills genuinely transmit live operational skill. The hybrid_reading (separate file, expected moderate ε, tangled_rope-shaped) claims memorial and competence functions coexist and are both partially real. All three share the same underlying kernel (the preparedness-commitment institution) but are structurally distinct claims about its actual operation, each requiring its own stakeholder structure and metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
