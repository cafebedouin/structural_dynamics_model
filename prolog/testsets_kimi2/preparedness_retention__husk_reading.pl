% ============================================================================
% CONSTRAINT STORY: preparedness_retention__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   domain: governance/institutional_memory/disaster_preparedness
 *
 * SUMMARY:
 *   This constraint story instantiates the husk_reading of the
 *   preparedness_retention kernel, which treats institutional disaster
 *   preparedness as memorial performance rather than live competence. Under
 *   this reading, recurring drills, certification audits, and inspection
 *   regimes have decoupled from operational reality: they coordinate budget
 *   flows and public reassurance, but the tacit skills required for effective
 *   disaster response have atrophied. The constraint extracts by diverting
 *   resources and attention from competence-building to ceremony, while the
 *   benefiting institutions gain diffuse legitimacy. It is authored as a
 *   piton because the primary coordination function (maintaining readiness)
 *   has degraded into theatrical maintenance, and the arrangement persists by
 *   inertia rather than by concentrated rent capture.
 *
 * KEY AGENTS:
 *   - emergency_management_institutions: Agenda-setter (institutional/constrained) â administers drills and audits, gains legitimacy, could reform but faces institutional inertia
 *   - disaster_affected_populations: Primary target (powerless/trapped) â bear the cost when ritualized preparedness fails during actual disasters
 *   - frontline_responders: Secondary target (moderate/constrained) â must participate in rituals that displace competence-building training
 *   - competence_advocates: Excluded voice (moderate/constrained) â pushed out of standard-setting because live competence is hard to metricize
 *   - public_oversight_commissions: Analytical observer (institutional/analytical) â assess compliance metrics rather than operational competence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__husk_reading, 0.65).
domain_priors:suppression_score(preparedness_retention__husk_reading, 0.5).
domain_priors:theater_ratio(preparedness_retention__husk_reading, 0.88).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, theater_ratio, 0.88).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__husk_reading, piton).
narrative_ontology:human_readable(preparedness_retention__husk_reading, "Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_retention__husk_reading, "governance/institutional_memory/disaster_preparedness").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__husk_reading, 'addddcaa-bb50-43f9-8c0c-7ae96e7270b6').
narrative_ontology:cs_kernel_codification('addddcaa-bb50-43f9-8c0c-7ae96e7270b6', distributed).
narrative_ontology:cs_authority_grounding('addddcaa-bb50-43f9-8c0c-7ae96e7270b6', practice).
narrative_ontology:cs_interpretation_layer_present('addddcaa-bb50-43f9-8c0c-7ae96e7270b6').
narrative_ontology:cs_reading_relation('addddcaa-bb50-43f9-8c0c-7ae96e7270b6', preparedness_retention__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('addddcaa-bb50-43f9-8c0c-7ae96e7270b6', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('addddcaa-bb50-43f9-8c0c-7ae96e7270b6', foundational, memorial_performance_constitutes_preparedness).
narrative_ontology:cs_axiom_status(memorial_performance_constitutes_preparedness, holdable).
narrative_ontology:cs_axiom_grounding('addddcaa-bb50-43f9-8c0c-7ae96e7270b6', memorial_performance_constitutes_preparedness, conventional).
narrative_ontology:cs_axiom('addddcaa-bb50-43f9-8c0c-7ae96e7270b6', secondary, observable_compliance_priority).
narrative_ontology:cs_axiom_status(observable_compliance_priority, holdable).
narrative_ontology:cs_axiom_grounding('addddcaa-bb50-43f9-8c0c-7ae96e7270b6', observable_compliance_priority, conventional).
narrative_ontology:cs_reference_frame('addddcaa-bb50-43f9-8c0c-7ae96e7270b6', memorial_performance_regime).
narrative_ontology:cs_drift_state('addddcaa-bb50-43f9-8c0c-7ae96e7270b6', contemporary_reform_pressure, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('addddcaa-bb50-43f9-8c0c-7ae96e7270b6', '').
narrative_ontology:cs_kernel_id(preparedness_retention__husk_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, emergency_management_institutions).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, disaster_affected_populations).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, frontline_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer preparedness programs, design drills, and manage audit regimes. Gain budgetary continuity and political legitimacy from visible compliance outputs. Could theoretically reform toward competence-based models, but institutional inertia, career risk, and funding mechanisms tied to countable metrics lock them into ceremonial reproduction.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, emergency_management_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Rely on institutional preparedness for survival during disasters. Cannot verify whether drills translate into live competence until a catastrophic event occurs. Bear the full cost when memorial performance fails to produce operational response capacity.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, disaster_affected_populations, payer,
    powerless, immediate, trapped, local).

% Required to participate in recurring drills and certification rituals that consume training time without developing adaptive, tacit competence. Career advancement depends on audit-friendly metrics rather than demonstrated operational skill. Alternative training pathways exist conceptually but are underfunded and institutionally devalued.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, frontline_responders, payer,
    moderate, biographical, constrained, regional).

% Argue for realistic exercises, tacit-skill retention, and competence-based evaluation. Structurally excluded from standard-setting bodies because their recommendations resist easy metricization and threaten the audit-based funding model.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, competence_advocates, excluded,
    moderate, generational, constrained, national).

% Review preparedness budgets and outputs. Typically assess compliance against plan metrics and drill counts rather than operational competence, unintentionally reinforcing the ceremonial logic they oversee.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, public_oversight_commissions, observer,
    institutional, generational, analytical, national).

narrative_ontology:fixing_cost_class(preparedness_retention__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates institutional expectations, budget flows, and public reassurance around visible, repeatable preparedness activities, providing a shared vocabulary of checklists, drills, and certifications that align disparate agencies and funders.
% TRANSFER_FUNCTION: Moves material and cognitive resources from tacit skill development and adaptive capacity toward ceremonial drill performance, audit documentation, and compliance certification; transfers disaster risk from institutional accountability to future affected populations.
% ABSENT_VOICES: Competence-focused practitioners and disaster-affected communities are structurally absent from standard-setting: the former because live competence is hard to metricize, the latter because their testimony arrives only after failure, when the ritual has already been validated by compliance.
% DISAPPEARANCE_RATIONALE: If the memorial performance constraint vanished, emergency management institutions would lose their primary legitimacy mechanism and audit-based funding justification; frontline responders would reallocate time; oversight bodies would need new evaluative criteria. The sector would be forced to reorganize around actual competence or collapse into visible dysfunction.
% FOUNDING_PROBLEM: How to maintain institutional readiness for rare, high-consequence events in the absence of continuous operational demand; how to demonstrate accountability to funders and publics who cannot directly observe response competence.
% FOUNDING_PROBLEM_CORROBORATION: Academic studies in disaster sociology and organizational theory (outside the benefiting institutions) attest that drill-based metrics have decoupled from operational outcomes; post-disaster inquiries routinely find competence gaps despite perfect compliance records. Institutional beneficiaries assert the problem is still live.
narrative_ontology:disappearance_verdict(preparedness_retention__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_retention__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__husk_reading, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.65) is substantial because the constraint systematically redirects resources and cognitive investment away from adaptive capacity toward ceremonial compliance. Suppression (0.50) reflects structural suppression of competence-based alternatives via funding mechanisms and audit criteria, not direct coercion. Theater_ratio (0.88) is high and rising because the constraint's visible output (drills, certifications) is increasingly decoupled from its nominal function. Accessibility_collapse (0.55) captures that genuine competence alternatives are conceptually available but institutionally inaccessible. Resistance (0.30) is low because the ritual is widely internalized and reform advocates are excluded from agenda-setting. The temporal series show a monotonic drift from modest ritualization (t=0) to mature performance regime (t=40), consistent with institutional decoupling dynamics.
 *
 * PERSPECTIVAL GAP:
 *   The emergency_management_institutions seat experiences the constraint as necessary organizational maintenance and legitimacy preservation; from the disaster_affected_populations and frontline_responders seats, the same structure is experienced as dangerous absence of real capacity. The engine computes this divergence from beneficiary/victim declarations and exit asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Emergency management institutions are declared beneficiaries because they capture institutional legitimacy and budgetary continuity, yielding a low directionality value. Disaster affected populations and frontline responders are declared victims because they bear the risk and opportunity cost of atrophied competence, yielding high directionality. The institutions' exit is constrained by political and budgetary lock-in, but they retain agenda-setting power, preventing them from reading as full targets despite their constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â maintaining readiness in the absence of continuous operational demand â was genuine coordination. The classification as piton (rather than snare) reflects that the extraction is not driven by a concentrated beneficiary capturing rents, but by the inertia of a solution that has outlived its function. The high theater_ratio and the absence of a sunset clause or active reform agenda confirm that the constraint persists because it is easier to maintain the ritual than to rebuild competence. This prevents mislabeling the original coordination as pure extraction, while acknowledging that the current state is extractive in effect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_atrophy_counterfactual,
    'Would operational competence atrophy even in the absence of memorial performance regimes, simply because high-consequence events are rare?',
    'Natural experiment or cross-organizational comparison among agencies with similar risk profiles but different training intensities.',
    'If competence decays inevitably, the constraint''s extraction is lower and it may approximate a Mountain-like limit; if ritualization actively accelerates atrophy, extraction is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_atrophy_counterfactual, empirical, 'Whether ritualization causes competence loss or merely masks natural decay.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of competence-based alternatives structural (funding and audit criteria) or internalized (professional identity fused with ceremonial performance)?',
    'Post-reform trajectory observation: if competence-based training expands immediately when structural incentives shift, suppression was structural; if institutional culture resists, internalization is significant.',
    'Internalized suppression raises effective extraction because the constraint persists even after structural barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of competence alternatives.').

omega_variable(
    cs_framing_underdetermination,
    'Does the commitment-system framing of this constraint rest on the ritual practice itself, or on the legitimacy claim layered above the ritual?',
    'Comparative analysis of organizations that abandoned drills without losing legitimacy versus those that retained drills despite known incompetence.',
    'If legitimacy is the operative layer, the constraint is better modeled as identity_coordination; if the ritual is self-sustaining, it is practice-based extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative CS framing of ritual versus legitimacy layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_husk_tr_t0, preparedness_retention__husk_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(prep_husk_tr_t8, preparedness_retention__husk_reading, theater_ratio, 8, 0.5).
narrative_ontology:measurement(prep_husk_tr_t16, preparedness_retention__husk_reading, theater_ratio, 16, 0.62).
narrative_ontology:measurement(prep_husk_tr_t24, preparedness_retention__husk_reading, theater_ratio, 24, 0.72).
narrative_ontology:measurement(prep_husk_tr_t32, preparedness_retention__husk_reading, theater_ratio, 32, 0.8).
narrative_ontology:measurement(prep_husk_tr_t40, preparedness_retention__husk_reading, theater_ratio, 40, 0.88).

% Extraction over time
narrative_ontology:measurement(prep_husk_be_t0, preparedness_retention__husk_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(prep_husk_be_t8, preparedness_retention__husk_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(prep_husk_be_t16, preparedness_retention__husk_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(prep_husk_be_t24, preparedness_retention__husk_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(prep_husk_be_t32, preparedness_retention__husk_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(prep_husk_be_t40, preparedness_retention__husk_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(prep_husk_su_t0, preparedness_retention__husk_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(prep_husk_su_t8, preparedness_retention__husk_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(prep_husk_su_t16, preparedness_retention__husk_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(prep_husk_su_t24, preparedness_retention__husk_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(prep_husk_su_t32, preparedness_retention__husk_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(prep_husk_su_t40, preparedness_retention__husk_reading, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(preparedness_retention__husk_reading, competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_retention kernel decomposes into three structurally distinct constraints because the natural-language label 'preparedness' conflates live competence, memorial performance, and stratified retention. Each reading carries a different epsilon, beneficiary structure, and classification. This husk_reading models the extractive, atrophied pole.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
