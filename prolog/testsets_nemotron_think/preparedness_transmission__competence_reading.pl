% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__competence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: preparedness_transmission__competence_reading
 *   human_readable: Drills and Inspections as Live Exercised Knowledge
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   This constraint models the 'competence reading' of the
 *   preparedness_transmission kernel: the view that drills and inspections
 *   function as live exercised knowledge, where each generation of
 *   practitioners re-validates capability through varied practice. The system
 *   exhibits high adaptive capacity — inspectors recognize novel failure
 *   signatures because they evaluate diverse implementations; drill
 *   participants improvise effectively under scenario variation because they
 *   have internalized principles, not scripts. This reading claims the
 *   arrangement is a genuine coordination mechanism (rope) with minimal
 *   extraction, sustained by demonstrated utility rather than coercion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__competence_reading, 0.12).
domain_priors:suppression_score(preparedness_transmission__competence_reading, 0.08).
domain_priors:theater_ratio(preparedness_transmission__competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__competence_reading, rope).
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Drills and Inspections as Live Exercised Knowledge").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "disaster_risk_management/institutional_memory/civil_defense").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, '2f694aab-0017-47c6-b515-5d243e46a1eb').
narrative_ontology:cs_kernel_codification('2f694aab-0017-47c6-b515-5d243e46a1eb', distributed).
narrative_ontology:cs_authority_grounding('2f694aab-0017-47c6-b515-5d243e46a1eb', practice).
narrative_ontology:cs_interpretation_layer_present('2f694aab-0017-47c6-b515-5d243e46a1eb').
narrative_ontology:cs_reading_relation('2f694aab-0017-47c6-b515-5d243e46a1eb', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f694aab-0017-47c6-b515-5d243e46a1eb', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('2f694aab-0017-47c6-b515-5d243e46a1eb', foundational, exercised_practice_validates_capability).
narrative_ontology:cs_axiom_status(exercised_practice_validates_capability, holdable).
narrative_ontology:cs_axiom_grounding('2f694aab-0017-47c6-b515-5d243e46a1eb', exercised_practice_validates_capability, empirically_contingent).
narrative_ontology:cs_axiom('2f694aab-0017-47c6-b515-5d243e46a1eb', foundational, adaptive_improvisation_indicates_live_knowledge).
narrative_ontology:cs_axiom_status(adaptive_improvisation_indicates_live_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('2f694aab-0017-47c6-b515-5d243e46a1eb', adaptive_improvisation_indicates_live_knowledge, empirically_contingent).
narrative_ontology:cs_reference_frame('2f694aab-0017-47c6-b515-5d243e46a1eb', continuous_practice_validation).
narrative_ontology:cs_drift_state('2f694aab-0017-47c6-b515-5d243e46a1eb', post_cold_war_atrophy, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('2f694aab-0017-47c6-b515-5d243e46a1eb', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, emergency_responders).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, civilian_population).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, inspectors_auditors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, political_leadership).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, emergency_responders).
narrative_ontology:constraint_vindicates(preparedness_transmission__competence_reading, exercised_practice_transmits_operational_knowledge).
narrative_ontology:constraint_vindicates(preparedness_transmission__competence_reading, adaptive_capacity_requires_regular_validation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, mandate, and resource the drill and inspection regime. Their legitimacy depends on demonstrating that the system produces real capability. They set scenarios, define pass/fail criteria, and allocate budgets for exercises.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Participate in drills as primary trainees. They invest significant time and operational capacity in exercises but gain validated competence, muscle memory for crisis procedures, and professional credibility. Exit means leaving the profession or accepting skill atrophy.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, emergency_responders, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, emergency_responders, payer).

% Conduct inspections and evaluate drill performance. Their professional standing depends on accurately distinguishing genuine capability from performative compliance. They recognize novel failure signatures because they see diverse implementations across jurisdictions.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, inspectors_auditors, beneficiary,
    organized, biographical, mobile, national).

% The ultimate beneficiaries of effective disaster response. They bear no direct cost of drills but depend entirely on the system's competence when crisis hits. No exit option — they cannot opt out of disaster risk.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, civilian_population, beneficiary,
    powerless, biographical, trapped, national).

% Authorize funding and public visibility for preparedness exercises. Gain political credit for demonstrated readiness. Can redirect attention and resources to other priorities between crises.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, political_leadership, agenda_setter,
    institutional, immediate, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, political_leadership, beneficiary).

% Study drill outcomes, knowledge transfer mechanisms, and institutional learning curves. Provide independent validation of whether exercises produce adaptive capacity or ritual compliance.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, academic_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits operational knowledge across generations of practitioners through repeated, varied practice — ensuring that when novel crises emerge, responders can improvise effectively because they have exercised the underlying principles, not just memorized scripts.
% TRANSFER_FUNCTION: Moves tacit operational knowledge from experienced practitioners to new cohorts via structured scenario exposure; moves validation authority from inspectors to the system by making competence visible in performance; moves resource allocation toward proven capabilities.
% ABSENT_VOICES: Communities that have never experienced a major disaster and therefore cannot validate whether drills match their specific vulnerabilities; future generations who will inherit the knowledge stock but have no say in current exercise design; marginalized populations whose evacuation and sheltering needs may not appear in standard scenarios.
% DISAPPEARANCE_RATIONALE: If exercised-practice transmission vanished, operational knowledge would degrade to procedural manuals within one generation. Novel failure signatures would go unrecognized. Improvisation capacity would collapse. Disaster response would become rigid and brittle, increasing casualties in non-standard events.
% FOUNDING_PROBLEM: After major disasters (e.g., 1906 San Francisco earthquake, 1923 Great Kanto earthquake), it became clear that written plans and static training failed when conditions deviated from expectations. The founding problem: how to maintain capability for unforeseen scenarios when the only reliable teacher is experience, but experience is too costly to wait for.
% FOUNDING_PROBLEM_CORROBORATION: After-action reports from recent disasters (2011 Tohoku, 2023 Turkey-Syria earthquakes, 2021 Pacific Northwest heat dome) consistently show that jurisdictions with regular, varied exercise programs improvised effectively while those relying on static plans failed. Independent researchers (e.g., NEHRP, JRC, academic disaster studies centers) corroborate that exercised practice correlates with adaptive outcomes.
narrative_ontology:disappearance_verdict(preparedness_transmission__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_transmission__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__competence_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__competence_reading_tests).
:- end_tests(preparedness_transmission__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the primary flow is knowledge and capability, not resource extraction. Participants pay with time but receive validated competence. Suppression is minimal (0.08) — alternatives (private training, academic programs) exist and are not blocked; the drill system persists because it works. Theater ratio is low (0.15) — exercises have measurable performance criteria and failure has consequences. Accessibility collapse is moderate (0.35) — while other training paths exist, the institutional drill system remains the dominant validated pathway. Resistance is low (0.18) — professional communities largely endorse the system because it produces visible competence.
 *
 * PERSPECTIVAL GAP:
 *   The competence reading and husk reading describe the SAME institutional forms (drills, inspections) but from different positions in the capability distribution. An agency with high adaptive capacity experiences the constraint as rope; one with hollowed-out knowledge experiences it as piton or snare. The engine computes this divergence from the stakeholder power/exit/scope data — the authored claim (rope) reflects the competence reading's epistemic position, not a universal verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Emergency management agencies and political leadership are agenda_setters with institutional power and analytical/arbitrage exit — they shape the constraint and can redirect resources. Emergency responders are primary beneficiaries (gain competence) but also payers (invest time) with constrained exit (career-bound). Inspectors are beneficiaries with mobile exit — their expertise is portable. Civilian population is a powerless beneficiary with trapped exit — they cannot opt out of disaster risk. Academic researchers are analytical observers. The directionality derivation from these structural positions yields low effective extraction for all seats except possibly political leadership (who extract political credit), but their arbitrage exit and immediate time horizon limit sustained extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining capability for unforeseen scenarios) remains live — recent disasters confirm that static plans fail and exercised practice succeeds. The constraint has not outlived its function. Mandatrophy is not resolved because the problem persists and the solution remains effective. The slight uptick in theater_ratio and suppression_requirement post-2015 warrants monitoring (compliance documentation burdens, scenario standardization pressures) but does not yet indicate functional decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_husk_boundary,
    'At what threshold of adaptive failure does a drill system cross from competence_reading to husk_reading? Is the boundary sharp or gradual?',
    'Longitudinal tracking of drill-to-event performance correlation: when exercises cease to predict novel-crisis improvisation, the reading shifts. Requires independent after-action analysis across multiple disaster cycles.',
    'If the boundary is gradual, most systems exist in a hybrid state and the competence reading is an idealization. If sharp, the classification cleanly separates functional from ritual systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_husk_boundary, empirical, 'Whether the competence/husk distinction is a clear classification boundary or a spectrum.').

omega_variable(
    stratification_mechanism,
    'Why does the hybrid_reading observe stratification (engineering competence high, civilian coordination decayed)? Is this a structural feature of different knowledge domains or a resource allocation artifact?',
    'Comparative analysis of engineering vs. civilian coordination drill designs: do they differ in scenario variation, inspector expertise, feedback loops, or institutional accountability?',
    'If structural, the competence reading applies only to domains with certain epistemic properties (quantifiable failure modes, clear standards). If artifact, resource reallocation could restore civilian coordination competence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stratification_mechanism, conceptual, 'Whether the hybrid_reading''s stratification reflects an irreducible epistemic difference or a contingent institutional choice.').

omega_variable(
    reading_framing_underdetermination,
    'Does the competence reading describe a real structural property of the drill system, or does it reflect the self-justifying framing of agencies that control the exercises?',
    'Blind evaluation: have independent observers assess drill performance and crisis outcomes without knowing which reading the agency endorses. Compare agencies that claim competence vs. those acknowledging decay.',
    'If the competence reading is a self-serving frame, its low extractiveness metrics may reflect measurement blind spots. If structurally grounded, the metrics are reliable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Whether the competence reading''s positive assessment is structurally warranted or an institutional self-portrait.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__competence_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1950, preparedness_transmission__competence_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(prep_tr_t1970, preparedness_transmission__competence_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(prep_tr_t1990, preparedness_transmission__competence_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(prep_tr_t2005, preparedness_transmission__competence_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(prep_tr_t2015, preparedness_transmission__competence_reading, theater_ratio, 2015, 0.14).
narrative_ontology:measurement(prep_tr_t2025, preparedness_transmission__competence_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(prep_be_t1950, preparedness_transmission__competence_reading, base_extractiveness, 1950, 0.18).
narrative_ontology:measurement(prep_be_t1970, preparedness_transmission__competence_reading, base_extractiveness, 1970, 0.14).
narrative_ontology:measurement(prep_be_t1990, preparedness_transmission__competence_reading, base_extractiveness, 1990, 0.11).
narrative_ontology:measurement(prep_be_t2005, preparedness_transmission__competence_reading, base_extractiveness, 2005, 0.1).
narrative_ontology:measurement(prep_be_t2015, preparedness_transmission__competence_reading, base_extractiveness, 2015, 0.12).
narrative_ontology:measurement(prep_be_t2025, preparedness_transmission__competence_reading, base_extractiveness, 2025, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1950, preparedness_transmission__competence_reading, suppression_requirement, 1950, 0.15).
narrative_ontology:measurement(prep_su_t1970, preparedness_transmission__competence_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(prep_su_t1990, preparedness_transmission__competence_reading, suppression_requirement, 1990, 0.07).
narrative_ontology:measurement(prep_su_t2005, preparedness_transmission__competence_reading, suppression_requirement, 2005, 0.06).
narrative_ontology:measurement(prep_su_t2015, preparedness_transmission__competence_reading, suppression_requirement, 2015, 0.08).
narrative_ontology:measurement(prep_su_t2025, preparedness_transmission__competence_reading, suppression_requirement, 2025, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__competence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__competence_reading, 0.06).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, disaster_response_coordination).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, critical_infrastructure_resilience).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, public_trust_in_institutions).

% DUAL FORMULATION NOTE:
% This constraint (competence_reading) and its siblings (husk_reading, hybrid_reading) form a constraint family decomposing the 'preparedness_transmission' kernel. The competence reading has low epsilon (0.12) because it evaluates the system where it functions; the husk reading would have higher epsilon where the same forms extract compliance without delivering capability; the hybrid reading would show domain-stratified epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_transmission__competence_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
