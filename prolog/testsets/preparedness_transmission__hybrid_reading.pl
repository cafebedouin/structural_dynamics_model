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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_transmission__hybrid_reading
 *   human_readable: Stratified Preparedness Transmission: Engineering Competence and Coordination Decay
 *   domain: institutional/disaster_management
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid reading of the
 *   preparedness_transmission kernel: the core assertion is that formal
 *   preparedness transmission is STRATIFIED — competence in the physical
 *   layer (dikes, early warning systems, shelter infrastructure) persists
 *   because it is embedded in hands-on engineering practice and continuous
 *   equipment interaction, while competence in the coordination layer
 *   (evacuation protocols, population assembly, civilian communication under
 *   stress) has decayed because it depends on periodic mass drills that have
 *   become increasingly ritualized and disconnected from actual population
 *   distribution, language diversity, and accessibility needs. The constraint
 *   coordinates engineering knowledge effectively but extracts coordination
 *   failure from the civilian population. It benefits the agencies that
 *   administer preparedness certification (they maintain institutional
 *   legitimacy regardless of real-world outcomes) and the specialized
 *   engineering workforce (whose indispensability is continuously validated).
 *   It harms the populations who must evacuate without having internalized
 *   the coordination knowledge and the emergency responders who discover the
 *   failure in the actual event.
 *
 * KEY AGENTS:
 *   - engineering_infrastructure_operators: maintain physical systems through continuous practice; competence chain unbroken
 *   - civil_defense_bureaucrats: administer formal preparedness transmission; benefit from institutional continuity
 *   - civilian_population: subject to evacuation orders; excluded from preparedness planning; trapped
 *   - vulnerable_demographics: bear highest casualty risk when coordination fails
 *   - emergency_responders: execute coordination under stress; discover the decay gap
 *   - disaster_risk_researchers: produce evidence of stratification and decay
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.48).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.41).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Stratified Preparedness Transmission: Engineering Competence and Coordination Decay").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "institutional/disaster_management").

domain_priors:requires_active_enforcement(preparedness_transmission__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, 'cddb3d9c-09d4-45a5-b010-bc9bc1d3fa36').
narrative_ontology:cs_kernel_codification('cddb3d9c-09d4-45a5-b010-bc9bc1d3fa36', fixed_text).
narrative_ontology:cs_authority_grounding('cddb3d9c-09d4-45a5-b010-bc9bc1d3fa36', lineage).
narrative_ontology:cs_interpretation_layer_present('cddb3d9c-09d4-45a5-b010-bc9bc1d3fa36').
narrative_ontology:cs_reading_relation('cddb3d9c-09d4-45a5-b010-bc9bc1d3fa36', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('cddb3d9c-09d4-45a5-b010-bc9bc1d3fa36', preparedness_transmission__husk_reading, influences).
narrative_ontology:cs_axiom('cddb3d9c-09d4-45a5-b010-bc9bc1d3fa36', foundational, preparedness_transmission_is_stratified).
narrative_ontology:cs_axiom_status(preparedness_transmission_is_stratified, holdable).
narrative_ontology:cs_axiom_grounding('cddb3d9c-09d4-45a5-b010-bc9bc1d3fa36', preparedness_transmission_is_stratified, empirically_contingent).
narrative_ontology:cs_axiom('cddb3d9c-09d4-45a5-b010-bc9bc1d3fa36', foundational, engineering_competence_persists_through_practice).
narrative_ontology:cs_axiom_status(engineering_competence_persists_through_practice, holdable).
narrative_ontology:cs_axiom_grounding('cddb3d9c-09d4-45a5-b010-bc9bc1d3fa36', engineering_competence_persists_through_practice, empirically_contingent).
narrative_ontology:cs_axiom('cddb3d9c-09d4-45a5-b010-bc9bc1d3fa36', foundational, coordination_competence_decays_without_integrated_training).
narrative_ontology:cs_axiom_status(coordination_competence_decays_without_integrated_training, holdable).
narrative_ontology:cs_axiom_grounding('cddb3d9c-09d4-45a5-b010-bc9bc1d3fa36', coordination_competence_decays_without_integrated_training, empirically_contingent).
narrative_ontology:cs_reference_frame('cddb3d9c-09d4-45a5-b010-bc9bc1d3fa36', unified_competence_transmission).
narrative_ontology:cs_drift_state('cddb3d9c-09d4-45a5-b010-bc9bc1d3fa36', contemporary_post_disaster_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cddb3d9c-09d4-45a5-b010-bc9bc1d3fa36', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, engineering_infrastructure_operators).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, civil_defense_bureaucrats).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, civilian_population).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, emergency_responders).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, vulnerable_demographics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and operate physical infrastructure (dikes, levees, shelters, early warning systems). Their competence chain is unbroken because technical knowledge transmits through apprenticeship, certification, and continuous equipment interaction. They benefit from the constraint because it legitimates their budget allocation and institutional authority — they are the seats that actually execute when physical systems activate. Their exit is constrained by the irreplaceability of their specialized knowledge.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, engineering_infrastructure_operators, beneficiary,
    institutional, generational, constrained, national).

% Set evacuation protocols, organize drills, maintain planning documents, and adjudicate preparedness certification. They benefit from the institutional continuity the constraint provides — the perception of readiness legitimates their administrative role and budget. They administer the formal knowledge transmission (drills, training materials, exercise scenarios). Their exit options are substantial: they move between agencies, retire, or transition to private security consulting.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civil_defense_bureaucrats, agenda_setter,
    institutional, biographical, mobile, national).

% Subject to evacuation orders that assume coordination knowledge (landmarks, assembly points, roles, expected behavior) that is not evenly distributed across the population. They bear the extraction cost when coordination fails: confusion, dispersal, preventable casualties. Their exit is trapped — they cannot opt out of exposure to disaster risk in their geographic location. They are excluded from preparedness planning in most jurisdictions; they encounter the constraint only in crisis, when coordination knowledge should already be internalized.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civilian_population, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__hybrid_reading, civilian_population, excluded).

% Execute evacuation, rescue, and coordination in the actual event. They discover the coordination decay when formal drills don't reflect real population distribution, actual communication infrastructure (cellphone congestion, language barriers), or civilian knowledge of routes and assembly points. They bear the operational cost of compensating for coordination failure and suffer the morale impact of preventable deaths. Their exit is constrained by professional duty and licensing; they cannot easily leave the jurisdiction.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, emergency_responders, payer,
    moderate, biographical, constrained, national).

% Elderly residents, non-fluent language speakers, disabled persons, and people living in informal settlements. Coordination decay hits them hardest because they depend most heavily on accessible, culturally-specific communication and physical accessibility of assembly points and shelter. They have zero exit options; they face the highest casualty risk when coordination fails.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, vulnerable_demographics, payer,
    powerless, immediate, trapped, local).

% Study preparedness transmission, post-disaster outcomes, and the gap between formal drill performance and real-world coordination. They produce evidence of the stratification (engineering competence persisting while coordination knowledge decays). Their analytical position is independent of the constraint's operation.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, disaster_risk_researchers, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__hybrid_reading, civil_defense_bureaucrats).
narrative_ontology:fixing_cost_class(preparedness_transmission__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preparedness transmission solves the problem of continuous knowledge maintenance across generational turnover in a domain where the knowledge is exercised infrequently. The constraint coordinates between engineering specialists (who re-validate physical systems through hands-on interaction) and civilian population (who must internalize behavioral protocols to coordinate under stress). Formal drills and inspections maintain institutional memory and validate system states.
% TRANSFER_FUNCTION: The constraint moves operational burden from the prepared (those who retained coordination knowledge through drills) to the unprepared (those who encounter the event with only residual, fragmented knowledge). It also transfers institutional legitimacy and budget from civilian safety outcomes to agency compliance with procedural requirements — drills are performed, documents filed, certifications issued, regardless of real-world coordination capacity.
% ABSENT_VOICES: Survivors of preparedness failures, casualties from coordination breakdowns in recent events, and informal-settlement residents who are structurally excluded from preparedness planning would object that the constraint performs readiness theater while actual evacuation coordination remains underdeveloped. Jurisdictions where preparedness transmission has successfully maintained BOTH engineering and coordination knowledge (rare cases) could testify that stratification is not inevitable but chosen — that integrated transmission is possible and that the decay reflects resource allocation to infrastructure over coordination training.
% DISAPPEARANCE_RATIONALE: If the formal constraint on preparedness transmission vanished — no more drills, no more certification, no more bureaucratic validation — the immediate effect would be visible chaos in procedures and paperwork. Physical infrastructure would continue operating because the competence is embedded in equipment interaction and apprenticeship (it would persist unmonitored, until a major failure exposes hidden decay). Civilian coordination would initially improve, paradoxically, because the formal pretense of readiness would lift and actual community-based mutual-aid networks would emerge from their suppressed state. Over a decade, infrastructure competence would degrade without inspection and fresh training; coordination might stabilize into lived neighborhood knowledge if not suppressed by new institutional forms.
% FOUNDING_PROBLEM: Post-World War II civil defense systems were built to maintain coordinated evacuation and shelter capacity across generational turnover in case of catastrophic events (nuclear war, major flooding, industrial disasters). The founding problem was institutional memory: how to keep a whole civilian population aligned on protocols and routes when the knowledge is exercised rarely and the population is constantly refreshed by births and migration.
% FOUNDING_PROBLEM_CORROBORATION: Civil defense authorities in developed nations attest the founding problem remains live and that preparedness transmission is essential. Disaster researchers and post-event analysts in jurisdictions that experienced major flooding or earthquakes testify that the problem has shifted: engineering systems performed as designed but civilian coordination failed, suggesting the founding problem for civil coordination is incompletely solved while the engineering problem is largely solved. Vulnerable populations testify they receive minimal preparation knowledge; their voices are absent from most official preparedness assessments but appear in disaster ethnography and community health literature.
narrative_ontology:disappearance_verdict(preparedness_transmission__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(preparedness_transmission__hybrid_reading, 'none', 1).

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
 *   The extraction score (0.48, rising to 0.48 over the interval) reflects the asymmetric harm: agencies gain institutional legitimacy from the appearance of preparedness, while the civilian population bears the operational risk of coordination failure. The theater ratio (0.62) is the central diagnostic: it indicates that more than half the measured preparedness activity is performative ceremony rather than functional knowledge transmission. Drills are conducted, certifications issued, plans filed — but the drills do not reach the populations who need the knowledge most (vulnerable demographics, those in informal settlements), and the plans do not reflect actual population distribution or language diversity. Suppression (0.41) is moderate because the constraint does not require overt coercion — it operates through institutional procedure and the invisibility of coordination decay until the crisis moment. The stratification itself suppresses alternatives: as long as the engineering layer performs, the coordination decay is not visible to external auditors; as long as drills are conducted, the absence of real knowledge internalization is not acknowledged. Accessibility collapse (0.58) reflects that civilians trapped in the geographic zone have no way to opt out of the preparedness constraint's operation, while alternatives (community-based mutual aid, informal evacuation networks) are structurally suppressed by the formal system's monopoly on official coordination authority. Resistance (0.72) is high because emergency responders, disaster researchers, and survivors of preparedness failures actively challenge the constraint's framing; the resistance is real but institutionally muted (not translated into formal policy change). The measurement series track extractiveness rising slowly (as coordination decay accumulates and the gap between engineering and coordination competence widens) and theater ratio rising faster (as the performative fraction of drills increases relative to real knowledge transmission).
 *
 * PERSPECTIVAL GAP:
 *   The civil_defense_bureaucrats and engineering_infrastructure_operators sit at the beneficiary end of the directionality gradient (d ~ 0.2–0.3): from their position, the constraint coordinates real knowledge and maintains institutional readiness. The civilian_population and vulnerable_demographics sit at the target end (d ~ 0.85–0.95): from their position, the same constraint is a transfer mechanism that imposes evacuation obligations without providing the knowledge needed to execute them. Emergency responders occupy an intermediate position (d ~ 0.65): they benefit from physical infrastructure competence but pay heavily for coordination failure. The engine should compute divergent classifications across these seats: the beneficiary seats may compute as Rope or Tangled Rope, while the target seats should compute as Snare or Tangled Rope with high extraction. This divergence is the measurement the constraint story exists to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary group (engineering_infrastructure_operators, civil_defense_bureaucrats) receives institutional legitimacy, budget allocation, and professional authority from the constraint. They participate in its administration and benefit from its continuation. Their directionality is low (full beneficiary range, d ~ 0.15–0.35). The victim group (civilian_population, vulnerable_demographics) is trapped geographically, excluded from planning, and bears the operational risk when coordination fails. Their directionality is high (d ~ 0.85–0.95). Emergency responders are partially beneficiaries (they gain from infrastructure competence) and partially victims (they pay the coordination failure cost and suffer the morale impact). No override is needed; the structural derivation from beneficiary/victim + power + exit produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to maintain coordinated preparedness knowledge across generational turnover. The founding problem (preserving institutional memory in a domain exercised rarely) is partially addressed and partially abandoned. The engineering layer successfully solves the mandate (competence is maintained). The coordination layer fails to solve it (knowledge has decayed, drills are theatrical, populations are unprepared). The constraint persists because the engineering success legitimates the whole apparatus, while the coordination failure is invisible until crisis and is then attributed to unforeseeable circumstances rather than transmission decay. This is a classic mandatrophy: the constraint's original function is partially obsolete (engineering preparedness is maintained; coordination preparedness is not), but the institutional apparatus persists because it captures rents (legitimacy, budget) from the appearance of readiness regardless of outcome. The remedy would require disaggregating the engineering and coordination layers, measuring actual coordination competence in the civilian population, and reallocating resources from drill theater to accessible, culturally-specific, population-integrated coordination training.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stratification_origin_mechanism,
    'What structural factors cause engineering competence to persist while coordination competence decays? Is the stratification a feature of knowledge types (technical knowledge transmits through apprenticeship and equipment; procedural knowledge requires repeated mass-scale coordination practice that is rare and expensive) or a feature of incentives (agencies prefer to invest in visible infrastructure over invisible civilian preparation)?',
    'Comparative analysis of preparedness transmission in jurisdictions with differing resource allocation (high investment in community-scale coordination training vs. low investment). Post-event analysis of actual evacuation outcomes correlated with pre-event training investment.',
    'If the stratification is inherent to knowledge type, remedies must target the rare-event problem directly (more frequent, accessible drills; culturally-specific coordination training). If it is incentive-driven, remedies target institutional accountability (outcome metrics tied to actual evacuation performance, not drill compliance). The classification remains Tangled Rope either way, but the intervention point shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_origin_mechanism, empirical, 'Whether stratification is structural-epistemic or institutionally-contingent.').

omega_variable(
    coordination_decay_vs_invisibility,
    'Has coordination knowledge actually decayed, or has it persisted in informal networks (neighbor knowledge, community assembly points, mutual aid) while the formal system''s invisibility of this informal competence is the true failure? Is the constraint suppressing real coordination knowledge by monopolizing the official narrative?',
    'Ethnographic study of actual evacuation and shelter use in informal communities. Post-event interviews with populations who successfully coordinated without formal knowledge. Measurement of informal coordination knowledge (do neighbors know assembly points, do communities have mutual aid structures) vs. formal knowledge (do residents know official protocols).',
    'If informal coordination knowledge persists and is suppressed by the formal system''s monopoly, the classification shifts: the constraint becomes a Snare (pure suppression of alternatives). If informal knowledge has also decayed, the Tangled Rope classification holds. Either way, the remedy involves recognizing and amplifying informal coordination networks rather than replacing them with formal systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_decay_vs_invisibility, empirical, 'Whether coordination decay is absolute or relative to suppression of informal alternatives.').

omega_variable(
    hybrid_reading_vs_husk_reading_boundary,
    'At what point does the stratification become so severe that the constraint shifts from hybrid_reading (stratified competence) to husk_reading (purely ritualized)? When does the engineering layer''s competence-based legitimacy no longer sustain the coordination layer''s ritualization?',
    'Longitudinal measurement of the theater_ratio and extraction_gap (engineering performance vs. coordination performance) over decades. Threshold identification: when theater_ratio reaches 0.75+ and extraction_gap widens beyond a specified margin, the reading-drift hypothesis triggers and reclassification to husk_reading is indicated.',
    'This omega documents the potential trajectory of the constraint: hybrid_reading is a metastable state that may degrade toward husk_reading as theater increases and the gap between performed and actual competence widens. If that degradation occurs, the classification should update to capture the increasing-extraction (and structural-dishonesty) profile. The measurement series provide the data to detect this drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_reading_vs_husk_reading_boundary, conceptual, 'Stability of the hybrid_reading relative to potential drift toward husk_reading.').

omega_variable(
    interplay_between_readings,
    'The three readings of this kernel (competence_reading, husk_reading, hybrid_reading) are not equally contested. The competence_reading reflects the official consensus of civil defense authorities. The husk_reading reflects a critical minority view (disaster researchers, communities that experienced failures). This hybrid_reading reflects an emergent synthesis: can all three readings coexist in the same discourse, or does acceptance of the hybrid reading foreclose the competence reading?',
    'Discourse analysis: do authorities that acknowledge stratification (hybrid_reading) simultaneously defend the claim that preparedness transmission is live and exercised (competence_reading), or do they retreat to a narrower claim that only the engineering layer is live? Policy change tracking: do jurisdictions that adopt the hybrid_reading language shift resource allocation away from drill-theater toward community coordination training, or do they absorb the hybrid framing while maintaining the status quo?',
    'If the hybrid reading forecloses the competence reading (authorities cannot simultaneously assert that preparedness is live while acknowledging stratification), then these readings are in logical conflict rather than coexistence. If they can coexist (authorities use hybrid framing for internal assessment while maintaining competence framing in public discourse), then the kernel exhibits factionalism: different audiences are given different readings. This informs the cs_structure.reading_relations value (currently authored as coexists_with; could be influences or even a partial foreclose if the logic is tighter).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interplay_between_readings, conceptual, 'Logical and discursive relationship between the three readings of the preparedness_transmission kernel.').

omega_variable(
    suppression_internalization_ambiguity,
    'The measured suppression (0.41) reflects the institutional monopoly on official preparedness narrative and the structural exclusion of vulnerable populations from planning. Is this suppression structural (you cannot opt out of geographic disaster risk, you cannot access official coordination information, formal channels exclude informal knowledge) or internalized (vulnerable populations have accepted their exclusion from preparedness planning, they do not articulate alternatives, they treat evacuation coordination as outside their responsibility)?',
    'Post-event interviews with vulnerable populations: Do they express frustration at being excluded and unchosen, or do they express fatalism and acceptance? Do they describe informal coordination networks they maintain despite formal exclusion, or do they defer entirely to formal systems? Community organizing attempts: does investment in accessible preparedness information lead to rapid uptake and active participation, or do cultural/psychological barriers persist after structural barriers are removed?',
    'If suppression is structural only, remedies focus on access and inclusion. If suppression is partially internalized, remedies must address both structural barriers and recovered agency in communities. If the constraint''s persistence depends on internalized suppression, it is more deeply extractive than the authored suppression metric suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Mechanism of suppression: structural vs. internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__hybrid_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement_basis(prep_tr_t0, projected).
narrative_ontology:measurement(prep_tr_t8, preparedness_transmission__hybrid_reading, theater_ratio, 8, 0.53).
narrative_ontology:measurement_basis(prep_tr_t8, observed).
narrative_ontology:measurement(prep_tr_t16, preparedness_transmission__hybrid_reading, theater_ratio, 16, 0.57).
narrative_ontology:measurement_basis(prep_tr_t16, observed).
narrative_ontology:measurement(prep_tr_t24, preparedness_transmission__hybrid_reading, theater_ratio, 24, 0.6).
narrative_ontology:measurement_basis(prep_tr_t24, observed).
narrative_ontology:measurement(prep_tr_t32, preparedness_transmission__hybrid_reading, theater_ratio, 32, 0.61).
narrative_ontology:measurement_basis(prep_tr_t32, observed).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__hybrid_reading, theater_ratio, 40, 0.62).
narrative_ontology:measurement_basis(prep_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__hybrid_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(prep_be_t0, projected).
narrative_ontology:measurement(prep_be_t8, preparedness_transmission__hybrid_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement_basis(prep_be_t8, observed).
narrative_ontology:measurement(prep_be_t16, preparedness_transmission__hybrid_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement_basis(prep_be_t16, observed).
narrative_ontology:measurement(prep_be_t24, preparedness_transmission__hybrid_reading, base_extractiveness, 24, 0.45).
narrative_ontology:measurement_basis(prep_be_t24, observed).
narrative_ontology:measurement(prep_be_t32, preparedness_transmission__hybrid_reading, base_extractiveness, 32, 0.47).
narrative_ontology:measurement_basis(prep_be_t32, observed).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__hybrid_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement_basis(prep_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__hybrid_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(prep_su_t0, projected).
narrative_ontology:measurement(prep_su_t8, preparedness_transmission__hybrid_reading, suppression_requirement, 8, 0.39).
narrative_ontology:measurement_basis(prep_su_t8, observed).
narrative_ontology:measurement(prep_su_t16, preparedness_transmission__hybrid_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement_basis(prep_su_t16, observed).
narrative_ontology:measurement(prep_su_t24, preparedness_transmission__hybrid_reading, suppression_requirement, 24, 0.41).
narrative_ontology:measurement_basis(prep_su_t24, observed).
narrative_ontology:measurement(prep_su_t32, preparedness_transmission__hybrid_reading, suppression_requirement, 32, 0.41).
narrative_ontology:measurement_basis(prep_su_t32, observed).
narrative_ontology:measurement(prep_su_t40, preparedness_transmission__hybrid_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement_basis(prep_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, disaster_response_institutional_preparedness).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, civilian_information_access__disaster_context).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, vulnerable_population_inclusion__civil_protection).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the preparedness_transmission kernel. The competence_reading (constraint_id: preparedness_transmission__competence_reading) asserts that preparedness transmission successfully maintains exercised knowledge across generations through continuous practice and validation. The husk_reading (constraint_id: preparedness_transmission__husk_reading) asserts that formal preparedness transmission has become purely ritualized with no underlying operational knowledge. This hybrid_reading asserts that both readings are partially true: the physical infrastructure layer maintains live, exercised competence, while the civilian coordination layer has decayed into ritualization. The three readings are linked via network.affects_constraints and should be analyzed as a constraint family. The hybrid reading is downstream of the other two in the sense that it explains how both can be partially empirically supported: different observers are looking at different layers of the same constraint and seeing different states of competence/decay.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
