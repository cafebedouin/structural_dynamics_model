% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: preparedness_transmission__hybrid_reading
 *   human_readable: Stratified Preparedness Transmission Regime (Hybrid Reading)
 *   domain: disaster risk management/institutional memory/civil defense
 *
 * SUMMARY:
 *   A fifty-year preparedness transmission regime — mandated drills, plan
 *   certification, inspection cadences, licensure, and public preparedness
 *   education — shows stratified decay under the hybrid reading. The
 *   engineering and physical layer genuinely transmits: each cohort of
 *   licensed engineers is trained and examined through live inspection and
 *   maintenance work, and the resulting infrastructure performs to design
 *   when events strike. The civilian coordination layer has decayed: drills
 *   continue on schedule and plans are filed, but the exercised knowledge no
 *   longer converts into operable evacuation, shelter, and
 *   neighborhood-communication capacity, so under stress the infrastructure
 *   holds while coordination fails. The D5 transmission break sits in the
 *   coordination layer, not the physical layer. Claim and metrics are
 *   authored independently: claimed_type tangled_rope states my structural
 *   belief that the arrangement carries a genuine coordination function (the
 *   physical layer) alongside asymmetric extraction (the decayed layer's
 *   costs) held up by active enforcement; the metric values state what I
 *   believe is descriptively true of its operation. The engine computes
 *   per-seat classifications from the structural data; divergence between
 *   claim and computed type is the measurement, not an error. KEY AGENTS (by
 *   structural relationship): see key_agents; the family relationship to the
 *   sibling readings is recorded in network.dual_formulation_note and
 *   commentary.kernel_context.
 *
 * KEY AGENTS:
 *   - emergency_management_agencies: agenda-setter and fiscal collector ([institutional]/[identity_locked]) — administers the drill-and-certification regime its budget and mandate depend on
 *   - civil_engineering_profession: primary beneficiary ([organized]/[mobile]) — its continuously re-validated competence is what the physical layer transmits
 *   - elected_officials: secondary beneficiary ([powerful]/[mobile], immediate horizon) — converts visible exercises into electoral credit
 *   - hazard_zone_residents: primary payer ([powerless]/[trapped]) — protected by the physical layer, exposed by the coordination layer
 *   - general_taxpayers: diffuse payer ([moderate]/[constrained]) — funds the whole apparatus, audits neither half
 *   - neighborhood_mutual_aid_networks: excluded voice ([powerless]/[constrained]) — carries the displaced everyday-coordination tradition
 *   - disaster_research_community: analytical observer ([analytical]/[analytical]) — documents the stratified pattern across events
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.62).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.48).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Stratified Preparedness Transmission Regime (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "disaster risk management/institutional memory/civil defense").

domain_priors:requires_active_enforcement(preparedness_transmission__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, 'b9a0221d-67f2-4ec5-a0ec-b0abc0970924').
narrative_ontology:cs_kernel_codification('b9a0221d-67f2-4ec5-a0ec-b0abc0970924', implicit).
narrative_ontology:cs_authority_grounding('b9a0221d-67f2-4ec5-a0ec-b0abc0970924', expertise).
narrative_ontology:cs_interpretation_layer_present('b9a0221d-67f2-4ec5-a0ec-b0abc0970924').
narrative_ontology:cs_reading_relation('b9a0221d-67f2-4ec5-a0ec-b0abc0970924', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('b9a0221d-67f2-4ec5-a0ec-b0abc0970924', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_axiom('b9a0221d-67f2-4ec5-a0ec-b0abc0970924', foundational, capability_transmission_stratifies_by_layer).
narrative_ontology:cs_axiom_status(capability_transmission_stratifies_by_layer, holdable).
narrative_ontology:cs_axiom_grounding('b9a0221d-67f2-4ec5-a0ec-b0abc0970924', capability_transmission_stratifies_by_layer, empirically_contingent).
narrative_ontology:cs_axiom('b9a0221d-67f2-4ec5-a0ec-b0abc0970924', secondary, coordination_decay_is_transmission_failure).
narrative_ontology:cs_axiom_status(coordination_decay_is_transmission_failure, holdable).
narrative_ontology:cs_axiom_grounding('b9a0221d-67f2-4ec5-a0ec-b0abc0970924', coordination_decay_is_transmission_failure, empirically_contingent).
narrative_ontology:cs_reference_frame('b9a0221d-67f2-4ec5-a0ec-b0abc0970924', stratified_transmission_baseline).
narrative_ontology:cs_drift_state('b9a0221d-67f2-4ec5-a0ec-b0abc0970924', contemporary_stress_event_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b9a0221d-67f2-4ec5-a0ec-b0abc0970924', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, civil_engineering_profession).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, elected_officials).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, hazard_zone_residents).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, general_taxpayers).
narrative_ontology:constraint_vindicates(preparedness_transmission__hybrid_reading, engineered_standards_transmission).
narrative_ontology:constraint_vindicates(preparedness_transmission__hybrid_reading, compliance_exercise_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the national drill calendar, certifies local emergency plans, and distributes preparedness grants to states and municipalities. Staffing and budget scale with the volume of exercises produced and plans reviewed. Internal doctrine treats completed exercise hours and filed plan updates as the measure of readiness. Stepping outside the exercise-and-certification frame would put its statutory mandate and funding line in question, so its planning horizon and self-conception are bound to the regime it administers.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__hybrid_reading, emergency_management_agencies, beneficiary).

% Licensed engineers staff the inspection, maintenance, and code-committee work that keeps dams, levees, bridges, and retrofits at design standard. Each cohort is trained, examined, and mentored through live projects, so material competence demonstrably passes between generations. Demand for this work is guaranteed by statute and by the aging asset base, and the skills port across industries.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civil_engineering_profession, beneficiary,
    organized, generational, mobile, national).

% Appropriate the preparedness budget, attend staged exercises, and cite exercise statistics in hearings and campaigns. Visible readiness yields electoral credit on election timescales, while the difference between exercised and operable capability surfaces on disaster timescales they may not occupy. Leaving office ends their relationship to the regime entirely.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, elected_officials, beneficiary,
    powerful, immediate, mobile, national).

% Live in floodplains, coastal storm zones, and wildland interfaces. They fund the apparatus through taxes, attend school and workplace drills, and are protected — successfully — by the physical layer: levees hold, retrofitted structures stand. When events exceed routine scale, evacuation routing, shelter assignment, and neighborhood communication are where outcomes fail. Moving out of the hazard zone is economically out of reach for many households.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, hazard_zone_residents, payer,
    powerless, biographical, trapped, regional).

% Fund the full apparatus without visibility into how spending splits between functional maintenance and exercise production. They bear the fiscal tail of coordination failures through disaster supplemental appropriations and recovery costs. Their main lever is blunt: electoral pressure applied long after budget decisions.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, general_taxpayers, payer,
    moderate, biographical, constrained, national).

% Informal resident networks that move neighbors, boats, chainsaws, food, and information in the first hours of an event. They operate outside the grant-and-certification system; liability rules and reimbursement structures route official resources to credentialed organizations instead. They are not seated in planning processes, though their members are the population the plans describe.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, neighborhood_mutual_aid_networks, excluded,
    powerless, generational, constrained, local).

% Compiles after-action findings across events and decades, separating infrastructure performance from coordination outcomes in the record. Publishes stratified assessments and skill-retention studies. Holds no appropriation stake and cannot move budgets; its influence runs through citation, testimony, and the occasional commission.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, disaster_research_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__hybrid_reading, emergency_management_agencies).
narrative_ontology:fixing_cost_class(preparedness_transmission__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a continuous, funded pipeline that keeps physical protective infrastructure at engineered standards — inspection cadences, maintenance budgets, licensure, and code committees solve the genuine problem of transmitting material competence across generations of practitioners. It also provides a common schedule, vocabulary, and legal framework linking response organizations across jurisdictions.
% TRANSFER_FUNCTION: Moves public appropriations and mandated civilian time into agencies, engineering work, and exercise production; moves operational risk onto residents in hazard zones, who absorb the difference between promised and delivered coordination when events strike; moves reputational credit to officials who preside over visible exercises.
% ABSENT_VOICES: Neighborhood mutual-aid organizers, elders carrying pre-bureaucratic coordination knowledge, and residents with lived disaster experience are outside the planning tables. Insurance actuaries who price the coordination gap publish but are not seated. The strongest objection — that repeated everyday cooperation, not episodic drills, is what builds coordination capacity — has no institutional venue.
% DISAPPEARANCE_RATIONALE: Without the apparatus, inspection and maintenance cadences lapse and the engineering transmission pipeline thins within a decade — levees, bridges, and retrofits degrade. Response organizations lose their common legal frame and funding. Civilians would need to rebuild mutual-aid coordination from scratch, likely after avoidable losses. Both layers rearrange, and they rearrange differently.
% FOUNDING_PROBLEM: Mid-twentieth-century civil defense confronted a standing hazard environment — nuclear attack planning, then recurring major floods, storms, and earthquakes — that killed rediscovery each generation: every disaster re-taught lessons that had been forgotten. The apparatus was built to make preparedness an inherited, continuously exercised capability rather than a post-catastrophe improvisation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: reinsurance and catastrophe-modeling firms independently price the widening gap between infrastructure performance and coordination outcomes; national climate assessments project rising hazard exposure; independent after-action researchers document the stratified pattern. The agencies whose budgets ride on the apparatus attest the problem's urgency but not the stratified diagnosis.
narrative_ontology:disappearance_verdict(preparedness_transmission__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_transmission__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__hybrid_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.62 is a blend over the two layers the hybrid reading separates: the physical layer operates near coordination-cost extraction (real service, real transmission), while the coordination layer takes appropriations and mandated time and returns compliance artifacts rather than operable skill, with the failure tail landing on residents. Suppression 0.48 reflects crowding-out rather than force: mandates, liability rules, and grant structures channel participation and resources through official channels, but community self-organization survives at the margins. Theater_ratio 0.44 blends functional engineering inspection with increasingly staged civilian exercises. Accessibility_collapse 0.35: understanding the split does not close exits — communities can self-organize and contract private engineering — but liability and funding friction keeps alternatives partial. Resistance 0.30: the apparatus enjoys broad legitimacy and its critics are fragmented across research and survivor communities. The temporal series run on one shared six-point grid (t=0..50, decade steps) with all three metrics authored at every point. The series are not monotone: post-event accountability windows (visible around t=20 and t=30) temporarily compress theater and extraction before decay resumes, and each cycle resets the baseline slightly higher than the last — a ratchet, not a stable oscillation. The oscillation itself is not the extraction mechanism; it is diagnostic of one: intermittent scrutiny is absorbed, and the secular trend underneath is upward. Coordination type identity_coordination: the dominant function whose failure would cause the transmission problem is maintenance of the membership-and-certification boundary that defines who counts as competent — licensure, exercise certification, plan approval. Default floor retained; no override justified.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the civil_engineering_profession seat the arrangement is experienced as genuine professional formation — live mentorship, examined competence, portable skill — and that seat's computed type should sit near rope. From the emergency_management_agencies seat the same regime is mission fulfillment fused with budget basis; its identity_locked exit means it cannot cheaply entertain the possibility that its coordination product has decayed. From the hazard_zone_residents seat the regime is protection that works until the moment protection is most needed, followed by abandonment of the coordination problem — that seat should compute near the extractive end, spiking at stress moments. The disaster_research_community seat sees the split itself. The engine derives these divergences from power, exit, and directionality; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: the engineering profession (mobile exit, organized power) sits nearest the beneficiary end — the regime subsidizes its formation and employment. Elected officials collect reputational return on an immediate horizon with full exit. Emergency_management_agencies are dual-positioned: agenda_setter with a beneficiary secondary role, deriving low d despite administering, because the appropriations accrue to them. Victims derive high directionality: general_taxpayers are diffuse payers with constrained exit, sitting near the full-target end. Hazard_zone_residents are the exception that defines this reading: a naive derivation from victim + trapped + powerless would push them toward full-target d, but the hybrid reading's defining fact is that the physical layer genuinely protects them — levees hold, retrofits stand — so their net position is high-but-not-full targeting. The directionality_overrides entry (powerless -> 0.78) encodes that partial subsidy; the same logic extends approximately to mutual-aid network members, who are drawn from the same protected-yet-exposed population. Neighborhood_mutual_aid_networks are excluded rather than coordinated: their displacement is what the enforcement machinery's grant and liability rules accomplish.
 *
 * MANDATROPHY ANALYSIS:
 *   The kernel contest is a mandatrophy dispute in miniature, and the classification choice determines what gets mislabeled. Reading the regime as uniformly competent (the competence_reading) launders the decayed layer's costs as ordinary coordination overhead — a false rope. Reading it as uniformly hollow (the husk_reading) erases the live physical layer and would misdirect reform at engineering transmission that still works — a false piton. The tangled_rope claim keeps both halves legible: the coordination-function gate is satisfied by the physical layer's genuine transmission, the asymmetric-extraction requirement by the coordination layer's decayed output, and requires_active_enforcement by the mandate, certification, and liability machinery that holds the whole in place. On genealogy: the founding problem — cross-generational capability against standing hazards — remains live, so this is not resolved mandatrophy; but the coordination channel's mandate has lapsed into compliance production, a partial mandatrophy recorded through the R5 fields (live status, contested method) rather than a resolved flag, because the physical channel still discharges its founding charge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_stratification_question,
    'This story instantiates the hybrid_reading of kernel preparedness_transmission: is capability transmission genuinely stratified (physical layer live, coordination layer decayed), or do the sibling readings hold — uniform competence (competence_reading) or uniform hollowness (husk_reading)?',
    'Layer-separated after-action audit across multiple stress events: score infrastructure performance against design basis separately from evacuation, shelter, and communication outcomes. A persistent split confirms stratification; uniform success favors competence_reading; uniform failure favors husk_reading.',
    'Under competence_reading, epsilon drops toward coordination-cost levels and the type moves toward rope; under husk_reading, theater_ratio rises toward the piton range and the physical-layer credit disappears. The hybrid classification and every downstream seat computation depend on where the D5 transmission break sits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_stratification_question, empirical, 'Which sibling reading of the preparedness-transmission kernel the evidence supports.').

omega_variable(
    mutual_aid_crowding_out,
    'Did the official drill-and-grant apparatus displace pre-existing mutual-aid coordination capacity, or did civilian coordination decay for reasons — urbanization, geographic mobility, media change — independent of the apparatus?',
    'Historical-comparative study of communities before and after program introduction, and comparison of regions with weak program penetration against regions with strong penetration.',
    'If displacement, part of the measured suppression is destroyed-alternative suppression and the apparatus bears causal responsibility for the coordination deficit; if independent, the apparatus is a failed response to exogenous decay rather than its cause, lowering attributed extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutual_aid_crowding_out, empirical, 'Whether the coordination-layer decay was caused or merely accompanied by the official apparatus.').

omega_variable(
    drill_skill_retention_fraction,
    'What fraction of current civilian drill activity produces durable, retrievable coordination skill versus momentary compliance performance?',
    'Unannounced exercise audits and longitudinal skill-retention testing of drill participants against control populations.',
    'Below roughly one-third skill-productive, the coordination layer is predominantly performance and piton drift becomes the live risk; above it, the hybrid reading overstates decay and epsilon should fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_skill_retention_fraction, empirical, 'Skill-production versus performance share of the civilian drill layer.').

omega_variable(
    hazard_intensity_confounding,
    'Are coordination failures under recent stress events attributable to decayed transmission, or to hazard intensity exceeding any design basis — climate amplification — such that a competent coordination layer would also have failed?',
    'Matched-event comparison: pair recent events with historical analogues of comparable magnitude and compare coordination outcomes at matched intensity.',
    'If intensity confounds, observed coordination failure overstates decay and the hybrid reading''s epsilon is inflated; if matched analogues show worse outcomes at equal intensity, the decay attribution stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hazard_intensity_confounding, empirical, 'Separating transmission decay from hazard-intensity escalation in outcome data.').

omega_variable(
    layer_boundary_conceptual,
    'Where is the line between the physical and coordination layers — does crew-based flood-fighting know-how count as infrastructure competence or as coordination knowledge?',
    'Conceptual: adopt the operational test used in after-action scoring — what fails when crews are present but command and communication fail, versus what fails when the assets themselves fail — and document the chosen boundary.',
    'Drawing the boundary to include crew-based flood-fighting in the physical layer strengthens the hybrid reading; drawing it into the coordination layer enlarges the decayed set and pushes toward husk_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(layer_boundary_conceptual, conceptual, 'Boundary-drawing choice that determines how much capability sits in each layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_transmission_hybrid_tr_t0, preparedness_transmission__hybrid_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(prep_transmission_hybrid_tr_t0, observed).
narrative_ontology:measurement(prep_transmission_hybrid_tr_t10, preparedness_transmission__hybrid_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(prep_transmission_hybrid_tr_t10, observed).
narrative_ontology:measurement(prep_transmission_hybrid_tr_t20, preparedness_transmission__hybrid_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement_basis(prep_transmission_hybrid_tr_t20, observed).
narrative_ontology:measurement(prep_transmission_hybrid_tr_t30, preparedness_transmission__hybrid_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement_basis(prep_transmission_hybrid_tr_t30, observed).
narrative_ontology:measurement(prep_transmission_hybrid_tr_t40, preparedness_transmission__hybrid_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(prep_transmission_hybrid_tr_t40, observed).
narrative_ontology:measurement(prep_transmission_hybrid_tr_t50, preparedness_transmission__hybrid_reading, theater_ratio, 50, 0.44).
narrative_ontology:measurement_basis(prep_transmission_hybrid_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(prep_transmission_hybrid_be_t0, preparedness_transmission__hybrid_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(prep_transmission_hybrid_be_t0, observed).
narrative_ontology:measurement(prep_transmission_hybrid_be_t10, preparedness_transmission__hybrid_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(prep_transmission_hybrid_be_t10, observed).
narrative_ontology:measurement(prep_transmission_hybrid_be_t20, preparedness_transmission__hybrid_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement_basis(prep_transmission_hybrid_be_t20, observed).
narrative_ontology:measurement(prep_transmission_hybrid_be_t30, preparedness_transmission__hybrid_reading, base_extractiveness, 30, 0.49).
narrative_ontology:measurement_basis(prep_transmission_hybrid_be_t30, observed).
narrative_ontology:measurement(prep_transmission_hybrid_be_t40, preparedness_transmission__hybrid_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement_basis(prep_transmission_hybrid_be_t40, observed).
narrative_ontology:measurement(prep_transmission_hybrid_be_t50, preparedness_transmission__hybrid_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(prep_transmission_hybrid_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_transmission_hybrid_su_t0, preparedness_transmission__hybrid_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(prep_transmission_hybrid_su_t0, observed).
narrative_ontology:measurement(prep_transmission_hybrid_su_t10, preparedness_transmission__hybrid_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(prep_transmission_hybrid_su_t10, observed).
narrative_ontology:measurement(prep_transmission_hybrid_su_t20, preparedness_transmission__hybrid_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement_basis(prep_transmission_hybrid_su_t20, observed).
narrative_ontology:measurement(prep_transmission_hybrid_su_t30, preparedness_transmission__hybrid_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement_basis(prep_transmission_hybrid_su_t30, observed).
narrative_ontology:measurement(prep_transmission_hybrid_su_t40, preparedness_transmission__hybrid_reading, suppression_requirement, 40, 0.47).
narrative_ontology:measurement_basis(prep_transmission_hybrid_su_t40, observed).
narrative_ontology:measurement(prep_transmission_hybrid_su_t50, preparedness_transmission__hybrid_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement_basis(prep_transmission_hybrid_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__competence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'is preparedness transmitted?' decomposes, per the epsilon-invariance principle, into three structurally distinct claims that differ in WHERE the transmission break sits. competence_reading (uniform live exercised knowledge) carries low epsilon and rope-leaning structure; husk_reading (uniform memorial ritual) carries high theater_ratio and piton-leaning structure; this hybrid_reading (stratified: physical live, coordination decayed) carries intermediate epsilon with a tangled_rope structure. Each story has its own epsilon, beneficiaries, and classification; measuring one observable across all three would conflate them. Upstream/downstream: competence_reading is the regime's self-description and is cited by agencies as evidence; husk_reading is the radical critique; the hybrid reading is the stratified synthesis both sides' evidence feeds into. This story links to both siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_transmission__hybrid_reading, powerless, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
