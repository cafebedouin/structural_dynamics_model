% ============================================================================
% CONSTRAINT STORY: preparedness_retention__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__hybrid_reading, []).

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
 *   constraint_id: preparedness_retention__hybrid_reading
 *   human_readable: Stratified Preparedness Retention: Live Core, Ceremonial Periphery (Hybrid Reading)
 *   domain: governance/disaster-preparedness
 *
 * SUMMARY:
 *   The Netherlands manages flood risk through a deliberately stratified
 *   preparedness system. On one track, Rijkswaterstaat and the water boards
 *   employ engineers, hydrologists, and dike wardens who exercise technical
 *   competence continuously on live infrastructure — storm surge barriers,
 *   dike and dune reinforcements, river discharge management. On the other
 *   track, everyone else: municipal crisis offices run prescribed evacuation
 *   exercises on national cycles, schools stage flood-awareness sessions, and
 *   residents receive information campaigns, but the working knowledge that
 *   once lived widely in a population that personally rebuilt its country
 *   after the 1953 North Sea flood has thinned into scheduled performance.
 *   Each generation remembers less directly; each exercise satisfies more
 *   documentation than capability. The arrangement under contest is this
 *   division of labor itself: competence is kept alive by concentrating it,
 *   and the price of that concentration is carried by the periphery, whose
 *   preparedness is now largely ceremonial while the concentrated core
 *   constitutes a single point of failure for the whole system. The claim and
 *   the metrics are authored independently: I claim tangled_rope from the
 *   structural reading below, and the metric values state what I believe
 *   descriptively true of the arrangement's operation.
 *
 * KEY AGENTS:
 *   - rijkswaterstaat: agenda-setting national agency — designs the water-safety regime, sets exercise standards, absorbs the largest share of preparedness resources
 *   - water_boards: constitutionally entrenched regional co-administrators — levy, maintain, and execute the prescribed cycles; gain standing and budget from the division of labor
 *   - core_technical_staff: specialist engineers and dike wardens — hold and exercise the live competence; careers and credentials bound to the institutions
 *   - municipal_crisis_offices: peripheral executors — run mandated drills and produce compliance records; depend on the center for technical content
 *   - flood_prone_residents: the protected public — carry residual risk with ceremonial preparation; cannot exit the protection system
 *   - community_resilience_organizations: excluded builders of distributed skill — crowded out of budgets, admitted only as scripted participants
 *   - dutch_safety_board: analytical observer — investigates gaps between exercised and actual capability after incidents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, 0.61).
domain_priors:suppression_score(preparedness_retention__hybrid_reading, 0.52).
domain_priors:theater_ratio(preparedness_retention__hybrid_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__hybrid_reading, "Stratified Preparedness Retention: Live Core, Ceremonial Periphery (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_retention__hybrid_reading, "governance/disaster-preparedness").

domain_priors:requires_active_enforcement(preparedness_retention__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, '88b98916-49f1-446f-96ed-c3c8b8e994c5').
narrative_ontology:cs_kernel_codification('88b98916-49f1-446f-96ed-c3c8b8e994c5', formalized).
narrative_ontology:cs_authority_grounding('88b98916-49f1-446f-96ed-c3c8b8e994c5', expertise).
narrative_ontology:cs_interpretation_layer_present('88b98916-49f1-446f-96ed-c3c8b8e994c5').
narrative_ontology:cs_reading_relation('88b98916-49f1-446f-96ed-c3c8b8e994c5', preparedness_retention__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('88b98916-49f1-446f-96ed-c3c8b8e994c5', preparedness_retention__competence_reading, forecloses).
narrative_ontology:cs_axiom('88b98916-49f1-446f-96ed-c3c8b8e994c5', foundational, competence_liveness_is_stratified).
narrative_ontology:cs_axiom_status(competence_liveness_is_stratified, holdable).
narrative_ontology:cs_axiom_grounding('88b98916-49f1-446f-96ed-c3c8b8e994c5', competence_liveness_is_stratified, empirically_contingent).
narrative_ontology:cs_axiom('88b98916-49f1-446f-96ed-c3c8b8e994c5', secondary, ceremonial_periphery_yields_single_point_of_failure).
narrative_ontology:cs_axiom_status(ceremonial_periphery_yields_single_point_of_failure, holdable).
narrative_ontology:cs_axiom_grounding('88b98916-49f1-446f-96ed-c3c8b8e994c5', ceremonial_periphery_yields_single_point_of_failure, empirically_contingent).
narrative_ontology:cs_reference_frame('88b98916-49f1-446f-96ed-c3c8b8e994c5', stratified_dual_track_competence).
narrative_ontology:cs_drift_state('88b98916-49f1-446f-96ed-c3c8b8e994c5', contemporary_post_limburg_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('88b98916-49f1-446f-96ed-c3c8b8e994c5', '2026-06-12T09:15:00Z').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, rijkswaterstaat).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, water_boards).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, core_technical_staff).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, flood_prone_residents).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, municipal_crisis_offices).
narrative_ontology:constraint_vindicates(preparedness_retention__hybrid_reading, centralized_expertise_doctrine).
narrative_ontology:constraint_vindicates(preparedness_retention__hybrid_reading, delegated_memory_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces the national water-safety regime: sets exercise standards, certifies regional plans, operates the major engineering works, and absorbs the largest share of preparedness funding. Its organizational identity is fused with being the keeper of flood expertise, so stepping back from that custodial role is not a live option for it.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, rijkswaterstaat, agenda_setter,
    institutional, generational, identity_locked, national).

% Constitutionally entrenched regional authorities that levy their own taxes, maintain regional dikes, and execute the nationally prescribed inspection and exercise cycles. They gain budget, staff, and political standing from the current division of labor, while carrying electoral accountability when regional flood defenses fail.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, water_boards, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, water_boards, agenda_setter).

% Engineers, hydrologists, and dike wardens employed inside the specialist institutions. They exercise real skills daily on live infrastructure, build careers and professional standing on the scarcity of that expertise, and their credentials are largely valid only within this institutional world.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, core_technical_staff, beneficiary,
    organized, biographical, identity_locked, national).

% Municipal and safety-region emergency planners required to run evacuation exercises and produce compliance documentation on nationally prescribed cycles. They spend real personnel hours on the drills, receive the technical content pre-packaged from the specialist institutions, and have no option to decline the exercise regime.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, municipal_crisis_offices, payer,
    moderate, biographical, trapped, regional).

% People living below sea level behind the dikes. They participate in periodic public drills and receive information campaigns, but hold little working knowledge of flood response themselves, and each generation inherits less direct memory than the last. Relocation away from the protection system their safety depends on is not a realistic choice.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, flood_prone_residents, payer,
    powerless, generational, trapped, national).

% Volunteer networks, trainers, and local initiatives that would build distributed response skills among residents. They compete unsuccessfully for preparedness budgets that flow to the specialist institutions, and are admitted to official exercises, if at all, as scripted participants rather than program builders.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, community_resilience_organizations, excluded,
    moderate, biographical, mobile, national).

% The independent investigative body that examines performance after incidents and near-misses. It reads drill records, interviews responders, and publishes findings on the gap between exercised and actual capability.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, dutch_safety_board, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__hybrid_reading, rijkswaterstaat).
narrative_ontology:fixing_cost_class(preparedness_retention__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates scarce hydraulic-engineering and crisis-management expertise in institutions that exercise it continuously against real infrastructure and real failure modes, solving a collective-action problem no household or municipality can solve alone: someone must hold, test, and transmit deep flood-defense competence.
% TRANSFER_FUNCTION: Moves preparedness resources — budget, training time, civic attention, and the burden of remembering — from broad social distribution toward the specialist institutions, and correspondingly moves residual risk exposure onto residents and execution burden onto municipal offices, compensated mainly by reassurance.
% ABSENT_VOICES: Community resilience organizations and independent trainers would argue for funded distributed-competence programs; flood-prone residents would ask what personal capability they were supposed to retain and why the drills feel like paperwork. Both sit outside the planning rooms — the former defunded, the latter consulted as scripted exercise participants rather than agenda participants.
% DISAPPEARANCE_RATIONALE: If the stratified arrangement vanished overnight, water-safety governance would reorganize around whatever replaced it: either an emergency redistribution of training and budget toward municipalities and communities, or a scramble to rebuild concentrated capacity — and the specialist institutions' funding, staffing rationale, and statutory standing would collapse with it. Visible arrangements depend on it.
% FOUNDING_PROBLEM: After the February 1953 North Sea flood killed more than 1,800 people in the Netherlands, the nation faced a double deficit: defenses inadequate to the sea, and too few people holding the knowledge to design, build, operate, and maintain what would be needed. The founding problem was how to create and keep such competence in a country that could never afford to lose it again.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the historical record of the 1953 disaster and the first Delta Commission's reports corroborate the founding problem itself; the Dutch Safety Board's post-incident investigations and national climate-adaptation assessments attest that the risk side remains live in altered form; academic critiques of exercise-based compliance attest that the competence-distribution question has changed character. No attestation rests solely on Rijkswaterstaat or the water boards.
narrative_ontology:disappearance_verdict(preparedness_retention__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_retention__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__hybrid_reading, 0.61, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Structurally I read a dual-track arrangement: a genuine coordination function (concentrating scarce hydraulic expertise where it is exercised daily against real failure modes — a problem individuals cannot solve alone) fused with an asymmetric cost structure (the periphery's preparedness atrophies into ceremony while the concentrated core becomes a single point of failure), held in place by active enforcement (statutory exercise cycles, budget concentration, professional gatekeeping). That combination is why I claim tangled_rope. The metrics describe operation as I observe it: extractiveness 0.61 — substantial but bounded by the real service the core provides; suppression 0.52 — structural (mandates, budget crowding-out, credential closure) rather than coercive, and authored as a raw structural property since suppression is not scaled by power or scope; theater_ratio 0.47 — nearly half of system-wide preparedness activity is peripheral performance measured in completions rather than capability; accessibility_collapse 0.42 — alternatives such as community resilience programs remain conceivable and locally demonstrable but are starved rather than impossible; resistance 0.35 — occasional municipal pushback on unfunded exercise mandates and press criticism after near-misses (Limburg 2021), blunted by the system's visible delivery of safety. The three temporal series share one grid (1980–2026, seven points) so no metric borrows another's endpoints; all rise together as generational memory faded and the compliance apparatus hardened through the EU Floods Directive transposition and the Safety Regions Act. Coalition note: individually powerless residents have repeatedly shown post-event mobilization capacity (resident inquiries and watersnood committees after Limburg), which is the main existing check on further periphery erosion. End-state values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the specialist institutions' seat the arrangement is what makes Dutch water safety possible: they see their own daily exercised competence and reasonably generalize it to the system. From the municipal crisis office's seat the same arrangement is a compliance routine: real personnel hours spent producing drill records whose technical content arrives pre-packaged from the center. From the resident's seat it is a promise of protection paired with no personal capability and no voice in the trade-off that spent their preparedness on institutional depth. The safety board's analytical seat sees both tracks and the gap between them. The engine computes these per-seat classifications from the structural data; the divergence between the beneficiary seats' coordination-flavored experience and the payer seats' cost-bearing experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: rijkswaterstaat, water_boards, and core_technical_staff sit near the beneficiary end (low d) — the arrangement subsidizes their budgets, standing, and careers. flood_prone_residents sit near the full-target end: they bear the lost distributed resilience and the concentrated-failure exposure, are trapped (relocation away from dike protection is not a real option), and their generational horizon means they inherit the atrophy. municipal_crisis_offices are targets with trapped exit — mandated participation they cannot decline. water_boards carry a mild counterweight (electoral accountability for regional flood failures) but remain net collectors. Community resilience organizations are excluded rather than targeted: the arrangement's effect on them is denial of the resource stream they would convert into distributed competence. No directionality overrides were needed: the beneficiary/victim declarations plus exit options already separate the seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconstructing the competence to protect a below-sea-level nation after 1953 — was solved so thoroughly that its solution became an institution with its own retention needs. Part of the mandate is now dead: the original distribution problem (who holds flood knowledge) was answered by concentration, and the machinery that answered it now defends the answer. But the mandatrophy is incomplete: climate-driven sea-level rise and compound events reopen the underlying risk in a form the founding settlement did not anticipate, which is why mandatrophy_resolved is left undeclared. The hybrid framing prevents misclassification in both directions: reading the whole system as pure coordination would erase the periphery's lost resilience and the single-point-of-failure cost; reading it as pure extraction would erase the live, exercised competence that genuinely protects millions. The dual-track structure carries both, which is the tangled-rope signature — and the mismatch consumer should watch founding_problem_status (contested) against disappearance_verdict (world_rearranges): the arrangement persists on a partially transformed problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the stratified structure best described by this hybrid reading (live core, ceremonial periphery), or by the sibling readings — uniform competence (competence_reading) or uniform hollowness (husk_reading)?',
    'Incident-level performance audits separating subsystems that retained live capability during real events (e.g., the July 2021 Limburg floods) from subsystems whose exercised routines failed on contact, cross-referenced with drill-design quality rather than drill-completion counts.',
    'If the husk_reading is right, classification shifts toward snare or piton and epsilon rises sharply; if the competence_reading is right, the arrangement approaches rope and epsilon falls. This story''s entire classification is conditional on the hybrid partition being the correct cut.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, empirical, 'Which reading of the preparedness_retention kernel the evidence supports.').

omega_variable(
    centralization_fragility,
    'Does concentrating operational competence in a few institutions create a genuine single point of failure (staff attrition, compound-event overload, disruption of headquarters functions), or do internal redundancy and succession planning neutralize the concentration risk?',
    'Workforce analytics on specialist-staff depth, stress tests simulating simultaneous multi-region events, and comparative study of institutional performance under compound conditions.',
    'If fragile, the cost borne by residents is higher than authored and effective extraction on the payer seats rises; if robust, part of the lost-distributed-resilience critique dissolves and epsilon falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(centralization_fragility, empirical, 'Whether the centralized core is a real single point of failure.').

omega_variable(
    counterfactual_atrophy_baseline,
    'Would broad societal flood-memory have persisted absent the centralized arrangement, or was its erosion driven by urbanization, generational turnover, and peacetime forgetting regardless?',
    'Comparison with regions retaining stronger local water-governance traditions, and historical case studies of decentralized preparedness under comparable modernization pressure.',
    'If erosion was inevitable, the arrangement''s extractiveness attributable to crowding-out falls (it failed to subsidize rather than actively displaced); if the arrangement displaced viable alternatives, epsilon stands as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_atrophy_baseline, conceptual, 'Counterfactual baseline for distributed-resilience loss.').

omega_variable(
    ceremonial_threshold,
    'How much of peripheral drill activity must be non-functional before ''ceremonial'' is the right description rather than ''low-intensity practice''?',
    'Outcome-linked evaluation distinguishing exercises that measurably change responder and resident behavior from exercises that satisfy documentation requirements.',
    'Below the threshold, theater_ratio falls and the periphery reads as weak-but-real practice; above it, the husk_reading gains ground for the periphery track specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremonial_threshold, conceptual, 'Definitional boundary of the ceremonial periphery.').

omega_variable(
    technocratic_deference_internalization,
    'Is resident passivity about flood preparedness structural (few alternatives offered, information asymmetry) or internalized (deference to expert institutions absorbed as identity)?',
    'Survey experiments measuring preparedness uptake when distributed-training offers are made available and salient to residents.',
    'If internalized, suppression persists even where structural barriers are removed, raising effective suppression on the resident seat beyond the structural measure; if structural, funded alternatives would quickly restore uptake.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technocratic_deference_internalization, empirical, 'Structural versus internalized component of resident passivity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 1980, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1980, preparedness_retention__hybrid_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(prep_tr_t1988, preparedness_retention__hybrid_reading, theater_ratio, 1988, 0.27).
narrative_ontology:measurement(prep_tr_t1996, preparedness_retention__hybrid_reading, theater_ratio, 1996, 0.33).
narrative_ontology:measurement(prep_tr_t2004, preparedness_retention__hybrid_reading, theater_ratio, 2004, 0.37).
narrative_ontology:measurement(prep_tr_t2012, preparedness_retention__hybrid_reading, theater_ratio, 2012, 0.41).
narrative_ontology:measurement(prep_tr_t2020, preparedness_retention__hybrid_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement(prep_tr_t2026, preparedness_retention__hybrid_reading, theater_ratio, 2026, 0.47).

% Extraction over time
narrative_ontology:measurement(prep_be_t1980, preparedness_retention__hybrid_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement(prep_be_t1988, preparedness_retention__hybrid_reading, base_extractiveness, 1988, 0.43).
narrative_ontology:measurement(prep_be_t1996, preparedness_retention__hybrid_reading, base_extractiveness, 1996, 0.49).
narrative_ontology:measurement(prep_be_t2004, preparedness_retention__hybrid_reading, base_extractiveness, 2004, 0.53).
narrative_ontology:measurement(prep_be_t2012, preparedness_retention__hybrid_reading, base_extractiveness, 2012, 0.56).
narrative_ontology:measurement(prep_be_t2020, preparedness_retention__hybrid_reading, base_extractiveness, 2020, 0.59).
narrative_ontology:measurement(prep_be_t2026, preparedness_retention__hybrid_reading, base_extractiveness, 2026, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1980, preparedness_retention__hybrid_reading, suppression_requirement, 1980, 0.28).
narrative_ontology:measurement(prep_su_t1988, preparedness_retention__hybrid_reading, suppression_requirement, 1988, 0.33).
narrative_ontology:measurement(prep_su_t1996, preparedness_retention__hybrid_reading, suppression_requirement, 1996, 0.39).
narrative_ontology:measurement(prep_su_t2004, preparedness_retention__hybrid_reading, suppression_requirement, 2004, 0.45).
narrative_ontology:measurement(prep_su_t2012, preparedness_retention__hybrid_reading, suppression_requirement, 2012, 0.5).
narrative_ontology:measurement(prep_su_t2020, preparedness_retention__hybrid_reading, suppression_requirement, 2020, 0.51).
narrative_ontology:measurement(prep_su_t2026, preparedness_retention__hybrid_reading, suppression_requirement, 2026, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__competence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'preparedness retention' covers three structurally distinct empirical claims about where live competence resides. Per the epsilon-invariance principle they are authored as three stories sharing one referent (the standing Dutch water-safety arrangement) with reading-indexed epsilon: competence_reading authors low extraction over the whole practice class; husk_reading authors near-total theatricality; hybrid_reading (this file) authors moderate-high extraction concentrated on the periphery. Upstream/downstream: competence_reading is the establishment claim the other two position against; husk_reading is the radical denial; this reading mediates between them. Family members link mutually via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
