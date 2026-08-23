% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__husk_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: preparedness_persistence__husk_reading
 *   human_readable: Memorial Drill-and-Inspection Regime (Husk Reading)
 *   domain: institutional/disaster-preparedness
 *
 * SUMMARY:
 *   A regional flood-response preparedness regime requires every response
 *   agency to complete a published annual cycle of scripted exercises and
 *   facility inspections, documented through compliance attestations that
 *   feed grant eligibility, insurance community ratings, and public
 *   preparedness messaging. Three decades into the regime the calendar runs
 *   flawlessly: every scheduled drill occurs, every checklist closes, every
 *   attestation files on time. The regime's operative premise - that
 *   rehearsing together preserves the ability to respond together - has
 *   quietly detached from its output: veteran operators have retired,
 *   equipment ages between photographed inspections, scenarios follow printed
 *   sequences with pre-staged casualties, and after-action records route into
 *   compliance summaries rather than redesign. What remains is a fully
 *   functioning form whose function has largely departed: the drills are
 *   performed faithfully by people who can no longer be said to become ready
 *   by performing them. Claimed type and metrics are authored independently:
 *   the structural reading is piton (an atrophied former coordination
 *   mechanism held up by inertia and theatrical maintenance), and the metrics
 *   describe that operation descriptively; the engine computes per-seat
 *   classifications from the structural data.
 *
 * KEY AGENTS:
 *   - emergency_management_administrators: Agenda-setter (organized/identity_locked) - administers the calendar, could redesign it, collects thin legitimacy benefit
 *   - elected_municipal_leadership: Beneficiary (powerful/mobile) - consumes the assurance, funds the form
 *   - flood_exposed_residents: Primary target (powerless/trapped) - bears false assurance and unprotected exposure
 *   - frontline_responders: Secondary target (organized/constrained) - executes the scripts, absorbs the consequences
 *   - flood_insurance_underwriters: Excluded voice (institutional/arbitrage) - prices around the certification without engaging it
 *   - post_flood_inquiry_commissions: Analytical observer (institutional/analytical) - sees the gap after each event, then dissolves
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__husk_reading, 0.62).
domain_priors:suppression_score(preparedness_persistence__husk_reading, 0.28).
domain_priors:theater_ratio(preparedness_persistence__husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__husk_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__husk_reading, "Memorial Drill-and-Inspection Regime (Husk Reading)").
narrative_ontology:topic_domain(preparedness_persistence__husk_reading, "institutional/disaster-preparedness").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__husk_reading, 'd9c27476-b4e9-4366-b149-a0472bee9084').
narrative_ontology:cs_kernel_codification('d9c27476-b4e9-4366-b149-a0472bee9084', formalized).
narrative_ontology:cs_authority_grounding('d9c27476-b4e9-4366-b149-a0472bee9084', lineage).
narrative_ontology:cs_interpretation_layer_present('d9c27476-b4e9-4366-b149-a0472bee9084').
narrative_ontology:cs_reading_relation('d9c27476-b4e9-4366-b149-a0472bee9084', preparedness_persistence__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('d9c27476-b4e9-4366-b149-a0472bee9084', preparedness_persistence__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('d9c27476-b4e9-4366-b149-a0472bee9084', foundational, memorial_form_outlives_operative_function).
narrative_ontology:cs_axiom_status(memorial_form_outlives_operative_function, holdable).
narrative_ontology:cs_axiom_grounding('d9c27476-b4e9-4366-b149-a0472bee9084', memorial_form_outlives_operative_function, empirically_contingent).
narrative_ontology:cs_axiom('d9c27476-b4e9-4366-b149-a0472bee9084', secondary, certified_compliance_is_not_capability_evidence).
narrative_ontology:cs_axiom_status(certified_compliance_is_not_capability_evidence, holdable).
narrative_ontology:cs_axiom_grounding('d9c27476-b4e9-4366-b149-a0472bee9084', certified_compliance_is_not_capability_evidence, empirically_contingent).
narrative_ontology:cs_reference_frame('d9c27476-b4e9-4366-b149-a0472bee9084', rehearsal_preserves_capability).
narrative_ontology:cs_drift_state('d9c27476-b4e9-4366-b149-a0472bee9084', contemporary_post_inquiry_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('d9c27476-b4e9-4366-b149-a0472bee9084', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__husk_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, emergency_management_administrators).
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, elected_municipal_leadership).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, flood_exposed_residents).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, frontline_responders).
narrative_ontology:constraint_vindicates(preparedness_persistence__husk_reading, documented_compliance_equals_readiness).
narrative_ontology:constraint_vindicates(preparedness_persistence__husk_reading, routine_drilling_preserves_capability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the emergency-management office: publish the annual drill calendar, schedule facility and equipment inspections, host the exercises, and file the compliance attestations that unlock state and federal grant money. Staff careers advance on flawless execution of the calendar. There is no procedural path to stopping or shrinking the program - the calendar is the office's visible product - and proposing outcome-based redesign would mean declaring past attestations misleading. Participation consumes a large share of staff hours each quarter.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, emergency_management_administrators, agenda_setter,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__husk_reading, emergency_management_administrators, beneficiary).

% Fund the preparedness program and appear at headline exercises; cite drill counts and inspection pass-rates in budget hearings and campaigns as evidence the city is protected. Electoral cycles reward the visible certificate more than unglamorous equipment replacement, so requests for realistic training compete poorly against visible deliverables. Leaving office ends any relationship to the program.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, elected_municipal_leadership, beneficiary,
    powerful, immediate, mobile, regional).

% Live behind aging levees and in mapped floodplains; receive annual notices and preparedness pamphlets citing completed drills. Many hold flood insurance priced partly on municipal preparedness ratings. When water rises they rely on evacuation routes and shelter plans they have never seen tested under real conditions; moving away means selling property whose value already discounts the risk.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, flood_exposed_residents, payer,
    powerless, generational, trapped, regional).

% Fire, EMS, and public-works crews execute the scripted scenarios: staged casualties, pre-positioned props, radio traffic following a printed sequence. They know which equipment works and which is for display, and their after-action comments note the gaps, but the written record feeds a compliance summary rather than a redesign process. Overtime spent on rehearsals competes with apparatus maintenance and actual call volume.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, frontline_responders, payer,
    organized, biographical, constrained, regional).

% Price flood policies using hydrological models, loss history, and municipal rating classifications that credit documented preparedness activity. They see claims data after events but never observe the drills behind the ratings; where loss experience diverges from rated preparedness they reprice or withdraw from the market rather than engage the drill system.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, flood_insurance_underwriters, excluded,
    institutional, biographical, arbitrage, national).

% Convene after major flood events with a mandate to establish why response fell short; take testimony, compare drill records against operational timelines, and issue recommendations. Findings enter the record and then compete for attention with the next budget cycle; the commission dissolves on delivery.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, post_flood_inquiry_commissions, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_persistence__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Publishes a shared annual calendar of exercises and inspections across fire, EMS, public works, and neighboring jurisdictions; maintains common terminology, radio interoperability habits, and documented contact rosters; inspection checklists surface physical defects in facilities and equipment on a fixed cadence; produces the attestations that satisfy grant and rating requirements.
% TRANSFER_FUNCTION: Moves staff hours and operating funds from response-ready capacity into scheduled rehearsal and documentation; moves assurance upward (drill counts and pass rates to elected leadership, grantors, and rating bureaus) and outward to residents through preparedness messaging; leaves the difference between certified and actual capability, when the flood arrives, on the households in the inundation zone.
% ABSENT_VOICES: Responder after-action critiques exist in writing but have no standing seat in exercise redesign; residents have no seat at all; independent engineers who would fail equipment under load are not contracted; underwriters observe only claims, never drills. Objection surfaces episodically through post-flood inquiries and then disperses with the commission.
% DISAPPEARANCE_RATIONALE: Substantively, overnight removal would change little about flood-day capability, which no longer depends on the drills. Institutionally, much depends on the form: grant eligibility cites compliance attestations, community ratings embed drill activity, mutual-aid agreements reference exercised contacts, and budget justifications quote exercise hours. Removal would force simultaneous re-justification of the preparedness budget line and public admission that certified readiness overstated capability - a rearrangement no seated actor volunteers for, which is why the form persists.
% FOUNDING_PROBLEM: Mid-century flood events found agencies without shared procedures, radios that did not interoperate, unmaintained equipment, and personnel who had never rehearsed together; drills and inspections were instituted to rehearse coordination and catch physical decay before the next flood.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: post-flood inquiry commissions, reinsurer loss records, federal flood-risk assessments, and updated hydrological studies all confirm that flood exposure persists and is intensifying along developed floodplains.
narrative_ontology:disappearance_verdict(preparedness_persistence__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__husk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_persistence__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__husk_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_persistence__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_persistence__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the regime draws staff hours and operating funds and returns assurance substantially decoupled from delivered capability; it is not higher because inspection checklists retain real defect-detection value and the shared calendar preserves residual interoperability. Suppression 0.28 and static: persistence requires no enforcement machinery - attendance is professionally normative, deviation carries only mild career friction, and no participant seeks exit - so per the static-enforcement rule no suppression_requirement series is authored and the scalar carries the whole picture. Theater_ratio 0.78: the majority of activity is scripted, scored, and filed; unscripted, failure-permitting exercise components are rare and shrinking. Accessibility_collapse 0.35: the alternatives (no-notice exercises, instrumented equipment trials, outcome-linked evaluation) are comprehensively documented in inquiry recommendations and procedurally available; they collapse only against incentive gravity, not comprehension. Resistance 0.40: episodic post-flood surges (inquiry testimony, editorial pressure) that decay between events. The measurement grid is shared across both tracked metrics at eleven points spanning the interval (units are years, t0 approximately 1995, t30 approximately 2025); the dip at t=12-15 marks a major-flood inquiry pulse after which scripting and staffing drift resumed. The oscillation is exogenous shock followed by relapse, not an intermittent-reinforcement mechanism, and the brevity of each reform pulse is itself evidence of the husk's resilience. End-state values equal the base_properties scalars; the series represents the regime's observed enforcement-free drift.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the administering seat should compute differently. From the emergency-management office's position the regime is its professional product executed with pride - the calendar is the office's identity, and identity lock binds critique to self-negation. From the flood-exposed household the same regime arrives as assurance documents describing protections that were never stress-tested, an experience visible only when water rises. Frontline responders straddle: they execute the scripts and inherit the consequences, so their seat should compute as target despite organizational membership. Same-level differentiation: administrators and responders hold comparable organizational power, yet exit differs - the office cannot stop being the office (identity_locked) while crews experience the drills as a schedulable burden (constrained) - so the same activity computes differently across seats. Elected leadership, mobile and electorally episodic, sits nearest the beneficiary pole. The engine computes this divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: emergency_management_administrators (legitimacy, budget continuity, career progression) and elected_municipal_leadership (visible assurance for campaigns and hearings) derive low d - the regime subsidizes them. Victim declarations: flood_exposed_residents and frontline_responders derive high d - residents are trapped (property, insurance, nowhere equivalent to go) and receive the assurance-extraction directly; responders are constrained (employment, vocation) and pay in hours and consequence exposure. Identity-lock modulation: the administrators' exit is identity_locked - the office has become its calendar - which holds them at the arrangement's center regardless of private doubt; the fusion here is institutional (the organization has become its function's performance), and if the identity frame broke, deliberate wind-down toward a sunset transition would become computable. Suppression is low and unscaled: nothing must be coerced because no one wants to leave; the husk is self-sustaining. Excluded and observer seats (underwriters, inquiry commissions) carry no beneficiary/victim declaration and fall to canonical fallback; underwriters' arbitrage-grade repricing keeps them structurally peripheral.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - interagency flood-response fragmentation - is live and independently corroborated; what has died is the arrangement's operative function, competence preservation, which the measurement series shows decaying beneath a stable form. The R5 genealogy therefore records founding_problem_status live alongside a regime whose working life is over: the hazard outlived the remedy's efficacy, and the remedy's form outlived its function. This prevents two mislabelings. Reading the regime as rope (because drills once coordinated) ignores three decades of theater_ratio growth; reading it as snare (because someone must profit) finds no capturer - gain_flow is affirmatively diffuse, no seat defends the form against reform, and fixing_cost is prohibitive relative to any seat's stake, which is the piton cost-asymmetry: the administrator could change the calendar but bears almost none of the atrophy's cost, while those who bear the cost have no lever. The R5 mismatch path (status=dead x verdict=world_rearranges) does not fire because the founding problem is live; the piton signal instead rides the theater trajectory and the receipt surface. Residents' coalition potential is structurally diffused - losses arrive stochastically and individually - which is why no payer seat accumulates the leverage to force repair.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the preparedness_persistence kernel; would instantiating the competence_reading or hybrid_reading instead yield a different epsilon and type for the same observable regime?',
    'Author the sibling stories and compare computed classifications against shared observational data correlating drill dosage with response performance; the reading whose epsilon survives outcome-linked measurement is the one the regime instantiates.',
    'If the competence_reading is correct, effective extraction collapses toward the coordination floor and the type moves toward rope; if the hybrid_reading is correct, the regime decomposes into competent and ritualized sub-constraints rather than one husk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which reading of the preparedness kernel the observable regime instantiates.').

omega_variable(
    atrophy_extent_ambiguity,
    'How much operational competence has genuinely atrophied versus migrated into equipment, software, and turnover-resistant written procedure?',
    'Instrumented no-notice exercises and equipment-under-load trials compared against archived drill scores and inspection records.',
    'Bounds the honest ceiling on theater_ratio; if capability migrated rather than died, the husk reading overstates atrophy and the regime sits nearer stratified decomposition than total husk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_extent_ambiguity, empirical, 'Extent of genuine competence loss beneath the persistent form.').

omega_variable(
    inertia_vs_capture_boundary,
    'Does any seat actively defend the memorial form against outcome-based reform, or does the husk persist purely through inertia and diffusion of stakes?',
    'Trace budget-defense and legislative behavior when inquiry recommendations propose no-notice testing: active lobbying against reform indicates a defender seat; consistent absence indicates pure inertia.',
    'A confirmed defender converts the classification toward tangled_rope or snare (coordinated extraction with a maintainer); confirmed absence secures the piton reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inertia_vs_capture_boundary, empirical, 'Whether the husk is defended by a capturer or sustained by inertia alone.').

omega_variable(
    false_assurance_harm_attribution,
    'How much of the flood-exposed population''s uncompensated loss is attributable to assurance effects of certification (deferred mitigation investment, delayed evacuation decisions) rather than baseline hazard?',
    'Difference-in-differences on mitigation spending and evacuation latency across jurisdictions with high versus low certification intensity, controlling for hydrological exposure.',
    'Sets the resident-side weight in epsilon; a large assurance effect raises effective extraction on the trapped seat and hardens the target classification of residents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_assurance_harm_attribution, empirical, 'Attribution of resident harm between certified-assurance effects and baseline flood hazard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__husk_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__husk_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t3, preparedness_persistence__husk_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement_basis(prep_tr_t3, observed).
narrative_ontology:measurement(prep_tr_t6, preparedness_persistence__husk_reading, theater_ratio, 6, 0.52).
narrative_ontology:measurement_basis(prep_tr_t6, observed).
narrative_ontology:measurement(prep_tr_t9, preparedness_persistence__husk_reading, theater_ratio, 9, 0.56).
narrative_ontology:measurement_basis(prep_tr_t9, observed).
narrative_ontology:measurement(prep_tr_t12, preparedness_persistence__husk_reading, theater_ratio, 12, 0.58).
narrative_ontology:measurement_basis(prep_tr_t12, observed).
narrative_ontology:measurement(prep_tr_t15, preparedness_persistence__husk_reading, theater_ratio, 15, 0.53).
narrative_ontology:measurement_basis(prep_tr_t15, observed).
narrative_ontology:measurement(prep_tr_t18, preparedness_persistence__husk_reading, theater_ratio, 18, 0.6).
narrative_ontology:measurement_basis(prep_tr_t18, observed).
narrative_ontology:measurement(prep_tr_t21, preparedness_persistence__husk_reading, theater_ratio, 21, 0.66).
narrative_ontology:measurement_basis(prep_tr_t21, observed).
narrative_ontology:measurement(prep_tr_t24, preparedness_persistence__husk_reading, theater_ratio, 24, 0.71).
narrative_ontology:measurement_basis(prep_tr_t24, observed).
narrative_ontology:measurement(prep_tr_t27, preparedness_persistence__husk_reading, theater_ratio, 27, 0.75).
narrative_ontology:measurement_basis(prep_tr_t27, observed).
narrative_ontology:measurement(prep_tr_t30, preparedness_persistence__husk_reading, theater_ratio, 30, 0.78).
narrative_ontology:measurement_basis(prep_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__husk_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t3, preparedness_persistence__husk_reading, base_extractiveness, 3, 0.43).
narrative_ontology:measurement_basis(prep_be_t3, observed).
narrative_ontology:measurement(prep_be_t6, preparedness_persistence__husk_reading, base_extractiveness, 6, 0.46).
narrative_ontology:measurement_basis(prep_be_t6, observed).
narrative_ontology:measurement(prep_be_t9, preparedness_persistence__husk_reading, base_extractiveness, 9, 0.5).
narrative_ontology:measurement_basis(prep_be_t9, observed).
narrative_ontology:measurement(prep_be_t12, preparedness_persistence__husk_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement_basis(prep_be_t12, observed).
narrative_ontology:measurement(prep_be_t15, preparedness_persistence__husk_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement_basis(prep_be_t15, observed).
narrative_ontology:measurement(prep_be_t18, preparedness_persistence__husk_reading, base_extractiveness, 18, 0.54).
narrative_ontology:measurement_basis(prep_be_t18, observed).
narrative_ontology:measurement(prep_be_t21, preparedness_persistence__husk_reading, base_extractiveness, 21, 0.58).
narrative_ontology:measurement_basis(prep_be_t21, observed).
narrative_ontology:measurement(prep_be_t24, preparedness_persistence__husk_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement_basis(prep_be_t24, observed).
narrative_ontology:measurement(prep_be_t27, preparedness_persistence__husk_reading, base_extractiveness, 27, 0.61).
narrative_ontology:measurement_basis(prep_be_t27, observed).
narrative_ontology:measurement(prep_be_t30, preparedness_persistence__husk_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(prep_be_t30, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_persistence__husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% Kernel decomposition note: the colloquial label 'our drills and inspections keep us ready' conflates three structurally distinct claims about the same observable regime. This file instantiates the husk_reading alone, with epsilon authored for the standing arrangement as that reading assesses it (high theater, atrophied function). The competence_reading carries the upstream general claim (spaced rehearsal preserves skill - higher empirical confidence in the abstract, routinely cited by the regime's defenders as cover), and the hybrid_reading decomposes the regime into competent and ritualized components; both are separate stories linked here as family members. The upstream competence claim is what the husk reading contests in application, which is why the edges run from this reading to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
