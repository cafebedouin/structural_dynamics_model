% ============================================================================
% CONSTRAINT STORY: preparedness_retention__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_retention__hybrid_reading
 *   human_readable: Stratified Flood-Preparedness Retention (Technical Core / Ceremonial Periphery)
 *   domain: governance/disaster_preparedness
 *
 * SUMMARY:
 *   This story instantiates the hybrid reading of the preparedness_retention
 *   kernel: rather than treating Dutch flood preparedness as either fully
 *   live competence (the competence_reading) or fully hollow ceremony (the
 *   husk_reading), the hybrid reading asserts a genuine structural
 *   bifurcation — technical mastery is real and concentrated in
 *   Rijkswaterstaat and the regional water boards, while the broader societal
 *   layer of drills, sirens, and civil-defense participation has drifted into
 *   ceremonial performance. The two halves of the system are not equally
 *   real; the split itself, and the vulnerability it creates (a single point
 *   of institutional failure with no distributed fallback), is the constraint
 *   this story measures.
 *
 * KEY AGENTS:
 *   - rijkswaterstaat: technical core, agenda_setter/beneficiary
 *   - water_boards: regional technical core, agenda_setter/beneficiary
 *   - national_flood_insurers: downstream beneficiary of assumed readiness
 *   - coastal_municipal_residents: payer, ceremonial participants mistaking ritual for capacity
 *   - volunteer_civil_defense_networks: payer, atrophied distributed responders
 *   - future_disaster_responders: payer, inherit concentrated single-point-of-failure risk
 *   - national_disaster_historians: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, 0.52).
domain_priors:suppression_score(preparedness_retention__hybrid_reading, 0.38).
domain_priors:theater_ratio(preparedness_retention__hybrid_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__hybrid_reading, "Stratified Flood-Preparedness Retention (Technical Core / Ceremonial Periphery)").
narrative_ontology:topic_domain(preparedness_retention__hybrid_reading, "governance/disaster_preparedness").

domain_priors:requires_active_enforcement(preparedness_retention__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, '326c059e-4d18-4723-a261-20406b868303').
narrative_ontology:cs_kernel_codification('326c059e-4d18-4723-a261-20406b868303', distributed).
narrative_ontology:cs_authority_grounding('326c059e-4d18-4723-a261-20406b868303', expertise).
narrative_ontology:cs_interpretation_layer_present('326c059e-4d18-4723-a261-20406b868303').
narrative_ontology:cs_reading_relation('326c059e-4d18-4723-a261-20406b868303', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('326c059e-4d18-4723-a261-20406b868303', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_axiom('326c059e-4d18-4723-a261-20406b868303', foundational, competence_is_structurally_stratified_not_uniform).
narrative_ontology:cs_axiom_status(competence_is_structurally_stratified_not_uniform, holdable).
narrative_ontology:cs_axiom_grounding('326c059e-4d18-4723-a261-20406b868303', competence_is_structurally_stratified_not_uniform, empirically_contingent).
narrative_ontology:cs_axiom('326c059e-4d18-4723-a261-20406b868303', secondary, concentrated_expertise_creates_single_point_of_failure_risk).
narrative_ontology:cs_axiom_status(concentrated_expertise_creates_single_point_of_failure_risk, holdable).
narrative_ontology:cs_axiom_grounding('326c059e-4d18-4723-a261-20406b868303', concentrated_expertise_creates_single_point_of_failure_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('326c059e-4d18-4723-a261-20406b868303', post_1953_delta_works_distributed_readiness_model).
narrative_ontology:cs_drift_state('326c059e-4d18-4723-a261-20406b868303', contemporary_technocratic_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('326c059e-4d18-4723-a261-20406b868303', '').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, rijkswaterstaat).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, water_boards).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, national_flood_insurers).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, coastal_municipal_residents).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, volunteer_civil_defense_networks).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, future_disaster_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the actual hydraulic engineering competence, storm-surge modeling, and dike-maintenance expertise. Sets national flood-safety standards and administers the drill calendar. Its institutional continuity and budget depend on being seen as the sole indispensable repository of technical mastery; it benefits from a system where competence stays concentrated rather than diffused into municipal or civilian hands.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, rijkswaterstaat, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, rijkswaterstaat, beneficiary).

% Regional bodies with centuries of specific local hydraulic knowledge, funded by dedicated water taxes. Their authority and funding stream depend on remaining the recognized technical experts; they administer local drills and infrastructure checks that keep their own staff's competence live while the public-facing versions of these events are largely symbolic.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, water_boards, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, water_boards, beneficiary).

% Price flood risk on the assumption that Rijkswaterstaat and the water boards maintain adequate technical readiness. Benefit from public confidence in the preparedness system without bearing the cost of verifying whether that confidence is distributed or concentrated; a single-point failure would be catastrophic for them but the current arrangement lets them underwrite risk cheaply in the meantime.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, national_flood_insurers, beneficiary,
    organized, biographical, mobile, national).

% Attend or witness annual flood drills, sirens tests, and evacuation exercises that have become largely ceremonial — reassuring rituals rather than tests of real individual or community capacity. They believe they are prepared because the ceremony says so, but their actual operational knowledge (evacuation routes, emergency roles, independent response capacity) has atrophied because the real competence sits elsewhere, in institutions they cannot access or verify.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, coastal_municipal_residents, payer,
    powerless, biographical, trapped, local).

% Formerly held meaningful distributed operational roles in flood response; now largely relegated to ceremonial participation in drills designed and scripted by technical agencies. Their organizational capacity has withered because the system no longer needs or trains them as genuine responders — they are a symbolic layer, not a functional one, and would be unable to act independently if the technical core were disabled or overwhelmed.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, volunteer_civil_defense_networks, payer,
    powerless, biographical, constrained, regional).

% Inherit a system where resilience is concentrated in a small number of technical institutions rather than distributed across society. If those institutions are degraded, overwhelmed, or fail simultaneously (a compound national crisis, a generational leadership gap, a budget collapse), there is no broad societal reservoir of live competence to fall back on — this vulnerability is not yet realized but is structurally built in.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, future_disaster_responders, payer,
    powerless, generational, trapped, national).

% Study the drift from post-1953-flood distributed civil competence toward today's technocratic concentration. Document the gap between the felt sense of national preparedness and the actual distribution of operational capacity, largely from outside any institution with a stake in either narrative.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, national_disaster_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__hybrid_reading, rijkswaterstaat).
narrative_ontology:fixing_cost_class(preparedness_retention__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrating deep technical hydraulic competence in specialized, well-resourced institutions solves a genuine problem: flood engineering requires decades of accumulated expertise that cannot be maintained credibly by a diffuse, rotating civilian population. The technical core's competence is real and load-bearing.
% TRANSFER_FUNCTION: Public trust, funding, and legitimacy flow toward Rijkswaterstaat and the water boards as the recognized experts, while genuine operational capacity and independent response readiness flow away from municipalities, volunteer networks, and residents — they receive reassurance rituals in place of retained capability.
% ABSENT_VOICES: Volunteer civil defense veterans and municipal emergency planners who remember a more distributed preparedness model would object that the current arrangement has hollowed out local capacity in exchange for a narrower, more fragile form of national safety; they are rarely consulted in the design of national drill protocols, which are set top-down by the technical agencies themselves.
% DISAPPEARANCE_RATIONALE: If the ceremonial layer (public drills, sirens, evacuation exercises) vanished overnight, the technical core's actual flood-management capacity would be unaffected — Rijkswaterstaat and the water boards would keep functioning. But public trust, insurance pricing assumptions, and the sense of shared societal readiness would visibly erode, which is contested: some argue the ceremony is inert theater whose loss changes nothing real, others argue the ceremony is doing genuine (if thin) coordination work in sustaining political willingness to fund the technical core.
% FOUNDING_PROBLEM: After the 1953 North Sea flood disaster, the Netherlands needed both world-class engineering capacity (the Delta Works) and a broadly prepared, vigilant citizenry that would never again be caught unaware — the original model combined institutional expertise with genuinely distributed civil defense competence.
% FOUNDING_PROBLEM_CORROBORATION: Rijkswaterstaat and the water boards attest the founding problem remains fully addressed, citing continuously updated engineering standards and disaster response plans. Independent disaster historians and several municipal safety officers, outside the benefiting technical institutions, attest that the distributed-competence half of the original founding problem has quietly lapsed into performance even as the engineering half remains genuinely solved — a bifurcated verdict rather than a clean resolution.
narrative_ontology:disappearance_verdict(preparedness_retention__hybrid_reading, contested).
narrative_ontology:founding_problem_status(preparedness_retention__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__hybrid_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.52) and theater_ratio (0.55) are both moderate-to-high and rising together, reflecting the hybrid reading's core claim: as the technical core has professionalized and consolidated over seven decades, the public-facing preparedness layer has increasingly substituted ritual reassurance for genuine distributed capability. Suppression (0.38) is moderate rather than severe — no one is actively coerced into the ceremonial layer, but accessibility_collapse (0.62) is substantial because most residents and volunteer networks no longer have any real avenue to acquire or exercise the operational competence that has migrated into specialized institutions. Resistance (0.4) is present but muted because the technical core's genuine expertise makes its authority hard to contest even by those who sense the periphery has hollowed out.
 *
 * DIRECTIONALITY LOGIC:
 *   Rijkswaterstaat and the water boards sit near the beneficiary end: they hold real competence, institutional funding, and unquestioned authority, and the stratified system protects their monopoly on legitimate expertise. National flood insurers benefit indirectly by underwriting risk cheaply on the assumption of adequate national readiness. Coastal residents, volunteer networks, and future responders sit near the target end: they bear the cost of a system that gives them the feeling of preparedness without the substance, and they would bear the full cost if the concentrated technical core were ever compromised, overwhelmed, or defunded.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resists mandatrophy-mislabeling in both directions: it does not let the technical core's genuine, still-functioning competence excuse the diagnosis of the periphery's hollowing (as a pure competence_reading would), and it does not let the periphery's theater discredit the technical core's real ongoing function (as a pure husk_reading would). The tangled_rope classification captures exactly this: real coordination (concentrated hydraulic engineering expertise genuinely protects the country) coexists with real extraction (public trust and funding sustain an image of shared societal readiness that no longer exists), both riding the same institutional structure and requiring active enforcement (drill calendars, licensing, funding gates) to hold in place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stratification_boundary_location,
    'Where exactly does the boundary between genuine competence and ceremonial performance fall — is it strictly institutional (inside vs. outside Rijkswaterstaat/water boards), or does ceremonial drift also affect junior or peripheral staff within the technical agencies themselves?',
    'Internal competence audits within Rijkswaterstaat and water boards comparing veteran engineering staff performance against newer hires and administrative staff on live simulation exercises, cross-checked against independent disaster-response drills with no institutional stake in the outcome.',
    'If ceremonial drift has penetrated the technical core itself, the hybrid reading collapses toward the husk_reading and the classification should move toward snare (concentrated institutions extracting trust while the core competence itself erodes). If the boundary holds cleanly at the institutional edge, the hybrid/tangled_rope reading is confirmed as the accurate structural account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_boundary_location, empirical, 'Whether the competence/ceremony boundary is a clean institutional line or itself eroding.').

omega_variable(
    single_point_of_failure_realism,
    'Is the single-point-of-failure vulnerability created by concentrating competence in a small technical core a realistic structural risk, or is it offset by redundancy the hybrid reading has not accounted for (e.g., international mutual-aid agreements, EU civil protection mechanisms)?',
    'Stress-test scenario analysis examining whether a simultaneous failure of Rijkswaterstaat and major water boards (budget collapse, leadership crisis, coincident crises) could be absorbed by external redundancy within an operationally relevant timeframe.',
    'If real redundancy exists, the victim designation for future_disaster_responders overstates the risk and the constraint may be closer to a benign division of labor (rope) than tangled_rope. If no meaningful redundancy exists, the tangled_rope classification is conservative and the true risk may be understated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(single_point_of_failure_realism, empirical, 'Whether external redundancy mitigates the concentrated single-point-of-failure risk.').

omega_variable(
    ceremonial_function_omega,
    'Is the ceremonial periphery purely extractive (reassurance theater that displaces real distributed capacity), or does it retain some residual coordination value (sustaining political will and budget support for the technical core, or providing minimal baseline evacuation awareness)?',
    'Compare regions/periods with high versus low ceremonial drill participation against actual evacuation compliance and outcome data during real flood events or near-miss incidents.',
    'If ceremony retains meaningful coordination value, the extraction attributed to the periphery should be revised downward and the constraint sits closer to a genuine rope with an inert-but-harmless ceremonial layer. If ceremony has zero measurable effect on real outcomes, the extraction assessment (0.52) may understate the true cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremonial_function_omega, conceptual, 'Whether the ceremonial layer performs any residual real function beyond theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 1953, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1953, preparedness_retention__hybrid_reading, theater_ratio, 1953, 0.1).
narrative_ontology:measurement(prep_tr_t1970, preparedness_retention__hybrid_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(prep_tr_t1990, preparedness_retention__hybrid_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(prep_tr_t2005, preparedness_retention__hybrid_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(prep_tr_t2015, preparedness_retention__hybrid_reading, theater_ratio, 2015, 0.48).
narrative_ontology:measurement(prep_tr_t2025, preparedness_retention__hybrid_reading, theater_ratio, 2025, 0.55).

% Extraction over time
narrative_ontology:measurement(prep_be_t1953, preparedness_retention__hybrid_reading, base_extractiveness, 1953, 0.18).
narrative_ontology:measurement(prep_be_t1970, preparedness_retention__hybrid_reading, base_extractiveness, 1970, 0.24).
narrative_ontology:measurement(prep_be_t1990, preparedness_retention__hybrid_reading, base_extractiveness, 1990, 0.33).
narrative_ontology:measurement(prep_be_t2005, preparedness_retention__hybrid_reading, base_extractiveness, 2005, 0.41).
narrative_ontology:measurement(prep_be_t2015, preparedness_retention__hybrid_reading, base_extractiveness, 2015, 0.47).
narrative_ontology:measurement(prep_be_t2025, preparedness_retention__hybrid_reading, base_extractiveness, 2025, 0.52).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_retention__hybrid_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the preparedness_retention kernel. competence_reading and husk_reading occupy the totalizing endpoints (all-live vs. all-ceremonial); this hybrid_reading occupies the structurally distinct middle position asserting a real stratification boundary with its own distinct victim (distributed resilience) and beneficiary (institutional continuity) structure. All three share the same underlying narrative material (Dutch flood preparedness institutions) but instantiate different ε values and different classifications because they make different structural claims about where competence actually resides.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
