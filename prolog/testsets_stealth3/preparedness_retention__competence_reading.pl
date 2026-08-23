% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__competence_reading, []).

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
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: National Flood-Preparedness Exercise and Inspection Regime
 *   domain: governance/institutional_memory/disaster_preparedness
 *
 * SUMMARY:
 *   A national flood-safety governance arrangement mandates a recurring cycle
 *   of response exercises and flood-defense inspections: a multi-year
 *   exercise calendar with regional and national flood scenarios, statutory
 *   dike-inspection intervals, evacuation rehearsals, and mandatory
 *   after-action evaluation feeding program redesign. This story instantiates
 *   the constraint as its competence-preserving operation: scheduled exercise
 *   hours and inspection cycles convert budget and personnel time into
 *   retained operational skill — scenario-current flood response, dike
 *   assessment, barrier operation, mass-evacuation coordination — that would
 *   otherwise decay between rare events. The validation record is
 *   event-based: large-scale exercised response preceding and during the 1995
 *   near-flood evacuation, and the 2021 Limburg floods, where exercised
 *   response structures held under live conditions. Beneficiary structure:
 *   the protected population, the response personnel whose competence is
 *   maintained, the water-authority inspection staff, and municipal planners;
 *   no seat is declared a victim — the costs (hours, budget, exercise-period
 *   disruption) are the coordination cost of rare-event capacity. The 1953
 *   founding record, independent post-incident investigations, and the 2021
 *   after-action literature corroborate the founding problem from outside the
 *   arrangement's benefiting parties.
 *
 * KEY AGENTS:
 *   - - national_flood_safety_directorate: Agenda-setter (institutional/arbitrage) — sets the exercise calendar, mandates inspection cycles, converts budget into scheduled exercises
 *   - - flood_plain_residents: Primary beneficiary (powerless/constrained) — protected population funding the cycle through taxes
 *   - - emergency_response_personnel: Net beneficiary bearing time costs (organized/constrained) — mandated hours maintain their live response skill
 *   - - water_board_dike_staff: Beneficiary (organized/constrained) — the inspection cycle keeps surveying and barrier-operation skill live
 *   - - municipal_emergency_planners: Beneficiary (moderate/constrained) — build and rehearse local evacuation capacity
 *   - - drill_zone_businesses: Excluded cost-bearer (moderate/constrained) — bear exercise-period disruption without a design seat
 *   - - fiscal_oversight_court: Excluded (institutional/analytical) — would press proportionality limits from outside the design conversation
 *   - - dutch_safety_board: Analytical observer (institutional/analytical) — post-incident investigations externally test whether exercised competence held
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.17).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.15).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.17).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "National Flood-Preparedness Exercise and Inspection Regime").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "governance/institutional_memory/disaster_preparedness").

domain_priors:requires_active_enforcement(preparedness_retention__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, '1fbf3c1f-6845-4910-b149-6460a89c8d44').
narrative_ontology:cs_kernel_codification('1fbf3c1f-6845-4910-b149-6460a89c8d44', distributed).
narrative_ontology:cs_authority_grounding('1fbf3c1f-6845-4910-b149-6460a89c8d44', practice).
narrative_ontology:cs_interpretation_layer_present('1fbf3c1f-6845-4910-b149-6460a89c8d44').
narrative_ontology:cs_reading_relation('1fbf3c1f-6845-4910-b149-6460a89c8d44', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('1fbf3c1f-6845-4910-b149-6460a89c8d44', preparedness_retention__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('1fbf3c1f-6845-4910-b149-6460a89c8d44', foundational, drills_preserve_operational_competence).
narrative_ontology:cs_axiom_status(drills_preserve_operational_competence, holdable).
narrative_ontology:cs_axiom_grounding('1fbf3c1f-6845-4910-b149-6460a89c8d44', drills_preserve_operational_competence, empirically_contingent).
narrative_ontology:cs_axiom('1fbf3c1f-6845-4910-b149-6460a89c8d44', secondary, exercise_investment_optimizes_adaptive_capacity).
narrative_ontology:cs_axiom_status(exercise_investment_optimizes_adaptive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('1fbf3c1f-6845-4910-b149-6460a89c8d44', exercise_investment_optimizes_adaptive_capacity, instrumental).
narrative_ontology:cs_reference_frame('1fbf3c1f-6845-4910-b149-6460a89c8d44', exercised_competence_baseline).
narrative_ontology:cs_drift_state('1fbf3c1f-6845-4910-b149-6460a89c8d44', contemporary_post_2021_validation, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('1fbf3c1f-6845-4910-b149-6460a89c8d44', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, flood_plain_residents).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, emergency_response_personnel).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, water_board_dike_staff).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, municipal_emergency_planners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, emergency_response_personnel).
narrative_ontology:constraint_vindicates(preparedness_retention__competence_reading, deliberate_practice_skill_retention).
narrative_ontology:constraint_vindicates(preparedness_retention__competence_reading, exercise_validity_transfer_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the multi-year exercise calendar, mandates the statutory dike-inspection intervals, and funds the exercise programs from the national safety budget. Reviews after-action reports from every exercise and adjusts scenario design in response. Its budget passes through it into scheduled exercises and inspection programs rather than terminating in its own operations, and it could restructure the calendar or the mandate if it chose.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, national_flood_safety_directorate, agenda_setter,
    institutional, generational, arbitrage, national).

% Live and work behind the dike system the exercise and inspection cycle protects. They fund the cycle through taxes and are the population evacuation drills rehearse for, but they operate no part of the response and hold no seat in calendar-setting. Relocating out of the flood plain is possible but carries high housing, family, and employment costs, so their safety depends on capacity they neither control nor operate.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, flood_plain_residents, beneficiary,
    powerless, biographical, constrained, national).

% Regional fire brigades, medical response teams, and evacuation coordinators who spend mandated hours each year on flood-scenario exercises. The hours are a real cost on top of operational duties; the same hours maintain the scenario fluency, equipment handling, and inter-agency coordination that their professional certification and their own safety in a live flood depend on. Leaving the profession is possible at career cost.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, emergency_response_personnel, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, emergency_response_personnel, payer).

% Dike wardens, inspection crews, and barrier and pumping-station operators at the regional water authorities. The statutory inspection cycle is the schedule on which they walk the defenses, assess soil and revetment condition, and operate barriers under load; the knowledge tradition passes between generations of staff through the inspection rounds themselves. Their workload is the inspection program, and their expertise is what the program maintains.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, water_board_dike_staff, beneficiary,
    organized, generational, constrained, regional).

% Write and rehearse municipal evacuation plans, run table-top and field exercises with local services, and feed local exercise findings upward into the national evaluation cycle. The exercise workload competes with their other statutory duties, and the plans they maintain are usable only insofar as the rehearsals keep them current.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, municipal_emergency_planners, beneficiary,
    moderate, biographical, constrained, regional).

% Shops, logistics firms, and service providers in areas used for large-scale exercises. During national and regional drills they bear road closures, access restrictions, and lost trading hours, and they learn exercise dates through notification rather than participating in schedule or scenario design. Their objection — disruption-minimizing scheduling — reaches the calendar only indirectly.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, drill_zone_businesses, excluded,
    moderate, immediate, constrained, local).

% The national audit institution that reviews government spending after the fact. It examines whether exercise and inspection spending is proportionate to the risk reduction it purchases, but it is not seated in calendar-setting or scenario design; its proportionality findings reach the program only as published criticism.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, fiscal_oversight_court, excluded,
    institutional, generational, analytical, national).

% The independent board that investigates disasters and near-disasters. After significant flood events it reconstructs the response and publishes findings on what held and what failed, which functions as the external test of whether exercised capacity performed under live conditions. It runs no part of the regime and bears none of its costs.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, dutch_safety_board, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__competence_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains distributed rare-event response capacity that no single actor would sustain voluntarily: emergency skills and dike-assessment knowledge decay without use, individual agencies under-invest in exercise because the benefit — protection during a rare flood — is diffuse and delayed, and the mandated cycle solves that under-provision by scheduling and verifying competence-preserving activity across all agencies at once.
% TRANSFER_FUNCTION: Moves scheduled personnel hours and public budget from response agencies and the general fund into exercised skill, verified defense condition, and rehearsed evacuation plans, and moves inspection findings into maintenance priorities. Net direction: from general taxpayers and agency time to response capacity protecting flood-plain residents.
% ABSENT_VOICES: Businesses in exercise zones bear road closures and lost trading hours during large drills but sit outside exercise design beyond notification procedures; the fiscal-oversight court reviews spending after the fact but is not seated in calendar-setting; residents in never-exercised regions fund the regime without seeing its activity locally. All three would press respectively for disruption-minimizing scheduling, proportionality limits, and geographic equity of exercise investment.
% DISAPPEARANCE_RATIONALE: Without the mandated cycle, exercise frequency would drop to what individual agencies choose under budget pressure — historically well below competence-maintaining levels — and inspection intervals would stretch. Response skill and dike-assessment knowledge would decay over roughly five to ten years, so the next significant flood would meet degraded evacuation coordination and slower defense assessment. The arrangement's absence would surface as measurable response failure at the next event rather than as any immediate visible change.
% FOUNDING_PROBLEM: Rare-event response competence decays between floods, and the cost of that decay is discovered only during the event: the 1953 North Sea flood found response coordination and defense knowledge degraded after years without a major event, at a death toll in the thousands. The regime was built to keep exercised capacity continuously live so that the interval between floods no longer erodes the response to the next one.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the regime's benefiting parties: the 1953 disaster record itself (historical, pre-regime), post-incident flood-response investigations published by the national safety board, the international skill-decay literature on rare-event emergency skills, and after-action analyses of the 2021 Limburg floods documenting where exercised capacity held and where gaps appeared. No seat inside the regime is the source of the founding-problem claim.
narrative_ontology:disappearance_verdict(preparedness_retention__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_retention__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__competence_reading, 0.17, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__competence_reading_tests).
:- end_tests(preparedness_retention__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.17 at interval end) because the arrangement's costs — mandated exercise hours, inspection workload, exercise-period business disruption — function as the coordination cost of maintaining rare-event capacity, and no named seat converts the budget into private gain: the directorate's budget passes through into scheduled exercises. Suppression is low (0.15) as a raw structural property, unscaled by scope or power: the mandate schedules and verifies activity but blocks no exit and closes no alternative — agencies may exceed the calendar, and residents face no coerced choice the regime enforces; only extractiveness is scaled downstream. Theater ratio is low (0.15): most exercise content is scenario-current and evaluated, with a ritual residue around annual fixture exercises. Accessibility collapse is moderate-low (0.35): self-organized exercises, contracted private training, and lighter inspection schedules remain workable alternatives; the mandate adds discipline rather than closing options. Resistance is low (0.22): drill fatigue, business-disruption complaints, and periodic proportionality criticism, with no organized opposition. The measurement series runs on one shared grid (0 = 1995, 30 = 2025, points every five years) for all three tracked metrics: theater_ratio and base_extractiveness creep upward through the 2005-2020 stretch as annual cycles settle into calendar fixtures and pandemic-era substitution shrinks field exercises toward tabletop formats, then fall back after the 2021 Limburg floods re-validated live field exercise and triggered program redesign; suppression_requirement stays flat because enforcement capacity was never the dynamic. The oscillation is not an extraction mechanism — it is ritualization creep corrected by live-event validation, and the base_properties values reflect the interval end state.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the directorate's seat the arrangement is a capacity-building instrument it administers; from the personnel and dike-staff seats the same calendar reads as mandated hours that return maintained professional competence — a net gain carrying a real time cost; from the resident seat it is protection that is funded and rehearsed but never operated by the protected; from the excluded business and audit seats the same exercise calendar reads as disruption and spend imposed without a design seat. The engine computes per-seat classifications from the structural data; the divergence between the setter's seat and the cost-bearing excluded seats is this story's live perspectival gap, bounded here by the reading's own low ceremony ratio.
 *
 * DIRECTIONALITY LOGIC:
 *   The declared beneficiaries (flood_plain_residents, emergency_response_personnel, water_board_dike_staff, municipal_emergency_planners) sit near the beneficiary end: the cycle subsidizes their safety and competence. The directorate, though it controls the calendar and the budget, converts rather than captures — under this reading its directionality stays near the beneficiary end because the budget terminates in scheduled exercises and its stake is a functioning mandate, not collected rent. No seat is declared a victim: personnel and dike staff bear real hours, and zone businesses bear real disruption, but under the reading these are coordination costs repaid in capacity — recorded as dual positioning and as absence from the design conversation respectively. The over_investment_boundary omega holds the open question of whether fiscal over-investment would promote a payer seat; until resolved, the no-victim structure stands.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — response competence decaying between rare floods — remains live: rare events stay inevitable and climate pressure is raising their frequency, so the arrangement has not outlived its function and no mandatrophy is declared. The classification guards against two mislabels. First, reading the mandate's enforcement machinery as extraction: the machinery schedules and verifies, it does not suppress exit or close alternatives, which is why the arrangement is claimed as pure coordination rather than a hybrid of coordination and extraction. Second, reading the ritual residue around fixture exercises as the arrangement's whole function: the theater_ratio series tracks that residue separately and shows it falling after live-event validation. The tripwire is in the series — if exercise content drifted to commemorative scheduling and evaluation became box-ticking, theater_ratio would climb past the point where maintenance is mostly performance and the arrangement would drift toward an inertially maintained shell; the current series shows the correction mechanism (post-event redesign) still working.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This file instantiates the competence_reading of the preparedness_retention kernel — the claim that the drill-and-inspection regime preserves live operational capacity. Would the sibling readings change the classification structurally: the husk_reading (same drills as memorial performance lacking live competence, re-authoring theater_ratio high and drifting the arrangement toward theatrical maintenance) or the hybrid_reading (competence retained only in specialized institutions while broader societal memory turns ceremonial, splitting the beneficiary structure between specialized and general seats)?',
    'Exercise-validity audit: correlate drill participation and performance with live-event performance in post-incident after-action records (notably the 2021 Limburg floods); audit exercise design for scenario currency versus commemorative or fixture scheduling; compare retained competence across specialized and general institutions.',
    'If the ceremony-to-competence ratio is actually high, theater_ratio re-authors upward and the arrangement drifts toward an inertially maintained shell; if competence is stratified, the beneficiary structure splits and the no-victim declaration narrows to the specialized seats. If this reading holds, the rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, empirical, 'Kernel contest: this story is one reading of preparedness_retention; siblings re-author the ceremony ratio and beneficiary structure of the same activity.').

omega_variable(
    over_investment_boundary,
    'At what exercise intensity does the regime begin over-investing — drawing fiscal efficiency from competing public budgets without proportional competence gain, i.e. where does the competence dose-response curve plateau relative to current drill and inspection intensity?',
    'Marginal-competence-per-euro analysis across exercise frequencies; skill-retention dose-response research; cross-jurisdiction comparison of drill intensity against flood-response outcomes.',
    'If a plateau exists below current intensity, a payer seat (competing budget programs and their constituencies) emerges and the arrangement carries a small extraction component; if returns hold at current intensity, the no-victim structure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(over_investment_boundary, empirical, 'Whether exercise and inspection spending sits on the productive or plateau side of the competence dose-response curve.').

omega_variable(
    drill_to_live_transfer_validity,
    'Does competence exercised in scheduled drills transfer to live-event performance under real stress, event duration, and cascading failure — or does the fidelity gap between scheduled exercise and live flood erode the transfer the regime is built on?',
    'Post-incident correlation between exercise participation and live response performance; simulation-fidelity and stress-inoculation research; comparison of exercised versus less-exercised units responding to the same event.',
    'Weak transfer undermines the reading''s foundational axiom and shifts warrant toward accounts on which the drills do not bear live competence; strong transfer consolidates the competence-preserving classification and the low theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_to_live_transfer_validity, empirical, 'Whether exercised competence survives the fidelity gap between scheduled drills and live floods.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(prep_tr_t5, preparedness_retention__competence_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__competence_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(prep_tr_t15, preparedness_retention__competence_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__competence_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(prep_tr_t25, preparedness_retention__competence_reading, theater_ratio, 25, 0.21).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__competence_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__competence_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(prep_be_t5, preparedness_retention__competence_reading, base_extractiveness, 5, 0.17).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__competence_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(prep_be_t15, preparedness_retention__competence_reading, base_extractiveness, 15, 0.2).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__competence_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement(prep_be_t25, preparedness_retention__competence_reading, base_extractiveness, 25, 0.22).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__competence_reading, base_extractiveness, 30, 0.17).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__competence_reading, suppression_requirement, 0, 0.14).
narrative_ontology:measurement(prep_su_t5, preparedness_retention__competence_reading, suppression_requirement, 5, 0.15).
narrative_ontology:measurement(prep_su_t10, preparedness_retention__competence_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(prep_su_t15, preparedness_retention__competence_reading, suppression_requirement, 15, 0.16).
narrative_ontology:measurement(prep_su_t20, preparedness_retention__competence_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(prep_su_t25, preparedness_retention__competence_reading, suppression_requirement, 25, 0.16).
narrative_ontology:measurement(prep_su_t30, preparedness_retention__competence_reading, suppression_requirement, 30, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% Kernel-family linkage: the preparedness_retention kernel decomposes into three readings of the same drill-and-inspection activity, each a separate constraint with its own ε and beneficiary structure. This file (competence_reading) authors the activity as competence-preserving — low ceremony-to-competence ratio, no victim seat. The sibling files author the same activity as memorial performance (husk_reading) and as institutionally stratified retention (hybrid_reading). The ε divergence is the point: the readings differ on the empirical referent of theater_ratio and on the beneficiary structure, so they cannot share one story without violating ε-invariance. Each file links the others here and via cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
