% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: preparedness_persistence__husk_reading
 *   human_readable: Flood Preparedness Drill Regime — Husk Reading (Form Without Function)
 *   domain: disaster preparedness / institutional memory / commitment systems
 *
 * SUMMARY:
 *   This story instantiates the husk_reading of the preparedness_persistence
 *   kernel: drills and inspections are read as memorial performance — a form
 *   maintained because it once mapped to real operational competence, but
 *   where the underlying competence has atrophied while the ceremony of
 *   practicing it persists unchanged. On this reading the drill regime is a
 *   Piton: what began as a Rope (genuine coordination against real flood
 *   risk) has degraded into inertial theater, kept alive not because any
 *   concentrated party profits from it but because no single administrator
 *   bears a cost sharp enough to force redesign, while the diffuse cost of
 *   failure falls on floodplain residents who have no voice in the process.
 *   This is deliberately ONE reading among three siblings sharing the same
 *   kernel: competence_reading holds the opposite empirical claim (that the
 *   drills are live exercised knowledge), and hybrid_reading holds that the
 *   truth is stratified by component (engineering inspection remains
 *   substantive; evacuation drills are ritualized). This story does not
 *   adjudicate between them — it is the clean, self-contained husk_reading
 *   constraint, ε-invariant on its own terms.
 *
 * KEY AGENTS:
 *   - emergency_management_agency_leadership: agenda_setter (institutional/constrained) — administers the ritual, cannot easily afford to expose it
 *   - municipal_officials_seeking_certification_credit: beneficiary (organized/constrained) — collects legitimacy and funding eligibility from documented completion
 *   - floodplain_residents: payer (powerless/trapped) — bears the risk the drills are supposed to mitigate, with no way to verify readiness
 *   - frontline_responders_relying_on_the_plan: payer/excluded (moderate/constrained) — knows the plan is stale, feedback not incorporated
 *   - engineering_inspectors_of_physical_infrastructure: excluded (moderate/mobile) — substantive work lends unearned credibility to the ritualized components
 *   - state_and_federal_grant_auditors: observer (institutional/analytical) — audits form, not function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__husk_reading, 0.58).
domain_priors:suppression_score(preparedness_persistence__husk_reading, 0.44).
domain_priors:theater_ratio(preparedness_persistence__husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, theater_ratio, 0.81).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__husk_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__husk_reading, "Flood Preparedness Drill Regime — Husk Reading (Form Without Function)").
narrative_ontology:topic_domain(preparedness_persistence__husk_reading, "disaster preparedness / institutional memory / commitment systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__husk_reading, '8e9d4fbe-dd09-4fe3-af8e-12c030318cbe').
narrative_ontology:cs_kernel_codification('8e9d4fbe-dd09-4fe3-af8e-12c030318cbe', formalized).
narrative_ontology:cs_authority_grounding('8e9d4fbe-dd09-4fe3-af8e-12c030318cbe', practice).
narrative_ontology:cs_interpretation_layer_present('8e9d4fbe-dd09-4fe3-af8e-12c030318cbe').
narrative_ontology:cs_reading_relation('8e9d4fbe-dd09-4fe3-af8e-12c030318cbe', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e9d4fbe-dd09-4fe3-af8e-12c030318cbe', preparedness_persistence__hybrid_reading, influences).
narrative_ontology:cs_axiom('8e9d4fbe-dd09-4fe3-af8e-12c030318cbe', foundational, repeated_form_without_verification_signals_atrophy).
narrative_ontology:cs_axiom_status(repeated_form_without_verification_signals_atrophy, holdable).
narrative_ontology:cs_axiom_grounding('8e9d4fbe-dd09-4fe3-af8e-12c030318cbe', repeated_form_without_verification_signals_atrophy, empirically_contingent).
narrative_ontology:cs_axiom('8e9d4fbe-dd09-4fe3-af8e-12c030318cbe', secondary, certification_completion_is_not_evidence_of_capacity).
narrative_ontology:cs_axiom_status(certification_completion_is_not_evidence_of_capacity, holdable).
narrative_ontology:cs_axiom_grounding('8e9d4fbe-dd09-4fe3-af8e-12c030318cbe', certification_completion_is_not_evidence_of_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('8e9d4fbe-dd09-4fe3-af8e-12c030318cbe', post_disaster_coordination_mandate).
narrative_ontology:cs_drift_state('8e9d4fbe-dd09-4fe3-af8e-12c030318cbe', contemporary_certification_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8e9d4fbe-dd09-4fe3-af8e-12c030318cbe', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__husk_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, emergency_management_agency_leadership).
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, municipal_officials_seeking_certification_credit).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, floodplain_residents).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, frontline_responders_relying_on_the_plan).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the annual drill calendar and inspection checklist, reports completion rates upward to state and federal funders, and renews accreditation on the basis of documented exercises rather than measured outcomes. Could redesign the drills to test real capacity but the cost of admitting the current regime is hollow — in political exposure, funding risk, and staff morale — exceeds what leadership is willing to bear. No party here profits in a concentrated way; the benefit is diffuse legitimacy, not rent.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, emergency_management_agency_leadership, agenda_setter,
    institutional, generational, constrained, regional).

% Uses completed drill logs and inspection sign-offs to satisfy grant conditions, insurance ratings, and constituent reassurance after past flood events. Benefits from the appearance of preparedness without bearing the cost of verifying it works; has no incentive to expose the gap because doing so would jeopardize the same certifications.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, municipal_officials_seeking_certification_credit, beneficiary,
    organized, biographical, constrained, regional).

% Live inside the mapped inundation zone and are told evacuation routes and shelter plans are current and tested. Cannot independently verify whether the plan would function during an actual flood; bear the full consequence if it does not, and have no meaningful say in how drills are designed or scored.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, floodplain_residents, payer,
    powerless, biographical, trapped, local).

% Fire, EMS, and public works crews who would execute the plan under real conditions. Many privately report the tabletop exercises do not reflect current staffing, equipment, or road conditions, but their operational feedback is not fed back into plan revision — the drill schedule is fixed by the calendar, not by after-action findings.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, frontline_responders_relying_on_the_plan, payer,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__husk_reading, frontline_responders_relying_on_the_plan, excluded).

% Perform levee and pump-station structural inspections that remain technically substantive (per the hybrid reading) but their findings are absorbed into the same reporting apparatus that treats evacuation drills as equivalently rigorous — their credible work lends unearned credibility to the ritualized components, and they are not consulted on the evacuation drill design.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, engineering_inspectors_of_physical_infrastructure, excluded,
    moderate, biographical, mobile, local).

% Review completion logs to release preparedness funding but audit for procedural compliance (was a drill held, was a checklist signed) rather than for operational efficacy (did the drill reveal or fix a real gap). Their audit design is itself part of why the husk persists undetected.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, state_and_federal_grant_auditors, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_persistence__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: standardize and rehearse a shared evacuation and response plan across agencies so that, in a real flood, everyone from dispatch to shelter staff executes a known, drilled sequence rather than improvising under crisis conditions.
% TRANSFER_FUNCTION: Moves institutional legitimacy and continued funding eligibility to the agencies that administer and certify the drills, at the cost of a false sense of security transferred to residents and an absence of genuine operational readiness delivered to responders and the public.
% ABSENT_VOICES: Frontline responders who know the plan is stale are structurally present in the drills but their after-action observations are not routed into plan revision. Floodplain residents, who would bear the entire cost of a failed evacuation, have no seat in drill design or scoring at all.
% DISAPPEARANCE_RATIONALE: If the drill and inspection regime vanished overnight, agencies would lose certification and funding immediately (a real institutional rearrangement), but on this reading actual flood-response capacity would change little, because the drills were not producing that capacity in the first place — the population's real exposure is already what it is; only the paperwork proving otherwise would disappear. Whether the world 'rearranges' therefore depends on whether you mean the institutional apparatus (yes) or actual operational safety (largely no) — hence contested rather than a single verdict.
% FOUNDING_PROBLEM: Repeated flood disasters where uncoordinated agencies improvised badly under crisis conditions, causing preventable deaths and chaotic evacuations; the drill regime was built to convert crisis response from improvisation into rehearsed, coordinated action.
% FOUNDING_PROBLEM_CORROBORATION: Frontline responders and independent after-action reviewers from outside the certifying agencies (in post-flood inquiries following underperforming evacuations) attest the founding problem remains live and unsolved because the drills no longer test real capacity. Agency leadership and municipal officials — the parties who benefit from continued certification — attest the problem is being actively managed; no corroboration for that claim exists from a source outside the benefiting institutions.
narrative_ontology:disappearance_verdict(preparedness_persistence__husk_reading, contested).
narrative_ontology:founding_problem_status(preparedness_persistence__husk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__husk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__husk_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Theater ratio is authored high and rising (0.35 -> 0.81) because, on this reading, the gap between what is performed (drills held, checklists signed) and what is actually verified (real operational capacity under flood conditions) widens over time as staff turnover, budget cuts, and route changes accumulate unaddressed inside a fixed ceremonial format. Extractiveness is moderate and rising in parallel (0.31 -> 0.58) — not extraction in the sense of rent capture by a concentrated actor, but in the sense that residents' trust and funders' resources are being spent on a mechanism that no longer delivers the coordination it claims to. Suppression is moderate (0.44): there is no active coercive enforcement keeping residents from questioning the drills, but accessibility collapse is fairly high (0.62) because once a jurisdiction is certified, there is no institutional pathway for an outside party to trigger an efficacy audit — the certification apparatus is self-referential. Resistance is low (0.28): the people best positioned to know the drills are hollow (frontline responders) have no formal channel and limited incentive to escalate, since doing so implicates their own agency's certification.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (agency leadership), the drill calendar looks like ongoing, diligent stewardship of a coordination function — boxes checked, funding secured, no crisis has yet exposed the gap. From the payer seat (floodplain residents, frontline responders), the same activity looks like an inherited ritual that has stopped tracking the reality it claims to represent. The engine should compute these as structurally different experiences of the identical drill calendar: institutional legitimacy accrues at one seat while risk accrues, unaddressed, at another.
 *
 * DIRECTIONALITY LOGIC:
 *   Agency leadership and certifying municipal officials sit near the beneficiary end: they collect legitimacy, funding eligibility, and reduced political exposure from the existence of the drill record, largely independent of whether the drills would work. Floodplain residents sit near the full-target end: trapped by geography, unable to verify the plan, and bearing total exposure if it fails. Frontline responders are a harder case — structurally close to targets (they would pay first and worst in a real event) but with enough professional standing to be marked payer/excluded rather than purely powerless; their moderate power does not translate into influence over drill design because their input channel does not exist.
 *
 * MANDATROPHY ANALYSIS:
 *   The husk reading is precisely a mandatrophy diagnosis: the founding problem (uncoordinated, deadly improvisation during floods) may or may not still be live, but the mechanism built to solve it (rehearsed, standardized drills) has kept its form while its function decayed. Classifying it as Piton rather than Mountain prevents the error of treating institutional inertia as irreducible necessity — a Mountain framing would suggest the drills are simply what preparedness requires and cannot be otherwise, foreclosing the question of whether they still do anything. Piton framing keeps the question open: this is a former (or partially former) coordination mechanism whose maintenance is now explainable by administrative inertia and legitimacy-signaling rather than active operational value, and which persists because no single actor's incentive is sharp enough to fix it, not because fixing it is impossible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_vs_competence_empirical_test,
    'Is the drill regime''s operational content genuinely atrophied (husk_reading) or does exercised repetition itself maintain real readiness even without visible innovation (competence_reading)?',
    'Compare drill outcomes against blind, unannounced full-scale exercises or actual flood event performance; a wide gap between scripted-drill success and unannounced-exercise/real-event performance corroborates the husk reading, while a narrow gap corroborates the competence reading.',
    'If competence_reading is empirically supported, this constraint''s classification as Piton is wrong and the arrangement should instead be read as a functioning Rope; the two readings make incompatible claims about the same drill calendar and only one is likely descriptively accurate for a given jurisdiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_competence_empirical_test, empirical, 'Whether atrophy or live maintenance better describes the drill regime''s actual operational content.').

omega_variable(
    component_stratification_ambiguity,
    'Does the husk framing apply uniformly across all preparedness components, or does the hybrid_reading''s stratification (competent engineering inspection, ritualized evacuation drills) better describe the actual mixed reality within a single jurisdiction?',
    'Component-by-component efficacy audit distinguishing structural/engineering inspection outcomes from human-coordination drill outcomes within the same jurisdiction and time period.',
    'If stratification holds, treating the entire preparedness apparatus as a single husk overstates the failure of the engineering components and understates the specific failure of the human-coordination components; classification should be split across sub-constraints rather than applied to the aggregate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(component_stratification_ambiguity, conceptual, 'Whether uniform husk framing or component-level stratification is the more accurate account of a given jurisdiction''s preparedness apparatus.').

omega_variable(
    d5_risk_exposure,
    'Given the framework''s D5 risk flag (atrophied capacity mistaken for Mountain-grade necessity), how confident can any external observer be that a specific jurisdiction''s drill regime is husk rather than competent, absent a triggering flood event to reveal the truth?',
    'Track jurisdictions through actual flood events and retrospectively code pre-event drill regimes as husk/competent/hybrid based on post-event performance; build a base rate.',
    'Without post-event corroboration, the husk classification remains a plausible but unverified hypothesis for any specific jurisdiction — high D5 risk means the cost of misclassifying a genuinely functional Rope as an atrophied Piton (or vice versa) is asymmetric and potentially severe (population safety on one side, unwarranted institutional distrust on the other).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(d5_risk_exposure, empirical, 'The irreducible difficulty of verifying husk-vs-competence classification absent a real disaster event.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__husk_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__husk_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(prep_tr_t4, preparedness_persistence__husk_reading, theater_ratio, 4, 0.46).
narrative_ontology:measurement(prep_tr_t8, preparedness_persistence__husk_reading, theater_ratio, 8, 0.56).
narrative_ontology:measurement(prep_tr_t12, preparedness_persistence__husk_reading, theater_ratio, 12, 0.65).
narrative_ontology:measurement(prep_tr_t16, preparedness_persistence__husk_reading, theater_ratio, 16, 0.72).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__husk_reading, theater_ratio, 20, 0.78).
narrative_ontology:measurement(prep_tr_t24, preparedness_persistence__husk_reading, theater_ratio, 24, 0.81).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__husk_reading, base_extractiveness, 0, 0.31).
narrative_ontology:measurement(prep_be_t4, preparedness_persistence__husk_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement(prep_be_t8, preparedness_persistence__husk_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(prep_be_t12, preparedness_persistence__husk_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(prep_be_t16, preparedness_persistence__husk_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__husk_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(prep_be_t24, preparedness_persistence__husk_reading, base_extractiveness, 24, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_persistence__husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__husk_reading, 0.1).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language concept 'flood preparedness drills and inspections' per the ε-invariance principle. competence_reading claims low extraction (drills genuinely maintain readiness); husk_reading (this story) claims moderate, rising extraction via institutional legitimacy capture at population risk expense; hybrid_reading splits the difference by component. All three share the kernel_id preparedness_persistence and are linked bidirectionally via affects_constraints; none averages or hedges across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
