% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Flood Drill and Inspection Calendar — Memorial Performance (Husk Reading)
 *   domain: institutional/disaster-preparedness
 *
 * SUMMARY:
 *   A basin-wide flood-response regime requires every agency in the watershed
 *   to run a fixed annual calendar of joint drills and equipment inspections,
 *   certified through reports to a central audit office and summarized to the
 *   council as standing assurance of readiness. This story instantiates the
 *   husk reading of that regime: the calendar's form — schedules, scripts,
 *   certificates, pass-rates — persists intact while the operational capacity
 *   it was built to rehearse has quietly atrophied beneath it. Exercise
 *   scenarios repeat with few injected surprises; the veteran staff who
 *   remembered the founding flood have retired; equipment ages faster than
 *   the inspection cycle renews it; and the assurance pipeline rewards
 *   completed documentation rather than tested capability, so each certified
 *   cycle deepens the gap between displayed and deliverable readiness. The
 *   regime is presented to the public as obvious necessity — agencies drill,
 *   therefore the region is ready — a presentation this reading treats as the
 *   load-bearing wall of its persistence. This file instantiates one reading
 *   of the preparedness_persistence kernel; sibling readings are separate
 *   constraint files linked via network.affects_constraints (see
 *   commentary.kernel_context). KEY AGENTS (by structural relationship): -
 *   emergency_management_agency: Agenda-setter and incidental beneficiary
 *   (institutional/identity_locked) — administers the calendar, collects
 *   budget continuity and legitimacy; could redesign the regime but bears
 *   little of its harm - elected_officials: Beneficiary (powerful/mobile) —
 *   converts drill visibility into stewardship credit; exits with the
 *   electoral cycle - flood_risk_residents: Primary payer (powerless/trapped)
 *   — funds the regime and bears the gap between performed and actual
 *   readiness - frontline_responders: Payer (organized/constrained) —
 *   executes the scripts, inherits the atrophied capability, bears
 *   operational risk in real events - regional_audit_office: Analytical
 *   observer (institutional/analytical) — audits compliance documentation,
 *   not field capability - community_resilience_advocates: Excluded voice
 *   (moderate/constrained) — proposes capability-based alternatives; holds no
 *   seat in the compliance conversation
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__husk_reading, 0.63).
domain_priors:suppression_score(preparedness_persistence__husk_reading, 0.52).
domain_priors:theater_ratio(preparedness_persistence__husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__husk_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__husk_reading, "Flood Drill and Inspection Calendar — Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_persistence__husk_reading, "institutional/disaster-preparedness").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__husk_reading, '9d8b5684-d865-4ac1-bd2d-c7676d34234d').
narrative_ontology:cs_kernel_codification('9d8b5684-d865-4ac1-bd2d-c7676d34234d', formalized).
narrative_ontology:cs_authority_grounding('9d8b5684-d865-4ac1-bd2d-c7676d34234d', extraction).
narrative_ontology:cs_interpretation_layer_present('9d8b5684-d865-4ac1-bd2d-c7676d34234d').
narrative_ontology:cs_reading_relation('9d8b5684-d865-4ac1-bd2d-c7676d34234d', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d8b5684-d865-4ac1-bd2d-c7676d34234d', preparedness_persistence__hybrid_reading, influences).
narrative_ontology:cs_axiom('9d8b5684-d865-4ac1-bd2d-c7676d34234d', foundational, ritual_form_outlives_operational_capacity).
narrative_ontology:cs_axiom_status(ritual_form_outlives_operational_capacity, holdable).
narrative_ontology:cs_axiom_grounding('9d8b5684-d865-4ac1-bd2d-c7676d34234d', ritual_form_outlives_operational_capacity, empirically_contingent).
narrative_ontology:cs_axiom('9d8b5684-d865-4ac1-bd2d-c7676d34234d', foundational, compliance_display_substitutes_for_capability).
narrative_ontology:cs_axiom_status(compliance_display_substitutes_for_capability, holdable).
narrative_ontology:cs_axiom_grounding('9d8b5684-d865-4ac1-bd2d-c7676d34234d', compliance_display_substitutes_for_capability, empirically_contingent).
narrative_ontology:cs_reference_frame('9d8b5684-d865-4ac1-bd2d-c7676d34234d', joint_capability_rehearsal_standard).
narrative_ontology:cs_drift_state('9d8b5684-d865-4ac1-bd2d-c7676d34234d', contemporary_post_turnover_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9d8b5684-d865-4ac1-bd2d-c7676d34234d', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__husk_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, emergency_management_agency).
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, elected_officials).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, flood_risk_residents).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, frontline_responders).
narrative_ontology:constraint_vindicates(preparedness_persistence__husk_reading, compliance_equals_readiness_doctrine).
narrative_ontology:constraint_vindicates(preparedness_persistence__husk_reading, scheduled_exercise_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the annual drill and inspection calendar mandated after the landmark flood. Schedules exercises, certifies completion, files compliance reports to the oversight office, and receives appropriations sized to program activity. Leadership rotates every few years; redesigning the calendar would require publicly revisiting decades of certified reports. Leaving is not a live option for the institution — its mandate, staffing plan, and public self-description are built around the calendar.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, emergency_management_agency, agenda_setter,
    institutional, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__husk_reading, emergency_management_agency, beneficiary).

% Attend headline exercises, preside over equipment unveilings, and cite inspection pass-rates in budget hearings and campaigns. Their tenure spans one or two election cycles; attention moves to the next visible issue after the term, and none of the flood outcomes land on them personally.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, elected_officials, beneficiary,
    powerful, immediate, mobile, regional).

% Live in the mapped floodplain, fund the regime through local levies, and receive its assurances — drill notices, pass-rate summaries, preparedness pamphlets. When flooding comes they rely on the response the exercises were meant to rehearse. Moving out of the floodplain means selling exposed property at a discount; most households stay.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, flood_risk_residents, payer,
    powerless, generational, trapped, local).

% Staff the fire, rescue, and volunteer brigades that execute the scripted exercises and respond to real floods. Equipment ages, mutual-aid rosters thin as neighboring services cut hours, and exercise scenarios rarely vary year to year. Flagging the gap between the script and actual capability carries career risk inside a small professional community.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, frontline_responders, payer,
    organized, biographical, constrained, regional).

% Reviews the drill reports and inspection certificates the agencies submit. Its methodology samples documentation for completeness and internal consistency rather than testing field capability; its findings feed the annual assurance summary presented to the council.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, regional_audit_office, observer,
    institutional, generational, analytical, regional).

% Propose no-notice exercises, independent equipment audits, and household-level preparedness subsidies. They petition the council and speak at budget hearings but hold no seat on the exercise-planning committee where scenarios, injected faults, and inspection criteria are set; their proposals surface in the record mainly after flood events.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, community_resilience_advocates, excluded,
    moderate, generational, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_persistence__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real multi-agency problem: flood response requires fire, rescue, public works, health, and neighboring jurisdictions to act in concert. The calendar gives them a fixed schedule of joint exercises, a shared inspection vocabulary for equipment, standing contact rosters, and pre-negotiated mutual-aid triggers — scaffolding that would otherwise have to be improvised mid-event.
% TRANSFER_FUNCTION: Moves local levy revenue and responder staff-hours into scheduled exercise and inspection activity; moves documented assurance upward (agency to audit office to council) and outward (council to residents); and, when delivered capability falls short of the certified version, moves the difference onto residents as unpriced casualty and property risk.
% ABSENT_VOICES: Community resilience advocates and independent hazard engineers would object that completion-certified exercises measure attendance, not capability; flood-risk residents appear in the process only as exercise audiences and pamphlet recipients. Both sit outside the planning committee where scenarios and inspection criteria are set, and their objections enter the record chiefly as post-flood inquiry recommendations.
% DISAPPEARANCE_RATIONALE: Overnight loss would not restore lost capability — this reading's own claim is that little operational capacity rides on the form — but the institutional web would rearrange immediately: levy formulas keyed to program activity, statutory drill mandates, inter-agency contact rosters and mutual-aid triggers, the audit office's review pipeline, and the council's assurance reporting would all lose their object. The first flood after dissolution would force improvisation exactly where rosters and triggers had stood.
% FOUNDING_PROBLEM: After the landmark flood, the region's agencies had no joint plan: commands could not reach each other on compatible channels, equipment was uninspected and mutually incompatible, and mutual aid was negotiated ad hoc mid-crisis. The drill-and-inspection calendar was built to rehearse joint response and verify equipment before the next event.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: the post-flood inquiry commission, chaired from outside the agency, found command-level interoperability objectives met by the regime's second decade, and the regional engineering society's independent reviews attest that equipment-compatibility goals were achieved. Both sources also record that later floods exposed failure modes the calendar never tested — aging equipment under load, thin night-shift rosters — supporting the reading that the original problem closed while the activity persisted.
narrative_ontology:disappearance_verdict(preparedness_persistence__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_persistence__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__husk_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

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
 *   Theater_ratio is the signature metric of this reading and is authored high (0.78 at interval end): scenarios recur annually with rare variation, evaluation criteria reward completion and punctuality, and the report template has not materially changed in a decade — the activity is real, but its content has decoupled from the capability it certifies. Base extractiveness (0.63) rises across the interval because the inputs the regime consumes — levy revenue, responder staff-hours, council attention — purchase progressively less deliverable capability as tacit knowledge retires and equipment depreciates; the end-state value matches the final measurement point. Suppression (0.52) is moderate and mostly structural: funding is conditioned on compliance documentation, statute mandates the calendar, and dissent about scenario realism carries career cost inside small professional communities; a working assumption of roughly 60% structural to 40% internalized is carried as an omega rather than resolved inside the scalar. Accessibility_collapse (0.45): alternatives — no-notice exercises, independent capability audits, household-level preparedness subsidies — remain conceivable and periodically proposed, but they are crowded out of budget and agenda rather than made impossible. Resistance (0.3) is the inter-flood baseline; it spikes after significant flood events when inquiries recommend capability testing, then decays as memory fades — the oscillation is documented as an omega rather than forced into the scalar series. The three tracked metrics share one time grid (t=0,6,12,18,24,30) so the engine samples every metric at every point without scalar substitution. Claim and metrics are independent authored facts: claimed_type piton is asserted from structure — inertial persistence, cost-asymmetry between the actor who could fix the regime and the actors harmed by it, and no seat capturing the gap — while the metrics describe observed operation; the engine computes per-seat types from the structural data, and any divergence from the claim is the datum the corpus exists to take.
 *
 * PERSPECTIVAL GAP:
 *   From the agency seat the calendar is mandate, craft, and public face — the same reports that read as boilerplate from the audit seat read as proof of diligence internally. Residents experience the regime as assurance received and risk retained; responders experience it as script executed and capability inherited. The analytical observer seat sees the full loop: assurance produced upward, legitimacy returned downward, capability gap carried by those with no seat. Per-seat classifications are computed by the engine from power, exit, and role — the divergence between the agency's lived coordination experience and the residents' lived exposure is the measurement, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations map cleanly onto the derivation chain, so no directionality overrides are authored. The agency and elected officials sit on the subsidy side (d near 0): the calendar spends on activity they preside over and take credit for, and both hold exit or identity positions that damp any felt cost. Flood-risk residents are near-full targets (d near 1), amplified by trapped exit — they cannot relocate cheaply and cannot opt out of the levy. Frontline responders are strong targets (d near 0.7) with constrained exit: vocation-bound, unable to individually alter the calendar. The audit office is analytically neutral. Receipt surface: the gap between funded, displayed readiness and deliverable capability accrues to no named seat — it dissipates as unpriced casualty and property risk at flood time — so gain_flow is affirmatively authored as diffuse after checking every seat; the agency's budget continuity and legitimacy are benefit-from-appropriation, not receipt of the gap. Fixing is prohibitive for the only actor positioned to attempt it: repair requires publicly recertifying decades of reports against capability tests, the cost lands on the agency, and the harm it would remedy lands on residents — the cost-asymmetry that lets the form persist. Regional spatial scope scales effective extraction modestly upward, since verifying field capability across jurisdictions is exactly what the regime fails to do.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the inter-agency coordination void exposed by the landmark flood — was substantively solved around the interval midpoint; the arrangement persisted past its function. The authored combination of founding_problem_status=dead with disappearance_verdict=world_rearranges is the mismatch signature the consumer cross-checks against the computed piton path: a zombie arrangement kept alive by inertia and legitimacy display rather than by its founding purpose. Mandatrophy discipline prevents two mislabels. Reading the regime as live coordination because coordination artifacts (rosters, mutual-aid triggers) exist ignores that those artifacts now maintain themselves through routine contact rather than through the exercises. Reading it as captured extraction because budgets flow ignores that no seat collects the gap — the losses materialize as diffuse catastrophe risk. The husk reading locates the regime between those poles: function atrophied, gains uncaptured, exit blocked by identity fusion and cost asymmetry rather than by coercion. Identity-lock dynamics: the agency's institutional self-concept has fused with the drill calendar — it has become the organization that drills — so admitting the atrophy is unthinkable from inside; if that frame broke (post-flood scandal, leadership turnover), the trajectory splits between a deliberate transitional rebuild and a hardened defensive posture, and the classification would move accordingly (carried as an omega).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Which reading of the preparedness_persistence kernel does the evidence support: does the drill-and-inspection regime preserve operational capacity (competence_reading), display a form whose capacity has atrophied (this husk_reading), or split into live and ritual components (hybrid_reading)?',
    'Unannounced no-notice exercises with injected faults, timed against the scripted drill baselines; equipment-function audits decoupled from the inspection calendar; comparison with jurisdictions that replaced scheduled drills with capability audits.',
    'If competence_reading is confirmed, theater_ratio collapses toward coordination cost and the regime recomputes as rope-like; if hybrid_reading is confirmed, the story decomposes into a live-inspection rope story and a ritual-drill piton story linked by network edges; if husk_reading is confirmed, the piton stands and reform pressure shifts to sunset and redesign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one reading of kernel preparedness_persistence; the sibling readings would restructure the theater load and the beneficiary/victim symmetry.').

omega_variable(
    naturality_framing_shield,
    'Is the drill calendar''s persistence treated as natural necessity — of course agencies drill — such that the framing itself shields a constructed, contestable arrangement from ever reaching the reform agenda?',
    'Trace the challenge history: council motions, budget hearings, and post-inquiry recommendations proposing replacement of scheduled drills with capability audits — did any reach a vote, and what framing did opponents invoke?',
    'If natural-law framing blocks agenda entry, the regime''s mountain-like presentation is doing shielding work and the piton is stickier than the metrics alone suggest; if challenges were defeated on assessed merits, the presentation is benign and the metrics carry the full story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturality_framing_shield, conceptual, 'Atrophied capacity mistaken for a natural law: whether naturality framing functions as a shield against revision.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is responder silence about the gap between the exercise script and actual capability structural (career and funding consequences for dissent) or internalized (professional culture equating drill loyalty with competence)?',
    'Anonymized reporting rates before and after whistleblower-protection adoption; exit interviews with veteran responders; comparison with units that adopted no-notice exercises.',
    'If the internalized share is large, suppression persists after formal protections are removed and the effective suppression exceeds the structural measure — the target carries it after exit; the classification consequence rides this omega rather than the scalar.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism split for the responder seat.').

omega_variable(
    identity_lock_break_trajectory,
    'If the agency''s institutional identity frame broke — post-flood scandal, abrupt leadership turnover — does the regime convert into a deliberate transitional rebuild or harden into a defensively administered extraction?',
    'Comparative case studies of peer agencies after capability-failure scandals; observe whether replacement regimes carry sunset clauses and independent capability audits or expand compliance documentation instead.',
    'A rebuild trajectory implies the current arrangement is a stalled transition with recoverable function; a hardening trajectory implies the legitimacy benefit has consolidated into capture and the effective victim set widens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_break_trajectory, conceptual, 'Post-identity-break classification trajectory for the agenda-setting seat.').

omega_variable(
    post_event_resistance_cycle,
    'Does resistance to the regime oscillate with flood memory — spiking after significant events as inquiries recommend capability testing, then decaying between events?',
    'Time-stamped inquiry launches, budget-amendment attempts, and coverage intensity aligned against flood-event dates across multiple event cycles.',
    'If the oscillation is confirmed, single-snapshot resistance understates reform windows and the base_properties value is a phase-dependent sample; the oscillation itself may function as a pressure-release valve that lets the form survive each spike intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_event_resistance_cycle, empirical, 'Cyclical resistance driven by flood-event memory; the authored 0.3 is the inter-event baseline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__husk_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__husk_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t6, preparedness_persistence__husk_reading, theater_ratio, 6, 0.48).
narrative_ontology:measurement_basis(prep_tr_t6, observed).
narrative_ontology:measurement(prep_tr_t12, preparedness_persistence__husk_reading, theater_ratio, 12, 0.6).
narrative_ontology:measurement_basis(prep_tr_t12, observed).
narrative_ontology:measurement(prep_tr_t18, preparedness_persistence__husk_reading, theater_ratio, 18, 0.68).
narrative_ontology:measurement_basis(prep_tr_t18, observed).
narrative_ontology:measurement(prep_tr_t24, preparedness_persistence__husk_reading, theater_ratio, 24, 0.74).
narrative_ontology:measurement_basis(prep_tr_t24, observed).
narrative_ontology:measurement(prep_tr_t30, preparedness_persistence__husk_reading, theater_ratio, 30, 0.78).
narrative_ontology:measurement_basis(prep_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__husk_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t6, preparedness_persistence__husk_reading, base_extractiveness, 6, 0.47).
narrative_ontology:measurement_basis(prep_be_t6, observed).
narrative_ontology:measurement(prep_be_t12, preparedness_persistence__husk_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement_basis(prep_be_t12, observed).
narrative_ontology:measurement(prep_be_t18, preparedness_persistence__husk_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement_basis(prep_be_t18, observed).
narrative_ontology:measurement(prep_be_t24, preparedness_persistence__husk_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement_basis(prep_be_t24, observed).
narrative_ontology:measurement(prep_be_t30, preparedness_persistence__husk_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement_basis(prep_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__husk_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t6, preparedness_persistence__husk_reading, suppression_requirement, 6, 0.41).
narrative_ontology:measurement_basis(prep_su_t6, observed).
narrative_ontology:measurement(prep_su_t12, preparedness_persistence__husk_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement_basis(prep_su_t12, observed).
narrative_ontology:measurement(prep_su_t18, preparedness_persistence__husk_reading, suppression_requirement, 18, 0.47).
narrative_ontology:measurement_basis(prep_su_t18, observed).
narrative_ontology:measurement(prep_su_t24, preparedness_persistence__husk_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement_basis(prep_su_t24, observed).
narrative_ontology:measurement(prep_su_t30, preparedness_persistence__husk_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(prep_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'flood preparedness drills' decomposes into three structurally distinct claims about the same activities, per the epsilon-invariance principle: competence_reading (practice maintains readiness — low theater, rope-flavored), husk_reading (this file — form persists while capacity atrophies, piton), and hybrid_reading (stratified — inspection live, drills ritualized, decomposing further into a rope story and a piton story). Epsilon differs sharply across the family because the assessed function of the same referent differs; each file authors its own epsilon for the standing arrangement and none averages across readings. Upstream/downstream structure: competence_reading is the official upstream claim — its pass-rate statistics are cited as evidence against the husk critique — while the husk reading is the critical downstream reading those statistics are deployed to rebut. Every family member reciprocally lists the others in affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
