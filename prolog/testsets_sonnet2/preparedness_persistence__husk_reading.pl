% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Flood Preparedness Drill-and-Inspection Regime (Husk Reading)
 *   domain: disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   This story instantiates the husk reading of the preparedness_persistence
 *   kernel: the claim that flood drills and infrastructure inspections have
 *   become memorial performance — form maintained for its own sake — while
 *   the operational competence they were meant to exercise and verify has
 *   atrophied. Under this reading, what presents itself as a Mountain (an
 *   unquestionable, necessary safety baseline) is structurally a Piton: a
 *   formerly functional coordination mechanism whose primary function has
 *   decayed into institutional theater, kept alive because agency leadership
 *   and municipal officials derive legitimacy and liability protection from
 *   the certification record itself, independent of whether the underlying
 *   capacity still works. The victims are the population living at flood
 *   risk, who bear the gap between certified and actual readiness, and the
 *   frontline responders whose on-the-ground findings are filtered out of the
 *   upward-reporting chain. Sibling readings of the same kernel —
 *   competence_reading (drills as live exercised knowledge) and
 *   hybrid_reading (some components competent, some ritualized) — are NOT
 *   this constraint; they are separate stories with their own ε values and
 *   structural data, linked here only through the kernel contest, per the
 *   ε-invariance principle.
 *
 * KEY AGENTS:
 *   - emergency_management_agency_leadership: institutional beneficiary and agenda_setter — collects legitimacy from certification, bears no cost when drills rehearse stale scenarios
 *   - municipal_officials: powerful beneficiary — uses certification record for liability insulation and political cover
 *   - floodplain_residents: powerless payer, trapped exit — bears the actual risk of the readiness gap
 *   - frontline_first_responders: moderate power payer, excluded voice — witnesses the gap directly but findings are not carried upward
 *   - state_auditors: institutional power, excluded from substantive review — audit scope checks occurrence, not function
 *   - flood_engineering_researchers: analytical observer — documents the certified-vs-actual divergence across jurisdictions post-hoc
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__husk_reading, 0.68).
domain_priors:suppression_score(preparedness_persistence__husk_reading, 0.58).
domain_priors:theater_ratio(preparedness_persistence__husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, theater_ratio, 0.81).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__husk_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__husk_reading, "Flood Preparedness Drill-and-Inspection Regime (Husk Reading)").
narrative_ontology:topic_domain(preparedness_persistence__husk_reading, "disaster_preparedness/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__husk_reading, 'c5c0e99e-caf7-4ffe-be89-d37a117b8318').
narrative_ontology:cs_kernel_codification('c5c0e99e-caf7-4ffe-be89-d37a117b8318', formalized).
narrative_ontology:cs_authority_grounding('c5c0e99e-caf7-4ffe-be89-d37a117b8318', practice).
narrative_ontology:cs_interpretation_layer_present('c5c0e99e-caf7-4ffe-be89-d37a117b8318').
narrative_ontology:cs_reading_relation('c5c0e99e-caf7-4ffe-be89-d37a117b8318', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5c0e99e-caf7-4ffe-be89-d37a117b8318', preparedness_persistence__hybrid_reading, influences).
narrative_ontology:cs_axiom('c5c0e99e-caf7-4ffe-be89-d37a117b8318', foundational, occurrence_certification_is_not_capacity_verification).
narrative_ontology:cs_axiom_status(occurrence_certification_is_not_capacity_verification, holdable).
narrative_ontology:cs_axiom_grounding('c5c0e99e-caf7-4ffe-be89-d37a117b8318', occurrence_certification_is_not_capacity_verification, empirically_contingent).
narrative_ontology:cs_axiom('c5c0e99e-caf7-4ffe-be89-d37a117b8318', secondary, administering_institution_captures_legitimacy_independent_of_function).
narrative_ontology:cs_axiom_status(administering_institution_captures_legitimacy_independent_of_function, holdable).
narrative_ontology:cs_axiom_grounding('c5c0e99e-caf7-4ffe-be89-d37a117b8318', administering_institution_captures_legitimacy_independent_of_function, empirically_contingent).
narrative_ontology:cs_reference_frame('c5c0e99e-caf7-4ffe-be89-d37a117b8318', post_disaster_reform_baseline).
narrative_ontology:cs_drift_state('c5c0e99e-caf7-4ffe-be89-d37a117b8318', contemporary_certification_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c5c0e99e-caf7-4ffe-be89-d37a117b8318', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__husk_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, emergency_management_agency_leadership).
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, municipal_officials).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, floodplain_residents).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, frontline_first_responders).
narrative_ontology:constraint_vindicates(preparedness_persistence__husk_reading, institutional_readiness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Schedules and certifies annual drills and facility inspections, reports completion rates upward to appropriators and to the public as evidence of readiness. Faces no penalty for a drill that rehearses an outdated flood map or a checklist inspection that never tests actual pump function under load. Career and budget renewal depend on the appearance of completed compliance, not on demonstrated capacity during an actual event.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, emergency_management_agency_leadership, agenda_setter,
    institutional, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__husk_reading, emergency_management_agency_leadership, beneficiary).

% Point to certified drill and inspection records when defending zoning approvals, insurance rate negotiations, and reelection campaigns. The paperwork trail insulates them from liability and public scrutiny regardless of whether the underlying evacuation routes or levee maintenance would function during an actual flood.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, municipal_officials, beneficiary,
    powerful, biographical, mobile, local).

% Attend or are told about scheduled drills, receive assurances that the levee system and evacuation plan are inspected and current. Bear the actual physical risk if the drilled procedures fail during a real flood because the drill rehearsed a scenario, route, or capacity that no longer matches ground conditions. Cannot verify agency claims independently and cannot afford to relocate out of the floodplain.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, floodplain_residents, payer,
    powerless, biographical, trapped, local).

% Execute the drills as scripted and report irregularities — equipment that doesn't operate, radios that don't reach the county network, routes now blocked by new construction — but see these findings routinely omitted from the certification summaries passed upward. Are the ones physically present when the gap between rehearsed competence and actual capacity surfaces during a real flood.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, frontline_first_responders, payer,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__husk_reading, frontline_first_responders, excluded).

% Would be positioned to compare drill completion records against post-incident performance data, but current audit scope only checks that a drill occurred on schedule, not whether it exercised current, functioning capacity. Their absence from substantive review is itself part of what allows the husk to persist uncontested.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, state_auditors, excluded,
    institutional, generational, analytical, regional).

% Study post-flood after-action reports across jurisdictions and document recurring gaps between certified readiness status and actual system performance, but their findings inform policy debate only slowly and carry no binding force on certification requirements.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, flood_engineering_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__husk_reading, emergency_management_agency_leadership).
narrative_ontology:fixing_cost_class(preparedness_persistence__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its original design, the drill-and-inspection regime coordinated a genuine collective-action problem: aligning evacuation routes, equipment maintenance, and inter-agency communication protocols across many actors who would otherwise drift out of sync between flood events.
% TRANSFER_FUNCTION: Moves the appearance of safety — certification records, public reassurance, legal insulation, budget justification — to agency leadership and municipal officials, while the actual residual risk of unexercised or outdated capacity is carried by floodplain residents and discovered under pressure by frontline responders.
% ABSENT_VOICES: Frontline responders' irregularity reports and state auditors' capacity-based review are structurally excluded from the certification pipeline, which measures occurrence of drills rather than demonstrated functional readiness; both would object that the metric being reported is not the metric that matters.
% DISAPPEARANCE_RATIONALE: Agency leadership and municipal officials would experience a rearrangement — the legitimacy shield the certification record provides would vanish, exposing them to direct liability and public scrutiny after any future flood. Floodplain residents would notice little immediate change, since the drills were not meaningfully improving their actual protection; the underlying levee and evacuation infrastructure would remain exactly as functional or dysfunctional as before, only now without the reassuring paperwork.
% FOUNDING_PROBLEM: Repeated flood disasters exposed uncoordinated evacuation, unmaintained levees, and unpracticed inter-agency communication; drills and inspections were built to force periodic, verifiable rehearsal of the full response chain so capacity would not decay silently between events.
% FOUNDING_PROBLEM_CORROBORATION: Flood engineering researchers, reviewing after-action reports from multiple jurisdictions, attest that certified-ready systems have repeatedly underperformed during actual floods in ways the certification process did not detect in advance. Frontline first responders, filing irregularity reports that are not carried into agency certification summaries, corroborate from inside the process that the rehearsed capacity and the actual capacity have diverged. No corroboration for continued live function comes from outside the agency leadership and municipal officials who benefit from the certification record.
narrative_ontology:disappearance_verdict(preparedness_persistence__husk_reading, contested).
narrative_ontology:founding_problem_status(preparedness_persistence__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__husk_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Theater ratio is authored high and rising (0.42 to 0.81) because this reading's core claim is that an increasing share of drill/inspection activity is performative — occurring on schedule, generating paperwork, but not exercising updated maps, current equipment function, or genuinely stress-tested coordination. Extractiveness is authored as substantial and rising (0.30 to 0.68) because the reading holds that the certification apparatus increasingly extracts legitimacy and budget continuity for agency leadership while transferring undetected risk onto residents — this is extraction through omission (what is NOT tested) rather than through overt coercion. Suppression is moderate and rising (0.35 to 0.58): it operates less through force and more through the structural exclusion of frontline reports and auditor capacity-checks from the certification pipeline, which forecloses the correction mechanism that would otherwise surface the husk. Accessibility collapse (0.62) reflects that once a jurisdiction accepts occurrence-based certification as sufficient evidence of readiness, the alternative — capacity-based verification — becomes institutionally invisible; resistance is authored low (0.35) because, under this reading, there is little organized pushback since the gap is not visible until an actual flood exposes it.
 *
 * PERSPECTIVAL GAP:
 *   From agency leadership's seat, the drill-and-inspection cycle looks like ongoing, necessary institutional diligence — a Rope, or even Mountain-adjacent baseline. From floodplain residents' seat, the same cycle is experienced as a hollow assurance that provides no verifiable protection and cannot be independently audited. This divergence is exactly what the engine is expected to compute from the structural data (power, exit, beneficiary/victim declarations) rather than from either party's self-report.
 *
 * DIRECTIONALITY LOGIC:
 *   Agency leadership sits nearest the beneficiary end: institutional power, arbitrage-grade exit (career mobility independent of local outcomes), direct capture of the legitimacy the certification produces. Municipal officials are similarly beneficiaries, with mobile exit options via reelection or relocation. Floodplain residents sit at the full-target end: powerless, trapped, and bearing the entire downside if drilled procedures fail in practice. Frontline responders are payers with constrained exit — they cannot refuse to run the drills, and their findings are structurally filtered out before reaching the certifying record, making them targets of the same suppression mechanism that protects the husk.
 *
 * MANDATROPHY ANALYSIS:
 *   The husk reading is precisely the mandatrophy detection case: the founding problem (uncoordinated, unpracticed flood response) is declared dead by this reading's own evidence trail — the certification apparatus persists and even intensifies (rising theater ratio) while the corroborating source (flood engineering researchers' after-action analysis and frontline responder irregularity reports) shows the mandate the arrangement was built to serve has not been re-verified as live, only re-certified as complete. Classifying this as Piton rather than accepting the surface Mountain framing prevents the classic false-summit error: treating an atrophied, inertially-maintained ritual as an irreducible natural necessity because it wears the vocabulary of unquestionable safety practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_vs_competence_reading_selection,
    'Is the drill-and-inspection regime actually a husk (atrophied, performative) or does it retain live exercised competence, as the competence_reading of this same kernel claims?',
    'Compare post-flood after-action performance data against pre-flood certification records across multiple jurisdictions and multiple flood events: if certified-ready systems reliably perform as drilled, the competence_reading is better supported; if certified systems reliably underperform relative to drilled assumptions, the husk_reading is better supported.',
    'If the competence_reading is empirically favored, this story''s classification (Piton) would not transfer to that reading — that reading''s own metrics would show low theater ratio and the arrangement would classify closer to Rope or Mountain from its own structural data. The two readings are separate constraints regardless of which turns out empirically stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_competence_reading_selection, conceptual, 'Kernel-level ambiguity: which reading of preparedness_persistence best fits the observable record.').

omega_variable(
    stratification_boundary,
    'Even within a jurisdiction, does the husk apply uniformly across all drill and inspection types, or does hybrid_reading''s stratification (competent engineering inspection, ritualized evacuation drills) better describe the actual mixture?',
    'Component-level audit distinguishing infrastructure inspection (levee, pump, structural) outcomes from procedural drill (evacuation, communication) outcomes, tracked separately over multiple cycles.',
    'If stratification holds, this husk_reading story''s uniform-atrophy framing overstates the decay for engineering components specifically — the hybrid_reading would be the more structurally accurate constraint for those components, while husk_reading would remain accurate for the procedural/evacuation components.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_boundary, empirical, 'Whether atrophy is uniform across preparedness components or concentrated in specific ones.').

omega_variable(
    legitimacy_beneficiary_naturalness,
    'Is institutional legitimacy from certification a natural incentive of any bureaucratic safety function (unavoidable, present even in genuinely functional systems), or is its capture by agency leadership in this case a constructed extraction specific to this arrangement''s design?',
    'Comparative study of preparedness regimes with independent, adversarial audit mechanisms (capacity-verification separate from occurrence-certification) versus those without, to see whether the legitimacy-capture pattern is structural to all such regimes or specific to weak-audit designs.',
    'If legitimacy capture is unavoidable in any certification-based safety regime, the husk pattern may be closer to a structural feature of bureaucratic safety coordination generally rather than a locally fixable extraction — softening the Piton classification toward Tangled Rope (genuine coordination plus unavoidable extraction) rather than pure inertial atrophy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_beneficiary_naturalness, conceptual, 'Whether legitimacy-capture by administering institutions is generic to safety certification regimes or specific to this arrangement''s weak audit design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__husk_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__husk_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(prep_tr_t4, preparedness_persistence__husk_reading, theater_ratio, 4, 0.51).
narrative_ontology:measurement(prep_tr_t8, preparedness_persistence__husk_reading, theater_ratio, 8, 0.6).
narrative_ontology:measurement(prep_tr_t12, preparedness_persistence__husk_reading, theater_ratio, 12, 0.68).
narrative_ontology:measurement(prep_tr_t16, preparedness_persistence__husk_reading, theater_ratio, 16, 0.74).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__husk_reading, theater_ratio, 20, 0.78).
narrative_ontology:measurement(prep_tr_t24, preparedness_persistence__husk_reading, theater_ratio, 24, 0.81).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__husk_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(prep_be_t4, preparedness_persistence__husk_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(prep_be_t8, preparedness_persistence__husk_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(prep_be_t12, preparedness_persistence__husk_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(prep_be_t16, preparedness_persistence__husk_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__husk_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(prep_be_t24, preparedness_persistence__husk_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__husk_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(prep_su_t4, preparedness_persistence__husk_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(prep_su_t8, preparedness_persistence__husk_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(prep_su_t12, preparedness_persistence__husk_reading, suppression_requirement, 12, 0.49).
narrative_ontology:measurement(prep_su_t16, preparedness_persistence__husk_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(prep_su_t20, preparedness_persistence__husk_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(prep_su_t24, preparedness_persistence__husk_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the preparedness_persistence kernel. competence_reading claims live exercised competence (lower ε, Rope/Mountain-adjacent); husk_reading (this story) claims atrophied performative form (higher ε, Piton); hybrid_reading claims a stratified mixture across components. Each reading authors its own ε and structural data independently per the ε-invariance principle; they are linked here as siblings in a kernel contest, not merged into one measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
