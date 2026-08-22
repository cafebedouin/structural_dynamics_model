% ============================================================================
% CONSTRAINT STORY: preparedness_retention__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__husk_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: preparedness_retention__husk_reading
 *   human_readable: Flood Preparedness as Memorial Performance (Husk Reading)
 *   domain: governance/disaster_preparedness
 *
 * SUMMARY:
 *   This story instantiates the husk reading of the preparedness_retention
 *   kernel: drills and inspections in a flood-response regime are read as
 *   memorial performance, ceremonies that produce the artifacts of readiness
 *   (checklists, certifications, photographs of coordinated response) while
 *   the tacit, improvisational competence needed to handle an actual
 *   D5-magnitude event has atrophied beneath the ceremony. The claimed type
 *   is piton — the constraint persists mainly through inertia and theatrical
 *   maintenance rather than active suppression of alternatives, and no single
 *   party profits enough to be a snare's concentrated beneficiary; the flood
 *   authorities and certifiers gain legitimacy and revenue but the extraction
 *   is diffuse relative to the concentrated, catastrophic cost borne by
 *   residents only in the tail event. This is deliberately ONE reading among
 *   three (husk, competence, hybrid) sharing the same underlying
 *   drill-and-inspection kernel; the competence_reading and hybrid_reading
 *   are separate constraint files with their own ε and stakeholder structure,
 *   not alternative interpretations folded into this one.
 *
 * KEY AGENTS:
 *   - regional_flood_authorities: agenda_setter/beneficiary (institutional/analytical) — administers the ceremony, collects legitimacy
 *   - certification_bodies: beneficiary (organized/arbitrage) — sells the certification product
 *   - frontline_emergency_responders: payer (moderate/constrained) — enacts scripted drills, skills atrophy
 *   - downstream_residents: payer (powerless/trapped) — bears the tail-event cost of the capacity gap
 *   - independent_disaster_researchers: observer (analytical) — documents the drill/outcome mismatch
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__husk_reading, 0.68).
domain_priors:suppression_score(preparedness_retention__husk_reading, 0.42).
domain_priors:theater_ratio(preparedness_retention__husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, theater_ratio, 0.81).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__husk_reading, piton).
narrative_ontology:human_readable(preparedness_retention__husk_reading, "Flood Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_retention__husk_reading, "governance/disaster_preparedness").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__husk_reading, '07a68975-ddc0-4dc2-a0de-63f58a2fd5d5').
narrative_ontology:cs_kernel_codification('07a68975-ddc0-4dc2-a0de-63f58a2fd5d5', implicit).
narrative_ontology:cs_authority_grounding('07a68975-ddc0-4dc2-a0de-63f58a2fd5d5', practice).
narrative_ontology:cs_interpretation_layer_present('07a68975-ddc0-4dc2-a0de-63f58a2fd5d5').
narrative_ontology:cs_reading_relation('07a68975-ddc0-4dc2-a0de-63f58a2fd5d5', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('07a68975-ddc0-4dc2-a0de-63f58a2fd5d5', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('07a68975-ddc0-4dc2-a0de-63f58a2fd5d5', foundational, ceremonial_activity_decouples_from_operational_capacity).
narrative_ontology:cs_axiom_status(ceremonial_activity_decouples_from_operational_capacity, holdable).
narrative_ontology:cs_axiom_grounding('07a68975-ddc0-4dc2-a0de-63f58a2fd5d5', ceremonial_activity_decouples_from_operational_capacity, empirically_contingent).
narrative_ontology:cs_axiom('07a68975-ddc0-4dc2-a0de-63f58a2fd5d5', secondary, compliance_legibility_is_the_true_optimized_target).
narrative_ontology:cs_axiom_status(compliance_legibility_is_the_true_optimized_target, holdable).
narrative_ontology:cs_axiom_grounding('07a68975-ddc0-4dc2-a0de-63f58a2fd5d5', compliance_legibility_is_the_true_optimized_target, empirically_contingent).
narrative_ontology:cs_reference_frame('07a68975-ddc0-4dc2-a0de-63f58a2fd5d5', post_disaster_reform_mandate).
narrative_ontology:cs_drift_state('07a68975-ddc0-4dc2-a0de-63f58a2fd5d5', contemporary_certification_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('07a68975-ddc0-4dc2-a0de-63f58a2fd5d5', '').
narrative_ontology:cs_kernel_id(preparedness_retention__husk_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, regional_flood_authorities).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, certification_bodies).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, downstream_residents).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, frontline_emergency_responders).
narrative_ontology:constraint_vindicates(preparedness_retention__husk_reading, institutional_readiness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Schedule and administer the annual drill calendar and inspection regime, publish compliance reports, and hold the authority to redesign the whole program. They collect legitimacy, budget continuity, and insurance-rate benefits from a clean compliance record, and they could redirect resources toward tacit-skill retention but the cost of dismantling the ceremonial apparatus (union relations, political optics, sunk audit infrastructure) exceeds what they currently bear.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, regional_flood_authorities, agenda_setter,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__husk_reading, regional_flood_authorities, beneficiary).

% Issue the checklists, run the audits, and certify facilities as 'prepared.' Their revenue and institutional standing depend on the drill-and-inspection cycle continuing to exist as a certifiable event, regardless of whether it produces retained operational skill; they can move between clients and jurisdictions if any single relationship sours.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, certification_bodies, beneficiary,
    organized, biographical, arbitrage, national).

% Attend the drills as scripted participants, following pre-distributed scenario scripts rather than exercising live decision-making under uncertainty. Between drills their actual skills atrophy because the drills are choreographed to avoid failure states that would generate the compliance record's blemishes. They cannot easily refuse participation without professional consequence, and the drill format itself is set above their heads.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, frontline_emergency_responders, payer,
    moderate, biographical, constrained, regional).

% Live in the flood-exposed zones the preparedness regime claims to protect. They have no visibility into whether the certified drills translate into real response capacity and no practical way to relocate or independently verify readiness; their exposure is realized only when a D5-magnitude event actually occurs and the gap between certified and actual capacity becomes lethal.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, downstream_residents, payer,
    powerless, biographical, trapped, local).

% A non-agent placeholder for the low-probability, high-consequence event (D5 magnitude) whose occurrence is the only mechanism that would falsify the certification record. It has no voice in the current planning process; the entire ceremonial apparatus is validated only against past drills, never against the actual future event it claims to prepare for.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, future_flood_event, excluded,
    analytical, generational, analytical, regional).
narrative_ontology:stakeholder_non_agent(preparedness_retention__husk_reading, future_flood_event).

% Study post-event after-action reports and compare drill scripts to actual incident response gaps. They have no operational authority but publish findings that occasionally surface the ceremony/competence gap, findings the certifying bodies and flood authorities have incentive to minimize or reframe as isolated failures rather than systemic pattern.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, independent_disaster_researchers, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_retention__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under the husk reading, the drill-and-inspection cycle still nominally coordinates a shared calendar, a common vocabulary of readiness, and a legible signal to insurers, legislators, and the public that some standard is being met — but this reading holds that the coordination has decoupled from the underlying skill it was meant to certify.
% TRANSFER_FUNCTION: Moves budget, professional attention, and political credit toward the production of compliance artifacts (signed checklists, drill photographs, certification stamps) and away from the harder, less legible work of maintaining live improvisational competence under realistic failure conditions; the resulting shortfall in response capacity is transferred, unpriced, onto downstream residents at the moment of an actual severe event.
% ABSENT_VOICES: Frontline responders who know the drills are scripted rarely say so in the compliance record itself, since doing so would flag their own certification as unreliable; downstream residents have no forum in which to demand evidence that certified preparedness would hold under real conditions; the future D5 event, by definition, cannot testify until it is too late to matter for planning.
% DISAPPEARANCE_RATIONALE: The flood authorities and certification bodies would say the world rearranges catastrophically without the drill regime — insurance markets, legislative oversight, and public confidence all reference it. The husk reading holds that what would actually rearrange is only the ceremonial layer: the underlying response capacity, already largely absent, would not visibly change until the next real event, at which point the absence becomes evident either way. The dispute is exactly the reading conflict this kernel names.
% FOUNDING_PROBLEM: Repeated flood disasters in which emergency response was ad hoc, uncoordinated, and lacked any shared operational picture, leading to preventable casualties and property loss; the drill-and-inspection regime was built to ensure responders could execute a coordinated, rehearsed response under pressure.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster researchers and several post-event after-action reviews (compiled by academic flood-risk centers outside the certifying chain) attest that scripted drills systematically fail to predict actual incident performance, and that certification pass rates have not correlated with post-event outcome quality. The flood authorities and certification bodies, who administer and profit from the compliance record, attest the founding problem remains live and adequately addressed by the current regime — but this is precisely the self-interested attestation the corroboration check is designed to flag.
narrative_ontology:disappearance_verdict(preparedness_retention__husk_reading, contested).
narrative_ontology:founding_problem_status(preparedness_retention__husk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__husk_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is authored moderate-high because the transfer is real but partially masked — resources flow to compliance production rather than a visible embezzlement, so it reads as institutional drift rather than overt theft. Theater ratio is high and rising (0.52 to 0.81) because this is precisely the metric substitution signature the husk reading claims: proxy compliance activity has been substituting for the harder-to-measure retained competence over the interval. Suppression is moderate (0.42) — there is no active coercion preventing alternative approaches, but professional and budgetary incentives create real friction against reform. Accessibility collapse is low-moderate (0.35) because, unlike a genuine mountain, better preparedness models are visibly available (documented in the after-action literature) — they are simply not adopted, which is a piton signature, not a mountain one.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (regional_flood_authorities), the drill regime looks like functioning, well-administered coordination — compliance is high, audits pass, legitimacy accrues. From the payer seat (downstream_residents, frontline_responders), the same structure is experienced as an unverified promise whose failure mode is invisible until catastrophic. The engine should compute these as structurally different seat classifications from the same base data — that divergence is the point of the husk reading, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Regional flood authorities and certification bodies are declared beneficiaries because the ceremonial apparatus produces legitimacy and revenue for them directly and continuously, independent of actual event outcomes — low d, near-beneficiary end. Downstream residents and frontline responders are declared victims/payers: residents bear an unpriced tail risk they cannot detect or exit (trapped, high d); responders bear atrophied competence and professional exposure without control over drill design (constrained, elevated d). No override is used — the derivation from beneficiary/victim plus exit options already captures the asymmetry cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as piton (rather than snare) prevents mislabeling this as pure predatory extraction: no party is siphoning off dedicated flood-preparedness budgets for personal enrichment, and the coordination function (a shared readiness calendar, a legible standard) was genuinely useful when founded. What has happened, on this reading, is that the founding problem's status has gone contested-to-dead while the apparatus that solved it has kept running on institutional momentum — the founding_problem/disappearance_verdict mismatch (contested status, contested verdict, but leaning toward 'ceremony persists past function') is exactly the piton signature the R5 genealogy check is designed to surface, distinguishing degraded-but-real coordination from a snare with an identifiable predator.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ceremony_competence_decoupling_test,
    'Has the drill-and-inspection regime actually decoupled from live response competence, or does the husk reading mistake surface theatricality for a deeper decoupling that has not occurred?',
    'Compare post-event after-action performance metrics (response time, coordination failures, casualty rates) against pre-event certification scores across multiple D3-D4 magnitude events; a strong correlation would support the competence_reading, a weak or inverse correlation would support the husk_reading.',
    'If decoupling is confirmed, the piton classification and the diffuse-victim structure (downstream residents bearing unpriced tail risk) hold. If competence is in fact retained despite ceremonial appearance, this story''s ε and beneficiary structure would need to be revised toward the competence_reading''s lower extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremony_competence_decoupling_test, empirical, 'Whether ceremonial appearance genuinely tracks a competence deficit or is a surface feature over intact capacity.').

omega_variable(
    kernel_framing_locus_of_disagreement,
    'Where exactly do the three sibling readings (husk, competence, hybrid) disagree — is it about the SAME drills (differing only in interpretation of identical evidence) or about DIFFERENT institutional layers (the hybrid reading''s claim that competence is stratified by institution type)?',
    'Map which specific institutions (technical water-management bodies vs. general emergency-response agencies) each reading''s evidence base draws from; if the readings are drawing on disjoint institutional samples, the disagreement is partly definitional rather than purely evidentiary.',
    'If the readings are sampling different institutional layers, the hybrid_reading may be the most descriptively accurate composite, and this husk_reading''s ε (0.68) may only be locally true of the general/ceremonial layer rather than the whole preparedness system — this bears on how the network edges to sibling constraints should be weighted in downstream contamination analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_locus_of_disagreement, conceptual, 'Whether the kernel''s three readings disagree on evidence or on institutional scope.').

omega_variable(
    false_legitimacy_beneficiary_naturalization_risk,
    'Is the ''institutional legitimacy'' benefit to flood authorities and certification bodies a genuine, freely chosen governance choice, or is it functioning as a naturalized cover — i.e., does the system present the drill regime as simply ''what responsible preparedness looks like'' in a way that forecloses scrutiny of the ceremony/competence gap?',
    'Examine whether legislative or insurance-regulatory bodies treat certification pass rates as dispositive evidence of readiness (naturalized) or as one input among several independently verified competence measures (non-naturalized).',
    'If naturalized, the constraint''s effective suppression is higher than the authored 0.42 — the legitimacy-conferring function actively forecloses the kind of scrutiny that would surface the gap, which would push this reading closer to snare-adjacent territory rather than pure piton drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_legitimacy_beneficiary_naturalization_risk, conceptual, 'Whether institutional legitimacy accrual functions as passive byproduct or active scrutiny-foreclosure mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__husk_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__husk_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(prep_tr_t4, preparedness_retention__husk_reading, theater_ratio, 4, 0.6).
narrative_ontology:measurement(prep_tr_t8, preparedness_retention__husk_reading, theater_ratio, 8, 0.66).
narrative_ontology:measurement(prep_tr_t12, preparedness_retention__husk_reading, theater_ratio, 12, 0.71).
narrative_ontology:measurement(prep_tr_t16, preparedness_retention__husk_reading, theater_ratio, 16, 0.75).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__husk_reading, theater_ratio, 20, 0.78).
narrative_ontology:measurement(prep_tr_t24, preparedness_retention__husk_reading, theater_ratio, 24, 0.81).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__husk_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(prep_be_t4, preparedness_retention__husk_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(prep_be_t8, preparedness_retention__husk_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(prep_be_t12, preparedness_retention__husk_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(prep_be_t16, preparedness_retention__husk_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__husk_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(prep_be_t24, preparedness_retention__husk_reading, base_extractiveness, 24, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_retention__husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__husk_reading, 0.1).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the preparedness_retention kernel. competence_reading claims low ε (drills genuinely preserve capacity); hybrid_reading claims stratified ε (low for specialized technical institutions, high for general societal/ceremonial layers); this husk_reading claims high ε uniformly (ceremony has decoupled from competence system-wide). Each reading has its own stakeholder set, beneficiary/victim structure, and classification; they are linked here for contamination-propagation analysis, not averaged into one value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
