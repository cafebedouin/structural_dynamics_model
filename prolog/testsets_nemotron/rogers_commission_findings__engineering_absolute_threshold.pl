% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__engineering_absolute_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__engineering_absolute_threshold, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: rogers_commission_findings__engineering_absolute_threshold
 *   human_readable: Rogers Engineering Absolute Threshold: O-Ring Redesign Required Before Flight
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   The Rogers Commission findings on the Challenger disaster established
 *   that O-ring failure at low temperatures was a known, documented physical
 *   phenomenon. This reading interprets the findings as establishing an
 *   absolute engineering threshold: flight operations must cease until the
 *   O-ring is redesigned and certified for the operational temperature
 *   envelope. The constraint is not a management decision but a recognition
 *   of physical law — rubber does not seal below its glass transition
 *   temperature regardless of organizational pressure. Engineers hold veto
 *   authority over Flight Readiness Reviews because they are the designated
 *   interpreters of physical reality for the program. The beneficiary is
 *   flight crew safety (and by extension, public trust in human spaceflight);
 *   the victim is launch cadence (schedule pressure, program momentum,
 *   political commitments).
 *
 * KEY AGENTS:
 *   - flight_crew: Primary beneficiary (powerless/identity_locked) — lives depend on the threshold holding
 *   - launch_cadence: Primary victim (institutional/trapped) — schedule pressure bears the cost of the redesign pause
 *   - flight_readiness_review_engineers: Agenda setter (organized/constrained) — hold veto authority as designated interpreters of physical law
 *   - nasa_management: Secondary actor (institutional/constrained) — receives the constraint as external limit on authority
 *   - contractor_management_morton_thiokol: Secondary actor (organized/constrained) — bears redesign cost and certification burden
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__engineering_absolute_threshold, 0.12).
domain_priors:suppression_score(rogers_commission_findings__engineering_absolute_threshold, 0.91).
domain_priors:theater_ratio(rogers_commission_findings__engineering_absolute_threshold, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, extractiveness, 0.12).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, resistance, 0.07).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__engineering_absolute_threshold, mountain).
narrative_ontology:human_readable(rogers_commission_findings__engineering_absolute_threshold, "Rogers Engineering Absolute Threshold: O-Ring Redesign Required Before Flight").
narrative_ontology:topic_domain(rogers_commission_findings__engineering_absolute_threshold, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__engineering_absolute_threshold).
domain_priors:emerges_naturally(rogers_commission_findings__engineering_absolute_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__engineering_absolute_threshold, '084c6968-fe77-4894-bc9d-ffbd2c6d37a2').
narrative_ontology:cs_kernel_codification('084c6968-fe77-4894-bc9d-ffbd2c6d37a2', formalized).
narrative_ontology:cs_authority_grounding('084c6968-fe77-4894-bc9d-ffbd2c6d37a2', expertise).
narrative_ontology:cs_interpretation_layer_present('084c6968-fe77-4894-bc9d-ffbd2c6d37a2').
narrative_ontology:cs_reading_relation('084c6968-fe77-4894-bc9d-ffbd2c6d37a2', rogers_commission_findings__management_compliance_narrative, forecloses).
narrative_ontology:cs_reading_relation('084c6968-fe77-4894-bc9d-ffbd2c6d37a2', rogers_commission_findings__actuarial_risk_acceptance, forecloses).
narrative_ontology:cs_axiom('084c6968-fe77-4894-bc9d-ffbd2c6d37a2', foundational, physical_law_is_not_negotiable).
narrative_ontology:cs_axiom_status(physical_law_is_not_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('084c6968-fe77-4894-bc9d-ffbd2c6d37a2', physical_law_is_not_negotiable, empirically_contingent).
narrative_ontology:cs_axiom('084c6968-fe77-4894-bc9d-ffbd2c6d37a2', foundational, engineering_judgment_supersedes_management_authority_in_flight_readiness).
narrative_ontology:cs_axiom_status(engineering_judgment_supersedes_management_authority_in_flight_readiness, holdable).
narrative_ontology:cs_axiom_grounding('084c6968-fe77-4894-bc9d-ffbd2c6d37a2', engineering_judgment_supersedes_management_authority_in_flight_readiness, deontological).
narrative_ontology:cs_reference_frame('084c6968-fe77-4894-bc9d-ffbd2c6d37a2', rogers_commission_physical_law_recognition).
narrative_ontology:cs_drift_state('084c6968-fe77-4894-bc9d-ffbd2c6d37a2', post_columbia_institutionalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('084c6968-fe77-4894-bc9d-ffbd2c6d37a2', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, flight_crew).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, launch_cadence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, nasa_management).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, nasa_management).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, contractor_management_morton_thiokol).
narrative_ontology:constraint_vindicates(rogers_commission_findings__engineering_absolute_threshold, physical_law_is_not_negotiable).
narrative_ontology:constraint_vindicates(rogers_commission_findings__engineering_absolute_threshold, engineering_judgment_supersedes_management_authority_in_flight_readiness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Astronauts whose lives depend on the O-ring sealing at launch temperature. They cannot exit the constraint — their professional identity and survival are fused with the vehicle's integrity. They benefit from the absolute threshold because it prevents launch when physics says the seals will fail. They do not administer the constraint; they are its protected subjects.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, flight_crew, beneficiary,
    powerless, biographical, identity_locked, global).

% The program's launch schedule, political commitments, and institutional momentum. Bears the full cost of the redesign pause (32 months for Challenger return-to-flight). Cannot exit the constraint because it is a physical boundary — no amount of schedule pressure changes the glass transition temperature of the O-ring material. The victim is not a person but an institutional imperative that the constraint suppresses.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, launch_cadence, payer,
    institutional, generational, trapped, global).

% Engineers designated as the final technical authority in the Flight Readiness Review process. They hold veto power: if they do not sign, the vehicle does not fly. Their authority derives from expertise (they interpret physical law for the program), not hierarchy. They are constrained by professional ethics and the knowledge that a wrong sign-off kills crew. Their exit options are constrained — leaving the FRR means abandoning the protective function they embody.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, flight_readiness_review_engineers, agenda_setter,
    organized, biographical, constrained, national).

% NASA leadership that receives the constraint as an external limit on launch authority. They pay in schedule autonomy and political capital (explaining delays to Congress and the White House). They benefit secondarily: the constraint provides a defensible boundary against schedule pressure ('we cannot fly, physics says no'). Their exit is constrained — they could attempt to override the FRR (as occurred pre-Challenger) but the constraint's suppression mechanism (post-Challenger institutionalization) makes override structurally costly.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, nasa_management, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__engineering_absolute_threshold, nasa_management, beneficiary).

% Morton Thiokol management and engineering. Bears the redesign cost, certification burden, and contractual liability. Their engineers (notably Roger Boisjoly and Arnie Thompson) were the internal voices that the constraint vindicated. Exit is constrained — they hold the SRB contract and must deliver a certified redesign to continue; walking away means contract termination and reputational destruction.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, contractor_management_morton_thiokol, payer,
    organized, biographical, constrained, national).

% The analytical seat that sees the full structure: the constraint is a recognition of physical law (rubber does not seal below Tg) institutionalized as a veto authority. From this seat, the constraint is a mountain — it would persist regardless of who defends it because the physics is invariant. The observer sees the beneficiary/victim structure as a consequence of the physics, not its cause.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__engineering_absolute_threshold, diffuse).
narrative_ontology:fixing_cost_class(rogers_commission_findings__engineering_absolute_threshold, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the recognition of physical law across the flight program: ensures that launch decisions are gated by engineering judgment about material behavior at temperature, not by schedule pressure or management preference. Solves the coordination problem of 'who decides when physics says stop' by designating FRR engineers as the authoritative interpreters.
% TRANSFER_FUNCTION: Moves launch authority from management discretion to engineering veto. The 'transfer' is not of resources but of decision-rights: the right to say 'fly' is transferred from program management to the engineers who certify the hardware. The cost (schedule delay) is borne by the program; the benefit (crew survival) accrues to the crew and the public trust.
% ABSENT_VOICES: The pre-Challenger management chain that overrode engineering concerns (Lawrence Mulloy, George Hardy) — they would argue that risk acceptance is a management prerogative, not an engineering veto. They are absent because the constraint (post-Challenger institutionalization) structurally excluded their authority. Also absent: the crew of STS-51L, who had no voice in the launch decision that killed them — their absence is what the constraint was built to prevent recurring.
% DISAPPEARANCE_RATIONALE: If the engineering absolute threshold vanished overnight, launch decisions would revert to management discretion with engineering input as advisory only. The FRR veto would become consultative. Schedule pressure would again compete directly with technical concerns without a structural boundary. The physical law (O-ring temperature sensitivity) would not change, but the institutional mechanism that forces it to be respected would disappear — leading to a world where launch-on-schedule competes with launch-on-physics on equal footing.
% FOUNDING_PROBLEM: The Shuttle program had normalized deviance: O-ring erosion at low temperatures was observed repeatedly and treated as an acceptable risk rather than a design defect. Management overrode engineering objections (STS-51L FRR) because the risk was framed as probabilistic and manageable. The founding problem was the absence of a structural mechanism that made physical law non-negotiable in the launch decision chain.
% FOUNDING_PROBLEM_CORROBORATION: The Rogers Commission itself (chaired by William Rogers, with Feynman's minority report) attested that the founding problem was the normalization of deviance and the subordination of engineering to management. The Columbia Accident Investigation Board (2003) corroborated independently that the same organizational pattern (normalization of deviance, schedule pressure overriding technical concerns) recurred — confirming the founding problem remains live across generations of NASA human spaceflight. This corroboration comes from outside the benefiting parties (the CAIB was an external investigation, not a NASA self-assessment).
narrative_ontology:disappearance_verdict(rogers_commission_findings__engineering_absolute_threshold, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__engineering_absolute_threshold, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__engineering_absolute_threshold, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(rogers_commission_findings__engineering_absolute_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__engineering_absolute_threshold, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__engineering_absolute_threshold_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, ExtMetricName, E),
    domain_priors:suppression_score(rogers_commission_findings__engineering_absolute_threshold, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(rogers_commission_findings__engineering_absolute_threshold),
    narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(rogers_commission_findings__engineering_absolute_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint primarily prevents catastrophe rather than extracting resources — the 'cost' to launch cadence is the price of not killing crew. Suppression is very high (0.91) because the constraint actively suppresses launch operations until a physical condition is met; alternatives (waivers, risk acceptance, management override) are structurally excluded. Theater ratio is very low (0.08) because the engineering review function is genuine and the veto is real — Challenger proved the cost of theater. Accessibility collapse is near-total (0.93): once the physics is understood, no alternative to redesign exists. Resistance is near-zero (0.07): the constraint meets almost no active resistance because the physics is not negotiable; resistance would be irrational.
 *
 * DIRECTIONALITY LOGIC:
 *   Flight crew are full beneficiaries (d ≈ 0.0): the constraint subsidizes their survival. Launch cadence is the full target (d ≈ 1.0): it bears the entire schedule cost. Engineers at the FRR are symmetric (d ≈ 0.5): they hold authority but also bear responsibility — if they err on either side (false positive: unnecessary delay; false negative: catastrophe), they own the consequence. NASA management is near-target (d ≈ 0.8): they lose schedule control but gain a defensible boundary. The directionality derivation from beneficiary/victim declarations + power + exit captures this structure: flight_crew (beneficiary, powerless, identity_locked) → low d; launch_cadence (victim, institutional, trapped) → high d; engineers (neither beneficiary nor victim, organized, constrained) → mid d.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling coordination as extraction by making the coordination function (preventing O-ring failure) structurally inseparable from the suppression mechanism (halting flight until physics is satisfied). A pure extraction reading would predict the constraint relaxes when political pressure mounts; the mountain reading predicts it does not — and Challenger validated the mountain reading. The mandate (prevent O-ring failure) has not atrophied; the physics remains the same.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_institutional_construct,
    'Is the engineering absolute threshold a discovery of physical law (temperature-dependent O-ring resiliency) or an institutional construct that benefits identifiable agents by freezing the design?',
    'Counterfactual analysis: if NASA had accepted the risk with crew consent and full disclosure, would the physics have changed? If not, the threshold is institutional; if the physics would have killed crew regardless, the threshold reflects natural law.',
    'If institutional, false_summit_mountain triggers (beneficiaries: flight_crew + NASA institutional reputation; victim: launch_cadence) and reclassifies to tangled_rope. If natural law, mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_construct, conceptual, 'Whether the engineering absolute threshold reflects immutable physics or a constructed safety standard').

omega_variable(
    committer_frame_reading_dispute,
    'This constraint is one reading (engineering_absolute_threshold) of the contested kernel rogers_commission_findings. Sibling readings (management_compliance_narrative, actuarial_risk_acceptance) would change the beneficiary/victim structure and suppression profile. Where exactly does the structural disagreement lie?',
    'Map the three readings'' structural parameters: engineering_absolute_threshold (suppression=0.91, victims={launch_cadence}, beneficiaries={flight_crew}); management_compliance_narrative (suppression≈0.3, victims={}, beneficiaries={NASA_management, contractor_management}); actuarial_risk_acceptance (suppression≈0.1, victims={flight_crew_if_consent_flawed}, beneficiaries={program_continuity}). The disagreement is located in the authority_grounding of the Flight Readiness Review veto.',
    'If engineering_absolute_threshold forecloses management_compliance_narrative within a single framework (forecloses relation), the kernel has an unresolvable structural split. If they coexist_with, the kernel sustains multiple live institutional positions simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_reading_dispute, conceptual, 'Structural location of disagreement among sibling readings of the Rogers findings kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__engineering_absolute_threshold, 1986, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t1986, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 1986, 0.05).
narrative_ontology:measurement(roge_tr_t1996, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 1996, 0.06).
narrative_ontology:measurement(roge_tr_t2006, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 2006, 0.07).
narrative_ontology:measurement(roge_tr_t2016, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 2016, 0.07).
narrative_ontology:measurement(roge_tr_t2026, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 2026, 0.08).

% Extraction over time
narrative_ontology:measurement(roge_be_t1986, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 1986, 0.08).
narrative_ontology:measurement(roge_be_t1996, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 1996, 0.1).
narrative_ontology:measurement(roge_be_t2006, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 2006, 0.11).
narrative_ontology:measurement(roge_be_t2016, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 2016, 0.11).
narrative_ontology:measurement(roge_be_t2026, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 2026, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t1986, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 1986, 0.94).
narrative_ontology:measurement(roge_su_t1996, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 1996, 0.92).
narrative_ontology:measurement(roge_su_t2006, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 2006, 0.91).
narrative_ontology:measurement(roge_su_t2016, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 2016, 0.91).
narrative_ontology:measurement(roge_su_t2026, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 2026, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__engineering_absolute_threshold, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rogers_commission_findings__engineering_absolute_threshold, 0.08).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__management_compliance_narrative).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__actuarial_risk_acceptance).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, flight_readiness_review_veto_authority).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, o_ring_redesign_certification_requirement).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, challenger_lesson_institutionalization).

% DUAL FORMULATION NOTE:
% This constraint (engineering_absolute_threshold) and its siblings (management_compliance_narrative, actuarial_risk_acceptance) form a kernel family around rogers_commission_findings. They share the same empirical referent (the Rogers Commission report) but instantiate different constraints with different ε values, different beneficiary/victim structures, and different suppression profiles. engineering_absolute_threshold: ε≈0.12, suppression≈0.91, beneficiaries={flight_crew}, victims={launch_cadence}. management_compliance_narrative: ε≈0.35, suppression≈0.30, beneficiaries={NASA_management, contractor_management}, victims={}. actuarial_risk_acceptance: ε≈0.25, suppression≈0.10, beneficiaries={program_continuity}, victims={flight_crew_if_consent_flawed}. The ε-invariance principle requires separate stories because the extraction profile changes with the reading — this is not one constraint viewed from three angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
