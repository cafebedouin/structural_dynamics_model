% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__military_custodian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__military_custodian_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__military_custodian_reading
 *   human_readable: July Charter — Military Custodianship Reading
 *   domain: constitutional_law/political_transitions
 *
 * SUMMARY:
 *   This story instantiates the military-custodian reading of the contested
 *   July Charter kernel: the founding constitutional settlement is read as
 *   ratifying the armed forces as a permanent institutional guardian with
 *   veto authority over civilian governance, justified by post-revolutionary
 *   stability requirements. Under this reading, civilian institutions —
 *   political parties, the press, provincial administration, the
 *   constitutional court's own jurisdiction — are structurally subordinated
 *   to a security apparatus that determines what counts as a stability
 *   threat. This is ONE of three readings of the same Charter text; the
 *   secular_democratic_reading and guided_nationalism_reading are separate
 *   constraints with their own ε and stakeholder structures, not alternative
 *   measurements of this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, 0.78).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__military_custodian_reading, 0.82).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "July Charter — Military Custodianship Reading").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "constitutional_law/political_transitions").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, 'e5b4e2b5-58b7-481c-8d3e-fba6f66ff480').
narrative_ontology:cs_kernel_codification('e5b4e2b5-58b7-481c-8d3e-fba6f66ff480', formalized).
narrative_ontology:cs_authority_grounding('e5b4e2b5-58b7-481c-8d3e-fba6f66ff480', extraction).
narrative_ontology:cs_interpretation_layer_present('e5b4e2b5-58b7-481c-8d3e-fba6f66ff480').
narrative_ontology:cs_reading_relation('e5b4e2b5-58b7-481c-8d3e-fba6f66ff480', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('e5b4e2b5-58b7-481c-8d3e-fba6f66ff480', july_charter_sovereign_legitimacy__guided_nationalism_reading, coexists_with).
narrative_ontology:cs_axiom('e5b4e2b5-58b7-481c-8d3e-fba6f66ff480', foundational, military_institutional_continuity_is_sovereign_precondition).
narrative_ontology:cs_axiom_status(military_institutional_continuity_is_sovereign_precondition, holdable).
narrative_ontology:cs_axiom_grounding('e5b4e2b5-58b7-481c-8d3e-fba6f66ff480', military_institutional_continuity_is_sovereign_precondition, instrumental).
narrative_ontology:cs_axiom('e5b4e2b5-58b7-481c-8d3e-fba6f66ff480', foundational, civilian_political_contestation_subordinate_to_stability_determination).
narrative_ontology:cs_axiom_status(civilian_political_contestation_subordinate_to_stability_determination, holdable).
narrative_ontology:cs_axiom_grounding('e5b4e2b5-58b7-481c-8d3e-fba6f66ff480', civilian_political_contestation_subordinate_to_stability_determination, conventional).
narrative_ontology:cs_reference_frame('e5b4e2b5-58b7-481c-8d3e-fba6f66ff480', military_coercive_monopoly_as_transitional_necessity).
narrative_ontology:cs_drift_state('e5b4e2b5-58b7-481c-8d3e-fba6f66ff480', post_founding_crisis_consolidation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e5b4e2b5-58b7-481c-8d3e-fba6f66ff480', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, senior_officer_corps).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_owned_enterprises).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, security_aligned_technocrats).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement_organizers).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, independent_press).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, provincial_civilian_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds constitutionally ratified veto power over legislation, cabinet formation, and constitutional amendment under the Charter's stability clauses. Frames itself as the neutral guarantor preventing civil collapse. Can dissolve elected bodies it judges destabilizing and faces no electoral accountability for that judgment.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, senior_officer_corps, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate across construction, logistics, and manufacturing sectors with protections that stem directly from the Charter's guardian clause insulating military institutions from civilian regulatory and tax oversight. Their market position depends on the custodianship arrangement continuing.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_owned_enterprises, beneficiary,
    institutional, generational, arbitrage, national).

% Serve in ministries the military screens for loyalty, implementing policy within limits the guardian clause sets. They gain career stability and insulation from electoral turnover in exchange for administering the arrangement.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, security_aligned_technocrats, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__military_custodian_reading, security_aligned_technocrats, agenda_setter).

% Can contest elections but only within boundaries the security apparatus enforces — party registration, candidate vetting, and coalition formation are all subject to review that can disqualify or dissolve parties the military judges destabilizing. Exit means abandoning electoral politics entirely.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties, payer,
    moderate, biographical, constrained, national).

% Organized the mobilizations that produced the Charter's founding crisis and now face surveillance, arrest, and campus lockdowns whenever assembly is judged a stability threat. Their founding role in the revolutionary moment gives them no institutional standing under the ratified arrangement; many face prosecution under emergency provisions the Charter preserved rather than sunset.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement_organizers, payer,
    powerless, biographical, trapped, local).

% Reports on military enterprises and officer-corps conduct under licensing regimes the guardian clause lets the security apparatus revoke for 'threats to national stability.' Self-censorship on military-adjacent stories is now routine; exit means going underground or into exile.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, independent_press, payer,
    moderate, biographical, constrained, national).

% Nominally run local governance but budget allocations and senior appointments require security-council concurrence. Their formal authority is real on paper and hollow in practice whenever it touches anything the military's regional command deems sensitive.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, provincial_civilian_administrators, payer,
    moderate, biographical, constrained, regional).

% Nominally empowered to review Charter compliance but its jurisdiction over guardian-clause invocations was carved out during ratification; it can rule on ordinary legislation but not on the military's own stability determinations, leaving it structurally unable to adjudicate the core of the arrangement.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, constitutional_court, excluded,
    institutional, generational, constrained, national).

% Study the Charter against other post-transition guardian arrangements, documenting how stability clauses of this type have historically evolved (or failed to sunset) across comparable cases.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__military_custodian_reading, senior_officer_corps).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__military_custodian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, unified security guarantor during a fragile transition, preventing the factional violence and institutional collapse that the pre-Charter interregnum produced — genuine coordination against a real breakdown risk.
% TRANSFER_FUNCTION: Moves effective sovereign decision-making power from elected and judicial institutions to the officer corps, and moves economic rents from the regulated civilian economy to military-owned enterprises exempted from ordinary oversight.
% ABSENT_VOICES: Student movement organizers who forced the transition are structurally absent from the ratifying process; autonomous parties dissolved or disqualified under stability review have no seat in amending the Charter that disqualified them; the constitutional court's jurisdictional carve-out was negotiated without public deliberation.
% DISAPPEARANCE_RATIONALE: If the guardian clause vanished overnight, party registration and dissolution power would revert to ordinary electoral and judicial bodies, military enterprises would lose regulatory insulation and face civilian tax and antitrust exposure, press licensing would no longer carry a security veto, and provincial administrators would regain unencumbered budget authority — a substantial reallocation of real power and revenue, not a cosmetic change.
% FOUNDING_PROBLEM: In the immediate post-revolutionary interregnum, no civilian institution commanded enough coercive capacity or cross-factional trust to prevent renewed armed conflict between rival transitional factions; the military's coercive monopoly was the only actor capable of preventing state collapse in the founding weeks.
% FOUNDING_PROBLEM_CORROBORATION: The officer corps and its allied technocrats attest the stability threat remains live, citing residual factional militias and border instability. Independent constitutional scholars, dissolved-party leadership, and student movement organizers attest the acute collapse risk that justified emergency custodianship in the founding months has substantially receded, while the guardian clause itself was expanded rather than narrowed at each subsequent Charter revision — corroboration for the 'shifted function' reading comes from comparative transition scholarship outside the military's own institutions, not from the beneficiaries.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__military_custodian_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__military_custodian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__military_custodian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises across the interval (0.42 to 0.78) as the guardian clause, initially invoked narrowly against acute factional violence, is progressively used to dissolve parties, restrict press licensing, and override provincial budgets — a widening application of the same emergency logic. Suppression is high throughout and rises further (0.55 to 0.82) because holding this arrangement requires active, escalating enforcement: party disqualification, campus surveillance, licensing revocation. Theater is moderate (0.4) — the guardian function had genuine coordination content at the founding moment, but a growing share of its invocations now perform stability-protection against threats that comparative evidence suggests are no longer acute.
 *
 * PERSPECTIVAL GAP:
 *   From the officer corps' seat, the Charter is a legitimate constitutional settlement they steward for the nation's benefit — a rope securing the transition. From the seat of a dissolved party or a surveilled student organizer, the identical structure is an enforced extraction of political voice with no exit. The engine's per-seat computation should register this divergence directly from the declared power/exit asymmetry, not from any claim either side makes about itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The officer corps and military-linked enterprises sit at the beneficiary end: they set the terms of the arrangement, collect its economic and political rents, and face no reciprocal accountability mechanism. Student organizers, dissolved-party leadership, and independent journalists sit at the target end: trapped or constrained exit, no standing to contest guardian-clause invocations that determine their political survival. Provincial administrators occupy an intermediate position — formally empowered, substantively subordinated whenever security concurrence is invoked.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than pure snare preserves the genuine coordination content of the founding moment — a real breakdown risk existed and the military's coercive capacity was, at the founding, the only actor able to prevent renewed civil war. Mandatrophy is visible in the widening gap between the founding problem (acute factional collapse risk) and the persisting arrangement (permanent constitutional veto), and in the asymmetry between whose testimony corroborates that gap: the beneficiaries assert continuity of threat, external scholarship and the excluded parties assert the threat has receded while the guardian mechanism has only expanded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    guardian_clause_scope_ambiguity,
    'Does the Charter''s stability-guardian language, on its own textual terms, extend to routine party registration and press licensing decisions, or was it drafted to cover only acute existential threats to the state?',
    'Textual and drafting-history analysis of the Charter''s guardian clause, compared against the actual pattern of invocations over the interval; testimony from Charter drafters not currently serving in security-aligned roles.',
    'A narrow original scope with broad subsequent application would strengthen the mandatrophy reading (function has drifted from founding purpose); a genuinely broad original scope would suggest the current application is consistent with, not a departure from, the ratified text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guardian_clause_scope_ambiguity, conceptual, 'Whether guardian-clause scope has drifted from its drafted intent or was always broad.').

omega_variable(
    military_custodian_reading_kernel_indexing,
    'Is the military-custodian reading a legitimate independent reading of the Charter''s sovereign-legitimacy kernel, or is it a captured interpretation imposed by the very institution it empowers — i.e., is the officer corps both a party to the kernel contest and the reading''s chief author?',
    'Compare the drafting-committee composition and ratification-process record against the three declared readings (military_custodian, secular_democratic, guided_nationalism) to determine whether the military-custodian reading was adopted through a process the other readings'' proponents also had genuine access to, or through disproportionate security-apparatus influence over the ratifying body.',
    'If the ratification process was itself dominated by the reading''s chief beneficiary, this reading''s claim to representing the Charter''s actual sovereign-legitimacy ground is weaker than a reading produced through contested, pluralistic drafting — this would not change this story''s authored ε (which describes the standing arrangement under contest) but would bear on how much weight the reading''s own legitimacy claim deserves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_custodian_reading_kernel_indexing, conceptual, 'Whether this reading''s dominance reflects genuine kernel contest or capture of the ratifying process by its own beneficiary.').

omega_variable(
    acute_vs_residual_threat_level,
    'What is the current, empirically measurable level of factional-violence or state-collapse risk, relative to the level that prevailed at Charter ratification?',
    'Comparative security-incident data pre- and post-ratification; independent risk assessments from regional security scholars not embedded in the domestic security apparatus.',
    'A substantially reduced threat level would corroborate the founding_problem_status as effectively dead, sharpening the mandatrophy diagnosis; a sustained or rising threat level would support the officer corps'' live-threat claim and weaken the reclassification case.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(acute_vs_residual_threat_level, empirical, 'Whether the founding stability threat has empirically receded or persisted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(july_tr_t8, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(july_tr_t16, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(july_tr_t24, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(july_tr_t32, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(july_tr_t40, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(july_be_t8, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(july_be_t16, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(july_be_t24, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(july_be_t32, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(july_be_t40, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(july_su_t8, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(july_su_t16, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(july_su_t24, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(july_su_t32, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 32, 0.79).
narrative_ontology:measurement(july_su_t40, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the july_charter_sovereign_legitimacy kernel, decomposed per the epsilon-invariance principle: the same Charter text supports structurally distinct legitimacy claims with different beneficiary/victim sets and different epsilon values. military_custodian_reading (this story) authors high, rising extraction centered on security-apparatus subordination of civilian institutions. secular_democratic_reading authors near-zero extraction under a civilian-supremacy interpretation of the identical text. guided_nationalism_reading authors a distinct extraction profile centered on religious-national identity as the sovereignty ground, with a different victim set (religious minorities, secular dissenters) than this reading's victim set (political parties, students, press). The three are linked, not merged, and none is averaged against the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
