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
 *   July Charter kernel: the charter ratifies the armed forces as permanent
 *   institutional guardian of the state, holding veto authority over
 *   legislation, cabinet formation, and constitutional amendment on
 *   'stability' grounds. This is a distinct constraint from the
 *   secular_democratic_reading (which reads the same charter as mandating
 *   military subordination to civilian authority) and the
 *   guided_nationalism_reading (which reads it as establishing
 *   religious-nationalist sovereign legitimacy). Each reading has its own ε,
 *   its own beneficiary/victim structure, and its own classification; they
 *   are linked only through the shared kernel, not merged into one story.
 *
 * KEY AGENTS:
 *   - senior_officer_corps: Primary agenda-setter and beneficiary (institutional/arbitrage) — holds and administers the veto
 *   - internal_security_apparatus: Co-agenda-setter (institutional/arbitrage) — enforces the boundary conditions on political contestation
 *   - student_movement_organizers: Primary target (powerless/trapped) — founding actors of the transition now treated as standing security risk
 *   - autonomous_political_parties: Primary target (moderate/constrained) — bounded contestation space, subject to deregistration
 *   - civilian_legislature: Structurally subordinated actor (organized/constrained) — formally sovereign, substantively vetoable
 *   - constitutional_historians: Analytical observer (analytical/analytical) — traces whether custodian clauses recede or entrench
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, 0.71).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__military_custodian_reading, 0.79).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "July Charter — Military Custodianship Reading").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "constitutional_law/political_transitions").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, '25706ff8-f06e-4452-ae43-c4a3f9c80e6d').
narrative_ontology:cs_kernel_codification('25706ff8-f06e-4452-ae43-c4a3f9c80e6d', formalized).
narrative_ontology:cs_authority_grounding('25706ff8-f06e-4452-ae43-c4a3f9c80e6d', extraction).
narrative_ontology:cs_interpretation_layer_present('25706ff8-f06e-4452-ae43-c4a3f9c80e6d').
narrative_ontology:cs_reading_relation('25706ff8-f06e-4452-ae43-c4a3f9c80e6d', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('25706ff8-f06e-4452-ae43-c4a3f9c80e6d', july_charter_sovereign_legitimacy__guided_nationalism_reading, coexists_with).
narrative_ontology:cs_axiom('25706ff8-f06e-4452-ae43-c4a3f9c80e6d', foundational, security_apparatus_is_final_arbiter_of_political_legitimacy).
narrative_ontology:cs_axiom_status(security_apparatus_is_final_arbiter_of_political_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('25706ff8-f06e-4452-ae43-c4a3f9c80e6d', security_apparatus_is_final_arbiter_of_political_legitimacy, instrumental).
narrative_ontology:cs_axiom('25706ff8-f06e-4452-ae43-c4a3f9c80e6d', secondary, stability_preservation_overrides_electoral_sovereignty).
narrative_ontology:cs_axiom_status(stability_preservation_overrides_electoral_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('25706ff8-f06e-4452-ae43-c4a3f9c80e6d', stability_preservation_overrides_electoral_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('25706ff8-f06e-4452-ae43-c4a3f9c80e6d', military_guardianship_stability_doctrine).
narrative_ontology:cs_drift_state('25706ff8-f06e-4452-ae43-c4a3f9c80e6d', post_transition_decade, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('25706ff8-f06e-4452-ae43-c4a3f9c80e6d', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, senior_officer_corps).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_owned_enterprises).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, internal_security_apparatus).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement_organizers).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, independent_judiciary_appointees).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_legislature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds a charter-conferred veto over constitutional amendments, cabinet formation, and any legislation touching 'national security,' a term the charter leaves undefined and which the corps itself interprets. Presents this authority as the guarantor of stability during the transition; in practice sets the outer bounds of what civilian politics may attempt and can dissolve or sideline any government that tests those bounds.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, senior_officer_corps, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__military_custodian_reading, senior_officer_corps, beneficiary).

% Operate a substantial share of construction, logistics, and consumer manufacturing sectors under regulatory and procurement conditions shaped by the officer corps's veto power. The charter's stability framing insulates these enterprises from the antitrust or transparency scrutiny that would apply to any comparable civilian conglomerate.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_owned_enterprises, beneficiary,
    institutional, generational, arbitrage, national).

% Administers the charter's emergency and public-order provisions: surveillance of political organizing, permitting for assembly, and detention authority exercised outside ordinary judicial review. Its budget and personnel expansion are justified directly by the charter's stability mandate.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, internal_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__military_custodian_reading, internal_security_apparatus, beneficiary).

% May contest elections only within a charter-defined ideological floor that the security apparatus screens for; parties judged to threaten 'stability' face deregistration, leadership arrest, or exclusion from ballots. Leaving formal politics does not remove the constraint — it removes the party's only lawful channel.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties, payer,
    moderate, biographical, constrained, national).

% Organized the demonstrations that forced the prior regime's collapse and pressed for civilian constitutional drafting; under the charter's custodian reading they are treated as a standing security risk, subject to campus surveillance, assembly permitting, and periodic detention. Their founding role in the transition gives them no formal seat in the arrangement they helped produce.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement_organizers, payer,
    powerless, biographical, trapped, regional).

% Judges reviewing charter-adjacent cases operate under an appointment and removal process the officer corps can influence when rulings touch security matters; rulings against the military's interpretation of its own veto have been followed by transfer or non-renewal. Judicial independence exists on paper but is bounded in the cases that matter most to the arrangement.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, independent_judiciary_appointees, payer,
    moderate, biographical, constrained, national).

% Elected but structurally subordinate: any bill the officer corps designates as security-relevant requires its concurrence, and the corps can dissolve the sitting legislature under the charter's stability clause. Legislators who built careers within this arrangement have adapted to working around the veto rather than contesting its existence.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_legislature, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_legislature, excluded).

% The body that drafted the charter's civilian-facing provisions was not permitted to draft the security and veto clauses, which were presented to it as non-negotiable by the officer corps prior to ratification. It would have argued for full civilian supremacy but was not in the room for that section.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_transitional_council, excluded,
    organized, biographical, constrained, national).

% Study comparative post-revolutionary transitions where a security guarantor clause was ratified alongside civilian institutions, tracking whether such clauses recede over time or harden into permanent parallel sovereignty.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, constitutional_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__military_custodian_reading, senior_officer_corps).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__military_custodian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In the immediate post-collapse period, a functioning coercive apparatus capable of preventing renewed civil violence, securing borders, and preventing a security vacuum was a genuine problem; the charter's custodian clause solves the coordination problem of who holds the monopoly on organized force during a fragile transition.
% TRANSFER_FUNCTION: Moves veto authority over legislation, cabinet formation, and constitutional amendment from elected civilian institutions to the officer corps; moves budgetary and regulatory insulation to military-owned enterprises; moves organizing risk from the state onto political parties and student movements who must now operate inside security-defined boundaries.
% ABSENT_VOICES: The student movement that precipitated the transition, and the political parties later deregistered under the stability clause, were not party to the negotiation of the security provisions — the civilian transitional council itself was presented those clauses as a ratification condition, not a subject of drafting. Their objection would be that a charter drafted to end authoritarian rule has re-installed a parallel sovereign.
% DISAPPEARANCE_RATIONALE: If the military veto clause were removed overnight, civilian legislation and cabinet formation would proceed without a security check, deregistered parties would seek reinstatement, detained organizers would seek release, and military-owned enterprises would face the regulatory and antitrust scrutiny the stability framing currently forecloses — the entire post-charter political landscape would reorganize around unconditional civilian supremacy.
% FOUNDING_PROBLEM: In the immediate aftermath of the old regime's collapse, there was a genuine risk of security vacuum, factional armed violence, and external opportunism; the charter's custodian clause was presented as the mechanism to prevent state collapse during civilian institution-building.
% FOUNDING_PROBLEM_CORROBORATION: The officer corps and allied commentators attest the security risk remains live, citing residual factional violence and regional instability. Independent constitutional historians and international election-observation missions attest the acute security vacuum that justified the clause closed within the first two years of the transition, and that the veto's persistence since then tracks institutional entrenchment rather than any continuing emergency — this corroboration comes from outside the military's own institutional voice.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__military_custodian_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__military_custodian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__military_custodian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction begins moderate (0.38) reflecting a real, if narrow, coordination function in the acute post-collapse period, and rises to 0.71 by year 40 as the veto persists well past the security vacuum it was built for — the temporal series documents entrenchment rather than a fixed extractive baseline. Suppression is authored high throughout (0.5 rising to 0.79) because the constraint's core operating mechanism is the exclusion of political and student organizing from the boundaries it sets, not merely a side effect. Theater ratio rises moderately (0.2 to 0.42) as 'stability' invocations increasingly justify actions (enterprise protection, incumbent-legislature dissolution threats) with no plausible connection to preventing armed factional violence.
 *
 * PERSPECTIVAL GAP:
 *   From the officer corps's seat, the charter is coordination: a necessary guarantor function during fragile state-building, with a real founding problem behind it. From the student movement and deregistered parties' seats, the same structure is enforced extraction: their founding contribution to the transition bought them no seat, and the security classification used against them is the same apparatus whose expansion the charter licensed. The engine computes these as different seat-level types from the same structural data; the divergence is exactly the phenomenon the tangled_rope classification is built to hold — genuine coordination function at founding, genuine asymmetric extraction in persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   The officer corps and its affiliated enterprises and security apparatus are the structural beneficiaries — they collect veto power, budgetary insulation, and enterprise protection, and hold arbitrage-grade exit from any consequence of their own decisions. Student organizers sit at the extreme target end: powerless, trapped (there is no legal channel to exit the security apparatus's classification of them as a risk), and bearing the constraint's suppression most directly despite having authored the transition that produced the charter. Political parties and the judiciary sit closer to constrained-moderate: some institutional standing, but bounded contestation space. The civilian legislature is subordinated but not eliminated — hence payer role with an excluded secondary dimension on matters the corps designates as security-relevant.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored contested rather than resolved: this prevents the story from either (a) treating the custodian clause as pure legitimate coordination frozen at its founding moment, or (b) treating it as pure cynical extraction with no original coordination function. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges is the signal the R5 consumer is built to read — a genealogy where the officer corps insists the emergency is live while independent historians and observer missions attest it closed years ago is precisely the zombie-mandate pattern the framework flags for downstream capture analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the July Charter''s ambiguous drafting genuinely support the military-custodian reading as a matter of textual and constitutional-historical fact, or is this reading a post-ratification interpretive overlay imposed by the officer corps itself?',
    'Comparative analysis of the charter''s drafting record: were the security-veto clauses debated and approved by the civilian transitional council, or presented as a non-negotiable ratification condition? Testimony and drafting minutes would resolve this.',
    'If the drafting record shows the clauses were negotiated in good faith as a temporary stability bridge, this reading''s coordination function is stronger and a scaffold classification (with an implicit, if unstated, sunset expectation) becomes more defensible. If the record shows the clauses were imposed as a condition of ratification with no sunset ever contemplated, the tangled_rope classification undersells the extraction and a snare reading becomes more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the military-custodian reading reflects genuine charter intent or post-hoc institutional capture of ambiguous text.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does ratifying the military-custodian reading in practice foreclose the secular_democratic_reading from ever being realized under the same charter text, or can both readings persist as live contested interpretations indefinitely?',
    'Track whether any judicial or legislative body successfully invokes the charter''s civilian-supremacy language to curtail the veto in practice — a single successful invocation would demonstrate the readings coexist as live contest rather than one having foreclosed the other.',
    'If the custodian reading has become the only reading with practical force (all civilian-supremacy invocations fail), the sibling relation should be understood as increasingly close to foreclosure despite being authored as coexists_with — this omega tracks that drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, empirical, 'Whether the military-custodian reading''s entrenchment is displacing the secular_democratic_reading''s practical viability over time.').

omega_variable(
    stability_function_separability,
    'Is the coercive-stability function (preventing armed factional violence) structurally separable from the political-veto function (blocking legislation, dissolving legislatures, screening parties), or does the charter''s custodian reading require both to be bundled together?',
    'Comparative transition case study: identify post-revolutionary states where security guarantor status was constitutionally separated from political veto power, and assess whether stability outcomes held.',
    'If separable, the political-veto component is pure extraction riding on a genuine security-coordination function, sharpening the tangled_rope diagnosis. If inseparable, part of the measured extraction reflects the actual cost of the security coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stability_function_separability, conceptual, 'Whether the constraint''s coordination and extraction components can be structurally disentangled.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(july_tr_t8, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(july_tr_t16, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(july_tr_t24, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(july_tr_t32, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(july_tr_t40, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(july_be_t8, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(july_be_t16, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(july_be_t24, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(july_be_t32, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(july_be_t40, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(july_su_t8, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(july_su_t16, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(july_su_t24, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(july_su_t32, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(july_su_t40, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the july_charter_sovereign_legitimacy kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle. military_custodian_reading (this story) authors the security-veto arrangement as substantially extractive and entrenching (ε rising 0.38→0.71). secular_democratic_reading authors the same charter text as mandating civilian supremacy with the military properly subordinated — a different beneficiary/victim structure and a different ε. guided_nationalism_reading addresses a third, largely orthogonal axis of the same charter: religious-nationalist identity as the sovereign legitimacy ground, rather than the coercive-authority question this story and secular_democratic_reading contest. All three are linked via affects_constraints because they share the same founding text and interpretive contest, and a shift in one reading's institutional dominance (e.g., a successful civilian-supremacy court ruling) would structurally pressure the others' plausibility and entrenchment trajectory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
