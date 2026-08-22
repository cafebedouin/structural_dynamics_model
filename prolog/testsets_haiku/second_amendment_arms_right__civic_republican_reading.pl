% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__civic_republican_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Second Amendment Arms Right: Civic Republican Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The Second Amendment is a contested constitutional text that can be read
 *   through at least three structurally distinct lenses. This story
 *   instantiates the civic-republican reading: the Amendment protects armed
 *   citizenship not as an individual pre-political right, but as a
 *   prerequisite for republican self-governance — armed citizens maintain a
 *   distributed check on tyranny through militia participation and civic
 *   duty. The reading is neither libertarian individualism (which abandons
 *   the civic frame) nor state collectivism (which makes the right depend on
 *   the state militia). Instead, it treats bearing arms as a civic obligation
 *   bound up with republican participation. The beneficiary is the armed
 *   citizen-as-militia-member, and the cost is the extraction of civic
 *   participation, training, and regulatory constraint embedded in that civic
 *   framework. Exclusion from citizenship historically meant exclusion from
 *   this right — a feature, not a bug, of the reading's logic.
 *
 * KEY AGENTS:
 *   - armed_citizens_in_militia_tradition: Structural beneficiary (right + republican civic participation)
 *   - regulatory_authority: Agenda setter (defines civic-participation requirements, training standards)
 *   - those_excluded_from_civic_participation: Structural victims (barred by the reading's own logic)
 *   - individual_liberty_advocates: Excluded seat (would reject the civic framing entirely)
 *   - state_militia_authority_advocates: Excluded seat (would make the right state-dependent, not citizen-centered)
 *   - courts_interpreting_the_text: Observer (must decide enforceability and scope)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.38).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.42).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Second Amendment Arms Right: Civic Republican Reading").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, '74c9aef7-8f9e-4969-afe6-a0650b611f34').
narrative_ontology:cs_kernel_codification('74c9aef7-8f9e-4969-afe6-a0650b611f34', fixed_text).
narrative_ontology:cs_authority_grounding('74c9aef7-8f9e-4969-afe6-a0650b611f34', lineage).
narrative_ontology:cs_interpretation_layer_present('74c9aef7-8f9e-4969-afe6-a0650b611f34').
narrative_ontology:cs_reading_relation('74c9aef7-8f9e-4969-afe6-a0650b611f34', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('74c9aef7-8f9e-4969-afe6-a0650b611f34', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('74c9aef7-8f9e-4969-afe6-a0650b611f34', foundational, armed_citizenship_constitutive_of_republic).
narrative_ontology:cs_axiom_status(armed_citizenship_constitutive_of_republic, holdable).
narrative_ontology:cs_axiom_grounding('74c9aef7-8f9e-4969-afe6-a0650b611f34', armed_citizenship_constitutive_of_republic, deontological).
narrative_ontology:cs_axiom('74c9aef7-8f9e-4969-afe6-a0650b611f34', foundational, civic_participation_prerequisite_to_armament).
narrative_ontology:cs_axiom_status(civic_participation_prerequisite_to_armament, holdable).
narrative_ontology:cs_axiom_grounding('74c9aef7-8f9e-4969-afe6-a0650b611f34', civic_participation_prerequisite_to_armament, conventional).
narrative_ontology:cs_reference_frame('74c9aef7-8f9e-4969-afe6-a0650b611f34', founding_era_republican_theory).
narrative_ontology:cs_drift_state('74c9aef7-8f9e-4969-afe6-a0650b611f34', contemporary_professionalized_military, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('74c9aef7-8f9e-4969-afe6-a0650b611f34', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, armed_citizens_in_militia_tradition).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, republican_self_governance_framework).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, those_excluded_from_civic_participation).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, regulatory_capacity_constrained_by_civic_norm).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, armed_citizens_in_militia_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess and bear arms as part of a civic duty to maintain republican self-governance against tyranny. They receive constitutional protection for armed preparedness tied to militia participation or readiness. They also bear the cost of training, qualification, and responsiveness to civic requirements — the reading does not grant them unrestricted armament independent of civic context.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, armed_citizens_in_militia_tradition, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, armed_citizens_in_militia_tradition, payer).

% Sets and enforces rules defining who participates in the civic militia tradition and what training/qualification is prerequisite to armed citizenship. Acts as gatekeeper of the civic-participation framework. Under this reading, authority is constrained by the civic-republicanism norm: cannot disarm the population categorically or prevent genuine militia participation, but can require demonstrated capability and civic commitment.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, regulatory_authority, agenda_setter,
    institutional, generational, mobile, national).

% Historically and structurally: enslaved persons, women disenfranchised from political participation, non-citizens, those deemed unfit for civic trust (felons, the mentally ill by period standards). They bear the cost of being outside the civic-participation framework that the reading centers: they cannot claim arms rights because the right is predicated on membership in the civic body. Their exclusion is structural to the reading, not incidental.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, those_excluded_from_civic_participation, payer,
    powerless, biographical, identity_locked, national).

% Would argue that arms rights attach to individual persons independent of civic participation or militia duty — that the right is pre-political and cannot be conditioned on state-approved civic training. They are excluded from this reading's framing because the reading explicitly ties rights to republican civic participation, not individual autonomy.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, individual_liberty_advocates, excluded,
    moderate, biographical, constrained, national).

% Would argue that the right protects organized state militia alone, not armed citizenry outside formal military structure. They are excluded from this reading's framing because the reading treats citizenship and militia participation as inseparable from individual armed readiness — the right belongs to citizens-as-militia, not to the state apparatus.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, state_militia_authority_advocates, excluded,
    institutional, generational, constrained, national).

% Judicial seats that must decide what the Second Amendment permits. Under this reading, courts operate within the civic-republicanism frame: they cannot disarm citizens wholesale because that breaks the militia check, but they can require training and civic commitment as conditions of the right.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, courts_interpreting_the_text, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__civic_republican_reading, regulatory_authority).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__civic_republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a dispersed armed citizenry capable of resisting concentrated state tyranny through militia readiness. The coordination problem solved is: how does a republic preserve liberty without a standing army that itself becomes tyrannical? Answer: distribute arms to citizens and structure civic participation around militia duty.
% TRANSFER_FUNCTION: Moves constitutional protection and civic authority from the state monopoly to the distributed body of armed citizens. In exchange, it extracts civic participation, training, qualification, and the obligation to remain armed and ready as a constraint on personal autonomy.
% ABSENT_VOICES: Those excluded from citizenship and civic participation in the period of the Amendment's ratification — enslaved persons, women, non-property-holders, non-citizens. They would object that the right is structured on a civic framework that explicitly bars them from participation; their exclusion is not incidental to the reading but central to it.
% DISAPPEARANCE_RATIONALE: If this particular constraint (the civic-republicanism reading) disappeared, the republican check on tyranny through militia readiness would dissolve — but the underlying constitutional text would not. The real question is whether citizens would reorganize under the individual-right reading or the collective-right reading, or establish some alternative framework. The civic frame itself is contestable.
% FOUNDING_PROBLEM: How can a republic prevent tyranny without relying on a standing army that becomes tyrannical itself? The answer offered by this reading: an armed citizenry structured around militia participation and civic virtue, where bearing arms is a republican duty as well as a right.
% FOUNDING_PROBLEM_CORROBORATION: The civic-republicanism reading draws support from historical scholarship on founding-era political thought (Pocock, Skinner on republican theory; Wood on the founding generation's fear of standing armies). It is contested by scholars who emphasize Locke's individualism and by those who stress the militia clauses as state-centered. No single authoritative source outside the benefiting community attests the founding problem in these terms; the reading itself is a historiographic claim within democratic discourse.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__civic_republican_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_arms_right__civic_republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__civic_republican_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).
:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the reading embeds a genuine coordination function (militia check on tyranny) but also structural extraction: the right is conditioned on civic participation, training, and regulatory approval — a citizen cannot simply claim arms independent of state-evaluated civic fitness. The measurement trajectory shows a rise through the 19th and early 20th centuries (1934 National Firearms Act, 1968 Gun Control Act) as regulatory apparatus grew, peaking at mid-century, then declining as the individual-right reading gained judicial traction post-2008 (District of Columbia v. Heller). Theater is low-to-moderate (0.25), reflecting that the civic-republicanism frame is genuinely invoked in court opinions and scholarship, but increasingly competes with the individual-right reading for judicial adoption. Suppression mirrors extractiveness: the constraint requires active enforcement to define who counts as a civic participant and to exclude those deemed unfit. Accessibility collapse is moderate (0.65) because alternatives exist — the individual reading and the collective reading are live scholarly and judicial positions — but the civic-republican frame has deep historical and philosophical roots that make it hard to dismiss entirely.
 *
 * PERSPECTIVAL GAP:
 *   From the armed citizen's perspective (constrained to the civic frame), the right is genuine and the duty of participation is a civic honor, not extraction. From the perspective of those excluded from civic participation by law or circumstance, the same frame is purely extractive: they are barred from the right because they are barred from citizenship. From a court's perspective, the reading constrains regulatory authority (cannot disarm citizens wholesale) but permits training and qualification requirements. From an individual-liberty advocate's perspective, the reading IS the extraction — it subordinates individual autonomy to the civic frame. The engine computes these divergences from the structural data; the authored claim (tangled_rope) does not adjudicate which seat's experience dominates.
 *
 * DIRECTIONALITY LOGIC:
 *   Armed citizens in militia tradition: d ≈ 0.35–0.45 (beneficiary of the right, but payer of civic obligation and training requirements — symmetry slightly toward benefit because they are the intended constitutional subject). Regulatory authority: d ≈ 0.55–0.65 (constrained by the civic norm but still sets the rules and enforces membership criteria — moderate institutional power, slightly toward target). Those excluded: d ≈ 0.85–0.95 (full targets — the reading's logic locks them out of the right by denying them civic participation; identity_locked exit because the exclusion is structural to the reading, not a practical barrier they could overcome). Courts: analytical position, not a party to extraction. Individual-liberty advocates and state-militia advocates: excluded from the frame entirely, so directionality is not computed (they reject the frame, not just their position within it).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading carries moderate mandatrophy risk. The founding problem — preventing tyranny without a standing army — was live in 1791 and remained so through the 19th century. But by the late 20th century, several factors shifted the mandate's functional status: (1) The US developed a standing army and military-industrial complex, making the militia check structurally implausible. (2) Most citizens no longer identify with militia participation as a republican duty; the civic frame has atrophied. (3) The individual-right reading gained judicial ascendance (2008: Heller), fragmenting the consensus on the reading itself. The civic-republicanism frame persists in scholarship and some judicial opinions, but the mandate (the founding problem) is increasingly contested rather than live. The measurement trajectory reflects this: extractiveness rises into the early 20th century as regulatory capacity grows, peaks as the civic frame still carries authority, then stabilizes (rather than declining) because the reading persists as a live intellectual position even if its functional mandate has atrophied. Theater ratio rises modestly but does not dominate, because the reading is not purely theatrical — courts genuinely invoke it — but its practical grip on constitutional interpretation has loosened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_mandate_functional_atrophy,
    'Is the founding problem (preventing tyranny through distributed militia readiness) still live, or has it atrophied with the professionalization of military force and the erosion of civic militia participation?',
    'Historical-institutional analysis: track when militia participation became symbolic vs. functional; survey contemporary citizen identification with militia duty; examine whether distributed armament actually constrains government tyranny in the modern context or is theater maintained by the civic frame.',
    'If the mandate has atrophied, the reading may no longer support extraction of civic participation and training requirements — the constraint would become a piton (theatrical maintenance of a degraded civic norm). If the mandate is still live, the reading retains justification for the moderate extractiveness authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_mandate_functional_atrophy, empirical, 'Whether the civic-republicanism reading''s founding mandate persists as functional or has become decorative.').

omega_variable(
    civic_participation_exclusion_design,
    'Is the exclusion of non-citizens and non-voting persons from the arms right a feature of the civic-republicanism reading (integral to the frame), or a historical artifact that the reading would discard if the frame were renewed?',
    'Textual and historical analysis: can the civic-republicanism reading be extended to non-traditional participants (women, non-property-holders, non-citizens) while preserving the core logic of armed citizenship tied to republican participation? Or does the reading''s logic entail categorical exclusion?',
    'If exclusion is integral, the reading perpetuates systematic injustice by design and the extraction is higher than authored (would shift extractiveness upward, suppression upward, and add a high-extraction class dimension). If exclusion is artifact, the reading could be reframed to include broader civic participation, lowering effective exclusion-based extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civic_participation_exclusion_design, conceptual, 'Whether the civic-republicanism reading''s exclusions are structural or contingent on historical circumstances.').

omega_variable(
    militia_check_structural_plausibility,
    'In a modern state with professional standing armies, air forces, and nuclear weapons, can a distributed armed citizenry actually function as a check on tyranny, or is the militia-check narrative purely rhetorical?',
    'Comparative institutional analysis: examine cases where armed citizenry has actually resisted state tyranny in the modern era; assess whether the restraint effect operates through direct military capacity or through political legitimacy and cost-raising for the state.',
    'If plausible, the reading''s coordination function is genuine and the moderate extractiveness is justified. If purely rhetorical, the reading''s civic frame is theater, and the extractiveness should be reclassified as suppression (the constraint persists by maintaining a false narrative, not by delivering actual coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_check_structural_plausibility, empirical, 'Whether the civic-republicanism reading''s coordination claim (militia check on tyranny) is structurally viable in modernity.').

omega_variable(
    reading_foreclosure_test,
    'Does the civic-republican reading logically foreclose the individual-right reading, or do both readings remain coherent within different normative frameworks?',
    'Philosophical analysis: can a single party hold both that the right is individual (pre-political, inalienable) AND that it is conditioned on civic participation and republican duty? Or is this a genuine contradiction?',
    'If the readings foreclose each other, the reading_relations should declare ''forecloses'' instead of ''coexists_with''. If both remain logically coherent (even if politically contested), ''coexists_with'' is correct and the contest is between live options, not between a true claim and a false one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether the civic-republican and individual-right readings are logically incompatible or both coherent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement(seco_tr_t1870, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1870, 0.15).
narrative_ontology:measurement(seco_tr_t1934, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1934, 0.22).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1968, 0.28).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__civic_republican_reading, theater_ratio, 2008, 0.25).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_arms_right__civic_republican_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1791, 0.35).
narrative_ontology:measurement(seco_be_t1870, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1870, 0.42).
narrative_ontology:measurement(seco_be_t1934, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1934, 0.48).
narrative_ontology:measurement(seco_be_t1968, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1968, 0.52).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 2008, 0.38).
narrative_ontology:measurement(seco_be_t2024, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1791, 0.35).
narrative_ontology:measurement(seco_su_t1870, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1870, 0.38).
narrative_ontology:measurement(seco_su_t1934, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1934, 0.45).
narrative_ontology:measurement(seco_su_t1968, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1968, 0.48).
narrative_ontology:measurement(seco_su_t2008, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 2008, 0.42).
narrative_ontology:measurement(seco_su_t2024, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__civic_republican_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__collective_right_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment is a kernel (fixed constitutional text) that admits multiple readings. This story is ONE reading (civic-republican). Sibling stories instantiate the individual-right reading and the collective-right reading. All three share the same constitutional text but author different ε values, different beneficiary/victim structures, and different exclusion logics. They are not alternative measurements of one constraint; they are structurally distinct constraints derived from the same kernel via different readings. The ε-invariance principle requires separate stories: a civic reading that centers republican duty and civic participation cannot have the same ε as an individual reading that treats arms as pre-political autonomy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_arms_right__civic_republican_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
