% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__anti_caste_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__anti_caste_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: fourteenth_amendment_equal_protection__anti_caste_reading
 *   human_readable: Equal Protection Anti-Caste Mandate (Active Dismantling Reading)
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The anti-caste reading of the Fourteenth Amendment's Equal Protection
 *   Clause holds that the Constitution requires the state to actively
 *   dismantle racial, gender, and status hierarchy — not merely to refrain
 *   from explicit classification. This reading emerged from Reconstruction,
 *   was suppressed during the Lochner/Plessy era, revived in the civil rights
 *   era (Brown, Green, Swann), expanded to gender (Reed, Frontiero, Craig)
 *   and other status hierarchies, and now faces systematic retrenchment
 *   (Bakke, Croson, Adarand, Shelby County, SFFA). The constraint
 *   instantiates high base extractiveness (0.72) because remedial programs
 *   transfer substantial resources and positional goods from majority groups
 *   and taxpayers to subordinated groups. Suppression is high (0.68) because
 *   the constraint's persistence depends on active judicial enforcement
 *   against political resistance — when courts withdraw supervision (Shelby
 *   County), remedial structures collapse. Theater ratio (0.42) reflects that
 *   formal compliance often substitutes for substantive dismantling
 *   (diversity rhetoric without material redistribution). The claimed_type is
 *   tangled_rope: genuine coordination function (dismantling caste) plus
 *   asymmetric extraction (remedial costs borne by non-beneficiaries),
 *   requiring active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.72).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.68).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "Equal Protection Anti-Caste Mandate (Active Dismantling Reading)").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, 'f9e5397f-9cfc-45b0-9eca-78a7794e1059').
narrative_ontology:cs_kernel_codification('f9e5397f-9cfc-45b0-9eca-78a7794e1059', formalized).
narrative_ontology:cs_authority_grounding('f9e5397f-9cfc-45b0-9eca-78a7794e1059', lineage).
narrative_ontology:cs_interpretation_layer_present('f9e5397f-9cfc-45b0-9eca-78a7794e1059').
narrative_ontology:cs_reading_relation('f9e5397f-9cfc-45b0-9eca-78a7794e1059', fourteenth_amendment_equal_protection__formal_equality_reading, coexists_with).
narrative_ontology:cs_axiom('f9e5397f-9cfc-45b0-9eca-78a7794e1059', foundational, caste_dismantling_mandatory).
narrative_ontology:cs_axiom_status(caste_dismantling_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('f9e5397f-9cfc-45b0-9eca-78a7794e1059', caste_dismantling_mandatory, deontological).
narrative_ontology:cs_axiom('f9e5397f-9cfc-45b0-9eca-78a7794e1059', foundational, remedial_programs_constitutionally_required).
narrative_ontology:cs_axiom_status(remedial_programs_constitutionally_required, holdable).
narrative_ontology:cs_axiom_grounding('f9e5397f-9cfc-45b0-9eca-78a7794e1059', remedial_programs_constitutionally_required, deontological).
narrative_ontology:cs_axiom('f9e5397f-9cfc-45b0-9eca-78a7794e1059', secondary, structural_inequality_is_state_concern).
narrative_ontology:cs_axiom_status(structural_inequality_is_state_concern, holdable).
narrative_ontology:cs_axiom_grounding('f9e5397f-9cfc-45b0-9eca-78a7794e1059', structural_inequality_is_state_concern, deontological).
narrative_ontology:cs_reference_frame('f9e5397f-9cfc-45b0-9eca-78a7794e1059', reconstruction_amendment_mandate).
narrative_ontology:cs_drift_state('f9e5397f-9cfc-45b0-9eca-78a7794e1059', contemporary_colorblind_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f9e5397f-9cfc-45b0-9eca-78a7794e1059', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, racially_subordinated_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, gender_subordinated_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, status_hierarchy_subjected_persons).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, general_taxpayers).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, majority_group_members_in_zero_sum_contexts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, gender_subordinated_groups).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, state_and_local_governments).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, constitutional_anti_caste_principle).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, state_affirmative_duty_to_dismantle_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Black, Latino, Indigenous, and other racially minoritized populations who experience cumulative disadvantage from historical and ongoing caste-like hierarchy. They are the intended beneficiaries of race-conscious remedial programs (affirmative action, voting rights enforcement, disparate impact liability). Their exit from the caste structure is identity-locked — race is ascribed and inescapable within the current social order; they cannot opt out of being targeted by hierarchy even if they could theoretically exit remedial programs.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, racially_subordinated_groups, beneficiary,
    organized, generational, identity_locked, national).

% Women and gender minorities subjected to patriarchal status hierarchy. They benefit from anti-caste remedies (Title VII, Title IX, reproductive rights jurisprudence, pay equity enforcement) but also bear costs as taxpayers funding the remedial state. Gender is identity-locked in the same structural sense as race — ascribed, inescapable, constitutive of the hierarchy the constraint targets.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, gender_subordinated_groups, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__anti_caste_reading, gender_subordinated_groups, payer).

% Persons subjected to caste-like status hierarchies beyond race and gender: LGBTQ+ persons (sexual orientation hierarchy), disabled persons (ability hierarchy), religious minorities (sectarian hierarchy), immigrant communities (national-origin hierarchy). The anti-caste reading extends to these groups, but their inclusion is more contested and their remedial programs less entrenched. Exit is constrained — some statuses are identity-locked (sexual orientation, disability), others are more fluid but socially enforced.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, status_hierarchy_subjected_persons, beneficiary,
    moderate, biographical, constrained, national).

% The broad tax base that funds remedial programs: affirmative action administration, voting rights enforcement, disparate impact litigation, school desegregation orders, Title IX compliance infrastructure, disability accommodations. They bear the extraction without direct recourse; exit is constrained (emigration is costly, tax obligation is territorial). The anti-caste reading legitimates this extraction as constitutionally required, not discretionary policy.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, general_taxpayers, payer,
    organized, biographical, constrained, national).

% White, male, cisgender, heterosexual, non-disabled, native-born citizens who experience remedial programs as zero-sum losses: university seats, government contracts, promotions, legislative districts. They have political power (electoral majority in many jurisdictions) and mobile exit options (private alternatives, geographic sorting, capital mobility). They are the primary political resistance to the anti-caste constraint, framing it as reverse discrimination.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, majority_group_members_in_zero_sum_contexts, payer,
    powerful, biographical, mobile, national).

% The Supreme Court and lower federal courts that define the scope of the anti-caste mandate, supervise remedial orders, and adjudicate challenges. They set the agenda through precedent (Brown, Bakke, Grutter, Shelby County, SFFA). Their power is institutional (life tenure, judicial review); their horizon is generational (doctrine evolves over decades); they sit in an analytical seat relative to the constraint — they interpret it but do not directly bear its costs or collect its benefits.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% State and local governments that implement remedial programs (school districts, university systems, police departments, employers) and bear direct compliance costs. They are agenda-setters in designing remedies but also payers when courts impose structural injunctions. Exit is constrained by federalism — they cannot opt out of constitutional mandates but can resist, delay, or minimize compliance.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, state_and_local_governments, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__anti_caste_reading, state_and_local_governments, payer).

% Colorblind constitutionalists, originalists, and classical liberals who argue Equal Protection forbids race-conscious remedies. They hold significant institutional power (Federalist Society network, Supreme Court supermajority, state legislatures) and arbitrage-grade exit (they can forum-shop, litigate strategically, shift to state constitutional grounds). They are structurally excluded from the anti-caste reading's beneficiary set — their objection is that the reading itself creates a new caste system. Their voice is absent from the anti-caste constraint's internal logic but dominates the external contest.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, formal_equality_advocates, excluded,
    powerful, generational, arbitrage, national).

% Legal scholars and activists who articulate and defend the anti-caste reading. They do not directly administer or fund remedies, nor do they bear the extraction — they provide the intellectual architecture. Their seat is analytical: they observe the constraint's operation, diagnose its gaps, and contest its capture by formal equality advocates. They have no direct power to enforce or extract.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, critical_race_theorists, observer,
    moderate, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dismantles racial, gender, and status caste through state corrective action: the constraint coordinates society around the principle that hierarchy is not natural but imposed, and the state has an affirmative duty to undo it. It solves the collective action problem of hierarchy maintenance — no single actor can dismantle caste alone; the state must intervene against decentralized discrimination, structural accumulation, and elite capture.
% TRANSFER_FUNCTION: Moves resources, opportunities, and political power from majority groups and general taxpayers to subordinated groups via: race-conscious admissions and hiring (affirmative action), disparate impact liability (employment, housing, lending), voting rights preclearance and districting, school desegregation orders, Title IX enforcement, disability accommodations, language access mandates. The transfer is not merely monetary — it redistributes positional goods (elite university seats, legislative representation, occupational access) and imposes compliance costs on institutions.
% ABSENT_VOICES: Formal equality advocates (colorblind constitutionalists) are structurally excluded from the anti-caste reading's internal logic — their objection that race-conscious remedies constitute a new caste is treated as illegitimate within the reading's framework. Also absent: international human rights bodies that frame equality in dignity terms rather than anti-caste terms; libertarian critics who reject state corrective action entirely; conservative religious groups who view gender hierarchy as natural law. These voices exist in the external contest but are not seated within the constraint's operation.
% DISAPPEARANCE_RATIONALE: If the anti-caste reading vanished overnight, the entire architecture of race-conscious and gender-conscious remedial law would lose its constitutional foundation. Affirmative action, disparate impact doctrine, voting rights preclearance, school desegregation orders, Title IX's substantive reach, and disability accommodation mandates would be relegated to legislative grace — vulnerable to repeal, defunding, or narrow construction. Racial, gender, and status hierarchy would re-solidify without constitutional check; the state would revert to formal equality (prohibiting explicit classification only), leaving structural inequality untouched. The world rearranges because the constraint is the constitutional legitimator of the remedial state.
% FOUNDING_PROBLEM: The post-Civil War need to dismantle the slave caste system — the Black Codes, sharecropping, convict leasing, and the entire architecture of racial subordination that persisted after formal emancipation. The Fourteenth Amendment was ratified to empower the federal government to dismantle this caste system. The anti-caste reading extends this founding problem to gender hierarchy (coverture, exclusion from professions, reproductive control) and status hierarchies (immigrant caste, religious minority subordination, disability exclusion) — arguing the same structural logic applies: hierarchy is imposed, not natural, and the state must actively dismantle it.
% FOUNDING_PROBLEM_CORROBORATION: The Reconstruction Congress's debates and the Freedmen's Bureau legislation corroborate the anti-caste founding problem for race (Foner, 1988; Ackerman, 2014). The civil rights movement's demand for 'affirmative action' (not just non-discrimination) corroborates the extension to structural inequality (King, 1967; Rustin, 1965). Critical race theory (Bell, Crenshaw, Delgado) and feminist legal theory (MacKinnon, Williams) provide the analytical corroboration that caste extends beyond race. International human rights law (ICERD, CEDAW, CRPD) corroborates from outside the U.S. constitutional tradition that equality requires dismantling structural hierarchy. No corroboration exists from the formal equality tradition — they deny the founding problem persists.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__anti_caste_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__anti_caste_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__anti_caste_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the anti-caste mandate legitimates race-conscious and gender-conscious redistribution of elite positional goods (university admissions, government contracts, legislative seats) and imposes compliance costs on all institutions — this is extraction from the majority/taxpayer seat. Suppression (0.68) is high because the constraint cannot survive without judicial enforcement; political majorities consistently resist remedial programs, and when courts stop suppressing resistance (e.g., ending desegregation orders), the constraint's operation decays rapidly. Theater ratio (0.42) is moderate: the diversity rationale in Grutter was partly performative (legitimating extraction under a coordination banner), but the material transfers are real. Accessibility collapse (0.55) is moderate — alternatives (colorblind formal equality) remain cognitively available and politically potent, unlike a true mountain. Resistance (0.75) is high because the constraint faces organized, well-resourced, institutionally embedded opposition (Federalist Society, state legislatures, Supreme Court supermajority).
 *
 * PERSPECTIVAL GAP:
 *   From the subordinated group seat (identity_locked, organized), the constraint is a Mountain — the only thing preventing caste re-entrenchment; extraction is experienced as subsidy (d near 0). From the taxpayer/majority seat (constrained/mobile, powerful), the constraint is a Snare — extraction without consent, suppression of alternatives (colorblind formal equality), no exit (d near 1). From the federal court seat (analytical, institutional), the constraint is a Tangled Rope — genuine coordination mandate with extractive implementation, requiring constant doctrinal calibration. The engine computes this per-seat divergence from the structural data; the authored claim (tangled_rope) is the analytical seat's view.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinated groups (racial, gender, status) are beneficiaries — they receive the transfers (admissions, contracts, voting power, accommodations) and their exit from caste is identity-locked (ascribed, inescapable). General taxpayers are payers — they fund the remedial state with constrained exit (territorial tax obligation). Majority group members in zero-sum contexts are payers with mobile exit (they can opt into private alternatives, move, litigate). Federal courts are agenda_setters with analytical exit — they interpret but don't bear costs. State/local governments are dual agenda_setter/payer — they design remedies but bear compliance costs. Formal equality advocates are excluded with arbitrage exit — they contest from outside with full institutional mobility. The directionality derivation follows: beneficiaries have low d (subsidy), payers have high d (extraction), agenda_setters have analytical d.
 *
 * MANDATROPHY ANALYSIS:
 *   The anti-caste reading avoids mislabeling coordination as pure extraction because the coordination function (dismantling caste) is genuine and historically grounded — the slave caste, gender coverture, and status hierarchies were/are real collective action problems no individual could solve. It avoids mislabeling extraction as pure coordination because the remedial programs' costs are real, concentrated on non-beneficiaries, and the constraint's persistence depends on suppressing the formal equality alternative (which would eliminate the extraction). The mandatrophy risk is that the founding problem (slave caste) is arguably dead, but the arrangement persists and expands to new hierarchies — the founding_problem_status is contested, and the disappearance_verdict (world_rearranges) confirms the constraint still performs work. The reading does not resolve mandatrophy; it lives in the tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    caste_coherence,
    'Is ''caste'' a coherent structural category that unifies race, gender, and status hierarchies, or is the anti-caste reading an analogical overextension from the racial case?',
    'Comparative historical analysis: do gender hierarchy, sexual orientation hierarchy, disability hierarchy, and immigrant hierarchy operate through the same mechanisms as racial caste (hereditary, endogamous, occupationally fixed, ideologically justified)? Or do they differ in ways that require distinct remedial logics?',
    'If caste is not a coherent category, the anti-caste reading''s extension beyond race is analogical rather than structural — the constraint would be multiple constraints (racial caste dismantling, gender hierarchy dismantling, etc.) each with its own ε. This would trigger ε-invariance decomposition into separate constraint stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_coherence, conceptual, 'Whether the anti-caste reading''s unification of multiple hierarchies under one constraint is structurally valid or analogical overextension.').

omega_variable(
    remedial_extraction_necessity,
    'Is the high extractiveness of remedial programs (0.72) structurally necessary for dismantling caste, or does it reflect political capture and programmatic bloat?',
    'Counterfactual analysis: in jurisdictions where remedial programs were eliminated (e.g., California post-Prop 209, Michigan post-Schuette), did caste indicators (wealth gaps, representation, health outcomes) worsen at a rate consistent with the programs having been necessary? Or did alternative mechanisms (class-based affirmative action, universal programs) achieve similar results with lower extraction?',
    'If extraction is not necessary, the constraint''s high ε is not a feature of the coordination function but of political/institutional capture — the constraint would be more snare-like than tangled_rope. If extraction is necessary, the tangled_rope classification holds: genuine coordination requires asymmetric extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedial_extraction_necessity, empirical, 'Whether the measured extractiveness is the price of coordination or evidence of capture.').

omega_variable(
    foreclosure_relation,
    'Does the anti-caste reading''s core premise (hierarchy requires affirmative dismantling) logically foreclose the formal equality reading''s core premise (classification is the only evil), or do they coexist as competing frameworks?',
    'Doctrinal analysis: can a single legal system simultaneously hold that (a) the state must dismantle caste through race-conscious remedies AND (b) the state may never classify by race? The Supreme Court''s trajectory (Bakke → Grutter → SFFA) suggests the premises are treated as mutually exclusive in practice, but scholars argue for ''both/and'' frameworks (e.g., race-conscious means for race-neutral ends).',
    'If forecloses: the readings cannot coexist in one framework; the kernel is irreducibly contested and the engine should treat them as alternative constraints with no shared commitment. If coexists_with: both readings remain live in public discourse, and the engine should model their competition as institutional contestation. If influences: anti-caste reading creates downstream pressure on formal equality reading (e.g., strict scrutiny as compromise) without foreclosing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_relation, conceptual, 'Structural relationship between the two kernel readings.').

omega_variable(
    internalized_suppression,
    'Is the suppression experienced by subordinated groups under the anti-caste constraint structural (external barriers) or internalized (identity-fused acceptance of hierarchy), and does the constraint itself produce internalized suppression?',
    'Post-remedial-program suppression trajectory: if a remedial program ends (e.g., affirmative action ban), does the subordinated group''s suppression decrease (structural barrier removed) or persist/increase (internalized hierarchy reinforced by the program''s removal)? Longitudinal studies of stereotype threat, impostor phenomenon, and racial identity development in post-affirmative-action contexts.',
    'If suppression is substantially internalized, the constraint''s effective suppression is higher than the structural measure (0.68) suggests — the target carries the suppression with them. This would also affect the identity_locked exit option analysis: identity lock may be partly produced by the constraint''s own framing of the group as ''subordinated.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression, empirical, 'Structural vs. internalized suppression mechanism for subordinated groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 0, 156).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_tr_t0, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0, 0.85).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_tr_t12, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 12, 0.95).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_tr_t54, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 54, 0.9).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_tr_t86, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 86, 0.45).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_tr_t98, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 98, 0.3).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_tr_t110, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 110, 0.38).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_tr_t130, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 130, 0.4).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_tr_t156, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 156, 0.42).

% Extraction over time
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_be_t0, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_be_t12, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 12, 0.05).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_be_t54, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 54, 0.1).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_be_t86, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 86, 0.35).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_be_t98, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 98, 0.58).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_be_t110, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 110, 0.62).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_be_t130, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 130, 0.68).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_be_t156, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 156, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_su_t0, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_su_t12, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 12, 0.1).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_su_t54, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 54, 0.15).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_su_t86, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 86, 0.6).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_su_t98, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 98, 0.7).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_su_t110, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 110, 0.65).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_su_t130, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 130, 0.68).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_su_t156, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 156, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__anti_caste_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fourteenth_amendment_equal_protection__anti_caste_reading, 0.12).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection__formal_equality_reading).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, voting_rights_act_preclearance).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, title_vii_disparate_impact).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, title_ix_enforcement).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, ada_reasonable_accommodation).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, school_desegregation_orders).

% DUAL FORMULATION NOTE:
% This constraint and formal_equality_reading form a constraint family decomposing the kernel 'fourteenth_amendment_equal_protection'. The anti-caste reading has high ε (0.72) for remedial programs; the formal equality reading has near-zero ε (it only forbids explicit classification). They share the same constitutional text but instantiate different constraints with different beneficiary/victim structures, different enforcement needs, and different types (tangled_rope vs. rope/mountain). Linked via affects_constraints in both directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__anti_caste_reading, institutional, 0.15).
constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__anti_caste_reading, powerful, 0.85).
constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__anti_caste_reading, organized, 0.2).
constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__anti_caste_reading, moderate, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
