% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

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
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: Functional Accommodation Reading of War Powers Allocation
 *   domain: constitutional_law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   The functional accommodation reading holds that war powers allocation
 *   varies by operational context: imminent threats permit unilateral
 *   executive action while prolonged campaigns require congressional
 *   authorization. This reading emerged from early presidential practice
 *   (Jefferson's Barbary Wars, Polk's Mexican War) and was crystallized in
 *   the 1973 War Powers Resolution's 60/90-day framework. The constraint is
 *   structurally a tangled rope — it coordinates inter-branch interaction
 *   around a genuine problem (operationalizing constitutional silence) while
 *   extracting congressional authority into executive discretion through the
 *   'imminent threat' ambiguity zone. Both branches claim authority within
 *   the gray area; neither can fully displace the other without
 *   constitutional crisis.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.48).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.32).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "Functional Accommodation Reading of War Powers Allocation").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, 'c60e16b0-bb6e-44b2-821c-4b21d8e7db3f').
narrative_ontology:cs_kernel_codification('c60e16b0-bb6e-44b2-821c-4b21d8e7db3f', fixed_text).
narrative_ontology:cs_authority_grounding('c60e16b0-bb6e-44b2-821c-4b21d8e7db3f', lineage).
narrative_ontology:cs_interpretation_layer_present('c60e16b0-bb6e-44b2-821c-4b21d8e7db3f').
narrative_ontology:cs_reading_relation('c60e16b0-bb6e-44b2-821c-4b21d8e7db3f', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('c60e16b0-bb6e-44b2-821c-4b21d8e7db3f', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_axiom('c60e16b0-bb6e-44b2-821c-4b21d8e7db3f', foundational, constitutional_text_requires_contextual_balancing).
narrative_ontology:cs_axiom_status(constitutional_text_requires_contextual_balancing, holdable).
narrative_ontology:cs_axiom_grounding('c60e16b0-bb6e-44b2-821c-4b21d8e7db3f', constitutional_text_requires_contextual_balancing, conventional).
narrative_ontology:cs_axiom('c60e16b0-bb6e-44b2-821c-4b21d8e7db3f', foundational, operational_necessity_justifies_temporary_unilateral_action).
narrative_ontology:cs_axiom_status(operational_necessity_justifies_temporary_unilateral_action, holdable).
narrative_ontology:cs_axiom_grounding('c60e16b0-bb6e-44b2-821c-4b21d8e7db3f', operational_necessity_justifies_temporary_unilateral_action, instrumental).
narrative_ontology:cs_reference_frame('c60e16b0-bb6e-44b2-821c-4b21d8e7db3f', constitutional_text_as_flexible_framework).
narrative_ontology:cs_drift_state('c60e16b0-bb6e-44b2-821c-4b21d8e7db3f', post_911_aumf_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c60e16b0-bb6e-44b2-821c-4b21d8e7db3f', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, executive_branch_leadership).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, national_security_establishment).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congressional_war_powers).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, statutory_authorization_requirements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, president_commander_in_chief).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, national_security_council).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, military_command_structure).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congressional_leadership).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, military_command_structure).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, constitutional_flexibility_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, operational_necessity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directs military operations and determines whether a threat is 'imminent' enough for unilateral action. Claims inherent authority for immediate defense while accepting congressional role for sustained campaigns. The ambiguity zone allows operational discretion that institutional actors resist narrowing.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, president_commander_in_chief, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, president_commander_in_chief, beneficiary).

% Holds formal war declaration and authorization power but faces political costs of opposing 'imminent threat' actions. Attempts to reclaim authority through funding controls and reporting requirements (War Powers Resolution) but struggles to enforce against executive precedent. Bears institutional extraction when its constitutional role is displaced by functional accommodation.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, congressional_leadership, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, congressional_leadership, agenda_setter).

% Staffs the operational classification of threats as 'imminent' vs. 'prolonged.' Institutional identity fused to executive operational flexibility; career paths depend on maintaining the accommodation framework. Exit requires rejecting professional self-concept as national security professionals.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, national_security_council, beneficiary,
    powerful, biographical, identity_locked, global).

% Declines to adjudicate most war powers disputes as political questions. When forced to rule (e.g., detention cases), applies functional balancing rather than categorical rules. The accommodation reading creates doctrinal space for judicial avoidance while preserving review capacity for extreme cases.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, judicial_branch, observer,
    institutional, generational, analytical, national).

% Executes orders under both unilateral and authorized frameworks. Benefits from clear chains of command but bears operational risk when legal authority is contested. Professional ethos emphasizes civilian control while institutional interest favors operational autonomy.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, military_command_structure, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, military_command_structure, payer).

% Opposes both unilateral executive action and congressional rubber-stamping of prolonged campaigns. Structurally excluded from the accommodation framework — neither branch's institutional interest aligns with categorical constraint on war powers. Exit options limited to electoral pressure and litigation with low success rates.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, anti_war_civil_society, excluded,
    moderate, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable framework for military action across the spectrum from immediate defense to sustained warfare, avoiding both executive paralysis and congressional micromanagement of operations.
% TRANSFER_FUNCTION: Moves constitutional war authorization authority from Congress to the Executive in the 'imminent threat' zone; moves operational discretion and precedent-setting power to the Executive; moves political accountability costs to Congress for acquiescing to unilateral action.
% ABSENT_VOICES: Anti-war civil society, originalist constitutional scholars who reject functional balancing, and foreign populations affected by U.S. military action without their consent or representation. These voices would demand categorical rules (either congressional primacy or strict executive limitation) but are excluded from the inter-branch accommodation.
% DISAPPEARANCE_RATIONALE: If functional accommodation vanished, either congressional_primacy_reading would require explicit authorization for all non-defensive action (paralyzing rapid response) or inherent_executive_reading would remove all temporal limits on unilateral action (eliminating congressional war powers). The accommodation's collapse forces a binary choice neither branch currently accepts.
% FOUNDING_PROBLEM: Constitutional text grants Congress war declaration power and the President commander-in-chief authority without specifying how these interact across the full spectrum of military contingencies — from immediate attack to prolonged overseas campaigns. The founding problem is operationalizing this textual silence without constitutional amendment.
% FOUNDING_PROBLEM_CORROBORATION: The accommodation reading's founding problem is corroborated by the historical record of presidential military action without declarations (1801 Barbary Wars onward) and congressional acquiescence. However, congressional_primacy_reading proponents (e.g., War Powers Resolution sponsors, originalist scholars) attest the problem was misdiagnosed — the Constitution's allocation was clear and the 'silence' is a constructed ambiguity enabling executive aggrandizement.
narrative_ontology:disappearance_verdict(war_powers_allocation__functional_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__functional_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__functional_accommodation_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects the measurable transfer of war authorization authority from Congress to the Executive through precedent accumulation, moderated by the 1973 WPR's partial clawback and post-9/11 expansion. Suppression (0.32) is moderate — the constraint operates through precedent and political cost rather than overt coercion, but the 'political question' doctrine and standing barriers suppress judicial enforcement of congressional rights. Theater ratio (0.41) is elevated — WPR reporting requirements and consultations are frequently performative, with presidents treating the 60-day clock as advisory. Accessibility collapse (0.42) is partial: categorical alternatives (congressional primacy, inherent executive) remain intellectually available and politically advocated, but institutional inertia and precedent weight make them structurally difficult to instantiate. Resistance (0.58) is significant: congressional assertions (funding cuts, reporting demands), judicial pushback (Hamdi, Boumediene), and civil society opposition create sustained counter-pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the executive seat, the constraint is a rope — genuine coordination solving the constitutional silence problem. From the congressional seat, it is a snare — the coordination story covers systematic displacement of Article I authority. From the NSC seat, it is a scaffold — transitional accommodation that became permanent. From the civil society seat, it is a piton — degraded institutional theater maintaining the appearance of constitutional balance. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The President as agenda_setter/beneficiary sits at d ≈ 0.15 (subsidized by the constraint's operational flexibility). Congress as payer/agenda_setter sits at d ≈ 0.75 (bears extraction of its constitutional role but retains formal tools). NSC as identity_locked beneficiary sits at d ≈ 0.2 (institutional identity fused to accommodation framework). Military as beneficiary/payer sits at d ≈ 0.45 (mixed position). Judiciary as analytical observer sits at d ≈ 0.5. Anti-war civil society as excluded/trapped sits at d ≈ 0.9 (bears costs of unconstrained war with no structural voice).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (operationalizing constitutional silence) remains contested — the accommodation reading claims it is live (new threat types: cyber, drones, grey zone), while congressional primacy proponents argue it was solved by the Constitution's text and the 'problem' is executive non-compliance. The constraint persists because neither branch can force a categorical resolution without unacceptable risk: Congress cannot credibly threaten to defund imminent defense; the Executive cannot openly claim unlimited war power without triggering constitutional crisis. Mandatrophy is unresolved — the arrangement's function has mutated from 'operational necessity' to 'institutional equilibrium maintenance.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imminent_threat_definition_boundary,
    'Where does ''imminent threat'' end and ''prolonged campaign'' begin? Is there a principled boundary or is the zone inherently manipulable?',
    'Systematic coding of presidential unilateral actions with stated justifications; analysis of whether ''imminence'' criteria correlate with objective threat metrics or expand to cover political preferences.',
    'If the boundary is principled, the accommodation reading coordinates with bounded extraction. If inherently manipulable, the ambiguity zone is an extraction mechanism and the constraint is snare-adjacent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(imminent_threat_definition_boundary, conceptual, 'Whether the temporal/scope distinction has a stable structural referent or is a cover story for executive discretion.').

omega_variable(
    congressional_acquiescence_voluntariness,
    'Is congressional failure to constrain unilateral action voluntary (political preference) or structurally compelled (institutional incapacity)?',
    'Counterfactual analysis: when Congress has attempted to constrain (funding cutoffs, repeal of AUMFs), has the Executive complied? Structural analysis of collective action problems in congressional war powers enforcement.',
    'If voluntary, Congress is a beneficiary (avoiding accountability) not a victim — the constraint becomes rope-like. If structurally compelled, Congress is a genuine victim and the constraint is tangled_rope/snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_acquiescence_voluntariness, empirical, 'Whether congressional role displacement reflects preference or incapacity.').

omega_variable(
    kernel_reading_stability,
    'Does the functional accommodation reading have a stable core, or does it collapse into either congressional_primacy or inherent_executive under stress?',
    'Trace the reading''s doctrinal articulation across crises (Cuban Missile Crisis, Vietnam, post-9/11, Ukraine): does it maintain a distinct middle position or merge with a sibling?',
    'If the reading collapses under stress, it is not a stable constraint but a transitional mask — the real constraint is the sibling it collapses into.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Whether functional accommodation is a genuine third reading or an unstable compromise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wpa_far_tr_t1789, war_powers_allocation__functional_accommodation_reading, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(wpa_far_tr_t1801, war_powers_allocation__functional_accommodation_reading, theater_ratio, 1801, 0.12).
narrative_ontology:measurement(wpa_far_tr_t1846, war_powers_allocation__functional_accommodation_reading, theater_ratio, 1846, 0.18).
narrative_ontology:measurement(wpa_far_tr_t1898, war_powers_allocation__functional_accommodation_reading, theater_ratio, 1898, 0.25).
narrative_ontology:measurement(wpa_far_tr_t1950, war_powers_allocation__functional_accommodation_reading, theater_ratio, 1950, 0.38).
narrative_ontology:measurement(wpa_far_tr_t1973, war_powers_allocation__functional_accommodation_reading, theater_ratio, 1973, 0.32).
narrative_ontology:measurement(wpa_far_tr_t2001, war_powers_allocation__functional_accommodation_reading, theater_ratio, 2001, 0.47).
narrative_ontology:measurement(wpa_far_tr_t2024, war_powers_allocation__functional_accommodation_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(wpa_far_be_t1789, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 1789, 0.15).
narrative_ontology:measurement(wpa_far_be_t1801, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 1801, 0.22).
narrative_ontology:measurement(wpa_far_be_t1846, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 1846, 0.28).
narrative_ontology:measurement(wpa_far_be_t1898, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 1898, 0.35).
narrative_ontology:measurement(wpa_far_be_t1950, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 1950, 0.45).
narrative_ontology:measurement(wpa_far_be_t1973, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 1973, 0.42).
narrative_ontology:measurement(wpa_far_be_t2001, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement(wpa_far_be_t2024, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(wpa_far_su_t1789, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 1789, 0.1).
narrative_ontology:measurement(wpa_far_su_t1801, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 1801, 0.15).
narrative_ontology:measurement(wpa_far_su_t1846, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 1846, 0.2).
narrative_ontology:measurement(wpa_far_su_t1898, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 1898, 0.25).
narrative_ontology:measurement(wpa_far_su_t1950, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(wpa_far_su_t1973, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 1973, 0.4).
narrative_ontology:measurement(wpa_far_su_t2001, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 2001, 0.5).
narrative_ontology:measurement(wpa_far_su_t2024, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 2024, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__functional_accommodation_reading, 0.12).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_allocation__congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_allocation__inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, authorization_for_use_of_military_force_2001).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_resolution_1973).

% DUAL FORMULATION NOTE:
% This reading decomposes the 'war powers allocation' label into a structurally distinct claim: contextual balancing with an ambiguity zone. The congressional_primacy_reading claims categorical congressional authority (epsilon ≈ 0.1); the inherent_executive_reading claims categorical executive authority (epsilon ≈ 0.75). This reading's epsilon (0.48) measures the extraction of congressional authority through the ambiguity zone. All three are linked as a constraint family via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__functional_accommodation_reading, institutional, 0.75).
constraint_indexing:directionality_override(war_powers_allocation__functional_accommodation_reading, organized, 0.45).
constraint_indexing:directionality_override(war_powers_allocation__functional_accommodation_reading, moderate, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
