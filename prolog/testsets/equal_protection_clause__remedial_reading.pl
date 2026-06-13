% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__remedial_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equal_protection_clause__remedial_reading
 *   human_readable: Equal Protection Remedial Reading: Race-Conscious Remediation for Substantive Equality
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   The remedial reading of the equal protection clause holds that
 *   constitutional equality requires race-conscious remediation of historical
 *   group subordination to achieve substantive (not merely formal) equality.
 *   This constraint instantiates ONE reading of the contested equal
 *   protection kernel. The other readings — colorblind and diversity — are
 *   separate constraints with different ε values, different
 *   beneficiary/victim structures, and different classifications. This story
 *   focuses on the remedial reading's structural claim: race-consciousness is
 *   mandated (not merely permitted) to correct historical injustice. The
 *   remedial reading is classified as tangled_rope because it combines
 *   genuine coordination (correction of historical injustice, achievement of
 *   substantive equality) with asymmetric extraction (individual members of
 *   non-preferred groups bear the cost of group-level remediation). The
 *   constraint requires active enforcement (court validation, institutional
 *   implementation, legislative protection or resistance) and carries a
 *   sunset clause (remediation is justified as temporary, ending when
 *   substantive equality is achieved — though the parties contest fiercely
 *   when that endpoint is reached).
 *
 * KEY AGENTS:
 *   - historically_subordinated_racial_groups: Collective beneficiary of remedial mandate; group-level agents organizing around shared historical subordination and remedial claim.
 *   - individual_applicants_excluded_by_remedial_preferences: Direct victims; bear specific excluded-opportunity cost; their exit options are constrained by the constraint itself (other institutions may apply similar preferences).
 *   - educational_institutions: Agenda-setters that adopt and defend race-conscious policies; carry litigation risk and reputational cost.
 *   - federal_courts: Institutional agenda-setters holding authority to validate or foreclose the remedial reading through constitutional interpretation.
 *   - colorblind_equal_protection_advocates: Excluded stakeholders; their core premise (colorblindness) contradicts the remedial reading's core claim (race-consciousness); not accidentally excluded but structurally excluded by the reading's logic.
 *   - civil_rights_advocates: Organized beneficiaries who defend and advance the remedial reading; their institutional legitimacy depends on the reading's operative authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.62).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.58).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Equal Protection Remedial Reading: Race-Conscious Remediation for Substantive Equality").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).
narrative_ontology:has_sunset_clause(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, '058312d1-9e17-491d-b744-4ded5a2b1bc2').
narrative_ontology:cs_kernel_codification('058312d1-9e17-491d-b744-4ded5a2b1bc2', fixed_text).
narrative_ontology:cs_authority_grounding('058312d1-9e17-491d-b744-4ded5a2b1bc2', lineage).
narrative_ontology:cs_interpretation_layer_present('058312d1-9e17-491d-b744-4ded5a2b1bc2').
narrative_ontology:cs_reading_relation('058312d1-9e17-491d-b744-4ded5a2b1bc2', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('058312d1-9e17-491d-b744-4ded5a2b1bc2', equal_protection_clause__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('058312d1-9e17-491d-b744-4ded5a2b1bc2', foundational, race_consciousness_constitutionally_mandated).
narrative_ontology:cs_axiom_status(race_consciousness_constitutionally_mandated, overridden).
narrative_ontology:cs_axiom_grounding('058312d1-9e17-491d-b744-4ded5a2b1bc2', race_consciousness_constitutionally_mandated, deontological).
narrative_ontology:cs_axiom('058312d1-9e17-491d-b744-4ded5a2b1bc2', foundational, substantive_equality_requires_active_remediation).
narrative_ontology:cs_axiom_status(substantive_equality_requires_active_remediation, overridden).
narrative_ontology:cs_axiom_grounding('058312d1-9e17-491d-b744-4ded5a2b1bc2', substantive_equality_requires_active_remediation, empirically_contingent).
narrative_ontology:cs_reference_frame('058312d1-9e17-491d-b744-4ded5a2b1bc2', constitutional_color_consciousness_mandate).
narrative_ontology:cs_drift_state('058312d1-9e17-491d-b744-4ded5a2b1bc2', supreme_court_foreclosure_2023, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('058312d1-9e17-491d-b744-4ded5a2b1bc2', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, beneficiary_students_from_preferred_groups).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, individual_members_non_preferred_groups).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, individual_applicants_excluded_by_remedial_preferences).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__remedial_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(equal_protection_clause__remedial_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end) because the constraint mandates that race-conscious preferences be applied, which systematically withholds opportunities from non-preferred individuals — this is extraction in the narrow sense (A gets what B would have received, justified by group-level remediation rather than A's individual merit or need). The measurement series shows rising extractiveness from 1971 (when affirmative action policies began scaling) through 2023, reflecting the constraint's increasing institutional scope and legal contestation — more institutions adopted policies, more applicants faced exclusion, more litigation forced the constraint into explicit legal terms. Suppression is moderately high (0.58) because the constraint's persistence depends on courts, institutions, and legislatures actively suppressing the colorblind objection — they must reject colorblindness as a valid reading and defend race-consciousness as constitutionally required. Theater is moderate-low (0.28): the constraint's coordination function (correcting historical injustice) is real, but a growing share of enforcement activity in the 2010s–2020s focused on defending the reading against colorblind challenges rather than advancing remediation itself — the constraint became increasingly theatrical as courts and legislatures debated its legitimacy rather than implementing it. Accessibility_collapse (0.71) reflects the fact that once the remedial reading is understood, alternatives (colorblind selection, diversity-only framing) appear structurally distinct, and the parties occupy incommensurable positions — the collapse is high because the reading forecloses the colorblind alternative (you cannot hold both in a single constitutional framework). Resistance is very high (0.79) because the colorblind reading mobilized sustained legal and political opposition; the remedial reading never achieved consensus and faced escalating court challenges — this is a constraint that meets significant organized resistance.
 *
 * PERSPECTIVAL GAP:
 *   The remedial reading's beneficiary seats and victim seats experience profoundly different constraint structures. From the position of historically subordinated groups, the constraint solves a coordination problem: without race-conscious remediation, formal equality perpetuates inherited advantage, and the group remains locked in structural subordination. The constraint enables the group to correct that injustice and access opportunity. From the position of an individual applicant excluded by remedial preferences, the constraint is pure extraction: they bear a cost (lost opportunity) justified by the group-level remediation of a group to which they may not belong, without having personally caused historical subordination. Courts sit in the agenda-setter position: they hold authority to decide which reading (remedial, colorblind, diversity) governs. Their classification of the constraint depends on whether they accept the remedial reading's foundational claim (race-consciousness is mandated) or reject it (colorblindness is mandated). The engine computes per-seat classification by deriving directionality from beneficiary/victim structure and exit options — the beneficiary seats compute toward lower-extraction, lower-target directionality; the victim seats compute toward higher-extraction, higher-target directionality; the agenda-setter seats compute with the power to shift the entire constraint's operative frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary derivation: historically_subordinated_racial_groups and beneficiary_students_from_preferred_groups are listed as beneficiaries because the constraint's explicit purpose is to remedy historical subordination and to provide access to those groups. Victim derivation: individual_applicants_excluded_by_remedial_preferences are listed as victims because the constraint's operation systematically withholds opportunities from non-preferred applicants to fund the remedial transfer — they bear a direct, measurable cost. Directionality follows: beneficiaries compute toward low d (the constraint subsidizes them); victims compute toward high d (the constraint extracts from them). Exit options reinforce this: beneficiary students who lose access to remedial policies can exit to other institutions but face reduced opportunity (constrained exit); excluded applicants can exit to other institutions but may face the same remedial preferences elsewhere (spatially constrained exit — the constraint is national in scope, so local exit is limited). Colorblind advocates' exclusion is structural, not directional: they are not victims because they do not bear a cost from the constraint's operation — they bear a cost from the constraint's existence as a competing reading, which is different. Their exclusion from influence is enforced through legal and institutional authority (courts validate the remedial reading, institutions implement it), not through directionality. Power atoms: historically_subordinated_racial_groups hold organized power (collective mobilization, legal advocacy, institutional presence); individual applicants and colorblind advocates hold moderate or organized power depending on their institutional affiliation. Institutional actors (courts, educational institutions, legislatures) hold institutional power. This mix of power levels and exit options produces the tangled-rope classification: genuine coordination function (correcting historical injustice) + asymmetric extraction (victims bear direct cost) + active enforcement required + sunset clause.
 *
 * MANDATROPHY ANALYSIS:
 *   The remedial reading carries a mandate: to achieve substantive equality by remedying historical group subordination. The founding problem (centuries of slavery, segregation, and exclusion leaving persistent credential and wealth gaps) was supposed to be the measure of success — remediation continues until that historical injustice is corrected. However, the parties contest fiercely whether the founding problem is still live. Civil rights advocates argue persistent credential and representation gaps prove remediation is incomplete and the mandate is live. Colorblind advocates and the 2023 Supreme Court majority argue the problem was solved (or sufficient progress made) when legal discrimination ended, and mandated remediation now violates equal protection. The constraint's mandatrophy tension is explicit: it is justified by reference to a founding problem whose status is contested. The constraint claims to be temporary (sunset when remediation complete) but no agreed-upon endpoint exists. This is not a case of a constraint persisting when its founding problem is universally acknowledged as solved — it is a case where the constraint's persistence depends on the remedial reading's validation of the founding problem's continued salience, and that validation is exactly what colorblind reading denies. The measurement series shows the suppression_requirement rising (0.35 in 1971 to 0.58 in 2023) as opposition intensified, which is consistent with the constraint requiring more active enforcement to persist against colorblind challenges. The theater_ratio is moderate and stable (0.28 in 2023) — the constraint's coordination function (remedying historical injustice) remains genuine to its adherents, so theater does not dominate. But the constraint's classification as tangled_rope (not pure rope) reflects the asymmetric extraction inherent in race-conscious preferences: individual victims bear a real cost, and that cost must be justified by group-level remediation, not by their individual circumstance. Mandatrophy is resolved by transparency: the remedial reading's mandate is explicitly tied to a contested founding problem, and the constraint's continued operation depends on courts and institutions validating that problem as live and remediation as incomplete. This is structurally honest (the constraint names its mandate and its termination condition) but politically contested (the parties disagree about whether the conditions for sunset are met).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_completion_criteria,
    'When is historical group subordination sufficiently remedied to justify ending race-conscious remedial policies?',
    'Objective measures of substantive equality (credential distribution across groups, wealth gaps, institutional representation, economic outcomes) reaching specified thresholds, or procedural agreement among stakeholders on what ''remediation complete'' means.',
    'If thresholds are met, the remedial reading''s mandate is satisfied and the constraint''s sunset clause triggers. If thresholds are contested or if remediation is deemed infinitely incomplete (historical injustice cannot be fully undone), the constraint''s termination condition is never met and it persists indefinitely — shifting from a sunset constraint into a permanent one, which contradicts its foundational claim to be temporary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_completion_criteria, empirical, 'The constraint claims to be temporary, but its termination condition is undefined and contested.').

omega_variable(
    group_versus_individual_remediation,
    'Can historical group-level subordination be remedied through individual-level preferences applied to the current generation, or does group-level remedy require group-level compensation?',
    'Philosophical and legal analysis of what counts as sufficient remediation for historical injustice; empirical study of whether individual preferences for group members reduce persistent group-level inequality; consideration of alternative remedial mechanisms (group-level reparations, wealth transfers, institutional investment in historically excluded communities).',
    'If individual-level race-conscious preferences are sufficient to remedy group-level historical injustice, the remedial reading is structurally sound and the constraint is properly designed. If group-level remedy requires group-level compensation rather than individual-level selection, the constraint''s design is structurally misfitted to its mandate — it treats individuals as vessels for group remediation without addressing the group-level structural sources of inequality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(group_versus_individual_remediation, conceptual, 'Whether individual race-conscious preferences are a coherent remedy for group-level historical subordination.').

omega_variable(
    reading_contestation_and_kernel_instability,
    'Is the remedial reading a valid constitutional interpretation of the equal protection clause, or is the colorblind reading the correct interpretation?',
    'Supreme Court constitutional interpretation (binding on federal law, though contested in public discourse and state practice); scholarly consensus on the clause''s original meaning and living evolution; long-term institutional settlement on which reading prevails in enforcement.',
    'If the remedial reading is correct, the constraint is constitutionally mandated and should persist. If the colorblind reading is correct, the constraint violates equal protection and should be foreclosed. The Supreme Court''s 2023 Students for Fair Admissions decision moved toward the colorblind reading, foreclosing the remedial reading at the federal level — this is evidence that the remedial reading has lost its constitutional claim to authority, at least in the current institutional moment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contestation_and_kernel_instability, conceptual, 'The kernel is contested; the remedial reading competes with colorblind and diversity readings for constitutional authority. The contest is not abstract — it determines whether the constraint persists or is foreclosed.').

omega_variable(
    mismatch_theory_and_remedial_efficacy,
    'Do race-conscious remedial policies actually improve long-term outcomes for beneficiary students, or do they create ''mismatch'' (placing students in institutions where they underperform), undermining the remediation?',
    'Empirical study of outcomes (graduation rates, career earnings, professional advancement) for students admitted under remedial preferences versus colorblind selection; analysis of whether mismatch is real and significant or overstated.',
    'If remedial preferences improve long-term outcomes and reduce historical inequality gaps, the constraint''s coordination function is validated. If mismatch is substantial and preferences worsen outcomes for beneficiary students, the constraint''s design is self-defeating — it extracts from victims without achieving remediation for beneficiaries, making it closer to a snare than a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mismatch_theory_and_remedial_efficacy, empirical, 'The remedial reading assumes remedial preferences improve outcomes; if mismatch dominates, the constraint''s coordination function collapses.').

omega_variable(
    reading_kernel_contestation,
    'Which reading of the equal protection clause is correct: remedial, colorblind, or diversity?',
    'This omega documents the kernel-level contestation (OQ-83 Rule 2: route committer structure to omegas). The remedial reading competes with colorblind and diversity readings for constitutional authority. The Supreme Court''s 2023 decision (Students for Fair Admissions) is evidence that the remedial reading has lost institutional authority at the federal level, but it remains live in public discourse, lower courts (until full circuit alignment), and academic debate. The reading_relations and axioms in cs_structure document the structural relationships and foundational claims that distinguish this reading from siblings.',
    'If the remedial reading is foreclosed by courts (as the 2023 decision suggests), the constraint loses its constitutional basis and institutions lose legal authority to implement race-conscious remedial policies. The remedial reading would persist only as a normative claim (civil rights advocates'' position) without legal enforcement. The constraint would shift from operative legal rule to defeated advocacy position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contestation, conceptual, 'Kernel contestation: the remedial reading is one of three competing constitutional readings of equal protection. The Supreme Court''s 2023 decision is evidence of movement toward the colorblind reading, foreclosing the remedial reading''s claim to operative constitutional authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 1945, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1945, equal_protection_clause__remedial_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement(equa_tr_t1971, equal_protection_clause__remedial_reading, theater_ratio, 1971, 0.08).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_clause__remedial_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_clause__remedial_reading, theater_ratio, 2003, 0.21).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_clause__remedial_reading, theater_ratio, 2013, 0.26).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_clause__remedial_reading, theater_ratio, 2023, 0.28).

% Extraction over time
narrative_ontology:measurement(equa_be_t1945, equal_protection_clause__remedial_reading, base_extractiveness, 1945, 0.0).
narrative_ontology:measurement(equa_be_t1971, equal_protection_clause__remedial_reading, base_extractiveness, 1971, 0.38).
narrative_ontology:measurement(equa_be_t1990, equal_protection_clause__remedial_reading, base_extractiveness, 1990, 0.51).
narrative_ontology:measurement(equa_be_t2003, equal_protection_clause__remedial_reading, base_extractiveness, 2003, 0.58).
narrative_ontology:measurement(equa_be_t2013, equal_protection_clause__remedial_reading, base_extractiveness, 2013, 0.61).
narrative_ontology:measurement(equa_be_t2023, equal_protection_clause__remedial_reading, base_extractiveness, 2023, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1945, equal_protection_clause__remedial_reading, suppression_requirement, 1945, 0.0).
narrative_ontology:measurement(equa_su_t1971, equal_protection_clause__remedial_reading, suppression_requirement, 1971, 0.35).
narrative_ontology:measurement(equa_su_t1990, equal_protection_clause__remedial_reading, suppression_requirement, 1990, 0.42).
narrative_ontology:measurement(equa_su_t2003, equal_protection_clause__remedial_reading, suppression_requirement, 2003, 0.52).
narrative_ontology:measurement(equa_su_t2013, equal_protection_clause__remedial_reading, suppression_requirement, 2013, 0.56).
narrative_ontology:measurement(equa_su_t2023, equal_protection_clause__remedial_reading, suppression_requirement, 2023, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__remedial_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__remedial_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% The equal protection clause is contested among three structurally distinct readings, each instantiating a different constraint: remedial (this constraint), colorblind, and diversity. The remedial reading treats race-consciousness as constitutionally mandated to correct historical subordination; the colorblind reading treats all racial classifications as constitutionally forbidden; the diversity reading treats race-consciousness as permissible when serving compelling educational diversity interests. Each reading has a different ε, different beneficiary/victim structure, and different classification. They are linked as sibling readings of the same kernel. The remedial reading forecloses (within a single constitutional framework) the colorblind reading's core premise (constitutional colorblindness). It coexists with the diversity reading — both permit race-consciousness, but for different reasons (group remediation vs. educational diversity). See cs_structure.reading_relations for the typed relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_clause__remedial_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
