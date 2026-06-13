% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__colorblind_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equal_protection_clause__colorblind_reading
 *   human_readable: Equal Protection Colorblind Reading: Race-Neutral Individual Rights
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The colorblind reading of the Equal Protection Clause holds that the
 *   Constitution forbids all governmental racial classifications, treating
 *   individuals as rights-bearers independent of group membership. This
 *   reading presents itself as deriving from the text's plain meaning and the
 *   principle of formal equality. The claim/metric gap is deliberate and
 *   diagnostically essential: the reading is CLAIMED as a mountain (natural,
 *   inevitable, necessary), while the authored metrics reflect low but
 *   measurable extractiveness (0.08) and moderate theatrical performance
 *   (0.18). This gap enables false-summit detection: identifiable
 *   institutional actors benefit from the colorblind framing (federal
 *   executive branches that want racial policy flexibility, dominant-group
 *   narratives that prefer race-invisibility, formal rule administrations
 *   that minimize discretion claims). The constraint story models the reading
 *   as its advocates claim it—a natural law principle—while measuring whether
 *   the metrics are consistent with that claim or reveal constructed doctrine
 *   benefiting specific seats.
 *
 * KEY AGENTS:
 *   - All individuals (regardless of race): claimed as the sole beneficiary; rights-bearers whose equal protection is the reading's defined purpose
 *   - Race-neutral state authority: institutional beneficiary (benefits from formal rule application, no discretion claims)
 *   - Groups targeted by remedial policies: would lose access to group-remediation mechanisms if colorblind reading is enforced; excluded from beneficiary set by the reading's framing
 *   - Judicial interpreters adopting colorblind reading: institutional agents carrying the constraint forward; alternative readings occupied by dissenting judges
 *   - Historical subordinated groups: not named as agents but implicit in the contrast with remedial/diversity readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__colorblind_reading, 0.08).
domain_priors:suppression_score(equal_protection_clause__colorblind_reading, 0.12).
domain_priors:theater_ratio(equal_protection_clause__colorblind_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__colorblind_reading, mountain).
narrative_ontology:human_readable(equal_protection_clause__colorblind_reading, "Equal Protection Colorblind Reading: Race-Neutral Individual Rights").
narrative_ontology:topic_domain(equal_protection_clause__colorblind_reading, "constitutional_law/political_philosophy").

domain_priors:emerges_naturally(equal_protection_clause__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__colorblind_reading, '38d92496-8ec7-40cd-8faf-ccf4bd0c2a9c').
narrative_ontology:cs_kernel_codification('38d92496-8ec7-40cd-8faf-ccf4bd0c2a9c', fixed_text).
narrative_ontology:cs_authority_grounding('38d92496-8ec7-40cd-8faf-ccf4bd0c2a9c', lineage).
narrative_ontology:cs_interpretation_layer_present('38d92496-8ec7-40cd-8faf-ccf4bd0c2a9c').
narrative_ontology:cs_reading_relation('38d92496-8ec7-40cd-8faf-ccf4bd0c2a9c', equal_protection_clause__remedial_reading, coexists_with).
narrative_ontology:cs_reading_relation('38d92496-8ec7-40cd-8faf-ccf4bd0c2a9c', equal_protection_clause__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('38d92496-8ec7-40cd-8faf-ccf4bd0c2a9c', foundational, race_never_legitimate_government_purpose).
narrative_ontology:cs_axiom_status(race_never_legitimate_government_purpose, holdable).
narrative_ontology:cs_axiom_grounding('38d92496-8ec7-40cd-8faf-ccf4bd0c2a9c', race_never_legitimate_government_purpose, deontological).
narrative_ontology:cs_axiom('38d92496-8ec7-40cd-8faf-ccf4bd0c2a9c', foundational, individual_rights_trump_group_remediation).
narrative_ontology:cs_axiom_status(individual_rights_trump_group_remediation, holdable).
narrative_ontology:cs_axiom_grounding('38d92496-8ec7-40cd-8faf-ccf4bd0c2a9c', individual_rights_trump_group_remediation, deontological).
narrative_ontology:cs_reference_frame('38d92496-8ec7-40cd-8faf-ccf4bd0c2a9c', race_neutral_equal_protection).
narrative_ontology:cs_drift_state('38d92496-8ec7-40cd-8faf-ccf4bd0c2a9c', contemporary_post_2023, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('38d92496-8ec7-40cd-8faf-ccf4bd0c2a9c', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(equal_protection_clause__colorblind_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, all_individuals_regardless_of_race).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, race_neutral_state_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, all_individuals_qua_individuals).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, federal_executive_and_judicial_authority).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, individuals_in_subordinated_groups).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, historical_remedial_advocates).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, individuals_in_subordinated_groups).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, individual_rights_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, colorblindness_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The colorblind reading posits all individuals as rights-bearers protected from racial classification by government, irrespective of their own race or group history. They are the formal beneficiaries of the non-discrimination principle. However, this framing excludes recognition of group-level harms and group-level remedies, meaning individuals within historically subordinated groups lose access to a mechanism (race-conscious remediation) that the remedial reading would provide.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, all_individuals_qua_individuals, beneficiary,
    powerless, civilizational, analytical, universal).

% Institutional seats that adopt and enforce the colorblind reading. They benefit from the rule's clarity (no discretionary race-consciousness claims), the suppression of alternative policy framings (remedial, diversity), and the authority to declare race-consciousness categorically illegitimate. The institutional beneficiary is distinct from the individual beneficiary—the authority benefits from the rule's enforceability and from the narrowing of its own obligation to address historical group harms.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, federal_executive_and_judicial_authority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__colorblind_reading, federal_executive_and_judicial_authority, beneficiary).

% Institutional and scholarly advocates of the remedial reading, which holds that equal protection requires race-conscious remediation of historical subordination. They are excluded from the agenda-setting authority in colorblind courts and bear the cost of policy space closure: race-conscious remedial policies are declared illegitimate by the colorblind reading, leaving them unable to advocate for group-targeted remediation without challenging the equal protection doctrine itself.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, historical_remedial_advocates, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__colorblind_reading, historical_remedial_advocates, excluded).

% Institutions and actors advocating the diversity reading, which holds that equal protection permits race-conscious admissions policies serving compelling educational diversity. They are excluded from agenda-setting when courts adopt colorblindness and bear the cost of remedy foreclosure: affirmative action policies are invalidated, educational institutions lose a mechanism for inclusive enrollment, and the diversity reading's framing (group diversity benefits all students) is delegitimized.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, diversity_advocates_educational_context, excluded,
    moderate, generational, constrained, national).

% Individuals whose group histories include subordination. The colorblind reading benefits them as individuals (protection from race-conscious harm) but excludes them from a mechanism (group remediation, diversity consideration) that the remedial and diversity readings would provide. The constraint creates an asymmetry: they receive individual-level protection but are barred from group-level remedies.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, individuals_in_subordinated_groups, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__colorblind_reading, individuals_in_subordinated_groups, payer).

% Scholars and jurists who contest the colorblind reading, arguing for remedial or diversity framings. They occupy the analytical seat, documenting how the colorblind reading is constructed (not natural), benefits institutional race-neutrality narratives, and forecloses alternatives. Their scholarship feeds the theater ratio—the rhetorical work of defending the colorblind frame against competing interpretations.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, constitutional_scholars_dissenting, observer,
    moderate, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a formal rule preventing government from classifying individuals by race, thereby ensuring equal treatment and preventing state-sanctioned racial discrimination. The coordination problem solved is: how to ensure government does not use race as an organizing principle for distributing rights, benefits, or burdens.
% TRANSFER_FUNCTION: Transfers legitimacy and institutional authority from race-conscious policy (remedial, diversity-based) to race-neutral policy (colorblind). Individuals in historically dominant groups retain the option of race-conscious social advantage through market mechanisms; individuals in historically subordinated groups lose access to remedial policies. The constraint moves policy space from race-conscious to race-neutral framings.
% ABSENT_VOICES: Groups historically subordinated whose interests would be served by remedial or diversity readings. Dissenting judges and civil rights scholars who contest colorblindness as the correct reading. Institutional actors committed to race-conscious remediation (historically Black colleges, affirmative action administrators, civil rights enforcement agencies) are excluded from agenda-setting in colorblind courts and bear costs.
% DISAPPEARANCE_RATIONALE: Colorblind advocates argue the world rearranges if colorblindness disappears—without the constraint, race-conscious discrimination would resume and equal protection would be meaningless. Remedial and diversity advocates argue the world rearranges differently—without colorblindness, race-conscious remediation would resume and historical harms could be addressed. The disappearance verdict is contested because the three readings presuppose different worlds.
% FOUNDING_PROBLEM: The Fourteenth Amendment was adopted to ensure that government does not classify individuals by race to deny them rights or benefits—to establish that race is never a legitimate government purpose. The colorblind reading holds this problem as founding: government must be forbidden from using race at all.
% FOUNDING_PROBLEM_CORROBORATION: The colorblind reading is corroborated by formal textualists and originalists (e.g., Justice Scalia) who argue the Fourteenth Amendment's text forbids all racial classifications. However, the founding problem's interpretation is contested by remedial advocates who point to the same Amendment's enforcement clause (Section 5) as textual evidence of race-conscious remedial intent. Historical scholarship (Foner on Reconstruction, Gross on the original understanding) documents that the Amendment's drafters contemplated both race-neutral equal protection AND race-conscious enforcement, undermining the claim that colorblindness is the natural or only reading. Corroboration from outside the colorblind beneficiary set is mixed: civil rights scholars dispute the reading; originalists support it; institutional actors enforcing colorblindness naturally affirm it.
narrative_ontology:disappearance_verdict(equal_protection_clause__colorblind_reading, contested).
narrative_ontology:founding_problem_status(equal_protection_clause__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__colorblind_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(equal_protection_clause__colorblind_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__colorblind_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, ExtMetricName, E),
    domain_priors:suppression_score(equal_protection_clause__colorblind_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(equal_protection_clause__colorblind_reading),
    narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(equal_protection_clause__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.08) because the reading applies a formal rule (race classifications forbidden) uniformly and without discretion—the surface operation is rule-bound, not extractive. However, it is not zero because the rule's beneficiary structure (individuals over groups) creates an asymmetry: individuals who would benefit from group remediation are excluded from the reading's protected class. Suppression is modest (0.12) because the colorblind doctrine's authority depends partly on the legitimacy narrative (race-neutrality is axiomatically good) and partly on institutional suppression of alternative interpretations. Theater ratio is low-moderate (0.18) because most judicial application of the rule is genuine rule-following, but a portion (scholarly discourse, dissenting opinions) documents how the rule's reach is rhetorically deployed to block remedial policies. The measurement series show a slight upward drift in extractiveness and theater ratio from t=0 to t=60, reflecting the doctrine's increasing institutional consolidation and the rising rhetorical work (dissenting opinions, scholarly critique) needed to maintain the colorblind frame as the only legitimate reading.
 *
 * PERSPECTIVAL GAP:
 *   The institutional seat adopting colorblindness (Supreme Court majority, federal executive) and the seat advocating remedial/diversity readings (dissenting justices, civil rights advocates) compute differently. From the colorblind seat, the constraint is a natural principle of equal treatment—the engine computes it as a mountain, d near beneficiary. From the remedial seat, the same constraint is an institutional blocking mechanism preventing group remediation—the engine would compute the same facts as a tangled_rope or snare protecting institutional race-neutrality narratives. The perspectival gap is the point of generating this story alongside its siblings: each reading should compute differently from different seats because the readings instantiate different constraint structures.
 *
 * DIRECTIONALITY LOGIC:
 *   The colorblind reading benefits individuals qua individuals—all individuals are formal right-bearers in the doctrine's frame. However, declared beneficiaries here are 'all_individuals_regardless_of_race' (universal, unextractable) and 'race_neutral_state_authority' (institutional). The state authority benefits from the rule's clarity and from the suppression of race-conscious policy alternatives. The reading treats any individual subjected to a race-conscious policy as a victim (hence 'any race-conscious policy creates individual victims'). However, this frames the remedial reading's beneficiaries (historically subordinated groups) as victims of colorblindness. The directionality is asymmetric: the reading benefits the state apparatus (d near beneficiary, ~0.2) and individuals claimed as universal beneficiaries; it harms the remedial reading's institutional advocates (who lose policy space) and the groups they would serve.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading shows no mandatrophy. The founding problem (ensuring individuals are protected from governmental racial discrimination) remains live and contested; the constraint persists because the colorblind reading's institutional authority maintains it, not from inertia. The theater ratio's modest level and slow rise indicates the rule is mostly functional (genuine equal-protection application) with some rhetorical work (defending the colorblind frame against remedial/diversity challenges). No accumulated extraction indicates piton dynamics. The constraint is actively defended and argued because multiple institutional factions contest whether colorblindness is the correct equal protection reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_reading,
    'Is the colorblind reading a natural law interpretation of the Equal Protection Clause derived from the text''s intrinsic meaning, or a constructed legal reading that benefits institutional actors who prefer formal neutrality?',
    'Historical analysis of judicial rationales pre-dating the colorblind framing (Warren Court to Rehnquist Court); comparison of textualist derivations vs. policy-motivated selections of precedent; identification of institutional beneficiaries from the adoption of colorblind doctrine in particular historical moments.',
    'If natural, the reading should compute as a genuine mountain with negligible extraction. If constructed, FSM would reclassify to tangled_rope or snare, identifying institutional beneficiaries (color-conscious state actors, dominant-group insulation narratives) and victims (groups targeted by race-conscious remediation, who lose a mechanism for addressing historical subordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, conceptual, 'Whether colorblind reading is an inevitable derivation or a specific framing choice that benefits identifiable actors.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the constraint''s measured suppression (0.12) purely structural—external barriers to adopting race-conscious policy—or partially internalized—the doctrine''s legitimacy narrative causing institutional actors to internalize race-neutrality as a value, making the suppression persistent even if external barriers were removed?',
    'Post-removal counterfactual: if legal permission for race-conscious policy were suddenly granted (hypothetically overruling the colorblind reading), would institutional actors immediately adopt race-conscious policies, or would continued adherence to colorblindness suggest internalized legitimacy?',
    'If purely structural, the suppression metric stands as authored. If internalized, the effective suppression is higher than measured—the constraint persists through internalized belief in race-neutrality rather than external force. This feeds into whether the reading is truly a natural law or a constructed doctrine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Structural vs. internalized suppression of race-conscious alternatives.').

omega_variable(
    victims_identification_ambiguity,
    'Who are the victims of the colorblind reading—individuals subjected to race-conscious policies that the reading forbids, or groups historically subordinated who lose access to group-remediation mechanisms?',
    'Clarification of the reading''s own beneficiary/victim frame: the colorblind reading posits individuals (not groups) as rights-bearers, so ''individuals subjected to race-conscious policy'' are the natural victims in this frame. However, if race-conscious remediation addresses structural group subordination, the victims of colorblindness would be subordinated groups barred from remedy. This is a frame-dependent classification, not an empirical question.',
    'This omega documents the reading''s internal logic: colorblindness benefits all individuals qua individuals, while harming individuals who would benefit from group-remediation. The harmonization is impossible within a single rights-bearer frame; different readings place the victim boundary differently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victims_identification_ambiguity, conceptual, 'The reading''s internal beneficiary/victim frame (individuals as sole units) vs. group-based alternative frames.').

omega_variable(
    kernel_reading_contest_structure,
    'What is the relationship between this reading (colorblind) and its sibling readings (remedial, diversity)? Do they coexist as live scholarly/judicial positions, or does one''s adoption logically foreclose the others?',
    'Examination of contemporary constitutional debate: U.S. Supreme Court decisions (2003–2023) show coexistence of all three readings held by different judicial factions, with none logically foreclosing the others—they are competing interpretive choices. However, within a single court''s declared framework, a majority''s colorblind reading does foreclosure effects on dissenting remedial/diversity readings by refusing to recognize their legitimacy. The relationship is ''coexists_with'' (different factions), not ''forecloses'' (one framework).',
    'This documents the kernel context: the equal protection clause is a formalized, fixed text that can sustain multiple readings. Each reading is live in contemporary judicial and scholarly discourse; adoption of colorblind reading by a majority does not logically eliminate the remedial or diversity readings, only shifts which reading the institutional authority enforces.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Inter-reading logical relationships within the equal protection kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__colorblind_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_clause__colorblind_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(equa_tr_t0, observed).
narrative_ontology:measurement(equa_tr_t20, equal_protection_clause__colorblind_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(equa_tr_t20, observed).
narrative_ontology:measurement(equa_tr_t40, equal_protection_clause__colorblind_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement_basis(equa_tr_t40, observed).
narrative_ontology:measurement(equa_tr_t60, equal_protection_clause__colorblind_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement_basis(equa_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_clause__colorblind_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(equa_be_t0, observed).
narrative_ontology:measurement(equa_be_t20, equal_protection_clause__colorblind_reading, base_extractiveness, 20, 0.07).
narrative_ontology:measurement_basis(equa_be_t20, observed).
narrative_ontology:measurement(equa_be_t40, equal_protection_clause__colorblind_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement_basis(equa_be_t40, observed).
narrative_ontology:measurement(equa_be_t60, equal_protection_clause__colorblind_reading, base_extractiveness, 60, 0.09).
narrative_ontology:measurement_basis(equa_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_clause__colorblind_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(equa_su_t0, observed).
narrative_ontology:measurement(equa_su_t20, equal_protection_clause__colorblind_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement_basis(equa_su_t20, observed).
narrative_ontology:measurement(equa_su_t40, equal_protection_clause__colorblind_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement_basis(equa_su_t40, observed).
narrative_ontology:measurement(equa_su_t60, equal_protection_clause__colorblind_reading, suppression_requirement, 60, 0.12).
narrative_ontology:measurement_basis(equa_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__colorblind_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__colorblind_reading, 0.06).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% The equal protection clause kernel supports three structurally distinct readings. The colorblind_reading forbids all race-conscious classification (low ε, formal rule application). The remedial_reading requires race-conscious remediation (different ε, group-based framing). The diversity_reading permits race-conscious policy for educational diversity (different ε, benefit-shared framing). Each reading has different beneficiary/victim groups and different persistence mechanisms. These three stories are linked as a constraint family; each should be generated separately with its own metrics and classification. The colorblind reading claims natural law status; the remedial and diversity readings claim institutional discretion. FSM evaluation is warranted for the colorblind reading given beneficiary declarations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_clause__colorblind_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
