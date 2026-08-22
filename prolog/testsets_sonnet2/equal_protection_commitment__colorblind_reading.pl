% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__colorblind_reading, []).

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
 *   constraint_id: equal_protection_commitment__colorblind_reading
 *   human_readable: Equal Protection as Colorblind Prohibition on State Racial Classification
 *   domain: constitutional/political/social
 *
 * SUMMARY:
 *   This story instantiates the colorblind reading of the equal protection
 *   kernel: the doctrinal position, traced to Justice Harlan's Plessy
 *   dissent, that the Constitution forbids any state use of racial
 *   classification regardless of purpose. Under this reading, race-conscious
 *   admissions and similar remedial programs are not distinguished from
 *   invidious discrimination — the classification itself is the
 *   constitutional harm, independent of intent or asserted benefit. This
 *   reading treats Asian American and white applicants disadvantaged by
 *   race-conscious weighting as bearing the direct injury the Equal
 *   Protection Clause exists to prevent, and treats universities and other
 *   implementing institutions as the parties whose classification practice
 *   violates the rule. Two sibling readings of the SAME kernel text — the
 *   remedial reading (equal protection forbids caste subordination, permits
 *   race-conscious dismantling of it) and the diversity reading (race as one
 *   factor among many toward a compelling educational interest) — are NOT
 *   represented in this file; they are separate constraints with their own ε,
 *   beneficiary/victim structure, and stakeholders, linked here only by
 *   network reference. This file does not average across readings or hedge ε
 *   between them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__colorblind_reading, 0.42).
domain_priors:suppression_score(equal_protection_commitment__colorblind_reading, 0.48).
domain_priors:theater_ratio(equal_protection_commitment__colorblind_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__colorblind_reading, "Equal Protection as Colorblind Prohibition on State Racial Classification").
narrative_ontology:topic_domain(equal_protection_commitment__colorblind_reading, "constitutional/political/social").

domain_priors:requires_active_enforcement(equal_protection_commitment__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__colorblind_reading, 'fabce5c1-0c90-473d-9cbf-2c194be20b6f').
narrative_ontology:cs_kernel_codification('fabce5c1-0c90-473d-9cbf-2c194be20b6f', fixed_text).
narrative_ontology:cs_authority_grounding('fabce5c1-0c90-473d-9cbf-2c194be20b6f', lineage).
narrative_ontology:cs_interpretation_layer_present('fabce5c1-0c90-473d-9cbf-2c194be20b6f').
narrative_ontology:cs_reading_relation('fabce5c1-0c90-473d-9cbf-2c194be20b6f', equal_protection_commitment__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('fabce5c1-0c90-473d-9cbf-2c194be20b6f', equal_protection_commitment__diversity_reading, forecloses).
narrative_ontology:cs_axiom('fabce5c1-0c90-473d-9cbf-2c194be20b6f', foundational, racial_classification_itself_is_the_constitutional_harm).
narrative_ontology:cs_axiom_status(racial_classification_itself_is_the_constitutional_harm, holdable).
narrative_ontology:cs_axiom_grounding('fabce5c1-0c90-473d-9cbf-2c194be20b6f', racial_classification_itself_is_the_constitutional_harm, deontological).
narrative_ontology:cs_axiom('fabce5c1-0c90-473d-9cbf-2c194be20b6f', secondary, strict_scrutiny_applies_uniformly_regardless_of_remedial_purpose).
narrative_ontology:cs_axiom_status(strict_scrutiny_applies_uniformly_regardless_of_remedial_purpose, holdable).
narrative_ontology:cs_axiom_grounding('fabce5c1-0c90-473d-9cbf-2c194be20b6f', strict_scrutiny_applies_uniformly_regardless_of_remedial_purpose, conventional).
narrative_ontology:cs_reference_frame('fabce5c1-0c90-473d-9cbf-2c194be20b6f', harlan_dissent_anticlassification_principle).
narrative_ontology:cs_drift_state('fabce5c1-0c90-473d-9cbf-2c194be20b6f', post_sffa_v_harvard_2023, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('fabce5c1-0c90-473d-9cbf-2c194be20b6f', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__colorblind_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, asian_american_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, white_applicants_denied_under_race_conscious_programs).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, colorblind_legal_movement).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, race_conscious_admissions_programs).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, underrepresented_minority_applicants_relying_on_holistic_review).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, diversity_officers_and_administrators).
narrative_ontology:constraint_vindicates(equal_protection_commitment__colorblind_reading, harlan_plessy_dissent_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_commitment__colorblind_reading, individual_rights_over_group_classification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sought admission to selective institutions and, under this reading, were disadvantaged by race-conscious weighting that treated their racial category as a negative factor relative to other applicants. The colorblind rule, if enforced, removes that classification from the evaluation and is read by this group as restoring an individually-assessed process.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, asian_american_applicants, beneficiary,
    moderate, biographical, constrained, national).

% Denied admission or opportunity where race-conscious programs favored other applicants; under this reading, the constitutional harm is the classification itself, and this group's exclusion is treated as the paradigmatic injury the colorblind rule exists to prevent.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, white_applicants_denied_under_race_conscious_programs, beneficiary,
    moderate, biographical, constrained, national).

% Litigators, scholars, and advocacy organizations that press this reading in courts and public argument, drafting complaints, funding litigation, and shaping doctrine toward strict scrutiny of any racial classification regardless of remedial purpose. They set the doctrinal agenda that the courts then enforce.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, colorblind_legal_movement, agenda_setter,
    organized, generational, mobile, national).

% Universities and other institutions that built holistic review processes incorporating race as one factor among many. Under this reading their programs are per se suspect regardless of purpose; they face litigation, injunctions, and loss of a tool they consider essential to their mission, with no colorable path to retain race as a factor.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, race_conscious_admissions_programs, payer,
    institutional, generational, trapped, national).

% Black, Hispanic, and Native American applicants whose access to selective institutions was supported by race-conscious review. Under the colorblind reading, that support is constitutionally impermissible regardless of the institution's remedial or diversity rationale; they bear the loss of admission pathways with no alternative mechanism substituted for them.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, underrepresented_minority_applicants_relying_on_holistic_review, payer,
    powerless, biographical, trapped, national).

% Institutional staff whose professional function is administering race-conscious programs. Under this reading their function itself is the constitutional violation; enforcement dismantles their programs and can eliminate their institutional role, independent of any finding of individual misconduct.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, diversity_officers_and_administrators, payer,
    moderate, biographical, constrained, national).

% Adjudicate challenges to race-conscious programs, applying strict scrutiny under this reading's doctrine. They administer the classification-forbidding rule and could, in principle, recognize remedial or diversity exceptions, but this reading commits them to treating any racial classification as presumptively unconstitutional.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, federal_and_state_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__colorblind_reading, federal_and_state_courts, observer).

% Argue that colorblindness in a society with an ongoing history of racial subordination locks in existing racial hierarchy rather than dismantling it. Their remedial framework is foreclosed as a live doctrinal option once the colorblind reading controls; they participate in litigation but as the losing structural position under this reading.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, civil_rights_organizations_defending_race_conscious_remedies, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__colorblind_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_commitment__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, administrable rule — no state actor may classify individuals by race, for any purpose — that courts can apply without case-by-case weighing of a program's remedial intent, avoiding the administrability and line-drawing problems of distinguishing 'benign' from 'invidious' classifications.
% TRANSFER_FUNCTION: Moves admission and opportunity slots from applicants who would have benefited from race-conscious weighting (primarily Black, Hispanic, and Native American applicants under holistic review) to applicants evaluated under race-blind criteria (disproportionately Asian American and white applicants in the contested pool), and moves institutional discretion away from universities toward judicial oversight of admissions criteria.
% ABSENT_VOICES: Communities that experienced historical exclusion the race-conscious programs were built to address are not parties to the colorblind doctrinal argument in the same way — their historical claim is treated as constitutionally irrelevant to the classification analysis under this reading, and civil rights organizations pressing the remedial framework argue in a forum whose governing premise already excludes their theory of harm.
% DISAPPEARANCE_RATIONALE: If the colorblind rule were abandoned as constitutional doctrine, race-conscious admissions and other classification-based remedial programs could resume without strict-scrutiny per se treatment; universities would redesign admissions processes, litigation strategy on both sides would shift entirely, and the composition of selective institutions would change measurably within a single admissions cycle.
% FOUNDING_PROBLEM: Built to prevent the state from using racial classification to enforce a caste system — Justice Harlan's Plessy dissent responded directly to state-mandated racial segregation and subordination, asserting that the Constitution 'neither knows nor tolerates classes among citizens' as a bar against state-sponsored racial hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Colorblind-reading advocates attest the founding problem (state-enforced racial hierarchy) is best solved by forbidding all classification, including remedial classification, and cite Harlan's own text. Civil rights historians and remedial-reading advocates, working from the same Plessy dissent, attest that Harlan's target was subordination, not classification per se, and that applying the rule against remedial programs inverts the founding purpose — this is a genuine dispute over the text's own meaning rather than a claim corroborated only by beneficiaries; both sides cite the same 1896 document for opposite conclusions.
narrative_ontology:disappearance_verdict(equal_protection_commitment__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_commitment__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__colorblind_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__colorblind_reading_tests).
:- end_tests(equal_protection_commitment__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42 — moderate-high, per the expected structural delta — because under this reading's own lights, the classification itself constitutes the injury: race-conscious programs that classify applicants by race extract dignitary and material harm from every applicant excluded from a race-neutral evaluation process, regardless of the program's remedial intent. Suppression (0.48) reflects the doctrine's actively enforced character once adopted judicially — implementing institutions cannot simply opt to retain race-conscious criteria; strict scrutiny forecloses that option as a practical matter. Theater ratio is moderate-low (0.28) because the doctrine does real adjudicative work (courts genuinely strike down programs under it) rather than merely performing scrutiny. Accessibility collapse (0.40) is moderate rather than high: race-conscious alternatives (structured race-neutral proxies, socioeconomic weighting) remain available to institutions, so the collapse of alternatives is partial, not total. Resistance (0.62) is substantial — civil rights organizations, universities, and remedial-reading advocates actively contest the doctrine in litigation and public argument; this is not a settled natural fact but a heavily contested constitutional claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Asian American applicants and white applicants denied under race-conscious programs are coded as beneficiaries because, under this reading's own terms, the colorblind rule directly protects their interests by removing race as a factor that operated against them. Race-conscious admissions programs and the underrepresented minority applicants who relied on them are coded as victims/payers because the rule, as enforced, extracts the specific benefit those programs provided regardless of remedial purpose. The colorblind legal movement and reviewing courts are agenda-setters: the movement presses the doctrine, the courts administer and enforce it. This directionality is reading-specific — the remedial and diversity readings would assign nearly inverted beneficiary/victim sets to the same underlying kernel text, which is exactly why they are separate constraint files rather than parameters of this one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview surfaces a genuine, unresolved genealogical dispute rather than a settled mandate. Harlan's dissent targeted state-enforced racial caste (Plessy's segregation regime); the founding problem — is it 'classification' or 'subordination'? — is itself contested between this reading and its remedial sibling, both citing the same 1896 text. This story does not resolve that dispute; it records the disagreement as founding_problem_status: contested with corroboration drawn from both traditions, which is the honest position for a kernel reading rather than a settled constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harlan_text_interpretive_indeterminacy,
    'Does Harlan''s Plessy dissent — ''the Constitution neither knows nor tolerates classes among citizens'' — target racial classification as such, or does it target the specific caste-subordinating use of classification present in state-mandated segregation?',
    'Historical and textual scholarship on the full context of Harlan''s dissent, including his other writings and the specific facts of Plessy, weighed against subsequent doctrinal citation history in Brown, Bakke, Grutter, and SFFA v. Harvard.',
    'If Harlan''s target was classification as such, the colorblind reading is the more textually faithful reading and the remedial reading is the interpretive departure. If his target was subordination specifically, the remedial reading is closer to the original claim and the colorblind reading''s application to remedial programs is an extension beyond the founding case, not a straightforward continuation of it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harlan_text_interpretive_indeterminacy, conceptual, 'Whether the founding text targets classification itself or caste subordination specifically — the crux the three sibling readings divide on.').

omega_variable(
    colorblind_doctrine_effect_on_racial_composition,
    'Does enforcing the colorblind rule against race-conscious admissions actually reduce net racial disadvantage over time, or does it simply relocate the mechanism of disadvantage into facially race-neutral proxies with disparate impact?',
    'Longitudinal empirical study of institutional racial composition and applicant outcomes before and after colorblind-doctrine enforcement (e.g., post-SFFA v. Harvard admissions data across multiple admissions cycles).',
    'If facially neutral proxies substantially replicate prior racial composition effects, that would support the remedial reading''s claim that colorblindness formalizes rather than dismantles existing hierarchy — informing but not resolving which reading better serves the founding problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_doctrine_effect_on_racial_composition, empirical, 'Whether colorblind enforcement reduces or merely relocates racial disadvantage.').

omega_variable(
    classification_harm_versus_subordination_harm,
    'Is the constitutional injury located in the act of racial classification itself (dignitary harm to the classified individual, regardless of the classification''s purpose or effect) or in the material and social effects of subordination (harm measured by outcome, not by category-use)?',
    'Doctrinal analysis of standing and injury requirements across equal protection cases; comparison of how courts have treated stigmatic versus material-harm theories of injury in this line of cases.',
    'If injury is located in classification itself, this reading''s extractiveness score for race-conscious programs is directly warranted. If injury is properly located in subordination effects, this reading may be measuring the wrong harm entirely, which would suggest a materially different ε for the correctly specified constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_harm_versus_subordination_harm, conceptual, 'Where the constitutional harm is properly located — in the classification act or its subordinating effects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__colorblind_reading, 1954, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_commitment__colorblind_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_commitment__colorblind_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement(equa_tr_t1995, equal_protection_commitment__colorblind_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_commitment__colorblind_reading, theater_ratio, 2003, 0.22).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_commitment__colorblind_reading, theater_ratio, 2013, 0.25).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_commitment__colorblind_reading, theater_ratio, 2023, 0.28).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_commitment__colorblind_reading, base_extractiveness, 1954, 0.15).
narrative_ontology:measurement(equa_be_t1978, equal_protection_commitment__colorblind_reading, base_extractiveness, 1978, 0.22).
narrative_ontology:measurement(equa_be_t1995, equal_protection_commitment__colorblind_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(equa_be_t2003, equal_protection_commitment__colorblind_reading, base_extractiveness, 2003, 0.33).
narrative_ontology:measurement(equa_be_t2013, equal_protection_commitment__colorblind_reading, base_extractiveness, 2013, 0.37).
narrative_ontology:measurement(equa_be_t2023, equal_protection_commitment__colorblind_reading, base_extractiveness, 2023, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_commitment__colorblind_reading, suppression_requirement, 1954, 0.1).
narrative_ontology:measurement(equa_su_t1978, equal_protection_commitment__colorblind_reading, suppression_requirement, 1978, 0.2).
narrative_ontology:measurement(equa_su_t1995, equal_protection_commitment__colorblind_reading, suppression_requirement, 1995, 0.28).
narrative_ontology:measurement(equa_su_t2003, equal_protection_commitment__colorblind_reading, suppression_requirement, 2003, 0.34).
narrative_ontology:measurement(equa_su_t2013, equal_protection_commitment__colorblind_reading, suppression_requirement, 2013, 0.4).
narrative_ontology:measurement(equa_su_t2023, equal_protection_commitment__colorblind_reading, suppression_requirement, 2023, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language label 'equal protection / colorblind constitution' per the ε-invariance principle: colorblind_reading (this file, ε=0.42, tangled_rope), remedial_reading (equal protection forbids caste perpetuation, permits race-conscious dismantling — different beneficiary/victim structure, expected different ε), and diversity_reading (race as one factor among many for educational diversity — narrower scope, expected lower ε and different classification). Each reading is authored as a separate constraint with its own stable ε and its own stakeholder set because measuring the SAME kernel text under different interpretive commitments produces materially different extraction profiles — exactly the condition the ε-invariance test flags as requiring decomposition rather than a single hedged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
