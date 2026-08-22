% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__anti_caste_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Anti-Caste Reading of Equal Protection (Affirmative Dismantling Mandate)
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This constraint instantiates the anti-caste reading of the Equal
 *   Protection Clause: the view that the Fourteenth Amendment does not merely
 *   forbid explicit state racial or status classification but affirmatively
 *   requires the state to dismantle the practical residue of historical
 *   hierarchy, including through race- and sex-conscious remedial action and
 *   disparate-impact review of facially neutral rules. This is a single
 *   reading of a contested kernel; the sibling formal-equality reading holds
 *   the opposite core premise (that state classification by race or sex is
 *   itself the constitutional wrong, remedial purpose notwithstanding) and is
 *   authored as a separate constraint story, not folded into this one. The
 *   two readings share a text and an institutional history but diverge
 *   sharply on beneficiary structure, extraction profile, and legitimate
 *   state action.
 *
 * KEY AGENTS:
 *   - historically_subordinated_racial_groups: primary beneficiary group under this reading, organized/constrained
 *   - women_and_gender_subordinated_groups: secondary beneficiary group, organized/constrained
 *   - civil_rights_enforcement_agencies: agenda-setter, institutional, administers and enforces the doctrine
 *   - nonminority_applicants_in_remedial_programs: concentrated payer, moderate power, constrained exit
 *   - facially_neutral_institutions_subject_to_disparate_impact_review: institutional payer bearing compliance cost
 *   - originalist_judiciary_and_formal_equality_advocates: excluded from this reading's internal logic though present in the broader legal system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.42).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.38).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "Anti-Caste Reading of Equal Protection (Affirmative Dismantling Mandate)").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, '68163d5a-4b7b-4ab3-8530-477090194dfb').
narrative_ontology:cs_kernel_codification('68163d5a-4b7b-4ab3-8530-477090194dfb', fixed_text).
narrative_ontology:cs_authority_grounding('68163d5a-4b7b-4ab3-8530-477090194dfb', lineage).
narrative_ontology:cs_interpretation_layer_present('68163d5a-4b7b-4ab3-8530-477090194dfb').
narrative_ontology:cs_reading_relation('68163d5a-4b7b-4ab3-8530-477090194dfb', fourteenth_amendment_equal_protection__formal_equality_reading, forecloses).
narrative_ontology:cs_axiom('68163d5a-4b7b-4ab3-8530-477090194dfb', foundational, equal_protection_targets_hierarchy_not_classification).
narrative_ontology:cs_axiom_status(equal_protection_targets_hierarchy_not_classification, holdable).
narrative_ontology:cs_axiom_grounding('68163d5a-4b7b-4ab3-8530-477090194dfb', equal_protection_targets_hierarchy_not_classification, deontological).
narrative_ontology:cs_axiom('68163d5a-4b7b-4ab3-8530-477090194dfb', foundational, state_has_affirmative_duty_to_remedy_structural_disparity).
narrative_ontology:cs_axiom_status(state_has_affirmative_duty_to_remedy_structural_disparity, holdable).
narrative_ontology:cs_axiom_grounding('68163d5a-4b7b-4ab3-8530-477090194dfb', state_has_affirmative_duty_to_remedy_structural_disparity, instrumental).
narrative_ontology:cs_reference_frame('68163d5a-4b7b-4ab3-8530-477090194dfb', reconstruction_era_anti_subordination_purpose).
narrative_ontology:cs_drift_state('68163d5a-4b7b-4ab3-8530-477090194dfb', post_sffa_2023_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('68163d5a-4b7b-4ab3-8530-477090194dfb', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, women_and_gender_subordinated_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, civil_rights_enforcement_agencies).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, diversity_and_inclusion_administrators).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, nonminority_applicants_in_remedial_programs).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, facially_neutral_institutions_subject_to_disparate_impact_review).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, local_governments_bearing_compliance_costs).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, substantive_equality_doctrine).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, anti_subordination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have historically borne the accumulated effects of state-sponsored and state-tolerated racial hierarchy. Under this reading, courts and agencies treat facially neutral policies with disparate racial effects as constitutionally suspect, and affirmative remedial programs (set-asides, race-conscious admissions, disparate-impact liability) are read as required or at minimum permitted responses to that history. Their exit from the constraint is constrained because the remedy depends on continued state recognition of group-based harm; if the reading is abandoned, remedial programs built around it dissolve.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_racial_groups, beneficiary,
    organized, generational, constrained, national).

% Benefit from the extension of anti-subordination logic to sex-based hierarchy: pay equity mandates, pregnancy accommodation requirements, and gender-conscious remedial policy are legitimated as state corrective action rather than treated as suspect classification. Their position depends on courts continuing to read Equal Protection as targeting hierarchy rather than mere classification.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, women_and_gender_subordinated_groups, beneficiary,
    organized, generational, constrained, national).

% Administer disparate-impact review, approve or design affirmative action frameworks, and issue guidance treating structural inequality as an active constitutional concern. They set the operative standard for what counts as a hierarchy requiring correction and enforce compliance through investigation, litigation, and conditioned funding. Their institutional mandate and budget depend on this reading remaining live.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, civil_rights_enforcement_agencies, agenda_setter,
    institutional, civilizational, analytical, national).

% A professional class within universities, corporations, and government whose positions exist to implement anti-subordination compliance programs. They both administer the framework locally and derive career and institutional standing from its continued authority. If the anti-caste reading recedes, much of their institutional function disappears with it.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, diversity_and_inclusion_administrators, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__anti_caste_reading, diversity_and_inclusion_administrators, agenda_setter).

% Individuals who are denied admission, contracts, or positions because a remedial program under this reading weighs group membership as a corrective factor. They bear a concentrated, identifiable cost for a diffuse historical harm they did not personally cause, and they have limited exit — the programs operate wherever they seek entry into the relevant institution.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, nonminority_applicants_in_remedial_programs, payer,
    moderate, biographical, constrained, national).

% Employers, lenders, and municipalities whose neutral criteria (test scores, credit metrics, zoning rules) are reviewed for disparate racial or gender effect regardless of discriminatory intent. They must redesign practices, document justifications, and litigate defenses under a compelling-need or business-necessity standard, at substantial compliance cost, even absent any finding of purposeful discrimination.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, facially_neutral_institutions_subject_to_disparate_impact_review, payer,
    powerful, biographical, constrained, national).

% Municipal and state bodies must fund compliance offices, defend disparate-impact suits, and restructure hiring, contracting, and housing policy to satisfy anti-subordination review. Federal funding conditions and consent decrees make exit from the framework effectively unavailable while it is the controlling doctrine.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, local_governments_bearing_compliance_costs, payer,
    institutional, generational, trapped, regional).

% Judges, scholars, and litigants committed to the formal-equality reading — that Equal Protection forbids state racial classification as such, regardless of remedial purpose — argue this reading inverts the constitutional text into a license for state-sponsored racial and sex classification. They are not absent from the broader legal conversation but are structurally excluded from this reading's own framework: the anti-caste reading treats their objection as a category error (mistaking formal neutrality for substantive equality) rather than engaging it on its own terms.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, originalist_judiciary_and_formal_equality_advocates, excluded,
    powerful, civilizational, constrained, national).

% Evaluate competing readings of the Equal Protection Clause across decades of case law, academic commentary, and shifting judicial composition, without a personal stake in either reading's victory.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, constitutional_scholars_and_courts, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state and private institutions around a shared project of dismantling entrenched status hierarchy: without some mechanism recognizing group-based structural disadvantage, courts and agencies would have no doctrinal basis to address effects-based discrimination that persists after formal legal barriers are removed.
% TRANSFER_FUNCTION: Moves opportunities, positions, contracts, and institutional resources from individuals and institutions occupying formally neutral positions toward members of groups the reading identifies as historically subordinated, and moves compliance costs from those groups onto institutions and individuals not responsible for the specific historical harm being remedied.
% ABSENT_VOICES: Formal-equality advocates and originalist jurists are structurally present in the broader legal system but excluded from this reading's own internal logic, which treats their objection (that race-conscious remedy is itself a forbidden classification) as a failure to grasp substantive equality rather than a competing constitutional claim requiring engagement on equal footing.
% DISAPPEARANCE_RATIONALE: If the anti-caste reading disappeared overnight, affirmative action admissions programs, disparate-impact liability regimes, and race-and-sex-conscious remedial contracting would lose their constitutional foundation; enforcement agencies built around this doctrine would need to reconstitute their mandates, and decades of institutional diversity infrastructure would face immediate legal exposure.
% FOUNDING_PROBLEM: Formal legal equality (the end of de jure segregation and explicit classification) did not eliminate the practical, intergenerational effects of centuries of state-enforced racial and gender hierarchy; facially neutral rules continued to reproduce those effects, leaving subordinated groups with no doctrinal path to challenge structural rather than intentional discrimination.
% FOUNDING_PROBLEM_CORROBORATION: Empirical sociologists and economists studying persistent racial wealth, employment, and housing gaps (working outside any civil-rights enforcement agency) corroborate that formal-neutrality regimes alone did not close measured disparities. Formal-equality scholars and originalist jurists, also outside the beneficiary set, corroborate the opposite claim from the same data: that the persistence of disparity does not establish that race-conscious state remedy is the constitutionally correct or only permissible response, and note the doctrine has itself become a vehicle through which administrators and enforcement agencies expand their own mandate.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__anti_caste_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__anti_caste_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__anti_caste_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).
:- end_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate-high and rising across the measured interval, reflecting the doctrine's expansion from narrow desegregation remedies (1954-1968) into broader disparate-impact and affirmative-action frameworks (1978-2024) that impose concentrated costs on identifiable non-beneficiary individuals and institutions. Suppression (0.38) is moderate: the reading does not physically coerce compliance but conditions federal funding, exposes institutions to litigation, and treats resistance to remedial frameworks as itself evidence of discriminatory intent, which raises the practical cost of dissent. Theater ratio (0.3) reflects a real coordination function (addressing structural, not merely intentional, discrimination) alongside a growing layer of compliance administration whose scale sometimes exceeds its demonstrated remedial effect. Resistance (0.72) is high because the reading remains one of the most actively contested doctrines in constitutional law, drawing sustained legal and political challenge from the formal-equality camp. Accessibility collapse (0.35) is comparatively low — the formal-equality alternative is not foreclosed as a matter of legal possibility; it is a live, actively litigated competing reading, which is precisely why this is a kernel-reading story rather than a settled mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated racial and gender groups are the structural beneficiaries: the reading exists to authorize state action on their behalf, and their d sits toward the beneficiary end even though their exit from dependence on the doctrine is constrained by the fact that alternative remedies (formal-equality-only enforcement) have historically underperformed for them. Civil rights enforcement agencies and diversity administrators are agenda-setters/secondary beneficiaries whose institutional survival is tied to the doctrine's continued authority. Nonminority applicants and facially-neutral institutions are targets: they bear concentrated, identifiable costs (lost admission, litigation exposure, compliance spending) for a historical harm not attributable to them individually, which is exactly the asymmetric-extraction structure the tangled-rope classification requires alongside the genuine coordination function of addressing structural discrimination.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than pure snare) is deliberate: the founding problem — that formal legal equality did not eliminate structural hierarchy — was genuinely live at the doctrine's origin and remains partially live today (per the corroborating empirical record), which supplies the coordination function. But the doctrine has also generated an administrative class (diversity and inclusion administrators, enforcement agency staff) whose institutional interest in the doctrine's continuation now exceeds what the founding problem alone would justify, and the reading imposes costs on payers who have no voice in defining when the historical harm is sufficiently remedied to sunset the corrective action. Treating this purely as coordination would erase the payer side of the ledger; treating it purely as extraction would erase the genuine and independently corroborated persistence of structural disparity that motivated the reading. The tangled_rope classification holds both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anti_caste_reading_is_one_of_two_live_readings,
    'Is the anti-caste reading the constitutionally correct interpretation of Equal Protection, or is the sibling formal-equality reading correct, and how would the corpus represent a future where one reading definitively displaces the other?',
    'Track subsequent Supreme Court composition and rulings (e.g., trajectory after Students for Fair Admissions v. Harvard) to see whether disparate-impact and affirmative-action frameworks under this reading continue to be authorized or are progressively foreclosed by formal-equality holdings.',
    'If the formal-equality reading becomes doctrinally dominant, the anti-caste reading''s beneficiary-side legitimacy erodes even though the underlying structural-disparity problem it identifies may remain unaddressed by any doctrine; this would not resolve the omega but would shift which reading carries live institutional authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anti_caste_reading_is_one_of_two_live_readings, conceptual, 'Which of the two kernel readings holds live constitutional authority is unresolved and shifting with judicial composition.').

omega_variable(
    structural_disparity_persistence_vs_doctrine_capture,
    'To what extent does current racial and gender disparity reflect an ongoing live problem the anti-caste reading is still needed to address, versus how much of the doctrine''s continued scope reflects institutional self-perpetuation by the administrative class that implements it?',
    'Compare disparity trend lines in domains with and without active disparate-impact/affirmative-action enforcement, controlling for other policy variables, and separately audit compliance-administration budget growth against measured remedial outcomes.',
    'If disparities have substantially closed in enforcement-heavy domains while administrative infrastructure has grown, that supports reclassifying toward piton (atrophied function, persistent administrative theater); if disparities remain wide and tightly linked to enforcement gaps, that supports the tangled_rope/coordination-heavy reading remaining accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_disparity_persistence_vs_doctrine_capture, empirical, 'Whether the doctrine''s continued scope tracks a live founding problem or increasingly reflects administrative self-perpetuation.').

omega_variable(
    beneficiary_group_boundary_stability,
    'How is the boundary of the ''historically subordinated'' beneficiary class determined, by whom, and does that determination itself carry extraction risk (e.g., expanding the remedial beneficiary class to groups without comparable historical state-sponsored subordination)?',
    'Examine case law and agency guidance for criteria used to extend or contract protected-group status and track whether extension correlates with political/administrative convenience versus documented historical harm.',
    'An unstable or administratively-convenient beneficiary boundary would indicate the coordination function is being stretched beyond its founding justification, strengthening the extraction reading; a stable, historically-grounded boundary would support the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_group_boundary_stability, conceptual, 'Whether the beneficiary class boundary tracks documented historical subordination or administrative expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1954, 0.08).
narrative_ontology:measurement(four_tr_t1968, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1968, 0.12).
narrative_ontology:measurement(four_tr_t1978, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1978, 0.18).
narrative_ontology:measurement(four_tr_t1995, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1995, 0.24).
narrative_ontology:measurement(four_tr_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(four_tr_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(four_be_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1954, 0.15).
narrative_ontology:measurement(four_be_t1968, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1968, 0.22).
narrative_ontology:measurement(four_be_t1978, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1978, 0.31).
narrative_ontology:measurement(four_be_t1995, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1995, 0.36).
narrative_ontology:measurement(four_be_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(four_be_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1954, 0.2).
narrative_ontology:measurement(four_su_t1968, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1968, 0.3).
narrative_ontology:measurement(four_su_t1978, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1978, 0.34).
narrative_ontology:measurement(four_su_t1995, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1995, 0.36).
narrative_ontology:measurement(four_su_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2010, 0.37).
narrative_ontology:measurement(four_su_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__anti_caste_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection__formal_equality_reading).

% DUAL FORMULATION NOTE:
% This story and fourteenth_amendment_equal_protection__formal_equality_reading are sibling readings of the same kernel (fourteenth_amendment_equal_protection). The anti-caste reading treats the clause's object as hierarchy and authorizes affirmative remedial classification (higher ε, beneficiary set includes subordinated groups as groups); the formal-equality reading treats the clause's object as classification itself and treats remedial racial/sex classification as presumptively suspect (different beneficiary/victim structure, different ε). The two are linked here rather than merged because the reading choice determines the beneficiary set, not merely the label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
