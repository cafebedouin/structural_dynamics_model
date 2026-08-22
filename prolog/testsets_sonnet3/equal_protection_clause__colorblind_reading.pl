% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: equal_protection_clause__colorblind_reading
 *   human_readable: Equal Protection Clause — Colorblind (Anticlassification) Reading
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   This story authors the colorblind (anticlassification) reading of the
 *   Equal Protection Clause's kernel: the clause forbids all governmental
 *   racial classification, full stop, treating every person as a
 *   rights-bearer independent of racial group membership. Under this reading
 *   the constraint is a formal, near-mechanical rule — strict scrutiny
 *   applied uniformly regardless of which group a classification burdens or
 *   benefits — and its own metrics reflect that self-understanding: very low
 *   base extraction, because the rule is presented as neutral formal
 *   application rather than as a mechanism transferring value between groups.
 *   The suppression_requirement series rises over the interval as the
 *   doctrine's enforcement apparatus (from Bakke through Grutter's narrowing
 *   to Students for Fair Admissions) hardened into an actively litigated
 *   strict-scrutiny regime — the rule did not become more extractive, but it
 *   became more actively enforced against a widening set of race-conscious
 *   programs. This is one of three sibling readings of the same kernel
 *   (remedial_reading, diversity_reading); each is authored as its own
 *   constraint with its own ε and its own stakeholder set, per the
 *   ε-invariance principle — this file does not average or hedge across them.
 *
 * KEY AGENTS:
 *   - rejected_majority_group_applicants: primary beneficiary under this reading (moderate/constrained) — vindicated by the anticlassification rule
 *   - beneficiaries_of_race_conscious_admissions_and_contracting_programs: bear the cost when race-conscious programs are struck down under this reading (moderate/constrained)
 *   - universities_and_public_employers: agenda-setters forced to redesign criteria to comply (institutional/constrained)
 *   - federal_and_state_courts: adjudicate which kernel reading controls (institutional/analytical)
 *   - communities_with_histories_of_state_sanctioned_discrimination: excluded — their group-historical claim has no category in this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__colorblind_reading, 0.12).
domain_priors:suppression_score(equal_protection_clause__colorblind_reading, 0.22).
domain_priors:theater_ratio(equal_protection_clause__colorblind_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__colorblind_reading, rope).
narrative_ontology:human_readable(equal_protection_clause__colorblind_reading, "Equal Protection Clause — Colorblind (Anticlassification) Reading").
narrative_ontology:topic_domain(equal_protection_clause__colorblind_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__colorblind_reading, '99a92f21-e7a5-4dd0-a843-4cd7ee46b632').
narrative_ontology:cs_kernel_codification('99a92f21-e7a5-4dd0-a843-4cd7ee46b632', fixed_text).
narrative_ontology:cs_authority_grounding('99a92f21-e7a5-4dd0-a843-4cd7ee46b632', lineage).
narrative_ontology:cs_interpretation_layer_present('99a92f21-e7a5-4dd0-a843-4cd7ee46b632').
narrative_ontology:cs_reading_relation('99a92f21-e7a5-4dd0-a843-4cd7ee46b632', equal_protection_clause__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('99a92f21-e7a5-4dd0-a843-4cd7ee46b632', equal_protection_clause__diversity_reading, forecloses).
narrative_ontology:cs_axiom('99a92f21-e7a5-4dd0-a843-4cd7ee46b632', foundational, individual_is_sole_unit_of_constitutional_concern).
narrative_ontology:cs_axiom_status(individual_is_sole_unit_of_constitutional_concern, holdable).
narrative_ontology:cs_axiom_grounding('99a92f21-e7a5-4dd0-a843-4cd7ee46b632', individual_is_sole_unit_of_constitutional_concern, deontological).
narrative_ontology:cs_axiom('99a92f21-e7a5-4dd0-a843-4cd7ee46b632', foundational, racial_classification_is_per_se_suspect_regardless_of_purpose).
narrative_ontology:cs_axiom_status(racial_classification_is_per_se_suspect_regardless_of_purpose, holdable).
narrative_ontology:cs_axiom_grounding('99a92f21-e7a5-4dd0-a843-4cd7ee46b632', racial_classification_is_per_se_suspect_regardless_of_purpose, deontological).
narrative_ontology:cs_reference_frame('99a92f21-e7a5-4dd0-a843-4cd7ee46b632', reconstruction_era_individual_rights_guarantee).
narrative_ontology:cs_drift_state('99a92f21-e7a5-4dd0-a843-4cd7ee46b632', post_students_for_fair_admissions_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('99a92f21-e7a5-4dd0-a843-4cd7ee46b632', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__colorblind_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, individuals_subject_to_race_conscious_classification).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, rejected_majority_group_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, beneficiaries_of_race_conscious_admissions_and_contracting_programs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, universities_and_public_employers).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, individual_rights_bearer_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, anticlassification_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who apply for admission, employment, or contracts and are denied under a program that weighs race as a factor. Under this reading, the rule vindicates their claim to be judged without reference to racial classification and gives them standing to challenge any program that used race in the decision affecting them. They cannot exit the legal system that classified them; they can only litigate to have the classification struck down.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, rejected_majority_group_applicants, beneficiary,
    moderate, biographical, constrained, national).

% Individuals from historically underrepresented groups who currently gain some advantage from race-conscious admissions, hiring, or contracting programs. Under the colorblind reading, any such advantage constitutes an impermissible classification and is treated as harming the excluded applicant; enforcement of this reading removes the program and, with it, whatever benefit accrued to this group. They have no exit from the constitutional rule itself, only from the specific institutions applying it.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, beneficiaries_of_race_conscious_admissions_and_contracting_programs, payer,
    moderate, biographical, constrained, national).

% Design admissions, hiring, and contracting criteria and must certify compliance with whatever equal protection doctrine currently governs. Under the colorblind reading they must strip race as an explicit factor from every stage of decision-making and defend against strict scrutiny challenges; they bear the administrative and reputational cost of redesigning systems built around race-conscious criteria, and lose the diversity-management tools those criteria provided.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, universities_and_public_employers, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__colorblind_reading, universities_and_public_employers, payer).

% Organizations that bring or defend equal protection litigation on behalf of individual plaintiffs. Groups aligned with the colorblind reading bring suits against race-conscious programs on behalf of individual applicants; they actively shape which cases reach appellate courts and how the doctrine is articulated, and can redirect resources toward other legal strategies if this reading loses ground.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, civil_rights_litigation_groups, agenda_setter,
    organized, generational, mobile, national).

% Adjudicate which reading of equal protection controls in a given case, apply strict scrutiny to racial classifications, and issue precedent binding downstream institutions. Courts do not bear the constraint's costs directly but determine which stakeholders bear them by choosing among the competing kernel readings.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, federal_and_state_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Groups whose disadvantage traces to historical state action (redlining, segregated schooling, exclusionary immigration law) are not treated as a cognizable class under the colorblind reading — the doctrine explicitly declines to inquire into group history or ongoing structural disadvantage. Their claim that group-conscious remedy is necessary to actually equalize starting positions has no doctrinal home in this reading; they are heard, if at all, through the sibling remedial reading, not this one.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, communities_with_histories_of_state_sanctioned_discrimination, excluded,
    powerless, generational, trapped, national).

% Analyze which reading of the Equal Protection Clause best fits text, history, and precedent, and trace how the doctrine's practical effects diverge from its formal colorblind premise.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administrable, individual-level rule — no governmental actor may classify a person by race — that lets courts adjudicate discrimination claims without weighing competing group-level social-welfare judgments case by case.
% TRANSFER_FUNCTION: Removes racial classification as a permissible input to admissions, hiring, and contracting decisions; this shifts seats previously allocated in part on race toward applicants who would have been disadvantaged by that classification, and removes a benefit previously channeled to applicants who would have advantaged from it.
% ABSENT_VOICES: Communities whose current disadvantage is traceable to historical state-sanctioned discrimination are structurally unheard within this reading — the doctrine's individual-rights frame has no category for group-historical injury, so their claim that colorblindness freezes in place the effects of past classification is addressed, if at all, by the remedial reading rather than this one.
% DISAPPEARANCE_RATIONALE: If the colorblind reading lost controlling authority, institutions could reintroduce race-conscious admissions, hiring, and contracting criteria without automatic strict-scrutiny invalidation; individual plaintiffs currently able to challenge such programs would lose that cause of action, and the composition of selective institutions would shift toward whatever the controlling alternative reading permits.
% FOUNDING_PROBLEM: The Equal Protection Clause was adopted after the Civil War to prevent states from using law to entrench a racial caste system, principally to protect the formerly enslaved and their descendants from discriminatory state action.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the colorblind reading (including originalist scholars and several sitting justices) attest that the clause's text and framing generation debates support a universal, individual, anticlassification principle applicable to any racial classification regardless of which group is burdened. Historians of the Reconstruction Congress and scholars outside the colorblind advocacy tradition dispute this, noting that Congress simultaneously enacted race-conscious relief programs (e.g., Freedmen's Bureau appropriations) alongside the Fourteenth Amendment, which they read as evidence the framers did not understand equal protection to forbid race-conscious remediation of group subordination — corroboration for the founding-problem narrative is contested by the very historical record both sides cite.
narrative_ontology:disappearance_verdict(equal_protection_clause__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_clause__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__colorblind_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__colorblind_reading_tests).
:- end_tests(equal_protection_clause__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored very low (0.12) because, taken on its own formal terms, the colorblind reading is a rule of individual non-discrimination applied uniformly — it does not, by its own lights, transfer value from one racial group to another; it simply forbids race as a decision input. Suppression is moderate and rising (0.22 by 2026) because enforcing this reading against institutions that have built race-conscious systems requires increasingly assertive judicial intervention (heightened scrutiny, evidentiary burdens, injunctive relief) — the suppression is the cost of holding a formally neutral rule against institutions with strong incentives to retain race-conscious tools. Accessibility collapse is authored moderately high (0.68): once a court adopts this reading as controlling, essentially no race-conscious alternative survives strict scrutiny within that jurisdiction, closely resembling a mountain-like collapse of alternatives, though the collapse is doctrinal rather than physical. Resistance is high (0.55) because this reading is one of three actively, persistently contested framings — remedial and diversity readings remain live in scholarship, dissent, and shifting judicial coalitions.
 *
 * DIRECTIONALITY LOGIC:
 *   Rejected majority-group applicants (and any individual disadvantaged by a race-conscious classification) are the structural beneficiaries of this reading: it gives them a cause of action and, when successful, removes the classification that disadvantaged them. Beneficiaries of race-conscious programs are the structural payers: under this reading their prior advantage is recharacterized as an impermissible classification and is removed by the same doctrinal machinery. Universities and employers sit as agenda-setters who must operationalize whichever reading controls, bearing compliance costs regardless of which reading prevails. Courts set the agenda at the doctrinal level by choosing among readings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing state-sponsored racial subordination — is contested as either fully live (colorblindness proponents argue any racial classification, including 'benign' ones, recreates the caste logic the Fourteenth Amendment was built to end) or as reframed in a way that serves the interests of those who benefit from the classification's removal. Because the corroboration for the founding-problem narrative is itself split along the same partisan lines as the doctrinal dispute, this story flags founding_problem_status as contested rather than resolving it — the mismatch consumer should read this alongside the sibling readings' status fields rather than treat this reading's self-account as dispositive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblind_reading_kernel_identity,
    'Is the colorblind reading the historically correct interpretation of the Fourteenth Amendment''s original public meaning, or a later doctrinal overlay (emerging clearly only in the late 20th century) retrofitted onto Reconstruction-era text that itself permitted race-conscious remedial legislation?',
    'Historical analysis of the 39th Congress''s contemporaneous enactments (e.g., Freedmen''s Bureau Acts) alongside the Fourteenth Amendment, and tracing the doctrinal lineage of anticlassification theory from Plessy dissent through Bakke to Students for Fair Admissions.',
    'If the colorblind reading is a later doctrinal construction rather than original meaning, its claim to be the single correct reading of the kernel (rather than one contested reading among several) weakens substantially, and its low authored ε — premised on being a neutral formal rule rather than a value-laden choice among readings — becomes harder to sustain as purely formal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_reading_kernel_identity, conceptual, 'Whether the colorblind reading represents original constitutional meaning or a later interpretive overlay.').

omega_variable(
    individual_vs_group_injury_framing,
    'Does treating race-conscious program beneficiaries as ''victims'' when the program is struck down correctly capture the structural relationship, or does it mischaracterize a policy trade-off (which group receives a scarce seat) as an individual rights violation?',
    'Compare outcomes and standing doctrine across jurisdictions with different controlling readings — track whether individual plaintiffs framing works consistently or whether courts applying the remedial or diversity readings treat the same facts as involving no cognizable individual injury at all.',
    'If the individual-injury frame is itself an artifact of choosing the colorblind reading (rather than a neutral description of harm), then this story''s victim declaration is reading-relative rather than fact-relative, which is expected and consistent with kernel-reading methodology but should be flagged rather than treated as settled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_vs_group_injury_framing, conceptual, 'Whether individual-injury framing of program beneficiaries as victims is itself reading-dependent.').

omega_variable(
    cs_framing_kernel_vs_legitimacy_layer,
    'Is the relevant kernel the constitutional text itself (fixed_text, authority via judicial lineage), or is it more precisely the layered legitimacy claim that ''originalism correctly recovers the framers'' single true meaning'' — a claim that itself could be contested independent of the text?',
    'Compare classification outcomes if authority_grounding were coded as ''lineage'' (judicial precedent chain from Bakke through SFFA) versus a hypothetical alternate coding treating the legitimating claim (originalist recovery of a single fixed meaning) as the operative kernel object.',
    'Under the text-as-kernel framing (adopted here), this reading is one of several defensible readings of stable text via evolving judicial lineage. Under the legitimacy-claim-as-kernel framing, the story would instead be about contested epistemic authority to declare originalist recovery successful, which could shift kernel_codification from fixed_text toward distributed and could affect how forecloses/coexists_with is assigned relative to the other readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_legitimacy_layer, conceptual, 'Alternative framings of what the kernel object actually is (text vs. interpretive-legitimacy claim) and their effect on cs_structure classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__colorblind_reading, 1868, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1868, equal_protection_clause__colorblind_reading, theater_ratio, 1868, 0.1).
narrative_ontology:measurement_basis(equa_tr_t1868, observed).
narrative_ontology:measurement(equa_tr_t1954, equal_protection_clause__colorblind_reading, theater_ratio, 1954, 0.08).
narrative_ontology:measurement_basis(equa_tr_t1954, observed).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_clause__colorblind_reading, theater_ratio, 1978, 0.07).
narrative_ontology:measurement_basis(equa_tr_t1978, observed).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_clause__colorblind_reading, theater_ratio, 2003, 0.07).
narrative_ontology:measurement_basis(equa_tr_t2003, observed).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_clause__colorblind_reading, theater_ratio, 2016, 0.08).
narrative_ontology:measurement_basis(equa_tr_t2016, observed).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_clause__colorblind_reading, theater_ratio, 2023, 0.08).
narrative_ontology:measurement_basis(equa_tr_t2023, observed).
narrative_ontology:measurement(equa_tr_t2026, equal_protection_clause__colorblind_reading, theater_ratio, 2026, 0.08).
narrative_ontology:measurement_basis(equa_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t1868, equal_protection_clause__colorblind_reading, base_extractiveness, 1868, 0.05).
narrative_ontology:measurement_basis(equa_be_t1868, observed).
narrative_ontology:measurement(equa_be_t1954, equal_protection_clause__colorblind_reading, base_extractiveness, 1954, 0.06).
narrative_ontology:measurement_basis(equa_be_t1954, observed).
narrative_ontology:measurement(equa_be_t1978, equal_protection_clause__colorblind_reading, base_extractiveness, 1978, 0.08).
narrative_ontology:measurement_basis(equa_be_t1978, observed).
narrative_ontology:measurement(equa_be_t2003, equal_protection_clause__colorblind_reading, base_extractiveness, 2003, 0.09).
narrative_ontology:measurement_basis(equa_be_t2003, observed).
narrative_ontology:measurement(equa_be_t2016, equal_protection_clause__colorblind_reading, base_extractiveness, 2016, 0.1).
narrative_ontology:measurement_basis(equa_be_t2016, observed).
narrative_ontology:measurement(equa_be_t2023, equal_protection_clause__colorblind_reading, base_extractiveness, 2023, 0.12).
narrative_ontology:measurement_basis(equa_be_t2023, observed).
narrative_ontology:measurement(equa_be_t2026, equal_protection_clause__colorblind_reading, base_extractiveness, 2026, 0.12).
narrative_ontology:measurement_basis(equa_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1868, equal_protection_clause__colorblind_reading, suppression_requirement, 1868, 0.05).
narrative_ontology:measurement_basis(equa_su_t1868, observed).
narrative_ontology:measurement(equa_su_t1954, equal_protection_clause__colorblind_reading, suppression_requirement, 1954, 0.1).
narrative_ontology:measurement_basis(equa_su_t1954, observed).
narrative_ontology:measurement(equa_su_t1978, equal_protection_clause__colorblind_reading, suppression_requirement, 1978, 0.14).
narrative_ontology:measurement_basis(equa_su_t1978, observed).
narrative_ontology:measurement(equa_su_t2003, equal_protection_clause__colorblind_reading, suppression_requirement, 2003, 0.18).
narrative_ontology:measurement_basis(equa_su_t2003, observed).
narrative_ontology:measurement(equa_su_t2016, equal_protection_clause__colorblind_reading, suppression_requirement, 2016, 0.2).
narrative_ontology:measurement_basis(equa_su_t2016, observed).
narrative_ontology:measurement(equa_su_t2023, equal_protection_clause__colorblind_reading, suppression_requirement, 2023, 0.22).
narrative_ontology:measurement_basis(equa_su_t2023, observed).
narrative_ontology:measurement(equa_su_t2026, equal_protection_clause__colorblind_reading, suppression_requirement, 2026, 0.22).
narrative_ontology:measurement_basis(equa_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__colorblind_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__colorblind_reading, 0.1).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__diversity_reading).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__remedial_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the equal_protection_clause kernel decomposed per the ε-invariance principle: colorblind_reading (this file, ε≈0.12, individual rights-bearers as sole beneficiaries), diversity_reading (permits race-conscious policy for compelling educational-diversity interests, distinct beneficiary/victim structure and likely higher authored ε reflecting contested extraction), and remedial_reading (requires race-conscious remediation of historical group subordination, group-level beneficiary structure). Each carries its own ε, its own claimed_type, and its own stakeholder set; none averages over the others. Network edges reflect that the colorblind reading's success in litigation directly constrains the doctrinal space available to the diversity and remedial readings — a court adopting this reading forecloses or narrows the operating room of the siblings within that jurisdiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
