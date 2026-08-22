% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__formal_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__formal_equality_reading, []).

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
 *   constraint_id: fourteenth_amendment_equal_protection__formal_equality_reading
 *   human_readable: Equal Protection — Formal (Anticlassification) Reading
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   Since Brown v. Board and accelerating through the strict-scrutiny cases
 *   of the 1980s–2020s (Croson, Adarand, Grutter/Fisher's narrowing, and the
 *   2023 rejection of race-conscious admissions), Equal Protection doctrine
 *   has increasingly converged on an anticlassification principle: the state
 *   may not sort citizens by race for any purpose, remedial or otherwise,
 *   without surviving the highest level of judicial scrutiny. This reading
 *   treats the doctrine as coordination — a uniform rule against state racial
 *   sorting that protects individuals of every race symmetrically — while its
 *   critics read the same doctrine as extraction: a rule that disables the
 *   one legal tool historically excluded groups had to correct
 *   classification-produced disparities, while treating the disparities
 *   themselves as a neutral, non-actionable background condition once the
 *   explicit classifying statute is repealed.
 *
 * KEY AGENTS:
 *   - colorblind_legal_movement_litigators: agenda-setting seat, sets doctrinal strategy, bears no direct cost
 *   - white_plaintiffs_in_reverse_discrimination_suits: beneficiary of the individual right against classification, mobile exit
 *   - state_affirmative_action_administrators: institutional payer, constrained exit, must redesign or abandon programs
 *   - minority_beneficiaries_of_race_conscious_remedies: powerless payer, trapped, loses access channel with no substitute
 *   - historically_excluded_groups_relying_on_corrective_programs: powerless payer/excluded, generational harm treated as pre-constitutional background
 *   - incumbent_institutional_beneficiaries_of_facially_neutral_status_quo: institutional beneficiary of an unexamined baseline
 *   - federal_judiciary: analytical observer, adjudicates scrutiny tiers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, 0.42).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__formal_equality_reading, 0.58).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__formal_equality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__formal_equality_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__formal_equality_reading, "Equal Protection — Formal (Anticlassification) Reading").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__formal_equality_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__formal_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__formal_equality_reading, 'b6b067c8-93bf-4002-b6f5-6e9dcd689242').
narrative_ontology:cs_kernel_codification('b6b067c8-93bf-4002-b6f5-6e9dcd689242', fixed_text).
narrative_ontology:cs_authority_grounding('b6b067c8-93bf-4002-b6f5-6e9dcd689242', lineage).
narrative_ontology:cs_interpretation_layer_present('b6b067c8-93bf-4002-b6f5-6e9dcd689242').
narrative_ontology:cs_reading_relation('b6b067c8-93bf-4002-b6f5-6e9dcd689242', fourteenth_amendment_equal_protection__anti_caste_reading, coexists_with).
narrative_ontology:cs_axiom('b6b067c8-93bf-4002-b6f5-6e9dcd689242', foundational, state_racial_classification_symmetrically_suspect).
narrative_ontology:cs_axiom_status(state_racial_classification_symmetrically_suspect, holdable).
narrative_ontology:cs_axiom_grounding('b6b067c8-93bf-4002-b6f5-6e9dcd689242', state_racial_classification_symmetrically_suspect, deontological).
narrative_ontology:cs_axiom('b6b067c8-93bf-4002-b6f5-6e9dcd689242', foundational, individual_right_against_classification_supersedes_group_remedy).
narrative_ontology:cs_axiom_status(individual_right_against_classification_supersedes_group_remedy, holdable).
narrative_ontology:cs_axiom_grounding('b6b067c8-93bf-4002-b6f5-6e9dcd689242', individual_right_against_classification_supersedes_group_remedy, conventional).
narrative_ontology:cs_reference_frame('b6b067c8-93bf-4002-b6f5-6e9dcd689242', brown_era_anticlassification_consensus).
narrative_ontology:cs_drift_state('b6b067c8-93bf-4002-b6f5-6e9dcd689242', post_2023_admissions_ruling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b6b067c8-93bf-4002-b6f5-6e9dcd689242', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, white_plaintiffs_in_reverse_discrimination_suits).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, incumbent_institutional_beneficiaries_of_facially_neutral_status_quo).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, colorblind_legal_movement_litigators).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, state_affirmative_action_administrators).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, minority_beneficiaries_of_race_conscious_remedies).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, historically_excluded_groups_relying_on_corrective_programs).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, anticlassification_principle).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, individual_rights_over_group_remedy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bring and fund strategic litigation asserting that any explicit racial classification by the state, regardless of remedial intent, triggers strict scrutiny. They set the doctrinal agenda by selecting plaintiffs and framing challenges to admissions programs, contracting set-asides, and redistricting plans. They are not personally burdened by the doctrine — they administer and advance it.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, colorblind_legal_movement_litigators, agenda_setter,
    organized, generational, arbitrage, national).

% Individuals who were denied admission, a contract, or a position where a race-conscious program factored group membership into the decision. Under this reading they have a cognizable equal protection claim regardless of the program's remedial purpose or overall distributive effect. They can pursue litigation and, if unsuccessful in one venue, reapply or relocate — their exit from the specific decision point is real, even if the underlying competitive disadvantage (if any) persists.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, white_plaintiffs_in_reverse_discrimination_suits, beneficiary,
    moderate, biographical, mobile, national).

% Universities, agencies, and municipalities that designed corrective programs to offset documented historical exclusion now operate under strict scrutiny for any explicit racial classification. They must either abandon race-conscious criteria, redesign programs around facially neutral proxies (income, geography, first-generation status) with weaker fit to the harm, or defend the program in costly litigation with a high failure rate. Their institutional purpose — corrective redistribution — is directly targeted by the doctrine's operation.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, state_affirmative_action_administrators, payer,
    institutional, generational, constrained, national).

% Members of groups the dismantled or chilled programs were designed to reach. When race-conscious admissions, contracting, or hiring preferences are struck down or preemptively abandoned to avoid suit, this population loses access channels built around documented historical exclusion. They have no comparable substitute mechanism and cannot litigate their way back into a program that no longer exists; their only recourse is through facially neutral proxies that imperfectly track the original harm.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, minority_beneficiaries_of_race_conscious_remedies, payer,
    powerless, biographical, trapped, national).

% Communities whose current disadvantage is traceable to state-sponsored classification (redlining, exclusion from GI Bill benefits, segregated school funding) but whose remedy now requires proving present intentional discrimination rather than pointing to structural legacy. The formal equality reading treats the prior harm as pre-constitutional background once explicit classification ended, closing off the doctrinal path by which the harm could be redressed through group-conscious correction.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, historically_excluded_groups_relying_on_corrective_programs, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__formal_equality_reading, historically_excluded_groups_relying_on_corrective_programs, excluded).

% Institutions and demographic groups whose current advantageous position resulted from historical explicit classification that has since been formally repealed. Under this reading, once the explicit classification is off the books, present distribution is treated as the neutral baseline against which any new race-conscious correction must justify itself under strict scrutiny — a baseline that happens to have been produced by the very classifications the doctrine now forbids revisiting through group remedy.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, incumbent_institutional_beneficiaries_of_facially_neutral_status_quo, beneficiary,
    institutional, generational, analytical, national).

% Applies strict scrutiny to explicit racial classifications, assessing compelling interest and narrow tailoring. Adjudicates the doctrine without being a direct party to the underlying redistribution; its rulings determine which corrective programs survive and in what form.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, federal_judiciary, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__formal_equality_reading, diffuse).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__formal_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, administrable rule — explicit racial classification by the state triggers the highest level of judicial scrutiny — that constrains all state actors symmetrically and gives individuals a predictable, individually enforceable right against being sorted by race, regardless of which direction the sorting runs.
% TRANSFER_FUNCTION: Moves the burden of proof and the practical availability of race-conscious correction away from institutions seeking to remedy documented historical exclusion, and moves litigation leverage and access-channel stability toward individuals challenging those remedial classifications — while leaving in place, unexamined, whatever distribution resulted from now-repealed explicit historical classification.
% ABSENT_VOICES: Communities experiencing the structural residue of historical explicit classification (segregated school funding, redlining, exclusion from federal housing and lending programs) are not heard as claimants in most formal-equality cases — the doctrine's individual-plaintiff structure gives standing to the person denied a specific benefit under a remedial program, not to a class asserting an unremedied structural legacy. Their objection — that treating post-classification distribution as neutral background naturalizes the very harm the Amendment was ratified to address — is structurally excluded from the cause of action this reading recognizes.
% DISAPPEARANCE_RATIONALE: If the formal equality reading vanished and courts instead adjudicated equal protection claims under a pure anti-caste standard, race-conscious remedial programs would face rational-basis-like deference rather than strict scrutiny, colorblind litigation strategy would lose its primary doctrinal vehicle, and institutions currently constrained by anticlassification rules could redesign admissions, contracting, and districting around explicit group-conscious correction — a substantial reallocation of access and litigation posture.
% FOUNDING_PROBLEM: The doctrine was built, in its modern strict-scrutiny form, to prevent the state from ever again officially sorting citizens by race for ANY purpose — including purposes framed as beneficial — after a history in which 'benign' and 'protective' racial classifications (segregation defended as separate-but-equal, internment defended as security) proved to be vehicles for subordination.
% FOUNDING_PROBLEM_CORROBORATION: Colorblind-doctrine proponents (including several sitting and former federal judges) attest the problem — state actors weaponizing racial classification under benign pretext — remains live and justifies symmetric scrutiny. Civil rights historians, sociologists studying persistent racial wealth and education gaps, and dissenting judicial opinions attest from outside the colorblind litigation movement that the doctrine, in current application, primarily disables corrective classification while leaving classification-produced disparities unaddressed — a status the doctrine's own architects rarely characterize as their intended outcome.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__formal_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__formal_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__formal_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).
:- end_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42 — moderate, not extreme — because the formal equality reading genuinely solves a real coordination problem (preventing state racial sorting from ever again being weaponized under a benign label) while also, in its current operation, systematically disabling the one doctrinal path by which historically excluded groups could secure group-conscious correction. Suppression (0.58) is higher than extraction because the doctrine's enforcement mechanism — strict scrutiny applied symmetrically — actively forecloses the alternative (rational-basis review of remedial classifications) regardless of the remedial program's actual distributive effect; this is a structural foreclosure, not merely an extraction of value. Theater ratio is low-moderate (0.28): the anticlassification principle is not primarily performative, but a growing share of its application (facial-neutrality workarounds, proxy variables engineered around the doctrine rather than through it) has taken on a theatrical quality as institutions learn to satisfy the doctrine's letter while pursuing similar ends through indirection.
 *
 * DIRECTIONALITY LOGIC:
 *   Colorblind litigators occupy the agenda-setter seat: they administer the doctrine's expansion without bearing its costs. White plaintiffs in individual reverse-discrimination suits are structural beneficiaries — the doctrine creates a cognizable right that did not previously exist, and their exit options (reapply, relocate, litigate) are real. State affirmative action administrators are institutional payers with constrained exit: they cannot simply relocate the corrective mission, only redesign or abandon it under threat of suit. Minority beneficiaries of race-conscious remedies and historically excluded groups are the trapped targets: the doctrine removes their access channel and offers no litigable substitute, since the reading does not recognize present disparate impact traceable to past explicit classification as itself actionable absent a live classifying statute.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing state racial classification from serving as a vehicle for subordination under a benign label — remains partially live (contested, not dead): the historical pattern of 'protective' classification masking subordination is real and the doctrine's symmetric-scrutiny answer to it is not merely inertial theater. But the doctrine's current operation increasingly serves a different function than its founding case (Brown, which struck down classification that entrenched subordination) — it now most frequently strikes down classification that was designed to dismantle subordination. Classifying this as tangled_rope rather than snare or pure rope respects both halves: a genuine coordination function (symmetric anticlassification, preventing weaponized 'benign' racial sorting) persists alongside an asymmetric extraction that the formal equality reading's own metrics show growing over the measured interval (0.15 to 0.42) as the doctrine has been extended from clearly invidious classification toward remedial classification with a much stronger claim to legitimacy under the founding rationale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'This constraint is the formal_equality (anticlassification) reading of the fourteenth_amendment_equal_protection kernel. The sibling anti_caste_reading holds that the Amendment requires active state dismantling of racial, gender, and status hierarchy through corrective classification. The disagreement is located precisely at whether state-sponsored racial classification aimed at correcting documented historical subordination is itself a harm requiring the same strict scrutiny as invidious classification, or is instead a protected (or even mandated) coordination function that the anticlassification principle wrongly disables. Which reading correctly identifies the Amendment''s operative commitment?',
    'This is not empirically resolvable by further doctrinal analysis alone — it turns on a contested normative theory of what equal protection is FOR (protecting individuals from state sorting vs. dismantling caste hierarchy) that the constitutional text and Reconstruction-era legislative history do not unambiguously settle. Historians of the Reconstruction Congress''s intent, and comparative analysis of how peer jurisdictions with similar equality clauses (South Africa''s substantive equality jurisprudence, Canada''s Section 15(2)) have resolved the identical structural question, could inform but not conclusively resolve the disagreement.',
    'If the anti_caste_reading is correct, this reading''s classification of state corrective action as extraction-bearing (victims: state administrators, minority beneficiaries) inverts the true structure — the real victims would be the historically excluded groups under a colorblind regime, and this reading''s beneficiaries (individual plaintiffs, incumbent institutions) would be recharacterized as the extraction''s actual recipients. If this reading is correct, the anti_caste_reading''s classification would itself be extractive in the opposite direction, licensing state racial sorting under a corrective label with the same weaponization risk the Reconstruction Congress and Brown sought to foreclose.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, preference, 'The two kernel readings disagree at the level of what equal protection is fundamentally for, not at the level of any resolvable empirical fact.').

omega_variable(
    sibling_reading_structural_delta,
    'What would change structurally if the anti_caste_reading were adopted in place of this reading?',
    'Direct doctrinal substitution analysis: under anti_caste, race-conscious remedial programs would be reviewed under a standard closer to rational basis or intermediate scrutiny keyed to remedial purpose, state corrective action would exit the victim set of this constraint (and possibly enter the beneficiary set of its own constraint), and the population currently coded as excluded/trapped (historically_excluded_groups_relying_on_corrective_programs) would gain a doctrinal path to relief they currently lack.',
    'Adoption of the sibling reading would not merely adjust ε for this constraint — it would dissolve this constraint''s victim structure and construct a materially different one, confirming that these are properly two separate constraint stories (per the ε-invariance principle) rather than one story with a contested parameter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Names the specific structural delta the sibling reading would produce, per the kernel context instructions.').

omega_variable(
    pre_constitutional_background_framing,
    'Is treating post-classification distributional inequality as ''pre-constitutional background'' (outside the Amendment''s reach once the explicit classifying statute is repealed) itself a normatively loaded move, or a neutral consequence of requiring state action for an Equal Protection claim?',
    'Comparative doctrinal analysis of how state-action doctrine treats other forms of historically state-created but currently facially-neutral disparity (e.g., zoning-derived residential segregation) would clarify whether the ''background'' framing is applied consistently or selectively to racial remedy cases specifically.',
    'If selectively applied, this supports the critique that formal equality''s background-treatment of historical classification functions to protect the fruits of past explicit classification while forbidding new classification aimed at correcting it — strengthening the case for classifying this reading''s operation as substantially extractive rather than purely coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_constitutional_background_framing, conceptual, 'Whether treating repealed-classification-produced inequality as background is neutral doctrine or a substantive thumb on the scale.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__formal_equality_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t1954, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(four_tr_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1978, 0.14).
narrative_ontology:measurement(four_tr_t1995, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(four_tr_t2003, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2003, 0.21).
narrative_ontology:measurement(four_tr_t2013, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2013, 0.24).
narrative_ontology:measurement(four_tr_t2023, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2023, 0.28).

% Extraction over time
narrative_ontology:measurement(four_be_t1954, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1954, 0.15).
narrative_ontology:measurement(four_be_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1978, 0.22).
narrative_ontology:measurement(four_be_t1995, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1995, 0.31).
narrative_ontology:measurement(four_be_t2003, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2003, 0.34).
narrative_ontology:measurement(four_be_t2013, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2013, 0.38).
narrative_ontology:measurement(four_be_t2023, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2023, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t1954, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1954, 0.3).
narrative_ontology:measurement(four_su_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1978, 0.38).
narrative_ontology:measurement(four_su_t1995, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1995, 0.46).
narrative_ontology:measurement(four_su_t2003, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2003, 0.51).
narrative_ontology:measurement(four_su_t2013, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2013, 0.55).
narrative_ontology:measurement(four_su_t2023, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2023, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__formal_equality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, anti_caste_reading).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, affirmative_action_admissions_programs).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, voting_rights_act_preclearance_regime).

% DUAL FORMULATION NOTE:
% This story and anti_caste_reading are the two principal readings of the fourteenth_amendment_equal_protection kernel. They share the constitutional text (fixed_text kernel codification, lineage authority grounding through judicial precedent) but diverge in ε, beneficiary/victim structure, and classification: this reading authors moderate extraction (0.42) rising over the interval as strict scrutiny extended from invidious to remedial classification, with state corrective administrators and historically excluded groups as victims. The anti_caste_reading, authored separately, would place state corrective action in its beneficiary/coordination set and would likely place unremedied structural inequality and colorblind-doctrine beneficiaries in its victim set — a materially different structure, not a re-measurement of this one. Neither file should attempt to average or reconcile with the other; each stands as ε-invariant on its own terms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__formal_equality_reading, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
