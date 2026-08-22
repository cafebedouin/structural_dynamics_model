% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__antisubordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__antisubordination_reading, []).

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
 *   constraint_id: equal_protection_kernel__antisubordination_reading
 *   human_readable: Equal Protection Clause — Antisubordination Reading (Race-Conscious Remediation of Caste Hierarchy)
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   The antisubordination reading of the Equal Protection Clause holds that
 *   the clause's core target is caste — the entrenchment of a subordinated
 *   racial hierarchy — rather than racial classification as such. On this
 *   reading, state action that uses race to dismantle hierarchy (affirmative
 *   admissions, remedial contracting set-asides, disparate-impact-driven
 *   policy) is constitutionally permitted or even required, while state
 *   action that uses race to entrench or reproduce hierarchy is forbidden.
 *   This produces an asymmetric doctrine: dominant-group plaintiffs cannot
 *   successfully invoke equal protection against remedial measures, because
 *   from inside this reading their claim mischaracterizes what the clause
 *   protects. This is exactly the kind of asymmetric structure the
 *   ε-invariance principle requires isolating into its own story — the
 *   colorblind reading and the remedial reading are structurally distinct
 *   claims with different victim sets, different beneficiary sets, and
 *   different ε, and are authored as separate sibling constraints
 *   (colorblind_reading, remedial_reading) linked through the shared kernel
 *   rather than folded into this file.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, 0.58).
domain_priors:suppression_score(equal_protection_kernel__antisubordination_reading, 0.42).
domain_priors:theater_ratio(equal_protection_kernel__antisubordination_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__antisubordination_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__antisubordination_reading, "Equal Protection Clause — Antisubordination Reading (Race-Conscious Remediation of Caste Hierarchy)").
narrative_ontology:topic_domain(equal_protection_kernel__antisubordination_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__antisubordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__antisubordination_reading, '3f29e098-c5ea-4b59-b338-2197d47d78a3').
narrative_ontology:cs_kernel_codification('3f29e098-c5ea-4b59-b338-2197d47d78a3', fixed_text).
narrative_ontology:cs_authority_grounding('3f29e098-c5ea-4b59-b338-2197d47d78a3', lineage).
narrative_ontology:cs_interpretation_layer_present('3f29e098-c5ea-4b59-b338-2197d47d78a3').
narrative_ontology:cs_reading_relation('3f29e098-c5ea-4b59-b338-2197d47d78a3', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('3f29e098-c5ea-4b59-b338-2197d47d78a3', equal_protection_kernel__remedial_reading, influences).
narrative_ontology:cs_axiom('3f29e098-c5ea-4b59-b338-2197d47d78a3', foundational, clause_targets_caste_not_classification).
narrative_ontology:cs_axiom_status(clause_targets_caste_not_classification, holdable).
narrative_ontology:cs_axiom_grounding('3f29e098-c5ea-4b59-b338-2197d47d78a3', clause_targets_caste_not_classification, deontological).
narrative_ontology:cs_axiom('3f29e098-c5ea-4b59-b338-2197d47d78a3', secondary, dominant_groups_lack_standing_against_remedial_measures).
narrative_ontology:cs_axiom_status(dominant_groups_lack_standing_against_remedial_measures, holdable).
narrative_ontology:cs_axiom_grounding('3f29e098-c5ea-4b59-b338-2197d47d78a3', dominant_groups_lack_standing_against_remedial_measures, conventional).
narrative_ontology:cs_reference_frame('3f29e098-c5ea-4b59-b338-2197d47d78a3', reconstruction_era_antisubordination_purpose).
narrative_ontology:cs_drift_state('3f29e098-c5ea-4b59-b338-2197d47d78a3', post_sffa_2023_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('3f29e098-c5ea-4b59-b338-2197d47d78a3', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__antisubordination_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, historically_subordinated_racial_castes).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, black_applicants_to_selective_institutions).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, indigenous_and_native_communities).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, dominant_group_applicants_displaced_by_remedial_measures).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, asian_american_applicants_in_holistic_admissions_regimes).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, antisubordination_principle).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, caste_hierarchy_is_the_constitutional_evil).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups the reading identifies as having been placed and held in a subordinate caste position by centuries of state-enforced exclusion (slavery, Jim Crow, redlining, exclusion acts). Under this reading, state actors may take their group membership into account affirmatively to dismantle continuing structural disadvantage. They cannot exit the historical position that grounds the remedy; the remedy is the mechanism by which the constraint claims to eventually make itself unnecessary.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, historically_subordinated_racial_castes, beneficiary,
    organized, generational, constrained, national).

% Individual applicants who benefit from race-conscious admissions practices justified under this reading as dismantling continuing effects of subordination rather than as mere preference. Their exit option is limited to institutions and jurisdictions that still permit such consideration, which is a shrinking set.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, black_applicants_to_selective_institutions, beneficiary,
    moderate, biographical, constrained, national).

% Communities whose subordination arose through conquest, treaty violation, and forced assimilation rather than the paradigm chattel-slavery history. The antisubordination reading extends coverage to them by focusing on caste function rather than a specific historical mechanism, but their claim is more contested even within the reading's own doctrinal tradition.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, indigenous_and_native_communities, beneficiary,
    powerless, civilizational, trapped, national).

% Applicants from historically dominant groups who are denied admission or opportunity where race-conscious remediation is applied, and who under this reading cannot invoke equal protection to strike the remedy down because the clause is read as targeting hierarchy-entrenchment, not classification, and a remedial measure by definition does not entrench. They bear a real, individualized cost but the reading forecloses their standing to characterize that cost as a constitutional injury of the kind the clause exists to prevent.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, dominant_group_applicants_displaced_by_remedial_measures, payer,
    moderate, biographical, constrained, national).

% An intermediate-position group whose members are statistically disadvantaged relative to a race-neutral baseline by some holistic admissions systems defended under antisubordination reasoning, despite the group's own significant history of state-imposed exclusion (Chinese Exclusion Act, Japanese American internment). Their situation is the reading's hardest internal case: are they a dominant group whose equal-protection claim is foreclosed, or a subordinated group whose claim the reading should recognize? The reading's own doctrine is unsettled on this point.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, asian_american_applicants_in_holistic_admissions_regimes, payer,
    moderate, biographical, constrained, national).

% Design and defend race-conscious admissions, contracting, and hiring policies under the antisubordination theory, characterizing them as remedial rather than preferential. They select which groups count as subordinated, how subordination is measured, and when a program's job is done. Their institutional latitude is precisely what the colorblind reading calls impermissible and what the antisubordination reading calls constitutionally required.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, state_university_and_agency_administrators, agenda_setter,
    institutional, generational, mobile, national).

% Advocacy and litigation groups that develop and press the antisubordination theory in courts, select test cases, and build the doctrinal record connecting present policy to historical caste subordination. They shape which beneficiary groups get judicial recognition.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, civil_rights_litigation_organizations, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__antisubordination_reading, civil_rights_litigation_organizations, observer).

% Adjudicates which reading of the equal protection clause controls in a given case. Currently a minority position on the Supreme Court but historically influential in lower courts and in academic doctrine; the reading's practical force rises and falls with judicial composition rather than textual change.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% Litigants advancing the colorblind reading who, from inside the antisubordination frame, are treated as attempting to weaponize equal protection to preserve dominant-group advantage. Their objection — that any race-based sorting by the state is a constitutional harm regardless of direction — is not absent from public discourse but is structurally excluded from standing to prevail within this reading's own doctrinal logic; the reading's internal grammar has no category for their claim except as illegitimate.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, students_for_fair_admissions_and_allied_litigants, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__antisubordination_reading, historically_subordinated_racial_castes).
narrative_ontology:fixing_cost_class(equal_protection_kernel__antisubordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state action toward dismantling entrenched racial caste hierarchy by permitting institutions to take race into account when doing so counteracts, rather than reinforces, group subordination — solving the problem that formally identical treatment of unequally positioned groups can perpetuate rather than cure inequality.
% TRANSFER_FUNCTION: Moves admissions seats, contracting opportunities, and institutional positions from applicants in historically dominant or intermediate groups toward applicants from groups the reading designates as subordinated castes, justified as compensating for the ongoing effects of prior state-enforced exclusion.
% ABSENT_VOICES: Colorblind-reading litigants and dominant-group applicants who believe any race-conscious state action is itself the constitutional harm are present in public and legal discourse but structurally cannot prevail within this reading's own framework — their objection is treated as a misreading of what the clause protects, not as a competing legitimate claim to be weighed.
% DISAPPEARANCE_RATIONALE: If the antisubordination reading vanished as a live judicial doctrine, race-conscious remedial admissions, contracting set-asides, and disparate-impact-driven policy would lose their primary constitutional justification; institutions would either abandon such programs or restructure them around class-based or facially neutral proxies, and litigation currently foreclosed under this reading (dominant-group equal protection claims against remedial measures) would gain a viable doctrinal path.
% FOUNDING_PROBLEM: Formal equality under Reconstruction-era and mid-20th-century equal protection doctrine proved compatible with the persistence of a racial caste system — legally neutral rules administered within a society still structured by slavery's aftermath and Jim Crow reproduced hierarchy rather than dismantling it. The antisubordination reading was built to close that gap by making the clause track hierarchy rather than mere classification.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights historians and a substantial line of constitutional scholarship (including work outside litigation-funded circles) corroborate that formal-equality doctrine coexisted with caste maintenance through the mid-20th century. Colorblind-reading proponents and a current Supreme Court majority dispute that this history justifies present-day race-conscious remedies, holding the founding problem was substantially addressed by civil rights-era legislation and that continued race-consciousness now itself risks reproducing group-based sorting; no fully disinterested arbiter outside the two litigating traditions exists to settle the status question.
narrative_ontology:disappearance_verdict(equal_protection_kernel__antisubordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__antisubordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__antisubordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_kernel__antisubordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__antisubordination_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__antisubordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__antisubordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a substantial-but-not-extreme 0.58: real, identifiable costs are imposed on dominant-group and intermediate-group (Asian American) applicants under this reading's own operation, but the reading also performs a genuine, non-cover coordination function — dismantling documented structural hierarchy that formally neutral rules had failed to touch. Suppression (0.42) reflects that the reading forecloses one class of constitutional claim (dominant-group equal-protection suits against remedial measures) as a matter of doctrine, which is a real form of legal foreclosure even though it operates through argument and precedent rather than coercive enforcement in the ordinary sense. Resistance is high (0.72) because the reading is intensely contested — it commands a doctrinal minority on the current Supreme Court and faces sustained litigation challenge. Accessibility collapse is moderate-low (0.35): the reading has never fully displaced the colorblind or remedial alternatives, and all three remain live in different courts and different eras.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups are declared beneficiaries and sit near the low end of directionality: the reading exists to authorize measures that transfer opportunity toward them, and it does so by name. Dominant-group applicants displaced by remedial measures are declared victims and sit near the high end: they bear an identifiable, individualized cost and — distinctively for this reading — cannot use equal protection doctrine itself to contest that cost, which amplifies rather than dampens their effective extraction relative to a symmetric doctrine. Asian American applicants occupy the reading's most unstable position: they carry a documented history of state-imposed subordination (which under the reading's own logic should place them in the beneficiary class) yet are statistically disadvantaged by some of the policies the reading defends. I have placed them as payer with the internal tension flagged explicitly in their situation text and in an omega, because forcing a clean resolution here would misrepresent a genuinely unsettled doctrinal question as settled.
 *
 * MANDATROPHY ANALYSIS:
 *   The antisubordination reading was built to solve a specific founding problem: formally neutral rules coexisting with and reproducing caste hierarchy. That problem's live/dead status is itself the central mandatrophy question for this constraint — proponents hold ongoing statistical and structural disparities show the problem remains live; opponents hold that decades of civil rights enforcement, together with the risk that continued race-consciousness now itself sorts by race, mean the arrangement has outlived the narrower version of its founding problem even if broader inequality persists. The classification apparatus does not resolve this dispute; it records the tangled_rope structure (real coordination function + real, asymmetrically borne cost + required active doctrinal maintenance) so that neither a pure-rope story (subordination fully explains all costs) nor a pure-snare story (no genuine coordination function exists) is allowed to stand unexamined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    which_reading_actually_controls,
    'Which of the three kernel readings (antisubordination, colorblind, remedial) actually controls current doctrine, and how much of the antisubordination reading''s authority is residual academic/lower-court influence versus live Supreme Court doctrine?',
    'Track the holding and reasoning of controlling Supreme Court majorities across the interval (Bakke, Grutter, Fisher, Students for Fair Admissions v. Harvard) and code each for which reading''s logic actually decided the case versus which reading''s logic was rejected in dissent.',
    'If the antisubordination reading is now doctrinally marginal (post-2023 SFFA), its practical extraction and suppression values should be read as historical/contested rather than currently controlling — the reading persists as an advocacy and lower-court position more than as binding law, which would push effective enforcement lower than the authored suppression value suggests for the present moment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(which_reading_actually_controls, empirical, 'Doctrinal control question: which kernel reading is currently binding law versus contested academic/advocacy position.').

omega_variable(
    asian_american_classification_instability,
    'Does the antisubordination reading''s own logic classify Asian American applicants as a subordinated caste (given historical exclusion) or as effectively dominant-adjacent (given current statistical position in the policies at issue)?',
    'Examine whether antisubordination-reading scholarship and litigation briefs have produced a consistent, non-ad-hoc rule for group classification, or whether the classification shifts opportunistically by case outcome.',
    'If the reading cannot produce a principled answer, this is evidence the beneficiary/victim boundary is being drawn by outcome rather than by the stated caste-subordination principle — which would push the reading''s computed type toward snare (extraction dressed as remediation) rather than tangled_rope (genuine but costly coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asian_american_classification_instability, conceptual, 'Internal doctrinal instability in defining which groups count as subordinated.').

omega_variable(
    remediation_sunset_question,
    'Does the antisubordination reading have any internal principle for when subordination has been sufficiently dismantled that race-conscious remedy should end, or is the remedy structurally open-ended?',
    'Compare stated benchmarks across antisubordination-reading legal scholarship and litigation (e.g., statistical parity targets, generational sunset proposals) against actual program durations to see whether any program has ever been wound down under this reading''s own terms.',
    'An open-ended remedy with no internal sunset principle looks structurally more like scaffold-that-never-sunsets or tangled_rope-with-entrenchment-risk than a genuinely self-terminating remedial mechanism; if no antisubordination program has ever self-terminated, that is evidence for the mandatrophy concern raised in commentary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remediation_sunset_question, empirical, 'Whether the reading contains a coherent stopping condition or is open-ended by design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__antisubordination_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_kernel__antisubordination_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(equa_tr_t1968, equal_protection_kernel__antisubordination_reading, theater_ratio, 1968, 0.14).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__antisubordination_reading, theater_ratio, 1978, 0.18).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_kernel__antisubordination_reading, theater_ratio, 2003, 0.24).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_kernel__antisubordination_reading, theater_ratio, 2016, 0.26).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_kernel__antisubordination_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1954, 0.3).
narrative_ontology:measurement(equa_be_t1968, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1968, 0.38).
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1978, 0.45).
narrative_ontology:measurement(equa_be_t2003, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2003, 0.52).
narrative_ontology:measurement(equa_be_t2016, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2016, 0.55).
narrative_ontology:measurement(equa_be_t2024, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1954, 0.2).
narrative_ontology:measurement(equa_su_t1968, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1968, 0.28).
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1978, 0.33).
narrative_ontology:measurement(equa_su_t2003, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2003, 0.37).
narrative_ontology:measurement(equa_su_t2016, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2016, 0.4).
narrative_ontology:measurement(equa_su_t2024, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__antisubordination_reading, identity_coordination).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__remedial_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the equal_protection_kernel, decomposed per the ε-invariance principle because the natural-language label 'the Equal Protection Clause' covers structurally distinct claims with different victim sets, different beneficiary sets, and different ε. antisubordination_reading (this file, ε=0.58, tangled_rope: real hierarchy-dismantling coordination + real asymmetric cost on dominant/intermediate-group applicants + required active doctrinal defense) is linked to colorblind_reading (expected ε profile driven by dominant-group victimhood from ANY race-conscious action, expected snare-leaning or rope-leaning depending on authored metrics) and remedial_reading (narrower tailoring requirement, expected lower ε than antisubordination reading due to the compelling-interest/narrow-tailoring limiting principle). Do not average these three into one constraint; each is authored independently and linked here for contamination-propagation analysis only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
