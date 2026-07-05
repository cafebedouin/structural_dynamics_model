% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__formal_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Equal Protection — Formal Equality (Anticlassification) Reading
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   This story instantiates the FORMAL EQUALITY (anticlassification) reading
 *   of the Equal Protection Clause, as one of two structurally distinct
 *   constitutional claims sharing the same constitutional text. Under this
 *   reading, the clause prohibits any explicit state use of racial or status
 *   classification, whether the classification burdens or corrects a
 *   historically subordinated group, subject only to strict scrutiny's
 *   compelling-interest and narrow-tailoring test. This is a DIFFERENT
 *   constraint from the sibling anti-caste reading (constraint_id:
 *   anti_caste_reading), which holds that Equal Protection requires active
 *   dismantling of racial hierarchy through state corrective action. The two
 *   readings produce opposite verdicts on the same fact patterns: under
 *   formal equality, a race-conscious remedial program and a race-based
 *   exclusionary law face the same doctrinal test and the remedial program's
 *   designers become the story's payers; under anti-caste, the remedial
 *   program is the coordination function itself and only the exclusionary law
 *   is extractive. Per the ε-invariance principle, these are authored as two
 *   separate constraint files linked by network.affects_constraints, not as
 *   one story with a measurement parameter — the formal equality reading here
 *   carries its own stable ε (0.42, substantially extractive of remedial
 *   capacity) distinct from whatever ε the anti-caste reading carries for the
 *   same text.
 *
 * KEY AGENTS:
 *   - federal_judiciary_applying_strict_scrutiny: doctrinal agenda-setter, sets and enforces the anticlassification test
 *   - white_plaintiffs_challenging_remedial_programs: primary beneficiary of the doctrine's symmetrical treatment of classification
 *   - minority_communities_under_affirmative_remedial_programs: primary payer, loses access to remedial programs under strict scrutiny
 *   - historically_excluded_groups_seeking_structural_remedy: payer at civilizational time horizon, structural inequality treated as pre-constitutional background rather than ongoing injury
 *   - state_agencies_administering_race_conscious_remedies: institutional payer, bears litigation risk for corrective action
 *   - anti_caste_reading_proponents: excluded voice, dissenting doctrinal position not currently controlling
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, 0.42).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__formal_equality_reading, 0.38).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__formal_equality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__formal_equality_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__formal_equality_reading, "Equal Protection — Formal Equality (Anticlassification) Reading").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__formal_equality_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__formal_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__formal_equality_reading, 'aa845331-fc05-41ee-b574-0311bfb15c84').
narrative_ontology:cs_kernel_codification('aa845331-fc05-41ee-b574-0311bfb15c84', fixed_text).
narrative_ontology:cs_authority_grounding('aa845331-fc05-41ee-b574-0311bfb15c84', lineage).
narrative_ontology:cs_interpretation_layer_present('aa845331-fc05-41ee-b574-0311bfb15c84').
narrative_ontology:cs_reading_relation('aa845331-fc05-41ee-b574-0311bfb15c84', fourteenth_amendment_equal_protection__anti_caste_reading, coexists_with).
narrative_ontology:cs_axiom('aa845331-fc05-41ee-b574-0311bfb15c84', foundational, state_use_of_race_is_the_constitutional_harm).
narrative_ontology:cs_axiom_status(state_use_of_race_is_the_constitutional_harm, holdable).
narrative_ontology:cs_axiom_grounding('aa845331-fc05-41ee-b574-0311bfb15c84', state_use_of_race_is_the_constitutional_harm, deontological).
narrative_ontology:cs_axiom('aa845331-fc05-41ee-b574-0311bfb15c84', foundational, remedial_and_invidious_classification_warrant_identical_scrutiny).
narrative_ontology:cs_axiom_status(remedial_and_invidious_classification_warrant_identical_scrutiny, holdable).
narrative_ontology:cs_axiom_grounding('aa845331-fc05-41ee-b574-0311bfb15c84', remedial_and_invidious_classification_warrant_identical_scrutiny, conventional).
narrative_ontology:cs_reference_frame('aa845331-fc05-41ee-b574-0311bfb15c84', reconstruction_era_anticaste_enactment).
narrative_ontology:cs_drift_state('aa845331-fc05-41ee-b574-0311bfb15c84', post_1990s_strict_scrutiny_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa845331-fc05-41ee-b574-0311bfb15c84', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, white_plaintiffs_challenging_remedial_programs).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, institutional_defendants_facing_disparate_impact_claims).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, colorblind_jurisprudence_advocates).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, minority_communities_under_affirmative_remedial_programs).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, historically_excluded_groups_seeking_structural_remedy).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, state_agencies_administering_race_conscious_remedies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applies strict scrutiny to any explicit racial or status classification by the state, regardless of whether the classification burdens or benefits a historically subordinated group. Sets the doctrinal rule that triggers the highest level of judicial review for race-conscious remedies as well as race-based exclusions, treating both as structurally identical harms to be justified by a compelling interest and narrow tailoring.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, federal_judiciary_applying_strict_scrutiny, agenda_setter,
    institutional, generational, analytical, national).

% Bring litigation against affirmative action admissions, minority set-aside contracting, and race-conscious hiring remedies, framing themselves as victims of state racial classification. The formal equality reading gives them standing and a favorable strict-scrutiny framework regardless of the historical asymmetry between the program's beneficiaries and its challengers.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, white_plaintiffs_challenging_remedial_programs, beneficiary,
    organized, biographical, arbitrage, national).

% Universities, employers, and municipalities that would otherwise face liability or political pressure to adopt race-conscious remedies for documented disparities can now point to the formal equality doctrine as a legal shield against such remedies, treating any race-conscious corrective action as presumptively suspect.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, institutional_defendants_facing_disparate_impact_claims, beneficiary,
    institutional, generational, mobile, national).

% Communities whose access to selective universities, government contracting, and employment pipelines depended on race-conscious remedies now face the dismantling of those programs. They bear the cost of a doctrine that treats the remedy for historical exclusion as legally equivalent to the exclusion itself, with no comparable legal pathway to challenge facially neutral practices that reproduce the same disparities.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, minority_communities_under_affirmative_remedial_programs, payer,
    powerless, generational, trapped, national).

% Groups whose subordination was produced by centuries of explicit state racial classification (slavery, Jim Crow, redlining, exclusion statutes) now find that the same constitutional provision meant to remedy that history treats their proposed remedies as constitutionally suspect on the same terms as the original exclusion. Their claim that present disparity is the direct product of past state action is treated as background fact rather than an ongoing constitutional injury the state must correct.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, historically_excluded_groups_seeking_structural_remedy, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__formal_equality_reading, historically_excluded_groups_seeking_structural_remedy, excluded).

% Agencies that designed desegregation orders, minority contracting set-asides, and diversity admissions programs must now redesign or abandon them to survive strict scrutiny, converting what was intended as structural repair into a litigation liability. Compliance costs and legal exposure fall on the agencies attempting corrective action, not on the entities whose historical conduct created the disparity.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, state_agencies_administering_race_conscious_remedies, payer,
    institutional, generational, constrained, national).

% Legal advocacy organizations and scholars who hold that any government use of racial categories is itself the constitutional harm Equal Protection was designed to prevent. This reading vindicates their theory of the Fourteenth Amendment as an anticlassification principle and gives it doctrinal supremacy over rival accounts.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, colorblind_jurisprudence_advocates, beneficiary,
    organized, civilizational, arbitrage, national).

% Scholars, litigants, and movements holding that Equal Protection requires active dismantling of racial hierarchy through state corrective action are not absent from the conversation but are structurally on the losing side of controlling doctrine — their reading remains a dissenting and academic position rather than the operative constitutional rule, despite continuing to be argued in litigation and scholarship.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, anti_caste_reading_proponents, excluded,
    organized, civilizational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__formal_equality_reading, diffuse).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__formal_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administrable, facially neutral rule for reviewing any government use of racial or status classification, coordinating expectations across courts, legislatures, and agencies about when race-based state action is permissible, and preventing ad hoc or politically variable application of racial categories by government.
% TRANSFER_FUNCTION: Moves the burden of proving constitutional legitimacy from facially neutral practices that reproduce racial disparity onto explicit race-conscious remedies designed to correct that disparity — effectively transferring litigation risk, programmatic viability, and access to redress away from historically subordinated groups and onto the state actors attempting corrective action.
% ABSENT_VOICES: Communities whose access to opportunity depends on remedial programs are formally represented in litigation as intervenors but are structurally outside the doctrinal frame, since the controlling test does not ask whether a classification perpetuates or corrects subordination — only whether it classifies. Proponents of the anti-caste reading argue this repeatedly in briefs and scholarship but the formal equality doctrine treats that argument as outside the compelling-interest inquiry.
% DISAPPEARANCE_RATIONALE: If the formal equality reading disappeared as controlling doctrine, race-conscious admissions, contracting set-asides, and desegregation remedies would face a lower bar for justification (rational basis or intermediate scrutiny tied to remedial purpose rather than strict scrutiny triggered by classification alone), state agencies could resume corrective programs with reduced litigation exposure, and decades of accumulated case law narrowing permissible remedies would need to be revisited.
% FOUNDING_PROBLEM: The Fourteenth Amendment was ratified to prevent states from using explicit racial classification to entrench a subordinate caste of newly freed people — the immediate problem was Black Codes and state-sanctioned racial subordination in the Reconstruction South.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Reconstruction and originalist scholars on both sides of the doctrinal debate agree the Amendment's drafters were responding to explicit racial subordination, but dispute whether the drafters' evident purpose was purely anticlassification (no state use of race) or anti-subordination (no state entrenchment of racial hierarchy, including through inaction). Congressional Reconstruction-era legislative history, cited by anti-caste scholars from outside current litigation interests, documents race-conscious relief programs (the Freedmen's Bureau) passed by the same Congress that ratified the Amendment, which formal-equality proponents distinguish as pre-incorporation federal action rather than binding interpretive evidence.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__formal_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__formal_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.42) is moderate-substantial: the doctrine does not extract wealth or labor directly, but it extracts REMEDIAL CAPACITY from historically subordinated groups by converting the tool designed to address their subordination into a tool that constrains the state's ability to use that remedy. Suppression (0.38) reflects that the doctrine forecloses a category of state action (race-conscious remedy) rather than physically coercing anyone; it operates through litigation risk and doctrinal foreclosure rather than direct force. Theater ratio (0.28) is present but not dominant — the strict scrutiny framework does perform real judicial review work (it is not pure performance), but an increasing share of its operation since the 1990s functions to dismantle remedial infrastructure rather than to prevent new invidious classification, which is the rising theater component. Accessibility collapse (0.45) is moderate: alternative doctrinal paths (disparate impact theory, anti-subordination readings) still exist in academic and minority-opinion form, they have not fully collapsed, but their practical availability in controlling case law has narrowed substantially. Resistance (0.62) is high because this reading is actively and continuously contested — by dissenting justices, by the anti-caste scholarly tradition, and by litigants directly affected — unlike a genuine mountain which would meet little resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   White plaintiffs challenging remedial programs and institutional defendants sit near the beneficiary end: the doctrine gives them standing and a favorable test they would not have under a purpose-focused (anti-subordination) framework. Minority communities under remedial programs and historically excluded groups sit near the target end: the doctrine's symmetry treatment converts their historical claim to remedy into a burden they must overcome under strict scrutiny, and their exit options are trapped/constrained because there is no alternative constitutional forum once this reading controls. State agencies administering remedies are an institutional payer — they bear compliance and litigation cost for programs the doctrine treats as presumptively suspect, despite having no independent stake in the outcome beyond their corrective mandate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing explicit state racial subordination) is genuinely contested as live vs. dead: the doctrine treats the problem as substantially solved (hence applying the same skeptical scrutiny to remedial classification as to invidious classification), while payers and the anti-caste tradition treat the problem as unresolved and argue the doctrine has been captured by parties who benefit from the remedy's removal. This is exactly the mandatrophy question the R5 corroboration field is designed to surface: the founding-problem status is attested differently depending on which side of the beneficiary line the attesting party sits on, and even the Reconstruction-era legislative record (the Freedmen's Bureau) is read oppositely by each side.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anticlassification_vs_antisubordination_original_meaning,
    'Did the Reconstruction Congress that ratified the Fourteenth Amendment intend a pure anticlassification rule (no state use of race, full stop) or an anti-subordination rule (no state entrenchment of racial hierarchy, permitting or requiring race-conscious remedy)?',
    'Comprehensive historical analysis of the 39th Congress''s contemporaneous race-conscious legislation (the Freedmen''s Bureau Acts, the Civil Rights Act of 1866) passed alongside the Fourteenth Amendment, weighed against the text''s facially symmetric language (''any person''). This is a live historiographical dispute unlikely to be definitively resolved, but the weight of contemporaneous legislative practice is a strong empirical input.',
    'If anti-subordination is the better original-meaning account, the formal equality reading is a later doctrinal drift from founding intent rather than a stable original reading — this would reclassify the formal equality reading''s ε as artificially low relative to its actual displacement of the amendment''s remedial purpose, and would support treating it as a snare on the anti-caste tradition''s constituency rather than a defensible independent reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anticlassification_vs_antisubordination_original_meaning, conceptual, 'Whether Reconstruction-era legislative practice settles the anticlassification/anti-subordination interpretive dispute.').

omega_variable(
    sibling_reading_delta_location,
    'Where exactly does the disagreement between this reading and the anti-caste_reading sibling live — in the compelling-interest prong, the classification-trigger prong, or the underlying theory of what Equal Protection is FOR?',
    'Doctrinal mapping of specific case outcomes (school desegregation remedies, university admissions, contracting set-asides) under each reading''s test, isolating whether the readings diverge at the threshold question (does explicit racial classification trigger strict scrutiny regardless of purpose) or only at the compelling-interest stage (is remedying past discrimination a compelling interest).',
    'If the disagreement is purely at the compelling-interest stage, the two readings are closer than this story assumes and might be modeled as a single constraint with a contested justification prong rather than two fully separate ε values. If the disagreement is at the threshold classification-trigger stage (as this story assumes), the two readings are genuinely structurally distinct constraints with different victim sets, justifying the current decomposition into separate files.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_delta_location, conceptual, 'Locating the precise structural point of divergence between the two kernel readings.').

omega_variable(
    structural_inequality_baseline_ambiguity,
    'Is present-day racial disparity properly treated as ''pre-constitutional background'' (the state''s only obligation is prospective non-classification) or as an ongoing constitutional injury directly traceable to prior state action (redlining, segregation, exclusion statutes) that the state remains obligated to correct?',
    'Causal historical analysis tracing specific present-day disparities (wealth gaps, school segregation patterns, contracting access) to identifiable state action within the living memory of affected communities, versus disparities attributable to private choice or non-state causes.',
    'If disparities are traceable predominantly to state action within a legally cognizable causal chain, treating them as background rather than ongoing injury is itself an extractive move — this would raise this reading''s effective extraction beyond the authored 0.42 and support the anti-caste reading''s characterization of formal equality as a doctrine of entrenchment rather than neutrality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_inequality_baseline_ambiguity, empirical, 'Whether present disparity should be treated as background fact or continuing state-caused injury.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__formal_equality_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t0, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(four_tr_t0, observed).
narrative_ontology:measurement(four_tr_t10, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement_basis(four_tr_t10, observed).
narrative_ontology:measurement(four_tr_t20, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(four_tr_t20, observed).
narrative_ontology:measurement(four_tr_t30, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement_basis(four_tr_t30, observed).
narrative_ontology:measurement(four_tr_t40, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(four_tr_t40, observed).
narrative_ontology:measurement(four_tr_t50, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement_basis(four_tr_t50, observed).
narrative_ontology:measurement(four_tr_t60, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(four_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(four_be_t0, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(four_be_t0, observed).
narrative_ontology:measurement(four_be_t10, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement_basis(four_be_t10, observed).
narrative_ontology:measurement(four_be_t20, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(four_be_t20, observed).
narrative_ontology:measurement(four_be_t30, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 30, 0.34).
narrative_ontology:measurement_basis(four_be_t30, observed).
narrative_ontology:measurement(four_be_t40, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 40, 0.37).
narrative_ontology:measurement_basis(four_be_t40, observed).
narrative_ontology:measurement(four_be_t50, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 50, 0.4).
narrative_ontology:measurement_basis(four_be_t50, observed).
narrative_ontology:measurement(four_be_t60, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement_basis(four_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t0, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(four_su_t0, observed).
narrative_ontology:measurement(four_su_t10, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement_basis(four_su_t10, observed).
narrative_ontology:measurement(four_su_t20, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 20, 0.24).
narrative_ontology:measurement_basis(four_su_t20, observed).
narrative_ontology:measurement(four_su_t30, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement_basis(four_su_t30, observed).
narrative_ontology:measurement(four_su_t40, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 40, 0.33).
narrative_ontology:measurement_basis(four_su_t40, observed).
narrative_ontology:measurement(four_su_t50, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 50, 0.36).
narrative_ontology:measurement_basis(four_su_t50, observed).
narrative_ontology:measurement(four_su_t60, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 60, 0.38).
narrative_ontology:measurement_basis(four_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__formal_equality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fourteenth_amendment_equal_protection__formal_equality_reading, 0.1).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, anti_caste_reading).

% DUAL FORMULATION NOTE:
% This story and anti_caste_reading are the two readings of the fourteenth_amendment_equal_protection kernel. Both are generated as separate ε-invariant constraint files per the ε-invariance principle: this reading (formal_equality_reading) carries ε=0.42 with state corrective action in the victim set; the sibling carries its own independently-authored ε reflecting the opposite victim/beneficiary structure (invidious classification in the victim set, remedial action as coordination function). Neither file's classification should be read as adjudicating which reading is doctrinally correct — the network edge records that the two readings share a contested kernel, not that one supersedes the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__formal_equality_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
