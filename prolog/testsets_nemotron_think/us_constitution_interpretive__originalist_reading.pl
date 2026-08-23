% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__originalist_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_constitution_interpretive__originalist_reading
 *   human_readable: Originalist Constitutional Interpretation (Fixed Meaning at Ratification)
 *   domain: constitutional_law/legal_interpretation/political_theory
 *
 * SUMMARY:
 *   The originalist reading of the US Constitution claims that constitutional
 *   meaning was fixed at ratification (1787 for the original document, 1791
 *   for the Bill of Rights, 1868 for the Fourteenth Amendment) and that
 *   legitimate interpretation requires fidelity to the framers' intent or the
 *   original public meaning. This reading presents itself as a Mountain — a
 *   constraint of legal logic and democratic legitimacy that emerges
 *   naturally from the nature of written constitutions. However, identifiable
 *   beneficiaries (federalism advocates, originalist religious liberty
 *   claimants, property rights defenders) and victims (unenumerated rights
 *   claimants, federal regulatory expansion advocates) exist, and the
 *   constraint requires active enforcement through judicial appointments,
 *   institutional networks, and methodological policing. The claim/metric
 *   divergence is the central analytical object: the reading claims Mountain
 *   status while the metrics describe substantial extraction and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, 0.65).
domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, 0.75).
domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, mountain).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "Originalist Constitutional Interpretation (Fixed Meaning at Ratification)").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "constitutional_law/legal_interpretation/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).
domain_priors:emerges_naturally(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, 'b915ec57-5f21-4c5d-a31d-abf75bd0980d').
narrative_ontology:cs_kernel_codification('b915ec57-5f21-4c5d-a31d-abf75bd0980d', fixed_text).
narrative_ontology:cs_authority_grounding('b915ec57-5f21-4c5d-a31d-abf75bd0980d', lineage).
narrative_ontology:cs_interpretation_layer_present('b915ec57-5f21-4c5d-a31d-abf75bd0980d').
narrative_ontology:cs_reading_relation('b915ec57-5f21-4c5d-a31d-abf75bd0980d', us_constitution_interpretive__living_constitution_reading, forecloses).
narrative_ontology:cs_reading_relation('b915ec57-5f21-4c5d-a31d-abf75bd0980d', us_constitution_interpretive__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('b915ec57-5f21-4c5d-a31d-abf75bd0980d', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('b915ec57-5f21-4c5d-a31d-abf75bd0980d', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('b915ec57-5f21-4c5d-a31d-abf75bd0980d', secondary, judicial_restraint_required_by_article_iii).
narrative_ontology:cs_axiom_status(judicial_restraint_required_by_article_iii, holdable).
narrative_ontology:cs_axiom_grounding('b915ec57-5f21-4c5d-a31d-abf75bd0980d', judicial_restraint_required_by_article_iii, deontological).
narrative_ontology:cs_reference_frame('b915ec57-5f21-4c5d-a31d-abf75bd0980d', original_public_meaning_1787).
narrative_ontology:cs_drift_state('b915ec57-5f21-4c5d-a31d-abf75bd0980d', warren_court_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('b915ec57-5f21-4c5d-a31d-abf75bd0980d', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, originalist_religious_liberty_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, constitutional_fixed_meaning_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, judicial_restraint_principle).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, democratic_legitimacy_of_written_constitutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Appointed to enforce originalist methodology; their professional identity and legitimacy within the conservative legal movement are fused with this interpretive commitment. Exit would mean abandoning the judicial philosophy that earned them appointment and the institutional network that sustains their influence.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, originalist_judges, agenda_setter,
    institutional, generational, identity_locked, national).

% Gain constrained federal power and expanded state autonomy under originalist readings of the Commerce Clause, Tenth Amendment, and Eleventh Amendment. Their policy goals align with the structural outcomes of originalist interpretation; they invest in the judicial appointment pipeline to maintain this alignment.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federalism_advocates, beneficiary,
    organized, biographical, constrained, national).

% Benefit from originalist readings of the Free Exercise Clause that prioritize historical understandings of religious liberty over contemporary balancing tests. Their litigation strategy depends on originalist methodology; alternative frameworks (Smith-era neutrality) disadvantage their claims.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, originalist_religious_liberty_claimants, beneficiary,
    organized, biographical, constrained, national).

% Benefit from originalist Takings Clause jurisprudence that expands regulatory takings doctrine and limits government regulatory power. Their legal strategy is built around originalist arguments; living constitutionalist frameworks tend toward deferential review that they cannot win under.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, property_rights_defenders, beneficiary,
    organized, biographical, constrained, national).

% Lose constitutional protection for rights not historically recognized at ratification (reproductive autonomy, LGBTQ+ intimacy and marriage, informational privacy, etc.). Originalism's historical scope limitation structurally excludes their claims; they have no exit from this exclusion short of constitutional amendment or methodological surrender by the Court.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants, payer,
    moderate, biographical, trapped, national).

% Lose regulatory authority under originalist non-delegation doctrine, major questions doctrine, and narrow Commerce Clause readings. Congress and agencies built the modern administrative state on living constitutionalist premises; originalism systematically dismantles that authority. Their exit is legislative (amendments, Court reform) but politically constrained.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates, payer,
    institutional, generational, constrained, national).

% Their interpretive framework is ruled illegitimate by originalism's own terms — not merely wrong but anti-constitutional. They participate in academic discourse but are structurally excluded from judicial authority when originalists control appointments. Their students face a professional landscape where originalist fluency is required for elite clerkships.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, living_constitutionalist_scholars, excluded,
    organized, biographical, constrained, national).

% Argue that constitutional meaning belongs to democratic movements, not courts. Originalism excludes them by fixing meaning at ratification, denying the legitimacy of popular mobilization as interpretive authority. They are doubly excluded: by originalism's historical fixation and by judicial supremacy's institutional capture.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, popular_constitutionalism_advocates, excluded,
    moderate, biographical, constrained, national).

% Study the contest between interpretive methodologies without holding institutional power to enforce either. Their analysis maps the structural positions, but they do not collect rents or bear costs from the constraint's operation.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, legal_academy_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates legal interpretation around a fixed historical anchor (the Constitution's original public meaning), preventing judicial discretion from becoming unbounded legislation and providing a stable rule of law that democratic majorities can rely on when structuring their affairs.
% TRANSFER_FUNCTION: Moves interpretive authority from contemporary judges and democratic majorities to the historical framers and ratifiers; moves policy outcomes from federal regulatory expansion toward state-level variation and legislative resolution; moves constitutional protection from evolving rights claims toward historically enumerated rights only.
% ABSENT_VOICES: Those whose dignity and liberty claims depend on constitutional understandings that post-date 1787/1791/1868 — LGBTQ+ persons seeking marriage equality and anti-discrimination protection, women seeking reproductive autonomy, racial minorities seeking protection from facially neutral policies with disparate impact, disabled persons seeking accommodation rights. These voices are structurally excluded because originalism's historical scope rule denies their claims standing before the analysis begins.
% DISAPPEARANCE_RATIONALE: If originalism vanished overnight, the Supreme Court would revert to living constitutionalist or pluralist methodology within one appointment cycle. Federal regulatory power would expand (Chevron deference restored, non-delegation doctrine abandoned, Commerce Clause widened). Unenumerated rights jurisprudence would resume expansion (substantive due process, equal protection). State autonomy would contract. The Federalist Society's judicial appointment pipeline would lose its organizing principle. The entire institutional architecture of conservative legal movement — law reviews, clerkship networks, think tanks — would face existential crisis.
% FOUNDING_PROBLEM: The problem of judicial legitimacy in a democratic republic: how to prevent unelected judges from imposing their policy preferences under the guise of constitutional interpretation, when the Constitution contains open-textured provisions (due process, equal protection, cruel and unusual punishment, commerce among the states) that seem to invite judicial discretion.
% FOUNDING_PROBLEM_CORROBORATION: Originalists attest the problem is live — judicial activism remains the central threat. Critics (legal historians like Jack Balkin, political scientists like Keith Whittington, liberal constitutional scholars) attest the problem is substantially solved by layered institutional checks (Senate confirmation, stare decisis, judicial hierarchy, public opinion) and that originalism itself has become a vehicle for conservative policy preferences (evidenced by correlation between originalist methodology and conservative outcomes in contested cases). No neutral arbiter corroborates either side; the founding problem's status is itself a contested interpretive question.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_interpretive__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__originalist_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(us_constitution_interpretive__originalist_reading),
    narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(us_constitution_interpretive__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the systematic denial of constitutional claims outside the historical scope — a transfer of protective authority from contemporary claimants to historical understandings. Suppression (0.75) reflects the exclusion of alternative methodologies from legitimate judicial discourse — originalism doesn't just disagree with living constitutionalism; it declares it illegitimate. Theater ratio (0.45) captures the gap between originalism's claimed neutral methodology and its strong correlation with conservative policy outcomes; the methodology is real but its application is contested. Accessibility collapse (0.85) is high because the Mountain claim itself collapses alternatives — if meaning is fixed, other methods aren't just wrong, they're category errors. Resistance (0.70) is high because the living constitutionalist tradition has deep institutional roots and democratic appeal.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist judge's seat (agenda_setter, institutional, identity_locked), the constraint is genuine coordination — a neutral methodology that solves the founding problem of judicial legitimacy. From the unenumerated rights claimant's seat (payer, moderate, trapped), the same structure is extraction — their claims are categorically foreclosed by a methodology they had no hand in choosing and cannot escape. From the federal regulatory advocate's seat (payer, institutional, constrained), it is coordinated extraction — they accept judicial review as an institution but contest this methodology's asymmetric outcomes. The engine computes these divergences from the structural data; the claim of Mountain status does not resolve them.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges are agenda_setters with institutional power and identity_locked exit — their professional identity is fused with the methodology. Federalism advocates, religious liberty claimants, and property rights defenders are beneficiaries with organized power but constrained exit — they benefit structurally but depend on continued judicial control. Unenumerated rights claimants are payers with moderate power and trapped exit — they cannot leave the constitutional system and have no alternative interpretive pathway. Federal regulatory advocates are payers with institutional power but constrained exit — they control the political branches but not the courts. Living constitutionalist scholars and popular constitutionalism advocates are excluded — their frameworks are ruled out of bounds, not merely outvoted.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (judicial legitimacy) remains contested. Originalists argue it is live and originalism solves it; critics argue originalism has become the problem — a methodology that claims neutrality but produces ideologically aligned results. The mandatrophy question: has the originalist constraint outlived its coordinating function (constraining judicial discretion) and become primarily a vehicle for substantive policy outcomes? The theater ratio trajectory (low at founding, peaking during Warren Court as performative dissent, declining during early revival as genuine methodology, rising again as outcomes correlate with politics) suggests cyclical mandatrophy — the constraint's function oscillates with institutional control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the originalist_reading of the us_constitution_interpretive kernel. What structural elements distinguish it from sibling readings (living_constitution_reading, popular_constitutionalism_reading)?',
    'Compare the three readings'' cs_structure blocks: kernel_codification, authority_grounding, axioms, reference_frame, and drift_state. The kernel is the shared commitment; readings are distinct constraints with distinct ε, beneficiaries, victims, and types.',
    'If the readings share the same ε and beneficiary/victim structure, they are the same constraint under different labels (violating ε-invariance). If they differ structurally, they are correctly modeled as separate constraints linked by affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system framing: one kernel, three readings, three constraint stories.').

omega_variable(
    structural_delta_sibling_readings,
    'What would change structurally if a sibling reading (living constitution or popular constitutionalism) were the governing constraint instead of originalism?',
    'Author the sibling constraint stories and compare: beneficiary/victim sets flip (unenumerated rights claimants become beneficiaries, federalism advocates become payers), claimed_type shifts (living constitution claims scaffold or rope, popular constitutionalism claims rope), suppression targets shift (originalism suppresses living constitutionalism; living constitutionalism suppresses originalism).',
    'The kernel contest is a struggle over which constraint governs — each reading''s beneficiaries are the other''s victims. The ε values differ because the standing arrangements under contest differ.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_delta_sibling_readings, conceptual, 'Structural consequences of sibling reading governance.').

omega_variable(
    disagreement_location,
    'Where exactly is the disagreement located between originalism and its siblings — in the kernel_codification, authority_grounding, axioms, reference_frame, or drift_state?',
    'Originalism: fixed_text + lineage + axioms(fixed_meaning, judicial_restraint) + reference_frame(original_public_meaning_1787) + drift_state(practice_drift, severe, unacknowledged). Living constitution: distributed + practice + axioms(evolving_meaning, judicial_adaptation) + reference_frame(contemporary_values) + drift_state(revival_pressure, substantial, acknowledged). Popular constitutionalism: implicit + diffuse_epistemic + axioms(popular_sovereignty_interpretive) + reference_frame(democratic_contestation) + drift_state(authority_erosion, substantial, acknowledged). The disagreement spans all levels.',
    'The kernel contest is not a single-axis disagreement; it is a total framing conflict. This explains why the readings foreclose/coexist/influence each other in complex ways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location, conceptual, 'Multi-level disagreement location across the cs_structure.').

omega_variable(
    false_summit_ambiguity,
    'Is the originalist claim of Mountain status (fixed meaning as natural law of written constitutions) a genuine natural law constraint, or a constructed constraint that benefits identifiable agents (federalism advocates, religious liberty claimants, property rights defenders)?',
    'Test whether the constraint''s operation would persist without the institutional enforcement apparatus (Federalist Society, judicial appointment pipeline, originalist legal academy). If the constraint collapses without active enforcement, it is not a Mountain. The FSM signature in the engine will evaluate this automatically given the declared beneficiaries on a claimed Mountain.',
    'If FSM triggers, the engine reclassifies to tangled_rope (default override target). This would confirm the constraint has both coordination function (stable interpretation) and asymmetric extraction (beneficiaries gain, victims lose).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_ambiguity, empirical, 'Natural-law vs. constructed ambiguity for a claimed Mountain with beneficiaries.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.75) structural (judicial hierarchy, appointment power, stare decisis) or internalized (legal academy socialization, professional identity formation, clerkship pipeline incentives)?',
    'Post-exit suppression trajectory: if law professors and judges who reject originalism still operate within its conceptual vocabulary and citation practices, suppression is partially internalized. Measure the proportion of non-originalist scholarship that engages originalist arguments on originalist terms.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint shapes thought even of its opponents. This would increase χ for excluded seats beyond the structural calculation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the legal interpretive community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 0, 237).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__originalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(us_c_tr_t70, us_constitution_interpretive__originalist_reading, theater_ratio, 70, 0.2).
narrative_ontology:measurement(us_c_tr_t150, us_constitution_interpretive__originalist_reading, theater_ratio, 150, 0.6).
narrative_ontology:measurement(us_c_tr_t170, us_constitution_interpretive__originalist_reading, theater_ratio, 170, 0.55).
narrative_ontology:measurement(us_c_tr_t200, us_constitution_interpretive__originalist_reading, theater_ratio, 200, 0.35).
narrative_ontology:measurement(us_c_tr_t237, us_constitution_interpretive__originalist_reading, theater_ratio, 237, 0.45).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__originalist_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(us_c_be_t70, us_constitution_interpretive__originalist_reading, base_extractiveness, 70, 0.25).
narrative_ontology:measurement(us_c_be_t150, us_constitution_interpretive__originalist_reading, base_extractiveness, 150, 0.55).
narrative_ontology:measurement(us_c_be_t170, us_constitution_interpretive__originalist_reading, base_extractiveness, 170, 0.7).
narrative_ontology:measurement(us_c_be_t200, us_constitution_interpretive__originalist_reading, base_extractiveness, 200, 0.5).
narrative_ontology:measurement(us_c_be_t237, us_constitution_interpretive__originalist_reading, base_extractiveness, 237, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__originalist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(us_c_su_t70, us_constitution_interpretive__originalist_reading, suppression_requirement, 70, 0.35).
narrative_ontology:measurement(us_c_su_t150, us_constitution_interpretive__originalist_reading, suppression_requirement, 150, 0.8).
narrative_ontology:measurement(us_c_su_t170, us_constitution_interpretive__originalist_reading, suppression_requirement, 170, 0.85).
narrative_ontology:measurement(us_c_su_t200, us_constitution_interpretive__originalist_reading, suppression_requirement, 200, 0.7).
narrative_ontology:measurement(us_c_su_t237, us_constitution_interpretive__originalist_reading, suppression_requirement, 237, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__originalist_reading, 0.08).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__living_constitution_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint (originalist_reading) and its siblings (living_constitution_reading, popular_constitutionalism_reading) form the us_constitution_interpretive constraint family. They share the kernel (the US Constitution as a stabilized commitment) but instantiate different constraints with different ε, beneficiaries, victims, and types. Originalism claims Mountain with fixed_text/lineage; living constitution claims Scaffold/Tangled Rope with distributed/practice; popular constitutionalism claims Rope with implicit/diffuse_epistemic. The originalist reading forecloses the living constitution reading's core premise but coexists with popular constitutionalism (different answer to 'who interprets' not 'how').

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_interpretive__originalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(us_constitution_interpretive__originalist_reading, organized, 0.25).
constraint_indexing:directionality_override(us_constitution_interpretive__originalist_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
