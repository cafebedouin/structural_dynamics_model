% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: equal_protection_commitment__colorblind_reading
 *   human_readable: Equal Protection Color-Blind Reading
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the colorblind reading of the equal
 *   protection commitment kernel. The reading holds that the Fourteenth
 *   Amendment's equal protection clause categorically forbids any state use
 *   of racial classification, rendering the Constitution 'color-blind' in
 *   Justice Harlan's Plessy dissent formulation. The constraint operates by
 *   treating racial classification itself as the constitutional injury — the
 *   act of classifying by race is the harm, regardless of the
 *   classification's purpose or effect. This reading has been the doctrinal
 *   foundation for challenges to affirmative action, majority-minority
 *   districting, and race-conscious admissions. The standing arrangement
 *   under contest is the regime of colorblind constitutional law as it has
 *   evolved from Plessy through Brown, Bakke, Grutter, Parents Involved, and
 *   Students for Fair Admissions. ε is assessed by this reading's lights on
 *   that standing arrangement: the colorblind reading sees the constraint as
 *   moderately extractive (0.42) because the prohibition on classification
 *   blocks race-conscious remedies that the reading's adherents believe are
 *   unnecessary or themselves harmful, while the reading's beneficiaries
 *   (those who would be classified by race-conscious programs) are treated as
 *   victims of the classification itself. The engine computes per-seat
 *   classifications from the structural data below; the claimed_type (snare)
 *   reflects the authoring seat's structural judgment that the constraint
 *   extracts from identifiable victims (applicants denied admission under
 *   race-conscious programs, institutions barred from using race) while its
 *   coordination function (preventing racial caste) is genuine but
 *   subordinated to the extraction logic.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__colorblind_reading, 0.42).
domain_priors:suppression_score(equal_protection_commitment__colorblind_reading, 0.58).
domain_priors:theater_ratio(equal_protection_commitment__colorblind_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__colorblind_reading, snare).
narrative_ontology:human_readable(equal_protection_commitment__colorblind_reading, "Equal Protection Color-Blind Reading").
narrative_ontology:topic_domain(equal_protection_commitment__colorblind_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__colorblind_reading, '976f9b29-c7e3-4099-962c-fe31d91133f1').
narrative_ontology:cs_kernel_codification('976f9b29-c7e3-4099-962c-fe31d91133f1', fixed_text).
narrative_ontology:cs_authority_grounding('976f9b29-c7e3-4099-962c-fe31d91133f1', lineage).
narrative_ontology:cs_interpretation_layer_present('976f9b29-c7e3-4099-962c-fe31d91133f1').
narrative_ontology:cs_reading_relation('976f9b29-c7e3-4099-962c-fe31d91133f1', equal_protection_commitment__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('976f9b29-c7e3-4099-962c-fe31d91133f1', equal_protection_commitment__diversity_reading, forecloses).
narrative_ontology:cs_axiom('976f9b29-c7e3-4099-962c-fe31d91133f1', foundational, classification_itself_is_harm).
narrative_ontology:cs_axiom_status(classification_itself_is_harm, holdable).
narrative_ontology:cs_axiom_grounding('976f9b29-c7e3-4099-962c-fe31d91133f1', classification_itself_is_harm, deontological).
narrative_ontology:cs_axiom('976f9b29-c7e3-4099-962c-fe31d91133f1', foundational, constitution_is_colorblind).
narrative_ontology:cs_axiom_status(constitution_is_colorblind, holdable).
narrative_ontology:cs_axiom_grounding('976f9b29-c7e3-4099-962c-fe31d91133f1', constitution_is_colorblind, conventional).
narrative_ontology:cs_reference_frame('976f9b29-c7e3-4099-962c-fe31d91133f1', harlan_plessy_dissent).
narrative_ontology:cs_drift_state('976f9b29-c7e3-4099-962c-fe31d91133f1', post_sfv_harvard_unc, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('976f9b29-c7e3-4099-962c-fe31d91133f1', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__colorblind_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, colorblind_doctrine_adherents).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, meritocratic_allocation_proponents).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, asian_applicants).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, white_applicants).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, race_conscious_institutions).
narrative_ontology:constraint_vindicates(equal_protection_commitment__colorblind_reading, colorblind_constitutional_principle).
narrative_ontology:constraint_vindicates(equal_protection_commitment__colorblind_reading, harlan_plessy_dissent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Originalist judges, conservative legal organizations, and policymakers who advocate and enforce the colorblind principle. They benefit from a clear, administrable rule that blocks race-conscious policies they view as divisive or unconstitutional. They can advance their reading through judicial appointments, litigation, and legislation; exit to alternative constitutional frameworks is politically available but ideologically costly.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, colorblind_doctrine_adherents, beneficiary,
    institutional, generational, mobile, national).

% Advocates of test-based, race-neutral merit allocation who view the colorblind constraint as protecting individual merit from group-based adjustment. They gain when admissions and hiring rely solely on quantifiable metrics. Their exit is constrained by the dominance of holistic review in elite institutions; they can advocate for policy change but cannot unilaterally implement it.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, meritocratic_allocation_proponents, beneficiary,
    organized, biographical, constrained, national).

% Asian American applicants to selective universities who, under race-conscious holistic review, face effective penalties relative to other groups with similar academic metrics. The colorblind reading treats their disadvantage under race-conscious programs as the primary constitutional injury. Their exit is identity-locked: the constraint defines their injury through their racial identity, and they cannot exit the racial classification without exiting the demographic category itself.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, asian_applicants, payer,
    moderate, biographical, identity_locked, national).

% White applicants to selective institutions who are disadvantaged by race-conscious admissions relative to a colorblind baseline. The colorblind reading centers their injury as the paradigmatic equal protection violation. Their exit is identity-locked for the same structural reason as Asian applicants: the constraint constitutes their injury through their racial identity.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, white_applicants, payer,
    moderate, biographical, identity_locked, national).

% Universities, legislatures, school districts, and employers that would use racial classification for remedial, diversity, or inclusion purposes. They bear the compliance cost of strict scrutiny litigation, the opportunity cost of foregone race-conscious programs, and the political cost of operating under a constraint they view as substantively wrong. Their exit is constrained: they must either comply, litigate, or use race-neutral proxies that the colorblind reading may also challenge.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, race_conscious_institutions, payer,
    institutional, generational, constrained, national).

% Black, Latino, Native American, and other applicants who benefit from race-conscious admissions and would be disadvantaged by a colorblind regime. They are structurally excluded from this reading's victim calculus — the colorblind reading does not recognize their disadvantage under colorblindness as a cognizable injury. Their exit is trapped: they cannot access the political or judicial levers that would change the constraint, and the constraint's logic treats their preferred remedy as the constitutional violation.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, underrepresented_minority_applicants, excluded,
    powerful, biographical, trapped, national).

% The current doctrinal authority that sets the scope and intensity of colorblind enforcement through strict scrutiny jurisprudence. They administer the constraint, define its boundaries, and can expand or contract it through precedent. They have arbitrage-grade exit: they can shift doctrinal frameworks (e.g., from strict scrutiny to categorical ban) without personal cost, and their institutional role insulates them from the constraint's extraction.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, supreme_court_originalist_majority, agenda_setter,
    institutional, generational, arbitrage, national).

% Academic observers who analyze the constraint's operation across readings. They neither collect nor pay the constraint's extraction. Their analytical seat has universal scope and analytical exit — they can adopt any reading's framework without material consequence.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the state from using racial classification, thereby blocking the re-emergence of a formal racial caste system. Solves the coordination problem of credible commitment: by categorically forbidding classification, the state cannot selectively classify to disadvantage disfavored groups.
% TRANSFER_FUNCTION: Moves the power to classify by race from state institutions (universities, legislatures, agencies) to a categorical prohibition enforced by courts. The transfer is from race-conscious institutions (who lose the tool) to colorblind doctrine adherents (who gain the guarantee). The cost falls on would-be beneficiaries of race-conscious programs and the institutions that would run them.
% ABSENT_VOICES: Underrepresented minority applicants and communities who would benefit from race-conscious remedies; civil rights organizations operating under the remedial reading; university diversity officers and administrators who view race-conscious policies as essential to their mission. They are excluded because the colorblind reading's logic treats their preferred policies as the constitutional violation, not the remedy.
% DISAPPEARANCE_RATIONALE: If the colorblind constraint vanished overnight, universities would immediately reinstate race-conscious admissions; legislatures would enact race-conscious districting and contracting programs; the remedial and diversity readings would become the operative constitutional frameworks. The institutional landscape of racial classification would fundamentally reorganize.
% FOUNDING_PROBLEM: The post-Reconstruction promise of equal protection was betrayed by Plessy's 'separate but equal' doctrine, which used racial classification to create and maintain a caste system. The colorblind reading was built to solve the problem of state-sponsored racial caste by categorically forbidding the tool (classification) that made caste possible.
% FOUNDING_PROBLEM_CORROBORATION: Colorblind adherents (originalist judges, conservative legal movement) attest the founding problem is live: racial classification inherently creates caste, and the constraint remains necessary. Remedial adherents (civil rights organizations, critical race theorists, liberal legal scholars) attest the founding problem is dead: formal caste is gone, but material subordination persists, and the constraint now blocks remediation. Diversity adherents (universities, corporate America, military) attest it is contested: diversity is a distinct compelling interest from remedying caste. No single corroboration exists outside the beneficiary set; the dispute is structural.
narrative_ontology:disappearance_verdict(equal_protection_commitment__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__colorblind_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(equal_protection_commitment__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__colorblind_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.42) is moderate-high because the constraint's central operation — prohibiting all racial classification — blocks race-conscious programs that their designers and beneficiaries view as remedial or diversity-serving, while the prohibition's beneficiaries (those who would be disadvantaged by such programs) are a narrower set. The constraint's coordination function (preventing racial caste) is genuine but the extraction from race-conscious institutions and their intended beneficiaries is substantial. Suppression (0.58) is significant because the constraint requires active judicial enforcement to maintain — legislatures and universities that attempt race-conscious programs face litigation and injunction. The suppression is structural (legal barriers to race-conscious policy) not internalized, though omega colorblind_coercion_mechanism tracks the ambiguity. Theater ratio (0.22) is low-moderate: the constraint's enforcement machinery (strict scrutiny review of racial classifications) is functional, not performative, though the remedial reading's adherents would argue the colorblind principle is invoked theatrically to block genuine remediation. Accessibility collapse (0.35) is moderate: alternatives (race-neutral proxies, class-based affirmative action) exist but are treated as constitutionally distinct rather than equivalent. Resistance (0.72) is high: the constraint faces sustained political, academic, and institutional opposition from remedial and diversity reading adherents.
 *
 * PERSPECTIVAL GAP:
 *   From the colorblind reading's agenda-setter seat (originalist/textualist judges, colorblind advocacy organizations), the constraint is a rope — genuine coordination against racial classification with minimal coercive overhead. From the payer seats (race-conscious institutions, would-be beneficiaries of race-conscious programs), the constraint is a snare — the coordination story (preventing caste) is cover for blocking remediation, persistence depends on judicial coercion suppressing race-conscious alternatives. The engine computes this divergence from the structural data. The remedial and diversity readings instantiate different constraints with different ε, different victim sets, different types — they are not perspectival variants of this constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The colorblind reading's structural beneficiaries are those who would be subject to racial classification under race-conscious programs — primarily Asian and white applicants in competitive admissions, plus meritocratic allocation proponents who benefit from the prohibition. These agents have d near the beneficiary end (low effective extraction). The victims are the institutions that would run race-conscious programs (universities, legislatures, school districts) and the intended beneficiaries of those programs (underrepresented minority applicants). These agents have d near the target end (high effective extraction). The remedial_reading and diversity_reading stakeholders are excluded voices — they would object to the colorblind constraint's victim set but are not seated within this reading's framework. Competition authorities analog (here, the Supreme Court as institutional agenda-setter) occupies the analytical seat with arbitrage-grade exit. The engine derives d from these beneficiary/victim declarations plus exit options: colorblind_doctrine_adherents have mobile exit (can advocate other frameworks), race_conscious_institutions have constrained exit (must comply or litigate), asian_applicants and white_applicants have identity_locked exit (the constraint defines their injury through their racial identity).
 *
 * MANDATROPHY ANALYSIS:
 *   The colorblind reading's founding problem (preventing racial caste à la Plessy) is contested: the colorblind reading's adherents say the problem is live (racial classification inherently creates caste), remedial adherents say the problem is dead (formal caste is gone, but subordination persists), diversity adherents say it's contested (caste and diversity are distinct problems). The constraint persists because its agenda-setters (conservative legal movement, originalist judges) have the power to maintain it, while its payers (universities, minority applicants) lack the political power to override it. The constraint is not a piton — it has concentrated beneficiaries (colorblind doctrine adherents) who actively maintain it, and active enforcement (strict scrutiny). It is a snare because the coordination function is real but the extraction is asymmetric and sustained by coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'On which structural element do the colorblind, remedial, and diversity readings of equal protection fundamentally disagree?',
    'Map each reading''s victim set, beneficiary set, and ε referent to identify the precise structural delta.',
    'If disagreement is on the victim set (who bears the harm), the kernel has multiple instantiations with different ε values. If on the beneficiary set, the coordination function differs. If on the ε referent itself, the readings describe different constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Structural location of disagreement among equal_protection_commitment readings').

omega_variable(
    colorblind_coercion_mechanism,
    'Does the colorblind reading''s prohibition on racial classification function as structural suppression (barring state action) or internalized suppression (delegitimizing race-conscious thought)?',
    'Trace post-adoption trajectories: if institutions that abandon race-conscious programs under colorblind doctrine later reinvent them under neutral proxies, suppression is structural; if the prohibition reshapes what policymakers consider thinkable, it is partially internalized.',
    'Internalized suppression raises effective extraction beyond the structural measure — the constraint carries itself forward through cognitive channels even when legal enforcement relaxes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_coercion_mechanism, empirical, 'Structural vs. internalized suppression in colorblind equal protection enforcement').

omega_variable(
    reading_foreclosure_boundary,
    'Does a commitment to the colorblind reading logically foreclose the remedial reading within a single legal framework, or do they coexist as competing interpretations?',
    'Test whether a single jurisdiction''s doctrine can simultaneously hold ''no racial classification ever'' and ''race-conscious measures to dismantle subordination are required'' without contradiction.',
    'If forecloses, the kernel''s readings are mutually exclusive frameworks. If coexists_with, the dispute is political, not logical. Determines cs_structure.reading_relations assignment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Logical foreclosure vs. political coexistence among equal protection readings').

omega_variable(
    axiom_grounding_stability,
    'Is the colorblind reading''s foundational axiom (''classification_itself_is_harm'') grounded in empirically contingent claims about social outcomes, or in deontological commitment to individual equality?',
    'Track doctrinal evolution: if colorblind advocates shift from ''classification causes social division'' to ''classification violates dignity regardless of outcomes'' when evidence contradicts the former, the axiom has moved from empirically_contingent to deontological grounding.',
    'Empirically_contingent axioms route to foreclosure under axiom_overriding drift; deontological axioms do not. Affects cs_structure.axioms.grounding_type and drift_state forecast.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_grounding_stability, conceptual, 'Epistemic grounding of the colorblind reading''s core normative claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__colorblind_reading, 1896, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1896, equal_protection_commitment__colorblind_reading, theater_ratio, 1896, 0.05).
narrative_ontology:measurement(equa_tr_t1954, equal_protection_commitment__colorblind_reading, theater_ratio, 1954, 0.08).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_commitment__colorblind_reading, theater_ratio, 1978, 0.12).
narrative_ontology:measurement(equa_tr_t1996, equal_protection_commitment__colorblind_reading, theater_ratio, 1996, 0.15).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_commitment__colorblind_reading, theater_ratio, 2003, 0.18).
narrative_ontology:measurement(equa_tr_t2014, equal_protection_commitment__colorblind_reading, theater_ratio, 2014, 0.2).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_commitment__colorblind_reading, theater_ratio, 2023, 0.22).

% Extraction over time
narrative_ontology:measurement(equa_be_t1896, equal_protection_commitment__colorblind_reading, base_extractiveness, 1896, 0.15).
narrative_ontology:measurement(equa_be_t1954, equal_protection_commitment__colorblind_reading, base_extractiveness, 1954, 0.18).
narrative_ontology:measurement(equa_be_t1978, equal_protection_commitment__colorblind_reading, base_extractiveness, 1978, 0.28).
narrative_ontology:measurement(equa_be_t1996, equal_protection_commitment__colorblind_reading, base_extractiveness, 1996, 0.35).
narrative_ontology:measurement(equa_be_t2003, equal_protection_commitment__colorblind_reading, base_extractiveness, 2003, 0.38).
narrative_ontology:measurement(equa_be_t2014, equal_protection_commitment__colorblind_reading, base_extractiveness, 2014, 0.4).
narrative_ontology:measurement(equa_be_t2023, equal_protection_commitment__colorblind_reading, base_extractiveness, 2023, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1896, equal_protection_commitment__colorblind_reading, suppression_requirement, 1896, 0.25).
narrative_ontology:measurement(equa_su_t1954, equal_protection_commitment__colorblind_reading, suppression_requirement, 1954, 0.3).
narrative_ontology:measurement(equa_su_t1978, equal_protection_commitment__colorblind_reading, suppression_requirement, 1978, 0.42).
narrative_ontology:measurement(equa_su_t1996, equal_protection_commitment__colorblind_reading, suppression_requirement, 1996, 0.5).
narrative_ontology:measurement(equa_su_t2003, equal_protection_commitment__colorblind_reading, suppression_requirement, 2003, 0.52).
narrative_ontology:measurement(equa_su_t2014, equal_protection_commitment__colorblind_reading, suppression_requirement, 2014, 0.55).
narrative_ontology:measurement(equa_su_t2023, equal_protection_commitment__colorblind_reading, suppression_requirement, 2023, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__colorblind_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_commitment__colorblind_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the equal_protection_commitment kernel family. The colorblind_reading (this story), remedial_reading, and diversity_reading are three structurally distinct constraints with different ε, different victim/beneficiary structures, and different types. They are linked by network.affects_constraints. The colorblind reading's prohibition on classification creates downstream pressure on the other readings by setting the doctrinal baseline they must overcome.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_commitment__colorblind_reading, institutional, 0.15).
constraint_indexing:directionality_override(equal_protection_commitment__colorblind_reading, powerful, 0.85).
constraint_indexing:directionality_override(equal_protection_commitment__colorblind_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
