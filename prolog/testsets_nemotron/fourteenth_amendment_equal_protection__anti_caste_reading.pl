% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__anti_caste_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Equal Protection as Anti-Caste Mandate (Anti-Caste Reading)
 *   domain: constitutional_law/civil_rights/political_philosophy
 *
 * SUMMARY:
 *   This constraint story captures the anti-caste reading of the Fourteenth
 *   Amendment's Equal Protection Clause: the Constitution requires the state
 *   to actively dismantle racial, gender, and status hierarchies through
 *   affirmative corrective action. This reading — rooted in Reconstruction's
 *   radical purpose, revived by the Warren Court and critical race theory —
 *   treats equality as a structural condition to be achieved, not merely a
 *   formal prohibition on classification. It legitimates race-conscious
 *   remedial programs (affirmative action, disparate impact liability, voting
 *   rights enforcement, school desegregation) as constitutional mandates. The
 *   constraint operates as a tangled rope: it coordinates a genuine
 *   collective-action problem (dismantling caste requires state capacity no
 *   private actor possesses) while extracting substantial resources and
 *   discretion from taxpayers, incumbent institutions, and individuals who
 *   experience remedial policies as exclusion. The engine computes per-seat
 *   types from the structural data; the claimed tangled_rope reflects the
 *   authoring seat's structural assessment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.68).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.42).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "Equal Protection as Anti-Caste Mandate (Anti-Caste Reading)").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional_law/civil_rights/political_philosophy").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, 'd710f908-d6a5-47fa-9278-121709ef66f2').
narrative_ontology:cs_kernel_codification('d710f908-d6a5-47fa-9278-121709ef66f2', formalized).
narrative_ontology:cs_authority_grounding('d710f908-d6a5-47fa-9278-121709ef66f2', lineage).
narrative_ontology:cs_interpretation_layer_present('d710f908-d6a5-47fa-9278-121709ef66f2').
narrative_ontology:cs_reading_relation('d710f908-d6a5-47fa-9278-121709ef66f2', fourteenth_amendment_equal_protection__formal_equality_reading, coexists_with).
narrative_ontology:cs_axiom('d710f908-d6a5-47fa-9278-121709ef66f2', foundational, equality_requires_hierarchy_dismantling).
narrative_ontology:cs_axiom_status(equality_requires_hierarchy_dismantling, holdable).
narrative_ontology:cs_axiom_grounding('d710f908-d6a5-47fa-9278-121709ef66f2', equality_requires_hierarchy_dismantling, deontological).
narrative_ontology:cs_axiom('d710f908-d6a5-47fa-9278-121709ef66f2', foundational, state_affirmative_obligation_to_subordinated_groups).
narrative_ontology:cs_axiom_status(state_affirmative_obligation_to_subordinated_groups, holdable).
narrative_ontology:cs_axiom_grounding('d710f908-d6a5-47fa-9278-121709ef66f2', state_affirmative_obligation_to_subordinated_groups, deontological).
narrative_ontology:cs_axiom('d710f908-d6a5-47fa-9278-121709ef66f2', secondary, race_consciousness_permissible_for_remediation).
narrative_ontology:cs_axiom_status(race_consciousness_permissible_for_remediation, holdable).
narrative_ontology:cs_axiom_grounding('d710f908-d6a5-47fa-9278-121709ef66f2', race_consciousness_permissible_for_remediation, instrumental).
narrative_ontology:cs_reference_frame('d710f908-d6a5-47fa-9278-121709ef66f2', reconstruction_anti_caste_mandate).
narrative_ontology:cs_drift_state('d710f908-d6a5-47fa-9278-121709ef66f2', contemporary_anti_subordination_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d710f908-d6a5-47fa-9278-121709ef66f2', '2026-08-15T14:32:00Z').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_gender_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_status_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, state_corrective_institutions).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, taxpayers_funding_remedial_programs).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, institutions_subject_to_affirmative_obligations).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, individuals_denied_colorblind_treatment).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, anti_caste_principle).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, structural_equality_doctrine).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, state_affirmative_obligation).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, hierarchy_dismantling_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that have experienced state-enforced racial hierarchy (Black, Indigenous, Latino, Asian American). The anti-caste reading legitimates race-conscious remedial programs (affirmative action, voting rights enforcement, school desegregation, disparate impact liability) as constitutional requirements rather than exceptions. They gain enforceable claims to structural remediation but remain constrained by political resistance and judicial retrenchment; exit from the constraint's framework means abandoning the constitutional vocabulary that makes remedial claims legible.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_racial_groups, beneficiary,
    organized, generational, constrained, national).

% Women and LGBTQ+ communities subject to gender-based caste hierarchies. The reading extends anti-caste logic to sex discrimination (intermediate scrutiny as anti-subordination, reproductive autonomy as caste exit, pregnancy discrimination as status enforcement). They benefit from heightened scrutiny of gender classifications that reinforce hierarchy but face contested application (e.g., whether anti-caste logic protects trans people). Exit is constrained by the same constitutional framework dependence.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_gender_groups, beneficiary,
    organized, generational, constrained, national).

% Groups marginalized by poverty, disability, immigration status, or criminal record — categories the anti-caste reading argues trigger heightened scrutiny when they function as caste markers. They gain potential constitutional leverage for positive rights (education, housing, healthcare) but the Court has largely rejected status-as-caste; their beneficiary position is aspirational and politically fragile.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_status_groups, beneficiary,
    moderate, generational, constrained, national).

% Courts (especially lower federal courts during the Warren/Burger era), EEOC, DOJ Civil Rights Division, state civil rights agencies, school districts under desegregation orders. These institutions administer the remedial machinery the reading authorizes — they gain jurisdiction, resources, and institutional mission from the anti-caste mandate. They can pivot enforcement priorities (arbitrage-grade exit) but their structural role depends on the mandate's vitality.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, state_corrective_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__anti_caste_reading, state_corrective_institutions, beneficiary).

% The diffuse fiscal base bearing the cost of court-ordered remedies (busing, affirmative action administration, voting rights compliance, institutional reform litigation). They experience the constraint as extraction without direct remedial benefit; their exit is constrained by the tax obligation and the democratic process's failure to limit judicial remedies.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, taxpayers_funding_remedial_programs, payer,
    moderate, biographical, constrained, national).

% Employers, universities, government contractors, school districts, voting jurisdictions — entities compelled by disparate impact doctrine, affirmative action mandates, or consent decrees to restructure practices. They bear compliance costs, litigation risk, and loss of discretionary authority. Exit is constrained by pervasive regulatory reach; some (universities) have partial arbitrage through private status but remain subject to Title VI/IX.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, institutions_subject_to_affirmative_obligations, payer,
    powerful, biographical, constrained, national).

% Applicants, employees, voters, or students who experience race- or gender-conscious policies as individualized exclusion (e.g., white or Asian applicants to selective universities, white voters in majority-minority districts). They bear the concentrated cost of the remedial transfer. Exit is constrained — they cannot opt out of the constitutional framework that authorizes the policy, and political remedies require supermajority coalitions.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, individuals_denied_colorblind_treatment, payer,
    moderate, biographical, constrained, national).

% Originalist jurists, colorblind constitutionalists, conservative legal movement (Federalist Society, EDF, Pacific Legal Foundation) — they argue the Fourteenth Amendment forbids race-consciousness categorically. They are excluded from the anti-caste reading's beneficiary structure (it treats their position as caste-preserving) but retain mobile exit: they can advance their reading in courts, legislatures, and public discourse, and have successfully captured the Supreme Court.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, formal_equality_advocates, excluded,
    powerful, biographical, mobile, national).

% Legal historians, political philosophers, critical race theorists, originalist scholars — analysts who map the reading's genealogy, internal coherence, and empirical consequences. They neither collect nor pay; they furnish the interpretive vocabulary in which the contest is framed.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of dismantling entrenched caste hierarchies that no individual or private group can dismantle alone: the state must affirmatively restructure institutions, redistribute opportunities, and police status-enforcing practices across society — a coordination task that requires authoritative compulsion and resource mobilization.
% TRANSFER_FUNCTION: Moves material resources (funding for remedial programs, compliance costs), institutional authority (decision-making discretion from local actors to courts/agencies), and status positions (admissions slots, jobs, political representation) from taxpayers, incumbent institutions, and non-beneficiary individuals to historically subordinated groups and the corrective institutions that serve them.
% ABSENT_VOICES: Future generations who will inherit the remediated or unremediated social structure; non-citizen residents subject to caste-like exclusions (undocumented immigrants, territorial populations) whom the reading's citizenship-anchored logic may not reach; the global majority for whom U.S. constitutional caste analysis is exported as a model without their consent.
% DISAPPEARANCE_RATIONALE: If the anti-caste reading vanished overnight, the constitutional architecture of race-conscious remediation (affirmative action, disparate impact, voting rights preclearance, school desegregation orders, Section 1983 municipal liability) would lose its doctrinal foundation. The remedial state would shrink dramatically; hierarchy would persist without constitutional constraint; the civil rights enforcement apparatus would lose its mandate.
% FOUNDING_PROBLEM: The Reconstruction Congress confronted a South that had abolished slavery but reconstructed racial caste through Black Codes, KKK terrorism, and the withdrawal of federal protection. The Equal Protection Clause was enacted to empower the federal government to dismantle this re-entrenching hierarchy — not merely to forbid explicit racial classifications but to authorize affirmative national action against the substance of caste.
% FOUNDING_PROBLEM_CORROBORATION: The Congressional debates (Howard, Bingham, Stevens, Trumbull), the Civil Rights Act of 1866, the Enforcement Acts of 1870-71, and the Freedmen's Bureau legislation corroborate the anti-caste purpose from the enacting coalition. The Slaughterhouse Cases (1873), Cruikshank (1876), and the Compromise of 1877 corroborate the counter-reading from the retrenchment coalition. Contemporary originalist scholars (e.g., McConnell, Calabresi) contest the anti-caste reading's historical scope; critical race theorists (Bell, Crenshaw, Delgado) and liberal constitutionalists (Brest, Ely, Sunstein) corroborate its continuity with Reconstruction's radical purpose.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__anti_caste_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__anti_caste_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__anti_caste_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the substantial transfer of resources, authority, and opportunity the remedial state effects — court-ordered busing, affirmative action administration, consent decree compliance, voting rights preclearance costs. The rise from Reconstruction (0.15) through the civil rights era (0.38-0.55) to the contemporary period (0.68) tracks the expansion of remedial obligations (disparate impact, affirmative action, institutional reform litigation) and their entrenchment in the administrative state. Suppression (0.42) is moderate: the constraint's persistence depends on active enforcement (court orders, agency action, private litigation) but alternatives exist (political repeal, judicial reversal, constitutional amendment) — the formal equality reading has successfully captured the Supreme Court, demonstrating exit is possible. Theater ratio (0.28) reflects that while remedial programs have real operational substance, a growing share of enforcement activity performs compliance without achieving structural remediation (diversity bureaucracy, symbolic inclusion, box-checking). Accessibility collapse (0.35) is low: the anti-caste reading coexists with the formal equality reading; alternatives are not collapsed. Resistance (0.61) is high: the reading has faced sustained political, judicial, and intellectual opposition from Reconstruction's overthrow to the present anti-DEI movement.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (subordinated groups, corrective institutions) experience the constraint as coordination — it solves a collective-action problem they cannot solve alone. The payer seats (taxpayers, compelled institutions, excluded individuals) experience it as extraction — they bear costs without consent or direct benefit. The agenda-setter seat (corrective institutions) straddles both: it administers the coordination and captures institutional rents. The excluded seat (formal equality advocates) experiences it as illegitimate imposition but retains the power to dismantle it. The engine computes this divergence from the declared structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups (racial, gender, status) are structural beneficiaries: the constraint channels state capacity and resources toward their remediation (d near beneficiary end). State corrective institutions are agenda-setters who also benefit institutionally (mission, resources, jurisdiction — d near beneficiary end but with arbitrage exit). Taxpayers, compelled institutions, and individuals denied colorblind treatment are payers bearing concentrated costs with constrained exit (d near target end). Formal equality advocates are excluded from the beneficiary structure but retain mobile exit through judicial and political channels. Constitutional theorists are analytical observers. The beneficiary/victim declarations map to real structural relationships: the remedial transfer is the constraint's operational core.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (dismantling Reconstruction-era racial caste) was live in 1868, contested through Jim Crow, live again in the civil rights era, and is now contested — formal equality advocates argue caste is dismantled and the mandate is obsolete; anti-caste advocates argue caste has mutated (mass incarceration, wealth gap, voting suppression) and the mandate remains live. The mandate has not been resolved; it has expanded to gender and status hierarchies the framers did not anticipate. Mandatrophy is unresolved: the constraint persists because the coordination problem persists, but extraction has accumulated beyond the original remedial scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    caste_boundary_contestation,
    'Which hierarchies count as ''caste'' triggering the anti-caste mandate? Race (settled), gender (intermediate), poverty/disability/immigration status (rejected by Court but claimed by theorists)?',
    'Supreme Court doctrine on suspect classifications; legislative expansion of protected classes; social movement pressure to extend anti-caste logic.',
    'If caste boundary expands, beneficiary set grows, extractiveness rises, constraint type may shift toward snare (if coordination function does not scale). If boundary contracts to race-only, extraction falls but coordination legitimacy narrows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_boundary_contestation, conceptual, 'The contested boundary of what counts as caste under the anti-caste reading').

omega_variable(
    remedial_proportionality,
    'Is there a structural limit on remedial extraction proportional to the caste injury, or does the mandate authorize open-ended transfer until hierarchy is ''dismantled'' (a condition with no clear metric)?',
    'Court articulation of remedial endpoint (e.g., ''unitary status'' in desegregation, ''narrow tailoring'' in affirmative action); political resistance threshold; institutional capacity to administer remedies.',
    'If no structural limit exists, extractiveness trends toward 1.0 and the constraint becomes a snare (coordination cover for unbounded extraction). If limits are enforceable, the tangled rope character is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedial_proportionality, conceptual, 'Whether the anti-caste mandate contains an internal proportionality limit on extraction').

omega_variable(
    kernel_reading_foreclosure,
    'Does the anti-caste reading''s core premise (Equality Clause requires affirmative hierarchy-dismantling) logically foreclose the formal equality reading''s core premise (Equality Clause forbids race-conscious classification), or do they coexist as competing frameworks?',
    'Doctrinal analysis: can a single constitutional framework simultaneously require race-conscious remediation and forbid race-conscious classification? Historical practice: have jurisdictions ever operated both logics simultaneously?',
    'If forecloses, the kernel is a binary fork — one reading''s victory is the other''s logical elimination. If coexists_with, the kernel is a permanent contested space where both readings operate in different institutional sites (courts vs. agencies, federal vs. state). This determines the reading_relation and the kernel''s structural dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Structural relationship between the two readings of the Equal Protection kernel').

omega_variable(
    extraction_coordination_boundary,
    'At what point does the remedial state''s extraction exceed the coordination function''s requirements — i.e., when does the tangled rope become a snare?',
    'Empirical measurement of remedial program costs vs. measured hierarchy reduction; cost-benefit analysis of specific remedies (busing, affirmative action, disparate impact litigation); political economy of the civil rights enforcement apparatus.',
    'If extraction systematically exceeds coordination necessity, the constraint reclassifies toward snare for payer seats. If coordination necessity scales with extraction, tangled_rope holds. This is the central classificatory ambiguity for the payer seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_coordination_boundary, empirical, 'The boundary between genuine remedial coordination and extractive overreach').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 1868, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_tr_t1868, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1868, 0.1).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_tr_t1877, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1877, 0.6).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_tr_t1896, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1896, 0.85).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_tr_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1954, 0.35).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_tr_t1964, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1964, 0.25).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_tr_t1978, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1978, 0.28).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_tr_t1995, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1995, 0.32).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_tr_t2013, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2013, 0.25).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_tr_t2023, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2023, 0.28).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_tr_t2026, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_be_t1868, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1868, 0.15).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_be_t1877, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1877, 0.08).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_be_t1896, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1896, 0.05).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_be_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1954, 0.22).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_be_t1964, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1964, 0.38).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_be_t1978, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1978, 0.55).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_be_t1995, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_be_t2013, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2013, 0.58).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_be_t2023, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2023, 0.68).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_be_t2026, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_su_t1868, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1868, 0.7).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_su_t1877, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1877, 0.2).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_su_t1896, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1896, 0.1).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_su_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1954, 0.45).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_su_t1964, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1964, 0.55).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_su_t1978, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1978, 0.48).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_su_t1995, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_su_t2013, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2013, 0.38).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_su_t2023, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2023, 0.42).
narrative_ontology:measurement(fourteenth_amendment_equal_protection__anti_caste_reading_su_t2026, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2026, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__anti_caste_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fourteenth_amendment_equal_protection__anti_caste_reading, 0.12).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection__formal_equality_reading).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, civil_rights_act_1964_title_vii).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, voting_rights_act_1965_preclearance).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, affirmative_action_jurisprudence).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, disparate_impact_doctrine).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, school_desegregation_orders).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, section_1983_municipal_liability).

% DUAL FORMULATION NOTE:
% This constraint and formal_equality_reading form the Fourteenth Amendment Equal Protection constraint family. The anti-caste reading treats the remedial state as coordination; the formal equality reading treats it as extraction. Their ε values diverge because they assess different standing arrangements: anti-caste ε evaluates the hierarchy-dismantling mandate; formal equality ε evaluates the race-conscious classification regime. They are linked by affects_constraints in both directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__anti_caste_reading, institutional, 0.15).
constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__anti_caste_reading, powerful, 0.85).
constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__anti_caste_reading, moderate, 0.75).
constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__anti_caste_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
