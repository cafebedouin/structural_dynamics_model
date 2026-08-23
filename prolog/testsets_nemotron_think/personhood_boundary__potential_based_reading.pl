% ============================================================================
% CONSTRAINT STORY: personhood_boundary__potential_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__potential_based_reading, []).

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
 *   constraint_id: personhood_boundary__potential_based_reading
 *   human_readable: Personhood Grounded in Potential for Rational Agency (Potential-Based Reading)
 *   domain: moral_philosophy/commitment_systems
 *
 * SUMMARY:
 *   The potential-based reading of the personhood boundary holds that moral
 *   standing requires not merely human DNA or live birth, but the potential
 *   to develop rational agency — the capacity for self-governance, moral
 *   reasoning, and autonomous life-planning. Severely disabled infants (e.g.,
 *   anencephalic, severe hypoxic-ischemic injury, profound cortical
 *   malformation) are judged to lack this potential and therefore fall
 *   outside the personhood boundary. The constraint coordinates medical
 *   decision-making by providing a clear threshold for
 *   withdrawing/withholding life-sustaining treatment, but it does so by
 *   extracting moral standing from the most vulnerable. The assessment of
 *   'potential' is performed by medical authorities using neurological
 *   prognostication, creating an institutional power to define who counts.
 *   The coordination function (resolving tragic clinical uncertainty) and
 *   extraction function (denying standing to the cognitively impaired) are
 *   fused in the same clinical-legal structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, 0.78).
domain_priors:suppression_score(personhood_boundary__potential_based_reading, 0.72).
domain_priors:theater_ratio(personhood_boundary__potential_based_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__potential_based_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__potential_based_reading, "Personhood Grounded in Potential for Rational Agency (Potential-Based Reading)").
narrative_ontology:topic_domain(personhood_boundary__potential_based_reading, "moral_philosophy/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__potential_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__potential_based_reading, '63382f61-aeb8-46e5-9c3f-da995438aff7').
narrative_ontology:cs_kernel_codification('63382f61-aeb8-46e5-9c3f-da995438aff7', formalized).
narrative_ontology:cs_authority_grounding('63382f61-aeb8-46e5-9c3f-da995438aff7', lineage).
narrative_ontology:cs_interpretation_layer_present('63382f61-aeb8-46e5-9c3f-da995438aff7').
narrative_ontology:cs_reading_relation('63382f61-aeb8-46e5-9c3f-da995438aff7', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('63382f61-aeb8-46e5-9c3f-da995438aff7', personhood_boundary__fitness_contingent_reading, coexists_with).
narrative_ontology:cs_axiom('63382f61-aeb8-46e5-9c3f-da995438aff7', foundational, personhood_requires_rational_agency_potential).
narrative_ontology:cs_axiom_status(personhood_requires_rational_agency_potential, holdable).
narrative_ontology:cs_axiom_grounding('63382f61-aeb8-46e5-9c3f-da995438aff7', personhood_requires_rational_agency_potential, deontological).
narrative_ontology:cs_axiom('63382f61-aeb8-46e5-9c3f-da995438aff7', secondary, capacity_assessment_authority_rests_with_guardians).
narrative_ontology:cs_axiom_status(capacity_assessment_authority_rests_with_guardians, holdable).
narrative_ontology:cs_axiom_grounding('63382f61-aeb8-46e5-9c3f-da995438aff7', capacity_assessment_authority_rests_with_guardians, conventional).
narrative_ontology:cs_reference_frame('63382f61-aeb8-46e5-9c3f-da995438aff7', potentiality_metaphysics).
narrative_ontology:cs_drift_state('63382f61-aeb8-46e5-9c3f-da995438aff7', contemporary_bioethics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('63382f61-aeb8-46e5-9c3f-da995438aff7', '').
narrative_ontology:cs_kernel_id(personhood_boundary__potential_based_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, parental_guardians).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, medical_assessment_authorities).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, moral_legal_community).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, severely_disabled_infants).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, cognitively_impaired_children).
narrative_ontology:constraint_vindicates(personhood_boundary__potential_based_reading, rational_agency_as_personhood_criterion).
narrative_ontology:constraint_vindicates(personhood_boundary__potential_based_reading, potentiality_metaphysics_of_moral_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Infants with severe congenital cognitive impairments who are deemed to lack potential for rational agency. They bear the full cost of exclusion from moral personhood — no legal standing, no inherent right to life protections, medical decisions made entirely by others. They cannot exit, advocate, or consent; their situation is structurally fixed by the constraint's own judgment of their potential.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, severely_disabled_infants, payer,
    powerless, immediate, trapped, universal).

% Children who survive infancy but continue to be assessed as lacking rational agency potential. They remain in the excluded category throughout development, subject to substitute decision-making for all significant life matters. The constraint's application to them is the ongoing enforcement of the initial potential assessment.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, cognitively_impaired_children, payer,
    powerless, biographical, trapped, universal).

% Parents or legal guardians of severely disabled infants who hold authority to make life-and-death medical decisions based on capacity assessments. They benefit from decision-making control and social validation of their authority, but are also constrained by the assessment framework — they cannot simply choose personhood for their child; the potential judgment is made by medical authorities.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, parental_guardians, agenda_setter,
    moderate, biographical, constrained, universal).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, parental_guardians, beneficiary).

% Neonatologists, neurologists, bioethics committees, and legal frameworks that operationalize 'potential for rational agency' into clinical assessments. They set the thresholds, administer the tests, and their judgments carry legal force. They benefit from professional authority and institutional control over the personhood boundary, with exit options through career mobility and institutional repositioning.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, medical_assessment_authorities, agenda_setter,
    institutional, generational, arbitrage, universal).

% The broader society and legal system that gains a clear, administrable boundary for moral standing. The potential criterion provides a (putatively) objective line that simplifies resource allocation, legal protections, and medical prioritization. They benefit from reduced ambiguity but can shift to alternative frameworks through legislative or cultural change.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, moral_legal_community, beneficiary,
    organized, generational, mobile, universal).

% Advocates who argue that all human beings possess inherent moral standing regardless of cognitive capacity. They are structurally excluded from the assessment process — their testimony about the value of disabled lives is treated as ideological rather than evidentiary within the potential-based framework. Their exit is constrained because the framework itself defines their objections as conceptually confused.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, disability_rights_advocates, excluded,
    organized, generational, constrained, universal).

% Philosophers and ethicists who analyze the constraint from outside — tracking its theoretical coherence, empirical adequacy, and historical trajectory. They neither collect rents nor bear costs from the constraint's operation, but their analyses influence the legitimacy conditions of all readings.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, bioethics_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a putatively objective, administrable criterion for moral personhood that resolves borderline cases (anencephaly, severe cortical malformation, persistent vegetative state) by reference to future potential rather than current capacity or birth status.
% TRANSFER_FUNCTION: Moves moral standing and legal protections from severely disabled infants to parental guardians and medical authorities, who gain decision-making power over life-sustaining treatment, resource allocation, and legal personhood. The transfer is mediated by clinical assessments of 'rational agency potential.'
% ABSENT_VOICES: The infants and children themselves — who cannot speak, consent, or contest their classification. Disability rights advocates are structurally excluded from the assessment process; their framework (inherent dignity regardless of capacity) is treated as conceptually incompatible with the potential-based criterion rather than as a competing evidentiary claim.
% DISAPPEARANCE_RATIONALE: If the potential-based criterion vanished overnight, medical decision-making for severely disabled infants would shift to either a birth-threshold framework (all born humans protected) or a best-interests/substituted-judgment framework. Legal protections would expand, resource allocation protocols would change, and the authority of medical assessment bodies would be substantially reduced. The moral and legal landscape would reorganize around a different personhood boundary.
% FOUNDING_PROBLEM: Pre-modern and early modern medicine faced unresolvable uncertainty about which severely impaired newborns would develop meaningful consciousness. The potential criterion offered a metaphysically grounded, prospectively applicable line that avoided both infanticide-on-demand and obligatory vitalism for hopeless cases.
% FOUNDING_PROBLEM_CORROBORATION: Historical medical ethics texts (e.g., Ramsey, Engelhardt) attest the founding problem was genuine clinical uncertainty about prognosis. Disability rights scholars (e.g., Silvers, Francis, Kittay) and contemporary neonatologists attest the problem is substantially transformed: improved diagnostics, changed survival trajectories, and the disability rights critique have shifted the epistemic and moral landscape. The American Academy of Pediatrics' evolving guidelines corroborate the contested status.
narrative_ontology:disappearance_verdict(personhood_boundary__potential_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__potential_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__potential_based_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(personhood_boundary__potential_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__potential_based_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__potential_based_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__potential_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint transfers the most fundamental moral asset — personhood itself — from infants who cannot consent to authorities who control the assessment criteria. Suppression (0.72) is high because the constraint's persistence depends on actively maintaining the assessment framework (clinical guidelines, legal precedents, bioethics consensus statements) and marginalizing alternative frameworks (disability rights, birth-threshold). Theater ratio (0.45) is moderate: the clinical assessment function is real and tragic, but a growing share of the framework's energy goes into defending the boundary against disability-rights challenges rather than improving prognostic accuracy. Accessibility collapse (0.68) reflects that once the potential criterion is accepted, alternatives (birth-threshold, inherent dignity) appear conceptually incoherent from within the framework. Resistance (0.55) is substantial and growing — disability rights movements, some neonatologists, and legal scholars contest both the empirical basis of potential assessments and the moral legitimacy of the criterion.
 *
 * PERSPECTIVAL GAP:
 *   From the medical authority seat, the constraint appears as a tragic but necessary coordination mechanism — genuine uncertainty about prognosis demands a decision rule. From the infant/child seat, the same structure is pure extraction — a metaphysical criterion they cannot meet, administered by authorities they cannot challenge, removing the only protection they have. From the disability advocate seat, the constraint is a snare disguised as clinical judgment — the 'potential' assessment smuggles in a quality-of-life judgment that the framework officially denies making. The engine computes these divergences from the declared structural data; the authored claim (tangled_rope) acknowledges both coordination and extraction without adjudicating which dominates.
 *
 * DIRECTIONALITY LOGIC:
 *   Severely disabled infants are full targets (d → 1.0): they bear the total cost of exclusion, have zero exit, and the constraint's operation is defined by their classification. Cognitively impaired children are similarly trapped. Parental guardians are dual-positioned: they are agenda_setters who administer the constraint's judgments but are also constrained by the medical assessment framework — their d is moderate (~0.4). Medical authorities are near-beneficiaries (d → 0.1): they control the assessment criteria, gain professional authority, and have arbitrage-grade exit. The moral-legal community is a diffuse beneficiary (d ~0.3): gains clarity but can shift frameworks. Disability advocates are excluded — their structural position is outside the constraint's coordination logic entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (clinical uncertainty about prognosis in severe neonatal impairment) was genuine in 1960 but has been substantially transformed by: (1) improved neuroimaging and prognostic accuracy, (2) changed survival and quality-of-life trajectories for many conditions once deemed hopeless, (3) the disability rights critique exposing the quality-of-life assumptions embedded in 'potential' assessments. The constraint persists because the assessment infrastructure (bioethics committees, legal precedents, clinical guidelines) has become self-justifying — the mandate has outlived its founding epistemic conditions. This is a textbook mandatrophy case: the arrangement continues to extract from the vulnerable because the institutions that administer it have no incentive to revise the boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does the potential_based_reading''s structural relationship to the personhood_boundary kernel differ from its siblings, and what classification consequences follow from the committer-frame analysis?',
    'Comparative structural analysis of all three readings using the same ε-invariance discipline: each reading gets its own constraint story with its own ε, beneficiaries, victims, and type. The kernel context omega records the committer-frame relationships (forecloses/coexists_with) and the axioms that differentiate readings.',
    'If the forecloses relation to birth_threshold_reading is structurally valid, then any framework adopting potential_based_reading cannot simultaneously adopt birth_threshold_reading — this produces a genuine binary in the commitment system. If the coexists_with relation to fitness_contingent_reading holds, both can be live positions in the same discourse, creating a spectrum of capacity-based accounts. The engine uses reading_relations to compute cross-reading contamination and foreclosure dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commitment-system framing of the kernel contest: forecloses birth_threshold, coexists_with fitness_contingent.').

omega_variable(
    potential_assessment_empirical_validity,
    'Do clinical assessments of ''potential for rational agency'' in severely disabled infants have sufficient predictive validity to support the moral weight placed on them, or do they function as a veneer for quality-of-life judgments?',
    'Longitudinal studies comparing potential assessments at birth/infancy with actual developmental outcomes; meta-analysis of prognostic accuracy across conditions; audit of assessment criteria for covert quality-of-life assumptions.',
    'If assessments are empirically valid, the coordination function is genuine and extraction is a tragic byproduct of a necessary decision rule. If assessments are invalid or value-laden, the coordination story is cover and the constraint is a snare. This omega directly bears on the tangled_rope vs. snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(potential_assessment_empirical_validity, empirical, 'Whether the coordination function (prognostic assessment) is empirically adequate or a cover for extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative personhood frameworks (birth-threshold, inherent dignity) structural (legal-institutional barriers) or internalized (conceptual framework makes alternatives unthinkable)?',
    'Post-exit trajectory analysis: when jurisdictions adopt birth-threshold frameworks (e.g., some European neonatal protocols), does the potential-based conceptual framework persist in clinical practice? If suppression persists after legal barriers are removed, it is partially internalized.',
    'If internalized, effective suppression is higher than structural measures suggest — clinicians and parents carry the constraint''s logic even where the law no longer requires it. This would increase the constraint''s effective extraction for trapped agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the interpersonal/institutional personhood boundary.').

omega_variable(
    cs_framing_underdetermination,
    'Does the commitment-system analysis correctly identify the kernel as ''personhood_boundary'' with ''potentiality_metaphysics'' as reference frame, or is the kernel better framed as ''moral_status_of_the_vulnerable'' with ''capacity_assessment_authority'' as the operative commitment?',
    'Comparative CS analysis: author the alternative framing as a separate constraint story and compare classification outputs. If different framings produce different cs_pattern classifications or different drift_state assessments, the underdetermination is real and must be documented.',
    'If the kernel is ''moral_status_of_the_vulnerable'', the authority_grounding shifts from ''lineage'' (Aristotelian potentiality tradition) to ''extraction'' (institutional control over vulnerability classifications), changing the CS pattern from stabilized_interpolation to drift_denial. This would alter the mandate_resolution analysis and the predicted terminal attractor.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative CS framing of the same constraint: kernel identity and authority grounding ambiguity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__potential_based_reading, 1960, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pbpbr_tr_t1960, personhood_boundary__potential_based_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(pbpbr_tr_t1975, personhood_boundary__potential_based_reading, theater_ratio, 1975, 0.32).
narrative_ontology:measurement(pbpbr_tr_t1990, personhood_boundary__potential_based_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(pbpbr_tr_t2005, personhood_boundary__potential_based_reading, theater_ratio, 2005, 0.42).
narrative_ontology:measurement(pbpbr_tr_t2015, personhood_boundary__potential_based_reading, theater_ratio, 2015, 0.44).
narrative_ontology:measurement(pbpbr_tr_t2025, personhood_boundary__potential_based_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(pbpbr_be_t1960, personhood_boundary__potential_based_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement(pbpbr_be_t1975, personhood_boundary__potential_based_reading, base_extractiveness, 1975, 0.58).
narrative_ontology:measurement(pbpbr_be_t1990, personhood_boundary__potential_based_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(pbpbr_be_t2005, personhood_boundary__potential_based_reading, base_extractiveness, 2005, 0.74).
narrative_ontology:measurement(pbpbr_be_t2015, personhood_boundary__potential_based_reading, base_extractiveness, 2015, 0.77).
narrative_ontology:measurement(pbpbr_be_t2025, personhood_boundary__potential_based_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pbpbr_su_t1960, personhood_boundary__potential_based_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(pbpbr_su_t1975, personhood_boundary__potential_based_reading, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement(pbpbr_su_t1990, personhood_boundary__potential_based_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(pbpbr_su_t2005, personhood_boundary__potential_based_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(pbpbr_su_t2015, personhood_boundary__potential_based_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(pbpbr_su_t2025, personhood_boundary__potential_based_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__potential_based_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(personhood_boundary__potential_based_reading, 0.08).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__fitness_contingent_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, neonatal_treatment_withdrawal_protocols).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, disability_rights_legal_protections).

% DUAL FORMULATION NOTE:
% This constraint is one member of the personhood_boundary constraint family (kernel_id: personhood_boundary). The three readings — potential_based_reading (this story), birth_threshold_reading, and fitness_contingent_reading — decompose the colloquial 'personhood debate' into structurally distinct constraints with different ε values, victim sets, and coordination functions. The potential_based_reading forecloses the birth_threshold_reading (logical contradiction in a single framework) and coexists_with the fitness_contingent_reading (different threshold on the same capacity dimension). All three stories should be authored and linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personhood_boundary__potential_based_reading, moderate, 0.35).
constraint_indexing:directionality_override(personhood_boundary__potential_based_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
