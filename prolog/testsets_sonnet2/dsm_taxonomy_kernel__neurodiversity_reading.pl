% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__neurodiversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__neurodiversity_reading, []).

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
 *   constraint_id: dsm_taxonomy_kernel__neurodiversity_reading
 *   human_readable: DSM Taxonomy as Institutional Conformity Enforcement (Neurodiversity Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction
 *
 * SUMMARY:
 *   The DSM's behavioral-symptom categories for conditions like autism, ADHD,
 *   and oppositional defiant disorder are, on the neurodiversity reading,
 *   built from clinical presentations gathered inside institutions (schools,
 *   clinics, courts) that already impose narrow behavioral tolerances.
 *   Individuals whose processing, attention, or social communication patterns
 *   diverge from those tolerances are diagnosed as disordered rather than the
 *   institutional design being questioned. Diagnosis becomes the sole
 *   legitimate gateway to accommodation, funding, and legal protection, which
 *   locks neurodivergent individuals into a deficit framing as the price of
 *   access to support they should be entitled to as a baseline.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.78).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.72).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Taxonomy as Institutional Conformity Enforcement (Neurodiversity Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical_epistemology/psychiatric_taxonomy/social_construction").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, '641dbf6e-d221-4774-98b4-e9003541c0d0').
narrative_ontology:cs_kernel_codification('641dbf6e-d221-4774-98b4-e9003541c0d0', formalized).
narrative_ontology:cs_authority_grounding('641dbf6e-d221-4774-98b4-e9003541c0d0', expertise).
narrative_ontology:cs_interpretation_layer_present('641dbf6e-d221-4774-98b4-e9003541c0d0').
narrative_ontology:cs_reading_relation('641dbf6e-d221-4774-98b4-e9003541c0d0', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('641dbf6e-d221-4774-98b4-e9003541c0d0', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('641dbf6e-d221-4774-98b4-e9003541c0d0', foundational, neurological_variation_is_not_intrinsically_pathological).
narrative_ontology:cs_axiom_status(neurological_variation_is_not_intrinsically_pathological, holdable).
narrative_ontology:cs_axiom_grounding('641dbf6e-d221-4774-98b4-e9003541c0d0', neurological_variation_is_not_intrinsically_pathological, empirically_contingent).
narrative_ontology:cs_axiom('641dbf6e-d221-4774-98b4-e9003541c0d0', secondary, impairment_is_substantially_environment_contingent).
narrative_ontology:cs_axiom_status(impairment_is_substantially_environment_contingent, holdable).
narrative_ontology:cs_axiom_grounding('641dbf6e-d221-4774-98b4-e9003541c0d0', impairment_is_substantially_environment_contingent, empirically_contingent).
narrative_ontology:cs_reference_frame('641dbf6e-d221-4774-98b4-e9003541c0d0', distress_and_impairment_diagnostic_standard).
narrative_ontology:cs_drift_state('641dbf6e-d221-4774-98b4-e9003541c0d0', post_dsm5_field_trial_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('641dbf6e-d221-4774-98b4-e9003541c0d0', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, public_school_systems).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, employers_requiring_standard_performance).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, carceral_and_disciplinary_institutions).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, special_education_bureaucracies).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, insurance_and_disability_administration_systems).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, autistic_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, adhd_diagnosed_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, children_labeled_oppositional_defiant).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_adults_denied_accommodation).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, families_coerced_into_normalization_treatment).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__neurodiversity_reading, institutional_behavioral_uniformity_is_functional_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers classroom behavioral standards calibrated to a narrow band of attention, stillness, and social responsiveness. Uses DSM categories (ADHD, ODD, ASD) to sort students who cannot or will not meet those standards into diagnostic pathways that justify segregation, medication referral, or disciplinary tracking, rather than restructuring the classroom itself. Bears none of the diagnostic cost and gains a legible administrative mechanism for managing deviation from its own design choices.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, public_school_systems, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, public_school_systems, beneficiary).

% Sets workplace norms around eye contact, small talk, fixed-hour presence, and multitasking that reflect one neurotype's comfort zone. Uses psychiatric diagnosis as the sole legitimate gateway to accommodation, shifting the burden of proof onto the individual to be certified as deficient rather than onto the workplace to justify why its norms are the only acceptable configuration.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, employers_requiring_standard_performance, beneficiary,
    institutional, generational, arbitrage, national).

% Uses conduct-disorder and oppositional-defiant framings to individualize the pathology of resistance to institutional authority, converting structural conflicts (over-policing, punitive discipline, sensory-hostile environments) into diagnosable traits located inside the person being disciplined.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, carceral_and_disciplinary_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Administers funding streams and staffing formulas keyed to diagnostic codes. Depends on a steady supply of certified diagnoses to justify budget lines and specialist positions, which creates institutional interest in maintaining rather than narrowing the diagnostic net, even where the underlying variation is benign.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, special_education_bureaucracies, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, special_education_bureaucracies, agenda_setter).

% Requires a DSM code as the sole acceptable evidentiary basis for reimbursement, accommodation, or benefit eligibility. This administrative gatekeeping function locks the entire accommodation-seeking population into needing a pathology label regardless of whether the underlying difference causes distress or only friction with institutional design.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, insurance_and_disability_administration_systems, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, insurance_and_disability_administration_systems, beneficiary).

% Neurological processing differences in sensory integration, social communication, and attention allocation are coded as a deficit disorder rather than a variant configuration. Diagnosis is required to access any accommodation, but the diagnosis itself carries stigma, coercive intervention risk (ABA-style compliance training), and a permanent institutional record framing the person as disordered. Exit from the diagnostic system means losing all accommodation access.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, autistic_individuals, payer,
    powerless, biographical, trapped, national).

% Attention and activity patterns that fall outside classroom and office design tolerances are diagnosed and frequently medicated to restore compliance with fixed-schedule, seated, sustained-attention environments, rather than the environments being redesigned. Medication access itself often requires accepting and internalizing the deficit framing.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, adhd_diagnosed_individuals, payer,
    powerless, biographical, constrained, national).

% Resistance to authority, often a rational response to punitive, unresponsive, or unsafe environments, is individualized as a disorder located in the child. Has no voice in the diagnostic process, no exit from the institutions applying it, and no mechanism to contest that the institutional environment itself might be the causal factor.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, children_labeled_oppositional_defiant, payer,
    powerless, biographical, trapped, national).

% Adults whose processing differences do not rise to a threshold recognized by DSM criteria, or who cannot access formal diagnosis due to cost or provider scarcity, are denied any accommodation at all, since the system recognizes no legitimate claim to difference outside a certified pathology.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_adults_denied_accommodation, payer,
    powerless, biographical, constrained, national).

% Parents are told that compliance-training interventions aimed at extinguishing autistic traits (stimming, echolalia, atypical eye contact) are the only evidence-recognized path to school inclusion and future employability, placing them under pressure to pursue normalization treatment their child may experience as harmful, in order to secure basic institutional access for that child.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, families_coerced_into_normalization_treatment, payer,
    moderate, biographical, constrained, national).

% Argue that diagnostic categories should describe difference and support accommodation, not certify defect as a precondition for institutional access. Have gained visibility in the last two decades but remain structurally outside DSM revision committees, which continue to be dominated by clinicians credentialed within the deficit-model tradition.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_advocates_and_autistic_self_advocates, excluded,
    moderate, generational, constrained, national).

% Revise diagnostic criteria through consensus committee processes, drawing on clinical presentation data gathered overwhelmingly within institutions applying conformity pressure (schools, clinics, courts) rather than from population-level studies of unmedicalized neurological variation. Positioned to observe the pattern but structurally embedded within the same institutions that benefit from its persistence.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, clinical_researchers_and_dsm_committee_members, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The taxonomy does solve a genuine coordination problem for institutions: it provides a shared, portable vocabulary that schools, insurers, employers, and courts can use to allocate scarce accommodation resources and staffing without each institution independently assessing every individual case from scratch.
% TRANSFER_FUNCTION: Moves the burden of adaptation from institutions (which could redesign environments, schedules, and behavioral expectations) onto individuals (who must accept a pathology label, pursue treatment, or go without accommodation), while moving diagnostic-driven funding, staffing budgets, and administrative legibility to the institutions applying the categories.
% ABSENT_VOICES: Neurodivergent adults who were never formally diagnosed, autistic self-advocates who reject the deficit framing entirely, and disabled children too young to participate in the committee process that defines the categories applied to them are structurally outside DSM revision, which is conducted by credentialed clinicians trained within the tradition the categories were built to serve.
% DISAPPEARANCE_RATIONALE: If the DSM's behavioral-conformity categories vanished overnight, schools, employers, and courts would lose their primary mechanism for individualizing responsibility for institutional friction; funding streams tied to diagnostic codes would collapse and require redesign around either universal accommodation or renegotiated behavioral standards; some individuals would lose formal accommodation access built on the diagnostic gateway, while others would be freed from a stigmatizing label that currently functions as their only route to support.
% FOUNDING_PROBLEM: Clinicians needed a shared vocabulary to identify genuine distress and functional impairment so that people in crisis could receive consistent care rather than idiosyncratic, provider-specific judgment.
% FOUNDING_PROBLEM_CORROBORATION: Clinical researchers and DSM committee members attest the categories still track genuine impairment and distress. Autistic self-advocates and disability studies scholars operating outside the committee structure attest that criteria have drifted from distress-and-impairment toward conformity-with-institutional-norms, citing the DSM-5's own field trials showing high false-positive rates for traits that are benign outside specific institutional settings; this corroboration comes from a constituency structurally excluded from the benefiting committee process, not from the institutions applying the categories.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__neurodiversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__neurodiversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__neurodiversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.78 by 2024) because the harm is not incidental to the taxonomy but constitutive of it on this reading: the categories extract self-determination and dignity by requiring individuals to accept a disorder label to receive support, and extract institutional cost-savings by locating the fix inside the individual (medication, behavioral training) rather than in redesigned environments. Suppression is high (0.72) because exit from the diagnostic gateway typically means exit from all accommodation. Theater ratio is moderate (0.4) and rising, reflecting a growing gap between symbolic 'neurodiversity-affirming' institutional language and continued reliance on deficit-coded diagnostic gatekeeping for actual resource allocation.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional agenda-setter seats this looks like efficient, evidence-based resource triage. From the payer seats it looks like coerced self-pathologization as the price of survival inside institutions that will not accommodate difference without a medical certificate. The engine computes this divergence from the declared power/exit structure; the claimed_type (tangled_rope) reflects that a genuine coordination function (shared triage vocabulary) coexists with asymmetric extraction (self-determination cost borne only by the diagnosed).
 *
 * DIRECTIONALITY LOGIC:
 *   Institutions that administer or fund diagnostic-triggered services (schools, employers, insurers, carceral systems) are the structural beneficiaries: the taxonomy gives them a portable, low-cost mechanism for allocating resources and assigning responsibility for friction, at no cost to their own design choices. Diagnosed individuals and their families are the structural targets: they bear stigma, coercive treatment risk, and the burden of proving deficiency, and their exit options are trapped or constrained because accommodation access is conditioned on accepting the label.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — clinicians needing a shared vocabulary for genuine distress and impairment — is only partly dead: severe distress and functional impairment remain real and the taxonomy still serves some of that function. But on the neurodiversity reading, a substantial share of current diagnostic activity has drifted toward certifying non-distressing variation as disorder solely because it conflicts with institutional design, which is the mandatrophied residue: the mandate expanded to cover conformity enforcement long after its narrower clinical justification was satisfied. Tangled Rope rather than Snare or Piton captures this because the coordination function persists in genuine use alongside the extractive expansion — collapsing the two would either exonerate the extraction (calling it pure Rope) or deny the coordination function's continued reality (calling it pure Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    variation_vs_disorder_boundary,
    'Where, if anywhere, is the principled line between neurological variation that causes intrinsic distress/impairment independent of environment, and variation that only produces friction because of institutional design choices?',
    'Cross-cultural and cross-environmental studies measuring whether functional impairment for a given trait profile persists across radically different institutional/environmental designs (e.g. self-paced vs. fixed-schedule work, sensory-adapted vs. standard classrooms); if impairment is environment-invariant, the biomedical reading gains support for that trait; if impairment is environment-contingent, the neurodiversity reading gains support.',
    'Resolving this would determine how much of the DSM''s current diagnostic net should be read as tracking genuine intrinsic impairment (reducing this story''s extractiveness) versus institutional conformity enforcement (sustaining or raising it).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(variation_vs_disorder_boundary, conceptual, 'Whether impairment is environment-invariant or environment-contingent for the traits in question.').

omega_variable(
    committer_kernel_reading_disagreement,
    'This story is one of three declared readings of the dsm_taxonomy_kernel (biomedical_reading: categories map to objective neurobiological disease; critical_psychiatry_reading: categories are reverse-engineered from available pharmaceutical treatments; neurodiversity_reading: categories pathologize institutional-conformity-violating variation, authored here). The disagreement is located in what the diagnostic criteria are ultimately tracking: disease process, market opportunity, or environmental mismatch.',
    'No single empirical test adjudicates all three at once; each reading is falsifiable on different evidence (biomarker discovery for biomedical_reading, pharmaceutical industry funding/authorship analysis of DSM revision panels for critical_psychiatry_reading, environment-invariance testing per the omega above for this reading). The readings are not mutually exclusive for every category — some DSM entries may be best read biomedically, others as critical_psychiatry captures, others as neurodiversity mismatches.',
    'If this reading is adopted for a given category, the appropriate policy response is environmental redesign and de-linking accommodation from diagnosis; if the biomedical_reading is adopted, the response is continued biomarker research; if critical_psychiatry_reading is adopted, the response is structural reform of DSM revision funding and conflict-of-interest rules. The three readings license incompatible remedies, which is why they are authored as separate constraints rather than one story with a parameter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_disagreement, conceptual, 'Where the three kernel readings of DSM taxonomy locate the disagreement, and why no single test adjudicates all three.').

omega_variable(
    self_advocate_representation_gap,
    'Would DSM revision committees produce structurally different criteria for autism, ADHD, and ODD if autistic and neurodivergent self-advocates held voting seats proportional to those most affected, rather than advisory or absent status?',
    'Compare criteria proposals and revision outcomes in jurisdictions or working groups that have experimentally included self-advocate voting members against the standard clinician-only committee process.',
    'If criteria would differ substantially, this corroborates the excluded-voices structure identified in six_questions as a live driver of the extraction, rather than a merely rhetorical grievance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_advocate_representation_gap, empirical, 'Whether self-advocate committee representation would materially change diagnostic criteria.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(dsm__tr_t1990, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(dsm__tr_t2010, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2010, 0.34).
narrative_ontology:measurement(dsm__tr_t2018, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2018, 0.38).
narrative_ontology:measurement(dsm__tr_t2024, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(dsm__be_t1990, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2000, 0.64).
narrative_ontology:measurement(dsm__be_t2010, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2010, 0.71).
narrative_ontology:measurement(dsm__be_t2018, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2018, 0.75).
narrative_ontology:measurement(dsm__be_t2024, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(dsm__su_t1990, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2000, 0.63).
narrative_ontology:measurement(dsm__su_t2010, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2010, 0.67).
narrative_ontology:measurement(dsm__su_t2018, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement(dsm__su_t2024, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__neurodiversity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__neurodiversity_reading, 0.08).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the DSM' per the epsilon-invariance principle: biomedical_reading (categories track objective disease, low-to-moderate contested extraction), critical_psychiatry_reading (categories are reverse-engineered from pharmaceutical markets, high extraction via manufactured demand), and this neurodiversity_reading (categories pathologize institutional-nonconformity, high extraction via coerced self-pathologization and denial of self-determination). All three share the same kernel (the DSM's diagnostic-category structure and its gatekeeping authority) but diverge sharply on what the categories are ultimately tracking and therefore on beneficiary/victim structure and ε. They are linked via affects_constraints rather than merged because averaging or parameterizing across them would violate epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
