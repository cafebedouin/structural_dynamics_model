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
 *   human_readable: DSM Taxonomy as Pathologization of Neurological Variation (Neurodiversity Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This story instantiates the neurodiversity reading of the DSM taxonomy
 *   kernel: DSM categories such as autism spectrum disorder, ADHD, and
 *   oppositional defiant disorder are read not as descriptions of discrete
 *   disease entities but as administrative constructions that convert
 *   institutional demand for behavioral conformity into a medical deficit
 *   located in the individual. Under this reading, schools, employers, and
 *   carceral systems benefit from a diagnostic vocabulary that routes the
 *   cost of their own inflexibility onto neurodivergent people, who must
 *   either mask, comply, or be formally pathologized to access accommodation
 *   at all. The extraction is the pathologization itself — the imposition of
 *   a disorder narrative on variation that does not intrinsically require one
 *   — compounded by coercive normalization practices (compliance therapies,
 *   punitive disciplinary tracking, carceral escalation) justified by the
 *   diagnostic label.
 *
 * KEY AGENTS:
 *   - school_systems_requiring_behavioral_conformity: institutional agenda-setter and beneficiary, sorts children via diagnostic labeling
 *   - employers_requiring_standardized_workplace_performance: institutional beneficiary, externalizes accommodation cost onto diagnosed employees
 *   - carceral_and_juvenile_justice_systems: institutional beneficiary, converts behavioral difference into security risk via diagnostic framing
 *   - psychiatric_disability_administration_apparatus: agenda-setter and beneficiary, controls the taxonomy's boundaries and gatekeeps resource access
 *   - autistic_individuals / adhd_diagnosed_individuals / children_labeled_disruptive / neurodivergent_employees / neurodivergent_incarcerated_people: primary targets, bear the cost of both the diagnostic framing and coercive normalization
 *   - neurodiversity_advocates_and_disabled_researchers: excluded voice, argues for accommodation-first social model over disorder model
 *   - clinical_researchers_studying_neurodivergence: analytical observer, documents the gap between statistically continuous traits and discrete diagnostic thresholds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.72).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.68).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Taxonomy as Pathologization of Neurological Variation (Neurodiversity Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, '2c419fc5-d5d6-453e-a25f-fd2833f8cb62').
narrative_ontology:cs_kernel_codification('2c419fc5-d5d6-453e-a25f-fd2833f8cb62', formalized).
narrative_ontology:cs_authority_grounding('2c419fc5-d5d6-453e-a25f-fd2833f8cb62', expertise).
narrative_ontology:cs_interpretation_layer_present('2c419fc5-d5d6-453e-a25f-fd2833f8cb62').
narrative_ontology:cs_reading_relation('2c419fc5-d5d6-453e-a25f-fd2833f8cb62', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c419fc5-d5d6-453e-a25f-fd2833f8cb62', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('2c419fc5-d5d6-453e-a25f-fd2833f8cb62', foundational, variation_is_not_intrinsically_pathological).
narrative_ontology:cs_axiom_status(variation_is_not_intrinsically_pathological, holdable).
narrative_ontology:cs_axiom_grounding('2c419fc5-d5d6-453e-a25f-fd2833f8cb62', variation_is_not_intrinsically_pathological, conventional).
narrative_ontology:cs_axiom('2c419fc5-d5d6-453e-a25f-fd2833f8cb62', secondary, impairment_is_environmentally_relative_not_intrinsic).
narrative_ontology:cs_axiom_status(impairment_is_environmentally_relative_not_intrinsic, holdable).
narrative_ontology:cs_axiom_grounding('2c419fc5-d5d6-453e-a25f-fd2833f8cb62', impairment_is_environmentally_relative_not_intrinsic, empirically_contingent).
narrative_ontology:cs_reference_frame('2c419fc5-d5d6-453e-a25f-fd2833f8cb62', clinical_consensus_diagnostic_manual).
narrative_ontology:cs_drift_state('2c419fc5-d5d6-453e-a25f-fd2833f8cb62', post_social_model_disability_advocacy_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2c419fc5-d5d6-453e-a25f-fd2833f8cb62', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, school_systems_requiring_behavioral_conformity).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, employers_requiring_standardized_workplace_performance).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, carceral_and_juvenile_justice_systems).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, special_education_bureaucracies).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_disability_administration_apparatus).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, autistic_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, adhd_diagnosed_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, children_labeled_disruptive_in_schools).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_employees_denied_accommodation).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_people_in_carceral_settings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses DSM categories (ADHD, oppositional defiant disorder, autism spectrum disorder) to sort children into disciplinary and special-education tracks. A diagnosis routes a child into an administrative pathway that manages classroom disruption to a fixed curriculum and seating arrangement rather than adapting the environment; the school captures funding tied to diagnosed enrollment and offloads behavior management onto medical framing.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, school_systems_requiring_behavioral_conformity, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, school_systems_requiring_behavioral_conformity, beneficiary).

% Structures hiring, evaluation, and workplace norms around neurotypical communication and attention patterns, then relies on DSM diagnostic categories to determine who 'qualifies' for narrow, often burdensome accommodation processes rather than redesigning tasks or environments. Bears none of the cost of the categorization system while benefiting from a workforce sorted by conformity to its existing norms.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, employers_requiring_standardized_workplace_performance, beneficiary,
    institutional, biographical, arbitrage, national).

% Applies DSM categories to neurodivergent individuals whose behavior under stress, sensory overload, or communication difference is read as defiance or dangerousness, using diagnostic labels to justify restraint, seclusion, or extended sentencing rather than accommodation. The label transfers responsibility for institutional failure onto the individual's supposed disorder.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, carceral_and_juvenile_justice_systems, beneficiary,
    institutional, generational, arbitrage, national).

% Administers benefits, licensing, and accommodation eligibility entirely through DSM diagnostic gatekeeping. Maintains and revises the taxonomy through committee process, controlling which forms of neurological variation are legible to the state and which are not; the apparatus's continued authority depends on the categories remaining the exclusive gateway to recognition and resources.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_disability_administration_apparatus, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_disability_administration_apparatus, agenda_setter).

% Carries a diagnostic label built around deficit-framed behavioral checklists rather than a description of their actual cognitive style. The label determines access to accommodation but also invites coercive interventions (compliance-based therapies, forced eye contact, suppression of self-regulatory behaviors like stimming) aimed at normalizing appearance rather than improving functioning or wellbeing. Exit from the diagnostic framework is not meaningfully available — refusing the label forfeits accommodation and legal protection; accepting it means living inside a pathology narrative not of one's authorship.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, autistic_individuals, payer,
    powerless, civilizational, identity_locked, national).

% Diagnosed based on attention and activity patterns measured against a classroom or office norm rather than against any independent marker of dysfunction. Faces a binary: accept medication and behavioral compliance to remain inside institutional good standing, or be treated as willfully disruptive. Has limited power to contest the diagnostic frame that structures their treatment.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, adhd_diagnosed_individuals, payer,
    powerless, biographical, constrained, national).

% A child cannot opt out of compulsory schooling or contest a diagnostic label applied by adults with institutional authority over them. The label follows them through educational records, shaping teacher expectations and disciplinary treatment for years, often before the child has any capacity to understand or object to the framing being applied to their behavior.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, children_labeled_disruptive_in_schools, payer,
    powerless, biographical, trapped, local).

% Must produce a DSM-coded diagnosis to request workplace accommodation, then finds employers narrowly interpreting the accommodation as minimal and revocable rather than as evidence the workplace itself is designed around a specific neurotype. Changing jobs does not escape the pattern, since the same diagnostic gate structures accommodation everywhere in the labor market.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_employees_denied_accommodation, payer,
    moderate, biographical, constrained, national).

% Sensory sensitivities, communication differences, or stress responses characteristic of autism or ADHD are read through a security lens rather than a disability lens, often triggering escalation, isolation, or extended detention. Has essentially no capacity to contest how staff interpret and act on their behavior once inside the institution.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_people_in_carceral_settings, payer,
    powerless, immediate, trapped, local).

% Argue the diagnostic categories encode institutional convenience rather than biological pathology and push for a social-model reframing (accommodation-first, variation not disorder). Their input is solicited symbolically in some DSM revision processes but structurally outvoted by clinician and institutional stakeholders whose funding and legal frameworks depend on the disorder model persisting.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_advocates_and_disabled_researchers, excluded,
    moderate, generational, constrained, national).

% Studies the discordance between DSM category boundaries and underlying neurological and behavioral continua, publishing evidence that many diagnostic thresholds are statistically arbitrary cut points on continuous traits rather than discrete disease boundaries. Can document the gap between taxonomy and biology but has limited power to change how the categories are administratively deployed.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, clinical_researchers_studying_neurodivergence, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The DSM does solve a real coordination problem for institutions: it gives schools, insurers, employers, and courts a shared vocabulary for triaging limited accommodation and treatment resources without each institution independently re-litigating what counts as impairment. Under this reading, though, that coordination function is inseparable from an underlying category error — coordinating around a deficit model of variation that did not need to be framed as pathology at all.
% TRANSFER_FUNCTION: Moves the cost of institutional inflexibility (rigid classrooms, standardized workplace expectations, security-first carceral procedures) onto neurodivergent individuals, who must either absorb compliance costs (medication, masking, behavioral suppression) or lose access to education, employment, and liberty. Institutions receive administratively tractable populations and funding tied to diagnostic counts; individuals receive a deficit narrative and conditional, revocable accommodation.
% ABSENT_VOICES: Neurodivergent people whose variation does not map cleanly onto any DSM category, and disabled community members who reject the disorder framing entirely, are structurally underrepresented in DSM revision committees, which remain dominated by clinicians and institutional stakeholders with a professional and financial interest in the categorical model persisting.
% DISAPPEARANCE_RATIONALE: If DSM pathologization of these traits vanished overnight — replaced by a purely descriptive, non-pathologizing account of neurological variation — school disciplinary tracking, disability benefit eligibility, employment accommodation law, and carceral risk assessment would all have to be rebuilt on different footing, since all currently route through diagnostic gatekeeping. Institutions would lose a ready-made sorting mechanism and would have to build accommodation systems around actual functional need rather than diagnostic category membership.
% FOUNDING_PROBLEM: Clinicians needed a shared, reliable vocabulary to communicate about patients' distress and functional difficulty across practitioners, insurers, and researchers, replacing inconsistent, idiosyncratic diagnostic language with standardized criteria.
% FOUNDING_PROBLEM_CORROBORATION: Disabled self-advocates, several clinical psychologists working in the neurodiversity paradigm (e.g., writing on the social model of disability), and sociologists of medicine attest from outside the diagnostic-administration apparatus that the original communicative-standardization problem has been substantially overtaken by an institutional-sorting function; the American Psychiatric Association and allied diagnostic bodies, who administer and revise the DSM, maintain the categories track genuine biomedical entities and dispute this reading.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__neurodiversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__neurodiversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__neurodiversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 0.72, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored high (0.72 at interval end) because, under this reading, the harm is not merely denial of services but the pathologization act itself — the imposition of a disorder identity on variation, plus the coercive normalization practices that pathologization licenses (compliance-based therapy, disciplinary tracking, carceral escalation). Suppression is substantial (0.68) because exit from the diagnostic frame is largely unavailable: refusing the label forfeits legal accommodation, while accepting it locks the individual into an institutionally administered deficit narrative. Theater ratio is moderate (0.4) — much of DSM revision activity (field trials, committee review, cross-cultural validation studies) is real epistemic work, but a rising share of enforcement activity increasingly defends categorical boundaries against mounting evidence of dimensional, non-discrete trait distributions, which is where the theater component sits.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional beneficiaries (schools, employers, carceral systems, the administration apparatus) sit near the beneficiary end of directionality — they receive sorting, funding, and liability-management value from the categories while bearing none of the diagnostic label's personal cost. Neurodivergent individuals sit near the full-target end: they carry the label, the compliance burden, and the risk of coercive intervention, with identity-locked or trapped exit options depending on setting (school-age children and incarcerated individuals have essentially no exit; employees have constrained exit since the same diagnostic gate structures accommodation across the labor market).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — clinicians needing standardized communicative vocabulary — is authored as contested rather than flatly dead, because the coordination function (shared diagnostic language across practitioners and institutions) has genuine residual value even under this reading. What has drifted is the deployment: a tool built for clinical communication has become the primary gatekeeping mechanism for education, employment, and carceral treatment, well beyond its original clinical-communication scope. This is exactly the tangled_rope signature — a real coordination function persists (shared diagnostic vocabulary) while active enforcement channels asymmetric extraction (coercive normalization, denied self-determination) onto a specific population, rather than the pure-extraction snare pattern, since the coordination function is not merely cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_variation_vs_functional_impairment_boundary,
    'Is there a principled, reading-independent boundary between ''natural neurological variation'' and ''functional impairment warranting clinical intervention,'' or is that boundary itself always drawn relative to a specific institutional/behavioral norm?',
    'Cross-cultural and cross-institutional comparison: if the same neurological profile is diagnosed as disordered in one institutional context (standard classroom, competitive workplace) but unremarkable or even advantageous in another (different educational model, different labor structure), that supports the reading that the boundary tracks institutional norms rather than an intrinsic biological threshold.',
    'If a genuine reading-independent impairment boundary exists, some current diagnostic categories would retain biomedical validity even under this reading, narrowing the pathologization claim to the subset of categories that are demonstrably norm-relative rather than intrinsically impairing. If no such boundary exists, the pathologization claim generalizes across most current behavioral diagnostic categories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_variation_vs_functional_impairment_boundary, conceptual, 'Whether impairment is an intrinsic property of neurological profiles or relative to institutional context.').

omega_variable(
    kernel_reading_disagreement_locus,
    'The three readings of the dsm_taxonomy_kernel (biomedical, critical_psychiatry, neurodiversity) disagree fundamentally about WHERE the DSM categories originate — from biological research, from pharmaceutical market construction, or from institutional conformity demands. Which upstream causal story is most accurate for the historical construction of the categories this story addresses (ADHD, ASD, ODD)?',
    'Historical and sociological analysis of DSM revision committee composition, funding sources, and category-boundary changes across editions (DSM-III through DSM-5-TR), cross-referenced against pharmaceutical industry involvement and against institutional (educational, occupational, carceral) pressure documented in committee correspondence and public comment records.',
    'If institutional-conformity pressure is shown to be the dominant causal driver of category construction and boundary placement for these specific diagnoses, this reading is strongly corroborated. If pharmaceutical market incentives dominate, the critical_psychiatry_reading is favored instead. If neurobiological research findings dominate and institutional/pharmaceutical pressures are marginal, the biomedical_reading is favored. In practice the three factors likely co-exist and this omega is unlikely to resolve to a single dominant reading; it is retained to make explicit where this story''s classification would be vulnerable to disconfirmation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Contested causal origin of DSM categories — the locus of disagreement between the three kernel readings.').

omega_variable(
    self_report_vs_institutional_report_diagnostic_criteria,
    'To what extent do current diagnostic criteria for the categories in this story rely on institutional third-party reports of behavior (teacher, employer, corrections staff observations) versus the individual''s own account of internal experience and distress?',
    'Audit of DSM-5-TR criteria language and standard diagnostic instruments (e.g., behavior rating scales used in ADHD diagnosis) for source of evidence required — self-report versus third-party behavioral observation.',
    'Heavy reliance on third-party institutional observation as diagnostic evidence would further corroborate the reading that the categories operationalize institutional conformity expectations rather than individual distress or dysfunction; heavy reliance on self-reported internal distress would weaken the pathologization claim for those specific criteria.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_report_vs_institutional_report_diagnostic_criteria, empirical, 'Whether diagnostic evidence is sourced from institutional behavioral observation or individual self-report.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t0, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dsm__tr_t8, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(dsm__tr_t16, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(dsm__tr_t24, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(dsm__tr_t32, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(dsm__tr_t40, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(dsm__be_t0, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dsm__be_t8, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(dsm__be_t16, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(dsm__be_t24, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(dsm__be_t32, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 32, 0.69).
narrative_ontology:measurement(dsm__be_t40, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t0, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(dsm__su_t8, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(dsm__su_t16, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(dsm__su_t24, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement(dsm__su_t32, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 32, 0.67).
narrative_ontology:measurement(dsm__su_t40, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__neurodiversity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__neurodiversity_reading, 0.1).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the dsm_taxonomy_kernel, decomposed per the ε-invariance principle because 'the DSM' names structurally distinct claims depending on the observer's theory of where the categories come from. biomedical_reading claims the categories map to discoverable neurobiological disease entities (low ε, Mountain-leaning). critical_psychiatry_reading claims the categories are reverse-engineered to construct pharmaceutical markets (very high ε, Snare-leaning, victims are patients broadly and beneficiaries are pharmaceutical manufacturers). neurodiversity_reading (this story) claims the categories pathologize natural variation to serve institutional conformity demands (high ε, tangled_rope, victims are neurodivergent individuals and beneficiaries are conformity-requiring institutions). All three share the same underlying kernel — the DSM taxonomy as a codified, formally revised diagnostic authority — but diverge on the origin and function of the categories, producing different beneficiary/victim structures and different ε values. They are linked here rather than merged because merging would violate DP-001 ε-invariance: no single ε value could honestly represent all three causal stories at once.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
