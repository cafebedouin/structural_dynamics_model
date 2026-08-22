% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__critical_psychiatry_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__critical_psychiatry_reading, []).

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
 *   constraint_id: dsm_taxonomy_kernel__critical_psychiatry_reading
 *   human_readable: DSM Diagnostic Taxonomy as Reverse-Engineered Pharmaceutical Market Construction
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This story instantiates the critical psychiatry reading of the DSM
 *   taxonomy kernel: the claim that diagnostic categories are substantially
 *   reverse-engineered from available or in-development pharmaceutical
 *   compounds rather than discovered through independent biological research,
 *   and that this reversal serves to construct and expand markets for
 *   psychotropic drugs. Under this reading, the coordination function of a
 *   shared diagnostic vocabulary is real but has been substantially captured
 *   by pharmaceutical capital and industry-funded clinicians who sit on the
 *   bodies that revise and expand the categories. This is a distinct
 *   constraint from the biomedical reading (which holds DSM categories track
 *   discoverable neurobiological disease entities) and the neurodiversity
 *   reading (which holds DSM categories pathologize normal variation against
 *   institutional norms) — each reading has its own ε, victim set, and
 *   beneficiary structure and is authored as a separate story per the
 *   ε-invariance principle.
 *
 * KEY AGENTS:
 *   - pharmaceutical_manufacturers: institutional beneficiary funding trials and CME that shape category boundaries
 *   - industry_funded_psychiatrists: agenda-setters on revision task forces with financial ties to matched treatments
 *   - overprescribed_patients: powerless, trapped payers bearing prescribing volume shaped by category expansion
 *   - patients_with_adverse_drug_effects: powerless payers whose harms are frequently reframed as illness progression
 *   - critical_psychiatry_researchers: excluded voices publishing dissenting evidence outside the closed revision process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.68).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.61).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM Diagnostic Taxonomy as Reverse-Engineered Pharmaceutical Market Construction").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, '2b546a55-4783-48c9-aba4-717c55fd445f').
narrative_ontology:cs_kernel_codification('2b546a55-4783-48c9-aba4-717c55fd445f', formalized).
narrative_ontology:cs_authority_grounding('2b546a55-4783-48c9-aba4-717c55fd445f', extraction).
narrative_ontology:cs_interpretation_layer_present('2b546a55-4783-48c9-aba4-717c55fd445f').
narrative_ontology:cs_reading_relation('2b546a55-4783-48c9-aba4-717c55fd445f', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b546a55-4783-48c9-aba4-717c55fd445f', dsm_taxonomy_kernel__neurodiversity_reading, influences).
narrative_ontology:cs_axiom('2b546a55-4783-48c9-aba4-717c55fd445f', foundational, diagnostic_categories_track_treatment_availability_not_independent_pathology).
narrative_ontology:cs_axiom_status(diagnostic_categories_track_treatment_availability_not_independent_pathology, holdable).
narrative_ontology:cs_axiom_grounding('2b546a55-4783-48c9-aba4-717c55fd445f', diagnostic_categories_track_treatment_availability_not_independent_pathology, empirically_contingent).
narrative_ontology:cs_axiom('2b546a55-4783-48c9-aba4-717c55fd445f', secondary, task_force_financial_entanglement_undermines_category_validity_claims).
narrative_ontology:cs_axiom_status(task_force_financial_entanglement_undermines_category_validity_claims, holdable).
narrative_ontology:cs_axiom_grounding('2b546a55-4783-48c9-aba4-717c55fd445f', task_force_financial_entanglement_undermines_category_validity_claims, empirically_contingent).
narrative_ontology:cs_reference_frame('2b546a55-4783-48c9-aba4-717c55fd445f', biomedical_diagnostic_reliability_standard).
narrative_ontology:cs_drift_state('2b546a55-4783-48c9-aba4-717c55fd445f', post_industry_funding_disclosure_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2b546a55-4783-48c9-aba4-717c55fd445f', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_task_force_members_with_industry_ties).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_reimbursement_apparatus).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, overprescribed_patients).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_with_adverse_drug_effects).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, misdiagnosed_children).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, long_term_psychiatric_drug_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, primary_care_prescribers).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, primary_care_prescribers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fund clinical trials, continuing medical education, and research chairs that shape which symptom clusters get codified as disorders matching existing or in-development compounds. Capture the revenue stream when a new or broadened diagnostic category expands the prescribing population.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Sit on DSM revision task forces and speakers' bureaus, receive consulting fees and honoraria from drug companies, and advocate for diagnostic thresholds and new categories that align with marketed or pipeline treatments. Careers, funding, and professional standing are built on the current nosology.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists, beneficiary).

% Vote on category inclusion, threshold criteria, and boundary revisions. Financial disclosure requirements are weak or unenforced; many members retain consulting relationships with the same companies whose drugs match the categories under revision.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_task_force_members_with_industry_ties, agenda_setter,
    institutional, generational, constrained, national).

% Requires a DSM code to authorize reimbursement for medication and treatment. Benefits from a stable, expanding taxonomy that channels care into billable, drug-centered treatment pathways rather than harder-to-code psychosocial interventions.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_reimbursement_apparatus, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_reimbursement_apparatus, agenda_setter).

% Receive a diagnosis matched to an available drug rather than a full accounting of etiology, and are placed on medication regimens they did not seek to question. Face social, insurance, and employment consequences for declining or discontinuing treatment; discontinuation itself can be difficult due to withdrawal effects.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, overprescribed_patients, payer,
    powerless, biographical, trapped, national).

% Experience physical, cognitive, or metabolic harms from psychotropic medications prescribed on the basis of DSM categories with weak biological validity. Have limited recourse: harms are frequently reframed as underlying illness worsening rather than iatrogenic effect, which increases rather than decreases prescribing.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_with_adverse_drug_effects, payer,
    powerless, biographical, trapped, national).

% Behavioral variation is coded against categories (e.g., expanding attention and mood disorder criteria) that happen to match approved pediatric formulations. Cannot consent meaningfully and depend entirely on caregivers and clinicians whose judgment is shaped by the same taxonomy and its promotional infrastructure.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, misdiagnosed_children, payer,
    powerless, biographical, trapped, national).

% Remain on medication regimens for years or decades once a diagnostic label is attached, with tapering support and withdrawal research chronically under-resourced relative to prescribing infrastructure. Identity and self-understanding become organized around the diagnosis over time.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, long_term_psychiatric_drug_users, payer,
    powerless, generational, trapped, national).

% Publish evidence of diagnostic inflation, industry influence on task forces, and weak biomarker validity for DSM categories. Represented in academic literature but structurally marginal in the DSM revision process itself, which retains its own closed nomination and voting procedure.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, critical_psychiatry_researchers, excluded,
    moderate, generational, constrained, national).

% Rely on DSM codes to justify billing and treatment decisions under time-constrained visits, without capacity to independently evaluate diagnostic validity. Benefit from a workable shorthand but bear liability and moral burden when the shorthand proves wrong for a given patient.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, primary_care_prescribers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, primary_care_prescribers, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__critical_psychiatry_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared diagnostic vocabulary that allows clinicians, insurers, and researchers to communicate about symptom clusters using common codes rather than idiosyncratic descriptions — a genuine coordination problem in a fragmented care system.
% TRANSFER_FUNCTION: Moves prescribing volume, reimbursement dollars, and research funding from patients and insurance pools toward pharmaceutical manufacturers and clinicians with financial ties to those manufacturers, mediated by diagnostic categories whose boundaries are shaped by drug availability rather than independent disease discovery.
% ABSENT_VOICES: Patients harmed by long-term psychotropic use, independent researchers without industry funding, and critical psychiatry clinicians are represented in the broader literature but are structurally outside the closed nomination and disclosure processes that actually set DSM category boundaries.
% DISAPPEARANCE_RATIONALE: If the current DSM taxonomy vanished overnight, insurance billing, prescribing algorithms, disability determinations, school accommodation processes, and research funding streams would all require reconstruction around a different classification system — the taxonomy is deeply load-bearing for administrative and commercial infrastructure, which is itself evidence that its persistence is not explained by biological validity alone.
% FOUNDING_PROBLEM: Psychiatry needed a shared, reliable classification system so clinicians could communicate consistently, insurers could authorize treatment, and researchers could compare study populations across institutions — replacing the earlier era of idiosyncratic, psychoanalytically-inflected diagnosis with something resembling medical nosology.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of psychiatry (outside pharmaceutical industry funding) and critical psychiatry researchers attest that the original reliability problem has been partially addressed but has been supplanted by a category-proliferation dynamic driven by treatment availability; industry-funded task force members and manufacturers attest the taxonomy tracks genuine, evolving scientific understanding. No party fully outside either the psychiatric establishment or patient advocacy stands as a neutral corroborator, which is itself noted as a limitation of this genealogy.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__critical_psychiatry_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__critical_psychiatry_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__critical_psychiatry_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__critical_psychiatry_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__critical_psychiatry_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-high (0.68 at interval end) reflecting the reading's core claim: profit extraction via pharmaceutical sales channeled through diagnostic categories whose boundaries move opportunistically toward available compounds. It is not authored as maximal because a genuine coordination function (shared clinical vocabulary, insurance interoperability) persists alongside the extraction — this is precisely why the reading is tangled_rope rather than snare. Theater ratio rises across the interval (0.30 to 0.55) reflecting this reading's view that an increasing share of DSM revision activity (task force deliberation, disclosure statements, evidence review) is procedural cover for outcomes substantially predetermined by industry relationships. Suppression is moderate (0.61) — the mechanism is less coercive exclusion of alternatives and more the closed, credential-gated nomination process that keeps critical psychiatry voices structurally outside the room, plus the practical suppression patients face in questioning or discontinuing treatment once diagnosed.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical manufacturers and industry-funded psychiatrists sit at the beneficiary end: they capture revenue and professional standing from the taxonomy's operation and have arbitrage-grade or constrained-but-influential exit relative to the structure they help author. Patients — overprescribed, adverse-effect-bearing, misdiagnosed children, and long-term users — sit at the target end: powerless, trapped, bearing costs with no meaningful exit once diagnosed and medicated. Insurance apparatus and primary care prescribers are dual-positioned: they benefit from a workable administrative shorthand while also bearing liability and moral costs from the shorthand's failures, hence the secondary_role designations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diagnostic reliability and shared clinical vocabulary) is authored as contested rather than flatly dead: some genuine coordination benefit persists, which is why this reading classifies as tangled_rope rather than pure snare. But the corroboration field records that independent historians and critical psychiatry researchers — sources outside the beneficiary set — attest the original problem has been substantially supplanted by a category-proliferation dynamic tied to treatment availability, while industry-tied task force members deny this. This mismatch (contested status, world_rearranges disappearance verdict) is exactly the signal the R5 consumer is built to surface: a genealogy attested primarily from inside the benefiting parties is treated as weak corroboration, not dispositive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reverse_engineering_vs_discovery_causal_direction,
    'Does the causal arrow actually run from available treatments to diagnostic category construction, or does it run from genuine (if imperfect) disease discovery to treatment development that happens to follow the biology, with industry influence operating only at the margins of threshold-setting?',
    'Archival analysis of DSM revision committee correspondence and internal memos (where available through litigation discovery or historical research) tracing whether proposed category boundaries were adjusted to match compounds already in a manufacturer''s pipeline, versus compounds being developed in response to independently characterized clinical presentations.',
    'If the reverse-engineering direction is substantiated broadly across categories, this reading''s high extractiveness and tangled_rope classification are strongly supported. If the direction is shown to run predominantly from discovery to treatment with only isolated capture incidents, this reading would need to be narrowed to specific categories rather than the taxonomy as a whole, and the biomedical reading would gain relative credibility for the remainder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reverse_engineering_vs_discovery_causal_direction, empirical, 'Whether category construction follows treatment availability or independent disease discovery.').

omega_variable(
    kernel_framing_choice_committer_axis,
    'Is the DSM taxonomy properly understood as one kernel with three contested readings (biomedical, critical psychiatry, neurodiversity), or are these actually describing three structurally different objects (a nosological classification system, a market-construction mechanism, and a normalization apparatus) that happen to share the same document?',
    'This is a conceptual/framing question rather than an empirical one. The decomposition into three linked stories (per network.affects_constraints) reflects a judgment that a single underlying kernel — ''what DSM categories are for and how they came to exist'' — is being read three ways by three different epistemic communities, rather than three unrelated phenomena being confused under one label. An alternative framing would decompose further by individual diagnostic category (e.g., a separate kernel per disorder) since the reverse-engineering claim plausibly applies unevenly across categories (stronger for some mood and behavioral disorders, weaker for e.g. psychotic disorders with clearer phenomenological continuity).',
    'If the alternative per-category framing is adopted, this story''s ε would need to be split further — some DSM categories would score much higher extractiveness under critical psychiatry analysis (e.g., disruptive mood dysregulation disorder, adult ADHD) and others much lower (e.g., schizophrenia, autism spectrum presentations with strong early-childhood markers). The current story''s ε=0.68 is a category-weighted average that could obscure this variance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice_committer_axis, conceptual, 'Whether the kernel should be read as one taxonomy-wide contest or decomposed per diagnostic category.').

omega_variable(
    task_force_disclosure_adequacy,
    'Do current financial conflict-of-interest disclosure requirements for DSM revision task force members adequately capture the influence pathways this reading alleges, or does industry influence operate through channels (informal relationships, institutional funding to academic departments, prior career benefit) that disclosure rules do not reach?',
    'Comparative analysis of disclosed financial relationships against independently documented industry funding flows (e.g., through pharmaceutical payment databases) for task force members across multiple DSM revision cycles.',
    'If disclosed relationships substantially undercount actual industry ties, the beneficiary set and enforcement mechanism (closed task force process) are more tightly coupled than currently modeled, supporting higher suppression and extractiveness scores. If disclosure is found broadly adequate, the mechanism this reading alleges would need a different explanation than direct financial capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(task_force_disclosure_adequacy, empirical, 'Whether disclosed conflicts of interest capture the actual influence mechanism this reading alleges.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(dsm__tr_t8, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(dsm__tr_t16, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement(dsm__tr_t24, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 24, 0.46).
narrative_ontology:measurement(dsm__tr_t32, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 32, 0.51).
narrative_ontology:measurement(dsm__tr_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(dsm__be_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dsm__be_t8, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(dsm__be_t16, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(dsm__be_t24, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(dsm__be_t32, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(dsm__be_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dsm__su_t8, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(dsm__su_t16, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(dsm__su_t24, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(dsm__su_t32, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(dsm__su_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.1).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__neurodiversity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the dsm_taxonomy_kernel. The biomedical_reading authors a much lower extractiveness (treating category boundaries as tracking discoverable neurobiology, with error but not systemic capture). The neurodiversity_reading authors a different victim set entirely (neurologically atypical individuals pathologized against institutional norms, rather than patients harmed by pharmaceutical overprescription) and a different mechanism (normalization pressure rather than profit extraction). All three share the same kernel text and codification but diverge sharply on beneficiary structure, victim set, and ε — per the ε-invariance principle, these are three separate constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
