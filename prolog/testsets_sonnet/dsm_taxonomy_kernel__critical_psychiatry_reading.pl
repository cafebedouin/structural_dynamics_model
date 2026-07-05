% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__critical_psychiatry_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: DSM Taxonomy as Pharmaceutical-Market Reverse Engineering (Critical Psychiatry Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This story instantiates the critical psychiatry reading of the DSM
 *   taxonomy kernel: the claim that diagnostic category boundaries have been
 *   shaped, edition over edition, by the availability and marketability of
 *   pharmaceutical treatments rather than by prior, independent discovery of
 *   neurobiological disease entities. Under this reading, categories such as
 *   pediatric bipolar disorder, expanded ADHD criteria, and broadened
 *   mood-disorder thresholds tracked drug development and marketing timelines
 *   closely enough, and DSM task force financial conflicts of interest were
 *   documented extensively enough, that the taxonomy functions partly as a
 *   market-construction device layered onto a genuine (if imperfect)
 *   coordination function — clinicians, insurers, and courts do need a shared
 *   vocabulary. This is TANGLED ROPE, not pure snare: the coordination
 *   function is real (shared diagnostic language enables billing, research
 *   comparability, and treatment access), but it is coupled to asymmetric
 *   extraction (pharmaceutical capital and industry-linked psychiatrists
 *   profit from category construction; overprescribed and adverse-effect
 *   patients bear the cost) and requires active enforcement (professional
 *   gatekeeping of who may diagnose, insurance requirements tied to DSM
 *   codes, marginalization of dissenting researchers).
 *
 * KEY AGENTS:
 *   - pharmaceutical_manufacturers: primary beneficiary (institutional/arbitrage) — profits from category-treatment matching
 *   - industry_funded_psychiatrists: beneficiary and agenda-setter (organized/identity_locked) — drafts criteria while receiving industry funding
 *   - dsm_task_force_members_with_industry_ties: agenda-setter (institutional/identity_locked) — administers the formal revision process
 *   - overprescribed_patients: primary target (powerless/trapped) — bears prescribing and diagnostic consequences
 *   - critical_psychiatry_researchers: excluded voice (organized/constrained) — documents the conflicts but lacks committee power
 *   - independent_epidemiologists: analytical observer (analytical/analytical) — studies prevalence and reliability trends
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.68).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.61).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.57).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.57).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM Taxonomy as Pharmaceutical-Market Reverse Engineering (Critical Psychiatry Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, '9a01e921-4428-430e-9905-f388376513e5').
narrative_ontology:cs_kernel_codification('9a01e921-4428-430e-9905-f388376513e5', formalized).
narrative_ontology:cs_authority_grounding('9a01e921-4428-430e-9905-f388376513e5', extraction).
narrative_ontology:cs_interpretation_layer_present('9a01e921-4428-430e-9905-f388376513e5').
narrative_ontology:cs_reading_relation('9a01e921-4428-430e-9905-f388376513e5', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a01e921-4428-430e-9905-f388376513e5', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('9a01e921-4428-430e-9905-f388376513e5', foundational, diagnostic_boundaries_track_commercial_treatment_availability).
narrative_ontology:cs_axiom_status(diagnostic_boundaries_track_commercial_treatment_availability, holdable).
narrative_ontology:cs_axiom_grounding('9a01e921-4428-430e-9905-f388376513e5', diagnostic_boundaries_track_commercial_treatment_availability, empirically_contingent).
narrative_ontology:cs_axiom('9a01e921-4428-430e-9905-f388376513e5', secondary, financial_conflict_of_interest_in_kernel_administration_undermines_claimed_neutrality).
narrative_ontology:cs_axiom_status(financial_conflict_of_interest_in_kernel_administration_undermines_claimed_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('9a01e921-4428-430e-9905-f388376513e5', financial_conflict_of_interest_in_kernel_administration_undermines_claimed_neutrality, empirically_contingent).
narrative_ontology:cs_reference_frame('9a01e921-4428-430e-9905-f388376513e5', dsm_iii_operationalized_criteria_standard).
narrative_ontology:cs_drift_state('9a01e921-4428-430e-9905-f388376513e5', post_conflict_of_interest_disclosure_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9a01e921-4428-430e-9905-f388376513e5', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_task_force_members_with_industry_ties).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, private_psychiatric_hospital_chains).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, overprescribed_patients).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, children_diagnosed_with_expanding_categories).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_experiencing_adverse_drug_effects).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, off_label_prescription_recipients).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, insured_populations_bearing_drug_costs).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__critical_psychiatry_reading, medical_necessity_of_treatment_doctrine).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatric_diagnostic_reliability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fund clinical trials, sponsor continuing medical education, and support key opinion leaders who sit on DSM revision committees. Benefit directly when new or broadened categories (e.g. expanded mood disorder or ADHD criteria) create prescribing populations for drugs already in the pipeline or off-patent-adjacent. Can shift marketing globally regardless of any single jurisdiction's regulatory response.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Serve on DSM task forces and subcommittees while receiving consulting fees, speaker honoraria, or research funding from drug manufacturers. Draft and vote on diagnostic criteria that determine which conditions are billable and treatable with existing drugs. Their professional identity and career advancement are bound to the diagnostic categories they helped construct, making genuine reconsideration costly to their own standing.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists, agenda_setter).

% Administer the formal revision process for each DSM edition, deciding which proposed categories survive committee review. A documented majority of task force members across multiple editions disclosed financial relationships with pharmaceutical companies. They control the taxonomy's content and could in principle decouple category construction from treatment availability, but doing so would require rejecting frameworks many built their careers on.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_task_force_members_with_industry_ties, agenda_setter,
    institutional, generational, identity_locked, global).

% Bill insurers and patients using DSM codes as the basis for admission, length of stay, and billable services. Broader or more numerous diagnostic categories expand the population eligible for reimbursable inpatient and outpatient treatment.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, private_psychiatric_hospital_chains, beneficiary,
    organized, biographical, mobile, national).

% Receive a diagnosis whose criteria were shaped by which drugs already existed, then take medication that carries side effects, withdrawal risk, or limited long-term efficacy evidence. Rarely have the clinical training to contest a diagnosis or independently evaluate whether the category itself is a coherent disease entity; exit means either non-compliance (with social and medical costs) or finding a dissenting clinician, both difficult.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, overprescribed_patients, payer,
    powerless, biographical, trapped, local).

% Are diagnosed under criteria that have broadened over successive DSM editions (e.g. pediatric bipolar disorder, ADHD) coinciding with expanded pharmaceutical marketing to that age group. Have no capacity for informed consent or independent exit; decisions are made by parents and clinicians operating inside the same diagnostic and treatment infrastructure.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, children_diagnosed_with_expanding_categories, payer,
    powerless, generational, trapped, national).

% Bear physical and psychological harms from psychotropic medications prescribed under diagnostic categories whose validity is itself contested. Adverse effects are often attributed to the underlying 'illness' rather than the treatment, closing off the interpretive path back to reconsidering the diagnosis.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_experiencing_adverse_drug_effects, payer,
    powerless, biographical, trapped, local).

% Are prescribed drugs for diagnostic uses beyond the original trial indications, often as diagnostic boundaries expand to match a drug's marketable profile rather than the reverse. Have limited access to the underlying trial data distinguishing tested from marketed uses.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, off_label_prescription_recipients, payer,
    powerless, biographical, constrained, national).

% Pay, through premiums and copays, for a psychotropic prescribing base whose diagnostic boundaries have expanded over time. Cannot individually negotiate the diagnostic criteria that determine what is billable as treatment.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, insured_populations_bearing_drug_costs, payer,
    moderate, biographical, constrained, national).

% Publish evidence of industry influence on DSM committees, low inter-rater reliability of categories, and mismatch between diagnostic expansion and independent epidemiological need. Are structurally outnumbered on the committees that actually set the taxonomy and are frequently characterized as fringe or anti-psychiatry within the profession, limiting their influence on the kernel itself.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, critical_psychiatry_researchers, excluded,
    organized, biographical, constrained, global).

% Study prevalence trends, diagnostic reliability, and prescribing patterns across DSM editions without direct stake in either the pharmaceutical industry or psychiatric professional bodies. Their analyses are often cited by critics of the taxonomy but do not themselves set diagnostic policy.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, independent_epidemiologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__critical_psychiatry_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The DSM does solve a real coordination problem: it gives clinicians, insurers, researchers, and courts a shared vocabulary for what would otherwise be idiosyncratic clinical judgment, enabling billing, epidemiological tracking, and cross-institutional communication about mental distress.
% TRANSFER_FUNCTION: Moves money from patients, insurers, and public health budgets to pharmaceutical manufacturers, industry-linked psychiatrists, and hospital chains, via a diagnostic vocabulary whose category boundaries are shaped in significant part by which treatments are already on the market rather than by independent disease discovery.
% ABSENT_VOICES: Patients diagnosed under contested or newly expanded categories have essentially no seat on DSM revision committees. Critical psychiatry researchers publish extensively but hold a small minority of committee seats relative to industry-connected members, and their reliability and validity critiques are frequently marginalized within mainstream professional discourse rather than adjudicated on the evidence.
% DISAPPEARANCE_RATIONALE: If the DSM's category-treatment feedback loop were severed overnight — diagnostic criteria fixed independently of treatment availability and industry funding removed from committee composition — prescribing volumes for several major drug classes would likely fall, hospital billing categories would need restructuring, and a substantial portion of continuing medical education and research funding tied to current categories would need new justification. The taxonomy is not free-standing; a large commercial and professional apparatus is organized around its current boundaries.
% FOUNDING_PROBLEM: Psychiatry needed a shared, reliable diagnostic vocabulary to replace idiosyncratic clinical impressions, enable research comparability, and support insurance reimbursement — the stated founding purpose of DSM-III's move to operationalized criteria.
% FOUNDING_PROBLEM_CORROBORATION: Independent epidemiologists and critical psychiatry researchers outside the pharmaceutical and psychiatric-professional beneficiary set attest that diagnostic reliability remains poor for many categories and that category expansion has tracked treatment marketing rather than independent epidemiological discovery (documented in published financial-conflict-of-interest audits of DSM task forces and prescribing-trend studies). Industry-funded psychiatrists and pharmaceutical manufacturers attest the founding problem (need for reliable, treatable diagnostic categories) remains live and that current criteria reflect improved science; this attestation comes entirely from within the benefiting parties.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__critical_psychiatry_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__critical_psychiatry_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored moderate-high (0.68 at interval end) reflecting profit extraction via expanded prescribing bases, but not maximal — genuine diagnostic need exists for many patients and not every category expansion is purely commercial. Suppression (0.61) reflects professional gatekeeping (only credentialed psychiatrists diagnose), insurance requirements tied to DSM codes, and the marginalization of dissenting research within mainstream psychiatric discourse — but alternatives (second opinions, non-pharmacological treatment, critical psychiatry literature) are not fully foreclosed, keeping suppression below snare-level. Theater ratio rises across the interval (0.30 to 0.57) reflecting a documented pattern: as financial conflict-of-interest disclosure requirements were introduced for later DSM editions, disclosure itself became partly performative — conflicts are disclosed but committee composition and voting power remain largely unchanged, so the appearance of accountability has grown faster than the substance. Accessibility collapse is moderate (0.48): patients and clinicians do retain some ability to seek alternative frameworks, but institutional and insurance structures make this costly. Resistance is moderate (0.55): critical psychiatry as a field has grown, but remains a minority position within mainstream psychiatric institutions.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (task force members, industry-funded psychiatrists), the DSM revision process looks like evidence-based clinical science responding to accumulating research. From the payer seats (patients across all victim groups), the same process delivers diagnoses and prescriptions whose boundaries happen to track what is profitably treatable. The engine should compute divergent seat-level classifications from this same structural data; the divergence is exactly what a tangled rope predicts — the coordination story is genuinely available from one seat and the extraction is genuinely available from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical manufacturers and industry-funded psychiatrists sit near the full-beneficiary end: they collect the surplus generated by category-treatment matching and can shift resources or claims globally. DSM task force members with industry ties administer the kernel and are identity-locked by career investment in the categories they authored, even though they are structurally positioned to change it. Patients across all victim groups sit near the full-target end: trapped or constrained exit, no seat in the revision process, and the costs (financial, physical, psychological) flow through the same diagnostic structure that determines their treatment options. Insured populations bear diffuse cost with moderate power (collective bargaining through employers/insurers exists but is indirect).
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (shared diagnostic vocabulary) has not become fully obsolete — clinicians and researchers still need common terms. What has drifted is the coupling between that vocabulary's boundaries and treatment marketability. Treating this as tangled_rope rather than snare avoids mislabeling all diagnostic classification as pure extraction (some categories track real, treatment-independent distress patterns) while still registering that the specific mechanism — criteria shaped by industry-funded committee members using treatment availability as an input to category construction — is a genuine extractive layer riding on the coordination function, not incidental to it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_treatment_causal_direction,
    'Do diagnostic category boundaries drive treatment development, or does treatment availability drive category boundary construction — or is the causal arrow genuinely bidirectional and inseparable at this point in psychiatric history?',
    'Historical analysis of DSM revision committee minutes and drafts cross-referenced against pharmaceutical patent and marketing timelines, examining whether proposed criteria changes preceded or followed the availability of a matching drug class.',
    'If treatment availability demonstrably preceded and shaped specific criteria (rather than following independently established criteria), this substantially strengthens the critical psychiatry reading''s core causal claim; if criteria changes generally preceded treatment development, the reading weakens toward the biomedical reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_treatment_causal_direction, empirical, 'Whether treatment-market construction or independent diagnostic discovery is causally prior.').

omega_variable(
    reliability_vs_conflict_of_interest_weighting,
    'How much of the DSM''s documented low inter-rater reliability for certain categories is attributable to industry-influenced criteria construction versus the inherent difficulty of operationalizing psychiatric phenomena at all?',
    'Comparative reliability studies of categories with documented high industry involvement in their construction versus categories with minimal industry involvement, controlling for phenomenon complexity.',
    'If low reliability tracks industry involvement specifically, this supports treating the extraction as a distinct mechanism separable from ordinary diagnostic difficulty; if reliability problems are uniform regardless of industry involvement, the critical psychiatry reading''s causal story is weaker and the extraction may be better explained by other factors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reliability_vs_conflict_of_interest_weighting, empirical, 'Whether diagnostic unreliability is attributable to industry influence specifically or to psychiatric taxonomy''s inherent difficulty.').

omega_variable(
    sibling_reading_incommensurability,
    'Are the critical_psychiatry_reading, biomedical_reading, and neurodiversity_reading genuinely incompatible accounts of the same kernel, or do they describe different, non-overlapping subsets of DSM categories (some categories market-constructed, some biologically grounded, some pathologizing normal variation)?',
    'Category-by-category structural analysis distinguishing which DSM diagnoses show strong evidence for each reading, rather than treating the DSM as a monolithic kernel requiring one dominant reading.',
    'If the readings partition cleanly by category, each reading is locally correct for its subset and the kernel-level contest is partly a category error; if they genuinely compete for the same categories, the contest is real and unresolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_incommensurability, conceptual, 'Whether the three kernel readings compete over the same categories or partition the diagnostic manual.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(dsm__tr_t9, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 9, 0.36).
narrative_ontology:measurement(dsm__tr_t18, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 18, 0.42).
narrative_ontology:measurement(dsm__tr_t27, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 27, 0.48).
narrative_ontology:measurement(dsm__tr_t36, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 36, 0.53).
narrative_ontology:measurement(dsm__tr_t45, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 45, 0.57).

% Extraction over time
narrative_ontology:measurement(dsm__be_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dsm__be_t9, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 9, 0.45).
narrative_ontology:measurement(dsm__be_t18, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 18, 0.53).
narrative_ontology:measurement(dsm__be_t27, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 27, 0.6).
narrative_ontology:measurement(dsm__be_t36, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 36, 0.65).
narrative_ontology:measurement(dsm__be_t45, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 45, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dsm__su_t9, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 9, 0.46).
narrative_ontology:measurement(dsm__su_t18, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 18, 0.51).
narrative_ontology:measurement(dsm__su_t27, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 27, 0.55).
narrative_ontology:measurement(dsm__su_t36, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 36, 0.58).
narrative_ontology:measurement(dsm__su_t45, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 45, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.08).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_patent_extension_practices).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatric_insurance_reimbursement_coding).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the dsm_taxonomy_kernel: biomedical_reading (DSM categories map to objective neurobiological disease entities), critical_psychiatry_reading (this story — categories reverse-engineered from available treatments to construct pharmaceutical markets), and neurodiversity_reading (categories pathologize natural neurological variation against institutional norms). Each reading has its own epsilon, beneficiary/victim structure, and classification, per the epsilon-invariance principle. The biomedical_reading, if authored, would likely classify closer to rope or mountain (framing diagnostic categories as convergent with discoverable natural kinds, negligible extraction); the neurodiversity_reading would likely classify as tangled_rope or snare with a different victim framing (normal-variation individuals rather than overprescription victims) and a different beneficiary framing (institutions enforcing behavioral conformity rather than pharmaceutical capital specifically). All three link to each other via affects_constraints since they are structurally coupled through the shared kernel text (the DSM itself) even though they instantiate distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
