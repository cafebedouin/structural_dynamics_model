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
 *   human_readable: DSM Taxonomy as Reverse-Engineered Pharmaceutical Market Construction
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This story instantiates the critical psychiatry reading of the DSM
 *   taxonomy kernel: that diagnostic category boundaries are substantially
 *   shaped backward from available and patentable psychotropic drugs rather
 *   than forward from independently validated biological mechanisms, and that
 *   this reverse-engineering serves a genuine (if now compromised)
 *   coordination function — shared clinical vocabulary and reimbursement
 *   standardization — while simultaneously constructing captive markets for
 *   pharmaceutical sales. The taxonomy is not claimed to be pure fabrication;
 *   it is claimed to be a hybrid structure where real coordination value has
 *   been substantially colonized by commercial interest embedded in the
 *   category-construction process itself, via task force composition,
 *   industry funding of the research base each revision cites, and
 *   threshold-setting that expands eligible populations for existing drugs.
 *
 * KEY AGENTS:
 *   - pharmaceutical_manufacturers: primary beneficiary (institutional/arbitrage) — funds the research base and CME infrastructure that shapes category boundaries
 *   - industry_funded_psychiatrists: agenda-setting seat (organized/arbitrage) — writes and revises the categories while holding financial relationships with beneficiaries
 *   - overprescribed_patients and patients_with_adverse_drug_effects: primary targets (powerless/trapped-constrained) — bear the taxonomy's downstream clinical consequences
 *   - critical_psychiatry_researchers: excluded analytical voice (moderate/constrained) — documents the pattern from outside the formal revision process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.68).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.58).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM Taxonomy as Reverse-Engineered Pharmaceutical Market Construction").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, '5d145d18-dbb8-48c8-b469-5a2bba6d1926').
narrative_ontology:cs_kernel_codification('5d145d18-dbb8-48c8-b469-5a2bba6d1926', formalized).
narrative_ontology:cs_authority_grounding('5d145d18-dbb8-48c8-b469-5a2bba6d1926', extraction).
narrative_ontology:cs_interpretation_layer_present('5d145d18-dbb8-48c8-b469-5a2bba6d1926').
narrative_ontology:cs_reading_relation('5d145d18-dbb8-48c8-b469-5a2bba6d1926', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d145d18-dbb8-48c8-b469-5a2bba6d1926', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('5d145d18-dbb8-48c8-b469-5a2bba6d1926', foundational, category_construction_tracks_commercial_availability).
narrative_ontology:cs_axiom_status(category_construction_tracks_commercial_availability, holdable).
narrative_ontology:cs_axiom_grounding('5d145d18-dbb8-48c8-b469-5a2bba6d1926', category_construction_tracks_commercial_availability, empirically_contingent).
narrative_ontology:cs_axiom('5d145d18-dbb8-48c8-b469-5a2bba6d1926', secondary, diagnostic_legitimacy_requires_independence_from_treatment_market).
narrative_ontology:cs_axiom_status(diagnostic_legitimacy_requires_independence_from_treatment_market, holdable).
narrative_ontology:cs_axiom_grounding('5d145d18-dbb8-48c8-b469-5a2bba6d1926', diagnostic_legitimacy_requires_independence_from_treatment_market, conventional).
narrative_ontology:cs_reference_frame('5d145d18-dbb8-48c8-b469-5a2bba6d1926', dsm_iii_reliability_reform_baseline).
narrative_ontology:cs_drift_state('5d145d18-dbb8-48c8-b469-5a2bba6d1926', post_industry_funding_disclosure_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5d145d18-dbb8-48c8-b469-5a2bba6d1926', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_task_force_members_with_industry_ties).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, overprescribed_patients).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_with_adverse_drug_effects).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_denied_nonpharmacological_care).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, insured_populations_bearing_drug_costs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fund clinical trials, continuing medical education, and research chairs that shape which symptom clusters get codified as discrete disorders matching drugs already in the development pipeline. Collect revenue directly from prescription volume generated once a diagnostic category is formalized and reimbursable.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Sit on DSM revision task forces and write treatment guidelines while receiving speaking fees, consulting income, and research funding from drug manufacturers. Set diagnostic thresholds and category boundaries; disclosure requirements are procedural rather than exclusionary, so financial relationships persist through revision cycles.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists, agenda_setter,
    organized, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists, beneficiary).

% Receive a formal diagnosis that opens insurance-reimbursable pharmaceutical treatment as the default first-line response, often without equal access to therapy, social intervention, or watchful waiting. Their exit options are limited by insurance structures that reimburse medication more readily than alternatives and by the social authority of the diagnostic label itself.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, overprescribed_patients, payer,
    powerless, biographical, constrained, national).

% Bear physical and psychological harms from medications prescribed on the basis of DSM categories whose boundaries were drawn to match available drugs rather than independently validated biological mechanisms. Discontinuation is often difficult due to withdrawal effects and lack of clinical support for tapering.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_with_adverse_drug_effects, payer,
    powerless, biographical, trapped, national).

% Would prefer or benefit more from therapy, social support, or lifestyle intervention but face insurance systems and short appointment structures built around the diagnose-and-medicate pathway that DSM categories enable. Their preference for alternatives is structurally underweighted.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_denied_nonpharmacological_care, payer,
    powerless, biographical, constrained, national).

% Pay elevated insurance premiums and public health expenditures that reflect the aggregate cost of high psychotropic prescription volume, without individually consenting to or benefiting from the taxonomic choices that drive that volume.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, insured_populations_bearing_drug_costs, payer,
    moderate, generational, constrained, national).

% Publish evidence questioning the validity, reliability, and industry entanglement of DSM categories but operate largely outside the task force and guideline-writing bodies that control the categories' formal adoption and reimbursement consequences.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, critical_psychiatry_researchers, excluded,
    moderate, generational, constrained, national).

% Publishes and licenses the DSM, controlling the formal taxonomy that determines insurance reimbursement eligibility, legal disability determinations, and treatment guideline construction. Derives substantial revenue from DSM licensing and sales while managing disclosure policies for task force members' industry relationships.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, american_psychiatric_association, agenda_setter,
    institutional, generational, arbitrage, national).

% Use DSM categories as the administrative basis for reimbursement decisions, drug approval indications, and disability determinations, largely deferring to the taxonomy's face validity rather than independently auditing its construction process.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, regulatory_and_insurance_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__critical_psychiatry_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A shared diagnostic vocabulary allows clinicians, insurers, researchers, and regulators to communicate about patient presentations using common terms, enabling billing, research replication, and treatment protocol standardization across a fragmented healthcare system.
% TRANSFER_FUNCTION: Moves diagnostic legitimacy and reimbursement eligibility toward symptom clusters matching existing patentable drugs, and moves prescription revenue from patients and insurers to pharmaceutical manufacturers, while moving disclosure and validation burden away from category architects.
% ABSENT_VOICES: Patients harmed by long-term psychotropic use, critical psychiatry researchers, and former patients advocating for withdrawal support are largely absent from DSM task force deliberations, which are dominated by clinician-researchers with active industry funding relationships.
% DISAPPEARANCE_RATIONALE: If the DSM taxonomy vanished overnight, insurance reimbursement structures, drug approval indications, disability determinations, research funding categories, and clinical training would all require reconstruction around some alternative framework — the entire administrative and commercial apparatus of psychiatric practice is keyed to these category boundaries.
% FOUNDING_PROBLEM: Clinicians needed a shared, reliable vocabulary to communicate about patient presentations, enable research replication across sites, and qualify psychiatry as a scientifically legitimate medical specialty comparable to other branches of medicine (particularly after the reliability crises of the 1960s-70s).
% FOUNDING_PROBLEM_CORROBORATION: APA leadership and industry-funded task force members attest the founding problem (diagnostic reliability, scientific legitimacy) remains live and is being actively addressed through ongoing revision. Independent researchers outside DSM task forces, congressional inquiry testimony on psychiatrist-industry financial ties, and internal whistleblower accounts from task force members (e.g. published critiques by former DSM-IV task force chair Allen Frances) attest that category expansion has outpaced genuine biological validation and increasingly tracks commercial opportunity rather than the original reliability problem.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__critical_psychiatry_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__critical_psychiatry_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored moderate-high (0.68 by interval end) reflecting genuine but partial capture: the DSM performs real coordination work (shared vocabulary, research replicability) that is not fabricated, but category boundaries and threshold placements have measurably tracked drug availability and patent timelines in documented cases (e.g. expansion of mood disorder and ADHD diagnostic criteria coinciding with new drug approvals). Theater ratio rises over the interval (0.30 to 0.62) reflecting the growing gap between the taxonomy's claimed empirical/biological grounding and its actual construction process — disclosure statements and conflict-of-interest policies function increasingly as procedural cover rather than substantive barriers to industry influence. Suppression is moderate (0.58): the mechanism is less coercive exclusion of alternatives and more structural — insurance reimbursement architecture, legal disability determinations, and research funding categories are all keyed to DSM codes, making genuine alternatives (dimensional models, formulation-based approaches) administratively costly to adopt even where clinically supported. All three metrics share the same six-point time grid.
 *
 * PERSPECTIVAL GAP:
 *   From the industry-funded psychiatrist and pharmaceutical manufacturer seats, DSM revision is a scientific consensus-building process that happens to produce clinically actionable, reimbursable categories — coordination with commercial benefit as a byproduct. From the overprescribed patient and adverse-effect patient seats, the same process is experienced as a diagnostic apparatus that routes them toward medication as a structural default regardless of their actual presentation's fit to any stable biological entity. The engine computes these divergently from the same structural facts; this story does not average or hedge between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical manufacturers and industry-funded psychiatrists are declared beneficiaries with arbitrage-grade exit (they profit from and can navigate around any single revision's specifics) — this drives d toward the beneficiary end. Patient groups are declared victims with constrained-to-trapped exit (insurance structures, withdrawal effects, and label-based social consequences limit their mobility) — this drives d toward the target end. The American Psychiatric Association is an agenda-setter with institutional arbitrage exit: it administers the taxonomy and derives licensing revenue but is not solely a pharmaceutical beneficiary, so it is kept structurally distinct from the manufacturer/funded-psychiatrist beneficiary set.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diagnostic reliability, scientific legitimacy for psychiatry) was genuinely live in the 1970s and drove the DSM-III reform. This reading holds that problem has been substantially addressed by the reliability gains DSM-III achieved, while the apparatus built to solve it has since been colonized by a downstream function (market construction) that was never its founding purpose. Declaring this tangled_rope rather than snare preserves the real coordination residue (shared vocabulary still has genuine clinical and research value) against a pure-extraction reading that would erase the taxonomy's non-commercial utility entirely — the mandatrophy is that the coordination function persists as legitimating cover for the extraction function that has partially supplanted it, not that coordination never existed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reverse_engineering_vs_genuine_discovery,
    'Were specific DSM category boundaries (e.g. major depressive disorder threshold, bipolar II, ADHD in adults) drawn to match the pharmacological profile of available drugs, or did drug development follow independently validated diagnostic boundaries?',
    'Historical analysis of task force meeting minutes, correspondence, and funding disclosures cross-referenced against drug approval and patent timelines for each contested category, as has been done retrospectively for DSM-III and DSM-IV revisions.',
    'If reverse-engineering is documented as the dominant pattern across most contested categories, this reading''s tangled_rope classification is well-supported; if boundary-setting is shown to precede and be independent of drug availability in most cases, the extraction component is overstated and the constraint moves closer to a rope with isolated capture incidents rather than systemic capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reverse_engineering_vs_genuine_discovery, empirical, 'Whether category construction causally follows drug availability or vice versa.').

omega_variable(
    committer_frame_kernel_disagreement_locus,
    'This constraint is one reading (critical_psychiatry_reading) of the dsm_taxonomy_kernel; the biomedical_reading holds the same categories map to objective disease entities and the neurodiversity_reading holds they pathologize natural variation. Where exactly does the disagreement between these readings live — in the underlying facts about category construction, or in the normative framework used to evaluate any given construction process?',
    'Trace whether biomedical_reading proponents would dispute the documented industry-funding and task-force-composition facts this reading relies on, or whether they accept those facts but interpret them as normal scientific practice rather than capture; similarly for neurodiversity_reading''s relationship to the same facts.',
    'If the readings disagree primarily on interpretation of shared facts (a framing/normative divide), the three constraints are properly disjoint per DP-001 and this omega documents the disagreement locus without threatening epsilon-invariance. If the readings actually dispute the underlying facts, the empirical omega above becomes decisive across all three sibling constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_kernel_disagreement_locus, conceptual, 'Whether the kernel''s sibling readings diverge on facts or on evaluative framework.').

omega_variable(
    disclosure_policy_efficacy,
    'Do current APA conflict-of-interest disclosure requirements for DSM task force members meaningfully constrain industry influence on category construction, or do they function primarily as procedural legitimation without substantive effect?',
    'Compare category-boundary outcomes and revision patterns before and after disclosure policy strengthening (post-2005 APA policy changes), controlling for the underlying clinical evidence base available at each revision point.',
    'If disclosure has measurably changed outcomes, the theater_ratio trajectory in this story should be revised downward for the post-policy period; if outcomes are unchanged, the rising theater_ratio series is corroborated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disclosure_policy_efficacy, empirical, 'Whether disclosure policy is substantive reform or procedural theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(dsm__tr_t8, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(dsm__tr_t16, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(dsm__tr_t24, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 24, 0.52).
narrative_ontology:measurement(dsm__tr_t32, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 32, 0.58).
narrative_ontology:measurement(dsm__tr_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(dsm__be_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dsm__be_t8, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(dsm__be_t16, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(dsm__be_t24, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(dsm__be_t32, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(dsm__be_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dsm__su_t8, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(dsm__su_t16, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(dsm__su_t24, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(dsm__su_t32, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(dsm__su_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.08).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_patent_extension_practices).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatric_drug_reimbursement_criteria).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the dsm_taxonomy_kernel (biomedical_reading, critical_psychiatry_reading, neurodiversity_reading), each authored as a structurally distinct constraint with its own epsilon, beneficiary/victim structure, and claimed type per the epsilon-invariance principle. The biomedical_reading treats the same taxonomic text as approaching mountain-like natural-kind discovery (low epsilon, no victims if biologically valid); this critical_psychiatry_reading treats it as tangled_rope (moderate-high epsilon, pharmaceutical-industry beneficiaries, patient victims); the neurodiversity_reading treats it as snare-flavored with a distinct victim set (neurodivergent individuals subjected to pathologization independent of pharmaceutical harm). All three link to each other via affects_constraints rather than being merged into a single averaged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
