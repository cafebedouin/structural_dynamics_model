% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__critical_psychiatry_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: dsm_taxonomy_kernel__critical_psychiatry_reading
 *   human_readable: DSM Taxonomy as Pharmaceutical Market Construction (Critical Reading)
 *   domain: medical/psychiatric/economic
 *
 * SUMMARY:
 *   The Diagnostic and Statistical Manual of Mental Disorders (DSM) is
 *   presented as objective nosology — a catalog of naturally occurring
 *   psychiatric conditions. Under the critical psychiatry reading, the DSM is
 *   reverse-engineered: pharmaceutical companies invest in research,
 *   psychiatrists receive industry funding, diagnostic categories are
 *   expanded or created to correspond to available treatments, and the
 *   resulting taxonomy is defended as empirical discovery. The founding
 *   problem (lack of standardized diagnostic language, post-WWII) was real
 *   and the early DSM-III addressed it. But post-1990, DSM revision cycles
 *   show systematic alignment with pharmaceutical product lifecycles: new
 *   categories expand when new drugs enter the market, categories shrink or
 *   disappear when drugs lose patent protection or are superseded. The
 *   constraint exhibits tangled rope structure because genuine coordination
 *   (diagnostic standardization for research and clinical communication)
 *   coexists with asymmetric extraction (pharmaceutical profit, clinician
 *   funding, and overprescription-driven harm to patients). The claim/metric
 *   gap is deliberate: this reading asserts the constraint is tangled rope
 *   (both coordination and extraction), while acknowledging the biomedical
 *   reading claims it is pure rope (coordination with minimal extraction).
 *   The engine measures the structural divergence; this story's metrics are
 *   authored independently of the biomedical reading's metrics.
 *
 * KEY AGENTS:
 *   - Pharmaceutical manufacturers: institutional agenda-setters, arbitrage exit, direct benefit from DSM category expansion
 *   - DSM gatekeepers (psychiatry academic leadership): organized agenda-setters, constrained exit, both profit from industry funding and benefit from professional authority
 *   - Industry-aligned psychiatrists: moderate power beneficiaries, identity-locked exit, fused professional identity to medication-first paradigms
 *   - Over-diagnosed patients: powerless payers, trapped exit, prescribed drugs they may not clinically need
 *   - Polypharmacy-harmed patients: powerless payers, identity-locked exit, suppression is both structural (difficult to exit care) and internalized (identity constituted through psychiatric diagnosis)
 *   - Diagnostic orphans: powerless payers, constrained exit, distress that does not fit pharmaceutical categories is rendered invisible
 *   - Critical psychiatrists: moderate observers, constrained exit, professional isolation for questioning DSM alignment with pharmaceutical incentives
 *   - Regulatory agencies: institutional observers, analytical exit, defer to medical authority and operate within DSM-defined categories
 *   - Insurance systems: institutional beneficiary-payers, constrained exit, benefit from diagnostic clarity but pay for the extraction via drug costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.68).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.71).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.54).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM Taxonomy as Pharmaceutical Market Construction (Critical Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical/psychiatric/economic").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, 'a9fd1079-b0c3-4692-b8c7-6b7a9fdf513f').
narrative_ontology:cs_kernel_codification('a9fd1079-b0c3-4692-b8c7-6b7a9fdf513f', formalized).
narrative_ontology:cs_authority_grounding('a9fd1079-b0c3-4692-b8c7-6b7a9fdf513f', extraction).
narrative_ontology:cs_interpretation_layer_present('a9fd1079-b0c3-4692-b8c7-6b7a9fdf513f').
narrative_ontology:cs_reading_relation('a9fd1079-b0c3-4692-b8c7-6b7a9fdf513f', dsm_taxonomy_kernel__biomedical_reading, forecloses).
narrative_ontology:cs_reading_relation('a9fd1079-b0c3-4692-b8c7-6b7a9fdf513f', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('a9fd1079-b0c3-4692-b8c7-6b7a9fdf513f', foundational, dsm_categories_reverse_engineered_from_pharmaceuticals).
narrative_ontology:cs_axiom_status(dsm_categories_reverse_engineered_from_pharmaceuticals, holdable).
narrative_ontology:cs_axiom_grounding('a9fd1079-b0c3-4692-b8c7-6b7a9fdf513f', dsm_categories_reverse_engineered_from_pharmaceuticals, empirically_contingent).
narrative_ontology:cs_axiom('a9fd1079-b0c3-4692-b8c7-6b7a9fdf513f', secondary, psychiatric_diagnosis_reflects_pharmaceutical_market_alignment_not_discovery).
narrative_ontology:cs_axiom_status(psychiatric_diagnosis_reflects_pharmaceutical_market_alignment_not_discovery, holdable).
narrative_ontology:cs_axiom_grounding('a9fd1079-b0c3-4692-b8c7-6b7a9fdf513f', psychiatric_diagnosis_reflects_pharmaceutical_market_alignment_not_discovery, empirically_contingent).
narrative_ontology:cs_reference_frame('a9fd1079-b0c3-4692-b8c7-6b7a9fdf513f', psychiatric_nosology_as_disease_mapping).
narrative_ontology:cs_drift_state('a9fd1079-b0c3-4692-b8c7-6b7a9fdf513f', contemporary_pharmaceutical_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a9fd1079-b0c3-4692-b8c7-6b7a9fdf513f', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrists_industry_aligned).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_manual_gatekeepers).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, over_diagnosed_patients).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, polypharmacy_harmed_populations).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, diagnostic_orphans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_systems).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fund psychiatric research, support DSM revision processes through professional societies, finance continuing medical education for prescribers, and conduct clinical trials designed to demonstrate efficacy within DSM categories. Direct benefit: market expansion for existing drugs and creation of new diagnostic categories that justify new pharmaceutical products. Exit from DSM alignment would abandon a multi-billion-dollar revenue stream.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers, agenda_setter,
    institutional, generational, arbitrage, global).

% Psychiatrists and researchers who author and revise DSM categories. Incentive structure: professional authority, funding for research, consulting relationships with pharmaceutical firms, and institutional prestige. They defend the DSM as objective nosology while receiving material support from the industry whose products the revised categories justify. Their revision choices are constrained by need for consensus among stakeholders (including industry-funded researchers) and by the requirement that categories map onto existing or planned pharmaceutical interventions.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_manual_gatekeepers, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_manual_gatekeepers, beneficiary).

% Clinicians who receive pharmaceutical industry funding (speaker fees, consulting, research support). They benefit from expanded diagnostic categories that increase patient volume and justification for prescription. Their professional identity is fused with medication-first treatment paradigms; exit would mean reframing their entire practice model and research portfolio. They operate largely as unknowing or semi-aware intermediaries: the funding and diagnostic expansion create aligned incentives without requiring explicit coordination.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrists_industry_aligned, beneficiary,
    moderate, biographical, identity_locked, national).

% People diagnosed with DSM categories driven by pharmaceutical market expansion rather than clinical need. They receive prescriptions for drugs that may not address their condition, experience adverse effects, and face stigma from a diagnosis that may be category-creep driven. Their exit options are severely constrained: questioning a psychiatric diagnosis requires challenging the authority of the DSM and the clinician; stopping medication without professional support risks withdrawal effects and loss of clinical care; seeking alternative explanations for distress is blocked by the medical framing itself.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, over_diagnosed_patients, payer,
    powerless, biographical, trapped, national).

% Patients who receive multiple psychotropic drugs in combination (polypharmacy), often with inadequate evidence for efficacy or safety in the specific combination. Drug interactions, metabolic effects, cognitive impairment, and physical health decline accumulate. Exit is identity-locked: they have internalized the psychiatric diagnosis as core identity; psychiatric care is presented as essential to survival; families and clinicians reinforce dependency. Many experience suppression as internalized belief that medication is non-negotiable, not as external coercion alone.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, polypharmacy_harmed_populations, payer,
    powerless, biographical, identity_locked, national).

% People whose distress does not fit DSM categories because those categories are constructed around existing pharmaceutical treatments. They are denied diagnosis and treatment within the medical system, yet simultaneously framed as untreatable or malingering. They bear the cost of a taxonomy built around pharmacological fitness rather than clinical comprehensiveness. Their exit from the psychiatric system often means losing access to any institutionalized support.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, diagnostic_orphans, payer,
    powerless, biographical, constrained, national).

% Psychiatrists and mental health researchers who question whether DSM categories are reverse-engineered from pharmaceutical treatments rather than discovered through epidemiology. They publish critiques, cite evidence of diagnostic expansion, and advocate for non-pharmacological models. They operate at institutional disadvantage: funding flows to industry-aligned researchers, challenging the DSM risks professional isolation, and their critiques are reframed as 'anti-psychiatry' rather than empirical disagreement.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, critical_psychiatrists, observer,
    moderate, biographical, constrained, national).

% FDA and equivalent bodies that approve psychiatric medications. They evaluate drugs within DSM diagnostic categories and may inadvertently entrench those categories by approving drugs for DSM-defined indications. Their role is constrained by the presumption that DSM categories are objective; questioning the taxonomy is outside their explicit mandate.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, regulatory_agencies, observer,
    institutional, generational, analytical, national).

% Health insurers use DSM categories to adjudicate coverage; they benefit from diagnostic clarity (reduces claim disputes) and pay for the medications prescribed under those categories. They have dual incentive: cost containment pulls toward diagnostic restrictiveness; industry pressure and clinician norms pull toward diagnostic expansion. They lack the expertise to independently verify DSM validity and defer to medical authority.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_systems, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_systems, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__critical_psychiatry_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized diagnostic nomenclature that enables psychiatric researchers to conduct comparable studies across time and jurisdictions, clinicians to communicate about symptom clusters using consistent terminology, insurance systems to adjudicate coverage claims, and reduces idiosyncratic variation in diagnosis that would fragment the field into incompatible frameworks.
% TRANSFER_FUNCTION: Moves pharmaceutical revenue from patients and insurers to manufacturers and their distribution networks (psychiatrists); transfers research funding from pharmaceutical companies to psychiatry academics and clinicians; transfers diagnoses (disease framings) to individuals experiencing distress; transfers authority (decision-making power over treatment choices) from patients to clinicians authorized by DSM categories.
% ABSENT_VOICES: Patients harmed by overprescription and polypharmacy are excluded from DSM revision processes; critical psychiatrists are marginalized despite published epidemiological evidence; non-Western healing frameworks and alternative models of distress (trauma-informed, social-relational, neurodiversity) are not represented in DSM decision-making; survivor-led critiques of psychiatric authority are dismissed as 'lacking insight'; researchers funded by non-pharmaceutical sources have lower influence on revision outcomes.
% DISAPPEARANCE_RATIONALE: If the DSM were suddenly unavailable, psychiatric practice would radically reorganize: diagnostic rates would likely fall sharply (no standardized categories to apply), pharmaceutical sales would collapse (no DSM-category-based prescribing), insurance reimbursement would shift to outcome-based or alternative models, and patient populations would be re-evaluated under different frameworks (some would retain diagnosis, many would be reclassified or undiagnosed). The immediate disruption would be severe; the secondary effect would be major restructuring of psychiatry away from pharmaceutical alignment.
% FOUNDING_PROBLEM: From the 1950s–1970s, psychiatry lacked a shared diagnostic nomenclature; clinicians used ad-hoc criteria, research findings could not be reliably compared across studies, and mental illness was poorly understood and heavily stigmatized. A standardized taxonomy was needed to professionalize psychiatry and enable reproducible research.
% FOUNDING_PROBLEM_CORROBORATION: The pharmaceutical industry and DSM gatekeepers claim the founding problem remains live (diagnostic standardization is ongoing need). Critical psychiatrists, epidemiologists (not funded by pharmaceutical companies), and patient advocacy groups attest the founding problem was substantially solved by DSM-III (1980) and early DSM-IV (1994), but post-2000 DSM revisions track pharmaceutical market cycles rather than epidemiological evidence or therapeutic outcomes. Academic meta-analyses comparing DSM revision outcomes to pharmaceutical approval timelines (audited in legal discovery and published by critical researchers) support the claim that the founding problem is functionally extinct and post-1990 constraint operation is driven by pharmaceutical interest, not residual coordination need.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__critical_psychiatry_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__critical_psychiatry_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__critical_psychiatry_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.68 at 2024) because the DSM categories drive pharmaceutical sales revenue, clinician funding, and patient volume that would not exist under a neutral taxonomy. The extraction is not maximum because diagnostic standardization does produce genuine coordination benefits — the beneficiary categories cannot be characterized as pure extraction. Suppression is slightly higher (0.71) than extractiveness because maintaining the framework requires active enforcement: critical voices are delegitimized, alternative diagnostic models are excluded from medical authority, patients are isolated from counter-narratives, and industry funding concentrates around DSM-aligned researchers. Theater ratio (0.54) reflects a constraint where scientific rhetoric (the DSM is empirically discovered) has become partially decoupled from function (coordination is real, but pharmaceutical alignment is the driver of post-1990 revision). The measurement trajectory shows steady increase from 1980–2020, plateauing thereafter: DSM-III addressed the original coordination problem, but DSM-IV and DSM-5 tracked pharmaceutical market expansion, and by 2020 the constraint had stabilized at high extractiveness with persistent therapeutic theater. Accessibility collapse (0.48) is moderate-low because alternatives to psychiatric diagnosis remain conceptually available (biomedical reading, neurodiversity reading, social model) even though institutional barriers are high. Resistance (0.62) is substantial because critical psychiatry movements, patient advocacy groups, and epidemiological evidence questioning DSM categories have grown, even though they remain institutionally marginal.
 *
 * PERSPECTIVAL GAP:
 *   The biomedical reading (sibling constraint, authored separately) claims the DSM categories map to objective neurobiological disease entities. From the biomedical seat, extractiveness should be near 0.0 (no extractive component, purely coordination), suppression should be near 0.0 (alternatives are not suppressed, they are simply wrong or less accurate), theater_ratio should be near 0.0 (the DSM IS the functional thing, not a mask). The critical reading claims higher extractiveness, higher suppression (alternatives are actively delegitimized), and substantial theater (scientific rhetoric masks pharmaceutical alignment). These are not measurements of the same constraint viewed from different angles — they are different constraints (different ε, different stakeholder sets, different structural claims) that share a common kernel (the DSM text). The kernel is fixed; the readings' ε values diverge because they make incompatible claims about what the DSM's operation actually is. The sibling neurodiversity reading would occupy a third structural position: DSM categories are not diseases but normal variation; extractiveness would be high (they extract by pathologizing variation), suppression would be highest (alternatives — neurodiversity self-identification — are actively suppressed), theater would be highest (biological-disease rhetoric is cover for enforcing behavioral conformity). The three readings, siblings under one kernel, instantiate three structurally distinct constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical manufacturers: institutional power, arbitrage exit (can shift business models), explicit beneficiary (direct revenue) → d near 0.0 (full beneficiary). DSM gatekeepers: organized power, constrained exit (careers built on psychiatry and DSM revision), beneficiary (research funding, professional authority) → d around 0.1–0.2 (beneficiary-tilted but not pure beneficiary because they also absorb some reputational risk). Industry-aligned psychiatrists: moderate power, identity-locked exit (professional identity fused to medication-first psychiatry), beneficiary role → d around 0.15–0.25 (beneficiary but constrained by identity lock). Over-diagnosed patients: powerless, trapped exit (cannot exit psychiatric system without losing access to any mental health support), victim role (harmed by unnecessary diagnosis and medication) → d near 1.0 (full target). Polypharmacy-harmed patients: powerless, identity-locked exit (diagnosis internalized as identity, family reinforces dependency), victim role (active harm), suppression is internalized → d near 0.9–1.0 (target, with higher suppression due to internalization). Diagnostic orphans: powerless, constrained exit (distress is not validated, alternative frameworks are unavailable), victim role (denied diagnosis and treatment) → d around 0.85–0.95 (target). The gap between agenda-setter d-values (near 0.0) and victim d-values (near 0.9–1.0) is the structural asymmetry that defines tangled rope from the critical reading's perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The critical psychiatry reading asserts that the DSM's founding problem (lack of diagnostic standardization, 1950s–1970s) has been solved since DSM-III (1980). The DSM-IV (1994) and DSM-5 (2013) address different problems: pharmaceutical market expansion and expansion of diagnostic scope into normal variation. The contemporary constraint is sustained not by residual coordination need but by pharmaceutical revenue, clinician funding dependencies, and internalized patient belief in psychiatric diagnosis. Mandatrophy signals when a constraint's original function is extinct and only extraction remains; this reading asserts diagnostic standardization (the original function) is functionally complete, and post-1990 expansion is mandatrophic — the therapeutic rhetoric persists, but the structural function has shifted to pharmaceutical market capture. The founding_problem_status of 'contested' captures this: the pharmaceutical and psychiatry establishment attest ongoing coordination need (taxonomy must evolve with science); critical parties attest the founding problem was solved forty years ago and contemporary DSM changes serve pharmaceutical interests, not coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pharmaceutical_alignment_causality,
    'Does pharmaceutical availability drive DSM category creation, or does DSM category prevalence drive pharmaceutical development? Is the alignment causal or correlational?',
    'Detailed timeline analysis of DSM revision history vs. pharmaceutical patent issuance, product approval dates, and marketing expenditure. Regulatory discovery in litigation over pharmaceutical marketing practices. Comparison with non-DSM psychiatric systems (ICD-10, international variation) to test whether similar diagnoses emerge independent of pharmaceutical pressure.',
    'If pharmaceutical availability drives category creation (causal arrow: drug → diagnosis), the constraint is clearly extractive and the critical reading is vindicated. If DSM categories drive pharmaceutical development (reverse causality), the pharmaceutical alignment is incidental to epidemiologically valid taxonomy. If the alignment is correlational without clear causal direction, the extractiveness claim weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmaceutical_alignment_causality, empirical, 'The causal direction of pharmaceutical availability and DSM category alignment').

omega_variable(
    clinician_awareness_of_industry_capture,
    'To what degree are psychiatrists who prescribe and revise DSM categories aware that their decision-making is shaped by pharmaceutical industry funding and incentives?',
    'Survey data from psychiatrists about perceived conflicts of interest; qualitative interviews about funding sources and impact on practice; comparison of DSM revision outcomes by industry-funding status of revision committee members.',
    'If clinicians are largely unaware of the structural incentive alignment, suppression is higher (captured actors cannot mount conscious resistance) and the constraint approaches snare structure. If clinicians are aware and choose the alignment anyway (perverse incentives accepted), the constraint remains tangled rope but with higher agency. If clinicians are actively aware and resisting, the resistance metric would be higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(clinician_awareness_of_industry_capture, empirical, 'Psychiatric clinicians'' awareness of pharmaceutical industry capture in DSM revision').

omega_variable(
    internalization_of_psychiatric_diagnosis,
    'To what degree is the suppression experienced by over-diagnosed and polypharmacy-harmed patients structural (external barriers to exit) versus internalized (the patient''s identity is constituted through psychiatric diagnosis)?',
    'Longitudinal ethnography of patients post-psychiatric-exit; measurement of suppression persistence after formal psychiatric contact is ended; comparison of suppression levels between patients with early-onset identity-fused diagnosis (high internalization expected) and late-onset diagnosis (lower internalization). Post-exit trajectory analysis: if suppression persists after exit, internalization was present; if suppression resolves, it was structural.',
    'If suppression is largely structural, removing barriers (insurance access, clinician training in alternatives, normalization of non-pharmacological models) could reduce the constraint''s force. If suppression is largely internalized, the constraint is more resilient because the target carries the suppression with them after physical exit; reframing at the identity level would be necessary. This affects the fixing_cost assessment: structural suppression is cheaper to fix; internalized suppression is prohibitive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_of_psychiatric_diagnosis, empirical, 'Structural versus internalized suppression in over-diagnosed patients').

omega_variable(
    framing_alternative_epistemologies,
    'Would a social-model account of psychiatric distress (framing symptoms as responses to social conditions rather than internal disease) substantially alter the classification of this constraint, or is the ε-value stable across different epistemological frames?',
    'Author a parallel constraint story from the social-medicine reading; compare ε values, victim/beneficiary sets, and enforcement mechanisms. Test whether the structural extraction claim holds under different framing, or whether pharmaceutical alignment only appears extractive under the disease model.',
    'If the social-model reading yields substantially different ε and victim/beneficiary sets (because the constraint is framed as enforcement of institutional norms rather than medical extraction), then this story and the social-model story are separate constraints (per ε-invariance principle) and should be authored separately and linked in network. If ε is stable and extraction claim holds under both frames, the structural analysis is robust to epistemological variation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_alternative_epistemologies, conceptual, 'Whether ε-invariance holds across social-model versus disease-model framing of DSM categories').

omega_variable(
    contested_readings_kernel_identity,
    'Do the biomedical, critical, and neurodiversity readings of the DSM constitute genuinely different kernels (fundamentally incommensurable claims about what the DSM is), or do they represent different empirical interpretations of a single stable kernel?',
    'Examine whether the three readings can be held simultaneously within a single coherent framework without contradiction. Test whether accepting the truth of one reading logically forecloses the others, or whether they can coexist as different empirical claims about the same text. Analyze the genealogy of each reading: do they cite and dispute each other (same kernel, different readings), or do they operate in separate disciplinary spaces (different kernels)?',
    'If the readings genuinely foreclose each other (holding one true makes the others impossible), they are separate kernels and each should be authored as its own constraint with its own cs_structure block (no reading_relations needed, only network.affects_constraints). If they coexist as interpretations of the same DSM text, they are readings of one kernel and the cs_structure.reading_relations should capture the coexistence. This affects the taxonomic structure of the constraint corpus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contested_readings_kernel_identity, conceptual, 'Whether biomedical, critical, and neurodiversity readings are siblings of one kernel or separate kernels').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement_basis(dsm__tr_t1980, observed).
narrative_ontology:measurement(dsm__tr_t1990, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1990, 0.32).
narrative_ontology:measurement_basis(dsm__tr_t1990, observed).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2000, 0.41).
narrative_ontology:measurement_basis(dsm__tr_t2000, observed).
narrative_ontology:measurement(dsm__tr_t2010, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement_basis(dsm__tr_t2010, observed).
narrative_ontology:measurement(dsm__tr_t2020, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2020, 0.54).
narrative_ontology:measurement_basis(dsm__tr_t2020, observed).
narrative_ontology:measurement(dsm__tr_t2024, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2024, 0.54).
narrative_ontology:measurement_basis(dsm__tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement_basis(dsm__be_t1980, observed).
narrative_ontology:measurement(dsm__be_t1990, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement_basis(dsm__be_t1990, observed).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement_basis(dsm__be_t2000, observed).
narrative_ontology:measurement(dsm__be_t2010, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement_basis(dsm__be_t2010, observed).
narrative_ontology:measurement(dsm__be_t2020, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement_basis(dsm__be_t2020, observed).
narrative_ontology:measurement(dsm__be_t2024, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(dsm__be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1980, 0.38).
narrative_ontology:measurement_basis(dsm__su_t1980, observed).
narrative_ontology:measurement(dsm__su_t1990, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1990, 0.48).
narrative_ontology:measurement_basis(dsm__su_t1990, observed).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2000, 0.56).
narrative_ontology:measurement_basis(dsm__su_t2000, observed).
narrative_ontology:measurement(dsm__su_t2010, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2010, 0.64).
narrative_ontology:measurement_basis(dsm__su_t2010, observed).
narrative_ontology:measurement(dsm__su_t2020, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement_basis(dsm__su_t2020, observed).
narrative_ontology:measurement(dsm__su_t2024, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(dsm__su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.12).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_marketing_to_psychiatrists).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatric_adverse_event_reporting).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the DSM-taxonomy kernel. The biomedical reading claims the DSM maps to objective disease entities (low extraction, pure coordination). The critical reading (this story) claims the DSM is reverse-engineered from pharmaceutical treatments (moderate-high extraction, tangled rope with asymmetry). The neurodiversity reading claims the DSM pathologizes natural variation (high extraction, behavioral conformity enforcement). Each reading has different ε, different beneficiary/victim sets, and different structural claims. They are not the same constraint viewed from different perspectives — they are different constraints that share a common kernel text. The three should be authored as separate stories and linked via network.affects_constraints to reflect their sibling relationship under the common kernel. The foundational disagreement is about the DSM's referent: objective neurobiological disease (biomedical), pharmaceutical market construction (critical), or enforced behavioral normality (neurodiversity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__critical_psychiatry_reading, organized, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
