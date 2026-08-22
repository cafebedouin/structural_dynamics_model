% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__neurodiversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: dsm_taxonomy_kernel__neurodiversity_reading
 *   human_readable: DSM Pathologization of Neurodiversity (Neurodiversity Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction
 *
 * SUMMARY:
 *   The Diagnostic and Statistical Manual of Mental Disorders (DSM)
 *   categorizes neurodevelopmental and neuropsychiatric variation as
 *   disorders requiring clinical intervention. From the neurodiversity
 *   reading perspective, the DSM constitutes a kernel — a contested
 *   commitment to categorical disease ontology — whose operation extracts
 *   from neurodivergent individuals by pathologizing natural neurological
 *   variation, suppressing neurodiversity-affirming alternatives, and
 *   justifying coercive institutional conformity. Beneficiaries are
 *   institutional systems (schools, workplaces) that escape pressure to
 *   accommodate diversity and pharmaceutical markets that expand their
 *   customer base with each DSM category expansion. The constraint persists
 *   through diagnostic authority (gatekeeping by psychiatrists),
 *   institutional enforcement (mandatory evaluation and treatment in
 *   educational and workplace contexts), and internalized shame that embeds
 *   suppression in the victims' self-concept. This story instantiates ONE
 *   reading of a three-way kernel contest: the biomedical reading claims DSM
 *   categories are objective disease discoveries; the critical psychiatry
 *   reading claims they are pharmaceutical-driven constructions; the
 *   neurodiversity reading claims they pathologize natural variation to
 *   legitimize institutional conformity. The three readings coexist across
 *   different institutional and professional coalitions and are not logically
 *   foreclosed by any single framework — they represent genuinely contested
 *   epistemic and normative positions. This file generates only the
 *   neurodiversity reading, not the kernel contest as a whole.
 *
 * KEY AGENTS:
 *   - neurodivergent_individuals: victims subjected to pathologization, coercive treatment, and identity-locked suppression
 *   - marginalized_neurotypes: victims with high support needs, subject to most severe coercion and institutional confinement
 *   - institutional_conformity_systems: agenda-setters and beneficiaries (schools, workplaces, carceral systems) that justify exclusion via DSM pathology
 *   - psychiatric_pharmaceutical_markets: beneficiaries that expand revenue with DSM category expansion
 *   - diagnostic_gatekeepers: psychiatrists and psychologists who author DSM criteria and enforce diagnosis
 *   - neurodiversity_advocates: excluded stakeholders who would reframe neurotype difference as adaptation rather than pathology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.82).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.78).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Pathologization of Neurodiversity (Neurodiversity Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical_epistemology/psychiatric_taxonomy/social_construction").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, 'e96f85d9-25f9-49dc-b398-90536e5bf326').
narrative_ontology:cs_kernel_codification('e96f85d9-25f9-49dc-b398-90536e5bf326', fixed_text).
narrative_ontology:cs_authority_grounding('e96f85d9-25f9-49dc-b398-90536e5bf326', extraction).
narrative_ontology:cs_interpretation_layer_present('e96f85d9-25f9-49dc-b398-90536e5bf326').
narrative_ontology:cs_reading_relation('e96f85d9-25f9-49dc-b398-90536e5bf326', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('e96f85d9-25f9-49dc-b398-90536e5bf326', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('e96f85d9-25f9-49dc-b398-90536e5bf326', foundational, neurotype_diversity_is_natural_variation).
narrative_ontology:cs_axiom_status(neurotype_diversity_is_natural_variation, holdable).
narrative_ontology:cs_axiom_grounding('e96f85d9-25f9-49dc-b398-90536e5bf326', neurotype_diversity_is_natural_variation, empirically_contingent).
narrative_ontology:cs_axiom('e96f85d9-25f9-49dc-b398-90536e5bf326', foundational, pathologization_itself_is_harm).
narrative_ontology:cs_axiom_status(pathologization_itself_is_harm, holdable).
narrative_ontology:cs_axiom_grounding('e96f85d9-25f9-49dc-b398-90536e5bf326', pathologization_itself_is_harm, deontological).
narrative_ontology:cs_axiom('e96f85d9-25f9-49dc-b398-90536e5bf326', secondary, neurodivergent_self_determination_is_right).
narrative_ontology:cs_axiom_status(neurodivergent_self_determination_is_right, holdable).
narrative_ontology:cs_axiom_grounding('e96f85d9-25f9-49dc-b398-90536e5bf326', neurodivergent_self_determination_is_right, deontological).
narrative_ontology:cs_reference_frame('e96f85d9-25f9-49dc-b398-90536e5bf326', neurodiversity_affirming_epistemic_framework).
narrative_ontology:cs_drift_state('e96f85d9-25f9-49dc-b398-90536e5bf326', contemporary_dsmv_expansion_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('e96f85d9-25f9-49dc-b398-90536e5bf326', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, institutional_conformity_systems).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_pharmaceutical_markets).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, marginalized_neurotypes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, educational_accommodations_administrators).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__neurodiversity_reading, categorical_disease_ontology).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__neurodiversity_reading, neurotypical_behavioral_norm_universality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Diagnosed with DSM disorders for neurological traits (attention patterns, sensory processing, social communication styles, stimming, executive function organization) that are functionally adaptive in some contexts but conflict with institutional schedules and conformity demands. Face coercive treatment (forced medication, behavioral suppression, isolation from peer groups with similar neurotypes), denial of accommodations that would enable their participation without neurotoxic medication, and internalized shame that their neurology is defective rather than different. Exit means rejecting the diagnosis, but institutional power (schools, employers, medical systems, family) treats rejection as denial of illness. Identity is fused with the neurotype — they cannot exit without rewriting their self-concept.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals, payer,
    powerless, biographical, identity_locked, global).

% Children and adults whose neurological presentations (high support needs, communication disabilities, emotional dysregulation, trauma responses, culturally non-normative behavior) are most harshly pathologized. Confined to segregated educational and residential settings, subject to restraint and seclusion, chemically restrained, excluded from peer interaction. No meaningful exit: their institutional dependence is near-total.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, marginalized_neurotypes, payer,
    powerless, biographical, trapped, global).

% Schools, workplaces, military, carceral systems, and family structures that operate under assumptions of behavioral uniformity and require populations to conform to narrow neurotypical schedules, communication protocols, attention demands, and emotional expression norms. DSM categories legitimize the exclusion or forced compliance of those who deviate. These systems benefit from not having to accommodate neurological diversity: diagnosing neurodivergence as disease justifies exclusion or coercive normalization as treatment rather than as institutional inflexibility. They set and enforce the DSM diagnostic criteria through institutional practice and policy.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, institutional_conformity_systems, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, institutional_conformity_systems, beneficiary).

% Pharmaceutical manufacturers, prescribing physicians, and psychiatric treatment industries that profit from the medicalization of neurodiversity. Each DSM category expansion creates new market segments for psychotropic drugs. The constraint's persistence creates a reliable revenue stream from long-term medication use by neurodivergent populations, many of whom begin treatment in childhood and continue into adulthood.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_pharmaceutical_markets, beneficiary,
    institutional, generational, arbitrage, global).

% Psychiatrists, developmental psychologists, clinical social workers, and academic medicine who author, revise, and operationalize DSM categories. They claim scientific authority grounded in empirical research but operate within institutional incentive structures (funding for pathology research, careers built on disease discovery, pharmaceutical marketing influence). Their power to diagnose is the enforcement mechanism: a diagnosis triggers institutional exclusion and coercive treatment.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, diagnostic_gatekeepers, agenda_setter,
    powerful, generational, constrained, global).

% Autistic self-advocates, ADHD activists, disability rights organizations that argue DSM pathologization is social construction, not medical discovery. They would reframe the constraint: institutional design is the problem, not neurodivergent neurology. They are systematically excluded from DSM revision committees, research funding, and medical authority; their testimony is treated as bias or denial.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_advocates, excluded,
    moderate, biographical, constrained, global).

% Researchers investigating neurobiological variation publish findings showing continuity rather than categorical disease boundaries (spectrum models, neurotype gradients, context-dependent adaptation). Their work documents the constraint but faces publication bias (negative findings on disease categorization are less fundable) and institutional pressure from medical and pharmaceutical constituencies.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neuroscience_researchers, observer,
    institutional, generational, constrained, global).

% School districts and workplace HR departments that must provide accommodations to diagnosed neurodivergent individuals under disability law. The DSM diagnosis is the gate: without it, accommodation is discretionary; with it, accommodation is required but stigmatized. They bear the cost of accommodations while institutional conformity systems resist design changes that would benefit all students/employees.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, educational_accommodations_administrators, payer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_pharmaceutical_markets).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__neurodiversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared diagnostic taxonomy enabling communication between disparate medical and institutional systems (schools, workplaces, clinical care, insurance): professionals use DSM categories to coordinate on what constitutes illness, who requires treatment, and which institutional accommodations or exclusions are medically justified.
% TRANSFER_FUNCTION: Transfers decision-making authority and bodily autonomy from neurodivergent individuals to medical and institutional gatekeepers (psychiatrists, school psychologists, HR departments). Transfers the cost of institutional inflexibility (schools and workplaces designed for neurotypical conformity) onto neurodivergent individuals as medication burdens, behavioral suppression, cognitive side effects, shame, and exclusion from peer groups. Transfers economic value (pharmaceutical sales, insurance coding and billing, clinical labor from diagnosis and treatment) to pharmaceutical manufacturers, prescribing clinicians, and diagnostic industries. Transfers institutional legitimacy (DSM categories justify exclusionary policies as medical necessity rather than institutional design choices) to conformity-demanding systems.
% ABSENT_VOICES: Neurodiversity advocates are structurally excluded from DSM revision committees (closed to non-clinicians) and clinical authority. Neurodivergent individuals, especially those with high support needs or communication differences, have minimal input into criteria that define their neurology as disordered. Researchers documenting neurobiological continuity rather than disease categories face publication bias (journals preferring disease-model findings) and funding barriers (NIH preferring research that reifies disorders). School clinicians and special educators who would argue for institutional accommodation design change rather than individual medicalization are professionally marginalized in favor of diagnostic and pharmaceutical frameworks. Parents of neurodivergent children who reject the medicalization framing are positioned as in denial.
% DISAPPEARANCE_RATIONALE: If DSM pathologization of neurodiversity disappeared overnight, institutional conformity systems would face pressure to accommodate neurological diversity through design change rather than exclusion or coercive normalization. Pharmaceutical markets would lose the DSM-enabled diagnosis-to-treatment pipeline and face reduced revenue. Schools would redesign classroom environments, schedules, and assessment methods to accommodate neurodiversity. Workplaces would implement sensory-friendly and neurodiversity-affirming policies rather than requiring medication compliance for employment. Neurodivergent individuals would not face systematic coercion into medication or behavioral suppression but would retain access to chosen supports and accommodations. The entire institutional and pharmaceutical apparatus built on DSM pathologization would reorganize around neurodiversity affirmation frameworks.
% FOUNDING_PROBLEM: Mid-20th century: American psychiatry sought to establish diagnostic categories enabling systematic study of mental illness and development of pharmaceutical treatments. The DSM (first published 1952) aimed to provide reliable, replicable diagnostic criteria divorced from theoretical school preferences, facilitating research and clinical communication. Early DSM categories (Major Depression, Schizophrenia, Bipolar Disorder) targeted severe conditions with significant impairment. Later revisions expanded categories to include developmental and personality patterns previously considered normal variation (ADHD, Autism Spectrum Disorder).
% FOUNDING_PROBLEM_CORROBORATION: Biomedical psychiatry attests the founding problem remains live and DSM expansion is scientific progress: new categories better capture actual disease entities and enable earlier intervention. Neuroscience researchers independently document that conditions DSM defines as diseases (autism, ADHD) show continuous neurobiological variation rather than categorical boundaries, suggesting the founding problem of reliably identifying disease categories is only partially solved and partially overclaimed. Neurodiversity advocates and disability scholars (outside the benefiting parties) attest the founding problem has been partially solved (we have diagnostic categories) but corrupted: the apparatus designed to identify disease has been expanded to pathologize normal neurological variation, and the expansion is driven by pharmaceutical marketing and institutional pressure rather than scientific discovery. Published epidemiological data showing autism prevalence increasing 10-fold in decades (not reflecting actual neurotype frequency change but diagnostic scope expansion) provides external corroboration of category inflation rather than disease discovery.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__neurodiversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__neurodiversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__neurodiversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.82) because the constraint transfers decision-making autonomy, bodily integrity, and self-concept from neurodivergent individuals to medical and institutional authorities. The constraint operates on living, conscious beings whose sense of self is targeted for medicalization. Suppression is also high (0.78) because institutional power enforces diagnosis and treatment through multiple mechanisms: educational mandates, workplace accommodations conditioned on diagnosis, family pressure, medication compliance, and internalized shame that makes alternative identity frameworks feel impossible. Theater is moderate (0.41) because the DSM does perform genuine coordinating functions (enabling clinician communication, facilitating research, creating accommodation pathways through legal recognition of disability) while simultaneously serving the extractive function. The rising trajectory of extractiveness from t0 to t25 reflects both the DSM's expansion (more conditions medicalized, wider institutional use) and the intensification of pharmaceutical marketing to neurodivergent populations. By t30-t40, extractiveness plateaus as the DSM reaches near-saturation in institutional adoption and the primary dynamic shifts to deepening treatment intensity rather than expanding categories.
 *
 * PERSPECTIVAL GAP:
 *   The diagnostic gatekeepers and pharmaceutical beneficiaries perceive the constraint as beneficial coordination — a shared taxonomy enabling research and treatment. Institutional conformity systems perceive it as legitimating their exclusionary policies as medical necessity rather than institutional inflexibility. Neurodivergent individuals perceive it as pathologization that denies their neurology validity and imposes coercive treatment. Neurodiversity advocates perceive it as a mechanism of social control that constructs impairment through institutional barriers rather than neurology. The engine computes these seat divergences from the structural data: the agenda-setter's power and arbitrage-grade exit options yield low effective extraction on their seat; the victims' powerlessness and identity-locked exit yield high effective extraction. The claimed type (tangled_rope) reflects the neurodiversity reading's framing: the constraint does coordinate (shared diagnostic language) while extracting (pathologization and coercive normalization), with active enforcement required to suppress neurodiversity-affirming alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Neurodivergent individuals and marginalized neurotypes are the structural targets (d near 1.0): they bear the costs of forced diagnosis, medication, behavioral suppression, institutional exclusion, and shame; their exit options are severely constrained (identity_locked and trapped, respectively). Institutional conformity systems and pharmaceutical markets are the structural beneficiaries (d near 0.0): they collect the benefits of standardized diagnostic categories, mandatory treatment pathways, and revenue streams without bearing significant costs; their exit options are high (arbitrage — they can adopt or discard the DSM framework based on institutional advantage). Diagnostic gatekeepers sit between: they have power and relative autonomy (constrained rather than trapped exit) but are themselves embedded in institutional and pharmaceutical incentive structures that reward disease expansion. The identity-lock mechanism is critical: neurodivergent individuals cannot exit the constraint by simply choosing a different diagnosis or institutional context because the DSM pathology has become fused with their self-concept, professional identity (many autistic and ADHD-identified people have built identity and community around the diagnosis), and institutional status (formal disability status gates accommodations). Breaking the identity lock requires community support and alternative identity frameworks that validate neurotype difference as adaptation, not defect — precisely what the constraint suppresses.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is partially obsolete: the mid-20th century need for coherent diagnostic categories enabling pharmacological research is substantially solved (current psychopharmacology is mature; new drug development does not require DSM expansion). However, the constraint persists because it has become institutionalized as the authority structure for exclusion and medicalization. A mandatrophy signal would read: the founding coordination problem is dead (we have diagnostic categories, we have pharmacological options), but the institutional apparatus designed to solve it is now maintained theatrically through category expansion and mandatory diagnosis, extracting from neurodivergent populations to sustain pharmaceutical markets and institutional conformity systems. The rising theater_ratio and the cessation of extractiveness growth at t30-t40 suggest the constraint has matured: additional category expansion is exhausted, but the enforcement machinery is maintained at high intensity to protect the accumulated capture. The tangled_rope classification (rather than snare) reflects that genuine coordination value exists alongside the extraction, and the beneficiaries could not sustain the constraint purely through coercion — they depend on the diagnostic apparatus's claim to scientific legitimacy. If that legitimacy erodes (empirical findings of continua rather than categories, institutional adoption of neurodiversity-affirming approaches), the beneficiaries' ability to enforce pathologization declines sharply.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the DSM taxonomy kernel is empirically justified: the biomedical reading (categories map to objective neurobiological disease), the critical psychiatry reading (categories are reverse-engineered from pharmaceutical markets), or the neurodiversity reading (categories pathologize natural neurological variation)?',
    'Comparative neuroscience and nosology: examine whether DSM-defined conditions show categorical boundaries or continuous gradients; whether diagnostic criteria cluster by neurobiological substrate or by pharmaceutical treatability; whether conditions are reified as disease or constructed through institutional labeling.',
    'If the biomedical reading is supported, extractiveness is lower and the constraint is genuine coordination with side effects. If critical psychiatry or neurodiversity reading is supported, extractiveness is higher and the constraint functions as a legitimation apparatus for institutional conformity and pharmaceutical extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, empirical, 'Which reading''s core epistemic premise about DSM categories is empirically grounded.').

omega_variable(
    identity_lock_mechanism_structural_vs_internalized,
    'Is the measured suppression of neurodivergent individuals structural (institutional barriers, medication side effects, economic dependence, legal status) or internalized (identification with diagnostic category, shame, belief in defectiveness)?',
    'Longitudinal observation of neurodivergent individuals after community exit to affirming environments: if suppression persists after institutional pressure is removed (joining neurodivergent-affirming communities, unmedicated, legal status changed), suppression is partially internalized; if suppression dissolves, it was primarily structural.',
    'If suppression is internalized, the constraint carries forward as self-enforcement even after institutional pressure is removed; victims remain locked by adopted self-concept. If structural, removal of institutional pressure creates space for de-internalization and reorganization around affirming identity. This affects the exit options classification: identity-locked becomes constrained or mobile once the identity frame breaks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural institutional barriers or internalized self-concept.').

omega_variable(
    pharmaceutical_beneficiary_circulation,
    'Do pharmaceutical markets benefit primarily from pathologizing neurodiversity per se, or do they benefit from treating already-pathologized conditions regardless of whether the pathologization is deserved?',
    'Business model analysis: examine pharmaceutical profit models in jurisdictions with neurodiversity-affirming diagnosis (autism as neurotype, ADHD as neurotype requiring accommodation rather than medication) and compare revenue streams; examine marketing spend and expansion patterns correlated with DSM revisions.',
    'If pharmaceutical beneficiaries depend on the DSM pathologizing act itself (not just on treating the condition), they have strong incentive to oppose neurodiversity-affirming diagnostic frameworks. If they benefit from medication regardless of pathologization framing, they could survive a paradigm shift to neurodiversity-affirming diagnosis that still includes pharmacological options. This affects whether pharmaceutical capture can be separated from the pathologization mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmaceutical_beneficiary_circulation, empirical, 'Whether pharmaceutical revenue depends on pathologization or on treatment access.').

omega_variable(
    institutional_accommodation_cost_genuineness,
    'Is the institutional cost of accommodating neurodiversity genuine resource expenditure, or is it socially constructed gatekeeping (charging for accommodations, requiring clinical intermediaries, structuring accommodation availability around diagnostic categories)?',
    'Comparative institutional cost analysis: measure true accommodation costs in institutions with existing inclusive design versus institutions requiring expensive clinical assessment; compare universal design implementation costs and outcomes; examine accommodation wait times and denial rates between clinical-gate and rights-based accommodation models.',
    'If accommodation costs are genuine and high, institutional resistance to design change reflects rational economic constraint and the tangled_rope classification holds (real coordination cost). If costs are constructed through gatekeeping, institutional resistance reflects extractive capture and the constraint should be reclassified toward snare. This affects remediation strategy: genuine coordination requires cost-sharing solutions; gatekeeping requires institutional design reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_accommodation_cost_genuineness, empirical, 'Whether accommodation costs reflect genuine resource constraints or constructed institutional gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t0, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(dsm__tr_t0, observed).
narrative_ontology:measurement(dsm__tr_t5, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(dsm__tr_t5, observed).
narrative_ontology:measurement(dsm__tr_t10, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(dsm__tr_t10, observed).
narrative_ontology:measurement(dsm__tr_t15, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement_basis(dsm__tr_t15, observed).
narrative_ontology:measurement(dsm__tr_t20, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(dsm__tr_t20, observed).
narrative_ontology:measurement(dsm__tr_t25, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement_basis(dsm__tr_t25, observed).
narrative_ontology:measurement(dsm__tr_t30, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(dsm__tr_t30, observed).
narrative_ontology:measurement(dsm__tr_t40, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(dsm__tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(dsm__be_t0, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 0, 0.61).
narrative_ontology:measurement_basis(dsm__be_t0, observed).
narrative_ontology:measurement(dsm__be_t5, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement_basis(dsm__be_t5, observed).
narrative_ontology:measurement(dsm__be_t10, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(dsm__be_t10, observed).
narrative_ontology:measurement(dsm__be_t15, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement_basis(dsm__be_t15, observed).
narrative_ontology:measurement(dsm__be_t20, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement_basis(dsm__be_t20, observed).
narrative_ontology:measurement(dsm__be_t25, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement_basis(dsm__be_t25, observed).
narrative_ontology:measurement(dsm__be_t30, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement_basis(dsm__be_t30, observed).
narrative_ontology:measurement(dsm__be_t40, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(dsm__be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t0, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(dsm__su_t0, observed).
narrative_ontology:measurement(dsm__su_t5, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(dsm__su_t5, observed).
narrative_ontology:measurement(dsm__su_t10, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(dsm__su_t10, observed).
narrative_ontology:measurement(dsm__su_t15, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(dsm__su_t15, observed).
narrative_ontology:measurement(dsm__su_t20, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement_basis(dsm__su_t20, observed).
narrative_ontology:measurement(dsm__su_t25, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement_basis(dsm__su_t25, observed).
narrative_ontology:measurement(dsm__su_t30, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 30, 0.77).
narrative_ontology:measurement_basis(dsm__su_t30, observed).
narrative_ontology:measurement(dsm__su_t40, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement_basis(dsm__su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__neurodiversity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__neurodiversity_reading, 0.12).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_pharmaceutical_market_expansion).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, institutional_conformity_coercion__neurodivergent_targets).

% DUAL FORMULATION NOTE:
% The dsm_taxonomy_kernel is contested across three structurally distinct readings (biomedical, critical psychiatry, neurodiversity). Each reading instantiates a different constraint with different ε, victim/beneficiary sets, and classifications. This file generates the neurodiversity reading only. The three readings are coexisting live positions in medical epistemology discourse, not logically foreclosed by any single framework. The network links document the kernel family: each reading influences the others (adoption of one reading creates institutional pressure against the alternatives) but does not foreclose them. Separately, this constraint influences psychiatric_pharmaceutical_market_expansion (expanded DSM categories create new pharmaceutical markets) and institutional_conformity_coercion__neurodivergent_targets (DSM pathologization legitimizes institutional coercion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__neurodiversity_reading, powerless, 0.92).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__neurodiversity_reading, moderate, 0.68).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__neurodiversity_reading, powerful, 0.15).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__neurodiversity_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
