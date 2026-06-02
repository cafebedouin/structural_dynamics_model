% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__critical_psychiatry_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dsm_taxonomy_kernel__critical_psychiatry_reading
 *   human_readable: DSM Taxonomy Reverse-Engineered from Pharmaceutical Treatment Availability
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   The Diagnostic and Statistical Manual (DSM) presents itself as a
 *   scientific nosology — a classification of naturally occurring mental
 *   disorders discovered through psychiatric research. The critical
 *   psychiatry reading instantiated in this constraint argues that the actual
 *   mechanism is reversed: pharmaceutical companies develop treatments
 *   (SSRIs, antipsychotics, stimulants, benzodiazepines) and then DSM
 *   categories are engineered to match the pharmacological offerings,
 *   creating markets for psychiatric drugs. This reading does not deny that
 *   psychiatric distress exists or that some individuals benefit from
 *   treatment; rather, it argues that the specific categories, their
 *   boundaries, their expansion over successive DSM revisions, and the
 *   treatment pathways built into the diagnostic system are driven by
 *   pharmaceutical availability and profit incentives rather than nosological
 *   discovery. The extractive mechanism operates at three levels: (1) at the
 *   level of patients, through overprescription, adverse drug effects, and
 *   identity capture into the 'mentally ill' category; (2) at the level of
 *   the psychiatric epistemic commons, through suppression of alternative
 *   frameworks (neurodiversity, peer support, social models) and corruption
 *   of research through pharmaceutical funding; (3) at the level of society,
 *   through medicalization of normal human variation and attribution of
 *   social problems (poverty, trauma, inequality) to individual neurobiology
 *   rather than structural causes. The constraint exhibits a rising
 *   trajectory in both theater_ratio and extractiveness from 1980 to 2025,
 *   reflecting the expansion of DSM categories, the increasing influence of
 *   pharmaceutical industry funding on psychiatric research and education,
 *   and the consolidation of pharmaceutical treatment as the default
 *   intervention. The theater_ratio rise indicates that the scientific
 *   legitimacy claims (evidence-based diagnosis, neurobiological validity,
 *   treatment guidelines) have become increasingly performative as actual
 *   empirical coherence has remained low and pharmaceutical financial
 *   incentives have become more transparent. Suppression has risen as
 *   institutional barriers to alternative frameworks have strengthened and
 *   the cost of questioning psychiatric diagnosis has increased.
 *
 * KEY AGENTS:
 *   - Pharmaceutical Capital: Institutional/arbitrage — primary beneficiary; designs drugs first, then creates diagnostic demand; shapes DSM revisions through research funding and advisory positions
 *   - Psychiatrists Receiving Industry Funding: Institutional/arbitrage — secondary beneficiary; provide professional legitimacy for diagnostic categories and pharmaceutical prescribing; extract income through speaker fees, research grants, consulting
 *   - Diagnostic Gatekeepers (DSM Committee Members, Nosological Authorities): Institutional/constrained — maintain diagnostic system through revision cycles; constrained by industry funding and institutional politics; beneficiaries of professional authority and publishing opportunities
 *   - Pathologized Patients: Powerless/identity_locked — primary victims; subjected to overprescription, adverse drug effects, stigma, and identity capture; identity-locked because psychiatric diagnosis becomes part of self-concept
 *   - Parents/Guardians of Diagnosed Children: Moderate/constrained — secondary victims; pressured by schools and institutions to accept diagnosis and medication; constrained by fear of losing custody or school access
 *   - Neurodivergent Communities: Moderate/constrained — mixed position; benefit from diagnosis for accommodations and benefits, harmed by pathologizing framework and overprescription; constrained by dependence on medical system for resource access
 *   - Reform-Minded Psychiatrists: Powerful/mobile — recognize extractive mechanisms but constrained by professional incentives; benefit from system they critique; mobile enough to publish critique but exiting means loss of credibility
 *   - Mad Pride/Psychiatric Survivor Movements: Organized/constrained — organized resistance building alternative frameworks (neurodiversity, peer support); constrained by lack of institutional legitimacy and funding
 *   - Psychiatric Epistemic Commons: Powerless/trapped — abstract collective; the integrity of psychiatric knowledge production; bears cost of pharmaceutical influence corruption without ability to organize or exit
 *   - Analytical Observer: Analytical/analytical — sees full structure but risks naturalizing contingent arrangements as inevitable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.58).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.62).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM Taxonomy Reverse-Engineered from Pharmaceutical Treatment Availability").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, '1ac6a692-5cb3-4888-bb3b-452a25a3266c').
narrative_ontology:cs_kernel_codification('1ac6a692-5cb3-4888-bb3b-452a25a3266c', formalized).
narrative_ontology:cs_authority_grounding('1ac6a692-5cb3-4888-bb3b-452a25a3266c', extraction).
narrative_ontology:cs_interpretation_layer_present('1ac6a692-5cb3-4888-bb3b-452a25a3266c').
narrative_ontology:cs_reading_relation('1ac6a692-5cb3-4888-bb3b-452a25a3266c', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ac6a692-5cb3-4888-bb3b-452a25a3266c', dsm_taxonomy_kernel__neurodiversity_reading, influences).
narrative_ontology:cs_axiom('1ac6a692-5cb3-4888-bb3b-452a25a3266c', foundational, pharmaceutical_availability_drives_diagnostic_category_creation).
narrative_ontology:cs_axiom_status(pharmaceutical_availability_drives_diagnostic_category_creation, holdable).
narrative_ontology:cs_axiom_grounding('1ac6a692-5cb3-4888-bb3b-452a25a3266c', pharmaceutical_availability_drives_diagnostic_category_creation, empirically_contingent).
narrative_ontology:cs_axiom('1ac6a692-5cb3-4888-bb3b-452a25a3266c', foundational, pharmaceutical_industry_extracts_value_through_diagnostic_expansion).
narrative_ontology:cs_axiom_status(pharmaceutical_industry_extracts_value_through_diagnostic_expansion, holdable).
narrative_ontology:cs_axiom_grounding('1ac6a692-5cb3-4888-bb3b-452a25a3266c', pharmaceutical_industry_extracts_value_through_diagnostic_expansion, empirically_contingent).
narrative_ontology:cs_reference_frame('1ac6a692-5cb3-4888-bb3b-452a25a3266c', psychiatric_nosology_as_science_based_discovery).
narrative_ontology:cs_drift_state('1ac6a692-5cb3-4888-bb3b-452a25a3266c', contemporary_transparent_industry_influence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1ac6a692-5cb3-4888-bb3b-452a25a3266c', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_capital).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrists_receiving_industry_funding).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, diagnostic_gatekeepers).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_subjected_to_overprescription).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, people_with_neurodivergence_pathologized).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatric_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATHOLOGIZED PATIENT (SNARE) — Identity-locked into disease category assigned by diagnostic system. Structural mobility exists (could refuse diagnosis, seek alternative frameworks) but identity has become fused with the psychiatric label. Faces overprescription, long-term adverse drug effects, and stigma. No exit pathway that preserves social legitimacy within medical institutions. Maximum extraction experienced through pharmaceutical dependency and identity capture.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__critical_psychiatry_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: PARENTS/GUARDIANS OF DIAGNOSED CHILDREN (SNARE) — Face intense institutional and social pressure to accept psychiatric diagnosis and pharmaceutical treatment for children. Constrained by school systems requiring medication for attendance, social services gatekeeping, and fear of child welfare involvement if 'medical advice' is rejected. High extraction through repeated pharmaceutical exposure starting in childhood, with long-term health costs borne by families. Limited exit options when diagnosis affects school/social services access.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__critical_psychiatry_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NEURODIVERGENT COMMUNITIES (TANGLED ROPE) — Benefit from diagnostic categories that enable access to accommodations, disability benefits, and social recognition. Simultaneously harmed by pathologizing frameworks that treat neurodivergence as defect rather than difference, and by industry-driven overprescription. Constrained by dependence on medical system for resource access. Mixed extraction: gain resource coordination through diagnosis, lose autonomy through medicalization.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARMACEUTICAL CAPITAL (ROPE) — Experiences the DSM as a coordination mechanism for market creation and demand assurance. Each new DSM category (ADHD expansion, new mood disorder subtypes, anxiety spectrum broadening) opens market segments. Institutional power to influence diagnostic criteria through research funding, continuing medical education, and advisory board positions. Low experienced extraction because the system is designed to benefit this actor; arbitrage options abundant.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__critical_psychiatry_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PSYCHIATRISTS RECEIVING INDUSTRY FUNDING (ROPE) — Primary beneficiaries through speaker fees, research grants, consulting arrangements, and pharmaceutical-funded CME. Experience the DSM as enabling their professional authority and income. Arbitrage options high — can move between pharmaceutical companies, maintain private practice, or shift to institutional roles. Coordination function: the diagnostic framework legitimates pharmaceutical prescribing as science-based medicine, generating demand for psychiatrist expertise.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__critical_psychiatry_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REFORM-MINDED PSYCHIATRISTS (TANGLED ROPE) — Operate within the system but recognize its extractive mechanisms. Benefit from institutional legitimacy and diagnostic categories (patients come seeking diagnosis), but constrained by pressure to prescribe pharmaceuticals and institutional relationships with industry. Mobile enough to publish critiques and advocate for alternatives, but exiting entirely means loss of professional credibility and income. Mixed extraction: structural benefit from the system they critique, real costs to challenging it.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: MAD PRIDE / PSYCHIATRIC SURVIVOR MOVEMENTS (SCAFFOLD) — Organized resistance to psychiatric pathologization. Building alternative frameworks (neurodiversity, Mad Pride, user-led research) with sunset logic: as non-psychiatric support systems, mutual aid, and peer-led care expand, dependence on psychiatric diagnosis for resource access decreases. Constrained by lack of institutional legitimacy and funding, but organized agency creates structural alternatives. Low theater because alternative frameworks are built on lived experience rather than scientific authority claims.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__critical_psychiatry_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: NOSOLOGICAL AUTHORITY (DSM/ICD COMMITTEES) (PITON) — The formal authority structure maintains the diagnostic system through periodic revisions, but the primary function has atrophied from discovery-based taxonomy to market legitimation. High theater: revisions are presented as scientific progress, but actual driver is pharmaceutical market requirements and political accommodation of interested stakeholders. System persists through institutional inertia and gatekeeping power, not because the nosological categories themselves have empirical coherence. Theater ratio high because the revision process performs scientific legitimacy while driven by commercial interests.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__critical_psychiatry_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From universal scope, some psychiatric taxonomy is inherent to human experience: distress, hallucinations, mood variation, and social dysfunction exist independently of nomenclature. The temptation is to naturalize the current DSM as discovery of immutable disease categories. However, the structural data reveals this as a false summit: the specific categories, their boundaries, the treatment pathways, and the extraction mechanisms are all contingent on pharmaceutical availability and industry funding. The 'natural law' framing naturalizes what is actually a constructed institutional arrangement.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__critical_psychiatry_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__critical_psychiatry_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dsm_taxonomy_kernel__critical_psychiatry_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dsm_taxonomy_kernel__critical_psychiatry_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, TR),
    TR >= 0.70.

:- end_tests(dsm_taxonomy_kernel__critical_psychiatry_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The pharmaceutical-driven diagnosis mechanism extracts value at three levels: from patients through overprescription and long-term adverse effects; from the research commons through conflict-of-interest corruption; from insurance systems and public health through excessive medicalization. However, extraction is not maximal (0.66+) because the system does provide genuine coordination benefits for some patients (access to treatment, social legitimacy, insurance coverage, accommodations) and the constraint operates through institutional structures rather than pure coercion. The rising trajectory (0.22 in 1980 → 0.58 in 2025) reflects the expansion of DSM categories and the consolidation of pharmaceutical treatment as the default intervention. Suppression (0.62): Moderate-high. Significant barriers to exit the diagnostic system include institutional gatekeeping (school systems requiring medication for attendance), social sanctions (stigma of refusing psychiatric treatment), economic dependence (insurance coverage contingent on diagnosis), and cognitive capture (identity-locking into diagnostic category). Suppression is not total because alternative frameworks (neurodiversity, peer support, Mad Pride) are emerging and some patients successfully exit psychiatric treatment. Theater ratio (0.68): High. The scientific legitimacy claims around DSM categories have become increasingly performative: diagnostic revisions are presented as evidence-based updates but driven by pharmaceutical market requirements; treatment guidelines are promoted as scientifically validated but shaped by industry-funded research; the revision process itself performs scientific authority while being structured by political accommodation of interested stakeholders. Theater has risen from 0.35 in 1980 (when DSM-III was presented as a major scientific advance) to 0.68 in 2025 (as it has become transparently influenced by pharmaceutical interests). Claimed type (tangled_rope): The system has genuine coordination functions (patients access treatment, psychiatric expertise is mobilized, public health infrastructure is organized) AND significant asymmetric extraction (pharmaceutical profits, psychiatrist income from industry, patient harm from adverse effects). Active enforcement required: the diagnostic system is maintained through institutional gatekeeping (diagnostic authority), economic incentives (pharmaceutical marketing, insurance coverage), and suppression of alternative frameworks.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits stark perspectival gaps across institutional and individual positions. Pharmaceutical capital and industry-funded psychiatrists experience the system as Rope — a coordination mechanism that legitimates their products and income. Pathologized patients experience it as Snare — trapped by diagnostic identity and institutional pressure to take medications. Reform-minded psychiatrists experience it as Tangled Rope — they benefit from the system they critique. Psychiatric survivor movements experience it as Scaffold — building alternative pathways with sunset logic as peer-led care expands. The most revealing gap is between the analytical observer's temptation to see a Natural Law (psychiatric categories as discoveries of immutable disease) and the critical psychiatry reading that exposes this as a false summit — the 'natural law' is actually a contingent institutional arrangement engineered to create pharmaceutical markets. The identity_locked exit option for pathologized patients is diagnostically significant: they could materially exit the diagnostic system (stop taking medications, seek alternative support) but cannot exit their identity as 'mentally ill' because psychiatric diagnosis has become fused with their self-concept through years of clinical labeling, internalization of psychiatric narratives, and institutional reinforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each agent are derived from their structural position: beneficiary status, exit options, and power level. Pharmaceutical capital (d ≈ 0.05): full beneficiary with arbitrage options — experiences minimal effective extraction (negative χ). Industry-funded psychiatrists (d ≈ 0.10): beneficiaries with constrained exit (professional identity dependent on pharmaceutical relationships) — low extractiveness. Pathologized patients (d ≈ 0.90): victims with identity_locked exit — experience maximum extractiveness. Reform-minded psychiatrists (d ≈ 0.55): mixed position as beneficiaries who recognize harm, mobile exit options but professional costs — moderate extractiveness. The engine derives d automatically from these structural declarations. The sigmoid f(d) function amplifies the extraction experienced by victims (high d → high f(d)) and dampens or reverses extraction experienced by beneficiaries (low d → low/negative f(d)). Scope modifier σ(S) = 1.0 at national scope, meaning no scope amplification or dampening. Chi formula: χ = ε × f(d) × σ(S). For victims with high d, χ is amplified; for beneficiaries with low d, χ is minimized or negative.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING MANDATROPHY: This constraint avoids the classic mandatrophy (must be pure extraction or pure coordination) by grounding itself in a specific reading of the contested kernel. The critical psychiatry reading acknowledges BOTH genuine coordination functions (patients access treatment, psychiatric expertise is mobilized, some individuals benefit from pharmacotherapy) AND significant asymmetric extraction (pharmaceutical profits, overprescription harms, adverse effects, epistemic corruption). The tangled rope classification captures this mixed structure. The mandatrophy is resolved not by choosing pure type but by specifying the reading that clarifies when coordination is genuine (for patients who benefit) versus when it is cover story for extraction (market-driven diagnosis of non-disease states). The biomedical reading would resolve mandatrophy differently: it would argue that extraction is minimal because the categories track genuine biological reality. The neurodiversity reading would argue that much of what is classified as pathology is actually diversity, shifting the victim set and beneficiary structure. Each reading has coherent answers to the mandatrophy question — the three readings together demonstrate that the mandatrophy itself is an artifact of trying to classify a contested kernel from a single perspective. The appropriate response is to instantiate each reading clearly and allow empirical resolution via the omega variables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pharmaceutical_influence_detection,
    'To what extent are DSM diagnostic criteria shaped by pharmaceutical treatment availability versus genuine nosological discovery?',
    'Historical analysis: track DSM revisions correlating with new drug launches; funding source analysis of DSM committee members and their financial conflicts; counterfactual analysis of which diagnoses would exist if no psychotropic drugs were available.',
    'High pharmaceutical influence: confirms critical psychiatry reading (tangled rope with high extraction). Low pharmaceutical influence: supports biomedical reading (mountain or rope). Moderate influence: supports coexistence of readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmaceutical_influence_detection, empirical, 'Degree to which DSM criteria are shaped by pharmaceutical availability').

omega_variable(
    benefit_harm_ratio_empirical,
    'For each major DSM diagnostic category, what is the actual benefit-to-harm ratio of pharmaceutical treatment in population-level outcomes?',
    'Meta-analysis of long-term outcome studies (5+ years) comparing medicated vs. unmedicated cohorts, accounting for severity selection bias; measurement of adverse effects incidence and prevalence.',
    'If benefit-to-harm ratio < 1.5 for major categories: extraction is severe, victims set is large, suppression is structural. If ratio > 2.0: coordination function is genuine, tangled rope classification valid. If intermediate: supports current classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(benefit_harm_ratio_empirical, empirical, 'Population-level benefit-to-harm ratio of psychiatric medications').

omega_variable(
    diagnostic_category_empirical_coherence,
    'Do DSM diagnostic categories represent natural kinds with coherent neurobiology, etiology, and treatment response? Or are they convenience categories constructed post-hoc around pharmaceutical effects?',
    'Biological validation studies: test whether diagnostic categories show distinct neuroimaging patterns, genetic markers, biochemical signatures, or treatment response profiles. Decompose observed neurobiological patterns by symptom cluster rather than DSM category.',
    'High coherence: supports biomedical reading. Low coherence: confirms critical psychiatry reading (categories are constructed). Moderate coherence: supports tangled rope (categories are partially genuine, partially constructed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diagnostic_category_empirical_coherence, empirical, 'Empirical coherence of DSM diagnostic categories as natural kinds').

omega_variable(
    neurodiversity_validity_alternative_frameworks,
    'Do neurodiversity and Mad Pride frameworks provide functionally equivalent or superior support pathways compared to psychiatric diagnosis and pharmaceutical treatment?',
    'Comparative outcome studies: measure wellbeing, functional capacity, social integration, and harm metrics in communities using psychiatric treatment vs. peer-led/neurodiversity frameworks. Track outcomes longitudinally.',
    'If alternative frameworks are superior: scaffold perspective is accurate, sunset logic is structural. If inferior: tangled rope (mixed benefits). If equivalent: supports neurodiversity coexistence reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neurodiversity_validity_alternative_frameworks, empirical, 'Relative efficacy of neurodiversity frameworks versus psychiatric treatment').

omega_variable(
    reading_committer_frame_ambiguity,
    'Is the critical psychiatry reading itself identity-locked by anti-pharmaceutical ideology, or does it represent genuine structural insight into pharmaceutical industry capture of psychiatry?',
    'Cross-reading validation: do the biomedical and neurodiversity readings detect different structural mechanisms that the critical psychiatry reading cannot explain? Are there cases where the critical psychiatry reading''s victim-beneficiary structure inverts under empirical scrutiny?',
    'If critical psychiatry reading is ideologically captured: reduces confidence in its empirical claims. If genuine: strengthens case for overprescription and market-driven diagnosis. If mixed: supports coexistence framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_committer_frame_ambiguity, conceptual, 'Whether critical psychiatry reading is itself identity-locked by anti-pharmaceutical ideology').

omega_variable(
    identity_lock_mechanism_interpersonal,
    'For patients classified as ''identity_locked'' in Perspective 1, what specific identity-fusion mechanisms bind them to psychiatric diagnosis, and are these mechanisms reinforced by the DSM taxonomy itself?',
    'Qualitative analysis: interview patients about their self-concept relationship to psychiatric diagnosis; measure identity fusion with diagnostic label; test whether identity decouples from diagnosis when alternative (neurodiversity) frameworks are presented.',
    'If identity lock is primary binding mechanism: confirms snare classification for this population. If identity lock is secondary to material barriers: reclassify as ''trapped'' rather than ''identity_locked''. If identity is fluid: suggests exit pathway via reframing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_interpersonal, empirical, 'Identity-fusion mechanisms binding patients to psychiatric diagnosis').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.62) primarily structural (institutional barriers to exit: loss of benefits, social sanctions, institutional gatekeeping) or internalized (the patient has absorbed the psychiatric narrative and cannot imagine alternatives)?',
    'Longitudinal tracking: measure suppression persistence after removal of structural barriers (access to neurodiversity community, peer support, legal protection from discrimination). If suppression persists: internalized. If suppression decays: structural.',
    'If primarily structural: organizational intervention (policy change, institutional reform) can reduce suppression. If primarily internalized: requires cognitive reframing and identity reconstruction — harder to change. Mixed assessment should weight both.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm_crit_theater_1980, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(dsm_crit_theater_1994, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1994, 0.5).
narrative_ontology:measurement(dsm_crit_theater_2013, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2013, 0.62).
narrative_ontology:measurement(dsm_crit_theater_2025, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2025, 0.68).

% Extraction over time
narrative_ontology:measurement(dsm_crit_extract_1980, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement(dsm_crit_extract_1994, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1994, 0.38).
narrative_ontology:measurement(dsm_crit_extract_2013, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2013, 0.52).
narrative_ontology:measurement(dsm_crit_extract_2025, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dsm_crit_suppress_1980, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(dsm_crit_suppress_1994, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1994, 0.48).
narrative_ontology:measurement(dsm_crit_suppress_2013, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2013, 0.6).
narrative_ontology:measurement(dsm_crit_suppress_2025, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.18).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_adverse_effects_accumulation).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatric_epistemic_corruption_industry_funding).

% DUAL FORMULATION NOTE:
% The DSM taxonomy kernel instantiates three structurally distinct constraints via different readings. The critical psychiatry reading (this file) models the pharmaceutical reverse-engineering hypothesis with moderate-high extraction. The biomedical reading (separate file) models DSM categories as natural-kind discoveries with lower extraction. The neurodiversity reading (separate file) models diversity-affirming alternative to pathologization with different victim/beneficiary structure. All three share the same kernel (the DSM itself) but produce different ε values, victim sets, and extraction mechanisms. They are linked via network.affects_constraints because changes to one reading's empirical status affect the others' plausibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__critical_psychiatry_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
