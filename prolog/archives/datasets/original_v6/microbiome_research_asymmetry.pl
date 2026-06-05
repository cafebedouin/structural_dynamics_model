% ============================================================================
% CONSTRAINT STORY: microbiome_research_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_microbiome_research_asymmetry, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: microbiome_research_asymmetry
 *   human_readable: Microbiome Research Asymmetry: Knowledge Extraction and Host Agency
 *   domain: biomedical/microbiome_research
 *
 * SUMMARY:
 *   Microbiome research has exploded since 2008, driven by DNA sequencing
 *   costs collapse and recognition that microbial communities profoundly
 *   affect human health, immunity, metabolism, and behavior. However, the
 *   research ecosystem is structured around an asymmetry: subjects provide
 *   samples and health data; researchers generate knowledge, publications,
 *   patents, and products; benefits accrue primarily to research
 *   institutions, pharmaceutical companies, and commercial microbiome
 *   services; subjects have minimal access to their own microbiome data, no
 *   stake in downstream discoveries, and often no awareness of secondary
 *   uses. This constraint exhibits tangled rope structure — genuine
 *   coordination (research enables health benefits and therapeutics
 *   development) coupled with asymmetric extraction (knowledge control,
 *   benefit capture, patent positioning). The constraint is maintained
 *   through a combination of institutional mechanisms (consent frameworks
 *   that don't mandate transparency), legal regimes (research data classified
 *   as institutional property), and practical barriers (microbiome
 *   interpretation requires specialized knowledge). However, organized actors
 *   (data justice movements, CARE principles, participatory genomics) are
 *   building alternative pathways with explicit benefit-sharing, community
 *   data sovereignty, and sunset logic: as data governance standards mature
 *   and funders mandate community engagement, the traditional asymmetry
 *   should decline. Extractiveness has increased from 0.35 (early microbiome
 *   era, pre-commercialization) to 0.58 (current, high-volume commercial
 *   development) but shows signs of decline (0.52) as community-based
 *   alternatives mature. Theater ratio has also increased from 0.52 to 0.68,
 *   reflecting the growing performative character of consent and ethics
 *   review relative to actual subject protection.
 *
 * KEY AGENTS:
 *   - Research Subjects: Primary victims (powerless/trapped) — provide samples and data with limited understanding of secondary uses and no benefit. Trapped by information asymmetry and dependence on research institutions for access to their own results.
 *   - Research Institutions and Universities: Primary beneficiaries (institutional/arbitrage) — control sample access, data interpretation, publication, and intellectual property. Can arbitrage across funding sources, geographic regions, and commercial partnerships.
 *   - Pharmaceutical and Microbiome Companies: Primary beneficiaries (institutional/arbitrage) — develop commercial products, secure patents, and create markets around microbiome interventions. Extract maximum value from research-generated knowledge.
 *   - Global South Communities as Cohort Sources: Secondary victims (powerful/mobile) — provide high-prevalence, diverse microbiome samples but remain outside benefit distribution. Can exit by restricting access, but face cost of lost research participation.
 *   - Clinical Trial Patients: Secondary victims (moderate/constrained) — benefit from potential treatments but bear extraction through clinical trial participation and inability to access or control downstream knowledge.
 *   - Data Justice Movements and Community Advisory Boards: Organized actors (organized/constrained) — advocating for community data sovereignty, benefit-sharing, and participatory research governance. Building alternatives with sunset logic.
 *   - Bioethics and IRB Systems: Institutional theater maintainers (institutional/arbitrage) — perform subject protection functions through informed consent, risk-benefit review, data governance policies. Theater is substantial; functional protection remains limited.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(microbiome_research_asymmetry, 0.58).
domain_priors:suppression_score(microbiome_research_asymmetry, 0.62).
domain_priors:theater_ratio(microbiome_research_asymmetry, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(microbiome_research_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(microbiome_research_asymmetry, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(microbiome_research_asymmetry, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(microbiome_research_asymmetry, tangled_rope).
narrative_ontology:human_readable(microbiome_research_asymmetry, "Microbiome Research Asymmetry: Knowledge Extraction and Host Agency").
narrative_ontology:topic_domain(microbiome_research_asymmetry, "biomedical/microbiome_research").

domain_priors:requires_active_enforcement(microbiome_research_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(microbiome_research_asymmetry, research_institutions).
narrative_ontology:constraint_beneficiary(microbiome_research_asymmetry, pharmaceutical_companies).
narrative_ontology:constraint_beneficiary(microbiome_research_asymmetry, commercial_microbiome_services).
narrative_ontology:constraint_victim(microbiome_research_asymmetry, research_subjects).
narrative_ontology:constraint_victim(microbiome_research_asymmetry, host_patient_autonomy).
narrative_ontology:constraint_victim(microbiome_research_asymmetry, microbiome_commons_knowledge).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESEARCH SUBJECT (SNARE) — Trapped in asymmetric information extraction. Subjects provide biological samples (stool, saliva, skin) and lifestyle data under consent forms they do not fully understand. No ability to access their own microbiome data, benefit from discoveries about their own biology, or control secondary uses of samples. Maximum experienced extraction with minimal ability to exit or negotiate.
constraint_indexing:constraint_classification(microbiome_research_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PATIENT IN CLINICAL INTERVENTIONS (TANGLED ROPE) — Constrained by health status and dependency on clinical interventions. Benefits from potential microbiome therapies (FMT, probiotics, dietary interventions) coordinated through research, but also bears extraction: clinical trials generate proprietary knowledge, patent positions, and commercial products that are unavailable or unaffordable to the source population. Genuine coordination (treatment development) alongside asymmetric extraction (benefit capture).
constraint_indexing:constraint_classification(microbiome_research_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RESEARCH INSTITUTIONS AND PHARMACEUTICAL COMPANIES (ROPE) — Primary beneficiaries (institutional/arbitrage). Experience the constraint as coordination: collecting microbiome samples enables research, treatment development, and commercial products. Can arbitrage knowledge across jurisdictions, licensing agreements, and product development pipelines. Net flow of extraction runs toward these actors. Their exit option is arbitrage — they can shift research focus, geographic regions, or therapeutic targets.
constraint_indexing:constraint_classification(microbiome_research_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL SOUTH SOURCE POPULATIONS (TANGLED ROPE) — Extracted from at scale and over generations. Communities provide high-prevalence, diverse microbiome samples (precisely because of the health conditions and environmental exposures that make their microbiomes scientifically valuable). Research generates disease-understanding and treatment pipelines that remain inaccessible in source regions. Also benefits from basic health research access and diagnostic capabilities developed through studies. Mobile but at high cost — communities can exit by restricting sample access, but this costs them health research participation. Extraction is asymmetric and structural.
constraint_indexing:constraint_classification(microbiome_research_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL ETHICS REVIEW (PITON) — IRB/ethics review processes perform the ritual of subject protection while largely permitting the asymmetric extraction to continue. Informed consent forms, risk-benefit analyses, and data governance plans constitute a performative framework. Theater ratio is high because the institutional theater (consent documents, ethics review) is substantial while functional protection of subjects remains low — secondary use restrictions are routinely waived, genetic data is shared widely, and subjects have no access to or control over downstream knowledge. The review ritual persists through institutional inertia.
constraint_indexing:constraint_classification(microbiome_research_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, knowledge asymmetries in research are unavoidable: subjects cannot possess the specialized training to understand microbiome ecology, statistical inference, or clinical trial design. The extraction appears as an immutable feature of the research process itself. However, the structural data contradicts this natural law framing — the asymmetry is not about knowledge complexity but about control, access, and benefit distribution, all of which are contingent institutional arrangements. This perspective represents a false summit.
constraint_indexing:constraint_classification(microbiome_research_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: DATA JUSTICE AND PARTICIPATORY RESEARCH MOVEMENTS (SCAFFOLD) — Organized agents (community advisory boards, CARE principles, participatory genomics initiatives, CBPR frameworks) are building alternative research governance pathways: community data sovereignty, benefit-sharing agreements, open-access publication of findings, and community-controlled biobanks. These alternatives have lower extraction and higher theater reduction because they involve genuine community negotiation over data use. Constrained by resource limitations and resistance from incumbent research institutions, but organizing toward structural sunset of the traditional asymmetry. Sunset horizon: 15-25 years as funders mandate community engagement and data access standards.
constraint_indexing:constraint_classification(microbiome_research_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(microbiome_research_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(microbiome_research_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(microbiome_research_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(microbiome_research_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(microbiome_research_asymmetry, TR),
    TR >= 0.70.

:- end_tests(microbiome_research_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and increasing. The original microbiome research era (2008-2015) was extractive but modest — samples were valuable for basic science, but commercialization potential was unclear. Extractiveness has grown with the rise of commercial microbiome products (probiotics, FMT services, microbiome-guided diets) and patent positioning around microbiome biomarkers. Subjects provide the raw material for this value chain but have no ownership stake, data access, or benefit distribution. The value has increased from 0.35 to 0.58 as the commercial ecosystem matured. Suppression (0.62): High. Multiple barriers prevent subjects from exercising agency: information asymmetry (most subjects cannot interpret microbiome data), legal barriers (research data is institutional property, not subject property), practical barriers (no infrastructure for returning results to subjects), and institutional inertia (researcher incentives favor data hoarding and exclusive licensing). Subjects have no clear exit mechanism and face significant knowledge barriers. Theater ratio (0.68): High and increasing. Informed consent documents, IRB review processes, data governance plans, and ethics committees constitute substantial performative activity. The theater has increased as scrutiny of research ethics has increased, but functional subject protection has not kept pace. Secondary use restrictions are routinely waived through broad consent language; genetic data is shared through repositories with minimal oversight; subjects have no practical access to results or control over downstream uses. The institutions maintain theater as legitimacy cover while extraction continues.
 *
 * PERSPECTIVAL GAP:
 *   Research subjects and global institutions have opposite directionalities despite occupying the same physical relationship to samples. This gap reveals that 'power' is not absolute but relative to the constraint. A globally powerful institution is powerless within the research subject's experience — they cannot access their own data, cannot negotiate licensing agreements, cannot arbitrage across projects. Conversely, a research subject with low global power is a primary extractor in the research ecosystem: their microbiome data is the raw material from which all value is extracted. The gap shows that the classification (Snare from subjects' view, Rope from institutions' view, Tangled Rope from source communities' view) is not an error but a reflection of genuinely different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the structural position of each actor relative to extraction flow. Research subjects are trapped with no exit options; they bear full extraction (d → 1.0, high f(d)). Research institutions have arbitrage options; they benefit from the extraction (d → 0.0, negative f(d)). Global South communities are mobile but at high cost (d → 0.75-0.85); they experience moderate-to-high extraction but retain some agency. Clinical trial patients are constrained by health status; they experience moderate extraction but also receive some benefit from treatment development (d → 0.55-0.65). Data justice movements are organized with constrained but improving exit paths (d → 0.40-0.50); they see moderate extraction but increasing ability to shift the structural relationship through policy and governance change. The extracted values (extracted = χ = ε × f(d) × σ(S)) scale with scope — global scope (σ=1.2) amplifies extraction relative to local scope (σ=0.8).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves mandatrophy by satisfying all three gates. First, beneficiaries gate: research institutions, pharmaceutical companies, and commercial services clearly benefit (extractiveness flows toward them). Second, victims gate: research subjects, host patient autonomy, and the microbiome commons knowledge all bear costs (asymmetric extraction). Third, active enforcement gate: institutional mechanisms (consent frameworks, data governance, biobanks, publication norms) actively maintain the asymmetry. The constraint cannot be reduced to pure coordination (Rope) because of the asymmetric extraction and suppression. The extraction is real and substantial. However, it cannot be reduced to pure extraction (Snare) because genuine coordination occurs: research genuinely develops therapeutics, subjects genuinely benefit from health advances, and the sample collection mechanism solves a real coordination problem (how to identify microbiome patterns across diverse populations). The hybrid classification is structural, not perspectival. The constraint will remain tangled rope even after data justice reforms, because some level of researcher control and coordination structure will persist — the sunset is toward more balanced extraction and benefit-sharing, not toward pure coordination or complete exit of the research mechanism. The theater ratio increase from 0.52 to 0.68 reflects growing gap between the institutional legitimacy performance (ethics committees, consent forms, governance reviews) and functional subject protection — a sign of incipient Piton degradation if the performance-function gap continues to widen.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    knowledge_accessibility_threshold,
    'At what level of training and literacy can a research subject meaningfully understand their own microbiome data and make autonomous decisions about its use?',
    'Comparative study of informed consent comprehension in microbiome studies; pre- vs post-intervention literacy assessment; tracking of subject questions and decision-making patterns',
    'If threshold is very high (specialized education required): extraction is unavoidable by design (mountain framing). If threshold is achievable (secondary education + community education): extraction is contingent and remedial (snare/tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_accessibility_threshold, empirical, 'Knowledge threshold for autonomous subject decision-making about microbiome data').

omega_variable(
    secondary_use_data_leakage,
    'What fraction of microbiome samples collected for one study are actually used in secondary research, sold to commercial entities, or accessed for purposes beyond the original consent scope?',
    'Audit of biobank sample tracking; FOIA requests for sample transfer agreements; longitudinal tracking of cohort samples across publications and commercial products',
    'If leakage is high (>60%): extraction mechanism is structural and system-wide, supporting snare classification. If leakage is low (<10%): extraction is limited to intent, supporting rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secondary_use_data_leakage, empirical, 'Proportion of samples used in secondary research beyond original consent').

omega_variable(
    benefit_repatriation_mechanisms,
    'Do benefit-sharing agreements (free access to microbiome testing, free treatments, health education) actually reach source communities, or do they remain symbolic?',
    'Community survey of health benefits received; tracking of open-access publications from microbiome studies; audit of licensing fees vs community benefit payments',
    'If benefits are real and substantial: extraction is genuine but negotiated coordination (tangled rope confirmed). If benefits are symbolic or absent: extraction is asymmetric and coercive (snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_repatriation_mechanisms, empirical, 'Whether benefit-sharing mechanisms actually reach source communities').

omega_variable(
    microbiome_data_ownership_regime,
    'Is microbiome data classified as personal biological material (subject retains rights) or as research data (institution controls, subject has limited claim)?',
    'Comparative legal analysis across jurisdictions; tracking of court rulings and legislative changes (e.g., California health data privacy reforms); analysis of researcher attitudes toward data control',
    'If classified as personal material: subjects have property rights and can negotiate sale/licensing (constraint shifts toward coordination). If classified as research data: institutional control is assumed and extraction is routine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(microbiome_data_ownership_regime, conceptual, 'Legal classification of microbiome data ownership').

omega_variable(
    therapeutic_translation_timeline,
    'How long does it take for microbiome discoveries made from research cohorts to translate into accessible treatments in source communities? Does the timeline justify the lag in benefit repatriation?',
    'Historical case studies of specific microbiome discoveries (e.g., C. difficile FMT, butyrate producers in IBD); timeline analysis from sample collection to clinical availability in source vs non-source regions',
    'If timeline is long (>20 years) but benefits eventually reach sources: extraction is time-asymmetric but eventual coordination (tangled rope). If benefits never reach sources: extraction is permanent (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(therapeutic_translation_timeline, empirical, 'Timeline from microbiome discovery to treatment availability in source communities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(microbiome_research_asymmetry, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(micro_tr_t0, microbiome_research_asymmetry, theater_ratio, 0, 0.52).
narrative_ontology:measurement(micro_tr_t8, microbiome_research_asymmetry, theater_ratio, 8, 0.62).
narrative_ontology:measurement(micro_tr_t15, microbiome_research_asymmetry, theater_ratio, 15, 0.68).
narrative_ontology:measurement(micro_tr_t25, microbiome_research_asymmetry, theater_ratio, 25, 0.55).

% Extraction over time
narrative_ontology:measurement(micro_be_t0, microbiome_research_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(micro_be_t8, microbiome_research_asymmetry, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(micro_be_t15, microbiome_research_asymmetry, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(micro_be_t25, microbiome_research_asymmetry, base_extractiveness, 25, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(microbiome_research_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(microbiome_research_asymmetry, biobank_data_governance).
narrative_ontology:affects_constraint(microbiome_research_asymmetry, microbiome_therapeutics_access).
narrative_ontology:affects_constraint(microbiome_research_asymmetry, global_health_research_equity).

% DUAL FORMULATION NOTE:
% Microbiome research asymmetry decomposes into distinct constraints at different observables: (1) sample_collection_coordination (ε≈0.25, Rope) — genuine coordination for identifying health-microbiome associations. (2) microbiome_research_asymmetry (ε≈0.58, Tangled Rope) — asymmetric knowledge and benefit control. (3) microbiome_therapeutic_access_gap (ε≈0.70, Snare) — asymmetric access to treatments derived from research. Each has different beneficiaries, victims, and measurement horizons. The upstream constraint (sample collection) is often confused with the downstream constraint (benefit distribution), leading to misclassification as pure coordination when the real structure is tangled extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(microbiome_research_asymmetry, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
