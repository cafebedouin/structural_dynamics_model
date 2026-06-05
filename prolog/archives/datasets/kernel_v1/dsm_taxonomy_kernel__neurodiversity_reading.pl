% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__neurodiversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   constraint_id: dsm_taxonomy_kernel__neurodiversity_reading
 *   human_readable: DSM Pathologization of Neurodiversity: Coercive Normalization as Extractive Classification
 *   domain: medical_epistemology/psychiatric_taxonomy/neurodiversity
 *
 * SUMMARY:
 *   The DSM (Diagnostic and Statistical Manual) taxonomy, as read through the
 *   neurodiversity lens, represents a constraint that pathologizes natural
 *   human neurological variation to enforce institutional conformity. From
 *   this reading, the DSM is not a neutral medical classification tool but an
 *   apparatus of coercive normalization that extracts self-determination from
 *   neurodivergent individuals while serving the interests of institutional
 *   gatekeepers (schools, employers, insurance companies, carceral systems).
 *   The neurodiversity reading instantiates the kernel (DSM diagnostic
 *   system) but reframes what the kernel does: rather than discovering
 *   illness, it manufactures pathology to justify exclusion and forced
 *   normalization. The constraint exhibits high extractiveness (0.68) because
 *   the pathologization itself is the harm — the labeling creates legal and
 *   social scaffolding for institutional discrimination that neurodivergent
 *   individuals cannot refuse without losing access to accommodations,
 *   employment protections, and benefits. High suppression (0.72) reflects
 *   that exit is structurally impossible: refusal to accept the pathologizing
 *   diagnosis triggers loss of accommodations, benefits, and institutional
 *   access, effectively trapping neurodivergent individuals within the
 *   classification system. Rising extractiveness over the 30-year interval
 *   (0.42 → 0.68) tracks the increasing intensity of neurodiversity
 *   gatekeeping — earlier DSM editions had more permissive diagnostic
 *   criteria; successive revisions have narrowed eligibility, concentrated
 *   diagnostic power in clinicians, and tightened institutional requirements
 *   for documentation. The theater_ratio (0.58) reflects that much DSM
 *   classification is performative administrative activity rather than
 *   genuine clinical insight: clinicians apply DSM criteria not to provide
 *   treatment insight but to generate billing codes, determine resource
 *   allocation, and produce documents for institutional gatekeeping (school
 *   accommodations, employment accommodations, disability benefits). This
 *   reading coexists with the biomedical reading (which sees DSM as objective
 *   illness discovery) and the critical psychiatry reading (which sees DSM as
 *   broader psychiatric iatrogenesis). All three are live positions held by
 *   different parties — the biomedical clinician, the neurodiversity
 *   advocate, and the critical psychiatrist — but this reading foregrounds
 *   the extraction mechanism specific to the neurodiversity lens: coercive
 *   normalization via pathologization.
 *
 * KEY AGENTS:
 *   - Neurodivergent Individuals: Primary victims (powerless/trapped) — subject to forced normalization, denial of self-determination, institutional exclusion unless diagnostic compliance achieved
 *   - Institutional Gatekeepers: Primary beneficiaries (institutional/arbitrage) — schools, employers, government agencies use DSM classification to manage populations, allocate resources, and enforce conformity
 *   - Mental Health Professionals: Intermediary (moderate/constrained) — forced to pathologize within institutional constraints; limited autonomy despite potential neurodiversity-affirming values
 *   - Pharmaceutical Industry: Secondary beneficiary (institutional/arbitrage) — profits from DSM diagnostic categories driving medication prescriptions
 *   - Neurodiversity Movement: Organized resistance (organized/trapped) — advocates for alternative framing but remains trapped within DSM institutional requirements
 *   - DSM-Alternative Coalition: Powerful actors building exit pathways (powerful/mobile) — developing non-pathologizing classification frameworks
 *   - Insurance/Billing Systems: Infrastructure beneficiary (institutional/arbitrage) — require DSM codes for claims processing and profit optimization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.68).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.72).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, snare).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Pathologization of Neurodiversity: Coercive Normalization as Extractive Classification").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical_epistemology/psychiatric_taxonomy/neurodiversity").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, 'c70fc253-8778-4302-935f-6dc59f800ae5').
narrative_ontology:cs_kernel_codification('c70fc253-8778-4302-935f-6dc59f800ae5', formalized).
narrative_ontology:cs_authority_grounding('c70fc253-8778-4302-935f-6dc59f800ae5', extraction).
narrative_ontology:cs_interpretation_layer_present('c70fc253-8778-4302-935f-6dc59f800ae5').
narrative_ontology:cs_reading_relation('c70fc253-8778-4302-935f-6dc59f800ae5', dsm_taxonomy_kernel__biomedical_reading, forecloses).
narrative_ontology:cs_reading_relation('c70fc253-8778-4302-935f-6dc59f800ae5', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('c70fc253-8778-4302-935f-6dc59f800ae5', foundational, neurodiversity_as_natural_variation).
narrative_ontology:cs_axiom_status(neurodiversity_as_natural_variation, holdable).
narrative_ontology:cs_axiom_grounding('c70fc253-8778-4302-935f-6dc59f800ae5', neurodiversity_as_natural_variation, deontological).
narrative_ontology:cs_axiom('c70fc253-8778-4302-935f-6dc59f800ae5', foundational, self_determination_inviolability).
narrative_ontology:cs_axiom_status(self_determination_inviolability, holdable).
narrative_ontology:cs_axiom_grounding('c70fc253-8778-4302-935f-6dc59f800ae5', self_determination_inviolability, deontological).
narrative_ontology:cs_reference_frame('c70fc253-8778-4302-935f-6dc59f800ae5', neurodiversity_as_natural_variation).
narrative_ontology:cs_drift_state('c70fc253-8778-4302-935f-6dc59f800ae5', contemporary_gatekeeping_intensification, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c70fc253-8778-4302-935f-6dc59f800ae5', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, institutional_gatekeepers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, compliance_enforcement_apparatus).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, self_determination_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEURODIVERGENT INDIVIDUAL (SNARE) — Trapped by diagnostic classification machinery. The DSM pathologization creates legal, educational, and employment barriers that make refusal to conform impossible. Normalization is enforced through exclusion from institutions (schools, workplaces, housing) unless compliance achieved. No exit option; maximum experienced extraction in the form of forced identity reconstruction and denial of neurodivergent self-determination.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__neurodiversity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EDUCATIONAL/INSTITUTIONAL SYSTEM (ROPE) — Experiences DSM categories as pure coordination: standardized diagnostic categories enable resource allocation, accommodation decisions, and institutional management. The system benefits from the ability to label, track, and process populations according to recognized diagnostic codes. Extraction runs toward institutional systems; they see the constraint as coordination mechanism, not burden.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__neurodiversity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: MENTAL HEALTH PROFESSIONAL (TANGLED ROPE) — Constrained by licensing and insurance systems that require DSM diagnosis codes. The professional may genuinely value neurodivergent self-determination, but the institutional structure (billing systems, diagnostic manuals, legal accountability) forces diagnostic labeling to function at all. Some coordination benefit (can now access treatment and accommodations for those who want them); significant extraction (professional autonomy constrained, forced to pathologize what may be variation, complicit in coercive normalization for those who don't consent).
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__neurodiversity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: NEURODIVERSITY MOVEMENT (SNARE) — Organized agents within the neurodivergent community recognize the constraint as extractive and resist, but remain trapped within the DSM apparatus. Even radical refusal must navigate institutional systems that require diagnostic codes (accessing disability benefits, educational accommodations, employment protections). The very institutions designed to protect require the pathologizing classification. High-capacity organized actors still cannot fully exit.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__neurodiversity_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: DSM-ALTERNATIVE COALITION (SCAFFOLD) — Powerful actors (international disability rights bodies, neuroaffirming practitioners, neurodiversity-centered research programs) are building alternative classification frameworks outside the DSM apparatus. These alternatives do not pathologize neurodiversity but instead describe cognitive profiles and environmental fit. As these frameworks mature (Autism Speaks move toward neurodiversity frame, WHO ICD-11 neurodiversity acknowledgment), they create jurisdictional alternatives to DSM authority. This is sunset logic: the DSM's exclusive gatekeeping power is declining as alternatives gain institutional legitimacy.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__neurodiversity_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: DSM PUBLISHING AUTHORITY (PITON) — The DSM classification system persists through institutional inertia despite declining functional authority. Insurance systems, schools, and employers still require DSM codes, but the legitimacy of the system itself is increasingly contested. The high theater_ratio reflects that much DSM classification is performative: diagnostic criteria are applied to administrative populations for billing and resource allocation rather than representing genuine clinical insight. The authority knows its own functional decline (periodic revisions, defensive hedging language) but maintains the system because alternatives haven't fully replaced it.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__neurodiversity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — A naive analytical perspective risks treating the DSM pathologization as a natural law — 'variation that deviates from normal distribution requires medical classification.' From a civilizational/universal scope, this framing appears as immutable: every human population has trait distributions; some deviation from modal behavior is inherent to biology. This naturalizes what is actually a contingent choice: to construct neurological variation as illness rather than as diversity, accommodation-eligible trait, or neutral profile. The engine's false summit detector will identify this perspective as a naturalization of an institutional arrangement, not a discovery of natural law.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__neurodiversity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dsm_taxonomy_kernel__neurodiversity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dsm_taxonomy_kernel__neurodiversity_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, TR),
    TR >= 0.70.

:- end_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The primary extraction mechanism is the denial of self-determination. Neurodivergent individuals are forced to accept a pathologizing diagnosis (reframing neurological variation as disorder) in exchange for institutional access (accommodations, employment protections, disability benefits). The neurodiversity reading treats this forced identity reconstruction as the extraction harm itself, not as a necessary medical classification. The rising trajectory (0.42 → 0.68) reflects intensifying gatekeeping: earlier DSM criteria allowed more individuals to claim neurodivergent identity without deep medicalization; successive versions have tightened diagnostic criteria, requiring more extensive documentation and clinician gatekeeping, increasing the extraction cost of accessing accommodations. Suppression (0.72): High. Structural barriers prevent exit: (1) institutional requirement for DSM diagnosis to access accommodations and legal protections, (2) employment discrimination against openly neurodivergent individuals without documented diagnosis, (3) loss of disability benefits if diagnosis refused, (4) school exclusion or forced normalization if diagnosis not documented. No exit option exists that preserves institutional access. Theater (0.58): Moderate. Much DSM classification is administrative performance rather than clinical insight. Clinicians apply criteria primarily to generate codes for resource allocation and institutional gatekeeping rather than to provide differential treatment or insight. The criteria themselves are contested (DSM-5 changes from DSM-IV were driven by administrative convenience, not empirical discovery), reflecting that the system's legitimacy rests on institutional authority rather than objective validity. Rising theater trajectory (0.48 → 0.58) reflects increasing performativity: as neurodiversity critique has intensified, DSM defense has become more defensive and performative (DSM-5's neurodiversity language additions) while institutional gatekeeping has intensified, creating theater gap.
 *
 * PERSPECTIVAL GAP:
 *   The neurodivergent individual sees a Snare (trapped, no exit, coercive normalization). The institutional system sees a Rope (pure coordination mechanism for managing populations and allocating resources). The mental health professional experiences Tangled Rope (genuine coordination function in accessing care, but extraction through forced pathologization and constrained professional autonomy). The organized neurodiversity movement sees a Snare even at the organizational level (organized power insufficient to exit systemic requirements). The alternative-framework coalition sees a Scaffold (temporary authority, sunset in progress as alternatives gain legitimacy). The DSM authority sees a Piton (vestigial ritual, knows its functional decline, maintains through inertia). The naive analytical observer risks seeing a Mountain (variation from normal distribution is inherently pathological — a natural law). The neurodiversity reading rejects this last perspective as a false summit: the medicalization of variation is institutional choice, not biological law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from structural position. Neurodivergent individuals as victims with trapped exit = high d (0.90+) → high f(d) ≈ 1.35 → high experienced χ. Institutional gatekeepers as beneficiaries with arbitrage options = low d (0.10) → negative f(d) ≈ -0.08 → negative χ (extraction flows toward them). Mental health professionals as constrained moderates = medium d (0.55) → moderate f(d) ≈ 0.75 → moderate experienced χ. The high suppression (0.72) is a raw structural property: the institutional barriers are real and substantial regardless of power or scope. What changes with perspective is how agents experience the constraint's extractiveness, but the suppression itself is uniform. Scope amplification (global scope σ(S) = 1.2) increases χ slightly: the DSM's global reach makes verification and exit harder, darkening the extraction. The analytical perspective's machine-derived d (from trapped victim + zero exit) produces mountain-like immutability perception, but only because the perspective conflates victims with analytical observers — the engine's false summit detector will catch this.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by showing that the DSM is structurally both coordination mechanism AND extraction apparatus. The biomedical reading sees pure coordination (disorder discovery enabling treatment). The neurodiversity reading sees pure extraction (pathologization enabling coercive normalization). Both are right. The constraint exhibits tangled rope properties from the mental health professional's perspective (genuine care provision coordination + forced pathologization extraction), snare properties from the neurodivergent individual's perspective (trapped, no exit, coercive), and rope properties from the institutional system's perspective (pure coordination for resource management). The mandatrophy is resolved by recognizing that the DSM serves different structural functions for different agents: it genuinely coordinates treatment and accommodations access for those who want those things, while it simultaneously extracts self-determination from those who don't want medicalization. The high theater ratio indicates that much of the coordination function is performative — the actual clinical insight value is lower than the administrative coordination value. The rising extractiveness suggests that institutional gatekeeping has intensified beyond what the original neurodiversity framework anticipated: the DSM has become more coercive over time, not less, despite neurodiversity critique. This maps to the general law of extractive constraint evolution: extraction intensity tends to increase over time unless actively resisted (snare ratchet pattern).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pathologization_as_harm_vs_neutral_classification,
    'Is the DSM pathologization of neurodiversity inherently harmful, or can neutral diagnostic coding serve the same institutional coordination function without extraction?',
    'Empirical comparison of outcomes between DSM diagnostic framing (pathology language) vs. neurodiversity-affirming profile descriptions within identical institutional contexts (schools, employers, benefits systems). Track self-reported stigma, coercion intensity, and self-determination preservation.',
    'If harm is inherent to pathologization language itself: ε remains high even with identical institutional function. If harm flows from extraction (coercive norming) rather than from diagnosis: neutral coding with extractive enforcement is still Snare. If harm is primarily in denial of self-determination: alternative framings that preserve autonomy may reduce ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pathologization_as_harm_vs_neutral_classification, empirical, 'Whether DSM pathologization language itself is extractive or whether extraction flows from institutional enforcement mechanisms').

omega_variable(
    medical_model_vs_social_model_epistemic_incommensurability,
    'Are the biomedical reading (neurodiversity as disorder requiring treatment) and the neurodiversity reading (neurodiversity as variation requiring accommodation) logically foreclosing each other, or merely competing institutional framings that coexist?',
    'Philosophical/conceptual analysis of whether both readings can be simultaneously true in a single coherent framework. Test: can an individual hold both ''autism is a disorder'' and ''autism is neutral variation'' without internal contradiction? Or does accepting one necessitate rejecting the other?',
    'If foreclosing: reading_relations should use ''forecloses'' rather than ''coexists_with.'' If compatible: both readings are live positions. Affects how terminal attractor states are computed across the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_model_vs_social_model_epistemic_incommensurability, conceptual, 'Whether biomedical and neurodiversity readings logically foreclose or coexist').

omega_variable(
    beneficiary_identification_extraction_vehicle,
    'What is the primary beneficiary of DSM pathologization — pharmaceutical industry (revenue), institutional gatekeepers (control), insurance systems (coding/billing), or compliance enforcement apparatus (social conformity)?',
    'Structural analysis of extraction flows: Who captures career benefits, financial rents, or institutional control from the DSM classification? Track pharmaceutical marketing tied to DSM categories, institutional cost-shifting via diagnosis-based resource allocation, insurance company profit margins on diagnostic coding.',
    'If primary beneficiary is pharmaceutical industry: snare becomes explicitly about medicalization-for-profit. If primary beneficiary is institutional gatekeepers: snare is about control and conformity enforcement. Different beneficiaries imply different mandatrophy pathways and different dissolution mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_identification_extraction_vehicle, empirical, 'Primary beneficiary identity within DSM pathologization extraction').

omega_variable(
    neurodiversity_reading_as_kernel_reading,
    'Is the neurodiversity reading instantiating a commitment to the same kernel (DSM diagnostic apparatus) with alternative interpretation, or rejecting the kernel entirely in favor of a competing classification system?',
    'Examine whether neurodiversity movement''s institutional presence (advocacy, alternative frameworks, DSM critique) operates WITHIN the DSM apparatus or OUTSIDE it. If neurodiversity advocates seek DSM revision (staying within the kernel), it''s a reading. If they seek complete replacement (kernel rejection), it''s not a reading but a competing system.',
    'If kernel-reading: the DSM persists; this constraint persists. If kernel-rejection: this constraint is actually modeling the emergence of a successor classification system with its own constraint dynamics. The cs_structure.kernel_codification and reading_relations would need reframing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neurodiversity_reading_as_kernel_reading, conceptual, 'Whether neurodiversity reading operates within or outside the DSM kernel').

omega_variable(
    coercive_norming_mechanism_institutional_vs_internalized,
    'Is the coercive normalization enforced through institutional barriers (school exclusion, employment discrimination, benefits denial) or through internalized pathologization (shame, identity fusion with ''disorder'' label)?',
    'Empirical separation: measure enforcement intensity in jurisdictions with strong disability rights/accommodations laws (institutional barriers weakened) vs. permissive jurisdictions (institutional barriers active). Measure internalized shame/identity fusion independent of institutional coercion.',
    'If primarily institutional: institutional reform (accommodations laws, anti-discrimination enforcement) can reduce suppression. If primarily internalized: institutional reform alone insufficient; cultural cognitive capture persists. Affects whether constraint is truly Snare (trapped) or could become mobile/constrained under institutional change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercive_norming_mechanism_institutional_vs_internalized, empirical, 'Whether coercive norming is institutional, internalized, or both').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm_neuro_theater_t0, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(dsm_neuro_theater_t15, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 15, 0.54).
narrative_ontology:measurement(dsm_neuro_theater_t30, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(dsm_neuro_extract_t0, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dsm_neuro_extract_t15, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(dsm_neuro_extract_t30, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dsm_neuro_suppress_t0, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dsm_neuro_suppress_t15, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(dsm_neuro_suppress_t30, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__neurodiversity_reading, identity_coordination).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, pharmaceutical_incentive_alignment_pathologization).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, school_exclusion_via_diagnostic_gatekeeping).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, disability_benefits_medicalization_trap).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, employment_discrimination_diagnostic_requirement).

% DUAL FORMULATION NOTE:
% The DSM taxonomy kernel has three reading-specific constraint stories: (1) biomedical_reading — DSM as objective illness discovery (Mountain/Rope depending on perspective), (2) critical_psychiatry_reading — DSM as broader psychiatric iatrogenesis (Snare/Tangled Rope), (3) neurodiversity_reading — DSM as pathologization of natural variation enabling coercive norming (Snare from victim perspective, Rope from institutional perspective). Each reading decomposes differently and affects different downstream constraints. The neurodiversity reading specifically affects institutional constraints that depend on DSM diagnostic gatekeeping (educational accommodations, employment protections, benefits access) because it reframes the diagnostic requirement as extraction rather than neutral classification. Network links show how DSM pathologization creates bottlenecks in accommodation access, employment, and benefits systems — constraints that would not exist (or would have different structure) under alternative classification frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__neurodiversity_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
