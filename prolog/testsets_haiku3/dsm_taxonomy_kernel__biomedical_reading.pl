% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__biomedical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__biomedical_reading, []).

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
 *   constraint_id: dsm_taxonomy_kernel__biomedical_reading
 *   human_readable: DSM Biomedical Framing: Categories as Discoverable Disease Entities
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction
 *
 * SUMMARY:
 *   Under the biomedical reading of the DSM taxonomy kernel, psychiatric
 *   diagnostic categories are understood to map objective neurobiological
 *   disease entities discoverable through empirical research. This reading
 *   instantiates a tangled-rope constraint: it coordinates genuine clinical
 *   communication and evidence-based treatment (the coordination function)
 *   while simultaneously enabling pharmaceutical market expansion,
 *   institutional behavioral control, mandatory pharmaceutical intervention,
 *   and loss of legal capacity for diagnosed persons (the extraction
 *   function). The biomedical framing—that DSM categories are natural disease
 *   discoveries rather than administrative constructs—is the stabilized
 *   kernel text. This reading's core claim is that psychiatric categories CAN
 *   be empirically validated as natural kinds corresponding to distinct
 *   neurobiological mechanisms. Extractiveness has risen monotonically across
 *   the interval (1980–2026): diagnostic thresholds have contracted,
 *   off-label pharmaceutical use has expanded, and institutional reliance on
 *   DSM categories for exclusionary decisions has intensified. Theater ratio
 *   has also risen, indicating that a growing share of diagnostic and
 *   enforcement activity defends the biomedical framing against challenges
 *   rather than performing the coordination function itself.
 *
 * KEY AGENTS:
 *   - Psychiatric establishment (DSM authors): institutional agenda-setter; benefits from diagnostic authority and research funding capture; frames the reading as objective science.
 *   - Pharmaceutical manufacturers: institutional beneficiary; develop and market medications aligned with DSM categories; benefit from market expansion as diagnostic scope widens.
 *   - Persons meeting diagnostic criteria: powerless payers, identity-locked; bear costs of mandatory treatment, loss of legal capacity, and permanent disease labeling; may benefit from treatment access.
 *   - Neurodiversity advocates: excluded from diagnostic authority; would argue DSM categories pathologize human variation.
 *   - Critical psychiatry researchers: excluded from mainstream institutional authority; produce evidence that DSM categories are nosological artifacts rather than disease discoveries.
 *   - Institutional behavior enforcers (schools, workplaces, justice systems): institutional beneficiaries; use DSM categories to justify behavioral conformity enforcement as medical necessity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.78).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.72).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Biomedical Framing: Categories as Discoverable Disease Entities").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy/social_construction").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, '58bbe76e-38cf-446d-9112-d69d4ad86fb6').
narrative_ontology:cs_kernel_codification('58bbe76e-38cf-446d-9112-d69d4ad86fb6', fixed_text).
narrative_ontology:cs_authority_grounding('58bbe76e-38cf-446d-9112-d69d4ad86fb6', extraction).
narrative_ontology:cs_interpretation_layer_present('58bbe76e-38cf-446d-9112-d69d4ad86fb6').
narrative_ontology:cs_reading_relation('58bbe76e-38cf-446d-9112-d69d4ad86fb6', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_reading_relation('58bbe76e-38cf-446d-9112-d69d4ad86fb6', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('58bbe76e-38cf-446d-9112-d69d4ad86fb6', foundational, dsm_categories_map_neurobiological_entities).
narrative_ontology:cs_axiom_status(dsm_categories_map_neurobiological_entities, holdable).
narrative_ontology:cs_axiom_grounding('58bbe76e-38cf-446d-9112-d69d4ad86fb6', dsm_categories_map_neurobiological_entities, empirically_contingent).
narrative_ontology:cs_axiom('58bbe76e-38cf-446d-9112-d69d4ad86fb6', secondary, psychiatric_diagnosis_enables_evidence_based_treatment).
narrative_ontology:cs_axiom_status(psychiatric_diagnosis_enables_evidence_based_treatment, holdable).
narrative_ontology:cs_axiom_grounding('58bbe76e-38cf-446d-9112-d69d4ad86fb6', psychiatric_diagnosis_enables_evidence_based_treatment, instrumental).
narrative_ontology:cs_reference_frame('58bbe76e-38cf-446d-9112-d69d4ad86fb6', objective_neurobiological_disease_discovery).
narrative_ontology:cs_drift_state('58bbe76e-38cf-446d-9112-d69d4ad86fb6', contemporary_post_genomic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('58bbe76e-38cf-446d-9112-d69d4ad86fb6', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, institutional_behavior_enforcers).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, persons_meeting_diagnostic_criteria).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, childhood_neurodevelopmental_variation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, persons_meeting_diagnostic_criteria).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, insurance_and_legal_systems).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, mental_illness_is_brain_disease).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, psychiatric_categories_map_neurobiological_reality).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, empirical_psychiatry_as_medical_science).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors, revises, and maintains DSM diagnostic criteria. Controls the frame through which psychiatric phenomena are recognized and treated. Sets research funding priorities aligned with biomedical model. Justifies the taxonomy as mapping objective neurobiological entities discoverable through empirical research. Benefits through institutional authority, research funding capture, and treatment legitimacy.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Develop and market psychotropic medications aligned with DSM diagnostic categories. The biomedical framing legitimizes pharmaceutical intervention as treatment of discovered disease rather than behavioral control or enhancement. Benefit from market expansion as diagnostic thresholds are revised downward and off-label use expands into new populations.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_manufacturers, beneficiary,
    institutional, biographical, arbitrage, global).

% Individuals diagnosed with DSM categories under this reading. Bear costs through mandatory treatment (pharmacological or institutional), loss of legal capacity, occupational discrimination, permanent disease labeling, and side effects from pharmaceutical intervention. Identity becomes fused with disease status ('I am bipolar,' 'I am autistic'). May also benefit from access to treatment, reduced suffering under the medical model, and community recognition. Exit is constrained by identity-lock: once diagnosed, the category becomes integral to self-concept and institutional standing.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, persons_meeting_diagnostic_criteria, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, persons_meeting_diagnostic_criteria, beneficiary).

% Children whose neurological development diverges from institutional norms (attention patterns, sensory processing, social orientation, emotional regulation) are diagnosed with DSM categories. Subjected to pharmaceutical intervention, special segregation in education, behavioral modification regimens, and permanent medical records affecting future institutional access. They cannot consent to this framing and cannot exit: the diagnosis is assigned by gatekeepers before language or legal agency.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, childhood_neurodevelopmental_variation, payer,
    powerless, generational, trapped, global).

% Communities claiming autism, ADHD, and other DSM categories as neurological difference rather than disease. Are structurally excluded from diagnostic authority: their interpretation (neurodiversity as variation, not pathology) is not represented in DSM revision committees. Would contest the disease framing, demand alternative support models, and restrict pharmaceutical intervention to voluntary contexts.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, neurodiversity_advocates, excluded,
    moderate, biographical, constrained, global).

% Researchers and clinicians arguing that DSM categories are nosological artifacts reverse-engineered from available drugs rather than discoveries of natural disease entities. Are excluded from mainstream psychiatric authority: their work is marginalized, funding is restricted, and institutional prestige flows to biomedical model defenders. Would argue for depathologization, context-sensitivity, and decoupling of diagnosis from mandatory treatment.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, critical_psychiatry_researchers, excluded,
    moderate, generational, constrained, global).

% Schools, workplaces, criminal justice systems, military, and other institutions requiring behavioral conformity. Use DSM categories to legitimize exclusion, mandatory treatment, or punishment of nonconforming behavior. The biomedical framing converts institutional preferences (sit still, maintain eye contact, suppress emotional expression) into objective disease facts. Benefit through behavioral standardization without bearing the cost of justifying coercion as merely institutional preference.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, institutional_behavior_enforcers, beneficiary,
    institutional, generational, arbitrage, global).

% Insurance systems use DSM diagnoses to determine coverage, claims adjudication, and actuarial risk. Legal systems use DSM categories to determine competency, criminal responsibility, and civil commitment. Both benefit from the objective disease framing because it grounds coercive institutional decisions in medical fact rather than in policy or preference. Costs of diagnosis (permanent records affecting insurability, legal standing) are externalized to diagnosed persons.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, insurance_and_legal_systems, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, insurance_and_legal_systems, beneficiary).

% Empirical scientists investigating whether DSM categories correspond to distinct neurobiological entities. Produce mixed evidence: some categories show coherent neurobiological substrates, others show heterogeneous mechanisms, and boundaries between categories show substantial bleed. Sit outside the institutional decision-making structure but their research informs (or fails to inform) revision of diagnostic criteria.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, clinical_researchers, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__biomedical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables standardized communication among clinicians: shared diagnostic language allows treatment coordination across settings, research collaboration, and consistent application of evidence-based interventions. Provides patients with medical legitimacy for accessing care and accommodations within institutional systems.
% TRANSFER_FUNCTION: Moves pharmaceutical revenue, clinical authority, and institutional power to psychiatric establishment, pharmaceutical manufacturers, and behavior-enforcing institutions. Moves legal capacity, occupational opportunity, and bodily autonomy FROM persons diagnosed under this frame TO institutional gatekeepers and treatment providers.
% ABSENT_VOICES: Neurodiversity-identified persons and critical psychiatry researchers are structurally excluded from diagnostic authority. Their testimony would argue that DSM categories pathologize human variation and that diagnostic authority should rest with affected communities rather than with professional psychiatry and pharmaceutical interests.
% DISAPPEARANCE_RATIONALE: If the biomedical framing and its enforcement vanished, psychiatric diagnosis would cease to be grounds for involuntary treatment, pharmaceutical intervention, or loss of institutional access. Persons now labeled with DSM categories would retain their neurological characteristics but lose the disease identity and the coercive apparatus attached to it. Pharmaceutical markets would contract. Institutions would need alternative justifications for behavioral conformity requirements.
% FOUNDING_PROBLEM: Early psychiatry lacked a coherent nosology: practitioners used conflicting category systems, diagnosis was subjective and culture-dependent, and treatment efficacy was uneven and unexplained. The biomedical founding problem is: can psychiatric phenomena be understood as discoverable neurobiological entities analogous to somatic diseases, enabling objective diagnosis and mechanistic treatment?
% FOUNDING_PROBLEM_CORROBORATION: Psychiatric establishment and neuroscience researchers attest the founding problem is partially live: some DSM categories show distinct neurobiological substrates (schizophrenia spectrum, bipolar disorder show some coherence in imaging and genetics), but others show heterogeneous mechanisms and substantial category bleed. Independent critical psychiatry researchers, neurodiversity advocates, and meta-analyses of DSM validity studies (published in mainstream journals) attest the founding problem is substantially unresolved: no DSM category has a discovered biological marker; diagnostic boundaries are administrative rather than natural; and the biomedical model increasingly appears to be ideology masquerading as science.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__biomedical_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__biomedical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__biomedical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the biomedical framing enables coercive institutional decisions (involuntary treatment, loss of legal standing, pharmaceutical intervention) that would require different justification under alternative framings. The frame's authority depends on the claim that DSM categories are objective discoveries, not administrative constructs. Suppression is also high (0.72) because the reading's persistence requires active defense against alternative framings (neurodiversity, critical psychiatry): counter-evidence must be marginalized, dissenting researchers excluded from authority, and competing interpretations kept out of diagnostic committees. Theater ratio (0.41) is moderately elevated because a growing share of activity defends the biomedical framing itself (conference panels on 'biological basis of mental illness,' funding for neuroimaging studies of diagnostic validity, institutional protocols enforcing diagnostic categories) rather than performing the coordination function. Accessibility collapse (0.68) is moderate-high: once a person is diagnosed under this reading, alternatives (neurodiversity framing, social model of disability, medication refusal) are accessible but carry institutional penalties. Resistance (0.54) is moderate: neurodiversity advocates, critical psychiatry researchers, and some diagnosed persons actively contest the reading, but their institutional power is constrained and their voice is excluded from diagnostic authority.
 *
 * PERSPECTIVAL GAP:
 *   The psychiatric establishment and pharmaceutical manufacturers experience this constraint as genuine coordination (standardized language enables treatment, research, and access to care) and legitimately needed enforcement (defending biomedical discovery against ideological attacks). From the seats of persons diagnosed and excluded researchers, the constraint operates as asymmetric extraction: the biomedical framing serves institutional interests (pharmaceutical profits, behavioral control, research authority) by converting institutional preferences into disease facts. The payer seats (diagnosed persons, neurodiversity advocates) should compute the constraint as substantially more extractive than the beneficiary seats do. The engine's per-seat classification should diverge markedly because directionality differs: the psychiatric establishment and pharmaceutical manufacturers sit near d=0.0 (beneficiaries), while diagnosed persons sit near d=1.0 (targets). The excluded seats (neurodiversity advocates, critical psychiatry) sit in an asymmetric institutional position: they have meaningful power (research publications, media presence, legal advocacy) but are structurally excluded from diagnostic authority (no seats on DSM revision committees, funding flows to biomedical research).
 *
 * DIRECTIONALITY LOGIC:
 *   Psychiatric establishment: powerful institutional actors who set the diagnostic frame, benefit from its authority, and control the revision process—directionality near 0.0 (beneficiary). Pharmaceutical manufacturers: powerful institutional actors who benefit from diagnostic expansion and pharmaceutical market growth—directionality near 0.0 (beneficiary). Institutional behavior enforcers: institutional actors who benefit from the biomedical framing because it grounds behavioral conformity enforcement in medical necessity rather than in institutional preference—directionality near 0.1–0.2 (modest beneficiary). Persons meeting diagnostic criteria: powerless, identity-locked, bear costs through mandatory treatment and permanent labeling—directionality near 1.0 (full target). Neurodiversity advocates: moderate power, constrained exit (excluded from diagnostic authority but present in public discourse), disagree with the reading's core premise—directionality near 0.8–0.9 (strong target). Critical psychiatry researchers: organized, constrained exit (publishing, but marginalized from mainstream institutional authority), produce evidence that challenges the reading—directionality near 0.7–0.8 (target, but with more exit optionality than diagnosed persons).
 *
 * MANDATROPHY ANALYSIS:
 *   The biomedical reading does NOT exhibit mandatrophy as a piton (atrophied function maintained by theater). The coordination function—standardized diagnostic language enabling clinical communication and evidence-based treatment—remains genuinely active and valued. The extraction function (pharmaceutical market expansion, behavioral control, loss of legal capacity) is the added asymmetric layer, not the only remaining layer. This is therefore a tangled rope (genuine coordination + asymmetric extraction requiring enforcement) rather than a piton. However, the rising theater ratio (0.18→0.41 across the interval) and the measurement pattern suggest that a SECONDARY layer of theatrical activity is growing: defending the biomedical framing itself against challenges. The reading's persistence increasingly depends on active enforcement of the frame's authority (excluding dissenting researchers, marginalizing neurodiversity interpretations, funding biomedical neuroimaging studies that support the frame) rather than on the frame's empirical validity. Over a longer horizon (post-2030), this rising theater ratio could signal the reading approaching piton status: if the coordination function proves empirically weaker than early biomedical psychiatry hoped (DSM categories continue to lack biological markers, pharmaceutical efficacy plateaus), the constraint might persist through institutional inertia and theater while the founding coordination problem is substantially solved by alternative means.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neurobiological_validity_of_dsm_categories,
    'Do DSM diagnostic categories correspond to distinct, empirically discoverable neurobiological entities, or are they administrative constructs that correlate with overlapping neurobiological heterogeneity?',
    'Longitudinal neuroimaging, genomic, and biomarker studies comparing within-category homogeneity to between-category differentiation. Cross-validation of proposed biological markers against independent clinical samples. Meta-analysis of DSM validity literature.',
    'If DSM categories prove to map distinct neurobiological entities (biomedical reading validated), the constraint would be reclassifiable as genuine coordination with minimal excess extraction—the frame would be empirically grounded. If categories prove to correlate with heterogeneous neurobiological mechanisms and substantially overlapping brain states (alternative readings supported), the high extractiveness and rising theater ratio would indicate the biomedical reading is maintained through institutional enforcement rather than empirical validation, shifting the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neurobiological_validity_of_dsm_categories, empirical, 'Whether DSM categories correspond to natural neurobiological kinds or are administrative constructs.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of alternative readings (neurodiversity, critical psychiatry) primarily structural (these groups are excluded from diagnostic committees, funding flows to biomedical research) or internalized (people accept the biomedical framing as truth and self-suppress alternative interpretations)?',
    'Compare activist group composition and framing in jurisdictions with different institutional structures (e.g., countries with stronger neurodiversity representation in psychiatric institutions vs. countries with dominant biomedical institutional control). Survey diagnosed persons'' private interpretations of their diagnoses vs. public statements.',
    'If suppression is primarily structural, it is reversible through institutional change (including dissenting voices, shifting funding). If primarily internalized, the constraint''s persistence would require continuous enforcement even if institutional barriers were removed. Mixed suppression (both structural and internalized) would indicate the biomedical framing has become deeply embedded in self-concept and institutional identity, raising the effective suppression above the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternative readings is structural or internalized in the diagnosed population and professional communities.').

omega_variable(
    core_premise_foreclosure_boundary,
    'Does the biomedical reading''s core premise (DSM categories map objective neurobiological entities) logically foreclose the neurodiversity reading''s core premise (DSM categories pathologize natural human neurological variation), or can both be held simultaneously in different frameworks?',
    'Examine whether a coherent synthesis is possible: e.g., ''some DSM categories map disease (schizophrenia), while others pathologize neurodivergence (autism)'' — a mixed reading. If such syntheses can be endorsed by advocates of both readings, foreclosure does not hold. If advocates of each reading explicitly reject the other as logically incompatible with their framework, foreclosure holds.',
    'If foreclosure holds (biomedical core premise logically contradicts neurodiversity core premise), the relationship is FORECLOSES rather than COEXISTS_WITH. If foreclosure does not hold and mixed readings emerge, the relationship remains COEXISTS_WITH (different parties hold different readings; no single party cannot hold both; synthesis is intellectually possible even if institutionally rejected).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(core_premise_foreclosure_boundary, conceptual, 'Whether the biomedical and neurodiversity readings are logically foreclosed or can coexist.').

omega_variable(
    identity_locked_exit_trajectory,
    'For persons diagnosed under this reading, if the biomedical framing were removed (institutional enforcement ceased, alternative interpretations became legitimate), would the identity-lock persist or dissolve? Do diagnosed persons carry the disease identity into post-frame contexts, or is the identity dependent on institutional reinforcement?',
    'Natural experiments in jurisdictions where psychiatric categories lose institutional status (e.g., legal depathologization of homosexuality, decriminalization of drug use). Longitudinal interviews with diagnosed persons in contexts where alternative framings become dominant.',
    'If identity-lock persists without institutional enforcement, the suppression is substantially internalized and would remain even if structural barriers were removed. If identity-lock dissolves when alternative framings become legitimate and institutional enforcement ceases, suppression is primarily structural. This affects the constraint''s effective suppression: internalized identity-lock raises the effective value above the structural measure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_exit_trajectory, empirical, 'Whether identity-lock in diagnosed persons is dependent on institutional enforcement or would persist after frame removal.').

omega_variable(
    reading_specificity_to_kernel_context,
    'Is the biomedical reading specific to psychiatric diagnosis (the DSM context), or is it a more general epistemological claim about illness categories that instantiates across medicine?',
    'Compare the biomedical reading''s structure to readings of somatic medical categories (hypertension as disease vs. normal variation, fibromyalgia as discovered disease vs. functional syndrome, chronic Lyme disease as novel entity vs. persistent symptom labeling). Examine whether the same axioms and authority structures apply.',
    'If the biomedical reading is a general medical epistemology applied to psychiatry, the constraint might be one instance of a broader constraint family covering medical nosology across domains. If specific to psychiatry, the reading is isolated. The classification might extend to sister constraints in somatic medicine if the reading is general.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_specificity_to_kernel_context, conceptual, 'Scope of the biomedical epistemology: psychiatric-specific or general medical category instantiation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 1980, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement_basis(dsm__tr_t1980, projected).
narrative_ontology:measurement(dsm__tr_t1994, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1994, 0.24).
narrative_ontology:measurement_basis(dsm__tr_t1994, observed).
narrative_ontology:measurement(dsm__tr_t2004, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2004, 0.31).
narrative_ontology:measurement_basis(dsm__tr_t2004, observed).
narrative_ontology:measurement(dsm__tr_t2014, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2014, 0.37).
narrative_ontology:measurement_basis(dsm__tr_t2014, observed).
narrative_ontology:measurement(dsm__tr_t2020, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement_basis(dsm__tr_t2020, observed).
narrative_ontology:measurement(dsm__tr_t2026, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2026, 0.41).
narrative_ontology:measurement_basis(dsm__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement_basis(dsm__be_t1980, projected).
narrative_ontology:measurement(dsm__be_t1994, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1994, 0.52).
narrative_ontology:measurement_basis(dsm__be_t1994, observed).
narrative_ontology:measurement(dsm__be_t2004, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2004, 0.63).
narrative_ontology:measurement_basis(dsm__be_t2004, observed).
narrative_ontology:measurement(dsm__be_t2014, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2014, 0.72).
narrative_ontology:measurement_basis(dsm__be_t2014, observed).
narrative_ontology:measurement(dsm__be_t2020, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement_basis(dsm__be_t2020, observed).
narrative_ontology:measurement(dsm__be_t2026, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2026, 0.78).
narrative_ontology:measurement_basis(dsm__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1980, 0.42).
narrative_ontology:measurement_basis(dsm__su_t1980, projected).
narrative_ontology:measurement(dsm__su_t1994, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1994, 0.54).
narrative_ontology:measurement_basis(dsm__su_t1994, observed).
narrative_ontology:measurement(dsm__su_t2004, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2004, 0.62).
narrative_ontology:measurement_basis(dsm__su_t2004, observed).
narrative_ontology:measurement(dsm__su_t2014, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2014, 0.68).
narrative_ontology:measurement_basis(dsm__su_t2014, observed).
narrative_ontology:measurement(dsm__su_t2020, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement_basis(dsm__su_t2020, observed).
narrative_ontology:measurement(dsm__su_t2026, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement_basis(dsm__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__biomedical_reading, 0.12).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_psychotropic_market_expansion).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, psychiatric_involuntary_commitment_authority).

% DUAL FORMULATION NOTE:
% The DSM taxonomy kernel decomposes into three structurally distinct constraint stories, each representing a different reading of the same stabilized kernel (the DSM diagnostic authority structure). The biomedical reading (this story) claims DSM categories map objective neurobiological disease entities; it benefits the psychiatric establishment and pharmaceutical industry and extracts from diagnosed persons through mandatory treatment and loss of legal capacity. The critical psychiatry reading (sister constraint) claims DSM categories are reverse-engineered from available drugs; it produces higher extractiveness by making the mechanism explicit. The neurodiversity reading (sister constraint) claims DSM categories pathologize natural neurological variation; it reframes diagnosed persons as neurodivergent communities rather than disease patients. All three share the kernel (DSM diagnostic authority) but have radically different ε values, victim/beneficiary structures, and empirical status. They are linked through network.affects_constraints because the biomedical reading's persistence directly shapes the conditions for the critical psychiatry and neurodiversity readings: as biomedical extractiveness rises and theater ratio rises (defense of the frame increases), support for alternative readings grows. Decomposition follows ε-invariance: each reading instantiates a different constraint with a different ε because different observables apply (DSM-as-disease-discovery vs. DSM-as-drug-market vs. DSM-as-pathologizing-variation produce ε ≈ 0.78 vs. ≈ 0.85 vs. ≈ 0.82 respectively).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
