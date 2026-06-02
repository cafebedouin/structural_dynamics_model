% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__biomedical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   constraint_id: dsm_taxonomy_kernel__biomedical_reading
 *   human_readable: DSM Categories as Objective Neurobiological Disease Entities (Biomedical Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/neuroscience
 *
 * SUMMARY:
 *   The DSM (Diagnostic and Statistical Manual of Mental Disorders)
 *   represents the dominant institutional taxonomy for psychiatric diagnosis
 *   globally. The biomedical reading treats DSM categories as objective
 *   discoveries — real neurobiological disease entities that exist
 *   independently and will eventually be validated through empirical
 *   neuroscience. This reading justifies involuntary treatment,
 *   pharmaceutical intervention, and loss of civil capacity for diagnosed
 *   individuals. It enables the psychiatric establishment and pharmaceutical
 *   industry to capture substantial economic and epistemic value. However,
 *   this reading is contested. The neurodiversity reading claims that many
 *   DSM categories (autism, ADHD, bipolar disorder) describe natural
 *   neurological variation, not disease. The critical psychiatry reading
 *   claims that DSM categories are socially constructed mechanisms for
 *   medicalizing social nonconformity and normalizing institutional control.
 *   This constraint story instantiates the biomedical reading as a complete
 *   ε-invariant constraint — the core claim that DSM categories map to
 *   objective neurobiological entities. The measurement trajectory shows
 *   extractiveness, suppression, and theater ratio all increasing over 30
 *   years (1993-2023: DSM-IV to DSM-5 era), reflecting the growing
 *   institutionalization of psychiatric diagnosis, pharmaceutical expansion,
 *   and the increasing incoherence of biomarker research programs relative to
 *   DSM category stability.
 *
 * KEY AGENTS:
 *   - Diagnosed individuals: Primary victims (powerless/trapped) — classified as having objective disease; resistance is pathologized as symptom; exit options are extremely limited (treatment refusal risks coercion; diagnosis persists across institutional contexts)
 *   - Neurodiversity communities: Secondary victims (moderate/constrained) — experience biomedical framing as pathologizing natural variation; institutional barriers prevent adoption of alternative frameworks despite community-level exit options
 *   - Psychiatric establishment: Primary beneficiary (institutional/arbitrage) — captures professional legitimacy, insurance reimbursement, research funding, and institutional authority through DSM framework
 *   - Pharmaceutical industry: Primary beneficiary (institutional/arbitrage) — development pipelines, patent markets, treatment expansion, and drug sales justified by DSM categories
 *   - Reform psychiatry researchers: Mixed actors (organized/constrained) — benefit from DSM as shared research framework; constrained by pressure to produce biomarkers validating DSM categories; face career barriers if research suggests DSM categories lack biological validity
 *   - Insurance/healthcare administration: Degraded institutional actor (institutional/arbitrage) — relies on DSM for administrative coding but system is increasingly incoherent; maintains through inertia despite acknowledged limitations
 *   - Analytical observer: Risks naturalizing contingent arrangement — the mountain perspective that DSM categories ARE objectively real biological entities may be a false summit masking social construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.68).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.72).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, snare).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Categories as Objective Neurobiological Disease Entities (Biomedical Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy/neuroscience").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, '57a85d9a-67d5-4159-a5c5-4191d7d86323').
narrative_ontology:cs_kernel_codification('57a85d9a-67d5-4159-a5c5-4191d7d86323', formalized).
narrative_ontology:cs_authority_grounding('57a85d9a-67d5-4159-a5c5-4191d7d86323', extraction).
narrative_ontology:cs_interpretation_layer_present('57a85d9a-67d5-4159-a5c5-4191d7d86323').
narrative_ontology:cs_reading_relation('57a85d9a-67d5-4159-a5c5-4191d7d86323', neurodiversity_reading, forecloses).
narrative_ontology:cs_reading_relation('57a85d9a-67d5-4159-a5c5-4191d7d86323', critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('57a85d9a-67d5-4159-a5c5-4191d7d86323', foundational, dsm_categories_map_objective_biological_entities).
narrative_ontology:cs_axiom_status(dsm_categories_map_objective_biological_entities, holdable).
narrative_ontology:cs_axiom_grounding('57a85d9a-67d5-4159-a5c5-4191d7d86323', dsm_categories_map_objective_biological_entities, empirically_contingent).
narrative_ontology:cs_axiom('57a85d9a-67d5-4159-a5c5-4191d7d86323', secondary, psychiatric_diagnosis_enables_medical_treatment).
narrative_ontology:cs_axiom_status(psychiatric_diagnosis_enables_medical_treatment, holdable).
narrative_ontology:cs_axiom_grounding('57a85d9a-67d5-4159-a5c5-4191d7d86323', psychiatric_diagnosis_enables_medical_treatment, instrumental).
narrative_ontology:cs_reference_frame('57a85d9a-67d5-4159-a5c5-4191d7d86323', biomedical_taxonomy_validity).
narrative_ontology:cs_drift_state('57a85d9a-67d5-4159-a5c5-4191d7d86323', contemporary_post_dsm5_biomarker_impasse, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('57a85d9a-67d5-4159-a5c5-4191d7d86323', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, institutional_conformity_apparatus).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, diagnostic_threshold_population).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, neurodiversity_community).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, psychiatric_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIAGNOSED INDIVIDUAL (SNARE) — Person meeting DSM threshold experiences the classification as immutable biological fact. Exit options are extremely constrained: challenging the diagnosis risks being labeled as lacking insight or denial; treatment refusal risks involuntary intervention; labeled status persists across institutional contexts (employment, housing, family). Suppression is maximal because the victim is positioned as irrational if they resist — their own cognitive objections to the label are pathologized as symptoms of the condition. Maximum experienced extraction.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__biomedical_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NEURODIVERSITY COMMUNITY (SNARE) — Communities organized around non-standard neurology (autistic self-advocates, ADHD communities, bipolar peer networks) experience DSM biomedical framing as pathologizing their identity. High suppression: medicalization obscures the role of social environment and institutional design in creating 'disorder.' Constrained exit: communities can build alternative frameworks (neurodiversity paradigm) but face institutional barriers when schools, employers, insurers operate under DSM biomedical assumptions. Extraction is real but negotiable — some groups have developed countervailing institutional power.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__biomedical_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PSYCHIATRIC ESTABLISHMENT + PHARMA (ROPE) — The institutional beneficiary. DSM categories enable medical legitimacy, insurance reimbursement, pharmaceutical development pipelines, and professional scope. From this perspective, the constraint appears as pure coordination: establishing shared diagnostic language enables treatment, research collaboration, and resource allocation. Low experienced extraction because this agent captures the value and can exit by simply maintaining the framework. Coordination function is real: DSM enables communication across practitioners and insurance systems.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__biomedical_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM PSYCHIATRY COALITION (TANGLED ROPE) — Researchers pushing for biological validation of DSM categories (RDoC initiative, computational psychiatry, neuroimaging biomarker research) experience the constraint as mixed. Genuine coordination function: shared taxonomic framework enables collaborative research. Embedded extraction: pressure to produce biomarkers that validate DSM categories creates publication bias and threatens to pathologize variation; researchers who fail to find biological bases face career barriers. Constrained exit: can critique DSM but must work within institutionalized frameworks to access funding and journals.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__biomedical_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INSURANCE/HEALTHCARE ADMIN (PITON) — Institutional actors that rely on DSM for billing, coverage determination, and risk stratification experience the framework as increasingly degraded. The constraint persists through administrative inertia: changing coding systems would require massive system redesign, so DSM categories are maintained despite acknowledged limitations. Theater ratio is moderate-high: DSM categories serve administrative functions (billing codes) that are only loosely connected to actual clinical validity. These actors can exit by adopting alternative systems (ICD alternatives, dimensional frameworks) but the switching cost is prohibitive.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__biomedical_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL/BIOMEDICAL NATURALISM (MOUNTAIN) — Purely analytical stance that treats DSM categories as natural kinds (discoverable biological entities independent of social construction). From this perspective, the constraint appears immutable: psychiatric conditions ARE objectively real biological phenomena; the DSM is merely the imperfect mapping onto those real categories. Classification advances with better neuroscience will reveal the true biological boundaries. This perspective risks being a false summit — it naturalizes what may be a contingent institutional arrangement grounded in beneficiary interests.
constraint_indexing:constraint_classification(dsm_taxonomy_kernel__biomedical_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__biomedical_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dsm_taxonomy_kernel__biomedical_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dsm_taxonomy_kernel__biomedical_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, TR),
    TR >= 0.70.

:- end_tests(dsm_taxonomy_kernel__biomedical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The biomedical reading justifies substantial institutional extraction from diagnosed individuals: pharmaceutical consumption, involuntary treatment authority, loss of legal capacity, employment discrimination (legally permitted), insurance discrimination, surveillance, and identity colonization. The extraction is particularly severe because resistance is pathologized — the diagnosed person's objections to treatment are coded as denial or lack of insight, eliminating a standard exit option (vocal resistance). The 0.68 value reflects that extractiveness is not absolute (some diagnosed individuals decline treatment, some communities build countervailing frameworks) but is substantial and systematized. Suppression (0.72): Very high. Multiple suppression mechanisms: (1) Institutional — legal coercion (involuntary commitment, forced medication), insurance systems requiring DSM diagnosis for coverage; (2) Epistemic — biomedical framing is institutionalized in research, education, clinical training; alternative framings are marginalized; (3) Cognitive — diagnosed individuals are trained to interpret their own resistance as symptoms; the diagnosis becomes part of identity, making exit unthinkable. The 0.72 value reflects that suppression is severe but not absolute — some jurisdictions have reduced involuntary commitment, some individuals reject diagnosis, some research communities are exploring alternatives. Theater ratio (0.58): Moderate-high. DSM categories appear as objective scientific discoveries (theater), but empirical support is mixed: biomarker research has failed to find biological bases for most DSM categories; category boundaries are historically contingent (diagnostic criteria change between DSM editions); comorbidity rates suggest categories are overlapping constructs, not distinct entities. The theater persists because the biomedical framing is institutionally dominant, not because the categories have strong empirical validation. Measurement trajectory: All three metrics increase over 30 years, reflecting accumulating extractiveness and suppression as psychiatric diagnosis has expanded institutionally and as pharmaceutical treatment has become normalized.
 *
 * PERSPECTIVAL GAP:
 *   The biomedical reading produces radically different classifications from the same structural data when compared to sibling readings. From the biomedical perspective: DSM categories are Rope (coordination function) or Tangled Rope (coordination + extraction for the reform psychiatry coalition). From the neurodiversity reading: DSM categories are Snare (pure extraction pathologizing natural variation). From the critical psychiatry reading: DSM categories are Snare (institutionalized power mechanism). The gap reveals that the readings disagree on the fundamental nature of the constraint — whether it is a genuine coordination mechanism for identifying real disease (biomedical) or a social construction pathologizing difference (neurodiversity, critical psychiatry). The three readings cannot be reconciled within a single framework; they are alternative epistemic positions on what the DSM IS.
 *
 * DIRECTIONALITY LOGIC:
 *   The biomedical reading derives directionality from the assumption that DSM categories are objective disease entities. From this premise: diagnosed individuals are victims bearing costs of treatment/stigma (high d → high f(d) → high chi); the psychiatric establishment and pharma are beneficiaries capturing value from diagnostic framework (low d → negative f(d) → negative chi from their perspective). However, this directionality derivation is the crux of disagreement with sibling readings. The neurodiversity reading would derive different d values because it does not accept that DSM victims are legitimately identified as diseased — the 'disease' is the diagnostic framework itself, making the psychiatric establishment the true extractor. The critical psychiatry reading would argue d values are entirely backwards if the DSM's primary function is social control, not disease mapping. The biomedical reading's directionality logic is internally consistent but depends entirely on accepting the premise that DSM categories are objective entities — the premise the other readings deny.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not resolve mandatrophy through data. Instead, it instantiates the oracle-gap paradox (Theorem 4): the biomedical reading's core claim (DSM categories are objective biological entities) cannot be evaluated using instruments available from within the biomedical framework. The question 'Are DSM categories real biological entities or socially constructed?' requires cross-position analysis — comparing the biomedical reading's model of reality to alternative readings' models. The biomedical analyst, working within biomedical epistemology, can produce biomarker research, but cannot determine whether the biomarkers are discovering pre-existing entities or being constructed through the research process itself. This is the irreducible uncertainty encoded in the omega variables. Mandatrophy is resolved by acknowledging that the three readings are incommensurable — each internally coherent, each supported by evidence within its own frame, but no empirical data can arbitrate between them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_validity_threshold,
    'What constitutes sufficient biological evidence to claim a DSM category maps to an objective disease entity?',
    'Historical analysis of biomarker research programs; tracking of categories that gained or lost neurobiological evidence; meta-analysis of failed replication in psychiatric neuroscience',
    'If threshold is very high (multiple converging biomarkers with clear mechanism): few DSM categories qualify as objective entities. If threshold is permissive (any neurobiological correlate): nearly all behavior has some neural correlate, making the claim empirically empty. This determines whether the biomedical reading is scientifically defensible or whether it''s naturalizing social construction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_validity_threshold, empirical, 'What level of biological evidence validates DSM categories as natural kinds').

omega_variable(
    independence_of_diagnostic_framework,
    'Do the biological patterns attributed to DSM categories exist independently of the diagnostic framework, or are they constructed through the act of diagnosis and treatment?',
    'Cross-cultural psychiatric epidemiology; longitudinal studies of untreated populations; analysis of diagnostic drift over DSM editions and corresponding biological literature; examination of whether biomarker research follows from theory or from DSM category membership',
    'If biological patterns precede and constrain the DSM framework: the biomedical reading is valid — DSM maps to real biological entities. If biological patterns are shaped by diagnostic and treatment practices: the biomedical reading is backwards — diagnosis constructs the biological entity, not vice versa. This is the core oracle-gap uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independence_of_diagnostic_framework, empirical, 'Whether biological patterns are independent of or constructed by the diagnostic framework').

omega_variable(
    kernel_reading_committer_distinction,
    'This constraint is the biomedical reading of a contested kernel. What structural differences distinguish this reading from the neurodiversity_reading and critical_psychiatry_reading?',
    'Comparison of axioms, reference frames, and drift states across the three readings. The biomedical reading assumes mental health categories are objective biological entities; the neurodiversity reading assumes neurological variation is natural human diversity; the critical psychiatry reading assumes psychiatric categories are socially constructed power mechanisms. No empirical data resolves this — the readings are incommensurable frameworks.',
    'Each reading produces a different constraint type (snare, rope, tangled_rope, etc.) from identical base facts. The constraint family demonstrates that indexical classification works at the kernel level: the same DSM system is seen as one type from the biomedical reading and a different type from the neurodiversity reading because the readings disagree on what the DSM IS (natural mapping vs. social construction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_distinction, conceptual, 'Kernel reading incommensurability: the three readings of DSM taxonomy kernel cannot be adjudicated by empirical data alone').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) structural (institutional barriers, legal coercion, pharmaceutical dependence) or internalized (the diagnosed person accepts the biomedical framing and suppresses their own resistance)?',
    'Post-exit suppression persistence; comparison of suppression trajectories for people who maintain biomedical identity vs. those who adopt neurodiversity framing; analysis of whether resistance to diagnosis is pathologized (coded as lack of insight) or respected',
    'If suppression is primarily internalized: the constraint''s actual suppressive force persists even if external barriers are removed (the person has been trained to self-pathologize). This would argue for higher effective suppression than the structural measure alone indicates. If primarily structural: removal of institutional enforcement would reduce suppression significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Internalization vs. structural suppression in DSM biomedical framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm_biomed_tr_t0, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(dsm_biomed_tr_t15, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(dsm_biomed_tr_t30, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(dsm_biomed_be_t0, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dsm_biomed_be_t15, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(dsm_biomed_be_t30, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dsm_biomed_su_t0, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dsm_biomed_su_t15, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(dsm_biomed_su_t30, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__biomedical_reading, 0.18).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, psychiatric_pharmacological_expansion).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, involuntary_treatment_authority).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, neurodiversity_recognition_constraint).

% DUAL FORMULATION NOTE:
% The DSM taxonomy is a contested kernel with three structurally distinct readings. This file instantiates the biomedical reading (DSM = objective disease discovery). The neurodiversity_reading and critical_psychiatry_reading are separate constraint stories with different epsilon values, beneficiary/victim structures, and classifications. All three readings link via network.affects_constraints to show the family relationship. The epsilon values differ because the readings make incommensurable empirical claims about what the DSM is — the biomedical reading assumes valid mapping (lower extractiveness from within that frame), while the critical reading treats the DSM itself as the extraction mechanism (higher extractiveness).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
