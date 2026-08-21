% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__biomedical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: DSM Categories as Objective Neurobiological Disease Entities (Biomedical Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This constraint represents the 'biomedical reading' of the DSM taxonomy
 *   kernel, asserting that DSM categories map to objective neurobiological
 *   disease entities discoverable through empirical research. This reading
 *   underpins the dominant medical model of mental illness, enabling
 *   widespread pharmaceutical intervention and, in some cases, involuntary
 *   treatment and loss of legal capacity. The high extractiveness and
 *   suppression reflect the power dynamics inherent in this framing, where
 *   the coordination function of standardized diagnosis serves as a cover for
 *   significant transfers of authority and resources.
 *
 * KEY AGENTS:
 *   - psychiatric_establishment: Primary agenda_setter (institutional/arbitrage) — defines and promulgates the DSM.
 *   - pharmaceutical_industry: Primary beneficiary (institutional/arbitrage) — profits from medicalization.
 *   - diagnosed_individuals: Primary target (powerless/trapped) — subjected to diagnoses and interventions.
 *   - neurodiversity_advocates: Payer/Resistor (organized/constrained) — challenge the pathologization of variation.
 *   - critical_psychiatrists: Payer/Observer (analytical/mobile) — critique the scientific validity and social consequences.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.85).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.9).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, snare).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Categories as Objective Neurobiological Disease Entities (Biomedical Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, '874330ac-bc24-43b4-a06f-4fc0d3d6c11f').
narrative_ontology:cs_kernel_codification('874330ac-bc24-43b4-a06f-4fc0d3d6c11f', formalized).
narrative_ontology:cs_authority_grounding('874330ac-bc24-43b4-a06f-4fc0d3d6c11f', expertise).
narrative_ontology:cs_interpretation_layer_present('874330ac-bc24-43b4-a06f-4fc0d3d6c11f').
narrative_ontology:cs_reading_relation('874330ac-bc24-43b4-a06f-4fc0d3d6c11f', dsm_taxonomy_kernel__neurodiversity_reading, forecloses).
narrative_ontology:cs_reading_relation('874330ac-bc24-43b4-a06f-4fc0d3d6c11f', dsm_taxonomy_kernel__critical_psychiatry_reading, forecloses).
narrative_ontology:cs_axiom('874330ac-bc24-43b4-a06f-4fc0d3d6c11f', foundational, mental_illness_is_brain_disease).
narrative_ontology:cs_axiom_status(mental_illness_is_brain_disease, holdable).
narrative_ontology:cs_axiom_grounding('874330ac-bc24-43b4-a06f-4fc0d3d6c11f', mental_illness_is_brain_disease, empirically_contingent).
narrative_ontology:cs_axiom('874330ac-bc24-43b4-a06f-4fc0d3d6c11f', foundational, dsm_categories_reflect_biological_reality).
narrative_ontology:cs_axiom_status(dsm_categories_reflect_biological_reality, holdable).
narrative_ontology:cs_axiom_grounding('874330ac-bc24-43b4-a06f-4fc0d3d6c11f', dsm_categories_reflect_biological_reality, empirically_contingent).
narrative_ontology:cs_reference_frame('874330ac-bc24-43b4-a06f-4fc0d3d6c11f', objective_disease_model).
narrative_ontology:cs_drift_state('874330ac-bc24-43b4-a06f-4fc0d3d6c11f', contemporary_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('874330ac-bc24-43b4-a06f-4fc0d3d6c11f', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_conformity).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, insurance_companies).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, diagnosed_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, neurodiversity_advocates).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, critical_psychiatrists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and promulgates the DSM, asserting its categories represent objective disease entities. Benefits from the authority and funding derived from this medical model, guiding research and clinical practice.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefits directly from the medicalization of distress, as DSM diagnoses create markets for psychotropic medications. Funds research and advocacy that reinforces the biomedical model.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Schools, workplaces, and legal systems use DSM diagnoses to manage non-conforming behaviors, justify interventions, or allocate resources. Benefits from a standardized framework for categorizing and addressing 'deviance'.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_conformity, beneficiary,
    organized, biographical, constrained, national).

% Require DSM diagnoses for reimbursement of mental health treatments, thereby reinforcing the diagnostic framework and benefiting from its standardization, even while negotiating costs.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, insurance_companies, beneficiary,
    institutional, biographical, constrained, national).

% Relies on DSM diagnoses for decisions regarding involuntary commitment, criminal responsibility, disability claims, and parental rights, thereby enforcing the biomedical model's societal impact.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, legal_system, agenda_setter,
    institutional, civilizational, constrained, national).

% Are subjected to diagnoses that can lead to involuntary treatment, loss of legal capacity, social stigma, and lifelong medication. Their experiences are framed through a medical lens, often suppressing alternative understandings of their distress.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, diagnosed_individuals, payer,
    powerless, immediate, trapped, local).

% Challenge the pathologization of natural human neurological variation, arguing that the biomedical model imposes a deficit-based framework. Bear the cost of fighting for recognition and alternative support models against a dominant medical paradigm.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, neurodiversity_advocates, payer,
    organized, biographical, constrained, global).

% Critique the scientific validity and social consequences of the DSM's biomedical model, often highlighting its role in pharmaceutical market creation. Face professional marginalization for challenging the dominant paradigm.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, critical_psychiatrists, payer,
    analytical, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__biomedical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized nomenclature and diagnostic criteria for mental disorders, enabling consistent communication among clinicians, guiding research, and facilitating treatment development within a medical framework.
% TRANSFER_FUNCTION: Transfers authority over individuals' mental states and behaviors to the medical system, enabling pharmaceutical and therapeutic interventions, and transferring significant financial resources to the psychiatric and pharmaceutical industries.
% ABSENT_VOICES: Individuals who reject the disease model of distress, those who have experienced harm from psychiatric interventions, and indigenous/non-Western perspectives on mental well-being are largely excluded from shaping the DSM's core assumptions.
% DISAPPEARANCE_RATIONALE: If the DSM's biomedical framing vanished overnight, the entire system of psychiatric diagnosis, treatment, research funding, and legal interventions based on these categories would collapse. This would force a radical reorganization of how society understands and responds to mental distress, leading to a profound shift in medical, legal, and social practices.
% FOUNDING_PROBLEM: The original problem was a lack of standardized diagnostic criteria for mental illness, leading to inconsistent diagnoses, unreliable research, and difficulty in developing effective treatments and collecting epidemiological data.
% FOUNDING_PROBLEM_CORROBORATION: The psychiatric establishment and pharmaceutical industry assert the problem of diagnostic reliability and treatment efficacy is still live. Neurodiversity advocates and critical psychiatrists argue the founding problem has been substantially superseded by the problem of medicalizing normal human experience and creating drug markets; independent sociological and historical analyses support this shifted-function reading.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__biomedical_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) stems from the significant financial gains for the pharmaceutical industry and the transfer of autonomy from individuals to the medical system. Suppression (0.90) is severe due to the potential for involuntary treatment, legal consequences, and the marginalization of alternative perspectives. The theater ratio (0.40) indicates that while genuine scientific research and clinical care occur, a substantial portion of activity is dedicated to maintaining the legitimacy of the biomedical model against empirical challenges and social critiques. Accessibility collapse is high (0.80) because once a diagnosis is applied, it often becomes a dominant lens through which an individual's life is understood, limiting perceived alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the psychiatric establishment, this constraint is a necessary scientific framework for understanding and treating disease. From the perspective of diagnosed individuals and critical movements, it is a powerful, often coercive, system that medicalizes human experience for institutional and commercial gain. The engine's classification will highlight this divergence, showing a Snare from the target seats versus a claimed Rope/Scaffold from the agenda-setter's framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The psychiatric establishment and pharmaceutical industry are clear beneficiaries, controlling the narrative and profiting from the system. Institutions requiring conformity and insurance companies also benefit from the standardization and control it offers. Diagnosed individuals are the primary targets, bearing the direct costs of treatment, stigma, and potential loss of autonomy. Neurodiversity advocates and critical psychiatrists are also targets, as their efforts to challenge the dominant paradigm are suppressed or marginalized.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Snare, despite the claimed coordination function, prevents mislabeling by highlighting the high extraction and suppression. The 'contested' status of the founding problem, coupled with rising extractiveness and suppression over time, suggests that the constraint's original mandate (standardized diagnosis for research/treatment) has been substantially co-opted or superseded by its extractive functions, indicating a form of mandatrophy where the original problem is either solved or less pressing than the system's self-perpetuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_status_of_biomarkers,
    'To what extent do DSM categories correlate with objective, discoverable neurobiological markers, and how robust is the empirical evidence for these correlations?',
    'Longitudinal, large-scale empirical research identifying specific, reliable biomarkers for DSM-defined disorders, or a consensus among independent scientific bodies on the absence of such markers.',
    'Strong empirical evidence would bolster the biomedical reading''s legitimacy, potentially reducing perceived extractiveness by validating the ''disease'' claim. Lack of evidence would weaken its scientific grounding, increasing perceived extraction and suppression as the medical model would rely more heavily on social enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_status_of_biomarkers, empirical, 'The scientific validity of the biomedical model''s core claim.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by diagnosed individuals primarily structural (e.g., legal barriers, involuntary treatment) or internalized (e.g., self-stigma, identity fusion with diagnosis)?',
    'Post-diagnosis trajectory analysis: if suppression persists or intensifies after structural barriers are removed, it suggests a significant internalized component. Qualitative studies on lived experience of diagnosis.',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than the structural measures suggest, as individuals carry the suppression with them even in less coercive environments. This would make exit options like ''mobile'' or ''constrained'' more akin to ''identity_locked'' for many.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for diagnosed individuals.').

omega_variable(
    framing_of_distress_as_disease,
    'Is the framing of mental distress as ''disease'' an objective scientific discovery, or a conceptual choice with social and ethical implications?',
    'Philosophical and sociological analysis of the history of psychiatry, and cross-cultural comparisons of mental health frameworks. This is a conceptual debate, not purely empirical.',
    'If primarily a conceptual choice, the ''naturalness'' of the constraint is undermined, shifting its classification further towards a constructed Snare. If an objective discovery, it would lend more weight to the ''mountain'' aspect of the biomedical claim, though its extractive consequences would remain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_distress_as_disease, conceptual, 'Conceptual vs. objective nature of disease framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 1980, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(dsm__tr_t1990, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(dsm__tr_t2010, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(dsm__tr_t2020, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(dsm__tr_t2030, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2030, 0.42).
narrative_ontology:measurement_basis(dsm__tr_t2030, projected).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(dsm__be_t1990, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(dsm__be_t2010, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(dsm__be_t2020, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2020, 0.85).
narrative_ontology:measurement(dsm__be_t2030, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2030, 0.87).
narrative_ontology:measurement_basis(dsm__be_t2030, projected).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(dsm__su_t1990, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1990, 0.78).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(dsm__su_t2010, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(dsm__su_t2020, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2020, 0.9).
narrative_ontology:measurement(dsm__su_t2030, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2030, 0.92).
narrative_ontology:measurement_basis(dsm__su_t2030, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
