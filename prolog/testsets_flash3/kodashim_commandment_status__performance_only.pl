% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__performance_only, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: kodashim_commandment_status__performance_only
 *   human_readable: Kodashim Commandment Status: Performance Only (Husk)
 *   domain: religious_studies/halakhic_theory/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the reading that sacrifice laws (Kodashim) are
 *   strictly contingent on the existence of the Temple and its altar. Without
 *   these, the commandment to perform sacrifices is suspended, rendering
 *   continued scholarly emphasis on their *performance* as a primary
 *   religious obligation largely performative or inertial. This reading views
 *   the current state as a 'husk' – the form persists, but the functional
 *   core is absent. This is one reading of the 'kodashim_commandment_status'
 *   kernel, distinct from 'messianic_deferral' (which sees study as
 *   preparation for future performance) and 'study_as_performance' (which
 *   sees study as fulfilling the commandment itself).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.65).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.7).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.65).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, piton).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Kodashim Commandment Status: Performance Only (Husk)").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious_studies/halakhic_theory/commitment_system_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, '0469c23b-e77a-4550-9810-be9a8c603810').
narrative_ontology:cs_kernel_codification('0469c23b-e77a-4550-9810-be9a8c603810', fixed_text).
narrative_ontology:cs_authority_grounding('0469c23b-e77a-4550-9810-be9a8c603810', lineage).
narrative_ontology:cs_interpretation_layer_present('0469c23b-e77a-4550-9810-be9a8c603810').
narrative_ontology:cs_reading_relation('0469c23b-e77a-4550-9810-be9a8c603810', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('0469c23b-e77a-4550-9810-be9a8c603810', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('0469c23b-e77a-4550-9810-be9a8c603810', foundational, commandment_contingent_on_physical_altar).
narrative_ontology:cs_axiom_status(commandment_contingent_on_physical_altar, holdable).
narrative_ontology:cs_axiom_grounding('0469c23b-e77a-4550-9810-be9a8c603810', commandment_contingent_on_physical_altar, conventional).
narrative_ontology:cs_reference_frame('0469c23b-e77a-4550-9810-be9a8c603810', temple_era_direct_performance).
narrative_ontology:cs_drift_state('0469c23b-e77a-4550-9810-be9a8c603810', post_second_temple_destruction, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('0469c23b-e77a-4550-9810-be9a8c603810', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, yeshiva_students).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, community_resources).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, halakhic_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, lay_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invest significant time and intellectual effort into studying sacrifice laws, believing it to be a core religious obligation, despite the practical impossibility of performance. Their identity is deeply tied to this scholarly pursuit.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, yeshiva_students, payer,
    powerless, biographical, identity_locked, local).

% Administer and perpetuate the curriculum and interpretive tradition that prioritizes the study of Kodashim. Their professional identity and institutional standing are often tied to maintaining the relevance of these texts, even if only theoretically.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, halakhic_scholars, agenda_setter,
    institutional, generational, constrained, global).

% Financial and intellectual resources of the community are directed towards maintaining institutions and curricula focused on the study of sacrifice laws, which could otherwise be allocated to more practically relevant areas of Jewish law or social welfare.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, community_resources, payer,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_non_agent(kodashim_commandment_status__performance_only, community_resources).

% Benefit from the perceived continuity of tradition and the scholarly rigor of their religious leaders, even if they do not directly engage with the study of Kodashim. They bear indirect costs through communal resource allocation.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, lay_adherents, beneficiary,
    powerless, biographical, constrained, local).

% Advocate for a re-evaluation of the relevance of sacrifice laws in contemporary Judaism, often viewing their continued emphasis as anachronistic. They are largely outside the interpretive framework that sustains the constraint.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, reform_movements, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a continuous scholarly tradition and intellectual engagement with a foundational, albeit currently non-performable, area of Jewish law, ensuring its preservation for potential future restoration.
% TRANSFER_FUNCTION: Transfers significant intellectual and financial resources from students and communities to the maintenance of a scholarly apparatus focused on a non-performable set of commandments, in exchange for perceived religious continuity and scholarly depth.
% ABSENT_VOICES: Those who advocate for a re-prioritization of Jewish legal study towards currently performable commandments or social justice issues are largely excluded from the discourse that sustains the emphasis on Kodashim. They would argue for a redirection of resources and intellectual effort.
% DISAPPEARANCE_RATIONALE: If the emphasis on studying sacrifice laws as a primary religious obligation vanished, the structure of yeshiva curricula, scholarly careers, and communal funding priorities would significantly shift. Intellectual and financial resources would be reallocated to other areas of Jewish law or communal needs, leading to a substantial reorganization of religious educational institutions.
% FOUNDING_PROBLEM: The destruction of the Second Temple left a void in Jewish religious practice, as the central act of worship (sacrifices) became impossible. The problem was how to maintain the integrity of the Torah's commandments and the continuity of Jewish identity without the Temple.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem of Temple destruction is historically undeniable. However, this reading asserts that the commandment itself is suspended without the Temple, making the problem of 'how to perform' obsolete. Critics (e.g., reform movements, some modern orthodox scholars) corroborate that the problem of *performance* is dead, while the problem of *study* persists due to institutional inertia and alternative interpretations.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_commandment_status__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__performance_only, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because significant intellectual and communal resources are invested in a practice that, under this reading, has no direct performative outlet. Suppression (0.70) is high due to the strong social and identity-based pressures within traditional Jewish communities to engage with these texts, making it difficult for individuals or institutions to re-prioritize. Theater ratio (0.80) is very high, reflecting that the primary activity (study of sacrifice laws) is largely symbolic or preparatory, rather than directly functional in the absence of the Temple. The founding problem is 'dead' (the Temple is destroyed), but the activity persists, indicating a piton-like dynamic.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the halakhic scholars (agenda-setters), maintaining the study of Kodashim ensures the preservation of Torah and tradition, a vital coordination function. From the perspective of the students and community (payers), it is a demanding, identity-locked obligation with high opportunity costs. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Yeshiva students and community resources are the primary payers, investing heavily in a non-performable domain. Halakhic scholars act as agenda-setters, perpetuating the curriculum and interpretive tradition. Lay adherents are diffuse beneficiaries of perceived continuity, but also indirect payers. Reform movements are excluded, as their perspective challenges the very premise of this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_obsolescence_vs_continuity,
    'Is the continued emphasis on Kodashim study a genuine act of religious continuity and preservation, or an obsolete mandate sustained by institutional inertia and identity lock-in?',
    'Analysis of resource allocation shifts in communities that adopt alternative readings, and longitudinal studies of student satisfaction/retention in curricula that de-emphasize Kodashim.',
    'If primarily obsolete, the constraint is a stronger piton, with higher effective extraction from students and communities. If a genuine continuity mechanism, the coordination function is stronger, reducing effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_obsolescence_vs_continuity, conceptual, 'Ambiguity between genuine continuity and obsolete mandate.').

omega_variable(
    identity_lock_vs_free_choice,
    'To what extent is the engagement with Kodashim study a free choice of religious devotion, versus an identity-locked obligation driven by social and communal pressures?',
    'Sociological studies on exit options and social consequences for individuals who choose to de-emphasize Kodashim study within traditional communities.',
    'If primarily identity-locked, the suppression metric for students is higher, amplifying their effective extraction. If a free choice, suppression is lower, and the constraint is less extractive for this seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_free_choice, empirical, 'Structural vs. internalized suppression mechanism for students.').

omega_variable(
    resource_diversion_impact,
    'What is the opportunity cost of resources (time, money, intellectual capital) allocated to Kodashim study, if those resources were redirected to other areas of Jewish law or communal needs?',
    'Economic analysis comparing resource allocation in communities with different approaches to Kodashim study, and qualitative studies on unmet communal needs.',
    'A high opportunity cost strengthens the ''victim'' status of community resources and students, increasing the overall extractiveness of the constraint under this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_diversion_impact, empirical, 'Quantifying the opportunity cost of resource allocation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__performance_only, theater_ratio, 0, 0.6).
narrative_ontology:measurement(koda_tr_t20, kodashim_commandment_status__performance_only, theater_ratio, 20, 0.68).
narrative_ontology:measurement(koda_tr_t40, kodashim_commandment_status__performance_only, theater_ratio, 40, 0.75).
narrative_ontology:measurement(koda_tr_t60, kodashim_commandment_status__performance_only, theater_ratio, 60, 0.78).
narrative_ontology:measurement(koda_tr_t80, kodashim_commandment_status__performance_only, theater_ratio, 80, 0.79).
narrative_ontology:measurement(koda_tr_t100, kodashim_commandment_status__performance_only, theater_ratio, 100, 0.8).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__performance_only, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(koda_be_t20, kodashim_commandment_status__performance_only, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(koda_be_t40, kodashim_commandment_status__performance_only, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(koda_be_t60, kodashim_commandment_status__performance_only, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(koda_be_t80, kodashim_commandment_status__performance_only, base_extractiveness, 80, 0.64).
narrative_ontology:measurement(koda_be_t100, kodashim_commandment_status__performance_only, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__performance_only, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(koda_su_t20, kodashim_commandment_status__performance_only, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(koda_su_t40, kodashim_commandment_status__performance_only, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(koda_su_t60, kodashim_commandment_status__performance_only, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(koda_su_t80, kodashim_commandment_status__performance_only, suppression_requirement, 80, 0.69).
narrative_ontology:measurement(koda_su_t100, kodashim_commandment_status__performance_only, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__performance_only, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
