% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__domain_partition_reading
 *   human_readable: Shinbutsu Coexistence: Domain Partition
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint story instantiates the 'domain_partition_reading' of the
 *   'shinbutsu_coexistence_commitment' kernel. It describes the historical
 *   arrangement where Kami (Shinto deities) and Buddhas (Buddhist deities)
 *   were understood to govern separate existential domains—Kami for life,
 *   purity, and harvest; Buddhas for death, salvation, and the
 *   afterlife—without requiring deep ontological unification. This reading
 *   emphasizes functional coexistence and boundary maintenance, largely
 *   driven by popular practice and institutional custom, rather than a
 *   unified theological system. The constraint is claimed as a Tangled Rope
 *   because it provided genuine coordination (clear spiritual roles) but also
 *   involved subtle extraction by securing the distinct institutional
 *   authority and roles of both Shinto and Buddhist establishments, at the
 *   cost of suppressing alternative, more unified theological
 *   interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.58).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.65).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Shinbutsu Coexistence: Domain Partition").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious_studies/philosophy_of_religion/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__domain_partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, '93381bdf-1d1a-4096-a5e0-673d67214a5d').
narrative_ontology:cs_kernel_codification('93381bdf-1d1a-4096-a5e0-673d67214a5d', implicit).
narrative_ontology:cs_authority_grounding('93381bdf-1d1a-4096-a5e0-673d67214a5d', practice).
narrative_ontology:cs_interpretation_layer_present('93381bdf-1d1a-4096-a5e0-673d67214a5d').
narrative_ontology:cs_reading_relation('93381bdf-1d1a-4096-a5e0-673d67214a5d', shinbutsu_coexistence_commitment__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('93381bdf-1d1a-4096-a5e0-673d67214a5d', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('93381bdf-1d1a-4096-a5e0-673d67214a5d', foundational, kami_buddha_domain_separation).
narrative_ontology:cs_axiom_status(kami_buddha_domain_separation, holdable).
narrative_ontology:cs_axiom_grounding('93381bdf-1d1a-4096-a5e0-673d67214a5d', kami_buddha_domain_separation, conventional).
narrative_ontology:cs_axiom('93381bdf-1d1a-4096-a5e0-673d67214a5d', foundational, no_ontological_unification).
narrative_ontology:cs_axiom_status(no_ontological_unification, holdable).
narrative_ontology:cs_axiom_grounding('93381bdf-1d1a-4096-a5e0-673d67214a5d', no_ontological_unification, deontological).
narrative_ontology:cs_reference_frame('93381bdf-1d1a-4096-a5e0-673d67214a5d', pre_meiji_functional_division).
narrative_ontology:cs_drift_state('93381bdf-1d1a-4096-a5e0-673d67214a5d', post_meiji_separation_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('93381bdf-1d1a-4096-a5e0-673d67214a5d', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_priesthood).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_monastic_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, general_populace).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, theologians_seeking_unification).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, syncretic_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers rituals and practices related to kami, life, purity, and harvest. Benefits from the clear demarcation of its domain, which secures its institutional role and prevents direct competition or theological subsumption by Buddhism. Actively maintains the boundaries of its domain through tradition and practice.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_priesthood, agenda_setter,
    institutional, generational, constrained, national).

% Administers rituals and teachings related to Buddhas, death, salvation, and the afterlife. Benefits from the clear demarcation of its domain, securing its institutional role and preventing direct competition or theological subsumption by Shinto. Actively maintains the boundaries of its domain through doctrine and practice.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_monastic_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from a clear, functional division of spiritual labor, knowing which tradition to approach for specific life events (e.g., Shinto for birth, Buddhist for funerals). This provides practical spiritual guidance without requiring deep theological understanding or commitment to a single, unified system.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, general_populace, beneficiary,
    moderate, biographical, mobile, local).

% Bear the cost of intellectual and spiritual frustration due to the lack of ontological unification. Their efforts to create a coherent, unified theological framework are often marginalized or dismissed by the dominant practical partition, making their academic and spiritual pursuits constrained by the prevailing functional separation.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, theologians_seeking_unification, payer,
    powerless, generational, identity_locked, national).

% While their personal practices might blend elements, they operate within a social and institutional context that largely reinforces the domain partition. They may face subtle pressure to conform to distinct ritual roles or conceptual categories, even if their personal experience is more fluid.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, syncretic_practitioners, payer,
    powerless, biographical, constrained, local).

% Analyze the historical development and functional operation of shinbutsu coexistence, including the domain partition. They can identify the beneficiaries and victims of this arrangement but do not directly participate in its maintenance or suffer its costs.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, modern_religious_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, functional division of spiritual labor and existential concerns, allowing indigenous Shinto and imported Buddhism to coexist and serve distinct societal needs without direct conflict or forced theological synthesis.
% TRANSFER_FUNCTION: Transfers spiritual authority and ritual responsibility for specific life events (e.g., birth, harvest, purity) to Kami, and for death, afterlife, and salvation to Buddhas, securing institutional roles for both traditions.
% ABSENT_VOICES: Theologians and philosophical schools that sought a deeper, unified ontological understanding of Kami and Buddhas, as well as practitioners whose personal spiritual experience naturally blurred these boundaries, were often marginalized in the dominant discourse that emphasized functional separation.
% DISAPPEARANCE_RATIONALE: If the implicit commitment to domain partition vanished overnight, the religious landscape of Japan would be fundamentally altered. Institutional roles for Shinto and Buddhist clergy would lose their clear functional basis, leading to competition or forced integration, and popular religious practice would lose its established framework for navigating life and death events.
% FOUNDING_PROBLEM: To integrate two distinct religious traditions (indigenous Shinto and imported Buddhism) within Japanese society, allowing both to flourish and serve the populace without one subsuming the other or causing constant theological and institutional conflict.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts and anthropological studies attest to the functional integration of Shinto and Buddhism. Modern religious scholarship, however, often contests whether the 'problem' was truly solved or merely managed through a pragmatic, rather than unified, approach. The Meiji Restoration's forced separation (Shinbutsu-bunri) further complicates the status of the 'founding problem' by demonstrating its constructed nature.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 0.58, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.58) reflects the cost of maintaining distinct institutional authorities and the suppression of alternative theological paths. Suppression (0.65) is moderate, as the partition was maintained more by social norms, institutional inertia, and the lack of a strong counter-movement than by overt coercion. However, the institutional 'agenda-setters' actively enforced these boundaries through their practices and teachings. Theater ratio is low (0.15) because the functional division was largely real in practice, not merely performative. The metrics show a gradual increase in extractiveness and suppression over a long historical period, reflecting the solidification of institutional roles and the increasing difficulty for alternative views to gain traction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the institutional agenda-setters and the general populace, this arrangement was a successful coordination mechanism that provided clarity and stability. From the perspective of theologians and syncretic practitioners, it was a constraint that limited spiritual and intellectual exploration, imposing a cost for seeking a more unified understanding. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Shinto priesthood and Buddhist monastic institutions are the primary beneficiaries and agenda-setters, as the domain partition secured their distinct institutional authority and roles. The general populace also benefits from the clear functional guidance. Theologians seeking unification and syncretic practitioners bear the costs, as their efforts to bridge or blend the domains were implicitly constrained by the dominant partition. Their 'identity_locked' exit option reflects the deep intellectual and spiritual commitment to their pursuits within the prevailing religious framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''domain_partition_reading'' of the ''shinbutsu_coexistence_commitment'' kernel?',
    'Comparative analysis with historical texts and scholarly interpretations of shinbutsu-shugo, focusing on explicit statements or implicit assumptions about the relationship between Kami and Buddhas.',
    'If misidentified, the entire analysis of this constraint''s structural properties and its relationship to sibling readings would be flawed, requiring re-evaluation against the correct kernel reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verification of kernel and reading identification.').

omega_variable(
    ontological_unification_ambiguity,
    'Is the ''no ontological unification'' aspect of this reading a descriptive fact of historical practice, or a prescriptive claim enforced by institutional power?',
    'Examination of historical instances where attempts at ontological unification were explicitly suppressed or marginalized by religious institutions, versus periods where such attempts were merely uncommon but tolerated.',
    'If primarily prescriptive and enforced, the constraint''s suppression and extractiveness would be higher than currently estimated, reflecting a more active suppression of alternative theological frameworks. If purely descriptive, the constraint would lean more towards a pure Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_unification_ambiguity, empirical, 'Distinguishing descriptive vs. prescriptive nature of domain separation.').

omega_variable(
    institutional_vs_popular_authority,
    'To what extent was the domain partition maintained by institutional authority (priesthoods, monastic orders) versus popular religious practice and belief?',
    'Anthropological studies of local religious practices and beliefs compared with official institutional doctrines and pronouncements over time.',
    'If popular practice was the primary driver, the ''requires_active_enforcement'' flag might be overstated, and the constraint would lean more towards a Rope. If institutional authority was dominant, the Tangled Rope classification is further reinforced, highlighting the role of agenda-setters in maintaining the partition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_popular_authority, empirical, 'Source of authority for maintaining the domain partition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 0, 868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement(shin_tr_t250, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 250, 0.13).
narrative_ontology:measurement(shin_tr_t400, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 400, 0.14).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 600, 0.15).
narrative_ontology:measurement(shin_tr_t868, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 868, 0.15).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(shin_be_t100, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 100, 0.48).
narrative_ontology:measurement(shin_be_t250, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 250, 0.52).
narrative_ontology:measurement(shin_be_t400, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 400, 0.55).
narrative_ontology:measurement(shin_be_t600, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 600, 0.57).
narrative_ontology:measurement(shin_be_t868, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 868, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(shin_su_t100, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 100, 0.58).
narrative_ontology:measurement(shin_su_t250, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 250, 0.6).
narrative_ontology:measurement(shin_su_t400, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 400, 0.62).
narrative_ontology:measurement(shin_su_t600, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 600, 0.64).
narrative_ontology:measurement(shin_su_t868, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 868, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
