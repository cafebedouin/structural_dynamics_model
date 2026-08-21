% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__incoherent_bundle_reading
 *   human_readable: Shinbutsu Ontological Substrate (Incoherent Bundle Reading)
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'incoherent bundle' reading of
 *   Shinbutsu-shūgō (the historical fusion of Shinto and Buddhism in Japan).
 *   From this perspective, the syncretism is not a coherent theological or
 *   ontological unity, but rather an accumulated institutional drift enforced
 *   by state power, particularly during periods like the Meiji Restoration's
 *   Shinbutsu Bunri (separation of Kami and Buddhas) which paradoxically
 *   solidified a new, state-managed form of 'fusion' by defining what was
 *   'pure' Shinto. The constraint operates as a snare, extracting cognitive
 *   coherence and suppressing alternative religious identities for the
 *   benefit of state authorities and established religious institutions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.85).
domain_priors:suppression_score(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.75).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__incoherent_bundle_reading, "Shinbutsu Ontological Substrate (Incoherent Bundle Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__incoherent_bundle_reading, "religious_studies/japanese_history/commitment_systems").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__incoherent_bundle_reading, '449506a2-c1fa-4d4a-b3b5-a9089c26ce90').
narrative_ontology:cs_kernel_codification('449506a2-c1fa-4d4a-b3b5-a9089c26ce90', distributed).
narrative_ontology:cs_authority_grounding('449506a2-c1fa-4d4a-b3b5-a9089c26ce90', extraction).
narrative_ontology:cs_interpretation_layer_present('449506a2-c1fa-4d4a-b3b5-a9089c26ce90').
narrative_ontology:cs_reading_relation('449506a2-c1fa-4d4a-b3b5-a9089c26ce90', shinbutsu_ontological_substrate__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('449506a2-c1fa-4d4a-b3b5-a9089c26ce90', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('449506a2-c1fa-4d4a-b3b5-a9089c26ce90', foundational, no_ontological_unity).
narrative_ontology:cs_axiom_status(no_ontological_unity, holdable).
narrative_ontology:cs_axiom_grounding('449506a2-c1fa-4d4a-b3b5-a9089c26ce90', no_ontological_unity, empirically_contingent).
narrative_ontology:cs_axiom('449506a2-c1fa-4d4a-b3b5-a9089c26ce90', foundational, syncretism_as_state_tool).
narrative_ontology:cs_axiom_status(syncretism_as_state_tool, holdable).
narrative_ontology:cs_axiom_grounding('449506a2-c1fa-4d4a-b3b5-a9089c26ce90', syncretism_as_state_tool, empirically_contingent).
narrative_ontology:cs_reference_frame('449506a2-c1fa-4d4a-b3b5-a9089c26ce90', pre_meiji_religious_pluralism).
narrative_ontology:cs_drift_state('449506a2-c1fa-4d4a-b3b5-a9089c26ce90', post_shinbutsu_bunri_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('449506a2-c1fa-4d4a-b3b5-a9089c26ce90', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_authorities).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, established_religious_institutions).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, local_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, theological_scholars).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, marginalized_religious_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically enforced the fusion of Shinto and Buddhist elements to consolidate political control and manage religious institutions. Benefits from the ambiguity and lack of clear ontological distinction, which prevents independent power bases from forming.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from state endorsement and the institutional inertia of the syncretic arrangement, which provides stable funding and social legitimacy. They often maintain the theatrical aspects of fusion even when theological coherence is absent.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, established_religious_institutions, beneficiary,
    organized, generational, constrained, national).

% Bear the cognitive burden of maintaining contradictory beliefs and practices without a coherent theological framework. Their identity is often fused with local traditions that blend Shinto and Buddhist elements, making exit from the syncretic bundle unthinkable despite its incoherence.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, local_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Struggle to construct coherent theological frameworks from the historically enforced syncretism, often facing internal contradictions. Their careers depend on engaging with the existing religious landscape, even if they critique its lack of ontological unity.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, theological_scholars, payer,
    moderate, biographical, constrained, global).

% Are excluded from the mainstream discourse and institutional benefits of the syncretic arrangement. They often represent traditions that resisted fusion or seek to establish distinct identities, facing suppression from state and established religious authorities.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, marginalized_religious_groups, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically, it coordinated religious practice under a unified state ideology, preventing sectarian conflict and consolidating state control over diverse religious traditions.
% TRANSFER_FUNCTION: Transfers legitimacy and institutional stability to state authorities and established religious institutions, while transferring cognitive dissonance and suppressed alternatives to practitioners and scholars.
% ABSENT_VOICES: Theological purists and marginalized groups seeking distinct religious identities are absent from the dominant narrative; they would argue for clear ontological distinctions or the dismantling of enforced syncretism.
% DISAPPEARANCE_RATIONALE: If the enforced syncretism vanished, the institutional landscape of Japanese religion would undergo significant reorganization. Many temples and shrines would need to redefine their identities, state control over religious affairs would weaken, and new theological movements might emerge, leading to a rearrangement of religious power and practice.
% FOUNDING_PROBLEM: The problem of managing diverse and potentially conflicting religious traditions (Shinto and Buddhism) within a unified political state, particularly during periods of state formation and consolidation.
% FOUNDING_PROBLEM_CORROBORATION: Historical records and independent academic scholarship corroborate that the initial problem of political control through religious fusion is largely resolved. However, established religious institutions and some state actors maintain that the 'unity' is still vital for social harmony, a claim contested by critical scholars and marginalized groups.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the constraint forces practitioners and scholars to operate within a framework that lacks internal consistency, imposing a 'cost of incoherence.' Suppression (0.75) is significant due to historical state enforcement and the social pressure to conform to established religious norms. The high theater ratio (0.6) reflects the performative maintenance of a 'unified' religious identity despite underlying contradictions, serving institutional rather than theological ends. The claimed type is 'snare' because the coordination story (religious harmony) is cover for extraction and suppression.
 *
 * PERSPECTIVAL GAP:
 *   The state and established institutions perceive the syncretism as a functional, if not ontologically unified, system that maintains social order. Practitioners and scholars, however, experience it as an incoherent bundle of traditions, enforced by historical power, that demands intellectual compromise and suppresses genuine theological inquiry. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State authorities and established religious institutions are beneficiaries, gaining political control and institutional stability. Local practitioners and theological scholars are victims, bearing the costs of cognitive dissonance and suppressed intellectual inquiry. Marginalized groups are excluded, their distinct identities suppressed by the dominant, enforced narrative.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_agency_vs_drift,
    'To what extent was Shinbutsu-shūgō a conscious theological project versus an accumulated institutional drift under political pressure?',
    'Detailed historical analysis of theological treatises and state decrees, distinguishing between genuine intellectual synthesis and politically motivated administrative directives.',
    'If primarily drift, this reading''s ''snare'' classification is strengthened. If significant theological agency is found, the ''syncretic_fusion_reading'' gains more ground, potentially reclassifying the constraint as a ''tangled_rope'' (coordination with extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_agency_vs_drift, empirical, 'Distinguishing intentional theological synthesis from institutional inertia and state enforcement.').

omega_variable(
    practitioner_cognitive_dissonance,
    'How do local practitioners actually reconcile (or fail to reconcile) the apparent contradictions within the syncretic bundle?',
    'Ethnographic studies and qualitative interviews with diverse practitioners, exploring their lived experience of religious identity and belief.',
    'If practitioners experience high levels of unaddressed cognitive dissonance, the ''snare'' classification is reinforced. If they have developed effective, localized reconciliation strategies, the ''extractiveness'' metric might be slightly lower for their seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(practitioner_cognitive_dissonance, empirical, 'Measuring the lived experience of theological incoherence among practitioners.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state enforcement, institutional pressure) or internalized (self-censorship, identity fusion)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., after state disestablishment), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making genuine alternatives harder to realize.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in religious practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(shin_tr_t30, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(shin_tr_t60, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement(shin_tr_t90, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 90, 0.6).
narrative_ontology:measurement(shin_tr_t120, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 120, 0.58).
narrative_ontology:measurement(shin_tr_t150, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 150, 0.6).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(shin_be_t30, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(shin_be_t60, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 60, 0.8).
narrative_ontology:measurement(shin_be_t90, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 90, 0.85).
narrative_ontology:measurement(shin_be_t120, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 120, 0.83).
narrative_ontology:measurement(shin_be_t150, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 150, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(shin_su_t30, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(shin_su_t60, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(shin_su_t90, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 90, 0.75).
narrative_ontology:measurement(shin_su_t120, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 120, 0.73).
narrative_ontology:measurement(shin_su_t150, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 150, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__incoherent_bundle_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate__domain_partition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'shinbutsu_ontological_substrate' kernel. This 'incoherent bundle' reading emphasizes the lack of genuine ontological unity and the role of state enforcement in maintaining a functionally syncretic, but internally contradictory, religious landscape. The other readings ('syncretic_fusion_reading' and 'domain_partition_reading') offer alternative interpretations of the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
