% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__incoherent_bundle_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: shinbutsu_coexistence_commitment__incoherent_bundle_reading
 *   human_readable: Shinbutsu-shugo as Incoherent Bundle (Incoherent Bundle Reading)
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   This constraint story represents the 'incoherent bundle' reading of
 *   Shinbutsu-shugo, arguing that the historical coexistence of Kami and
 *   Buddhist traditions in Japan was not a coherent syncretism but an
 *   unstable, ambiguous arrangement maintained by institutional power. This
 *   reading posits that the system lacked a stable ontological or theological
 *   kernel and was sustained by avoiding categorical questions, ultimately
 *   collapsing under the external pressure of the Meiji Restoration's
 *   shinbutsu bunri (separation of Kami and Buddhas), which revealed rather
 *   than created its incoherence. The constraint is classified as a Tangled
 *   Rope because it provided a coordination function (integrating diverse
 *   religious practices) but also involved significant asymmetric extraction
 *   and required active enforcement to maintain its ambiguous state.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.6).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.7).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "Shinbutsu-shugo as Incoherent Bundle (Incoherent Bundle Reading)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "religious_studies/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '9cfa1e39-791e-419f-bbe7-22aad2b9cb2b').
narrative_ontology:cs_kernel_codification('9cfa1e39-791e-419f-bbe7-22aad2b9cb2b', distributed).
narrative_ontology:cs_authority_grounding('9cfa1e39-791e-419f-bbe7-22aad2b9cb2b', extraction).
narrative_ontology:cs_interpretation_layer_present('9cfa1e39-791e-419f-bbe7-22aad2b9cb2b').
narrative_ontology:cs_reading_relation('9cfa1e39-791e-419f-bbe7-22aad2b9cb2b', shinbutsu_coexistence_commitment__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('9cfa1e39-791e-419f-bbe7-22aad2b9cb2b', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_axiom('9cfa1e39-791e-419f-bbe7-22aad2b9cb2b', foundational, no_stable_ontological_kernel).
narrative_ontology:cs_axiom_status(no_stable_ontological_kernel, holdable).
narrative_ontology:cs_axiom_grounding('9cfa1e39-791e-419f-bbe7-22aad2b9cb2b', no_stable_ontological_kernel, empirically_contingent).
narrative_ontology:cs_axiom('9cfa1e39-791e-419f-bbe7-22aad2b9cb2b', foundational, ambiguity_as_institutional_tool).
narrative_ontology:cs_axiom_status(ambiguity_as_institutional_tool, holdable).
narrative_ontology:cs_axiom_grounding('9cfa1e39-791e-419f-bbe7-22aad2b9cb2b', ambiguity_as_institutional_tool, empirically_contingent).
narrative_ontology:cs_reference_frame('9cfa1e39-791e-419f-bbe7-22aad2b9cb2b', pre_meiji_institutional_ambiguity).
narrative_ontology:cs_drift_state('9cfa1e39-791e-419f-bbe7-22aad2b9cb2b', meiji_restoration_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('9cfa1e39-791e-419f-bbe7-22aad2b9cb2b', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_temples).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinto_shrines).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, bakufu_authorities).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, local_communities).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theological_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the combined religious sites, often holding formal control over Shinto shrines. Benefited from the ambiguity by expanding their influence and landholdings, and by collecting offerings from both Buddhist and Kami worshipers. Actively resisted attempts to clarify or separate the traditions.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_temples, agenda_setter,
    institutional, generational, constrained, national).

% Often managed by Buddhist clergy, their distinct identity was subsumed under the larger Buddhist institutional structure. While benefiting from the resources and legitimacy of the combined system, they lacked independent institutional power to assert a distinct theological or administrative identity.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinto_shrines, beneficiary,
    organized, generational, constrained, national).

% Maintained the ambiguous shinbutsu-shugo system as a tool for social control and stability. The lack of clear theological boundaries prevented sectarian conflict and allowed for flexible administration of religious institutions. Benefited from the system's ability to absorb diverse local practices without challenging central authority.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, bakufu_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Participated in syncretic practices, often without a deep understanding of the underlying theological ambiguities. They bore the costs of maintaining both Buddhist temples and Shinto shrines, and their local traditions were often shaped by the institutional arrangements imposed by the combined system. Had no real exit from the prevailing religious framework.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, local_communities, payer,
    powerless, biographical, trapped, local).

% Intellectuals and religious leaders who sought to clarify the distinct identities of Shinto and Buddhism, often advocating for a pure Shinto. They faced institutional resistance and suppression from the established Buddhist and Bakafu authorities, and their efforts were largely marginalized until the Meiji Restoration.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theological_reformers, payer,
    moderate, biographical, constrained, national).

% Upon its establishment, viewed shinbutsu-shugo as an obstacle to national unity and a pure, state-backed Shinto. Its policies of shinbutsu bunri (separation of Kami and Buddhas) revealed the underlying incoherence of the previous system, rather than creating it. Acted as an external force that exposed the constraint's fragility.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_government, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a flexible framework for integrating diverse local religious practices and beliefs under a single administrative and institutional umbrella, preventing sectarian conflict and facilitating social control.
% TRANSFER_FUNCTION: Transferred legitimacy, resources, and administrative control from local Shinto traditions to the larger Buddhist institutional complex, and from both to the Bakafu authorities for social stability.
% ABSENT_VOICES: Early Shinto purists and theological reformers who sought to define clear ontological boundaries between Kami and Buddhas were largely excluded from positions of institutional power and their arguments suppressed by the dominant Buddhist and Bakafu establishments.
% DISAPPEARANCE_RATIONALE: The Meiji Restoration's forced separation of Kami and Buddhas (shinbutsu bunri) led to a dramatic reorganization of religious institutions, land ownership, and theological discourse. Temples lost control of shrines, Buddhist statues were removed from shrines, and a distinct Shinto identity was actively constructed, demonstrating that the previous 'bundle' was a contingent, enforced arrangement.
% FOUNDING_PROBLEM: To integrate indigenous Kami worship with the newly introduced Buddhist traditions, creating a unified religious landscape that could be managed by central authorities and prevent religious strife.
% FOUNDING_PROBLEM_CORROBORATION: The Meiji government's successful, albeit violent, separation of the two traditions, and the subsequent construction of a distinct State Shinto, demonstrates that the 'problem' of integration was solved by political will, not by inherent theological coherence. Historical records and modern academic consensus, from outside the pre-Meiji religious institutions, corroborate that the original problem was superseded by a new political order.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) stems from the institutional power of Buddhist temples and Bakafu authorities, who benefited from the ambiguity and administrative control over Shinto sites and resources. Suppression (0.7) was high due to the active marginalization of Shinto purists and reformers who sought to clarify the distinct identities of Kami and Buddhas. The theater ratio (0.4) reflects the performative maintenance of a 'unified' religious front despite underlying theological tensions and administrative complexities. The system's persistence relied on deliberate ambiguity and the suppression of alternative theological framings, rather than genuine syncretic coherence.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Buddhist temples and Bakafu authorities, shinbutsu-shugo was a functional system for religious administration and social control, a 'rope' that coordinated diverse beliefs. For local communities and theological reformers, it was a 'snare' that extracted resources and suppressed distinct religious identities, maintained by institutional power and deliberate ambiguity. The engine's per-seat classification will reflect this divergence based on the declared power, exit options, and beneficiary/victim roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist temples and Bakafu authorities are beneficiaries and agenda-setters, actively shaping and benefiting from the ambiguous system, placing their directionality towards the beneficiary end. Shinto shrines, while part of the system, were often subsumed and lacked independent agency, making them beneficiaries with constrained exit. Local communities and theological reformers were payers and victims, bearing the costs and having their distinct practices or reform efforts suppressed, placing their directionality towards the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (integrating diverse traditions and preventing strife) became 'dead' by the Meiji era, as the system's primary function shifted from genuine coordination to maintaining institutional power and suppressing alternative religious framings. The Meiji government's intervention revealed that the 'coordination' was largely a cover for extraction and control, and that the system's persistence was due to institutional inertia and active enforcement, not its continued functional necessity. This prevents mislabeling it as a genuine Rope by highlighting its extractive and suppressive elements, which became dominant over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_theological_coherence,
    'To what extent did pre-Meiji shinbutsu-shugo possess an underlying, albeit unarticulated, theological coherence that transcended institutional ambiguity?',
    'Discovery of previously unexamined theological texts or ritual practices that demonstrate a consistent, integrated understanding of Kami and Buddhas across different regions and periods, independent of institutional pressures.',
    'If significant coherence is found, the ''incoherent bundle'' reading''s extractiveness and suppression metrics might be overstated, and the constraint could lean more towards a ''rope'' or ''scaffold'' that genuinely coordinated a complex theological landscape. If no such coherence is found, the current classification is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(degree_of_theological_coherence, empirical, 'Ambiguity regarding the true theological coherence of shinbutsu-shugo.').

omega_variable(
    role_of_popular_belief_vs_elite_discourse,
    'Was the ''incoherence'' primarily a feature of elite theological and administrative discourse, while popular religious practice maintained a more stable, functional syncretism?',
    'Extensive ethnographic and historical research into local religious practices and beliefs, distinguishing between the lived religion of common people and the institutional framings of temples and authorities.',
    'If popular belief showed stable syncretism, the constraint''s ''incoherence'' might be re-framed as a feature of the elite layer, and the ''payer'' status of local communities might be re-evaluated, potentially shifting the constraint''s classification towards a more complex ''tangled_rope'' with different dynamics at different social levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(role_of_popular_belief_vs_elite_discourse, conceptual, 'Whether incoherence was an elite or popular phenomenon.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 1600, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1600, 0.3).
narrative_ontology:measurement(shin_tr_t1650, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1650, 0.35).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1700, 0.38).
narrative_ontology:measurement(shin_tr_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1750, 0.4).
narrative_ontology:measurement(shin_tr_t1800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1800, 0.42).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1868, 0.4).

% Extraction over time
narrative_ontology:measurement(shin_be_t1600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1600, 0.5).
narrative_ontology:measurement(shin_be_t1650, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1650, 0.55).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1700, 0.58).
narrative_ontology:measurement(shin_be_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1750, 0.6).
narrative_ontology:measurement(shin_be_t1800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1800, 0.62).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1868, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1600, 0.6).
narrative_ontology:measurement(shin_su_t1650, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1650, 0.65).
narrative_ontology:measurement(shin_su_t1700, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1700, 0.68).
narrative_ontology:measurement(shin_su_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1750, 0.7).
narrative_ontology:measurement(shin_su_t1800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1800, 0.72).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1868, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__incoherent_bundle_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_state_shinto_establishment).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'shinbutsu_coexistence_commitment' kernel. This 'incoherent bundle' reading emphasizes the lack of a stable ontological basis and the role of institutional power in maintaining ambiguity, contrasting with the 'syncretic fusion' and 'domain partition' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
