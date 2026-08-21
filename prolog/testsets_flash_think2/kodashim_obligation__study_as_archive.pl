% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_archive, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kodashim_obligation__study_as_archive
 *   human_readable: Study of Kodashim as Historical Archive and Identity Maintenance
 *   domain: religious/cultural/legal
 *
 * SUMMARY:
 *   This constraint represents the 'study_as_archive' reading of the
 *   'kodashim_obligation' kernel. In this reading, the study of Kodashim (the
 *   order of the Mishnah dealing with sacrificial rites) is understood as a
 *   practice of historical preservation and identity-maintenance for the
 *   Jewish people, rather than a preparation for future legal obligation or
 *   an enactment of cosmic function. The system it describes is considered
 *   defunct due to the destruction of the Temple. The constraint is claimed
 *   as a Rope by its proponents (emphasizing identity coordination), but its
 *   metrics reflect a Tangled Rope due to the diversion of intellectual
 *   resources and the active enforcement of this focus within traditional
 *   educational systems.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, 0.6).
domain_priors:suppression_score(kodashim_obligation__study_as_archive, 0.4).
domain_priors:theater_ratio(kodashim_obligation__study_as_archive, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, extractiveness, 0.6).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_archive, "Study of Kodashim as Historical Archive and Identity Maintenance").
narrative_ontology:topic_domain(kodashim_obligation__study_as_archive, "religious/cultural/legal").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_archive, 'dfff5946-f7d1-4e69-adfc-36252dfcad60').
narrative_ontology:cs_kernel_codification('dfff5946-f7d1-4e69-adfc-36252dfcad60', fixed_text).
narrative_ontology:cs_authority_grounding('dfff5946-f7d1-4e69-adfc-36252dfcad60', lineage).
narrative_ontology:cs_interpretation_layer_present('dfff5946-f7d1-4e69-adfc-36252dfcad60').
narrative_ontology:cs_reading_relation('dfff5946-f7d1-4e69-adfc-36252dfcad60', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('dfff5946-f7d1-4e69-adfc-36252dfcad60', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_axiom('dfff5946-f7d1-4e69-adfc-36252dfcad60', foundational, sacrificial_system_defunct).
narrative_ontology:cs_axiom_status(sacrificial_system_defunct, holdable).
narrative_ontology:cs_axiom_grounding('dfff5946-f7d1-4e69-adfc-36252dfcad60', sacrificial_system_defunct, empirically_contingent).
narrative_ontology:cs_axiom('dfff5946-f7d1-4e69-adfc-36252dfcad60', foundational, study_as_identity_preservation).
narrative_ontology:cs_axiom_status(study_as_identity_preservation, holdable).
narrative_ontology:cs_axiom_grounding('dfff5946-f7d1-4e69-adfc-36252dfcad60', study_as_identity_preservation, conventional).
narrative_ontology:cs_reference_frame('dfff5946-f7d1-4e69-adfc-36252dfcad60', post_temple_diaspora_continuity).
narrative_ontology:cs_drift_state('dfff5946-f7d1-4e69-adfc-36252dfcad60', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dfff5946-f7d1-4e69-adfc-36252dfcad60', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_archive, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, jewish_communal_identity).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, rabbinic_scholars).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, intellectual_resources_diverted).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, students_of_halakha).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, students_of_halakha).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary custodians and transmitters of Jewish legal tradition. They actively promote and structure the study of Kodashim, deriving authority and legitimacy from their role in maintaining this historical continuity and communal identity.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_archive, rabbinic_scholars, beneficiary).

% The collective self-conception and continuity of the Jewish people. This identity is significantly reinforced and maintained through the study of historical texts like Kodashim, providing a sense of unbroken tradition and shared heritage.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, jewish_communal_identity, beneficiary,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_archive, jewish_communal_identity).

% Individuals dedicating significant time and intellectual effort to traditional Jewish legal study. While they gain cultural literacy and communal belonging, a portion of their intellectual resources is directed towards texts with no direct contemporary legal applicability, representing a cost.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, students_of_halakha, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_archive, students_of_halakha, beneficiary).

% The collective intellectual capacity and focus of the Jewish community, which is directed towards the study of Kodashim. This represents a diversion from other areas of study, such as contemporary applicable law (Halakha), ethics, or secular knowledge, which might offer more immediate practical or social benefit.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, intellectual_resources_diverted, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_archive, intellectual_resources_diverted).

% Academics who study Kodashim from a purely historical and cultural perspective, without internalizing its normative claims or participating in its religious practice. They analyze its role in Jewish history and identity formation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, secular_jewish_historians, observer,
    analytical, generational, analytical, global).

% Branches of Judaism that often de-emphasize the study of Kodashim due to its perceived irrelevance to modern religious practice and ethical concerns. While they are part of the broader Jewish intellectual landscape, their perspective on the normative value of Kodashim study is often marginalized within traditional discourse.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, reform_jewish_movements, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_archive, jewish_communal_identity).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared intellectual and cultural heritage, providing continuity for Jewish communal identity across generations by preserving historical legal texts and the interpretive tradition surrounding them.
% TRANSFER_FUNCTION: Transfers significant intellectual effort and educational focus from immediately applicable legal studies (Halakha) to historical and identity-affirming textual engagement, reinforcing the authority of traditional rabbinic institutions.
% ABSENT_VOICES: Those advocating for a purely utilitarian approach to Jewish legal study, or those who see the focus on defunct laws as a distraction from contemporary ethical and social issues, are often marginalized in traditional educational settings.
% DISAPPEARANCE_RATIONALE: If the study of Kodashim as an archival and identity-maintenance practice vanished overnight, it would lead to a significant erosion of historical consciousness and a weakening of a key pillar of traditional Jewish communal identity, forcing a re-evaluation of what constitutes Jewish continuity and the role of rabbinic authority.
% FOUNDING_PROBLEM: How to maintain the integrity and continuity of Jewish tradition, including its historical legal texts, after the destruction of the Temple and the cessation of sacrificial practice, without falsely asserting their contemporary legal applicability.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish thought and cultural anthropologists, from outside the immediate rabbinic establishment, corroborate the role of Kodashim study in identity formation and historical continuity, even while acknowledging its lack of direct legal applicability. This is supported by sociological studies of religious communities.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_archive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_archive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kodashim_obligation__study_as_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_archive, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_obligation__study_as_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.60) arises from the diversion of significant intellectual resources and educational focus towards texts that are not directly applicable to contemporary Jewish life, yet are presented as essential for communal identity. Suppression (0.40) is moderate, as alternative intellectual pursuits are not strictly forbidden but are often de-emphasized or seen as secondary within traditional frameworks. The theater ratio (0.30) reflects the performative aspect of maintaining a tradition for its own sake, though genuine historical and identity-forming functions remain. The increasing extractiveness and theater ratio over time reflect the gradual hardening of this interpretive stance and the growing distance from any practical application.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic scholars, the study is a vital Rope, coordinating identity and preserving heritage. From the perspective of students seeking applicable knowledge, or those concerned with the broader allocation of intellectual resources, it functions as a Tangled Rope, extracting effort for a non-functional output while providing communal benefits. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish communal identity and rabbinic scholars are beneficiaries, as the practice reinforces their continuity and authority. Students of Halakha and the intellectual resources of the community are victims, bearing the cost of diverted focus. Reform Jewish movements are excluded, as their alternative interpretations are often marginalized. Secular historians act as observers, analyzing the phenomenon without being subject to its internal normative claims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latent_function_ambiguity,
    'Does the study of Kodashim, even if defunct in its literal application, possess a latent, unacknowledged function beyond historical preservation and identity maintenance (e.g., a subtle influence on ethical reasoning or a symbolic cosmic role)?',
    'Detailed ethnographic studies of contemporary Kodashim study practices, or theological analysis of implicit claims made by practitioners, to identify any unstated functional outputs.',
    'If a significant latent function is identified, the constraint''s extractiveness might be lower than currently assessed, as the ''diverted resources'' would be contributing to an unacknowledged benefit. This could shift the classification closer to a pure Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latent_function_ambiguity, empirical, 'Whether Kodashim study has unacknowledged functional outputs.').

omega_variable(
    intellectual_resource_cost_measurement,
    'How precisely can the ''cost'' of diverting intellectual resources be quantified, and what are the opportunity costs in terms of foregone applicable legal or ethical development?',
    'Comparative studies of curricula in traditional vs. modern Jewish educational institutions, or economic analysis of intellectual labor allocation within the community, to estimate the impact of Kodashim study on other fields.',
    'A higher, quantifiable opportunity cost would increase the perceived extractiveness, potentially pushing the constraint closer to a Snare. A lower cost would support a more benign Tangled Rope or Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intellectual_resource_cost_measurement, empirical, 'Quantification of intellectual resource diversion costs.').

omega_variable(
    identity_maintenance_genuineness,
    'Is the ''identity-maintenance'' function a genuine coordination benefit for the community, or does it primarily serve to reinforce the authority and legitimacy of the rabbinic institutions that promote this study?',
    'Sociological surveys of communal members regarding their sense of identity and belonging derived from Kodashim study, contrasted with the self-perceived authority of rabbinic leadership. Analysis of power dynamics within educational institutions.',
    'If the primary benefit accrues to institutional authority rather than diffuse communal identity, the constraint''s coordination function is weaker, and its extractive component is amplified, pushing it closer to a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_maintenance_genuineness, conceptual, 'Distinguishing genuine identity coordination from institutional power reinforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_archive, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t70, kodashim_obligation__study_as_archive, theater_ratio, 70, 0.2).
narrative_ontology:measurement(koda_tr_t500, kodashim_obligation__study_as_archive, theater_ratio, 500, 0.23).
narrative_ontology:measurement(koda_tr_t1000, kodashim_obligation__study_as_archive, theater_ratio, 1000, 0.25).
narrative_ontology:measurement(koda_tr_t1500, kodashim_obligation__study_as_archive, theater_ratio, 1500, 0.28).
narrative_ontology:measurement(koda_tr_t1800, kodashim_obligation__study_as_archive, theater_ratio, 1800, 0.29).
narrative_ontology:measurement(koda_tr_t2024, kodashim_obligation__study_as_archive, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(koda_be_t70, kodashim_obligation__study_as_archive, base_extractiveness, 70, 0.5).
narrative_ontology:measurement(koda_be_t500, kodashim_obligation__study_as_archive, base_extractiveness, 500, 0.53).
narrative_ontology:measurement(koda_be_t1000, kodashim_obligation__study_as_archive, base_extractiveness, 1000, 0.55).
narrative_ontology:measurement(koda_be_t1500, kodashim_obligation__study_as_archive, base_extractiveness, 1500, 0.58).
narrative_ontology:measurement(koda_be_t1800, kodashim_obligation__study_as_archive, base_extractiveness, 1800, 0.59).
narrative_ontology:measurement(koda_be_t2024, kodashim_obligation__study_as_archive, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t70, kodashim_obligation__study_as_archive, suppression_requirement, 70, 0.35).
narrative_ontology:measurement(koda_su_t500, kodashim_obligation__study_as_archive, suppression_requirement, 500, 0.37).
narrative_ontology:measurement(koda_su_t1000, kodashim_obligation__study_as_archive, suppression_requirement, 1000, 0.38).
narrative_ontology:measurement(koda_su_t1500, kodashim_obligation__study_as_archive, suppression_requirement, 1500, 0.39).
narrative_ontology:measurement(koda_su_t1800, kodashim_obligation__study_as_archive, suppression_requirement, 1800, 0.4).
narrative_ontology:measurement(koda_su_t2024, kodashim_obligation__study_as_archive, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_archive, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_preparation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_obligation' kernel, each representing a distinct structural claim about the purpose and function of studying the Kodashim order of the Mishnah. This reading emphasizes historical preservation and identity maintenance, contrasting with readings focused on cosmic function or future applicability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
