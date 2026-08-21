% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__symbol_survival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__symbol_survival_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__symbol_survival_reading
 *   human_readable: Catastrophe Memory Survival: Symbolic Continuity Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes the 'symbol_survival_reading' of the
 *   'catastrophe_memory_survival' kernel. It posits that ritual's primary
 *   function in preserving memory and identity is through symbolic experience
 *   and the continuity of practice itself, rather than the transmission of
 *   practical knowledge. The constraint is claimed as a Tangled Rope because
 *   it genuinely coordinates identity and memory but does so with significant
 *   extraction from those who cannot or will not adhere to its strict forms,
 *   enforced by rabbinic authority. The metrics reflect this: high
 *   extractiveness for those who must conform or be excluded, and active
 *   suppression of alternative interpretations or practices.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.7).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.65).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Catastrophe Memory Survival: Symbolic Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, '7fd08b1f-bbc4-4c65-b842-f1f770842ef3').
narrative_ontology:cs_kernel_codification('7fd08b1f-bbc4-4c65-b842-f1f770842ef3', formalized).
narrative_ontology:cs_authority_grounding('7fd08b1f-bbc4-4c65-b842-f1f770842ef3', lineage).
narrative_ontology:cs_interpretation_layer_present('7fd08b1f-bbc4-4c65-b842-f1f770842ef3').
narrative_ontology:cs_reading_relation('7fd08b1f-bbc4-4c65-b842-f1f770842ef3', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('7fd08b1f-bbc4-4c65-b842-f1f770842ef3', catastrophe_memory_survival__hybrid_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('7fd08b1f-bbc4-4c65-b842-f1f770842ef3', foundational, ritual_meaning_is_inherent_in_form).
narrative_ontology:cs_axiom_status(ritual_meaning_is_inherent_in_form, holdable).
narrative_ontology:cs_axiom_grounding('7fd08b1f-bbc4-4c65-b842-f1f770842ef3', ritual_meaning_is_inherent_in_form, deontological).
narrative_ontology:cs_axiom('7fd08b1f-bbc4-4c65-b842-f1f770842ef3', foundational, continuity_of_practice_is_survival).
narrative_ontology:cs_axiom_status(continuity_of_practice_is_survival, holdable).
narrative_ontology:cs_axiom_grounding('7fd08b1f-bbc4-4c65-b842-f1f770842ef3', continuity_of_practice_is_survival, conventional).
narrative_ontology:cs_reference_frame('7fd08b1f-bbc4-4c65-b842-f1f770842ef3', unbroken_halakhic_transmission).
narrative_ontology:cs_drift_state('7fd08b1f-bbc4-4c65-b842-f1f770842ef3', contemporary_secular_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7fd08b1f-bbc4-4c65-b842-f1f770842ef3', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, orthodox_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, secularized_jews).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, reform_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the integrity and continuity of ritual practice, interpreting and enforcing halakhic (Jewish law) norms. Benefits from the authority derived from preserving tradition and defining communal identity through ritual.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Experience strong communal cohesion and identity through shared ritual practice. The rituals reinforce their boundary norms and provide a sense of continuity with historical memory, but require strict adherence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, orthodox_communities, beneficiary,
    organized, generational, constrained, local).

% May feel alienated or excluded by the strictures of traditional ritual, struggling to find meaning in forms whose symbolic content is no longer fully accessible or relevant to their modern lives. They bear the cost of cultural distance and potential loss of heritage.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secularized_jews, payer,
    moderate, biographical, identity_locked, local).

% Seek to adapt ritual practice to contemporary values and understanding, often simplifying or reinterpreting traditional forms. They bear the cost of contention with orthodox interpretations and potential loss of historical continuity in their adaptations.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, reform_movements, payer,
    organized, generational, mobile, national).

% The abstract concept of the collective memory of catastrophe, which is preserved or altered by the ritual. It is not an agent but is a key referent for the constraint's function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, historical_memory, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_survival__symbol_survival_reading, historical_memory).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective identity and boundary maintenance for Jewish communities by providing shared symbolic experiences and a continuous link to historical memory, particularly of catastrophe.
% TRANSFER_FUNCTION: Transfers cultural capital, communal belonging, and a sense of historical continuity from rabbinic authority and orthodox communities to participants, in exchange for adherence to prescribed ritual forms and interpretive frameworks.
% ABSENT_VOICES: Those who have fully assimilated or abandoned Jewish identity are absent from the conversation, having exited the system entirely. Their perspective would highlight the coercive aspects of identity maintenance and the costs of non-conformity.
% DISAPPEARANCE_RATIONALE: If the ritual practices and their enforcement vanished, the collective identity and boundary norms of Jewish communities would rapidly fragment. The sense of continuity with catastrophe memory would dissipate, leading to a profound reorganization of communal life and self-understanding.
% FOUNDING_PROBLEM: The existential threat of cultural and religious annihilation following historical catastrophes, requiring mechanisms to preserve identity and memory across generations.
% FOUNDING_PROBLEM_CORROBORATION: Historians and sociologists of religion, from outside the benefiting parties, corroborate the ongoing challenge of cultural transmission and identity preservation in the face of assimilation and secularization, affirming the founding problem's continued relevance.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__symbol_survival_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__symbol_survival_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__symbol_survival_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_survival__symbol_survival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__symbol_survival_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.7) reflects the cost of strict adherence to ritual forms, which can be alienating for those seeking more flexible or modern expressions of identity. Suppression (0.65) is necessary to maintain the 'correct' symbolic interpretation and practice, actively discouraging deviation. The theater ratio (0.4) indicates that while the symbolic function is real, a significant portion of the effort goes into maintaining the form for its own sake, rather than for direct, observable utility. The cyclical nature of the measurements reflects periods of increased adherence and enforcement followed by periods of relaxation or adaptation, driven by external cultural pressures and internal reform movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic authority, the constraint is a vital Rope, ensuring the survival of Jewish identity. From the perspective of secularized Jews, it is a Snare, extracting conformity and cultural alienation. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority and orthodox communities are beneficiaries, as the constraint reinforces their power and communal cohesion. Secularized Jews and reform movements are payers, bearing the costs of alienation, exclusion, or the effort required to adapt or resist the dominant ritual forms. Historical memory is an analytical observer, the object of the constraint's action.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_practical_efficacy,
    'Is the primary efficacy of ritual in catastrophe memory survival truly symbolic, or does it also transmit practical, adaptive knowledge?',
    'Ethnographic studies comparing survival outcomes and adaptive capacity in communities with varying degrees of ritual adherence and explicit practical knowledge transmission. If communities with high ritual adherence but low explicit practical knowledge transmission show lower adaptive capacity, it would challenge the purely symbolic reading.',
    'If practical efficacy is significant, this reading''s extractiveness might be overstated, as some ''cost'' is actually a ''benefit'' of transmitted competence. The constraint might reclassify closer to a Rope or Scaffold if the practical function is substantial and broadly beneficial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_practical_efficacy, empirical, 'Ambiguity regarding the true mechanism of ritual''s efficacy in survival.').

omega_variable(
    identity_coercion_vs_belonging,
    'To what extent does the enforcement of ritual forms constitute coercion for identity, versus providing a genuine sense of belonging and continuity?',
    'Longitudinal studies of individuals who leave orthodox communities: if they report sustained psychological distress or identity fragmentation after exit, it suggests a higher degree of identity coercion. If they find alternative forms of belonging and identity, it suggests the belonging function is more robust and less coercive.',
    'If coercion is dominant, the suppression and extractiveness metrics are accurate. If belonging is the primary driver, the constraint might be closer to a Rope, with ''costs'' being the price of entry into a valued community. This would shift the directionality for ''secularized_jews'' and ''reform_movements'' towards more symmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_coercion_vs_belonging, conceptual, 'Distinguishing between identity coercion and genuine belonging facilitated by ritual.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external barriers to alternative practices) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-exit suppression trajectory: if individuals who leave orthodox communities continue to self-censor or feel guilt regarding non-traditional practices, reclassify as partially internalized suppression. If they freely adopt new practices, it is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making exit less ''free'' than it appears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in ritual adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 75, 0.38).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 75, 0.68).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 100, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 50, 0.65).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 75, 0.63).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__symbol_survival_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_memory_survival' kernel, focusing on symbolic and practice-based survival. It coexists with readings emphasizing practical competence and hybrid encoding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
