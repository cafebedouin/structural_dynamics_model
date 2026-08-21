% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__competence_transmission_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__competence_transmission_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__competence_transmission_reading
 *   human_readable: Ritual as Catastrophe Memory: Competence Transmission Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes how ritual functions as a mechanism for
 *   encoding and transmitting practical survival knowledge across
 *   generations, particularly in contexts of recurring catastrophe or
 *   environmental instability. It is a specific reading of the broader
 *   'catastrophe_memory_survival' kernel, focusing on the tangible, adaptive
 *   competence conveyed through ritual practice. The constraint is claimed as
 *   a Tangled Rope because it genuinely coordinates vital knowledge
 *   transmission but involves moderate extraction in terms of adherence costs
 *   and potential loss of explicit content over time.
 *
 * KEY AGENTS:
 *   - diaspora_communities: Primary beneficiary (organized/constrained) — gains adaptive capacity.
 *   - future_generations: Ultimate beneficiary (powerless/trapped) — receives vital knowledge.
 *   - communities_losing_content_while_maintaining_form: Primary payer (moderate/constrained) — bears costs without full benefit.
 *   - individuals_burdened_by_ritual_adherence: Secondary payer (powerless/identity_locked) — bears social costs.
 *   - ritual_elders_and_keepers: Agenda setter (institutional/identity_locked) — enforces and transmits ritual.
 *   - anthropologists_and_scholars: Analytical observer (analytical/analytical) — studies the phenomenon.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.45).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.3).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Ritual as Catastrophe Memory: Competence Transmission Reading").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__competence_transmission_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, 'b15fd703-f8eb-4c19-91a3-0769b4cc3318').
narrative_ontology:cs_kernel_codification('b15fd703-f8eb-4c19-91a3-0769b4cc3318', implicit).
narrative_ontology:cs_authority_grounding('b15fd703-f8eb-4c19-91a3-0769b4cc3318', practice).
narrative_ontology:cs_interpretation_layer_present('b15fd703-f8eb-4c19-91a3-0769b4cc3318').
narrative_ontology:cs_reading_relation('b15fd703-f8eb-4c19-91a3-0769b4cc3318', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('b15fd703-f8eb-4c19-91a3-0769b4cc3318', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('b15fd703-f8eb-4c19-91a3-0769b4cc3318', foundational, ritual_encodes_adaptive_strategies).
narrative_ontology:cs_axiom_status(ritual_encodes_adaptive_strategies, holdable).
narrative_ontology:cs_axiom_grounding('b15fd703-f8eb-4c19-91a3-0769b4cc3318', ritual_encodes_adaptive_strategies, empirically_contingent).
narrative_ontology:cs_axiom('b15fd703-f8eb-4c19-91a3-0769b4cc3318', secondary, intergenerational_transmission_is_vital).
narrative_ontology:cs_axiom_status(intergenerational_transmission_is_vital, holdable).
narrative_ontology:cs_axiom_grounding('b15fd703-f8eb-4c19-91a3-0769b4cc3318', intergenerational_transmission_is_vital, instrumental).
narrative_ontology:cs_reference_frame('b15fd703-f8eb-4c19-91a3-0769b4cc3318', ancestral_adaptive_practice).
narrative_ontology:cs_drift_state('b15fd703-f8eb-4c19-91a3-0769b4cc3318', contemporary_globalized_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b15fd703-f8eb-4c19-91a3-0769b4cc3318', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, future_generations).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, communities_losing_content_while_maintaining_form).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, individuals_burdened_by_ritual_adherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities rely on ritual to maintain cultural continuity and transmit adaptive knowledge, especially when displaced from their original ecological contexts. They benefit from the resilience and practical guidance encoded in the practices.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities, beneficiary,
    organized, generational, constrained, global).

% They are the ultimate recipients of the intergenerationally transmitted survival knowledge, gaining adaptive capacity and resilience against future catastrophes. Their 'exit' is the loss of this knowledge, leaving them more vulnerable.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, future_generations, beneficiary,
    powerless, generational, trapped, universal).

% These communities continue to perform the rituals, bearing the social and temporal costs of adherence, but have lost the explicit understanding of the practical survival knowledge originally encoded. They pay the 'tax' of ritual without receiving the full 'dividend' of competence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, communities_losing_content_while_maintaining_form, payer,
    moderate, biographical, constrained, local).

% Individuals within the community who feel the pressure to conform to ritual practices, even if they find them onerous or their practical utility unclear. Their identity is often deeply intertwined with community participation, making exit difficult.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, individuals_burdened_by_ritual_adherence, payer,
    powerless, immediate, identity_locked, local).

% These individuals are responsible for the correct performance, interpretation, and transmission of the rituals. They actively enforce adherence to ensure the continuity of the practice, believing it vital for community survival and cultural integrity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, ritual_elders_and_keepers, agenda_setter,
    institutional, generational, identity_locked, local).

% They study the structure and function of these rituals, analyzing their role in collective memory and knowledge transmission. They do not directly participate in the constraint but provide external analysis and interpretation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, anthropologists_and_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__competence_transmission_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the encoding, storage, and intergenerational transmission of critical survival knowledge (e.g., timing for planting, resource management, disaster response protocols), ensuring community resilience against recurring threats.
% TRANSFER_FUNCTION: Transfers practical knowledge and adaptive strategies from past generations to present and future ones, often through mnemonic devices, embodied practice, and narrative, ensuring the community's long-term viability.
% ABSENT_VOICES: Those who reject ritual as superstitious or inefficient, advocating for more direct, empirically-driven knowledge transfer methods. They are often marginalized in communities where ritual holds deep cultural authority and is seen as integral to identity and survival.
% DISAPPEARANCE_RATIONALE: If rituals encoding survival knowledge vanished, communities would lose a vital, robust, and culturally embedded mechanism for intergenerational learning. This would make them significantly more vulnerable to environmental shifts, resource scarcity, or recurring catastrophes, and would also unravel significant aspects of their social and cultural fabric.
% FOUNDING_PROBLEM: Communities faced recurring existential threats (famine, flood, conflict) and needed a reliable, resilient, and culturally resonant method to store and transmit complex adaptive strategies across generations, especially in pre-literate or disrupted societies.
% FOUNDING_PROBLEM_CORROBORATION: Ethnographic studies, historical records of disaster response, and oral histories from community members (not just ritual leaders) corroborate the role of ritual in practical knowledge transmission and ongoing community resilience. Many communities continue to face similar challenges, making the knowledge still relevant.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__competence_transmission_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__competence_transmission_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_survival__competence_transmission_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__competence_transmission_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__competence_transmission_reading_tests).
:- end_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.45) reflects the effort required for intergenerational learning and adherence, as well as the potential for the practical content to become obscured or lost while the ritual form persists. Suppression (0.3) is present through social pressure and community expectations, but it's generally not overtly coercive. The theater ratio (0.2) is low because the performative aspects of ritual are integral to its mnemonic and transmission functions, not a substitute for them. Accessibility collapse (0.4) is moderate; while other knowledge transmission methods exist, ritual offers a unique, robust, and culturally embedded channel. Resistance (0.2) is low because the perceived survival value of these practices often outweighs individual reluctance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of diaspora communities and future generations, the constraint is a vital Rope, providing essential adaptive capacity. However, for individuals burdened by adherence or communities where the practical content has atrophied, it can feel more extractive, resembling a Snare or a Piton, as they bear the costs without fully realizing the benefits. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities and future generations are beneficiaries (low d) as they gain critical adaptive knowledge. Communities losing content and individuals burdened by adherence are payers/victims (higher d) as they bear the costs of ritual maintenance without full practical benefit. Ritual elders are agenda setters (low d) as they maintain the system and its perceived benefits. Anthropologists are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (transmitting survival knowledge) is still live for many communities, preventing it from being a Piton. However, the 'communities_losing_content_while_maintaining_form' stakeholder highlights a potential for mandatrophy where the form persists but the function atrophies, leading to increased extraction for those who adhere without understanding the original purpose. The 'contested' status of the founding problem further emphasizes this dynamic, where the original problem may be solved or changed, but the ritual persists due to inertia or other functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_form_persistence,
    'To what extent is the practical survival knowledge still actively understood and applied, versus only the ritual form being maintained?',
    'Ethnographic studies assessing explicit knowledge of ritual purpose, direct observation of adaptive behaviors linked to ritual, and community-led knowledge audits.',
    'If practical content is largely lost, the constraint''s effective extractiveness for ''communities_losing_content_while_maintaining_form'' is higher, pushing it closer to a Snare for that seat. If content is robust, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_form_persistence, empirical, 'Ambiguity regarding the active transmission of practical content versus mere formal adherence.').

omega_variable(
    kernel_reading_focus,
    'Is this constraint primarily about competence transmission, or are other functions (e.g., symbolic identity, social cohesion) equally or more dominant?',
    'Comparative analysis with sibling readings (''symbol_survival_reading'', ''hybrid_encoding_reading'') to identify the dominant function based on community self-description and observed outcomes.',
    'If symbolic or hybrid functions are dominant, this reading''s extractiveness and beneficiary structure might be miscalibrated, requiring re-evaluation or decomposition into a different primary constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_focus, conceptual, 'This constraint is the ''competence_transmission_reading'' of the ''catastrophe_memory_survival'' kernel, focusing on practical knowledge. Sibling readings emphasize symbolic or hybrid functions.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (social pressure to conform) structural (external community norms) or internalized (individual identity fused with ritual adherence)?',
    'Post-exit suppression trajectory: if individuals who leave the community still feel internal pressure or guilt, it suggests internalized suppression. If pressure ceases upon physical exit, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression for ''individuals_burdened_by_ritual_adherence'' is higher than the structural measure suggests, as they carry the suppression with them after any nominal ''exit''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for ritual adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t400, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 400, 0.17).
narrative_ontology:measurement(cata_tr_t800, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 800, 0.18).
narrative_ontology:measurement(cata_tr_t1200, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 1200, 0.19).
narrative_ontology:measurement(cata_tr_t1600, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 1600, 0.2).
narrative_ontology:measurement(cata_tr_t2000, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 2000, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cata_be_t400, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 400, 0.42).
narrative_ontology:measurement(cata_be_t800, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 800, 0.43).
narrative_ontology:measurement(cata_be_t1200, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 1200, 0.44).
narrative_ontology:measurement(cata_be_t1600, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 1600, 0.45).
narrative_ontology:measurement(cata_be_t2000, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 2000, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cata_su_t400, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 400, 0.27).
narrative_ontology:measurement(cata_su_t800, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 800, 0.28).
narrative_ontology:measurement(cata_su_t1200, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 1200, 0.29).
narrative_ontology:measurement(cata_su_t1600, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 1600, 0.3).
narrative_ontology:measurement(cata_su_t2000, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 2000, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__competence_transmission_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__hybrid_encoding_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
