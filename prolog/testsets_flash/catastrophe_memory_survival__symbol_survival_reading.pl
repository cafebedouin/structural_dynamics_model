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
 *   This constraint describes the role of ritual in preserving Jewish
 *   identity and boundary-norms, particularly in the aftermath of
 *   catastrophe, through the continuity of symbolic practice. It emphasizes
 *   that survival is achieved by maintaining the ritual form itself, rather
 *   than through the transmission of practical knowledge or adaptive
 *   strategies. This reading implies a high cost for those who deviate from
 *   prescribed forms, as they are seen as losing their connection to the
 *   collective identity.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: Agenda setter (institutional/generational) — defines and enforces ritual norms.
 *   - orthodox_communities: Beneficiary (organized/generational) — maintain identity and cohesion through strict adherence to ritual.
 *   - secularized_jews: Payer (moderate/biographical) — bear the cost of exclusion or alienation from traditional identity due to non-adherence.
 *   - reform_movements: Payer (organized/biographical) — challenge the rigidity of symbolic forms, often at the cost of being seen as deviating from 'authentic' tradition.
 *   - anthropologists_of_religion: Observer (analytical/civilizational) — analyze the structural role of ritual in cultural survival.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.7).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.6).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Catastrophe Memory Survival: Symbolic Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, '6e04588f-1ace-4b23-acfc-f826234dcd65').
narrative_ontology:cs_kernel_codification('6e04588f-1ace-4b23-acfc-f826234dcd65', formalized).
narrative_ontology:cs_authority_grounding('6e04588f-1ace-4b23-acfc-f826234dcd65', lineage).
narrative_ontology:cs_interpretation_layer_present('6e04588f-1ace-4b23-acfc-f826234dcd65').
narrative_ontology:cs_reading_relation('6e04588f-1ace-4b23-acfc-f826234dcd65', catastrophe_memory_survival__competence_transmission_reading, influences).
narrative_ontology:cs_reading_relation('6e04588f-1ace-4b23-acfc-f826234dcd65', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('6e04588f-1ace-4b23-acfc-f826234dcd65', foundational, symbolic_continuity_is_survival).
narrative_ontology:cs_axiom_status(symbolic_continuity_is_survival, holdable).
narrative_ontology:cs_axiom_grounding('6e04588f-1ace-4b23-acfc-f826234dcd65', symbolic_continuity_is_survival, deontological).
narrative_ontology:cs_axiom('6e04588f-1ace-4b23-acfc-f826234dcd65', secondary, deviation_erodes_identity).
narrative_ontology:cs_axiom_status(deviation_erodes_identity, holdable).
narrative_ontology:cs_axiom_grounding('6e04588f-1ace-4b23-acfc-f826234dcd65', deviation_erodes_identity, conventional).
narrative_ontology:cs_reference_frame('6e04588f-1ace-4b23-acfc-f826234dcd65', unbroken_ritual_chain).
narrative_ontology:cs_drift_state('6e04588f-1ace-4b23-acfc-f826234dcd65', post_holocaust_diaspora, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6e04588f-1ace-4b23-acfc-f826234dcd65', '').
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

% Defines, interprets, and enforces the ritual norms and symbolic practices deemed essential for Jewish identity and continuity. Their authority is grounded in the preservation of tradition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Derive strong collective identity, social cohesion, and a sense of historical continuity through strict adherence to traditional symbolic rituals. They benefit from the clarity and stability of prescribed forms.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, orthodox_communities, beneficiary,
    organized, generational, identity_locked, global).

% Experience alienation or a sense of loss of connection to their heritage if they cannot or choose not to adhere to traditional symbolic rituals. They bear the cost of exclusion from traditional identity frameworks.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secularized_jews, payer,
    moderate, biographical, constrained, national).

% Seek to adapt or reinterpret traditional symbolic rituals to align with modern values, often facing criticism or delegitimization from more traditional authorities. They pay the cost of being seen as deviating from 'authentic' tradition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, reform_movements, payer,
    organized, biographical, constrained, national).

% Study the structural role of ritual in cultural and identity preservation across different societies, analyzing the mechanisms and consequences of symbolic continuity without direct participation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, anthropologists_of_religion, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__symbol_survival_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective identity and boundary-maintenance for a dispersed people across generations, providing a shared symbolic language and practice that reinforces group cohesion and historical continuity.
% TRANSFER_FUNCTION: Transfers a sense of belonging, historical memory, and collective identity to adherents, in exchange for their adherence to prescribed symbolic ritual forms. It also transfers authority and legitimacy to those who administer these forms.
% ABSENT_VOICES: Those who advocate for purely secular or individualistic forms of identity preservation, or those who prioritize practical adaptation over symbolic continuity, are often marginalized or excluded from the discourse on collective survival. They would argue that rigid symbolic forms hinder adaptation.
% DISAPPEARANCE_RATIONALE: If the emphasis on symbolic ritual continuity vanished, the primary mechanism for collective identity preservation, particularly after catastrophe, would be lost. Communities would fragment, shared memory would dissipate, and the sense of a continuous historical people would be severely challenged, leading to a profound reorganization of Jewish collective life.
% FOUNDING_PROBLEM: The existential threat of cultural and physical annihilation, particularly after the Holocaust, and the challenge of maintaining a distinct identity in a secularizing world.
% FOUNDING_PROBLEM_CORROBORATION: Historians, sociologists of religion, and community leaders (including those outside orthodox circles) corroborate that the challenges of identity preservation and memory transmission remain live, even if the specific mechanisms and their efficacy are debated. The need for collective memory after catastrophe is widely acknowledged.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__symbol_survival_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__symbol_survival_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__symbol_survival_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_survival__symbol_survival_reading, 'none', 1).

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
 *   The extractiveness (0.7) is high because the emphasis on symbolic form can demand significant adherence costs, including social exclusion or loss of identity for those who cannot or will not conform. Suppression (0.6) is moderate, as social pressure and theological doctrines actively discourage deviation. Theater ratio (0.4) reflects that while the symbolic function is real, some adherence may become performative, especially when the underlying meaning is lost or contested. Accessibility collapse (0.7) is high because alternative paths to identity preservation are often framed as less legitimate or even dangerous. Resistance (0.3) is moderate, as there are ongoing internal debates and external challenges to the authority of traditional ritual forms.
 *
 * PERSPECTIVAL GAP:
 *   Rabbinic authority and orthodox communities experience this as a vital Rope or even Mountain, essential for collective survival. Secularized Jews and Reform movements experience it as a Snare or Tangled Rope, extracting conformity and alienating those who seek alternative expressions of identity. The engine's classification will reflect this divergence based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority and orthodox communities are beneficiaries (d near 0.0) as they gain legitimacy, cohesion, and continuity from the constraint's operation. Secularized Jews and Reform movements are targets (d near 1.0) as they bear the costs of exclusion or the pressure to conform. Anthropologists of religion are analytical observers (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to preserve identity through symbolic continuity. Mandatrophy would occur if the symbolic forms were maintained without actually fostering identity or meaning for participants. The 'contested' status of the founding problem (see six_questions) suggests an ongoing debate about whether the constraint's function has atrophied for some groups while remaining live for others. The classification as Tangled Rope reflects this hybrid nature, where a genuine coordination function (identity preservation) is intertwined with asymmetric extraction (cost of non-conformity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint primarily about symbolic survival, or does it also encode practical knowledge?',
    'Empirical study of ritual transmission content: if practical survival skills are demonstrably embedded and transmitted, reclassify as hybrid_encoding_reading.',
    'If the constraint is solely symbolic, its extractiveness from those who lose practical skills is higher; if practical skills are also transmitted, the coordination function is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''symbol_survival_reading'' of the ''catastrophe_memory_survival'' kernel. Sibling readings include ''competence_transmission_reading'' (ritual transmits practical knowledge) and ''hybrid_encoding_reading'' (ritual does both). This reading emphasizes symbolic continuity over practical utility, leading to higher perceived extraction for those who prioritize practical outcomes.').

omega_variable(
    mandatrophy_of_symbolic_form,
    'Has the symbolic form of ritual become an end in itself, detached from its original purpose of identity preservation?',
    'Sociological analysis of community engagement and self-reported meaning: if participants derive little meaning beyond formal adherence, mandatrophy is present.',
    'If mandatrophy is resolved, the constraint''s theater_ratio would increase, potentially reclassifying it as a piton for some participants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_symbolic_form, empirical, 'Assesses whether the emphasis on symbolic form has outlived its functional mandate for identity preservation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1945, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1945, 0.3).
narrative_ontology:measurement(cata_tr_t1965, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1965, 0.35).
narrative_ontology:measurement(cata_tr_t1985, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1985, 0.4).
narrative_ontology:measurement(cata_tr_t2005, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 2005, 0.42).
narrative_ontology:measurement(cata_tr_t2024, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t1945, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1945, 0.6).
narrative_ontology:measurement(cata_be_t1965, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1965, 0.65).
narrative_ontology:measurement(cata_be_t1985, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(cata_be_t2005, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 2005, 0.72).
narrative_ontology:measurement(cata_be_t2024, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1945, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(cata_su_t1965, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement(cata_su_t1985, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(cata_su_t2005, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement(cata_su_t2024, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__symbol_survival_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_survival' kernel, focusing on symbolic continuity. It is linked to sibling readings that emphasize practical knowledge transmission or a hybrid approach, as these readings offer competing interpretations of how ritual ensures collective survival.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
