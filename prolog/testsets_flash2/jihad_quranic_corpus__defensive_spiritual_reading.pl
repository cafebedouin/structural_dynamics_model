% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__defensive_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__defensive_spiritual_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__defensive_spiritual_reading
 *   human_readable: Jihad as Defensive and Spiritual Struggle (Quranic Corpus Reading)
 *   domain: islamic_jurisprudence/political_theology
 *
 * SUMMARY:
 *   This constraint represents a specific reading of 'jihad' within the
 *   Quranic corpus, emphasizing internal spiritual struggle (jihad al-nafs)
 *   and defensive armed response, strictly limited by proportionality and
 *   non-combatant immunity. This reading is prevalent among mainstream
 *   Islamic scholars and institutions. It is one reading of the
 *   'jihad_quranic_corpus' kernel, distinct from more expansionist or
 *   revolutionary interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.15).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.2).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Jihad as Defensive and Spiritual Struggle (Quranic Corpus Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "islamic_jurisprudence/political_theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, '884bc3b9-02d8-44ba-ae13-f461e72346e2').
narrative_ontology:cs_kernel_codification('884bc3b9-02d8-44ba-ae13-f461e72346e2', fixed_text).
narrative_ontology:cs_authority_grounding('884bc3b9-02d8-44ba-ae13-f461e72346e2', lineage).
narrative_ontology:cs_interpretation_layer_present('884bc3b9-02d8-44ba-ae13-f461e72346e2').
narrative_ontology:cs_reading_relation('884bc3b9-02d8-44ba-ae13-f461e72346e2', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('884bc3b9-02d8-44ba-ae13-f461e72346e2', jihad_quranic_corpus__revolutionary_vanguard_reading, coexists_with).
narrative_ontology:cs_axiom('884bc3b9-02d8-44ba-ae13-f461e72346e2', foundational, jihad_primarily_spiritual_struggle).
narrative_ontology:cs_axiom_status(jihad_primarily_spiritual_struggle, holdable).
narrative_ontology:cs_axiom_grounding('884bc3b9-02d8-44ba-ae13-f461e72346e2', jihad_primarily_spiritual_struggle, deontological).
narrative_ontology:cs_axiom('884bc3b9-02d8-44ba-ae13-f461e72346e2', foundational, armed_jihad_defensive_only).
narrative_ontology:cs_axiom_status(armed_jihad_defensive_only, holdable).
narrative_ontology:cs_axiom_grounding('884bc3b9-02d8-44ba-ae13-f461e72346e2', armed_jihad_defensive_only, conventional).
narrative_ontology:cs_reference_frame('884bc3b9-02d8-44ba-ae13-f461e72346e2', early_islamic_community_defense).
narrative_ontology:cs_drift_state('884bc3b9-02d8-44ba-ae13-f461e72346e2', contemporary_global_context, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('884bc3b9-02d8-44ba-ae13-f461e72346e2', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_community).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, non_combatants).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, state_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, individual_muslims).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, aggressive_external_forces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a framework that prioritizes internal spiritual development and limits armed conflict to legitimate defense, protecting its members from unnecessary aggression and internal strife. Identity-locked to the religious framework.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_community, beneficiary,
    organized, generational, identity_locked, global).

% Protected by strict rules of proportionality and immunity, ensuring their safety during any legitimate defensive armed response. Their protection is a core tenet of this reading.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, non_combatants, beneficiary,
    powerless, immediate, trapped, local).

% Holds the exclusive right to declare and conduct armed jihad, ensuring it is a measured, state-sanctioned defensive act rather than individual vigilantism. This centralizes authority and prevents chaos.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, state_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Primarily obligated to engage in spiritual struggle (jihad al-nafs), which requires continuous self-improvement and moral discipline. Bears the personal cost of this internal effort, but is largely shielded from the burden of armed conflict unless directly threatened.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, individual_muslims, payer,
    moderate, biographical, identity_locked, local).

% Faces legitimate defensive armed response if they initiate aggression against Muslim lands or people. This reading constrains their ability to act with impunity, imposing a cost on their aggression.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, aggressive_external_forces, payer,
    powerful, immediate, constrained, regional).

% Excluded from legitimate interpretation and action under this reading, as their calls for offensive or unauthorized armed struggle violate its core principles. They are actively resisted by adherents of this reading.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, radical_groups, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Muslim community's understanding and practice of 'jihad' by prioritizing internal spiritual struggle and limiting armed conflict to legitimate, state-sanctioned defense, thereby preventing unauthorized violence and promoting internal cohesion.
% TRANSFER_FUNCTION: Transfers the primary burden of 'jihad' from external armed conflict to internal spiritual discipline for individuals, and from individual initiative to state authority for defensive armed response. It transfers protection to non-combatants and stability to the Muslim community.
% ABSENT_VOICES: Radical groups and proponents of expansionist interpretations are excluded; they would argue for a broader, more aggressive application of armed jihad, bypassing state authority and potentially targeting non-Muslims indiscriminately. Their voices are actively suppressed by the interpretive tradition of this reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the interpretation of 'jihad' would likely fragment, leading to increased unauthorized violence, internal strife within Muslim communities, and a loss of protection for non-combatants. The framework for legitimate defense would collapse, and more aggressive interpretations might gain prominence, fundamentally altering the global religious and political landscape.
% FOUNDING_PROBLEM: The early Muslim community faced both internal moral challenges and external aggression, requiring a framework to guide both individual spiritual development and collective defense without falling into indiscriminate violence or internal chaos.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream Islamic scholars and institutions globally corroborate that the founding problems of internal moral struggle and the need for legitimate defense remain live. International legal frameworks and human rights organizations also implicitly corroborate the need for constraints on armed conflict, aligning with this reading's emphasis on proportionality and non-combatant immunity.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__defensive_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__defensive_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jihad_quranic_corpus__defensive_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).
:- end_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.15) is low because this reading primarily coordinates internal moral effort and legitimate defense, with minimal coercive overhead. Suppression (0.2) is also low, reflecting the internal nature of spiritual struggle and the high bar for legitimate armed response, which requires state authority. Theater ratio (0.05) is minimal, as the emphasis is on genuine spiritual and defensive action rather than performative displays. Accessibility collapse (0.7) is relatively high because this reading significantly narrows the scope of legitimate armed action, making other interpretations less accessible within its framework. Resistance (0.1) is low from within the community, as this reading is widely accepted by mainstream adherents, though it faces external resistance from radical groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of mainstream adherents and state authorities, this reading functions as a vital rope, coordinating defense and spiritual growth. From the perspective of radical groups, it is a suppressive constraint that prevents what they see as a religious obligation. The engine's classification will reflect the internal coherence of this reading, while omegas address the contestation.
 *
 * DIRECTIONALITY LOGIC:
 *   The Muslim community and non-combatants are clear beneficiaries, gaining protection and spiritual guidance. State authorities are agenda-setters, controlling the legitimate application of armed defense. Individual Muslims are payers in terms of spiritual effort, but beneficiaries in terms of protection. Aggressive external forces are targets of legitimate defense. Radical groups are excluded, as their interpretations are actively rejected by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretation_legitimacy_contest,
    'Is this defensive_spiritual_reading the most historically and textually legitimate interpretation of jihad, or do other readings hold comparable textual grounding?',
    'Comprehensive historical-critical analysis of early Islamic texts and jurisprudential development, assessing the evolution and contestation of ''jihad'' interpretations across different eras and schools of thought.',
    'If other readings are found to have equally strong textual grounding, the ''rope'' classification of this reading would be challenged by the existence of structurally distinct, equally legitimate (within their own frameworks) but more extractive interpretations. This would shift the kernel''s overall classification towards ''tangled_rope'' or ''snare'' due to the inherent contestation and potential for extraction across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretation_legitimacy_contest, conceptual, 'Contestation over the primary textual and historical legitimacy of this specific reading of jihad.').

omega_variable(
    state_authority_legitimacy,
    'Is the requirement for state authority to declare armed jihad universally accepted within the Muslim community, or do significant factions dispute this centralization of power?',
    'Sociological and political analysis of contemporary Muslim communities and movements, identifying the prevalence and influence of groups that advocate for non-state or individual declarations of armed jihad.',
    'If state authority is widely disputed, the ''agenda_setter'' role of state authorities would be weakened, and the constraint''s ability to prevent unauthorized violence would be compromised. This would increase the effective extractiveness and suppression for individual Muslims who might feel compelled to act outside state authority, potentially shifting the classification towards a ''tangled_rope'' or ''snare'' due to internal fragmentation and coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_authority_legitimacy, empirical, 'The extent to which state authority is accepted as the sole legitimate arbiter of armed jihad.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(jiha_tr_t350, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 350, 0.03).
narrative_ontology:measurement(jiha_tr_t700, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 700, 0.05).
narrative_ontology:measurement(jiha_tr_t1050, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1050, 0.04).
narrative_ontology:measurement(jiha_tr_t1400, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1400, 0.05).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(jiha_be_t350, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 350, 0.12).
narrative_ontology:measurement(jiha_be_t700, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 700, 0.15).
narrative_ontology:measurement(jiha_be_t1050, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1050, 0.14).
narrative_ontology:measurement(jiha_be_t1400, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1400, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(jiha_su_t350, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 350, 0.18).
narrative_ontology:measurement(jiha_su_t700, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 700, 0.2).
narrative_ontology:measurement(jiha_su_t1050, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1050, 0.19).
narrative_ontology:measurement(jiha_su_t1400, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1400, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jihad_quranic_corpus' kernel, focusing on defensive and spiritual aspects. It is structurally distinct from expansionist or revolutionary readings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
