% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__universal_rights_reading, []).

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
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Magna Carta (1215) as Universal Due Process Precedent
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint is the 'universal_rights_reading' of the
 *   'magna_carta_1215' kernel. It interprets Magna Carta, particularly Clause
 *   39 ('No free man shall be seized or imprisoned... except by the lawful
 *   judgment of his equals or by the law of the land'), as establishing a
 *   transhistorical precedent for universal due process rights, applying to
 *   all persons and constraining all state power. This contrasts with the
 *   'baronial_privilege_reading' (which limits its scope to feudal lords) and
 *   the 'living_document_reading' (which sees its meaning as evolving beyond
 *   original intent, but without necessarily anchoring in a foundational
 *   universal principle). The claimed type 'rope' reflects its function as a
 *   foundational coordination mechanism for lawful governance and individual
 *   rights, despite the historical and ongoing contestation over its
 *   universal application.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.15).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.1).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Magna Carta (1215) as Universal Due Process Precedent").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, 'b38c66ac-744c-431d-b18b-46a9957d45e2').
narrative_ontology:cs_kernel_codification('b38c66ac-744c-431d-b18b-46a9957d45e2', fixed_text).
narrative_ontology:cs_authority_grounding('b38c66ac-744c-431d-b18b-46a9957d45e2', lineage).
narrative_ontology:cs_interpretation_layer_present('b38c66ac-744c-431d-b18b-46a9957d45e2').
narrative_ontology:cs_reading_relation('b38c66ac-744c-431d-b18b-46a9957d45e2', magna_carta_1215__baronial_privilege_reading, forecloses).
narrative_ontology:cs_reading_relation('b38c66ac-744c-431d-b18b-46a9957d45e2', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('b38c66ac-744c-431d-b18b-46a9957d45e2', foundational, universal_personhood_rights).
narrative_ontology:cs_axiom_status(universal_personhood_rights, holdable).
narrative_ontology:cs_axiom_grounding('b38c66ac-744c-431d-b18b-46a9957d45e2', universal_personhood_rights, deontological).
narrative_ontology:cs_axiom('b38c66ac-744c-431d-b18b-46a9957d45e2', foundational, state_power_subordinate_to_law).
narrative_ontology:cs_axiom_status(state_power_subordinate_to_law, holdable).
narrative_ontology:cs_axiom_grounding('b38c66ac-744c-431d-b18b-46a9957d45e2', state_power_subordinate_to_law, deontological).
narrative_ontology:cs_reference_frame('b38c66ac-744c-431d-b18b-46a9957d45e2', foundational_due_process_principle).
narrative_ontology:cs_drift_state('b38c66ac-744c-431d-b18b-46a9957d45e2', contemporary_human_rights_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b38c66ac-744c-431d-b18b-46a9957d45e2', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, all_persons).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, human_rights_advocates).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, arbitrary_state_power).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, unjust_rulers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies the principles of due process derived from Magna Carta, expanding its scope over centuries to cover all persons and constrain state power. Actively defends the precedent against executive overreach.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Receives the protection of universal due process, safeguarding against arbitrary detention, seizure of property, and extrajudicial punishment. Their benefit is the absence of arbitrary state power.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, all_persons, beneficiary,
    powerless, biographical, trapped, universal).

% Actively champion and defend the universal application of due process, using Magna Carta as a foundational precedent in legal and political discourse. They work to expand its reach and ensure its enforcement.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Is constrained by the requirement for due process, limiting its ability to act without legal justification. Bears the cost of adhering to legal procedures and respecting individual rights.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, arbitrary_state_power, payer,
    institutional, immediate, constrained, national).

% Are directly challenged and limited by the universal due process constraint, preventing them from exercising unchecked authority over their subjects. Their power to act arbitrarily is curtailed.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, unjust_rulers, payer,
    powerful, immediate, constrained, national).

% Analyze the historical context, evolution, and interpretation of Magna Carta, documenting how its principles have been invoked and expanded over time to support universal rights.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, legal_historians, observer,
    analytical, generational, analytical, global).

% Adhere to a strict interpretation of Magna Carta's original meaning, limiting its application to the specific historical context of 1215 and the 'free men' of that era. Their narrower view is actively resisted by the universal rights reading.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, originalists, excluded,
    organized, generational, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__universal_rights_reading, diffuse).
narrative_ontology:fixing_cost_class(magna_carta_1215__universal_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state power by establishing a baseline of individual rights and due process, ensuring predictable and lawful governance for all persons, thereby fostering social order and trust in legal institutions.
% TRANSFER_FUNCTION: Transfers the power to arbitrarily detain, seize property, or punish from the state to a lawful, procedurally just process, thereby protecting individual liberty and security for all.
% ABSENT_VOICES: Historically, those excluded from the definition of 'free men' (e.g., serfs, women, non-landowners, later non-citizens) whose rights were not recognized under narrower interpretations. In contemporary discourse, originalists who argue against the universal application are excluded from this reading's interpretive framework.
% DISAPPEARANCE_RATIONALE: If the principle of universal due process derived from Magna Carta vanished, state power would become arbitrary, individual liberties would be severely curtailed, and the entire legal framework of many nations would collapse, leading to widespread social and political upheaval. The foundational concept of lawful governance would be lost.
% FOUNDING_PROBLEM: Arbitrary rule by the monarch (King John), leading to unjust imprisonment, seizure of property, and extrajudicial punishment of subjects, particularly the powerful barons.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, constitutional scholars, and legal precedents from numerous jurisdictions corroborate the ongoing relevance of due process against arbitrary power, extending far beyond the original baronial context. Legislative hearings and independent legal analyses consistently affirm the need for such constraints.
narrative_ontology:disappearance_verdict(magna_carta_1215__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__universal_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(magna_carta_1215__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__universal_rights_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__universal_rights_reading_tests).
:- end_tests(magna_carta_1215__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because the constraint's purpose is to prevent arbitrary state extraction from individuals. However, it is not zero due to the historical reality that its protections were not universally applied for centuries, and its universal application still requires active defense. Suppression is low (0.10) as it aims to counter state suppression, but again, not zero as its principles must be actively enforced and defended. Theater ratio is very low (0.05) because Magna Carta is a genuinely invoked and debated legal precedent, not a performative relic. Accessibility collapse is high (0.85) as it aims to eliminate alternatives to due process for state action. Resistance is moderate (0.40) reflecting the ongoing struggle to ensure its universal and consistent application against state overreach.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'all_persons' and 'human_rights_advocates', the constraint is a vital protection and a tool for justice. From the perspective of 'arbitrary_state_power' and 'unjust_rulers', it is a burdensome limitation on their authority. The engine computes this divergence from the structural data, showing how the same legal text can be experienced as a fundamental right by some and an impediment by others.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary, all persons, and human rights advocates are beneficiaries, as they either enforce or directly benefit from the constraint's protection against arbitrary power. Arbitrary state power and unjust rulers are the payers, as their capacity for unchecked action is curtailed by the due process requirement. Legal historians act as observers, analyzing its evolution. Originalists are excluded from this reading's interpretive framework, as their narrow view is incompatible with the universal application asserted here.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_free_men_interpretation,
    'Is the interpretation of ''free men'' in Magna Carta''s Clause 39 as ''all persons'' a legitimate historical extension or a re-reading that fundamentally alters the original intent?',
    'Further historical and jurisprudential analysis of the evolution of legal personhood and rights in common law, examining how and when the concept of ''free men'' expanded beyond its feudal origins.',
    'If deemed a fundamental alteration, it might weaken the historical grounding of the ''universal_rights_reading'', potentially reclassifying it as a ''living_document_reading'' or a ''scaffold'' built upon the original text. If seen as a legitimate, albeit gradual, extension, it reinforces the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_free_men_interpretation, conceptual, 'Ambiguity regarding the historical vs. universal interpretation of ''free men''.').

omega_variable(
    foundational_vs_adaptive_precedent,
    'Is Magna Carta''s Clause 39 a static, foundational principle of universal due process, or is its meaning primarily adaptive and subject to legitimate reinterpretation over time?',
    'Analysis of judicial decisions and legislative acts that invoke Magna Carta: do they consistently refer to a core, unchanging principle, or do they primarily adapt its meaning to new social and legal contexts?',
    'If primarily static, it reinforces the ''rope'' classification as a stable coordination mechanism. If primarily adaptive, it might shift the classification closer to a ''living_document_reading'', emphasizing its dynamic nature over its foundational claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_vs_adaptive_precedent, conceptual, 'Contest over Magna Carta as a static text vs. a living document.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 0, 809).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_1215__universal_rights_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(magn_tr_t160, magna_carta_1215__universal_rights_reading, theater_ratio, 160, 0.05).
narrative_ontology:measurement(magn_tr_t320, magna_carta_1215__universal_rights_reading, theater_ratio, 320, 0.05).
narrative_ontology:measurement(magn_tr_t480, magna_carta_1215__universal_rights_reading, theater_ratio, 480, 0.05).
narrative_ontology:measurement(magn_tr_t640, magna_carta_1215__universal_rights_reading, theater_ratio, 640, 0.05).
narrative_ontology:measurement(magn_tr_t809, magna_carta_1215__universal_rights_reading, theater_ratio, 809, 0.05).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_1215__universal_rights_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(magn_be_t160, magna_carta_1215__universal_rights_reading, base_extractiveness, 160, 0.3).
narrative_ontology:measurement(magn_be_t320, magna_carta_1215__universal_rights_reading, base_extractiveness, 320, 0.25).
narrative_ontology:measurement(magn_be_t480, magna_carta_1215__universal_rights_reading, base_extractiveness, 480, 0.2).
narrative_ontology:measurement(magn_be_t640, magna_carta_1215__universal_rights_reading, base_extractiveness, 640, 0.18).
narrative_ontology:measurement(magn_be_t809, magna_carta_1215__universal_rights_reading, base_extractiveness, 809, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_1215__universal_rights_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(magn_su_t160, magna_carta_1215__universal_rights_reading, suppression_requirement, 160, 0.4).
narrative_ontology:measurement(magn_su_t320, magna_carta_1215__universal_rights_reading, suppression_requirement, 320, 0.3).
narrative_ontology:measurement(magn_su_t480, magna_carta_1215__universal_rights_reading, suppression_requirement, 480, 0.2).
narrative_ontology:measurement(magn_su_t640, magna_carta_1215__universal_rights_reading, suppression_requirement, 640, 0.15).
narrative_ontology:measurement(magn_su_t809, magna_carta_1215__universal_rights_reading, suppression_requirement, 809, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
