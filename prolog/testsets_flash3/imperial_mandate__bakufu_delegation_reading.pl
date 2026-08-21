% ============================================================================
% CONSTRAINT STORY: imperial_mandate__bakufu_delegation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__bakufu_delegation_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: imperial_mandate__bakufu_delegation_reading
 *   human_readable: Imperial Mandate: Bakufu Delegation Reading
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   This constraint describes the 'Bakufu Delegation' reading of the Imperial
 *   Mandate in pre-Meiji Japan, where the Emperor's divine authority was
 *   understood to be delegated to the Shogun for practical governance. This
 *   reading enabled a stable, bifurcated sovereignty but suppressed direct
 *   imperial political involvement. The constraint is framed as a Tangled
 *   Rope, acknowledging both its coordination function (political stability)
 *   and its extractive nature (marginalization of the imperial court,
 *   concentration of power in the shogunate).
 *
 * KEY AGENTS:
 *   - emperor: Beneficiary/Excluded (institutional/identity_locked) — grants legitimacy, but politically constrained
 *   - shogunate: Agenda Setter (institutional/constrained) — exercises delegated authority, benefits from stability
 *   - samurai_class: Beneficiary (organized/constrained) — governing stratum, benefits from delegated authority
 *   - imperial_court: Payer/Excluded (moderate/identity_locked) — bears political marginalization
 *   - commoners: Payer (powerless/trapped) — bears costs of governance, no direct representation
 *   - loyalist_scholars: Excluded (moderate/constrained) — advocate for direct imperial rule, suppressed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.45).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.65).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Imperial Mandate: Bakufu Delegation Reading").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, '9c71c2b3-e2d7-4ef4-8317-1006ca2bde7e').
narrative_ontology:cs_kernel_codification('9c71c2b3-e2d7-4ef4-8317-1006ca2bde7e', formalized).
narrative_ontology:cs_authority_grounding('9c71c2b3-e2d7-4ef4-8317-1006ca2bde7e', lineage).
narrative_ontology:cs_interpretation_layer_present('9c71c2b3-e2d7-4ef4-8317-1006ca2bde7e').
narrative_ontology:cs_reading_relation('9c71c2b3-e2d7-4ef4-8317-1006ca2bde7e', imperial_mandate__loyalist_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('9c71c2b3-e2d7-4ef4-8317-1006ca2bde7e', foundational, imperial_authority_delegable).
narrative_ontology:cs_axiom_status(imperial_authority_delegable, holdable).
narrative_ontology:cs_axiom_grounding('9c71c2b3-e2d7-4ef4-8317-1006ca2bde7e', imperial_authority_delegable, conventional).
narrative_ontology:cs_axiom('9c71c2b3-e2d7-4ef4-8317-1006ca2bde7e', foundational, shogunal_governance_legitimate_by_delegation).
narrative_ontology:cs_axiom_status(shogunal_governance_legitimate_by_delegation, holdable).
narrative_ontology:cs_axiom_grounding('9c71c2b3-e2d7-4ef4-8317-1006ca2bde7e', shogunal_governance_legitimate_by_delegation, conventional).
narrative_ontology:cs_reference_frame('9c71c2b3-e2d7-4ef4-8317-1006ca2bde7e', bifurcated_sovereignty_framework).
narrative_ontology:cs_drift_state('9c71c2b3-e2d7-4ef4-8317-1006ca2bde7e', late_tokugawa_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9c71c2b3-e2d7-4ef4-8317-1006ca2bde7e', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, shogunate).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_class).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_court).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, commoners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, emperor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ritual head of state, whose divine lineage grants ultimate legitimacy to the entire political system. Receives symbolic deference and a secure, if politically constrained, position. His active political involvement is suppressed, making him a beneficiary of the system's stability but also a victim of its political limitations.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, emperor, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__bakufu_delegation_reading, emperor, excluded).

% The de facto administrative and military ruler, exercising authority through delegation from the emperor. Benefits from the legitimacy granted by the imperial mandate without bearing the direct burdens of ritual or divine connection. Actively enforces the separation of imperial ritual from shogunal governance.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, shogunate, agenda_setter,
    institutional, generational, constrained, national).

% The governing stratum, whose power and status are legitimized by the shogunate's delegated authority. Benefits from the stability and social order maintained by the bifurcated system, which secures their position as administrators and warriors.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, samurai_class, beneficiary,
    organized, generational, constrained, national).

% The aristocratic body surrounding the emperor, whose political influence is largely ceremonial. Bears the cost of political marginalization, with their traditional governing functions usurped by the shogunate. Their identity is deeply tied to the imperial institution, limiting exit options.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, imperial_court, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__bakufu_delegation_reading, imperial_court, excluded).

% The general populace, who are governed by the shogunate and samurai class. They bear the costs of taxation, conscription, and social hierarchy, with little direct political representation. Their lives are shaped by the stability (and occasional instability) of the delegated authority structure.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, commoners, payer,
    powerless, immediate, trapped, local).

% Intellectuals and activists who advocate for the direct exercise of imperial sovereignty and the abolition of shogunal rule. Their voices are suppressed by the shogunate, and their ideas are considered subversive to the established order. They represent an alternative reading of the imperial mandate.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, loyalist_scholars, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a stable political order by separating the sacred, legitimizing authority of the emperor from the practical, administrative authority of the shogun, allowing for effective governance while preserving divine sanction.
% TRANSFER_FUNCTION: Transfers active political power and administrative control from the imperial court to the shogunate and samurai class, in exchange for the shogunate's enforcement of the emperor's ritual and symbolic supremacy.
% ABSENT_VOICES: Loyalist scholars and movements advocating for direct imperial rule are suppressed; they would argue that the emperor's mandate requires active governance, not mere symbolic legitimacy, and that the delegation is a usurpation.
% DISAPPEARANCE_RATIONALE: If the concept of imperial delegation vanished, the shogunate's legitimacy would collapse, leading to immediate political instability, civil war, and a fundamental restructuring of the Japanese state, likely towards direct imperial rule or a new form of governance.
% FOUNDING_PROBLEM: The need to reconcile the divine, immutable authority of the emperor with the practical demands of military governance and administrative control in a feudal society, preventing direct conflict between imperial and military power.
% FOUNDING_PROBLEM_CORROBORATION: The shogunate and samurai class attest that the problem of balancing divine legitimacy with practical governance remains live, justifying their continued rule. Historians and political theorists outside the direct beneficiaries corroborate the historical necessity of this bifurcation for maintaining stability in pre-modern Japan, even while acknowledging its extractive elements.
narrative_ontology:disappearance_verdict(imperial_mandate__bakufu_delegation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__bakufu_delegation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(imperial_mandate__bakufu_delegation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__bakufu_delegation_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__bakufu_delegation_reading_tests).
:- end_tests(imperial_mandate__bakufu_delegation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting the transfer of political power and resources from the imperial court to the shogunate. Suppression (0.65) is high due to the active enforcement required to maintain the political marginalization of the emperor and suppress loyalist movements. Theater ratio (0.70) is also high, as the elaborate rituals and symbolic deference to the emperor serve primarily to legitimize the shogunate's rule, rather than reflecting direct imperial governance. The metrics show relative stability over the long interval, indicating a well-established, if contested, system.
 *
 * PERSPECTIVAL GAP:
 *   The shogunate and samurai class would experience this as a legitimate and necessary coordination mechanism for stable governance, with the emperor as a revered, if distant, source of authority. The imperial court and loyalist scholars, however, would perceive it as an extractive usurpation of imperial power, maintained through coercion and a theatrical display of deference.
 *
 * DIRECTIONALITY LOGIC:
 *   The shogunate and samurai class are clear beneficiaries, gaining power and legitimacy from the delegation (low directionality). The imperial court and commoners are payers, bearing the costs of political marginalization and governance respectively (high directionality). The emperor is a complex case: a beneficiary of the system's stability and divine status, but also a victim of his political suppression, leading to a mixed directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the system as pure extraction by recognizing the genuine coordination problem it solved (stable governance in a feudal context). However, the high theater ratio and suppression indicate that the coordination function is heavily intertwined with, and perhaps overshadowed by, the extractive elements that benefit the shogunate. The 'live' status of the founding problem is contested, suggesting potential mandatrophy where the original justification has atrophied but the structure persists due to concentrated benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a true representation of the ''Bakufu Delegation'' reading of the Imperial Mandate, or does it conflate elements of other readings?',
    'Comparative textual analysis of historical legal codes, political treatises, and court documents from the period, specifically focusing on how imperial authority and shogunal power were articulated and justified.',
    'If conflated, the classification might inaccurately represent the specific structural properties of this reading, potentially leading to mischaracterization of extractiveness or suppression. A purer reading would refine the metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ensuring the constraint accurately reflects the specific ''Bakufu Delegation'' reading.').

omega_variable(
    legitimacy_source_ambiguity,
    'To what extent did the shogunate''s legitimacy truly derive from imperial delegation versus its own military power and administrative efficacy?',
    'Historical counterfactual analysis: examine periods of imperial weakness or strong shogunal assertion to determine if the shogunate could maintain power without explicit imperial sanction, or if imperial sanction was always a necessary component.',
    'If legitimacy was primarily self-derived, the ''delegation'' aspect becomes more theatrical, increasing the constraint''s effective extractiveness and theater ratio, pushing it closer to a Snare. If delegation was crucial, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, empirical, 'Ambiguity in the true source of shogunal legitimacy.').

omega_variable(
    imperial_agency_suppression,
    'Was the emperor''s political marginalization a willing acceptance of a ritual role, or an actively enforced suppression of imperial agency?',
    'Analysis of imperial court records, diaries, and correspondence for evidence of attempts to assert political power, and the shogunate''s responses to such attempts. Examine the severity and frequency of shogunal interventions in court affairs.',
    'If willing, the suppression metric might be lower, and the emperor''s directionality would shift towards a more symmetric beneficiary. If actively enforced, the suppression metric is accurate, and the emperor''s position as a victim is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_agency_suppression, empirical, 'Nature of imperial political marginalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 0, 265).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t0, imperial_mandate__bakufu_delegation_reading, theater_ratio, 0, 0.6).
narrative_ontology:measurement(impe_tr_t50, imperial_mandate__bakufu_delegation_reading, theater_ratio, 50, 0.65).
narrative_ontology:measurement(impe_tr_t100, imperial_mandate__bakufu_delegation_reading, theater_ratio, 100, 0.7).
narrative_ontology:measurement(impe_tr_t150, imperial_mandate__bakufu_delegation_reading, theater_ratio, 150, 0.72).
narrative_ontology:measurement(impe_tr_t200, imperial_mandate__bakufu_delegation_reading, theater_ratio, 200, 0.71).
narrative_ontology:measurement(impe_tr_t265, imperial_mandate__bakufu_delegation_reading, theater_ratio, 265, 0.7).

% Extraction over time
narrative_ontology:measurement(impe_be_t0, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(impe_be_t50, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 50, 0.4).
narrative_ontology:measurement(impe_be_t100, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 100, 0.45).
narrative_ontology:measurement(impe_be_t150, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 150, 0.48).
narrative_ontology:measurement(impe_be_t200, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 200, 0.46).
narrative_ontology:measurement(impe_be_t265, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 265, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t0, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(impe_su_t50, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(impe_su_t100, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 100, 0.65).
narrative_ontology:measurement(impe_su_t150, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 150, 0.68).
narrative_ontology:measurement(impe_su_t200, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 200, 0.67).
narrative_ontology:measurement(impe_su_t265, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 265, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
