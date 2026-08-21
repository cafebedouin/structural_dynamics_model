% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__originalist_civic_virtue_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: second_amendment_text__originalist_civic_virtue_reading
 *   human_readable: Second Amendment: Originalist Civic Virtue Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'originalist civic virtue' reading of the
 *   Second Amendment, where the right to bear arms is primarily understood in
 *   the context of a 'well-regulated militia' composed of the general
 *   citizenry, essential for maintaining a free state. The right is not an
 *   unfettered individual right for personal self-defense, nor is it solely a
 *   collective right of the states, but rather a civic duty and capacity tied
 *   to the political community. Extraction is low because the constraint
 *   primarily coordinates civic responsibility, and suppression is low as it
 *   relies on voluntary participation rather than coercion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.15).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.05).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Second Amendment: Originalist Civic Virtue Reading").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional_law/political_theory/firearms_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, '963dbac1-7ad2-4174-9890-a816d56031fb').
narrative_ontology:cs_kernel_codification('963dbac1-7ad2-4174-9890-a816d56031fb', fixed_text).
narrative_ontology:cs_authority_grounding('963dbac1-7ad2-4174-9890-a816d56031fb', lineage).
narrative_ontology:cs_interpretation_layer_present('963dbac1-7ad2-4174-9890-a816d56031fb').
narrative_ontology:cs_reading_relation('963dbac1-7ad2-4174-9890-a816d56031fb', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('963dbac1-7ad2-4174-9890-a816d56031fb', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_axiom('963dbac1-7ad2-4174-9890-a816d56031fb', foundational, armed_citizenry_essential_to_free_state).
narrative_ontology:cs_axiom_status(armed_citizenry_essential_to_free_state, holdable).
narrative_ontology:cs_axiom_grounding('963dbac1-7ad2-4174-9890-a816d56031fb', armed_citizenry_essential_to_free_state, deontological).
narrative_ontology:cs_axiom('963dbac1-7ad2-4174-9890-a816d56031fb', foundational, militia_service_is_civic_duty).
narrative_ontology:cs_axiom_status(militia_service_is_civic_duty, holdable).
narrative_ontology:cs_axiom_grounding('963dbac1-7ad2-4174-9890-a816d56031fb', militia_service_is_civic_duty, conventional).
narrative_ontology:cs_reference_frame('963dbac1-7ad2-4174-9890-a816d56031fb', founding_era_civic_republicanism).
narrative_ontology:cs_drift_state('963dbac1-7ad2-4174-9890-a816d56031fb', contemporary_interpretive_landscape, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('963dbac1-7ad2-4174-9890-a816d56031fb', '').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, the_citizenry_qua_political_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_text__originalist_civic_virtue_reading, individual_citizens).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, civic_republicanism).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, popular_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the preservation of a well-regulated militia as a check on tyranny and a guarantor of a free state, embodying the civic republican ideal of an armed citizenry. The right is understood as protecting the capacity for collective self-defense and civic participation, not individual self-defense in isolation.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, the_citizenry_qua_political_community, beneficiary,
    institutional, generational, identity_locked, national).

% Have the responsibility to organize and regulate the militia, ensuring its effectiveness. Their power to regulate arms is understood as serving the 'well-regulated' aspect of the militia, not as an infringement on an individual right. They are constrained by the need to maintain the citizen-soldier capacity.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, state_governments, agenda_setter,
    institutional, generational, constrained, national).

% Interprets the Second Amendment in its role as a co-equal branch of government, often through judicial review. This reading emphasizes the collective and civic aspects, influencing federal policy on firearms and militia organization.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, federal_government, observer,
    institutional, generational, analytical, national).

% Bear the responsibility of militia service and the potential costs associated with maintaining arms for civic duty. Their individual right to bear arms is subsumed within the collective purpose of the militia, meaning personal preferences for certain arms or uses may be constrained by state regulation aimed at civic virtue.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, individual_citizens, payer,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the capacity of the citizenry to form a 'well-regulated militia' for collective defense and to secure a 'free State', ensuring popular sovereignty and preventing standing armies from becoming instruments of tyranny.
% TRANSFER_FUNCTION: Transfers the responsibility for collective security and civic virtue to the armed citizenry, rather than a professional military, thereby distributing the burden and power of defense.
% ABSENT_VOICES: Those who advocate for an unfettered individual right to bear any arms for any purpose, or those who seek complete disarmament, would object to this reading's emphasis on civic duty and state regulation for collective good. Their voices are often marginalized in interpretations that prioritize the 'well-regulated militia' clause.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the constitutional landscape around firearms would shift dramatically towards either an unfettered individual right or a purely state-controlled collective security model, fundamentally altering the relationship between citizens, arms, and the state, and potentially eroding the civic republican ideal of an armed citizenry.
% FOUNDING_PROBLEM: The problem of ensuring a free state and popular sovereignty against both internal usurpation and external threats, without relying on a standing army that could itself become tyrannical.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political theorists outside of direct beneficiary groups corroborate that the founding generation grappled with these concerns, viewing an armed citizenry as essential to republican governance. Contemporary debates over gun control and the role of militias continue to reflect these foundational tensions.
narrative_ontology:disappearance_verdict(second_amendment_text__originalist_civic_virtue_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__originalist_civic_virtue_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__originalist_civic_virtue_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_text__originalist_civic_virtue_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__originalist_civic_virtue_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__originalist_civic_virtue_reading_tests).
:- end_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) reflects that this reading primarily imposes a civic duty and coordinates collective defense, with minimal direct material extraction from individuals. The low suppression (0.05) indicates that the constraint's persistence relies on the civic commitment of the citizenry rather than active coercion to enforce participation. Theater ratio is low (0.1) as the civic function is considered genuine, even if its practical manifestation has evolved. Accessibility collapse is moderate (0.7) because while the civic duty is strong, alternative interpretations exist and are actively debated. Resistance is low (0.1) because this reading, while contested by other interpretations, is not actively resisted by those who adhere to its core tenets.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the citizenry as a whole, this is a foundational 'rope' that secures their liberty. From the perspective of an individual citizen, it might feel more 'constrained' as their personal choices regarding firearms are subordinated to a collective civic purpose. The engine's per-seat classification would reflect this difference in effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The citizenry qua political community is the primary beneficiary, as the constraint aims to secure a free state for them. State governments act as agenda-setters, responsible for regulating the militia. Individual citizens are payers, bearing the responsibility and potential costs of maintaining arms for civic duty, with their individual rights constrained by the collective purpose. The federal government acts as an observer, interpreting the amendment. There are no specific 'victims' in this reading, as any 'cost' to individuals is framed as a civic responsibility for the greater good.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_duty_vs_individual_liberty,
    'To what extent does the emphasis on civic duty and collective purpose in this reading genuinely coordinate collective action, versus implicitly suppressing individual liberty in a way that benefits the state?',
    'Analysis of historical and contemporary state regulations on militia service and arms ownership under this interpretive framework, examining the actual burdens placed on individuals versus the demonstrated collective benefits.',
    'If the suppression of individual liberty is found to be disproportionate to the collective benefit, the constraint''s effective extractiveness and suppression metrics would need to be re-evaluated upwards, potentially shifting its classification towards a ''tangled_rope'' for individual citizens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_duty_vs_individual_liberty, conceptual, 'Ambiguity in balancing collective civic duty with individual freedom in the context of arms bearing.').

omega_variable(
    militia_relevance_in_modern_era,
    'Is the ''well-regulated militia'' as understood in the founding era still a relevant and functional concept for securing a free state in the modern context, or has its function atrophied?',
    'Empirical study of the actual role and effectiveness of state militias and armed citizenry in contemporary national defense and civic security, compared to professional military and law enforcement.',
    'If the militia''s function is found to be largely atrophied, the constraint''s ''theater_ratio'' would increase, and its ''founding_problem_status'' might shift to ''dead'', potentially reclassifying it as a ''piton'' if its persistence is primarily inertial rather than functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_relevance_in_modern_era, empirical, 'The contemporary relevance and functional status of the founding-era militia concept.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(seco_tr_t60, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(seco_tr_t120, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 120, 0.1).
narrative_ontology:measurement(seco_tr_t180, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 180, 0.1).
narrative_ontology:measurement(seco_tr_t240, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 240, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(seco_be_t60, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 60, 0.15).
narrative_ontology:measurement(seco_be_t120, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 120, 0.15).
narrative_ontology:measurement(seco_be_t180, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 180, 0.15).
narrative_ontology:measurement(seco_be_t240, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 240, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(seco_su_t60, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 60, 0.05).
narrative_ontology:measurement(seco_su_t120, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 120, 0.05).
narrative_ontology:measurement(seco_su_t180, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 180, 0.05).
narrative_ontology:measurement(seco_su_t240, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 240, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
