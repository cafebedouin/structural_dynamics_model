% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__living_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__living_constitutionalism_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__living_constitutionalism_reading
 *   human_readable: Magna Carta: Inherited Due Process (Living Constitutionalism Reading)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'living constitutionalism' reading of
 *   Magna Carta, where its principles of due process and lawful restraint are
 *   understood to be inherited and binding on all subsequent rulers, evolving
 *   through juridical precedent. It is a foundational 'rope' that coordinates
 *   governance around shared legal principles, extracting from arbitrary
 *   power while benefiting citizens and the judiciary. This reading
 *   explicitly rejects the notion that Magna Carta is merely an obsolete
 *   feudal document or solely absorbed into parliamentary statute.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__living_constitutionalism_reading, 0.25).
domain_priors:suppression_score(magna_carta_constraint_authority__living_constitutionalism_reading, 0.15).
domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__living_constitutionalism_reading, rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__living_constitutionalism_reading, "Magna Carta: Inherited Due Process (Living Constitutionalism Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__living_constitutionalism_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__living_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__living_constitutionalism_reading, 'fdafcdae-d775-4a4d-8628-d98e25e1a0f8').
narrative_ontology:cs_kernel_codification('fdafcdae-d775-4a4d-8628-d98e25e1a0f8', fixed_text).
narrative_ontology:cs_authority_grounding('fdafcdae-d775-4a4d-8628-d98e25e1a0f8', lineage).
narrative_ontology:cs_interpretation_layer_present('fdafcdae-d775-4a4d-8628-d98e25e1a0f8').
narrative_ontology:cs_reading_relation('fdafcdae-d775-4a4d-8628-d98e25e1a0f8', magna_carta_constraint_authority__feudal_obsolescence_reading, forecloses).
narrative_ontology:cs_reading_relation('fdafcdae-d775-4a4d-8628-d98e25e1a0f8', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('fdafcdae-d775-4a4d-8628-d98e25e1a0f8', foundational, inherited_due_process_binding).
narrative_ontology:cs_axiom_status(inherited_due_process_binding, holdable).
narrative_ontology:cs_axiom_grounding('fdafcdae-d775-4a4d-8628-d98e25e1a0f8', inherited_due_process_binding, deontological).
narrative_ontology:cs_axiom('fdafcdae-d775-4a4d-8628-d98e25e1a0f8', foundational, constitutional_principles_evolve).
narrative_ontology:cs_axiom_status(constitutional_principles_evolve, holdable).
narrative_ontology:cs_axiom_grounding('fdafcdae-d775-4a4d-8628-d98e25e1a0f8', constitutional_principles_evolve, conventional).
narrative_ontology:cs_reference_frame('fdafcdae-d775-4a4d-8628-d98e25e1a0f8', inherited_constitutional_restraint).
narrative_ontology:cs_drift_state('fdafcdae-d775-4a4d-8628-d98e25e1a0f8', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fdafcdae-d775-4a4d-8628-d98e25e1a0f8', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, citizens).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, judiciary).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, executive_prerogative).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, unfettered_royal_power).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, parliament).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the protection of due process and lawful restraint against arbitrary state power, as interpreted and evolved through legal precedent. Their ability to exit arbitrary rule is through the legal system itself.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, citizens, beneficiary,
    organized, generational, constrained, national).

% Interprets and applies Magna Carta's principles, evolving their meaning through juridical precedent to fit contemporary contexts. Their institutional identity is bound to upholding constitutional principles, including those derived from Magna Carta.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, judiciary, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Represents the historical and ongoing limitation on the executive's ability to act without legal justification or due process. This 'agent' is an abstract concept of power that is constrained by the living interpretation of Magna Carta.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, executive_prerogative, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_non_agent(magna_carta_constraint_authority__living_constitutionalism_reading, executive_prerogative).

% Represents the historical concept of absolute monarchical authority, which is directly curtailed by Magna Carta's principles. This 'agent' is a historical construct whose power is permanently diminished by the constraint.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, unfettered_royal_power, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(magna_carta_constraint_authority__living_constitutionalism_reading, unfettered_royal_power).

% While sovereign, Parliament's actions are often interpreted through the lens of fundamental constitutional principles, including those derived from Magna Carta, especially regarding due process and rule of law. This reading suggests Parliament is constrained by these inherited principles, even if it theoretically holds the power to legislate otherwise.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, parliament, payer,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a foundational framework for lawful governance, ensuring that rulers are bound by law and providing a common understanding of due process and individual liberties, which evolves with society.
% TRANSFER_FUNCTION: Transfers authority from arbitrary executive power to a system of law and precedent, granting protections and rights to subjects/citizens.
% ABSENT_VOICES: Those who advocate for absolute executive or parliamentary sovereignty, unconstrained by historical or evolving constitutional principles, are often marginalized in this interpretive framework. They would argue for a more direct and unmediated exercise of state power.
% DISAPPEARANCE_RATIONALE: If Magna Carta's principles, as interpreted through living constitutionalism, vanished, the foundational understanding of due process, rule of law, and the limits of state power would collapse. Legal systems would lose a key historical anchor, leading to a profound reordering of governance and citizen rights.
% FOUNDING_PROBLEM: The problem of arbitrary rule by the monarch, where subjects lacked consistent legal protections against executive overreach and lacked a clear mechanism for redress.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, human rights advocates, and the judiciary consistently attest that the problem of potential arbitrary power remains live, requiring ongoing vigilance and interpretation of foundational documents like Magna Carta. This corroboration comes from outside the direct beneficiaries of executive power.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__living_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__living_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_constraint_authority__living_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).
:- end_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.25) because the constraint primarily limits arbitrary power rather than extracting resources from a coordinated group. Suppression is low (0.15) as its persistence relies on widespread acceptance and judicial enforcement, not active coercion against dissenters. Theater ratio is very low (0.05) as its function is genuinely active in legal interpretation and constitutional discourse. The historical measurements reflect periods of contestation (e.g., near 1215) and periods of greater stability or re-interpretation (e.g., post-Glorious Revolution 1688).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of citizens and the judiciary, Magna Carta is a foundational and evolving protection. From the perspective of executive or parliamentary power, it represents a historical limitation on their discretion. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizens and the judiciary are beneficiaries, gaining protection and interpretive authority, respectively. Executive prerogative and unfettered royal power are the primary victims, as their scope is curtailed. Parliament, while sovereign, is also a 'payer' in this reading, as its actions are implicitly constrained by these inherited principles. The constraint's directionality is towards limiting state power for the benefit of the governed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_drift_vs_original_intent,
    'To what extent does ''evolutionary interpretation'' depart from the original intent or historical context of Magna Carta, and at what point does it become a new constraint?',
    'Detailed historical-legal analysis comparing contemporary interpretations with 13th-century legal and political realities, coupled with a conceptual framework for ''fidelity to text'' vs. ''fidelity to principle''.',
    'If the departure is deemed too great, this ''living constitutionalism'' reading might be reclassified as a ''snare'' (if it extracts from historical fidelity) or a ''scaffold'' (if it''s a temporary bridge to a new constitutional order). If the fidelity is maintained, its ''rope'' classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_drift_vs_original_intent, conceptual, 'Ambiguity in the scope and limits of ''evolutionary interpretation'' of foundational legal texts.').

omega_variable(
    judicial_activism_vs_interpretation,
    'Is the judiciary''s ''evolutionary interpretation'' a legitimate adaptation of principles, or an instance of judicial activism that oversteps its role and effectively creates new law?',
    'Analysis of judicial decisions against established legal theory on interpretive methods, and comparison with legislative intent or public consensus on constitutional principles.',
    'If deemed activism, the ''judiciary'' stakeholder''s role might shift towards ''agenda_setter'' with higher extractiveness from other branches of government, potentially reclassifying the constraint towards ''tangled_rope'' or ''snare'' from those seats. If legitimate, the ''rope'' classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_activism_vs_interpretation, preference, 'Distinction between legitimate judicial interpretation and illegitimate judicial activism in evolving constitutional principles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__living_constitutionalism_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1688, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1688, 0.05).
narrative_ontology:measurement(magn_tr_t1800, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(magn_tr_t1900, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(magn_tr_t2000, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1215, 0.3).
narrative_ontology:measurement(magn_be_t1688, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1688, 0.2).
narrative_ontology:measurement(magn_be_t1800, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1800, 0.25).
narrative_ontology:measurement(magn_be_t1900, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(magn_be_t2000, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(magn_be_t2024, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1215, 0.4).
narrative_ontology:measurement(magn_su_t1688, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1688, 0.2).
narrative_ontology:measurement(magn_su_t1800, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1800, 0.15).
narrative_ontology:measurement(magn_su_t1900, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(magn_su_t2000, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 2000, 0.15).
narrative_ontology:measurement(magn_su_t2024, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__living_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'magna_carta_constraint_authority' kernel. This 'living constitutionalism' reading emphasizes inherited, evolving principles, distinct from the 'feudal obsolescence' and 'parliamentary sovereignty' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
