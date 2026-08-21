% ============================================================================
% CONSTRAINT STORY: maat_order_principle__distributed_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__distributed_maintenance_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: maat_order_principle__distributed_maintenance_reading
 *   human_readable: Ma'at Order Principle: Distributed Maintenance
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This constraint represents the 'distributed maintenance' reading of the
 *   Ma'at order principle in ancient Egypt. In this reading, Ma'at is not
 *   solely embodied by the Pharaoh but is a cosmic and social order that all
 *   individuals, from the ruler to the commoner, are responsible for
 *   upholding through their actions and adherence to ethical conduct. This
 *   distributed responsibility leads to lower extraction and suppression
 *   compared to readings where Ma'at is a top-down divine mandate. The
 *   constraint is claimed as a Rope due to its genuine coordination function
 *   and relatively low extraction, benefiting all of society.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__distributed_maintenance_reading, 0.15).
domain_priors:suppression_score(maat_order_principle__distributed_maintenance_reading, 0.25).
domain_priors:theater_ratio(maat_order_principle__distributed_maintenance_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__distributed_maintenance_reading, rope).
narrative_ontology:human_readable(maat_order_principle__distributed_maintenance_reading, "Ma'at Order Principle: Distributed Maintenance").
narrative_ontology:topic_domain(maat_order_principle__distributed_maintenance_reading, "ancient_history/political_philosophy/religious_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__distributed_maintenance_reading, 'ab48f7a7-1f44-4a50-8775-42a8e0d58397').
narrative_ontology:cs_kernel_codification('ab48f7a7-1f44-4a50-8775-42a8e0d58397', implicit).
narrative_ontology:cs_authority_grounding('ab48f7a7-1f44-4a50-8775-42a8e0d58397', practice).
narrative_ontology:cs_interpretation_layer_present('ab48f7a7-1f44-4a50-8775-42a8e0d58397').
narrative_ontology:cs_reading_relation('ab48f7a7-1f44-4a50-8775-42a8e0d58397', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab48f7a7-1f44-4a50-8775-42a8e0d58397', maat_order_principle__reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('ab48f7a7-1f44-4a50-8775-42a8e0d58397', foundational, universal_ethical_responsibility).
narrative_ontology:cs_axiom_status(universal_ethical_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('ab48f7a7-1f44-4a50-8775-42a8e0d58397', universal_ethical_responsibility, deontological).
narrative_ontology:cs_axiom('ab48f7a7-1f44-4a50-8775-42a8e0d58397', foundational, demonstrated_conduct_legitimizes_authority).
narrative_ontology:cs_axiom_status(demonstrated_conduct_legitimizes_authority, holdable).
narrative_ontology:cs_axiom_grounding('ab48f7a7-1f44-4a50-8775-42a8e0d58397', demonstrated_conduct_legitimizes_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('ab48f7a7-1f44-4a50-8775-42a8e0d58397', ideal_distributed_maat_society).
narrative_ontology:cs_drift_state('ab48f7a7-1f44-4a50-8775-42a8e0d58397', periods_of_pharaonic_centralization, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('ab48f7a7-1f44-4a50-8775-42a8e0d58397', '').
narrative_ontology:cs_kernel_id(maat_order_principle__distributed_maintenance_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, all_of_egyptian_society).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, priests_and_scribes).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, commoners_and_farmers).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, commoners_and_farmers).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, cosmic_order_doctrine).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, social_harmony_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary, but not sole, agent responsible for upholding Ma'at through just rule, ritual, and public works. Their legitimacy depends on demonstrating adherence to Ma'at, not merely claiming it. Failure to maintain Ma'at can lead to loss of divine favor and social unrest.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, pharaoh, agenda_setter,
    institutional, generational, identity_locked, national).

% Administer justice and manage resources according to Ma'at. Their conduct directly impacts the well-being of the populace and reflects on the overall state of cosmic order. They are accountable for their actions and can be judged for failing to uphold Ma'at.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, viziers_and_officials, agenda_setter,
    organized, biographical, constrained, regional).

% Interpret and transmit the principles of Ma'at, ensuring its understanding and practice across society. They benefit from the stability and order Ma'at provides, and their role is crucial in educating others on proper conduct.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, priests_and_scribes, beneficiary,
    organized, generational, identity_locked, national).

% Expected to live in accordance with Ma'at through honest labor, respect for elders, and adherence to social norms. They bear the diffuse cost of maintaining social order through self-regulation but also benefit directly from the stability and justice it provides. Their collective conduct contributes to the overall state of Ma'at.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, commoners_and_farmers, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, commoners_and_farmers, beneficiary).

% The ultimate beneficiary of Ma'at, experiencing cosmic harmony, social stability, and prosperity when it is upheld. The entire social and natural order is understood to depend on its maintenance.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, all_of_egyptian_society, beneficiary,
    institutional, civilizational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the conduct of all individuals and institutions in Egyptian society towards a shared understanding of cosmic order, justice, and truth, ensuring social harmony and divine favor.
% TRANSFER_FUNCTION: Transfers responsibility for maintaining cosmic order from a singular divine source to a distributed network of actors, requiring each to contribute proper conduct and ethical action, which in turn 'transfers' stability and prosperity back to society.
% ABSENT_VOICES: Those who might challenge the very concept of Ma'at or its distributed nature are not present in the discourse; their perspectives are foreclosed by the foundational cultural and religious framework.
% DISAPPEARANCE_RATIONALE: If the principle of Ma'at vanished, the entire social, political, and religious structure of ancient Egypt would collapse. The legitimacy of the Pharaoh, the justice system, and the very understanding of the cosmos would be lost, leading to chaos and societal breakdown.
% FOUNDING_PROBLEM: The need to establish and maintain cosmic order, justice, and social harmony in a complex society, ensuring divine favor and preventing chaos (Isfet).
% FOUNDING_PROBLEM_CORROBORATION: Historical texts, religious doctrines, and archaeological evidence consistently corroborate the foundational importance of Ma'at throughout ancient Egyptian history, indicating its problem-solving function remained live for millennia. The concept is attested by a vast body of evidence outside the immediate beneficiaries of any single reign.
narrative_ontology:disappearance_verdict(maat_order_principle__distributed_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__distributed_maintenance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__distributed_maintenance_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(maat_order_principle__distributed_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__distributed_maintenance_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__distributed_maintenance_reading_tests).
:- end_tests(maat_order_principle__distributed_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the primary 'cost' is ethical conduct and self-regulation, which is broadly beneficial. Suppression is also low (0.25) as enforcement relies more on social norms and individual conscience than overt coercion, though the threat of cosmic imbalance (Isfet) provides a background pressure. Theater ratio is low (0.1) because the performance of Ma'at (rituals, just decrees) is genuinely believed to contribute to its maintenance, rather than being a mere cover for extraction. The cyclical nature of measurements reflects periods of greater or lesser adherence to Ma'at, often tied to political stability or natural events (e.g., Nile floods).
 *
 * PERSPECTIVAL GAP:
 *   While the Pharaoh holds the most prominent role, this reading emphasizes that their legitimacy is contingent on upholding Ma'at, not inherent. Other readings (divine_mandate_reading) would see the Pharaoh as the sole embodiment, leading to higher extraction and suppression from other seats. This reading's distributed accountability means that even commoners have a role in maintaining cosmic order, and their failure to do so would be seen as contributing to societal imbalance.
 *
 * DIRECTIONALITY LOGIC:
 *   The Pharaoh and officials are agenda-setters, responsible for leading the maintenance of Ma'at. Priests and scribes are beneficiaries, interpreting and transmitting Ma'at. Commoners are payers through their conduct but also beneficiaries of the resulting social order. All of Egyptian society is the ultimate beneficiary. This distributed responsibility means no single actor captures excessive gains, leading to a more balanced distribution of costs and benefits.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_locus_of_maat_authority,
    'Is the authority to interpret and enforce Ma''at truly distributed, or does it ultimately reside with the Pharaoh and his appointed officials?',
    'Analysis of historical legal cases and social unrest: if commoners successfully appeal to Ma''at against official injustice, or if societal breakdown is attributed to widespread individual misconduct, it supports distributed authority. If only the Pharaoh''s decrees are binding, it supports centralized authority.',
    'If authority is truly distributed, the constraint is a genuine Rope. If it''s centralized, the constraint leans towards a Tangled Rope or Snare, with higher extraction from commoners and greater suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_locus_of_maat_authority, empirical, 'Ambiguity regarding the actual distribution of interpretive and enforcement authority for Ma''at.').

omega_variable(
    distinction_from_divine_mandate,
    'How distinct is this ''distributed maintenance'' reading from the ''divine mandate'' reading in practice, especially during periods of strong pharaonic rule?',
    'Comparative analysis of royal inscriptions, wisdom literature, and funerary texts from different periods: look for explicit statements of individual responsibility versus sole pharaonic embodiment of Ma''at. Examine periods of pharaonic weakness for evidence of distributed responsibility becoming more prominent.',
    'If the distinction is weak, this reading might collapse into a more extractive type, as the Pharaoh''s power would effectively override distributed accountability. If strong, it reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distinction_from_divine_mandate, conceptual, 'The practical and conceptual boundary between distributed and divinely mandated Ma''at.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__distributed_maintenance_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__distributed_maintenance_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__distributed_maintenance_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__distributed_maintenance_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(maat_tr_t60, maat_order_principle__distributed_maintenance_reading, theater_ratio, 60, 0.09).
narrative_ontology:measurement(maat_tr_t80, maat_order_principle__distributed_maintenance_reading, theater_ratio, 80, 0.07).
narrative_ontology:measurement(maat_tr_t100, maat_order_principle__distributed_maintenance_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(maat_be_t60, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 60, 0.14).
narrative_ontology:measurement(maat_be_t80, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 80, 0.13).
narrative_ontology:measurement(maat_be_t100, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(maat_su_t40, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(maat_su_t60, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 60, 0.23).
narrative_ontology:measurement(maat_su_t80, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 80, 0.24).
narrative_ontology:measurement(maat_su_t100, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 100, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__distributed_maintenance_reading, identity_coordination).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__reciprocity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Ma'at order principle. This 'distributed maintenance' reading emphasizes universal responsibility, leading to lower extraction and suppression compared to the 'divine mandate' and 'reciprocity' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
