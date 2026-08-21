% ============================================================================
% CONSTRAINT STORY: maat_order_principle__reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__reciprocity_reading, []).

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
 *   constraint_id: maat_order_principle__reciprocity_reading
 *   human_readable: Ma'at Order Principle (Reciprocity Reading)
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This constraint represents the 'reciprocity reading' of the ancient
 *   Egyptian principle of Ma'at. In this interpretation, Ma'at imposes mutual
 *   obligations: the Pharaoh must provide justice, stability, and proper
 *   resource distribution to maintain cosmic balance, and in return, the
 *   populace grants legitimacy and support. Failure by the Pharaoh to uphold
 *   these obligations could conceptually justify resistance or withdrawal of
 *   support, placing a moderate ceiling on extraction. This reading contrasts
 *   with 'divine mandate' (Pharaoh is Ma'at) and 'distributed maintenance'
 *   (all maintain Ma'at).
 *
 * KEY AGENTS:
 *   - Pharaoh: Agenda-setter, primary beneficiary of legitimacy, but also constrained by Ma'at's demands.
 *   - Egyptian Priesthood: Beneficiaries, interpreters of Ma'at, derive power from its necessity.
 *   - Common Egyptians: Payers, bear costs, benefit from stability, but have limited recourse.
 *   - Cosmic Order: Analytical observer, the abstract principle itself.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, 0.4).
domain_priors:suppression_score(maat_order_principle__reciprocity_reading, 0.6).
domain_priors:theater_ratio(maat_order_principle__reciprocity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__reciprocity_reading, "Ma'at Order Principle (Reciprocity Reading)").
narrative_ontology:topic_domain(maat_order_principle__reciprocity_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__reciprocity_reading, 'f62dc31a-e401-4bbb-836f-05698cd9fb6f').
narrative_ontology:cs_kernel_codification('f62dc31a-e401-4bbb-836f-05698cd9fb6f', implicit).
narrative_ontology:cs_authority_grounding('f62dc31a-e401-4bbb-836f-05698cd9fb6f', lineage).
narrative_ontology:cs_interpretation_layer_present('f62dc31a-e401-4bbb-836f-05698cd9fb6f').
narrative_ontology:cs_reading_relation('f62dc31a-e401-4bbb-836f-05698cd9fb6f', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('f62dc31a-e401-4bbb-836f-05698cd9fb6f', maat_order_principle__distributed_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('f62dc31a-e401-4bbb-836f-05698cd9fb6f', foundational, pharaoh_subject_to_maat).
narrative_ontology:cs_axiom_status(pharaoh_subject_to_maat, holdable).
narrative_ontology:cs_axiom_grounding('f62dc31a-e401-4bbb-836f-05698cd9fb6f', pharaoh_subject_to_maat, deontological).
narrative_ontology:cs_axiom('f62dc31a-e401-4bbb-836f-05698cd9fb6f', foundational, reciprocal_obligations_for_stability).
narrative_ontology:cs_axiom_status(reciprocal_obligations_for_stability, holdable).
narrative_ontology:cs_axiom_grounding('f62dc31a-e401-4bbb-836f-05698cd9fb6f', reciprocal_obligations_for_stability, conventional).
narrative_ontology:cs_reference_frame('f62dc31a-e401-4bbb-836f-05698cd9fb6f', balanced_reciprocal_order).
narrative_ontology:cs_drift_state('f62dc31a-e401-4bbb-836f-05698cd9fb6f', late_dynastic_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f62dc31a-e401-4bbb-836f-05698cd9fb6f', '').
narrative_ontology:cs_kernel_id(maat_order_principle__reciprocity_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, pharaoh).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, egyptian_priesthood).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, common_egyptians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The divine ruler, obligated by Ma'at to provide justice, stability, and resource distribution. Benefits from the legitimacy and stability provided by Ma'at, but is also constrained by its demands. Failure to uphold Ma'at could lead to loss of legitimacy and divine favor.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, pharaoh, agenda_setter,
    institutional, generational, identity_locked, national).

% Interpreters and custodians of Ma'at, they benefit from the social order and their central role in maintaining cosmic balance through rituals and counsel to the Pharaoh. Their power is derived from the perceived necessity of Ma'at.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, egyptian_priesthood, beneficiary,
    organized, generational, constrained, national).

% Expected to live in accordance with Ma'at, contributing labor and resources to the state. They benefit from the stability and justice Pharaoh is supposed to provide, but bear the costs of any imbalance or extraction. Their recourse against a Pharaoh who fails Ma'at is limited but conceptually justified.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, common_egyptians, payer,
    powerless, biographical, trapped, local).

% The abstract principle of truth, justice, and cosmic balance that Ma'at represents. It is the ultimate referent for the constraint, but not an active agent.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, cosmic_order, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(maat_order_principle__reciprocity_reading, cosmic_order).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a reciprocal social contract between the divine ruler and the populace, ensuring societal stability, justice, and proper resource allocation by linking them to cosmic balance.
% TRANSFER_FUNCTION: Transfers legitimacy and divine favor to the Pharaoh in exchange for justice and stability for the populace. It also transfers resources and labor from the populace to the state, justified by the Pharaoh's role in maintaining Ma'at.
% ABSENT_VOICES: Rebellious factions or those who would challenge the divine right of Pharaoh based on his failure to uphold Ma'at. Their voices are suppressed by the state's power, but their potential for resistance is acknowledged by the reciprocity principle.
% DISAPPEARANCE_RATIONALE: If the principle of Ma'at and its reciprocal obligations vanished, the entire socio-political and religious structure of ancient Egypt would collapse. The Pharaoh's legitimacy would evaporate, leading to chaos and a complete reorganization of power and social order.
% FOUNDING_PROBLEM: To establish and maintain a stable, just, and prosperous society in a harsh environment, by linking human governance to a divine, immutable cosmic order.
% FOUNDING_PROBLEM_CORROBORATION: The Egyptian priesthood and historical texts corroborate the founding problem, emphasizing the constant struggle against chaos (Isfet) and the necessity of Ma'at for societal survival. While the Pharaoh benefits, the widespread cultural acceptance and historical evidence support the problem's genuine nature.
narrative_ontology:disappearance_verdict(maat_order_principle__reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__reciprocity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__reciprocity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(maat_order_principle__reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__reciprocity_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__reciprocity_reading_tests).
:- end_tests(maat_order_principle__reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates societal functions (stability, justice) but also involves asymmetric extraction (Pharaoh and priesthood benefit more than commoners). Extractiveness is moderate (0.4) due to the reciprocal nature of the obligations, which theoretically limits the Pharaoh's ability to extract without consequence. Suppression is moderate (0.6) as the state actively enforces the social order, but the underlying reciprocity principle means outright tyranny could undermine the system's legitimacy. Theater ratio is low (0.2) because the rituals and pronouncements of Ma'at are genuinely believed to maintain cosmic order, not merely for show.
 *
 * PERSPECTIVAL GAP:
 *   From the Pharaoh's perspective, Ma'at is the foundation of his legitimate rule and the source of cosmic order. From the common Egyptian's perspective, it is a system that demands their labor and obedience in exchange for a promise of justice and stability, which may or may not be delivered. The priesthood mediates these perspectives, emphasizing both the Pharaoh's duties and the people's obligations.
 *
 * DIRECTIONALITY LOGIC:
 *   The Pharaoh is a beneficiary (d=0.0-0.2) due to the immense legitimacy and power derived from upholding Ma'at, despite the obligations. The priesthood also benefits (d=0.1-0.3) from their central role. Common Egyptians are payers (d=0.7-0.9) as they bear the costs of labor and resources, with limited direct agency. The reciprocity reading implies that their 'trapped' exit option is not absolute, as a Pharaoh's failure could theoretically justify resistance, though practically difficult.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling as pure extraction by emphasizing the genuine coordination function and the reciprocal obligations. It avoids the 'divine mandate' trap by asserting that the Pharaoh is subject to, not identical with, Ma'at. The constraint's mandate (cosmic balance through justice) is still live, preventing it from being a Piton. The potential for resistance, however theoretical, keeps it from being a pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pharaoh_accountability_mechanism,
    'What concrete mechanisms existed for common Egyptians to hold the Pharaoh accountable for failures to uphold Ma''at, beyond divine retribution or rebellion?',
    'Archaeological or textual evidence of formal grievance processes, popular assemblies with real power, or institutional checks on pharaonic authority.',
    'Stronger accountability mechanisms would lower the effective suppression and extractiveness, potentially shifting the classification closer to a Rope. Lack of such mechanisms would confirm higher suppression and extraction, pushing it towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharaoh_accountability_mechanism, empirical, 'The practical enforceability of Ma''at''s reciprocal obligations on the Pharaoh.').

omega_variable(
    reciprocity_vs_divine_mandate,
    'Is the ''reciprocity'' aspect of Ma''at a genuine structural feature, or a rhetorical device to legitimize the ''divine mandate'' of the Pharaoh?',
    'Analysis of historical periods of instability or regime change: did popular unrest or elite challenges explicitly invoke the Pharaoh''s failure to uphold Ma''at as justification, or was it purely a power struggle?',
    'If primarily rhetorical, the constraint''s extractiveness and suppression are higher, and the ''divine mandate'' reading gains strength, potentially reclassifying this as a Snare. If genuinely structural, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_vs_divine_mandate, conceptual, 'The true nature of the reciprocal obligations within Ma''at.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__reciprocity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__reciprocity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(maat_tr_t25, maat_order_principle__reciprocity_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(maat_tr_t50, maat_order_principle__reciprocity_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(maat_tr_t75, maat_order_principle__reciprocity_reading, theater_ratio, 75, 0.22).
narrative_ontology:measurement(maat_tr_t100, maat_order_principle__reciprocity_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__reciprocity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(maat_be_t25, maat_order_principle__reciprocity_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(maat_be_t50, maat_order_principle__reciprocity_reading, base_extractiveness, 50, 0.4).
narrative_ontology:measurement(maat_be_t75, maat_order_principle__reciprocity_reading, base_extractiveness, 75, 0.42).
narrative_ontology:measurement(maat_be_t100, maat_order_principle__reciprocity_reading, base_extractiveness, 100, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__reciprocity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(maat_su_t25, maat_order_principle__reciprocity_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(maat_su_t50, maat_order_principle__reciprocity_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(maat_su_t75, maat_order_principle__reciprocity_reading, suppression_requirement, 75, 0.62).
narrative_ontology:measurement(maat_su_t100, maat_order_principle__reciprocity_reading, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__reciprocity_reading, identity_coordination).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Ma'at Order Principle' kernel. This 'reciprocity reading' emphasizes mutual obligations, contrasting with the 'divine mandate' (Pharaoh embodies Ma'at) and 'distributed maintenance' (all maintain Ma'at) readings. Each reading instantiates a distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
