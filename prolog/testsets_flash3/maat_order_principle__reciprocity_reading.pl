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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: maat_order_principle__reciprocity_reading
 *   human_readable: Ma'at Order Principle (Reciprocity Reading)
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This constraint describes the principle of Ma'at in ancient Egypt,
 *   specifically from a 'reciprocity reading' where the Pharaoh's authority
 *   is conditional on fulfilling obligations to provide justice, stability,
 *   and proper resource distribution. This reading posits a mutual
 *   relationship: the Pharaoh maintains cosmic balance through good
 *   governance, and in return, receives legitimacy and the obedience of the
 *   populace. Failure to uphold these obligations can lead to a loss of
 *   legitimacy and potential unrest. The constraint is classified as a
 *   Tangled Rope due to its genuine coordination function (maintaining order)
 *   coupled with asymmetric extraction (Pharaoh benefits from the system
 *   while commoners bear costs and have limited recourse).
 *
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
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__reciprocity_reading, "Ma'at Order Principle (Reciprocity Reading)").
narrative_ontology:topic_domain(maat_order_principle__reciprocity_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__reciprocity_reading, '817a908c-ed42-457d-b8a7-44353059d1a8').
narrative_ontology:cs_kernel_codification('817a908c-ed42-457d-b8a7-44353059d1a8', formalized).
narrative_ontology:cs_authority_grounding('817a908c-ed42-457d-b8a7-44353059d1a8', lineage).
narrative_ontology:cs_interpretation_layer_present('817a908c-ed42-457d-b8a7-44353059d1a8').
narrative_ontology:cs_reading_relation('817a908c-ed42-457d-b8a7-44353059d1a8', maat_order_principle__divine_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('817a908c-ed42-457d-b8a7-44353059d1a8', maat_order_principle__distributed_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('817a908c-ed42-457d-b8a7-44353059d1a8', foundational, pharaoh_authority_is_conditional).
narrative_ontology:cs_axiom_status(pharaoh_authority_is_conditional, holdable).
narrative_ontology:cs_axiom_grounding('817a908c-ed42-457d-b8a7-44353059d1a8', pharaoh_authority_is_conditional, deontological).
narrative_ontology:cs_axiom('817a908c-ed42-457d-b8a7-44353059d1a8', secondary, cosmic_balance_requires_human_justice).
narrative_ontology:cs_axiom_status(cosmic_balance_requires_human_justice, holdable).
narrative_ontology:cs_axiom_grounding('817a908c-ed42-457d-b8a7-44353059d1a8', cosmic_balance_requires_human_justice, theological).
narrative_ontology:cs_reference_frame('817a908c-ed42-457d-b8a7-44353059d1a8', reciprocal_divine_contract).
narrative_ontology:cs_drift_state('817a908c-ed42-457d-b8a7-44353059d1a8', late_dynastic_period, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('817a908c-ed42-457d-b8a7-44353059d1a8', '').
narrative_ontology:cs_kernel_id(maat_order_principle__reciprocity_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, pharaoh).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, elite_officials).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, commoners).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, local_administrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, local_administrators).
narrative_ontology:constraint_vindicates(maat_order_principle__reciprocity_reading, cosmic_balance_doctrine).
narrative_ontology:constraint_vindicates(maat_order_principle__reciprocity_reading, social_harmony_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The divine ruler, obligated to uphold Ma'at by ensuring justice, stability, and resource distribution. Benefits from the legitimacy and stability provided by Ma'at, but is constrained by its demands. Failure to uphold Ma'at risks cosmic disorder and loss of legitimacy.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, pharaoh, agenda_setter,
    institutional, generational, identity_locked, national).

% Provide labor and resources to the state, expecting justice, protection, and a stable environment in return. Bear the costs of state demands but are beneficiaries of the order Ma'at is supposed to guarantee. If Ma'at is violated, they suffer injustice and instability, potentially leading to unrest.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, commoners, payer,
    powerless, biographical, trapped, local).

% Administer the state on behalf of the Pharaoh, interpreting and enforcing Ma'at in daily governance. Benefit from their position within the hierarchical structure and the stability it provides. They are also responsible for ensuring the Pharaoh's obligations are met.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, elite_officials, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, elite_officials, agenda_setter).

% Implement policies and collect resources at the local level, directly interacting with commoners. They are caught between the demands of the Pharaoh and the needs of the populace, often bearing the immediate costs of maintaining order and resource flow.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, local_administrators, payer,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, local_administrators, beneficiary).

% Guardians of religious doctrine and cosmic knowledge, they interpret Ma'at and advise the Pharaoh. They observe the adherence to Ma'at and can articulate when the balance is threatened, influencing legitimacy without direct enforcement power.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, priestly_class, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the mutual obligations between the ruler and the ruled, ensuring that the Pharaoh's provision of justice and stability is reciprocated by the populace's obedience and contribution, thereby maintaining cosmic and social order.
% TRANSFER_FUNCTION: Transfers legitimacy and divine favor to the Pharaoh in exchange for justice, stability, and resource distribution to the populace. It also transfers labor and resources from commoners to the state in exchange for protection and order.
% ABSENT_VOICES: Disaffected commoners or rebellious factions who might argue that the Pharaoh has failed in their obligations and thus forfeited their right to rule. Their voices are suppressed by the state's enforcement mechanisms and the pervasive ideology of Ma'at.
% DISAPPEARANCE_RATIONALE: If the principle of Ma'at vanished, the entire social and political structure of ancient Egypt would collapse. The Pharaoh's legitimacy would evaporate, leading to widespread chaos, civil unrest, and a breakdown of governance and resource allocation. The cosmic and social order would be perceived as fundamentally broken.
% FOUNDING_PROBLEM: To establish and maintain a stable, just, and prosperous society in a world perceived as inherently chaotic, by aligning human actions with a divine cosmic order.
% FOUNDING_PROBLEM_CORROBORATION: The priestly class and historical texts consistently attest to the ongoing necessity of Ma'at for societal function. While the specific challenges change, the fundamental problem of maintaining order and justice is considered perpetually live, corroborated by the continuous invocation of Ma'at in royal decrees and wisdom literature.
narrative_ontology:disappearance_verdict(maat_order_principle__reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__reciprocity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__reciprocity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.4) because while the Pharaoh benefits significantly, the reciprocal obligations place a ceiling on unchecked extraction; the system is designed to prevent extreme exploitation to maintain balance. Suppression is moderate (0.6) as the state actively enforces order and conformity to Ma'at, but the underlying ideology of reciprocity means outright tyranny is ideologically constrained. Theater ratio is low (0.2) because the rituals and pronouncements of Ma'at are largely functional in reinforcing the social contract, rather than purely performative. The claimed type is Tangled Rope because it genuinely coordinates a complex society while simultaneously enabling a hierarchical structure that extracts from the populace, requiring active enforcement to maintain this balance.
 *
 * PERSPECTIVAL GAP:
 *   From the Pharaoh's perspective, Ma'at is a sacred duty and a source of divine authority, making it appear as a Rope or even a Mountain (divine law). From the commoners' perspective, it is a system that demands their labor and obedience, with the promise of justice often falling short, making it feel more extractive. The 'reciprocity reading' attempts to bridge this gap by emphasizing the conditional nature of the Pharaoh's power, which is not universally accepted.
 *
 * DIRECTIONALITY LOGIC:
 *   The Pharaoh and elite officials are beneficiaries and agenda-setters, as they directly control the interpretation and enforcement of Ma'at and benefit from the stability it provides. Commoners and local administrators are payers, bearing the costs of labor and resources while receiving the promised benefits of order and justice, which are often unevenly distributed. The priestly class acts as an observer, interpreting Ma'at but not directly enforcing or paying its costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'reciprocity reading' prevents mislabeling Ma'at as a pure Snare by highlighting the genuine coordination function and the ideological constraints on the Pharaoh's power. It also prevents mislabeling it as a pure Rope by acknowledging the inherent extraction and the active enforcement required to maintain the asymmetric power structure. The constraint's mandate (cosmic balance through mutual obligation) is still live, but its implementation often drifts towards greater extraction, which this classification captures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pharaoh_accountability_mechanism,
    'What are the effective mechanisms for holding the Pharaoh accountable to the obligations of Ma''at, beyond ideological claims?',
    'Historical analysis of periods of unrest or dynastic change, examining whether these events were explicitly framed as responses to a Pharaoh''s failure to uphold Ma''at, and whether such framing led to actual shifts in governance.',
    'If strong accountability mechanisms existed, the constraint''s effective extractiveness for commoners would be lower, pushing it closer to a Rope. If accountability was purely rhetorical, it would lean more towards a Snare, with the reciprocity claim serving as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharaoh_accountability_mechanism, empirical, 'The practical enforceability of the Pharaoh''s obligations under Ma''at.').

omega_variable(
    reciprocity_vs_divine_mandate,
    'Is the Pharaoh''s authority truly conditional on reciprocity, or is the ''reciprocity'' a rhetorical device for a fundamentally divine and unconditional mandate?',
    'Comparative textual analysis of royal decrees, wisdom literature, and funerary texts, specifically looking for explicit statements or implied justifications for resistance or withdrawal of support in cases of perceived pharaonic failure, versus texts emphasizing unconditional obedience.',
    'If the divine mandate is truly unconditional, this ''reciprocity reading'' would be foreclosed, and the constraint would reclassify towards a Mountain (divine law) or a Snare (unconditional extraction). If reciprocity is a genuine structural feature, the Tangled Rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reciprocity_vs_divine_mandate, conceptual, 'The fundamental nature of the Pharaoh''s authority under Ma''at: conditional or unconditional.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state power, lack of alternatives) or internalized (ideological acceptance of Ma''at''s necessity)?',
    'Analysis of historical records for evidence of active dissent and its suppression versus widespread, uncoerced adherence to Ma''at''s principles. If dissent was rare even in times of hardship, internalized suppression is stronger.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as commoners carry the suppression with them. If purely structural, removing state power would lead to immediate breakdown.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for Ma''at adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__reciprocity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__reciprocity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__reciprocity_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__reciprocity_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(maat_tr_t60, maat_order_principle__reciprocity_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(maat_tr_t80, maat_order_principle__reciprocity_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(maat_tr_t100, maat_order_principle__reciprocity_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__reciprocity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__reciprocity_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__reciprocity_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(maat_be_t60, maat_order_principle__reciprocity_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(maat_be_t80, maat_order_principle__reciprocity_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(maat_be_t100, maat_order_principle__reciprocity_reading, base_extractiveness, 100, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__reciprocity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__reciprocity_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(maat_su_t40, maat_order_principle__reciprocity_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(maat_su_t60, maat_order_principle__reciprocity_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(maat_su_t80, maat_order_principle__reciprocity_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(maat_su_t100, maat_order_principle__reciprocity_reading, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__reciprocity_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
