% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Blood-Feud Obligations as Destructive Extraction Cycle
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the extraction_cycle_reading of the
 *   contested feud_obligation_kernel. Blood-feud obligations are read here
 *   not as self-enforcing justice or divine-law violation, but as a material
 *   cycle of destructive extraction: kin groups deplete their own productive
 *   capacity in retaliatory violence while royal authority consolidates a
 *   monopoly on legitimate violence and tax extraction against the backdrop
 *   of that chaos. The claim/metric independence is maintainedâthe reading
 *   claims a tangled rope (coordination function recognized but
 *   asymmetrically extractive) while the metrics describe severe extraction
 *   and high suppression.
 *
 * KEY AGENTS:
 *   - royal_authority: Primary beneficiary (institutional/arbitrage) â captures extraction via monopoly legitimization and taxation
 *   - feud_participants: Primary target (moderate/identity_locked) â bears resource depletion, mortality, and honor-bound obligation
 *   - kinship_elites: Agenda setter (organized/mobile) â enforces obligations and extracts status from feud leadership
 *   - noncombatant_kin: Secondary target (powerless/trapped) â suffers diffuse costs without agency or voice
 *   - christian_clergy: Excluded voice (organized/constrained) â offers pacification alternative but structurally marginal
 *   - merchant_class: Excluded voice (moderate/mobile) â commerce disrupted by territorial instability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.85).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.8).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Blood-Feud Obligations as Destructive Extraction Cycle").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, '74f01ea3-1a33-4a89-b281-c83ba2cb9f6e').
narrative_ontology:cs_kernel_codification('74f01ea3-1a33-4a89-b281-c83ba2cb9f6e', distributed).
narrative_ontology:cs_authority_grounding('74f01ea3-1a33-4a89-b281-c83ba2cb9f6e', practice).
narrative_ontology:cs_interpretation_layer_present('74f01ea3-1a33-4a89-b281-c83ba2cb9f6e').
narrative_ontology:cs_reading_relation('74f01ea3-1a33-4a89-b281-c83ba2cb9f6e', feud_obligation_kernel__stateless_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('74f01ea3-1a33-4a89-b281-c83ba2cb9f6e', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('74f01ea3-1a33-4a89-b281-c83ba2cb9f6e', foundational, blood_feud_depletes_productive_capacity).
narrative_ontology:cs_axiom_status(blood_feud_depletes_productive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('74f01ea3-1a33-4a89-b281-c83ba2cb9f6e', blood_feud_depletes_productive_capacity, empirically_contingent).
narrative_ontology:cs_axiom('74f01ea3-1a33-4a89-b281-c83ba2cb9f6e', foundational, royal_monopoly_requires_decentralized_violence).
narrative_ontology:cs_axiom_status(royal_monopoly_requires_decentralized_violence, holdable).
narrative_ontology:cs_axiom_grounding('74f01ea3-1a33-4a89-b281-c83ba2cb9f6e', royal_monopoly_requires_decentralized_violence, empirically_contingent).
narrative_ontology:cs_reference_frame('74f01ea3-1a33-4a89-b281-c83ba2cb9f6e', kinship_honor_and_retributive_justice).
narrative_ontology:cs_drift_state('74f01ea3-1a33-4a89-b281-c83ba2cb9f6e', royal_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('74f01ea3-1a33-4a89-b281-c83ba2cb9f6e', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, royal_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, kinship_elites).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, feud_participants).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, noncombatant_kin).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derives legitimacy and extractive capacity from its claimed monopoly on legitimate violence; benefits when decentralized kinship violence appears chaotic and self-depleting, making centralized taxation and adjudication seem indispensable.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, royal_authority, beneficiary,
    institutional, generational, arbitrage, national).

% Heads of kinship groups who enforce blood-debt obligations, adjudicate honor disputes, and organize retaliatory raids; their local authority and generational status depend on maintaining the feud cycle.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, kinship_elites, agenda_setter,
    organized, generational, mobile, regional).

% Adult males obligated to avenge kin deaths or injuries; bear direct costs in livestock, property, labor time, and mortality; productive capacity is systematically diverted from agriculture and trade to feud preparation and execution.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feud_participants, payer,
    moderate, biographical, identity_locked, local).

% Women, children, and elderly within feuding kin groups who suffer loss of providers, destruction of dwellings and livestock, and social exclusion but lack voice in feud decisions or adjudication.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, noncombatant_kin, payer,
    powerless, biographical, trapped, local).

% Preaches divine prohibition of vengeance and offers sanctuary or arbitration; structurally marginal to kinship enforcement networks and often ignored or threatened when intervening in blood disputes.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, christian_clergy, excluded,
    organized, generational, constrained, regional).

% Long-distance traders and artisans whose commerce is disrupted by territorial instability and predatory violence; would benefit from consolidated justice but excluded from kinship-based adjudication.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, merchant_class, excluded,
    moderate, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__extraction_cycle_reading, royal_authority).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__extraction_cycle_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decentralized mechanism for regulating lethal inter-group violence and enforcing social accountability in the absence of centralized judicial institutions.
% TRANSFER_FUNCTION: Moves productive capacityâlivestock, land, labor time, and livesâfrom feud participants and their kin groups toward royal authority and kinship elites, via depletion cycles that delegitimize decentralized violence and consolidate tax-monopoly claims.
% ABSENT_VOICES: Christian clergy advocating divine-law pacification, merchant classes seeking territorial stability, and noncombatant kin who bear depletion costs but are excluded from feud adjudication.
% DISAPPEARANCE_RATIONALE: If blood-feud obligations disappeared, kin groups would shift to compensation, arbitration, or state courts; the depletion cycle would halt; royal authority would lose the chaotic backdrop that legitimizes its monopoly on violence and tax extraction; territorial consolidation would accelerate.
% FOUNDING_PROBLEM: Absence of centralized enforcement in stateless or weak-state contexts, leaving lethal violence unregulated and social order unstable.
% FOUNDING_PROBLEM_CORROBORATION: Royal authority asserts the founding problem remains live to justify centralization. Comparative anthropological evidence from outside the benefiting parties shows that kinship-based feud systems persist even where centralized courts exist, suggesting the constraint's function has shifted from coordination to extraction. Clergy and chroniclers from non-kinship seats attest the depletion costs.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__extraction_cycle_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__extraction_cycle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__extraction_cycle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feud_obligation_kernel__extraction_cycle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__extraction_cycle_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the constraint systematically moves productive capacity from kin groups to royal authority through depletion cycles. Suppression is high (0.80) because kinship enforcement and identity-locking suppress exits into peacemaking or state arbitration. Theater_ratio is moderate (0.40) because feud violence is partly performative (honor displays, saga-narration) but produces genuine mortality and economic loss. Accessibility_collapse is high (0.70): once a kin group is feud-bound, alternatives like compensation courts or migration collapse under social ostracism. Resistance is moderate (0.45): church and merchant voices resist, but kinship participants resist exit rather than the constraint itself. Measurements trace rising extractiveness as royal authority consolidates and theater rises with ritualization.
 *
 * PERSPECTIVAL GAP:
 *   The royal authority seat computes the constraint as a useful backdrop for territorial consolidation and tax extraction; the feud participant seat computes it as an identity-locked trap that depletes household wealth and life. The kinship elite seat experiences it as a source of local authority and generational status maintenance. These divergences arise from the same structural facts: who monopolizes the legitimization of violence, who is bound by kinship honor, and who is physically trapped in the locality.
 *
 * DIRECTIONALITY LOGIC:
 *   Royal authority is the structural beneficiary (gains monopoly rent and tax legitimacy; d near the beneficiary end). Feud participants and noncombatant kin are the targets (bear mortality and resource depletion; d near the target end). Kinship elites sit near the agenda-setter end: they administer the constraint and gain status, but their power is bounded by the same honor code (moderate d). Christian clergy and merchants are excluded from the arrangement entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâregulating violence in the absence of centralized institutionsâis dead in contexts where royal authority already exists and extracts taxes. The constraint persists not because statelessness persists, but because the cycle of depletion legitimizes royal monopoly. This prevents mislabeling the constraint as a Rope (pure coordination) by showing the coordination function is either obsolete or cover for extraction; the R5 genealogy interview flags the mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges, indicating the constraint is maintained for reasons other than its original purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is this constraint a destructive extraction cycle (this reading), a self-enforcing coordination mechanism (stateless_coordination_reading), or a violation of divine law requiring pacification (christianized_pacification_reading)?',
    'Cross-reading comparison of empirical patterns: does feud persistence correlate with state weakness (coordination) or with state consolidation (extraction)? Do feud cycles deplete or sustain kinship wealth?',
    'Resolution determines whether the constraint is classified as tangled_rope/extraction or rope/coordination or commitment-system/church-authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural ambiguity between extraction, coordination, and theological readings of the same kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (kinship leaders enforcing participation through physical threat and resource denial) or internalized (honor-shame identity fusion making exit unthinkable)?',
    'Post-conversion or post-migration suppression trajectory: if obligations persist after structural enforcement is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, and the constraint operates as identity_coordination rather than pure enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in kinship-bound violence.').

omega_variable(
    royal_extraction_intentionality,
    'Does royal authority actively cultivate feud obligations as an extraction strategy, or passively benefit from a self-sustaining kinship cycle?',
    'Historical documentation of royal policy toward feuding: edicts prohibiting versus permitting feud violence, tax records showing extraction from depleted regions.',
    'If active cultivation, directionality of royal authority shifts toward agenda_setter and extraction is deliberate; if passive benefit, the constraint is an emergent tangled rope with diffuse extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(royal_extraction_intentionality, empirical, 'Intentionality of royal authority in maintaining the feud cycle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(feud_tr_t20, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(feud_tr_t60, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(feud_tr_t80, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 80, 0.48).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(feud_be_t20, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(feud_be_t60, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 60, 0.78).
narrative_ontology:measurement(feud_be_t80, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 80, 0.84).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(feud_su_t20, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(feud_su_t40, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(feud_su_t60, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(feud_su_t80, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 80, 0.8).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 100, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__extraction_cycle_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
