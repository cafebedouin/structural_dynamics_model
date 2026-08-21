% ============================================================================
% CONSTRAINT STORY: maat_order_principle__divine_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__divine_mandate_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: maat_order_principle__divine_mandate_reading
 *   human_readable: Ma'at Order Principle (Divine Mandate Reading)
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This constraint represents the 'divine mandate' reading of Ma'at in
 *   ancient Egypt, where cosmic and social order flows from a divine source
 *   through the Pharaoh to society. Under this reading, the Pharaoh embodies
 *   Ma'at and, by definition, cannot violate it. His actions are inherently
 *   just and necessary for maintaining cosmic balance. This framing justifies
 *   the Pharaoh's absolute authority and the extraction of resources and
 *   labor from the populace as a cosmic necessity, while actively suppressing
 *   any alternative interpretations that would imply royal accountability or
 *   distributed responsibility for Ma'at.
 *
 * KEY AGENTS:
 *   - Pharaoh: Primary beneficiary and agenda-setter (institutional/arbitrage) — source of Ma'at, cannot be constrained.
 *   - Priestly Elite: Secondary beneficiary (institutional/constrained) — legitimizes Pharaoh, benefits from the order.
 *   - Egyptian Populace: Primary target (powerless/trapped) — bears costs, no recourse.
 *   - Scribal Bureaucracy: Secondary target/beneficiary (moderate/constrained) — enforces rule, but also subject to it.
 *   - Alternative Interpretations: Excluded voice (powerless/identity_locked) — suppressed readings that challenge royal authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, 0.85).
domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, 0.92).
domain_priors:theater_ratio(maat_order_principle__divine_mandate_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__divine_mandate_reading, mountain).
narrative_ontology:human_readable(maat_order_principle__divine_mandate_reading, "Ma'at Order Principle (Divine Mandate Reading)").
narrative_ontology:topic_domain(maat_order_principle__divine_mandate_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__divine_mandate_reading).
domain_priors:emerges_naturally(maat_order_principle__divine_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__divine_mandate_reading, 'b9bc1e00-df9b-4b48-8458-3a4ba6b41c57').
narrative_ontology:cs_kernel_codification('b9bc1e00-df9b-4b48-8458-3a4ba6b41c57', formalized).
narrative_ontology:cs_authority_grounding('b9bc1e00-df9b-4b48-8458-3a4ba6b41c57', lineage).
narrative_ontology:cs_interpretation_layer_present('b9bc1e00-df9b-4b48-8458-3a4ba6b41c57').
narrative_ontology:cs_reading_relation('b9bc1e00-df9b-4b48-8458-3a4ba6b41c57', maat_order_principle__reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('b9bc1e00-df9b-4b48-8458-3a4ba6b41c57', maat_order_principle__distributed_maintenance_reading, forecloses).
narrative_ontology:cs_axiom('b9bc1e00-df9b-4b48-8458-3a4ba6b41c57', foundational, pharaoh_is_maat_incarnate).
narrative_ontology:cs_axiom_status(pharaoh_is_maat_incarnate, holdable).
narrative_ontology:cs_axiom_grounding('b9bc1e00-df9b-4b48-8458-3a4ba6b41c57', pharaoh_is_maat_incarnate, theological).
narrative_ontology:cs_axiom('b9bc1e00-df9b-4b48-8458-3a4ba6b41c57', foundational, royal_action_is_divinely_justified).
narrative_ontology:cs_axiom_status(royal_action_is_divinely_justified, holdable).
narrative_ontology:cs_axiom_grounding('b9bc1e00-df9b-4b48-8458-3a4ba6b41c57', royal_action_is_divinely_justified, theological).
narrative_ontology:cs_reference_frame('b9bc1e00-df9b-4b48-8458-3a4ba6b41c57', divine_pharaonic_absolutism).
narrative_ontology:cs_drift_state('b9bc1e00-df9b-4b48-8458-3a4ba6b41c57', historical_continuity, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b9bc1e00-df9b-4b48-8458-3a4ba6b41c57', '').
narrative_ontology:cs_kernel_id(maat_order_principle__divine_mandate_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, pharaoh).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, priestly_elite).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, egyptian_populace).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, scribal_bureaucracy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, scribal_bureaucracy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The divine ruler, believed to embody Ma'at and be its source. Cannot violate Ma'at by definition, as his actions are inherently just. Benefits from absolute authority and the resources of the state, justified as maintaining cosmic order.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, pharaoh, agenda_setter,
    institutional, generational, arbitrage, national).

% Interprets divine will and Ma'at, legitimizing the Pharaoh's rule. Benefits from their privileged position, access to resources, and social status within the established order. Their power is derived from and dependent on the Pharaoh's divine mandate.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, priestly_elite, beneficiary,
    institutional, generational, constrained, national).

% Subject to the Pharaoh's absolute rule and the demands of the state (labor, taxes, military service). Bears the costs of the system, with no formal recourse or ability to challenge the divine mandate. Their well-being is theoretically tied to Ma'at, but their agency is minimal.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, egyptian_populace, payer,
    powerless, immediate, trapped, national).

% Administers the state and enforces the Pharaoh's decrees. While benefiting from their literacy and position, they are ultimately instruments of the Pharaoh's will and subject to his absolute authority. They bear the burden of implementing policies that may be extractive.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, scribal_bureaucracy, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, scribal_bureaucracy, beneficiary).

% Any alternative understandings of Ma'at that would imply distributed responsibility, mutual obligation, or royal accountability are suppressed. These interpretations are not part of the public discourse and are actively marginalized by the state and religious authorities.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, alternative_interpretations, excluded,
    powerless, generational, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__divine_mandate_reading, pharaoh).
narrative_ontology:fixing_cost_class(maat_order_principle__divine_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable cosmic and social order, ensuring the prosperity and continuity of Egypt by centralizing authority in the divine Pharaoh, who is believed to maintain Ma'at through his very existence and actions.
% TRANSFER_FUNCTION: Transfers absolute authority, resources, and labor from the Egyptian populace to the Pharaoh and the priestly elite, in exchange for the perceived maintenance of cosmic balance, stability, and protection from chaos.
% ABSENT_VOICES: Those who would advocate for a more reciprocal relationship between ruler and ruled, or for a distributed responsibility for upholding Ma'at, are absent. Their perspectives are suppressed by the divine mandate and the state's enforcement mechanisms.
% DISAPPEARANCE_RATIONALE: If the divine mandate of Ma'at vanished overnight, the entire political, social, and religious structure of ancient Egypt would collapse. The Pharaoh's legitimacy would evaporate, leading to widespread chaos, civil unrest, and the disintegration of the state's administrative and religious institutions.
% FOUNDING_PROBLEM: To establish and maintain cosmic and social order in a world perceived as constantly threatened by chaos (Isfet), ensuring the prosperity, stability, and survival of Egypt through a divinely sanctioned ruler.
% FOUNDING_PROBLEM_CORROBORATION: The Pharaoh and priestly elite consistently attest that the founding problem of maintaining cosmic order against chaos is perpetually live. While external historical analysis can confirm the societal need for order, the specific divine mandate as the *only* solution is primarily asserted by the benefiting parties, with no independent corroboration outside this framework.
narrative_ontology:disappearance_verdict(maat_order_principle__divine_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__divine_mandate_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__divine_mandate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(maat_order_principle__divine_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__divine_mandate_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__divine_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, ExtMetricName, E),
    domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(maat_order_principle__divine_mandate_reading),
    narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(maat_order_principle__divine_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `claimed_type` is 'mountain' because this reading presents Ma'at as an unchangeable, natural law embodied by the Pharaoh. However, the metrics reflect its operational reality: `extractiveness` is very high (0.85) as the system channels immense resources to the Pharaoh and elite, justified by cosmic necessity. `Suppression` is also very high (0.92) because the entire ideological and state apparatus actively prevents any challenge to the Pharaoh's divine authority or alternative interpretations of Ma'at. `Theater_ratio` is low (0.10) because the belief in the Pharaoh's divine role and the cosmic necessity of his rule was deeply ingrained and genuinely enforced, not merely performative. `Accessibility_collapse` is high (0.90) as the divine mandate leaves virtually no conceptual or practical alternatives for the populace. `Resistance` is low (0.05) due to the overwhelming ideological and coercive power of the state.
 *
 * PERSPECTIVAL GAP:
 *   From the Pharaoh's and priestly elite's perspective, this is a genuine Mountain—an immutable cosmic principle that they embody and uphold. From the perspective of the Egyptian populace, it functions as a Snare, extracting labor and resources under the guise of divine order, with no viable exit. The engine's classification will highlight this divergence between the claimed type and the operational metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The Pharaoh is the ultimate beneficiary (d=0.0) as he is the source of Ma'at and collects all its benefits without being constrained by it. The priestly elite are also beneficiaries (d near 0.15) as they derive power and status from legitimizing the Pharaoh. The Egyptian populace are the primary targets (d=1.0) as they bear the full costs of extraction with no exit. The scribal bureaucracy is a target (d near 0.8) as they enforce the system but are also subject to the Pharaoh's absolute will. Alternative interpretations are excluded and suppressed, making them targets of the constraint's enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a highly extractive and suppressive system as a benign 'mountain' or 'rope' simply because it claims divine justification. By measuring high extractiveness and suppression, the framework identifies the operational reality of the constraint, regardless of its self-proclaimed naturalness. The 'false summit mountain' detection mechanism will flag this constraint for its combination of a mountain claim and identifiable beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_vs_political_construct,
    'Is the Pharaoh''s embodiment of Ma''at a genuine cosmic principle, or a political and religious construct designed to legitimize absolute power and extraction?',
    'Comparative historical analysis of other ancient civilizations'' legitimizing ideologies, archaeological evidence of shifts in royal ideology over time, and textual analysis of dissenting voices (if any exist and are recovered).',
    'If primarily a construct, the constraint''s ''emerges_naturally'' claim is false, reclassifying it from a claimed Mountain to a Snare or Tangled Rope, and increasing its effective extractiveness by removing the ''natural law'' discount.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_vs_political_construct, conceptual, 'Ambiguity between natural law and political construction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (divine authority, lack of alternatives) or internalized (deep belief in Pharaoh''s divinity and the cosmic necessity of his rule)?',
    'Analysis of historical periods of unrest or dynastic change: if the system rapidly collapses when the structural authority is challenged, it suggests less internalized suppression. If belief persists even after structural collapse, it suggests stronger internalization.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the populace carries the suppression with them, making resistance even more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem of maintaining cosmic order against chaos truly ''live'', or has the constraint''s function shifted primarily to maintaining the Pharaoh''s power and the elite''s privilege?',
    'Historical analysis of periods of stability vs. instability: if the system consistently fails to deliver ''order'' during periods of high extraction, it suggests obsolescence. Examination of contemporary accounts (if available) of the populace''s perception of ''order'' vs. ''burden''.',
    'If the founding problem is ''dead'' or primarily a cover, the constraint''s justification is undermined, reinforcing its classification as a Snare and increasing the perceived injustice of its extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the constraint''s original mandate is still relevant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__divine_mandate_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__divine_mandate_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__divine_mandate_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__divine_mandate_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(maat_tr_t60, maat_order_principle__divine_mandate_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(maat_tr_t80, maat_order_principle__divine_mandate_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(maat_tr_t100, maat_order_principle__divine_mandate_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__divine_mandate_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__divine_mandate_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__divine_mandate_reading, base_extractiveness, 40, 0.83).
narrative_ontology:measurement(maat_be_t60, maat_order_principle__divine_mandate_reading, base_extractiveness, 60, 0.84).
narrative_ontology:measurement(maat_be_t80, maat_order_principle__divine_mandate_reading, base_extractiveness, 80, 0.85).
narrative_ontology:measurement(maat_be_t100, maat_order_principle__divine_mandate_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__divine_mandate_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__divine_mandate_reading, suppression_requirement, 20, 0.89).
narrative_ontology:measurement(maat_su_t40, maat_order_principle__divine_mandate_reading, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(maat_su_t60, maat_order_principle__divine_mandate_reading, suppression_requirement, 60, 0.91).
narrative_ontology:measurement(maat_su_t80, maat_order_principle__divine_mandate_reading, suppression_requirement, 80, 0.92).
narrative_ontology:measurement(maat_su_t100, maat_order_principle__divine_mandate_reading, suppression_requirement, 100, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__divine_mandate_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
