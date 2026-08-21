% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__sovereign_override_reading, []).

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
 *   constraint_id: salic_prohibition__sovereign_override_reading
 *   human_readable: Salic Law (Sovereign Override Reading)
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint represents the reading of Salic Law as a revocable
 *   positive law, subject to modification by sovereign legislative authority
 *   (e.g., through a Pragmatic Sanction). It allows for female succession
 *   under specific conditions, prioritizing dynastic continuity and the
 *   sovereign's will over strict adherence to ancient Frankish custom.
 *   Challengers to such a modified succession are viewed as rebels against
 *   legitimate authority, often leading to defensive wars to protect the
 *   dynastic line. This is one reading of the 'salic_prohibition' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.4).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.7).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Salic Law (Sovereign Override Reading)").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__sovereign_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, '23a62f87-13b8-46c2-88f1-07ab2425137a').
narrative_ontology:cs_kernel_codification('23a62f87-13b8-46c2-88f1-07ab2425137a', formalized).
narrative_ontology:cs_authority_grounding('23a62f87-13b8-46c2-88f1-07ab2425137a', lineage).
narrative_ontology:cs_interpretation_layer_present('23a62f87-13b8-46c2-88f1-07ab2425137a').
narrative_ontology:cs_reading_relation('23a62f87-13b8-46c2-88f1-07ab2425137a', salic_prohibition__immutable_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('23a62f87-13b8-46c2-88f1-07ab2425137a', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('23a62f87-13b8-46c2-88f1-07ab2425137a', foundational, sovereign_legislative_supremacy_in_dynastic_law).
narrative_ontology:cs_axiom_status(sovereign_legislative_supremacy_in_dynastic_law, holdable).
narrative_ontology:cs_axiom_grounding('23a62f87-13b8-46c2-88f1-07ab2425137a', sovereign_legislative_supremacy_in_dynastic_law, conventional).
narrative_ontology:cs_axiom('23a62f87-13b8-46c2-88f1-07ab2425137a', foundational, dynastic_continuity_trumps_ancient_custom).
narrative_ontology:cs_axiom_status(dynastic_continuity_trumps_ancient_custom, holdable).
narrative_ontology:cs_axiom_grounding('23a62f87-13b8-46c2-88f1-07ab2425137a', dynastic_continuity_trumps_ancient_custom, instrumental).
narrative_ontology:cs_reference_frame('23a62f87-13b8-46c2-88f1-07ab2425137a', pragmatic_sanction_framework).
narrative_ontology:cs_drift_state('23a62f87-13b8-46c2-88f1-07ab2425137a', contemporary_constitutional_monarchy_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('23a62f87-13b8-46c2-88f1-07ab2425137a', '').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, ruling_dynasty).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, loyalist_nobility).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, female_heirs).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, rival_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The reigning royal family, whose succession is governed by this interpretation of Salic Law. They benefit from the stability of a clear, albeit modified, succession rule and the ability to secure their lineage through sovereign acts like the Pragmatic Sanction. They actively enforce this reading against challengers.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, ruling_dynasty, agenda_setter,
    institutional, generational, constrained, national).

% Nobles and powerful families whose status and lands are tied to the ruling dynasty. They benefit from the continuity and legitimacy provided by a clear succession rule, even if it's subject to sovereign modification. They provide military and political support to enforce this reading.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, loyalist_nobility, beneficiary,
    organized, generational, constrained, national).

% Princesses and other female members of the royal family who might otherwise inherit the throne under purely cognatic succession. Under this reading, their right to rule is contingent on a sovereign act, rather than inherent, making them subject to the constraint's modification.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, female_heirs, payer,
    powerless, biographical, identity_locked, national).

% Other dynastic lines or factions who might claim the throne based on alternative interpretations of succession law (e.g., strict male-only Salic Law or purely cognatic succession). They bear the cost of being excluded by the sovereign's legislative authority and often resort to rebellion or war.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, rival_claimants, payer,
    powerful, generational, constrained, regional).

% Neighboring states and empires who observe the succession disputes, potentially intervening to support or oppose claimants based on their own geopolitical interests. Their recognition or non-recognition of the sovereign's override can impact the stability of the ruling dynasty.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, foreign_powers, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit flexible, line of succession for the monarchy, preventing internal power struggles and external interference by allowing the sovereign to legislate on dynastic matters.
% TRANSFER_FUNCTION: Transfers the right to rule, and the associated power and resources, from potential female heirs (under strict Salic Law) or rival claimants (under other interpretations) to the designated heir by sovereign decree, ensuring dynastic continuity.
% ABSENT_VOICES: Strict Salicists who believe the law is immutable and divinely ordained are suppressed by the sovereign's legislative power; they would argue against any modification allowing female succession. Pure cognatic succession advocates are also excluded, as their claims are subject to sovereign discretion.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legitimacy of sovereign acts like the Pragmatic Sanction would collapse, plunging the monarchy into severe succession crises. Dynastic wars would erupt as rival claimants (both male-only and purely cognatic) pressed their cases, leading to widespread political instability and potential state collapse.
% FOUNDING_PROBLEM: The need to ensure dynastic continuity and prevent succession crises, particularly when direct male lines failed, while still acknowledging the historical precedent of Salic Law.
% FOUNDING_PROBLEM_CORROBORATION: Historians and constitutional scholars attest to the historical necessity of such sovereign acts to preserve dynastic stability in the face of Salic Law's limitations. The continued existence of monarchies with modified succession laws corroborates the problem's ongoing relevance.
narrative_ontology:disappearance_verdict(salic_prohibition__sovereign_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__sovereign_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__sovereign_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(salic_prohibition__sovereign_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__sovereign_override_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__sovereign_override_reading_tests).
:- end_tests(salic_prohibition__sovereign_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.4) is moderate, as it still limits female heirs' inherent rights but provides a mechanism for their eventual succession. Suppression (0.7) is high because this reading requires active enforcement against both strict Salicists and purely cognatic claimants, often through military means. Theater ratio (0.2) is low, as the sovereign acts are genuine exercises of power, not mere performance. The claimed type is Tangled Rope because it coordinates dynastic stability while extracting from those whose claims are overridden by sovereign decree.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ruling dynasty, this is a necessary and legitimate adaptation of law to ensure stability. From the perspective of strict Salicists or rival claimants, it is an illegitimate usurpation of fundamental law. The engine's classification will reflect the structural asymmetry of power and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The ruling dynasty and loyalist nobility are beneficiaries, as this reading secures their power and legitimacy. Female heirs and rival claimants are payers, as their succession rights are either conditional or entirely denied by sovereign legislative action. Foreign powers act as observers, their involvement contingent on their own strategic interests.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_sovereign_override,
    'Is the sovereign''s legislative authority truly capable of overriding fundamental dynastic law, or is such an act merely a temporary political maneuver lacking true constitutional force?',
    'Long-term historical analysis of the stability of dynasties established by such overrides, and the degree of internal and external recognition they received without continuous military enforcement.',
    'If such overrides are deemed constitutionally weak, the constraint''s legitimacy is lower, increasing its effective extractiveness and suppression, potentially reclassifying it closer to a Snare. If strong, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_sovereign_override, conceptual, 'The constitutional force of sovereign acts to modify dynastic law.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (military force, legal exclusion) or internalized (acceptance of sovereign authority by potential challengers)?',
    'Post-conflict analysis: if challenges persist even after military defeat, internalized suppression is low. If challenges cease, structural suppression is effective. If challenges are rare, internalized acceptance is high.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as challengers self-limit. If purely structural, the constraint is more brittle and requires constant active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dynastic challenges.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__sovereign_override_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sali_tr_t20, salic_prohibition__sovereign_override_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(sali_tr_t40, salic_prohibition__sovereign_override_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(sali_tr_t60, salic_prohibition__sovereign_override_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(sali_tr_t80, salic_prohibition__sovereign_override_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement(sali_tr_t100, salic_prohibition__sovereign_override_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__sovereign_override_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sali_be_t20, salic_prohibition__sovereign_override_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(sali_be_t40, salic_prohibition__sovereign_override_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(sali_be_t60, salic_prohibition__sovereign_override_reading, base_extractiveness, 60, 0.39).
narrative_ontology:measurement(sali_be_t80, salic_prohibition__sovereign_override_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(sali_be_t100, salic_prohibition__sovereign_override_reading, base_extractiveness, 100, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__sovereign_override_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(sali_su_t20, salic_prohibition__sovereign_override_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(sali_su_t40, salic_prohibition__sovereign_override_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(sali_su_t60, salic_prohibition__sovereign_override_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(sali_su_t80, salic_prohibition__sovereign_override_reading, suppression_requirement, 80, 0.69).
narrative_ontology:measurement(sali_su_t100, salic_prohibition__sovereign_override_reading, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__sovereign_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'salic_prohibition' kernel. This 'sovereign_override_reading' views Salic Law as revocable positive law, distinct from the 'immutable_mandate_reading' (irrevocable natural law) and the 'cognatic_reversion_reading' (Frankish anachronism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
