% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__sacral_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__sacral_fidelity_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: lycurgan_laws__sacral_fidelity_reading
 *   human_readable: Lycurgan Laws as Sacred, Unchangeable Divine Ordinance
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sacral fidelity' reading of the
 *   Lycurgan laws, which views them as a divinely ordained, unchangeable
 *   constitutional order requiring absolute adherence. From this perspective,
 *   the laws are a 'Mountain' — a fundamental, natural (or super-natural)
 *   feature of Spartan reality, not a human construct. Any decline in
 *   Sparta's fortunes is attributed to external pressures or the moral
 *   failings of its citizens, rather than any inherent flaw or inflexibility
 *   in the laws themselves. The laws are seen as providing a stable,
 *   virtuous, and militarily superior society, with any personal hardship
 *   being a necessary component of this divine order.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, 0.15).
domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, 0.85).
domain_priors:theater_ratio(lycurgan_laws__sacral_fidelity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__sacral_fidelity_reading, mountain).
narrative_ontology:human_readable(lycurgan_laws__sacral_fidelity_reading, "Lycurgan Laws as Sacred, Unchangeable Divine Ordinance").
narrative_ontology:topic_domain(lycurgan_laws__sacral_fidelity_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:requires_active_enforcement(lycurgan_laws__sacral_fidelity_reading).
domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__sacral_fidelity_reading, 'dcbeedb7-a213-4912-aae6-8b2deec856d1').
narrative_ontology:cs_kernel_codification('dcbeedb7-a213-4912-aae6-8b2deec856d1', fixed_text).
narrative_ontology:cs_authority_grounding('dcbeedb7-a213-4912-aae6-8b2deec856d1', lineage).
narrative_ontology:cs_interpretation_layer_present('dcbeedb7-a213-4912-aae6-8b2deec856d1').
narrative_ontology:cs_reading_relation('dcbeedb7-a213-4912-aae6-8b2deec856d1', lycurgan_laws__demographic_trap_reading, forecloses).
narrative_ontology:cs_reading_relation('dcbeedb7-a213-4912-aae6-8b2deec856d1', lycurgan_laws__adaptive_fiction_reading, forecloses).
narrative_ontology:cs_axiom('dcbeedb7-a213-4912-aae6-8b2deec856d1', foundational, lycurgan_laws_divinely_ordained).
narrative_ontology:cs_axiom_status(lycurgan_laws_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('dcbeedb7-a213-4912-aae6-8b2deec856d1', lycurgan_laws_divinely_ordained, theological).
narrative_ontology:cs_axiom('dcbeedb7-a213-4912-aae6-8b2deec856d1', foundational, absolute_adherence_is_virtue).
narrative_ontology:cs_axiom_status(absolute_adherence_is_virtue, holdable).
narrative_ontology:cs_axiom_grounding('dcbeedb7-a213-4912-aae6-8b2deec856d1', absolute_adherence_is_virtue, deontological).
narrative_ontology:cs_reference_frame('dcbeedb7-a213-4912-aae6-8b2deec856d1', divine_immutable_order).
narrative_ontology:cs_drift_state('dcbeedb7-a213-4912-aae6-8b2deec856d1', historical_decline_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dcbeedb7-a213-4912-aae6-8b2deec856d1', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartan_citizens).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, gerousia_ephors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, spartan_citizens).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, divine_mandate_theory).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, spartan_virtue_ideal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by absolute adherence to the Lycurgan laws, which dictate every aspect of life from birth to death. They bear the severe personal costs of the agoge (military training) and communal living, but are believed to benefit from the resulting stability, virtue, and military superiority of Sparta. Their identity is fused with the Lycurgan system.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_citizens, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, spartan_citizens, beneficiary).

% The governing bodies responsible for interpreting, enforcing, and upholding the Lycurgan laws. They derive their authority and legitimacy from their role as guardians of this divine order. They benefit from the stability and power the system confers upon them, and actively suppress any deviation from the established norms.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, gerousia_ephors, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, gerousia_ephors, beneficiary).

% The enslaved population of Laconia, whose labor supports the Spartan citizen body. They are not considered part of the Lycurgan system's beneficiaries or participants, but are essential to its economic function. They bear extreme costs and have no voice or means of exit from the system.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, helots, excluded,
    powerless, generational, trapped, local).

% Historians and political theorists who analyze the Lycurgan system from an external, critical perspective. They are not subject to the laws but seek to understand their structure, function, and historical impact, often contrasting the Spartan ideal with its practical realities.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, foreign_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a perfectly ordered, virtuous, and militarily superior society by dictating every aspect of citizen life, ensuring unity, discipline, and freedom from corruption and decadence.
% TRANSFER_FUNCTION: Transfers absolute obedience, personal sacrifice, and labor from Spartan citizens to the Lycurgan system, and from Helots to the Spartan state, in exchange for social stability, military prowess, and the maintenance of a divinely ordained order.
% ABSENT_VOICES: The Helots, who would undoubtedly object to their subjugation and exploitation. Also, any Spartan citizens who might have questioned the divine origin or immutability of the laws, or sought personal wealth or luxury, whose dissent was actively suppressed by the system.
% DISAPPEARANCE_RATIONALE: If the Lycurgan laws and their enforcement vanished overnight, Spartan society as it existed would immediately collapse. The unique social structure, military discipline, and communal living would dissolve, leading to a complete reorganization of political, economic, and social life, likely with significant internal conflict and external vulnerability.
% FOUNDING_PROBLEM: To prevent the moral decay, factionalism, and military weakness that plagued other Greek city-states, by establishing a system of absolute virtue, equality among citizens, and unwavering military focus, believed to be divinely inspired.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the sacral fidelity reading, including ancient Spartan authorities and later admirers, attest that the founding problem of maintaining virtue and order is perpetually live. They would argue that any historical decline was due to external pressures or individual moral failings, not the inherent design of the laws themselves. No corroboration from outside the benefiting parties exists for the divine origin or perpetual necessity, as this is a core tenet of the reading itself.
narrative_ontology:disappearance_verdict(lycurgan_laws__sacral_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__sacral_fidelity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__sacral_fidelity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(lycurgan_laws__sacral_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__sacral_fidelity_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__sacral_fidelity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, ExtMetricName, E),
    domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lycurgan_laws__sacral_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) reflects the reading's view that the laws are for the common good and virtue, not for rent-seeking. High suppression (0.85) and accessibility collapse (0.90) are consistent with the requirement for absolute adherence and the suppression of alternatives, which are seen as necessary for maintaining the divine order. The low theater ratio (0.10) indicates that adherence is considered genuine and deeply held, not merely performative. Resistance is low (0.10) because dissent is framed as a moral failing against a sacred mandate. The temporal measurements are flat, reflecting the immutable nature of the laws from this reading's perspective.
 *
 * PERSPECTIVAL GAP:
 *   The 'sacral fidelity' reading fundamentally differs from other interpretations by asserting the divine, immutable nature of the laws. While other readings might see high extraction or structural flaws, this reading interprets all outcomes through the lens of divine mandate and citizen virtue. The engine's classification will highlight this divergence by computing a 'Mountain' type from this reading's metrics, contrasting with potentially more extractive classifications from other readings of the same kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   Spartan citizens are both payers (bearing the severe costs of the system) and beneficiaries (from the perceived stability and virtue), but their 'identity_locked' exit option pushes their directionality towards the target end, as they cannot conceive of life outside the system. The Gerousia and Ephors are agenda-setters and beneficiaries, deriving authority from upholding the laws, placing them firmly at the beneficiary end. Helots are excluded and trapped, bearing extreme costs without any benefit from the system's stated purpose.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_origin_vs_political_construction,
    'Are the Lycurgan laws truly a divinely ordained, immutable order, or a human political construction presented as such to enforce social control and stability?',
    'Archaeological or textual evidence of pre-Lycurgan Spartan legal systems, or comparative analysis with other city-states'' constitutional myths. However, definitive empirical resolution of divine origin is inherently impossible.',
    'If a political construction, the ''emerges_naturally'' claim would be false, reclassifying the constraint away from Mountain towards a more extractive type (e.g., Tangled Rope or Snare), as its persistence would depend on active enforcement and suppression of alternatives, not naturalness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_origin_vs_political_construction, conceptual, 'Contestability of the divine origin claim for the Lycurgan laws.').

omega_variable(
    cause_of_spartan_decline,
    'Was the historical decline of Sparta primarily due to external pressures and the moral failings of its citizens (as this reading claims), or due to the inherent inflexibility and structural flaws of the Lycurgan system itself?',
    'Counterfactual historical analysis, or comparative studies of other rigid constitutional systems. This is a deeply contested historical and political question.',
    'If the laws'' inflexibility was the primary cause, the ''sacral fidelity'' reading''s low extractiveness and Mountain classification would be challenged, pushing towards a ''Snare'' or ''Tangled Rope'' classification that accounts for the system''s self-destructive extraction from its own population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cause_of_spartan_decline, empirical, 'Attribution of Spartan decline to internal system flaws vs. external factors.').

omega_variable(
    adherence_as_virtue_vs_suppression,
    'Is the absolute adherence to Lycurgan laws a genuine expression of civic virtue and shared purpose, or is it maintained primarily through intense social and institutional suppression?',
    'Analysis of historical accounts of dissent, flight, or covert non-compliance, and the severity of punishments for deviation. This is difficult given the historical distance and bias of sources.',
    'If primarily maintained by suppression, the ''sacral fidelity'' reading''s low resistance and low theater_ratio would be re-evaluated, potentially increasing the effective extractiveness and shifting the classification towards a more coercive type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adherence_as_virtue_vs_suppression, empirical, 'Nature of adherence: genuine virtue or enforced compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__sacral_fidelity_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lycu_tr_t100, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement(lycu_tr_t200, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 200, 0.1).
narrative_ontology:measurement(lycu_tr_t300, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 300, 0.1).
narrative_ontology:measurement(lycu_tr_t400, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 400, 0.1).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(lycu_be_t100, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 100, 0.15).
narrative_ontology:measurement(lycu_be_t200, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 200, 0.15).
narrative_ontology:measurement(lycu_be_t300, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 300, 0.15).
narrative_ontology:measurement(lycu_be_t400, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 400, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(lycu_su_t100, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 100, 0.85).
narrative_ontology:measurement(lycu_su_t200, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 200, 0.85).
narrative_ontology:measurement(lycu_su_t300, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 300, 0.85).
narrative_ontology:measurement(lycu_su_t400, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 400, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
