% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__institutional_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__institutional_displacement_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: dueling_disappearance_mechanism__institutional_displacement_reading
 *   human_readable: Dueling as Honor-Based Dispute Resolution Protocol (Institutional Displacement Reading)
 *   domain: historical sociology / legal history
 *
 * SUMMARY:
 *   This constraint story treats the historical institution of dueling as a
 *   coordination protocol for resolving honor disputes among social peers.
 *   Under the institutional displacement reading, dueling did not collapse
 *   due to suppression or cultural taboo but was gradually abandoned as
 *   courts, commercial banking, and libel law provided superior, lower-risk
 *   substitutes for the same coordination problem. The constraint persists
 *   today only in institutional gaps where formal dispute resolution is
 *   inaccessible or untrusted. The claim/metric independence is maintained:
 *   the constraint is claimed as rope (voluntary coordination) while metrics
 *   are authored to reflect its actual low-extraction, low-suppression
 *   operation.
 *
 * KEY AGENTS:
 *   - gentleman_class: Primary beneficiary (organized/national) â used dueling to coordinate honor disputes, voluntarily migrated to institutional alternatives
 *   - excluded_populace: Excluded (powerless/national) â barred from participation, subject to externalities of private violence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.15).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.1).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Dueling as Honor-Based Dispute Resolution Protocol (Institutional Displacement Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "historical sociology / legal history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, '83829719-788e-4672-b344-14d9ebc72bba').
narrative_ontology:cs_kernel_codification('83829719-788e-4672-b344-14d9ebc72bba', distributed).
narrative_ontology:cs_authority_grounding('83829719-788e-4672-b344-14d9ebc72bba', practice).
narrative_ontology:cs_interpretation_layer_present('83829719-788e-4672-b344-14d9ebc72bba').
narrative_ontology:cs_reading_relation('83829719-788e-4672-b344-14d9ebc72bba', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('83829719-788e-4672-b344-14d9ebc72bba', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('83829719-788e-4672-b344-14d9ebc72bba', foundational, institutional_dispute_resolution_preferred).
narrative_ontology:cs_axiom_status(institutional_dispute_resolution_preferred, holdable).
narrative_ontology:cs_axiom_grounding('83829719-788e-4672-b344-14d9ebc72bba', institutional_dispute_resolution_preferred, instrumental).
narrative_ontology:cs_reference_frame('83829719-788e-4672-b344-14d9ebc72bba', honor_culture_dispute_resolution).
narrative_ontology:cs_drift_state('83829719-788e-4672-b344-14d9ebc72bba', post_institutional_substitution_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('83829719-788e-4672-b344-14d9ebc72bba', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, gentleman_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Social peers who used dueling as a voluntary protocol to resolve honor disputes and maintain standing. As courts, banking, and libel law matured, they increasingly routed grievances through institutional channels instead, abandoning the dueling ground by choice rather than coercion.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, gentleman_class, beneficiary,
    organized, generational, mobile, national).

% Women, laborers, and those outside the honor culture were barred from the dueling ground and had no standing in its normative structure. They were subject to the violence and social disorder dueling produced without access to its supposed benefits or protections.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, excluded_populace, excluded,
    powerless, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a structured, mutually understood protocol for resolving interpersonal honor disputes among social peers when no trusted third-party arbiter was available.
% TRANSFER_FUNCTION: Moved the resolution of social injury from unregulated reputation damage to a regulated ritual exchange of satisfaction, distributing physical risk symmetrically between disputants and their seconds.
% ABSENT_VOICES: Women, laborers, and those outside the honor culture were excluded from the dueling ground and had no standing in its normative structure; they would have noted that the arrangement privatized violence with external social costs.
% DISAPPEARANCE_RATIONALE: If dueling vanished, gentlemen redirected disputes into libel courts, banker-mediated credit relationships, and formal legal process; the social technology of honor-through-violence was replaced by institutional alternatives, fundamentally rearranging how status and injury are processed.
% FOUNDING_PROBLEM: In a society without reliable contract enforcement or neutral third-party dispute resolution, personal honor served as the collateral for social and economic transactions; dueling provided a mechanism to verify that a man's word was backed by willingness to risk death.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and economic historians attest that the expansion of commercial banking and libel law correlates with dueling's decline, corroborating that institutional substitutes solved the founding problem. Judicial records and banking ledgers provide contemporaneous documentation from outside the gentleman beneficiary class.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__institutional_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__institutional_displacement_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__institutional_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 0.15, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).
:- end_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because dueling distributed risk symmetrically among volunteers and collected no third-party rent. Suppression is low (0.10) because the decline was driven by substitution, not by suppressing alternatives; courts and banking were available and attracted users by superior function. Theater ratio is moderate-low (0.30) because dueling was ritualized but the ritual served the functional need for credible commitment to violence. Accessibility collapse (0.35) reflects that while alternatives existed, they were not initially trusted for honor disputes. Resistance (0.10) is minimal because participants were net beneficiaries within their own normative framework.
 *
 * PERSPECTIVAL GAP:
 *   The gentleman class experienced the constraint as a rope â a useful if costly coordination device they could abandon when better options appeared. The excluded populace experienced it as an external violence risk with no compensating benefit, but they were not parties to the constraint. No seat experiences it as extraction because no seat collected asymmetric rents from its operation.
 *
 * DIRECTIONALITY LOGIC:
 *   The gentleman_class is the sole beneficiary (d near beneficiary end) because the protocol was built for and operated by them; they coordinated their own disputes. There are no declared victims because the reading frames abandonment as voluntary substitution. The excluded_populace bears external costs but is not a structural victim of the constraint's operation (they are excluded from its benefits, not targeted by its extraction). Effective extraction is negligible because the constraint's scope is limited to voluntary participants and its base extractiveness is low.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by distinguishing between a coordination mechanism that became obsolete and one that was captured or suppressed. Dueling's decline is read as successful technological substitution (a rope being replaced by better ropes) rather than as a snare being dismantled or a piton rotting. The mandatrophy risk would be labeling all obsolete institutions as pitons; here the constraint retains its rope character even as usage drops to zero, because its low theater ratio and low extraction show it was not maintained performatively after its function died â it was simply outcompeted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_causal_primacy,
    'Did institutional substitution outcompete dueling because the institutions were functionally superior, or because the same social changes that built the institutions also eroded honor culture?',
    'Comparative historiography tracking the timing of dueling''s decline against the diffusion of banking, libel law, and court access in specific localities.',
    'If institution-building preceded dueling decline locally, this reading is strengthened; if dueling declined uniformly regardless of institutional availability, the contraction reading gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_causal_primacy, empirical, 'Uncertainty about whether institutional substitution was the primary driver or merely correlated with cultural change.').

omega_variable(
    voluntary_exit_authenticity,
    'Was the abandonment of dueling genuinely voluntary, or did legal prohibition and social stigma drive the decline while institutional substitution provided a face-saving alternative?',
    'Micro-historical analysis of dueling rates in jurisdictions with differing legal regimes and institutional development.',
    'If dueling persisted where institutions were available but law was permissive, substitution was not the primary cause; if it disappeared wherever institutions emerged regardless of legal regime, this reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_exit_authenticity, empirical, 'Whether the substitution was voluntary or covertly coerced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t0, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(duel_tr_t15, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(duel_tr_t30, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(duel_tr_t45, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 45, 0.26).
narrative_ontology:measurement(duel_tr_t60, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(duel_tr_t75, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 75, 0.29).
narrative_ontology:measurement(duel_tr_t90, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 90, 0.3).

% Extraction over time
narrative_ontology:measurement(duel_be_t0, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(duel_be_t15, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 15, 0.17).
narrative_ontology:measurement(duel_be_t30, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 30, 0.16).
narrative_ontology:measurement(duel_be_t45, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 45, 0.16).
narrative_ontology:measurement(duel_be_t60, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 60, 0.15).
narrative_ontology:measurement(duel_be_t75, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 75, 0.15).
narrative_ontology:measurement(duel_be_t90, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 90, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dueling_disappearance_mechanism__institutional_displacement_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the dueling_disappearance_mechanism kernel, decomposed per the epsilon-invariance principle from the colloquial label 'dueling declined' into structurally distinct claims: institutional substitution, cultural contraction, and overdetermined causation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
