% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__decline_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Declining Honor-Satisfaction Dueling (Fringe Phase)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the decline_reading of the
 *   honor_satisfaction_mechanism kernel. It treats the historical practice of
 *   dueling not as a robust active institution but as a normative mechanism
 *   that weakened steadily from a widespread coordination/extraction device
 *   to a fringe, largely performative remnant maintained by aristocratic
 *   identity inertia. The constraint's epsilon drops through the interval as
 *   enforcement and social cost decline, while theater_ratio rises toward the
 *   fringe phase. The claim/metric independence is maintained: the reading
 *   claims piton (inertial, theatrical, no concentrated beneficiary) while
 *   metrics document the residual extraction and the rising performance
 *   component.
 *
 * KEY AGENTS:
 *   - aristocratic_duelists: Primary targets (powerful/identity_locked) â bear the lethal and social costs of the honor mechanism.
 *   - dueling_seconds: Agenda-setters (organized/constrained) â administer the code and could change it but face identity costs.
 *   - state_legal_apparatus: Observer (institutional/analytical) â criminalizes the practice but enforces unevenly.
 *   - bourgeois_public: Excluded voice (organized/mobile) â objects from outside the aristocratic conversation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.3).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.2).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, piton).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Declining Honor-Satisfaction Dueling (Fringe Phase)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "historical_sociology/legal_history/normative_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, '94728f02-dca3-4ce6-8ddf-6f40fd68c23a').
narrative_ontology:cs_kernel_codification('94728f02-dca3-4ce6-8ddf-6f40fd68c23a', fixed_text).
narrative_ontology:cs_authority_grounding('94728f02-dca3-4ce6-8ddf-6f40fd68c23a', lineage).
narrative_ontology:cs_interpretation_layer_present('94728f02-dca3-4ce6-8ddf-6f40fd68c23a').
narrative_ontology:cs_reading_relation('94728f02-dca3-4ce6-8ddf-6f40fd68c23a', honor_satisfaction_mechanism__contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('94728f02-dca3-4ce6-8ddf-6f40fd68c23a', honor_satisfaction_mechanism__composite_reading, coexists_with).
narrative_ontology:cs_axiom('94728f02-dca3-4ce6-8ddf-6f40fd68c23a', foundational, honor_retains_combat_legitimacy).
narrative_ontology:cs_axiom_status(honor_retains_combat_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('94728f02-dca3-4ce6-8ddf-6f40fd68c23a', honor_retains_combat_legitimacy, conventional).
narrative_ontology:cs_axiom('94728f02-dca3-4ce6-8ddf-6f40fd68c23a', foundational, decline_preserves_conceptual_structure).
narrative_ontology:cs_axiom_status(decline_preserves_conceptual_structure, holdable).
narrative_ontology:cs_axiom_grounding('94728f02-dca3-4ce6-8ddf-6f40fd68c23a', decline_preserves_conceptual_structure, conventional).
narrative_ontology:cs_reference_frame('94728f02-dca3-4ce6-8ddf-6f40fd68c23a', aristocratic_honor_sovereignty).
narrative_ontology:cs_drift_state('94728f02-dca3-4ce6-8ddf-6f40fd68c23a', bourgeois_hegemony_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('94728f02-dca3-4ce6-8ddf-6f40fd68c23a', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, aristocratic_duelists).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__decline_reading, combat_as_honor_restoration).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__decline_reading, aristocratic_status_reciprocity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by the aristocratic honor code to accept challenges or risk social annihilation within their class. They bear the direct costs of injury, death, legal jeopardy, and property loss. In the decline phase, the social cost of refusal rises as the practice shrinks to a fringe ritual, but the identity fusion with aristocratic masculinity makes exit psychologically unavailable even when behavioral frequency drops.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, aristocratic_duelists, payer,
    powerful, biographical, identity_locked, continental).

% Administer the code of honor, negotiate terms, arrange meetings, and enforce procedural compliance among duelists. They do not collect material rents but maintain the mechanism out of professional identity and aristocratic solidarity. They could change the rules or refuse to organize duels, but the cost is the collapse of their social role.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, dueling_seconds, agenda_setter,
    organized, biographical, constrained, national).

% Criminalizes dueling and prosecutes participants, yet often tolerates the practice among elites or punishes it unevenly. Observes the decline from enforcement data and public scandals, but its interventions are inconsistent and frequently subverted by class solidarity.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, state_legal_apparatus, observer,
    institutional, generational, analytical, national).

% Excluded from the aristocratic honor culture and its dispute-resolution rituals. They would advocate for legal or commercial handling of insults and view dueling as barbaric privilege, but their voices are not admitted to the code's interpretation or enforcement.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, bourgeois_public, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__decline_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a ritualized terminal mechanism for aristocratic men to restore social standing after public insult, preventing endless cycles of feud or escalation by assigning a recognized, violent reciprocity procedure.
% TRANSFER_FUNCTION: Moves life-and-limb risk, social prestige, and honor-status from the challenged party to the outcome of the duel, mediated by seconds and governed by the aristocratic code of honor.
% ABSENT_VOICES: Women, bourgeois merchants, and the state legal apparatus are structurally excluded from the honor-culture conversation; they would advocate for legal, commercial, or non-violent resolution of disputes but are not parties to aristocratic status negotiation.
% DISAPPEARANCE_RATIONALE: If the honor-satisfaction mechanism disappeared overnight, aristocratic status negotiation would reorganize toward libel law, social cutting, and state courts; the distinctive culture of aristocratic masculinity would lose its terminal dispute ritual and reshape around non-violent status performance.
% FOUNDING_PROBLEM: In an aristocratic society where status is personal, inheritable, and vulnerable to public insult, there was no recognized terminal mechanism to settle disputes between social equals without appealing to state or ecclesiastical authorities that aristocrats disdained.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and sociologists outside the aristocratic beneficiary set attest that libel law, bourgeois reputation markets, and state criminal courts have absorbed the dispute-resolution function. Contemporary aristocratic memoirs from the late 19th century acknowledge the practice is archaic, corroborating that the founding problem has been solved by alternative institutions.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__decline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__decline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__decline_reading, 0.3, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__decline_reading_tests).
:- end_tests(honor_satisfaction_mechanism__decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is scored low-to-moderate (0.30) because by the fringe phase the actual incidence of lethal outcomes has collapsed, though the conceptual structure still extracts life-risk commitments from identity-bound participants. Suppression is low (0.20) because state enforcement against dueling has largely replaced social enforcement of the duel itself; the constraint persists by inertia, not by active suppression of alternatives. Theater_ratio is high (0.70) because late-period dueling is dominated by ritual performanceâelaborate challenges, deloping, and symbolic gestures that maintain the form without the function. Accessibility_collapse is moderate (0.40): alternatives (courts, bourgeois norms) are widely available and legally dominant, but for the identity-locked aristocrat they remain psychologically inaccessible. Resistance is moderate (0.55): state legal apparatus and bourgeois culture actively resist, creating the decline dynamic.
 *
 * PERSPECTIVAL GAP:
 *   The aristocratic_duelist seat experiences the constraint as a high-cost identity trapâexit means social death within their classâyielding a high directionality and high effective extraction. The dueling_seconds seat experiences it as a professional maintenance burden with constrained exit, yielding a moderate directionality. The state_legal_apparatus sees a residue of aristocratic privilege to be eliminated, and the bourgeois_public sees an archaic barbarism. The engine will compute divergent seat classifications: the duelist seat may compute as snare-like due to identity-locked targeting, while the seconds seat may compute closer to rope or piton depending on the fallback derivation.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries are declared because the piton framing requires no concentrated capture of extraction. The aristocratic class does not profit from dueling in aggregate; it bleeds members and resources. The extraction is diffuseâno seat accumulates the rents. Victims are declared as aristocratic_duelists because they bear the direct costs of injury, death, and legal jeopardy. The directionality derivation therefore pushes duelists toward the full-target end (high d) and seconds toward the middle or administrator end. No override is needed because the structural data already encodes the relationship: identity_locked exit on a powerful agent amplifies effective extraction for the victim seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpersonal dispute resolution in a status-based aristocratic societyâis dead, corroborated by legal historians and the rise of libel law and commercial reputation systems. Yet the arrangement persists in rearranged (fringe) form, satisfying the mandatrophy mismatch flag (founding_problem_status=dead + disappearance_verdict=world_rearranges). The classification as piton rather than snare prevents mislabeling: there is no organized beneficiary defending the constraint for extraction; what persists is institutional inertia and identity performance. If a concentrated beneficiary were discovered (e.g., weapons suppliers or blood-money economies), the constraint would reclassify toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decline_vs_contraction_framing,
    'Does the honor-satisfaction mechanism persist as a conceptually available option into the fringe era, or did it undergo a category-level collapse making it cognitively unthinkable?',
    'Historical discourse analysis of aristocratic memoirs and legal records: if elite men in the late 19th century could still articulate dueling as a thinkable response to insult, the decline reading holds; if the very framing of honor-through-combat became nonsensical, the contraction reading holds.',
    'If contraction is correct, the constraint''s accessibility_collapse is near-total and the decline reading overstates conceptual availability; if decline is correct, the mechanism remains a live option for a shrinking subculture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decline_vs_contraction_framing, conceptual, 'Kernel ambiguity between conceptual availability and cognitive unthinkability').

omega_variable(
    enforcement_decay_driver,
    'Was the decline of dueling driven primarily by state legal suppression (structural) or by bourgeois normative displacement (alternative coordination)?',
    'Comparative legal history across jurisdictions with varying state enforcement timelines; if decline tracks state crackdowns, structural suppression dominates; if decline tracks bourgeois ascendancy regardless of state action, normative displacement dominates.',
    'If structural suppression dominated, the constraint is better read as a snare actively dismantled by external enforcement; if normative displacement dominated, it is a rope outcompeted by alternative coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_driver, empirical, 'Whether dueling declined through external suppression or normative substitution').

omega_variable(
    identity_lock_decay,
    'Does the identity fusion binding aristocratic men to the dueling code decay in parallel with practice frequency, or does it persist as a latent cultural schema?',
    'Longitudinal analysis of aristocratic autobiographical discourse: if honor-through-combat identity persists in memoirs even when practice stops, identity lock outlives structural enforcement.',
    'If identity lock persists, the constraint''s effective suppression remains higher than behavioral frequency suggests; if identity decays with practice, the constraint is purely inertial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_decay, empirical, 'Whether aristocratic identity fusion decays with dueling frequency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hsm_decline_tr_t0, honor_satisfaction_mechanism__decline_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hsm_decline_tr_t10, honor_satisfaction_mechanism__decline_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(hsm_decline_tr_t20, honor_satisfaction_mechanism__decline_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(hsm_decline_tr_t30, honor_satisfaction_mechanism__decline_reading, theater_ratio, 30, 0.55).
narrative_ontology:measurement(hsm_decline_tr_t40, honor_satisfaction_mechanism__decline_reading, theater_ratio, 40, 0.65).
narrative_ontology:measurement(hsm_decline_tr_t50, honor_satisfaction_mechanism__decline_reading, theater_ratio, 50, 0.75).

% Extraction over time
narrative_ontology:measurement(hsm_decline_be_t0, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(hsm_decline_be_t10, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(hsm_decline_be_t20, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(hsm_decline_be_t30, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(hsm_decline_be_t40, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(hsm_decline_be_t50, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(hsm_decline_su_t0, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hsm_decline_su_t10, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(hsm_decline_su_t20, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(hsm_decline_su_t30, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 30, 0.32).
narrative_ontology:measurement(hsm_decline_su_t40, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(hsm_decline_su_t50, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 50, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__decline_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is the decline_reading of the honor_satisfaction_mechanism kernel, decomposed from the colloquial label 'dueling' per the epsilon-invariance principle. Sibling readings (contraction_reading, composite_reading) capture structurally distinct claims about the same historical practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
