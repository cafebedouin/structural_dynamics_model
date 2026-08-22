% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__functional_protection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__functional_protection_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: combatant_status_definition__functional_protection_reading
 *   human_readable: Common Article 3 Status-Independent Minimum Treatment Floor
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   This story instantiates the functional-protection reading of the
 *   combatant-status-definition kernel: Common Article 3 to the Geneva
 *   Conventions establishes that ALL persons in the hands of a party to a
 *   conflict — regardless of whether they are found to be lawful combatants,
 *   unlawful combatants, or civilians — receive a floor of humane treatment,
 *   protection from violence and outrages upon personal dignity, and minimum
 *   fair-trial guarantees before any sentence is carried out. This reading
 *   treats status determination as irrelevant to whether the floor applies,
 *   in contrast to the state-centric reading (which makes full combatant/POW
 *   protection turn on formal state military organization under Article 4)
 *   and the national-liberation reading (which extends combatant status to
 *   organized non-state groups under AP I Art. 1(4) but still requires a
 *   status finding). Because this reading removes status as a precondition, ε
 *   is authored low and stable: the floor is not a bargaining chip that
 *   shifts value between parties so much as a near-universal baseline that
 *   detaining powers are bound to regardless of the underlying status
 *   dispute. The rise in theater_ratio and suppression_requirement around
 *   2001 documents the post-9/11 'unlawful enemy combatant' controversy,
 *   where states attempted to reintroduce status-contingent treatment
 *   (Guantanamo, extraordinary rendition) specifically by contesting whether
 *   Common Article 3's floor applied at all outside international armed
 *   conflict — a direct empirical test of this reading's resilience against
 *   status-based erosion.
 *
 * KEY AGENTS:
 *   - detaining_power_military_command: administers custody and classification (institutional/constrained) — bound not to condition the floor on status
 *   - all_detained_persons: primary beneficiaries (powerless/trapped) — receive the floor regardless of classification outcome
 *   - captured_irregular_fighters: the population whose protection is most structurally contingent on THIS reading over the state-centric sibling
 *   - icrc_and_monitoring_bodies: analytical/enforcement-adjacent observer with a simplified compliance target under this reading
 *   - field_commanders_seeking_intelligence_leverage: excluded voice whose operational preference (status-contingent treatment) is foreclosed by the reading's core premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.12).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.22).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, rope).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Common Article 3 Status-Independent Minimum Treatment Floor").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__functional_protection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, 'b8511cb8-c8f3-4e5a-b5fa-fb8437c89406').
narrative_ontology:cs_kernel_codification('b8511cb8-c8f3-4e5a-b5fa-fb8437c89406', fixed_text).
narrative_ontology:cs_authority_grounding('b8511cb8-c8f3-4e5a-b5fa-fb8437c89406', lineage).
narrative_ontology:cs_interpretation_layer_present('b8511cb8-c8f3-4e5a-b5fa-fb8437c89406').
narrative_ontology:cs_reading_relation('b8511cb8-c8f3-4e5a-b5fa-fb8437c89406', combatant_status_definition__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('b8511cb8-c8f3-4e5a-b5fa-fb8437c89406', combatant_status_definition__national_liberation_reading, influences).
narrative_ontology:cs_axiom('b8511cb8-c8f3-4e5a-b5fa-fb8437c89406', foundational, humane_treatment_precedes_status_determination).
narrative_ontology:cs_axiom_status(humane_treatment_precedes_status_determination, holdable).
narrative_ontology:cs_axiom_grounding('b8511cb8-c8f3-4e5a-b5fa-fb8437c89406', humane_treatment_precedes_status_determination, deontological).
narrative_ontology:cs_axiom('b8511cb8-c8f3-4e5a-b5fa-fb8437c89406', foundational, fair_trial_guarantee_is_universal_not_privilege_of_recognized_combatancy).
narrative_ontology:cs_axiom_status(fair_trial_guarantee_is_universal_not_privilege_of_recognized_combatancy, holdable).
narrative_ontology:cs_axiom_grounding('b8511cb8-c8f3-4e5a-b5fa-fb8437c89406', fair_trial_guarantee_is_universal_not_privilege_of_recognized_combatancy, deontological).
narrative_ontology:cs_reference_frame('b8511cb8-c8f3-4e5a-b5fa-fb8437c89406', common_article_3_universal_floor).
narrative_ontology:cs_drift_state('b8511cb8-c8f3-4e5a-b5fa-fb8437c89406', post_2001_war_on_terror_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b8511cb8-c8f3-4e5a-b5fa-fb8437c89406', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, all_detained_persons).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, captured_irregular_fighters).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, civilian_internees).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, humane_treatment_is_status_independent).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, fair_trial_minimum_guarantee_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the detention apparatus and decides, in the field and at policy level, how detainees are classified and treated. Under this reading, command is barred from making treatment turn on a prior status determination — the floor of humane treatment and fair trial guarantees applies the moment a person is in custody, before any combatant-status finding is made. Command retains discretion over classification for OTHER purposes (POW privileges, targeting) but not over whether Common Article 3 applies.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, detaining_power_military_command, agenda_setter,
    institutional, immediate, constrained, national).

% Held in custody with no control over how they are classified. Under the functional reading, they receive humane treatment, protection from violence, and fair trial guarantees regardless of whether they are found to be lawful combatants, unlawful combatants, or civilians. Their situation is structurally improved precisely because the floor does not wait on a contested status inquiry that they cannot control and that the detaining power has incentive to delay or resolve unfavorably.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, all_detained_persons, beneficiary,
    powerless, immediate, trapped, national).

% Members of non-state armed groups who would, under the state-centric sibling reading, receive no combatant protections at all and could be treated as ordinary criminals or worse. Under this reading they are guaranteed the Common Article 3 floor irrespective of any determination about whether their group meets organized-command criteria. This is the clearest structural gain the functional reading produces relative to its siblings.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, captured_irregular_fighters, beneficiary,
    powerless, immediate, trapped, national).

% Civilians detained on security grounds who might otherwise fall into a classification gap between combatant and protected-civilian regimes. The status-independent floor ensures they are never treated as rights-less merely because their classification is ambiguous or contested.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, civilian_internees, beneficiary,
    powerless, immediate, trapped, national).

% Monitor detention facilities and press for compliance with the minimum floor. They benefit from a rule that does not require them to litigate status before securing visitation, humane-treatment, and fair-trial commitments — the functional reading gives them a single, simpler compliance target instead of a status-contingent one.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, icrc_and_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% Would prefer treatment and process to remain contingent on status determinations because ambiguity over status has historically been used as leverage in interrogation and control. Their operational preference is structurally excluded by this reading's core premise, though they are not a party the reading names as a stakeholder in the ordinary sense — they object informally, through policy resistance, not through the classification apparatus itself.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, field_commanders_seeking_intelligence_leverage, excluded,
    organized, immediate, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__functional_protection_reading, diffuse).
narrative_ontology:fixing_cost_class(combatant_status_definition__functional_protection_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem that status-determination processes are slow, contested, and often controlled by the very party with an interest in denying protection — by decoupling the humane-treatment floor from any status finding, all parties to a conflict (however asymmetric in formal military organization) can be held to a minimum standard without first litigating who counts as a combatant.
% TRANSFER_FUNCTION: Moves nothing extractive between parties in the ordinary sense; it transfers a guaranteed floor of treatment and process TO every detained person FROM the detaining power's discretion to withhold treatment pending classification. The 'cost' borne by detaining powers is the loss of classification as a gatekeeping tool over treatment, not a resource transfer to a rent-collecting party.
% ABSENT_VOICES: Field commanders and intelligence services who use status ambiguity as leverage are not represented in the treaty text's own terms and would object informally through policy resistance, classification delay tactics, or reservations — but no party formally objects to the floor itself in treaty negotiation record, since Common Article 3 is near-universally ratified.
% DISAPPEARANCE_RATIONALE: If the status-independent floor vanished, detaining powers would regain the ability to condition humane treatment and fair trial access on contested status determinations — reintroducing the classification gap that historically left irregular fighters and ambiguous civilians without any guaranteed protection while status was disputed, often for the duration of a conflict.
% FOUNDING_PROBLEM: Post-WWII drafters observed that internal and asymmetric conflicts produced large populations (partisans, irregulars, ambiguous civilians) who fell into gaps between the POW regime and civilian protections, and that detaining powers exploited status ambiguity to withhold any protection at all.
% FOUNDING_PROBLEM_CORROBORATION: ICRC commentary and customary IHL studies (compiled by parties outside any single detaining power) document continued state practice of contesting detainee status specifically to delay or avoid Common Article 3 obligations — most visibly in non-international armed conflicts and counterterrorism detention regimes since 2001, corroborating that the classification-gap problem the floor was built to close remains active rather than historical.
narrative_ontology:disappearance_verdict(combatant_status_definition__functional_protection_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__functional_protection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__functional_protection_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(combatant_status_definition__functional_protection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__functional_protection_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__functional_protection_reading_tests).
:- end_tests(combatant_status_definition__functional_protection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) because the floor, by design, does not create a party that collects rents from its operation — it is a universal baseline, not a bargained allocation with winners and losers in the ordinary extraction sense. Suppression is moderate (0.22) reflecting the real enforcement burden required to prevent detaining powers from reintroducing status-contingent treatment through reclassification maneuvers (the 2001 spike models the 'unlawful combatant' controversy). Theater ratio is low-to-moderate: most compliance activity is substantive (ICRC visits, tribunal review) but a portion, especially post-2001, is performative compliance language layered over continued status-based practice. Accessibility collapse is authored low (0.2) — deliberately, because the reading's entire point is that alternatives to protection do NOT collapse merely because status is unresolved; if anything the floor is designed to keep the humane-treatment alternative open regardless of classification. Resistance is authored moderate-high (0.55) because states with strong incentives to condition treatment on status (particularly in asymmetric and non-international conflicts) have actively resisted the status-independent framing in practice even while formally ratifying the instrument.
 *
 * PERSPECTIVAL GAP:
 *   From the detaining power's seat, this reading imposes a constraint on discretion that constrains operational flexibility without offering a coordination benefit specific to that seat — it may compute closer to an externally imposed rule than a coordination good from that vantage. From the detained person's seat and the monitoring body's seat, the same rule computes as a genuine, low-cost coordination good: a floor that does not require winning a classification fight to receive humane treatment. The engine's per-seat computation should reflect this asymmetry in experienced burden even though the authored ε is low overall.
 *
 * DIRECTIONALITY LOGIC:
 *   Detained persons of every classification are beneficiaries under this reading by construction — the floor exists to reach them specifically because their status is unresolved or contested, and their exit option is trapped (they cannot litigate their own classification from custody). The detaining power sits as agenda_setter with constrained exit: it administers detention and retains classification discretion for OTHER regimes (POW privilege, targeting) but is bound not to gate the floor itself on that discretion. No victim group is named because the structural claim of this reading is precisely that no one bears extraction through the floor's operation — the floor's cost to the detaining power is a loss of leverage, not a transfer to an extracting party, which is why victims[] is authored empty and the constraint is NOT classified as tangled_rope or snare from this reading's own lights.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — status ambiguity being weaponized to withhold any protection — remains live, corroborated by post-2001 state practice rather than resolved into obsolescence. This blocks a mandatrophy reading in which the floor would be treated as a vestigial formality: the 2001 spike in suppression_requirement and theater_ratio is direct evidence that the mechanism the floor exists to prevent (status-contingent treatment) is still actively attempted, meaning the coordination function has not atrophied into pure inertia. It functions closer to a rope under continuous light enforcement than a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_reading_universality_scope,
    'Does the status-independent floor in this reading apply identically in international armed conflict, non-international armed conflict, and contested classifications like the post-2001 ''war on terror,'' or does its practical force vary by conflict classification despite the formal text being status-independent?',
    'Comparative analysis of state compliance records across IAC, NIAC, and transnational counterterrorism detention regimes; ICRC customary law study cross-referenced against actual tribunal and habeas findings.',
    'If the floor''s practical force varies significantly by conflict classification, the ''status-independent'' character of this reading is itself qualified by a conflict-classification determination, which would mean this reading has its own hidden precondition analogous to the ones it claims to remove — narrowing the structural delta from its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_reading_universality_scope, empirical, 'Whether the floor''s status-independence itself depends on an unstated conflict-classification threshold.').

omega_variable(
    kernel_framing_state_centric_alternative,
    'Is the more analytically salient framing of the combatant-status kernel the formal status-taxonomy question (who counts as a combatant) or the treatment-floor question (what treatment attaches regardless of the taxonomy)? Different scholars and tribunals foreground different questions when discussing ''the combatant status debate.''',
    'Survey of how the ICTY, ICC, and domestic habeas courts frame the operative legal question in mixed-status detention cases — do they resolve status first, or apply the floor first and treat status as relevant only to supplementary privileges?',
    'If courts consistently resolve status first (state-centric framing dominant in practice), this reading''s claim that status is a non-precondition is descriptively weaker than its normative claim, even though the treaty text supports the functional reading; this would suggest the three sibling readings are less coequal in practice than in doctrine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_state_centric_alternative, conceptual, 'Whether tribunals treat the functional floor as genuinely prior to status, or as a fallback invoked only after status inquiry proves inconclusive.').

omega_variable(
    enforcement_asymmetry_across_state_capacity,
    'Does the status-independent floor''s suppression requirement (0.22) fall disproportionately on already-resource-constrained detaining powers, while well-resourced states absorb compliance costs without altering behavior, meaning the floor''s enforcement burden is regressive across state capacity?',
    'Comparative study of ICRC monitoring outcomes and compliance costs across high-capacity versus low-capacity detaining powers in recent NIAC settings.',
    'If enforcement burden is regressive, the floor''s low authored ε may mask a distributional effect not currently captured by the single-scalar extractiveness metric, warranting a future decomposition by state capacity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_asymmetry_across_state_capacity, empirical, 'Whether the floor''s enforcement cost falls disproportionately on lower-capacity detaining powers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1949, combatant_status_definition__functional_protection_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(comb_tr_t1970, combatant_status_definition__functional_protection_reading, theater_ratio, 1970, 0.11).
narrative_ontology:measurement(comb_tr_t1990, combatant_status_definition__functional_protection_reading, theater_ratio, 1990, 0.13).
narrative_ontology:measurement(comb_tr_t2001, combatant_status_definition__functional_protection_reading, theater_ratio, 2001, 0.2).
narrative_ontology:measurement(comb_tr_t2010, combatant_status_definition__functional_protection_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__functional_protection_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(comb_be_t1949, combatant_status_definition__functional_protection_reading, base_extractiveness, 1949, 0.1).
narrative_ontology:measurement(comb_be_t1970, combatant_status_definition__functional_protection_reading, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(comb_be_t1990, combatant_status_definition__functional_protection_reading, base_extractiveness, 1990, 0.11).
narrative_ontology:measurement(comb_be_t2001, combatant_status_definition__functional_protection_reading, base_extractiveness, 2001, 0.13).
narrative_ontology:measurement(comb_be_t2010, combatant_status_definition__functional_protection_reading, base_extractiveness, 2010, 0.13).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__functional_protection_reading, base_extractiveness, 2024, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1949, combatant_status_definition__functional_protection_reading, suppression_requirement, 1949, 0.18).
narrative_ontology:measurement(comb_su_t1970, combatant_status_definition__functional_protection_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(comb_su_t1990, combatant_status_definition__functional_protection_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(comb_su_t2001, combatant_status_definition__functional_protection_reading, suppression_requirement, 2001, 0.28).
narrative_ontology:measurement(comb_su_t2010, combatant_status_definition__functional_protection_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__functional_protection_reading, suppression_requirement, 2024, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__functional_protection_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(combatant_status_definition__functional_protection_reading, 0.1).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, national_liberation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the combatant_status_definition kernel. state_centric_reading authors a much narrower protection scope (categorical exclusion of non-state actors from POW status) and correspondingly should carry a higher ε for the excluded population under that reading's own lights. national_liberation_reading sits structurally between the two: it extends full combatant status to organized non-state groups meeting AP I Art. 1(4) criteria, but still requires an affirmative status finding, unlike this reading's status-independent floor. All three share the underlying kernel — what determines who is protected and how — but instantiate different constraints with different beneficiary sets, different ε, and different enforcement postures. This reading's ε is deliberately the lowest of the three because it is the only reading that removes the status determination as a precondition for baseline protection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
