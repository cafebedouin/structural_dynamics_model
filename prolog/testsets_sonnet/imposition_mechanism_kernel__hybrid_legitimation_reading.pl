% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__hybrid_legitimation_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Hybrid Imperial-Charisma Legitimation of New Social Norms
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This story instantiates the hybrid_legitimation_reading of the
 *   imposition_mechanism_kernel: a new social norm (e.g. a dress, ritual, or
 *   kinship practice) spreads across an imperial polity neither through
 *   organic bottom-up convergence (the endogenous_climb_reading) nor through
 *   raw state coercion backed by the monopoly on violence (the
 *   exogenous_override_reading), but through the emperor's personal example
 *   converted into a status-and-incentive cascade. The court declares or
 *   performs the new norm; provincial elites adopt it early to capture
 *   prestige and office; bureaucrats institutionalize the incentive; and
 *   adoption filters downward through landlord and tax-registry channels to
 *   peasant households and religious minorities last and least voluntarily.
 *   Enforcement cost is moderate — lower than coercive override because
 *   compliance is partly self-enforcing through elite status competition,
 *   higher than pure climb because the incentive structure requires ongoing
 *   administrative maintenance and produces real losers (non-aligned
 *   notables, excluded minorities) who would not have borne costs under an
 *   organically emergent norm.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.48).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.42).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Hybrid Imperial-Charisma Legitimation of New Social Norms").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, 'd78c34bb-5108-4d79-ab9d-ac7ce991ffa2').
narrative_ontology:cs_kernel_codification('d78c34bb-5108-4d79-ab9d-ac7ce991ffa2', implicit).
narrative_ontology:cs_authority_grounding('d78c34bb-5108-4d79-ab9d-ac7ce991ffa2', lineage).
narrative_ontology:cs_interpretation_layer_present('d78c34bb-5108-4d79-ab9d-ac7ce991ffa2').
narrative_ontology:cs_reading_relation('d78c34bb-5108-4d79-ab9d-ac7ce991ffa2', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('d78c34bb-5108-4d79-ab9d-ac7ce991ffa2', imposition_mechanism_kernel__exogenous_override_reading, influences).
narrative_ontology:cs_axiom('d78c34bb-5108-4d79-ab9d-ac7ce991ffa2', foundational, legitimacy_transmitted_through_charismatic_exemplarity_plus_incentive).
narrative_ontology:cs_axiom_status(legitimacy_transmitted_through_charismatic_exemplarity_plus_incentive, holdable).
narrative_ontology:cs_axiom_grounding('d78c34bb-5108-4d79-ab9d-ac7ce991ffa2', legitimacy_transmitted_through_charismatic_exemplarity_plus_incentive, conventional).
narrative_ontology:cs_axiom('d78c34bb-5108-4d79-ab9d-ac7ce991ffa2', secondary, adoption_stratification_reflects_proximity_to_incentive_not_belief_depth).
narrative_ontology:cs_axiom_status(adoption_stratification_reflects_proximity_to_incentive_not_belief_depth, holdable).
narrative_ontology:cs_axiom_grounding('d78c34bb-5108-4d79-ab9d-ac7ce991ffa2', adoption_stratification_reflects_proximity_to_incentive_not_belief_depth, empirically_contingent).
narrative_ontology:cs_reference_frame('d78c34bb-5108-4d79-ab9d-ac7ce991ffa2', imperial_charismatic_exemplarity).
narrative_ontology:cs_drift_state('d78c34bb-5108-4d79-ab9d-ac7ce991ffa2', post_succession_incentive_entrenchment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d78c34bb-5108-4d79-ab9d-ac7ce991ffa2', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, provincial_elites).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, aligned_bureaucratic_officials).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, non_aligned_local_notables).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, peasant_households).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, religious_minorities_outside_favored_cult).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the new norm by public imperial example (a ritual observance, a dress code, a marriage or burial practice) and pairs it with material incentives — office, tax relief, favor — for those who visibly adopt it. Its authority is symbolic capital accumulated over generations; it risks little by declaring the norm and gains prestige and compliance leverage whichever way adoption proceeds.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court, agenda_setter,
    institutional, generational, arbitrage, national).

% Adopt the norm early and visibly to align themselves with imperial favor, converting symbolic conformity into local political capital, marriage alliances, and appointments. They then become secondary enforcers, demanding the same conformity of their own clients and tenants to secure their own position.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, provincial_elites, beneficiary,
    powerful, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, provincial_elites, agenda_setter).

% Administer the incentive structure — recommending office, certifying compliance, distributing exemptions — and draw career advancement from being seen to champion the emperor's example. Their institutional position depends on the norm's continued salience.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, aligned_bureaucratic_officials, beneficiary,
    organized, biographical, constrained, national).

% Hold local standing under older norms and are structurally disadvantaged by the switch: adopting late costs prestige already spent by rivals, refusing costs access to office and favor. They bear reputational and economic cost without having chosen the change.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, non_aligned_local_notables, payer,
    moderate, biographical, constrained, regional).

% Adopt the norm last, often decades after elites, under pressure transmitted downward through landlords, local officials, and tax registers that now encode the new practice as a marker of loyalty or eligibility for relief. They have no symbolic capital to trade and absorb whatever material or ritual cost the norm imposes with least ability to negotiate exemptions.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, peasant_households, payer,
    powerless, biographical, trapped, local).

% Practice a rival or older cult that the new norm implicitly or explicitly displaces. They face exclusion from the incentive structure entirely and, where the norm becomes a marker of political loyalty, increasing suspicion or formal disability regardless of behavior.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, religious_minorities_outside_favored_cult, payer,
    powerless, generational, trapped, regional).

% Record the emperor's example and its adoption, producing the textual record later readers use to adjudicate whether the norm change was climb, override, or hybrid. Their accounts are themselves shaped by proximity to the court and by which register (edict, memorial, popular chronicle) they write in.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, court_historians_and_chroniclers, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, legible signal (the emperor's personal example) around which disparate regional elites can coordinate status competition and career advancement without requiring either grassroots consensus-building or costly province-by-province coercion.
% TRANSFER_FUNCTION: Moves prestige, office, and material favor from the imperial center to early adopters, and moves reputational and material cost from late or non-adopters (especially those without symbolic capital to spend) toward compliance with a norm they did not originate.
% ABSENT_VOICES: Peasant households and religious minorities outside the favored cult are adopting last or bearing exclusion, but the chronicled record is produced by and for the court and provincial elite audience — their experience of the transition survives mostly as silence or as later administrative complaint, not as first-person testimony.
% DISAPPEARANCE_RATIONALE: Court-aligned sources treat the norm as having become simply 'how things are done,' implying the world would not rearrange if the original imperial sponsorship were forgotten. Provincial and peasant-facing administrative records suggest otherwise: compliance tracks continued proximity to incentive structures, and where imperial favor visibly withdraws from a practice, elite adoption reverses faster than mass adoption does, indicating the arrangement still depends on active symbolic-plus-material reinforcement rather than having become self-sustaining custom.
% FOUNDING_PROBLEM: The court needed a mechanism to standardize a marker of loyalty or cultural alignment across a large, heterogeneous territory without either waiting for organic convergence (too slow, too uncertain) or funding a coercive apparatus capable of monitoring universal compliance (too costly, too fragile).
% FOUNDING_PROBLEM_CORROBORATION: Provincial administrative correspondence and later dynastic historians outside the immediate beneficiary circle attest that compliance tracked incentive availability rather than genuine internalization for at least the first two generations; court chronicles, produced by the direct beneficiaries of the incentive structure, attest instead to organic and near-universal acceptance. No fully independent third-party source (e.g. a rival state's observers) has been identified for this specific norm transition.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__hybrid_legitimation_reading, contested).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__hybrid_legitimation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__hybrid_legitimation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) sits at a genuine midpoint: real coordination value exists (a legible common marker reduces status-signaling friction across a large territory) but genuine transfer also occurs (office and favor move to early adopters at the expense of non-aligned notables and excluded minorities, who pay in status and material terms). Suppression (0.42) reflects that most compliance is incentive-driven rather than coercive, but a meaningful minority — especially religious minorities and peasant households with no symbolic capital to trade — face effectively coercive pressure once the norm becomes encoded in tax and loyalty registers. Theater ratio (0.4) is moderate-to-elevated because a substantial share of elite 'adoption' is performative alignment rather than internalized belief, particularly in the first two generations.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial court sits nearest the beneficiary pole: it originates the norm, bears essentially no adoption cost, and gains prestige and leverage regardless of uptake speed. Provincial elites and aligned officials are secondary beneficiaries who convert early compliance into durable capital and then become co-enforcers. Non-aligned notables, peasant households, and religious minorities are targets: they bear cost (lost prestige, forced compliance, exclusion) without having authored the norm, and their exit options range from constrained (notables, who could in principle relocate or realign) to trapped (peasants and minorities bound by land, tax registration, or lack of alternative institutional shelter).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification resists collapsing into either sibling reading: treating this purely as coordination (rope) would erase the real transfer to non-aligned notables and excluded minorities; treating it purely as coercive override (snare) would erase the genuine status-competition mechanism through which elites voluntarily race to adopt. Tangled Rope captures both: a real coordination function (a legible common marker across a heterogeneous territory) riding alongside asymmetric extraction (elites and court gain, non-aligned actors and the powerless pay), sustained by active administrative enforcement of the incentive structure rather than by either pure demand or pure force.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_reading_boundary_ambiguity,
    'At what point does elite status-competition adoption (hybrid) become indistinguishable from coerced compliance (override), given that both can produce identical observable adoption curves?',
    'Compare adoption reversal rates when imperial favor visibly withdraws from the norm: rapid elite reversal with continued mass compliance indicates hybrid incentive-driven adoption; uniform reversal across all strata indicates the norm was never internalized and coercion (or its threat) was doing the real work throughout.',
    'If reversal patterns show uniform collapse, this story''s classification should shift toward the exogenous_override_reading rather than remaining tangled_rope hybrid — the coordination component would be revealed as cover for pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_boundary_ambiguity, empirical, 'Whether elite-driven adoption is genuinely incentive-based or coercion dressed as status competition.').

omega_variable(
    corroboration_asymmetry,
    'Is the founding_problem narrative (standardization need without coercive apparatus) an accurate genealogy, or a court-produced retrospective justification for what was, at the time, straightforwardly coercive imposition later reframed as elegant hybrid statecraft?',
    'Cross-reference court chronicle claims against non-court administrative correspondence, tax registry marginalia, and any surviving records from excluded religious minorities or foreign observers describing the same transition period.',
    'If no independent corroboration exists beyond court-adjacent sources, the founding_problem_status should be treated as effectively self-asserted by beneficiaries, which would weaken confidence in the tangled_rope classification relative to a snare reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corroboration_asymmetry, conceptual, 'Whether the hybrid framing is a structurally accurate description or a beneficiary-authored cover narrative for coercive imposition.').

omega_variable(
    cs_framing_alternative_kernel_locus,
    'Should the kernel be located in the emperor''s personal example (the obvious framing used here) or in the institutional incentive apparatus that converts example into compliance (the less obvious framing, since the incentive structure could survive a change of emperor or even a change of exemplary practice)?',
    'Trace whether the incentive apparatus (office-for-conformity linkage) persisted across imperial succession with a DIFFERENT exemplary norm substituted — if so, the apparatus, not the specific emperor''s example, is the true kernel.',
    'If the incentive apparatus is the real kernel, this constraint''s authority_grounding might be better modeled as institutional/practice-based rather than lineage-charismatic, which would shift the cs_structure axioms toward conventional/instrumental grounding rather than the charisma-based grounding authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_alternative_kernel_locus, conceptual, 'Alternative framing of where the kernel actually sits — in the exemplary act or in the surviving incentive machinery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(impo_tr_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(impo_tr_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(impo_tr_t30, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(impo_tr_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement(impo_tr_t60, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(impo_be_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(impo_be_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(impo_be_t30, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(impo_be_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 50, 0.47).
narrative_ontology:measurement(impo_be_t60, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 60, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(impo_su_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(impo_su_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(impo_su_t30, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(impo_su_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 50, 0.41).
narrative_ontology:measurement(impo_su_t60, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 60, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__hybrid_legitimation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.1).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of imposition_mechanism_kernel, each authored as a structurally distinct constraint per the ε-invariance principle: endogenous_climb_reading (legitimacy from bottom-up demand, low enforcement cost, rope-leaning), exogenous_override_reading (legitimacy from coercive monopoly, high enforcement cost, snare-leaning), and this hybrid_legitimation_reading (legitimacy from charismatic-institutional fusion, moderate enforcement cost, tangled_rope). Each carries its own ε and stakeholder set; none is a measurement variant of another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
