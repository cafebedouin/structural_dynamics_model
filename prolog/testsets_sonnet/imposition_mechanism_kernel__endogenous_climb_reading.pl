% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__endogenous_climb_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__endogenous_climb_reading
 *   human_readable: Endogenous Norm Climb — Bottom-Up Legitimacy Preceding State Mandate
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This story instantiates the 'endogenous climb' reading of the
 *   imposition_mechanism_kernel: a new social norm spread through voluntary,
 *   decentralized adoption among communities and norm entrepreneurs,
 *   achieving practical near-universality BEFORE the state issued any formal
 *   mandate. The state's later codification is a ratification of an
 *   accomplished fact, not an imposition against resistance. This is
 *   structurally distinct from a coercive-override reading of the same
 *   historical episode, where the same surface label ('the norm became law')
 *   would describe a state forcing adoption through violence-backed mandate
 *   preceding acceptance — that is a different constraint, generated
 *   separately, linked by network.affects_constraints and
 *   cs_structure.reading_relations, not folded into this file's ε.
 *
 * KEY AGENTS:
 *   - early_adopter_communities: primary beneficiaries of first-mover coordination advantage
 *   - norm_entrepreneurs: agenda-setters who spread the norm through persuasion and demonstration, not coercion
 *   - state_administrators: beneficiaries and secondary agenda-setters who ratify an already-accomplished adoption
 *   - holdout_communities: bear the cost of being late or resistant once informal consensus hardens
 *   - historians_of_state_formation: analytical observers reconstructing the adoption-before-mandate sequence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__endogenous_climb_reading, 0.18).
domain_priors:suppression_score(imposition_mechanism_kernel__endogenous_climb_reading, 0.12).
domain_priors:theater_ratio(imposition_mechanism_kernel__endogenous_climb_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__endogenous_climb_reading, rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__endogenous_climb_reading, "Endogenous Norm Climb — Bottom-Up Legitimacy Preceding State Mandate").
narrative_ontology:topic_domain(imposition_mechanism_kernel__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__endogenous_climb_reading, '189dacc6-1481-4c05-81a3-a46db958db20').
narrative_ontology:cs_kernel_codification('189dacc6-1481-4c05-81a3-a46db958db20', distributed).
narrative_ontology:cs_authority_grounding('189dacc6-1481-4c05-81a3-a46db958db20', practice).
narrative_ontology:cs_interpretation_layer_present('189dacc6-1481-4c05-81a3-a46db958db20').
narrative_ontology:cs_reading_relation('189dacc6-1481-4c05-81a3-a46db958db20', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('189dacc6-1481-4c05-81a3-a46db958db20', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('189dacc6-1481-4c05-81a3-a46db958db20', foundational, legitimacy_precedes_and_generates_mandate).
narrative_ontology:cs_axiom_status(legitimacy_precedes_and_generates_mandate, holdable).
narrative_ontology:cs_axiom_grounding('189dacc6-1481-4c05-81a3-a46db958db20', legitimacy_precedes_and_generates_mandate, empirically_contingent).
narrative_ontology:cs_axiom('189dacc6-1481-4c05-81a3-a46db958db20', secondary, state_as_coordinator_not_coercer).
narrative_ontology:cs_axiom_status(state_as_coordinator_not_coercer, holdable).
narrative_ontology:cs_axiom_grounding('189dacc6-1481-4c05-81a3-a46db958db20', state_as_coordinator_not_coercer, instrumental).
narrative_ontology:cs_reference_frame('189dacc6-1481-4c05-81a3-a46db958db20', decentralized_voluntary_convergence).
narrative_ontology:cs_drift_state('189dacc6-1481-4c05-81a3-a46db958db20', post_codification_consolidation, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('189dacc6-1481-4c05-81a3-a46db958db20', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, early_adopter_communities).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, norm_entrepreneurs).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, state_administrators).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__endogenous_climb_reading, holdout_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopted the new norm voluntarily before any state mandate existed, gaining local status, trade advantage, or social coordination benefits from being early. They set the practical template that the state later ratified rather than invented.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, early_adopter_communities, beneficiary,
    organized, generational, mobile, regional).

% Merchants, local notables, or itinerant preachers who actively promoted the norm through demonstration and persuasion, accumulating reputational capital as the practice spread. Their influence depended on the norm's voluntary uptake continuing to look attractive, not on any coercive backing.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, norm_entrepreneurs, agenda_setter,
    moderate, biographical, mobile, regional).

% Observed the norm's spread, waited until adoption was near-universal in practice, then issued a mandate codifying what most communities already did. This let them claim credit for order without bearing the cost of imposing a norm against resistance; enforcement apparatus was minimal because compliance already existed.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, state_administrators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__endogenous_climb_reading, state_administrators, agenda_setter).

% Communities that resisted or adopted the norm late found themselves increasingly isolated from trade networks and social recognition organized around the new practice, and once the state mandate arrived, bore the cost of being visibly out of compliance with a rule that had already become socially default. Their exit options narrowed as the informal consensus hardened around them.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, holdout_communities, payer,
    powerless, biographical, constrained, local).

% Reconstruct the sequence of adoption versus mandate from documentary and material evidence, distinguishing genuine bottom-up climb from retrospective state narratives claiming to have led what they in fact only ratified.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, historians_of_state_formation, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine multi-party coordination problem: once a critical mass of communities adopts a practice (a calendar, a script, a unit of measure, a dress or trade convention), everyone benefits from converging on the same standard, and voluntary adoption lets the practice be tested and refined before any formal codification locks it in.
% TRANSFER_FUNCTION: Moves social status and coordination advantage from early adopters and norm entrepreneurs toward alignment with the eventual state mandate; moves reputational and market-access costs onto holdout communities as the informal consensus hardens around them, with the state mandate arriving late and imposing minimal marginal cost on those already compliant.
% ABSENT_VOICES: Holdout communities' own reasons for resisting adoption — religious, economic, or simply path-dependent attachment to prior practice — are rarely preserved in the documentary record, which is dominated by accounts from adopters and eventual administrators; their perspective survives mostly as an absence.
% DISAPPEARANCE_RATIONALE: If the state mandate were struck from the record, the underlying practice would likely persist largely unchanged since it was already near-universal before codification — communities that adopted it did so for reasons independent of state backing. But the mandate's removal would matter for holdout communities and for boundary disputes, since the formal rule is what they are currently measured against; whether the world 'rearranges' depends on whose vantage point is asked.
% FOUNDING_PROBLEM: Fragmented local practices created coordination friction (incompatible measures, calendars, or conventions) that voluntary convergence solved organically before any central authority existed to solve it by fiat.
% FOUNDING_PROBLEM_CORROBORATION: Norm entrepreneurs and early adopter communities (through surviving guild and trade records) attest the practice spread by demonstration and imitation well before codification. Independent historians of state formation, working from dated material and documentary evidence, corroborate the adoption-before-mandate sequence in some cases; state administrative records themselves are the least reliable corroboration, since retrospective state narratives have institutional incentive to claim leadership they did not exercise.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__endogenous_climb_reading, contested).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__endogenous_climb_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).
:- end_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low and rises only modestly over the interval (0.08 to 0.18) because the coordination benefit dominates: most communities adopted the norm because it solved a real problem for them, and the state mandate arriving late added little marginal extraction. Suppression is correspondingly low (0.12 at endpoint) because the state never needed to build coercive machinery — compliance already existed by the time any mandate was issued. Theater ratio stays low (0.15) since the mandate reflects genuine underlying practice rather than performative rule-issuance covering a gap between claim and reality. Accessibility collapse is moderate (0.45), not high: alternative practices did not vanish through suppression, they lost out through voluntary convergence, which leaves a different signature than coercive foreclosure.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters and norm entrepreneurs sit near the beneficiary end: they gained status, market access, and reputational capital from being ahead of the curve, and their exit options remained mobile throughout. State administrators are also beneficiaries — they get to claim credit for social order at minimal enforcement cost, arriving after the hard coordination work was already done by decentralized actors. Holdout communities are the sole victim group, and their victimhood is a secondary effect of relative isolation rather than direct extraction: as the informal consensus hardened, their constrained exit options meant continuing resistance became progressively more costly, culminating in exposure once the mandate finally arrived.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification here avoids two adjacent errors. First, it avoids treating a genuinely voluntary, low-coercion coordination process as if it were extraction merely because the practice eventually became coercively enforceable law elsewhere (the sibling exogenous_override_reading captures that different historical mechanism as a separate constraint). Second, it avoids treating the state's later mandate as pure ceremony with no function at all — the mandate did lock in the standard against future defection and gave holdouts a clear, if late, signal, which is a genuine residual coordination function even after the climb was substantially complete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adoption_sequence_evidentiary_ambiguity,
    'For any given historical episode classified under this reading, how confidently can the documentary and material record actually establish that popular adoption preceded state mandate, rather than the record simply reflecting a state narrative constructed after the fact to claim minimal coercion?',
    'Dated material evidence (archaeological strata, dated manuscripts, trade records) independent of state archives, cross-checked against the timing of the first formal mandate; convergence of adoption dating across multiple independent community records strengthens the endogenous-climb reading, while reliance solely on state-produced retrospective accounts weakens it.',
    'If the evidentiary basis for pre-mandate adoption turns out to be largely state-constructed retrospective narrative, the episode reclassifies toward the hybrid_legitimation_reading or even the exogenous_override_reading, since the appearance of bottom-up legitimacy would itself be a legitimation technique rather than a description of what happened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adoption_sequence_evidentiary_ambiguity, empirical, 'Whether the climb-before-mandate sequence is independently evidenced or a retrospective state legitimation narrative.').

omega_variable(
    committer_framing_choice,
    'Given that the same underlying historical episode could be described under any of the three kernel readings (endogenous_climb, exogenous_override, hybrid_legitimation) depending on which evidence is weighted most heavily, what specific signals in the source material justified selecting the endogenous_climb framing for THIS constraint rather than treating the episode as inherently ambiguous?',
    'Explicit weighting of the source material''s own claim — ''state mandate followed rather than preceded popular acceptance'' — taken as the operative structural fact for this reading; a differently-weighted reading of the same episode (e.g. emphasizing the state''s role in publicizing or symbolically endorsing early adopters) would support the hybrid_legitimation_reading instead.',
    'If the source material''s sequencing claim were reversed or found unreliable, this file''s claimed_type and metrics would need to migrate to a different reading (hybrid or exogenous) with substantially different extractiveness and suppression values — the ε would not simply update in place, since ε-invariance requires a new constraint identity for a differently-sequenced claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_choice, conceptual, 'Documents which reading of the imposition_mechanism_kernel this file instantiates and what would change if the sequencing evidence supported a sibling reading instead.').

omega_variable(
    holdout_coalition_potential,
    'Could holdout communities, despite being individually powerless, have exercised coalition power to resist the eventual mandate or negotiate terms, and does the absence of such coalition in the historical record reflect genuine inability or simply absent documentation?',
    'Search for cross-community resistance correspondence, joint petitions, or coordinated non-compliance episodes in regional archives contemporaneous with the mandate''s issuance.',
    'Evidence of coalition attempts would suggest the low-resistance profile of this reading understates actual contestation and that suppression, while lower than in the coercive sibling reading, was not as negligible as the base metrics suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(holdout_coalition_potential, empirical, 'Whether holdout communities had latent coalition capacity that the low-resistance metric may understate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(impo_tr_t8, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 8, 0.07).
narrative_ontology:measurement(impo_tr_t16, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 16, 0.09).
narrative_ontology:measurement(impo_tr_t24, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement(impo_tr_t32, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 32, 0.13).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(impo_be_t8, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 8, 0.1).
narrative_ontology:measurement(impo_be_t16, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 16, 0.13).
narrative_ontology:measurement(impo_be_t24, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 24, 0.16).
narrative_ontology:measurement(impo_be_t32, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 32, 0.17).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 40, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(impo_su_t8, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 8, 0.06).
narrative_ontology:measurement(impo_su_t16, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 16, 0.08).
narrative_ontology:measurement(impo_su_t24, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 24, 0.09).
narrative_ontology:measurement(impo_su_t32, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 32, 0.11).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 40, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__endogenous_climb_reading, 0.06).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% This file is one of three sibling readings of imposition_mechanism_kernel, decomposed per the ε-invariance principle: the natural-language label 'the norm became state law' covers structurally distinct sequencing claims (adoption-then-mandate here; mandate-then-adoption in exogenous_override_reading; simultaneous symbolic-plus-institutional legitimation in hybrid_legitimation_reading). Each carries its own ε, beneficiary/victim structure, and classification; none is a measurement-basis variant of another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
