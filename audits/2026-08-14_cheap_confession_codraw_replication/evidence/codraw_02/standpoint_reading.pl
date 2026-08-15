% ============================================================================
% CONSTRAINT STORY: standpoint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_standpoint_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: standpoint_reading
 *   human_readable: Standpoint Reading: Institutional Disagreement Adjudication via Positional Testimony Weighting
 *   domain: epistemology/institutional_analysis
 *
 * SUMMARY:
 *   This story is a single reading of the contested kernel
 *   positional_disagreement_as_evidence: when a facility's management and the
 *   families it serves give conflicting accounts of how the arrangement
 *   operates, is the disagreement evidence to be pooled symmetrically, or is
 *   it evidence of an asymmetric epistemic advantage held by the marginalized
 *   position, which structural credibility deficits systematically discount?
 *   This story instantiates the standpoint reading: the marginalized position
 *   (here, parents and frontline caregivers) sees structural features of the
 *   arrangement invisible in principle from the beneficiary position
 *   (management, credentialed experts), because that visibility is a function
 *   of direct, embodied, repeated exposure that credentialing and
 *   institutional distance do not substitute for. The reading's claim is that
 *   adjudication procedures which treat the two accounts as symmetric inputs
 *   requiring pooling are themselves an extraction mechanism: they launder a
 *   credibility deficit produced by social position into a procedural default
 *   that favors whoever already holds institutional power. The sibling
 *   readings — pragmatist (what works, adjudicated by outcomes),
 *   proceduralist (fair process regardless of outcome), instrumentalist
 *   (whichever account better serves the operative goal) — are separate
 *   constraints with separate ε values and are not part of this reading's own
 *   adjudication logic.
 *
 * KEY AGENTS:
 *   - facility_management: primary beneficiary (institutional/arbitrage) — sets adjudication procedure, retains default credibility
 *   - marginalized_parents: primary target (powerless/trapped) — bears the credibility deficit, cannot exit the dependency relation
 *   - frontline_caregivers: secondary payer/beneficiary (moderate/constrained) — corroborates at risk to employment
 *   - credentialed_experts: secondary beneficiary (organized/mobile) — portable presumptive credibility
 *   - oversight_boards: analytical observer (institutional/analytical) — site of the adjudication choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(standpoint_reading, 0.68).
domain_priors:suppression_score(standpoint_reading, 0.71).
domain_priors:theater_ratio(standpoint_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(standpoint_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(standpoint_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(standpoint_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(standpoint_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(standpoint_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(standpoint_reading, tangled_rope).
narrative_ontology:human_readable(standpoint_reading, "Standpoint Reading: Institutional Disagreement Adjudication via Positional Testimony Weighting").
narrative_ontology:topic_domain(standpoint_reading, "epistemology/institutional_analysis").

domain_priors:requires_active_enforcement(standpoint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(standpoint_reading, 'f9eeb0b2-2952-4e1b-9ba7-24679635677f').
narrative_ontology:cs_kernel_codification('f9eeb0b2-2952-4e1b-9ba7-24679635677f', distributed).
narrative_ontology:cs_authority_grounding('f9eeb0b2-2952-4e1b-9ba7-24679635677f', distributed).
narrative_ontology:cs_reading_relation('f9eeb0b2-2952-4e1b-9ba7-24679635677f', standpoint_reading__pragmatist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9eeb0b2-2952-4e1b-9ba7-24679635677f', standpoint_reading__proceduralist_reading, influences).
narrative_ontology:cs_reading_relation('f9eeb0b2-2952-4e1b-9ba7-24679635677f', standpoint_reading__instrumentalist_reading, coexists_with).
narrative_ontology:cs_axiom('f9eeb0b2-2952-4e1b-9ba7-24679635677f', foundational, positional_advantage_is_asymmetric_not_symmetric_input).
narrative_ontology:cs_axiom_status(positional_advantage_is_asymmetric_not_symmetric_input, holdable).
narrative_ontology:cs_axiom_grounding('f9eeb0b2-2952-4e1b-9ba7-24679635677f', positional_advantage_is_asymmetric_not_symmetric_input, empirically_contingent).
narrative_ontology:cs_axiom('f9eeb0b2-2952-4e1b-9ba7-24679635677f', foundational, credibility_discounting_along_social_position_constitutes_epistemic_injustice).
narrative_ontology:cs_axiom_status(credibility_discounting_along_social_position_constitutes_epistemic_injustice, holdable).
narrative_ontology:cs_axiom_grounding('f9eeb0b2-2952-4e1b-9ba7-24679635677f', credibility_discounting_along_social_position_constitutes_epistemic_injustice, empirically_contingent).
narrative_ontology:cs_reference_frame('f9eeb0b2-2952-4e1b-9ba7-24679635677f', testimonial_symmetry_baseline).
narrative_ontology:cs_drift_state('f9eeb0b2-2952-4e1b-9ba7-24679635677f', post_fricker_epistemic_injustice_literature, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f9eeb0b2-2952-4e1b-9ba7-24679635677f', '').
narrative_ontology:cs_kernel_id(standpoint_reading, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(standpoint_reading, facility_management).
narrative_ontology:constraint_beneficiary(standpoint_reading, credentialed_experts).
narrative_ontology:constraint_victim(standpoint_reading, marginalized_parents).
narrative_ontology:constraint_victim(standpoint_reading, frontline_caregivers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(standpoint_reading, frontline_caregivers).
narrative_ontology:constraint_vindicates(standpoint_reading, epistemic_injustice_thesis).
narrative_ontology:constraint_vindicates(standpoint_reading, credibility_deficit_documentation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the intake, complaint, and grievance procedures that determine whose testimony about the facility's operation counts as evidence. Frames disagreements between itself and families as differences of interpretation to be resolved by procedure, and adjudicates which accounts are treated as credible without itself being subject to the same credibility screening it applies to others.
narrative_ontology:constraint_stakeholder(standpoint_reading, facility_management, agenda_setter,
    institutional, generational, arbitrage, national).

% Report patterns of neglect or harm they observe daily and up close, from a position with direct, repeated, embodied exposure to how the facility actually operates day to day. Structurally cannot exit without losing the placement or service entirely; when their reports conflict with staff or management accounts, their testimony is discounted by default, and they must overcome documented credibility deficits to have the same claim register as evidence.
narrative_ontology:constraint_stakeholder(standpoint_reading, marginalized_parents, payer,
    powerless, biographical, trapped, local).

% Perform the day-to-day labor of the arrangement and often corroborate what marginalized parents observe, but their employment dependency on management makes corroboration risky; they benefit from employment continuity but pay a cost when their observations conflict with the institutional narrative.
narrative_ontology:constraint_stakeholder(standpoint_reading, frontline_caregivers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(standpoint_reading, frontline_caregivers, beneficiary).

% Their assessments are treated as presumptively credible by procedure and by disciplinary training, regardless of proximity to the actual operation being assessed. They can move between institutions and retain standing; their credibility is portable in a way that a parent's lived testimony about one specific facility is not.
narrative_ontology:constraint_stakeholder(standpoint_reading, credentialed_experts, beneficiary,
    organized, generational, mobile, national).

% Receive competing accounts from management, staff, and families and must decide how to weight them. Under a standpoint reading, they are the site where corrective weighting toward the marginalized report either happens or fails to happen; their procedures currently default to symmetric pooling, which the standpoint reading treats as itself an extraction mechanism favoring the already-credible.
narrative_ontology:constraint_stakeholder(standpoint_reading, oversight_boards, observer,
    institutional, generational, analytical, national).

% Pragmatist, proceduralist, and instrumentalist readings of the same disagreement are not consulted within this reading's own adjudication logic; the standpoint reading treats their symmetric-pooling premise as part of what must be corrected, not as a live alternative to be weighed alongside it.
narrative_ontology:constraint_stakeholder(standpoint_reading, rival_disagreement_frameworks, excluded,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(standpoint_reading, diffuse).
narrative_ontology:fixing_cost_class(standpoint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared mechanism for resolving disagreements between institutional actors and the people the institution serves, so that conflicting reports about the same arrangement do not simply cancel each other out or default to whoever speaks first.
% TRANSFER_FUNCTION: Moves epistemic authority — whose account of the arrangement counts as evidence in subsequent decisions — from the party with less structural power and more direct exposure to the party with more structural power and less direct exposure, unless the adjudication procedure is corrected to weight testimony by structural position rather than by credentialing or institutional proximity to power.
% ABSENT_VOICES: The marginalized parents' own account of what the credibility deficit costs them is itself frequently absent from the record used to evaluate the adjudication procedure — their testimony about the mechanism of discounting is subject to the same discounting the mechanism produces. The sibling readings (pragmatist, proceduralist, instrumentalist) are also structurally absent from this reading's adjudication logic by design, not oversight.
% DISAPPEARANCE_RATIONALE: If the standpoint-corrective weighting were removed and disagreements defaulted purely to credential-based or procedural symmetry, oversight boards would systematically resolve conflicts in favor of management and credentialed experts; documented patterns of harm currently surfaced primarily through parent and caregiver testimony would go unregistered as evidence, and the institutional record would diverge further from ground-level operation.
% FOUNDING_PROBLEM: Institutions with structural power over dependent populations routinely produce official accounts of their own operation that differ from the accounts of those most exposed to that operation, and existing adjudication procedures had no principled way to say why the two accounts should not simply be pooled or averaged.
% FOUNDING_PROBLEM_CORROBORATION: Independent empirical work in social epistemology (documented patterns of testimonial injustice and credibility discounting along lines of social position, as catalogued by standpoint theorists and epistemic-injustice researchers outside any single facility's management or credentialing body) corroborates that the asymmetry the reading identifies is observed across institutional settings, not asserted only by the marginalized parties themselves.
narrative_ontology:disappearance_verdict(standpoint_reading, world_rearranges).
narrative_ontology:founding_problem_status(standpoint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(standpoint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(standpoint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(standpoint_reading, 0.68, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(standpoint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(standpoint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(standpoint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects that the standing arrangement — procedural pooling that defaults to institutional and credentialed accounts — extracts epistemic standing from the marginalized position by treating a structurally produced credibility deficit as a neutral starting point rather than as itself the thing requiring correction. Suppression (0.71) is high because the deficit is not merely a passive gap but an actively maintained discounting mechanism: grievance procedures, credentialing hierarchies, and institutional gatekeeping of what counts as documented evidence all function to keep the marginalized account below the evidentiary threshold by default. Theater ratio (0.42) captures that oversight and grievance procedures perform responsiveness (intake forms, hearings, review boards) while the underlying weighting logic remains uncorrected, so a substantial share of institutional process is performative accountability rather than functional correction. Accessibility collapse (0.58) is moderate: alternative adjudication logics (the sibling readings) are conceptually available and contested in the literature, but within any given facility's actual procedure, the pooling default is largely locked in. Resistance (0.62) is substantial because standpoint theorists, epistemic-injustice researchers, and organized parent/caregiver advocacy actively contest the pooling default — this is a live, contested claim, not a settled one.
 *
 * DIRECTIONALITY LOGIC:
 *   Facility management sets the adjudication rules and is not itself subject to the credibility screening it applies to families, placing it at the beneficiary end of directionality. Marginalized parents are trapped by the dependency relation (removing a child or dependent from the facility is not a low-cost exit) and bear the discounting directly, placing them at the target end. Frontline caregivers are structurally intermediate: they often possess the same ground-level observational advantage as parents but their employment dependency on management pulls their effective directionality toward the beneficiary pole when they do not corroborate, and toward the target pole when they do. Credentialed experts benefit from portable, presumptively credible standing regardless of their proximity to any specific facility's actual operation — this is the precise asymmetry Fricker's testimonial-injustice framework and standpoint theory identify as unearned credibility excess.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that institutions with power over dependent populations produce self-accounts diverging from ground-level accounts, with no principled non-symmetric way to adjudicate the difference — remains live, not resolved and not obsolete. Treating this as tangled_rope (rather than pure snare) preserves the genuine coordination function: institutions and families do need SOME shared mechanism for resolving conflicting reports, and abandoning adjudication entirely would leave every dispute to raw power. The extraction is not in having an adjudication mechanism at all, but in defaulting that mechanism to symmetric pooling when the reading holds the positions are not epistemically symmetric. This prevents mislabeling the standpoint correction itself as pure extraction (it is not: it proposes an active coordination fix) while also not laundering the current pooling-default arrangement as pure coordination (it is not: it has a beneficiary and a victim running through the same structure).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    standpoint_vs_pooling_kernel_disagreement,
    'Is positional disagreement between an institution and the people it serves best modeled as asymmetric testimony requiring corrective weighting (standpoint reading), or as symmetric input requiring procedural pooling (proceduralist reading), outcome-adjudicated input (pragmatist reading), or goal-relative selection (instrumentalist reading)?',
    'This is not resolvable by further data within a single reading — it is a conceptual commitment about the structure of epistemic authority itself. Each reading is authored as a separate constraint story (pragmatist_reading, proceduralist_reading, instrumentalist_reading) with its own ε and stakeholder structure; the disagreement between readings is located in whether credibility deficits documented along lines of social position constitute evidence of structural epistemic advantage or are treated as noise to be averaged out.',
    'If the pragmatist or proceduralist framing is adopted instead, the same underlying facts (management account vs. parent account) would not generate a victim/beneficiary asymmetry at all — extraction would likely be authored near zero and the constraint would classify as rope or scaffold rather than tangled_rope. The classification is reading-dependent by design (per the ε-invariance principle), not an error to be reconciled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standpoint_vs_pooling_kernel_disagreement, conceptual, 'Which reading of the positional_disagreement_as_evidence kernel best models the adjudication structure — located at the choice of reading itself, not resolvable by data internal to this reading.').

omega_variable(
    credibility_deficit_measurement_validity,
    'How reliably can a credibility deficit be measured as distinct from a genuine difference in the accuracy of competing accounts — i.e., is the marginalized account systematically more accurate about the arrangement''s operation, or merely systematically less credited regardless of accuracy?',
    'Independent verification studies comparing parent/caregiver reports against later-confirmed facility records (inspection findings, incident logs, whistleblower corroboration) across multiple facilities, tracking whether initially-discounted marginalized testimony was subsequently vindicated at rates exceeding chance.',
    'If marginalized testimony is vindicated at high rates post-hoc, this substantially strengthens the standpoint reading''s core empirical premise (asymmetric epistemic advantage, not mere asymmetric preference). If vindication rates are low or comparable across positions, the standpoint reading''s extraction claim weakens and the disagreement looks more like the proceduralist or pragmatist framing predicts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_deficit_measurement_validity, empirical, 'Whether documented credibility deficits track a genuine positional epistemic advantage or merely track social position independent of accuracy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(standpoint_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stan_tr_t0, standpoint_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(stan_tr_t4, standpoint_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(stan_tr_t8, standpoint_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(stan_tr_t12, standpoint_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(stan_tr_t16, standpoint_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(stan_tr_t20, standpoint_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(stan_tr_t24, standpoint_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(stan_be_t0, standpoint_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(stan_be_t4, standpoint_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(stan_be_t8, standpoint_reading, base_extractiveness, 8, 0.59).
narrative_ontology:measurement(stan_be_t12, standpoint_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(stan_be_t16, standpoint_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(stan_be_t20, standpoint_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(stan_be_t24, standpoint_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stan_su_t0, standpoint_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stan_su_t4, standpoint_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(stan_su_t8, standpoint_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(stan_su_t12, standpoint_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(stan_su_t16, standpoint_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(stan_su_t20, standpoint_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(stan_su_t24, standpoint_reading, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(standpoint_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(standpoint_reading, 0.08).
narrative_ontology:affects_constraint(standpoint_reading, pragmatist_reading).
narrative_ontology:affects_constraint(standpoint_reading, proceduralist_reading).
narrative_ontology:affects_constraint(standpoint_reading, instrumentalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the kernel positional_disagreement_as_evidence (standpoint_reading, pragmatist_reading, proceduralist_reading, instrumentalist_reading). Each reading authors its own ε, beneficiary/victim structure, and claimed type from the same underlying disagreement scenario. The standpoint reading uniquely authors a victim set (marginalized_parents, frontline_caregivers) and a beneficiary set (facility_management, credentialed_experts) because it treats the disagreement as asymmetric testimony rather than symmetric input; siblings that treat the disagreement as procedurally or instrumentally symmetric would author substantially lower extraction and likely no victim set at all. Do not average or reconcile ε across the family — per the ε-invariance principle, each reading is a structurally distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
