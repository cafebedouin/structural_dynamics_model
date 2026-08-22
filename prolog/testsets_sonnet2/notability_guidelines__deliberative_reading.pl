% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deliberative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__deliberative_reading, []).

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
 *   constraint_id: notability_guidelines__deliberative_reading
 *   human_readable: WP:N as Perpetual Deliberative Process (AfD-Mediated Boundary Negotiation)
 *   domain: digital_commons_governance/knowledge_infrastructure/platform_constitutionalism
 *
 * SUMMARY:
 *   This story instantiates the deliberative reading of the WP:N kernel:
 *   notability is not a fixed input applied mechanically at AfD but an output
 *   continuously produced BY AfD deliberation, revised discussion by
 *   discussion as participants argue over sourcing, significance, and
 *   precedent. Under this reading the guideline text is a scaffold — a
 *   starting frame for argument, explicitly provisional, meant to be
 *   superseded by evolving consensus rather than held as a stable boundary.
 *   The sibling readings (deletionist: WP:N as a necessary quality filter;
 *   inclusionist: WP:N as gatekeeping apparatus) describe the SAME textual
 *   artifact but attribute different function and different extraction
 *   profiles to it; this reading's ε (0.28) reflects real but modest cost to
 *   marginal-topic creators whose labor is spent negotiating a moving
 *   boundary, not the deletionist's near-zero extraction or the
 *   inclusionist's high extraction from excluded knowledge domains.
 *
 * KEY AGENTS:
 *   - afd_participants: agenda_setter/beneficiary (organized/mobile) — continuously re-derive notability through deliberation
 *   - editing_community: beneficiary (organized/mobile) — benefits from a living, recalibrating standard
 *   - marginal_topic_creators: payer (powerless/constrained) — bears cost when the moving boundary resolves against them
 *   - subject_matter_specialist_wikiprojects: excluded (moderate/constrained) — expertise underweighted in general deliberation
 *   - wikimedia_foundation: observer (institutional/analytical) — infrastructural, does not adjudicate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deliberative_reading, 0.28).
domain_priors:suppression_score(notability_guidelines__deliberative_reading, 0.32).
domain_priors:theater_ratio(notability_guidelines__deliberative_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deliberative_reading, scaffold).
narrative_ontology:human_readable(notability_guidelines__deliberative_reading, "WP:N as Perpetual Deliberative Process (AfD-Mediated Boundary Negotiation)").
narrative_ontology:topic_domain(notability_guidelines__deliberative_reading, "digital_commons_governance/knowledge_infrastructure/platform_constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__deliberative_reading).
narrative_ontology:has_sunset_clause(notability_guidelines__deliberative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deliberative_reading, 'f0f20ddf-9a11-4664-9771-53b0dc774fb6').
narrative_ontology:cs_kernel_codification('f0f20ddf-9a11-4664-9771-53b0dc774fb6', distributed).
narrative_ontology:cs_authority_grounding('f0f20ddf-9a11-4664-9771-53b0dc774fb6', practice).
narrative_ontology:cs_interpretation_layer_present('f0f20ddf-9a11-4664-9771-53b0dc774fb6').
narrative_ontology:cs_reading_relation('f0f20ddf-9a11-4664-9771-53b0dc774fb6', notability_guidelines__deletionist_reading, influences).
narrative_ontology:cs_reading_relation('f0f20ddf-9a11-4664-9771-53b0dc774fb6', notability_guidelines__inclusionist_reading, influences).
narrative_ontology:cs_axiom('f0f20ddf-9a11-4664-9771-53b0dc774fb6', foundational, notability_is_process_output_not_fixed_input).
narrative_ontology:cs_axiom_status(notability_is_process_output_not_fixed_input, holdable).
narrative_ontology:cs_axiom_grounding('f0f20ddf-9a11-4664-9771-53b0dc774fb6', notability_is_process_output_not_fixed_input, conventional).
narrative_ontology:cs_axiom('f0f20ddf-9a11-4664-9771-53b0dc774fb6', secondary, boundary_revision_through_deliberation_is_legitimating).
narrative_ontology:cs_axiom_status(boundary_revision_through_deliberation_is_legitimating, holdable).
narrative_ontology:cs_axiom_grounding('f0f20ddf-9a11-4664-9771-53b0dc774fb6', boundary_revision_through_deliberation_is_legitimating, instrumental).
narrative_ontology:cs_reference_frame('f0f20ddf-9a11-4664-9771-53b0dc774fb6', consensus_based_boundary_negotiation).
narrative_ontology:cs_drift_state('f0f20ddf-9a11-4664-9771-53b0dc774fb6', contemporary_afd_practice, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('f0f20ddf-9a11-4664-9771-53b0dc774fb6', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deliberative_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, editing_community).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, readers_seeking_reliable_coverage).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, afd_participants).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, marginal_topic_creators).
narrative_ontology:constraint_vindicates(notability_guidelines__deliberative_reading, consensus_based_epistemics).
narrative_ontology:constraint_vindicates(notability_guidelines__deliberative_reading, process_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Editors who show up at Articles for Deletion discussions, argue keep/delete/merge on a given article against the current wording of WP:N and its subject-specific guidelines, and in doing so continuously re-derive what 'notable' means in practice. Their accumulated precedent, not the guideline text alone, is what actually decides borderline cases. They can walk away from any single discussion without cost; the process persists whether or not any one of them participates.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, afd_participants, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deliberative_reading, afd_participants, beneficiary).

% The broader body of editors relies on a working, if perpetually contested, standard to keep the encyclopedia from becoming an unmanageable directory of every possible subject. The deliberative process gives them a living mechanism to recalibrate that standard as sourcing practices, topic areas, and social consensus about significance shift over time.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, editing_community, beneficiary,
    organized, generational, mobile, global).

% Readers benefit from an encyclopedia whose coverage has been filtered by ongoing argument rather than static rule application, on the theory that deliberation catches cases a fixed rule would get wrong in either direction. They have no direct participation but can always exit to other sources.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, readers_seeking_reliable_coverage, beneficiary,
    powerless, biographical, mobile, global).

% Editors who create articles on topics near the current boundary of notability bear the direct cost when a discussion resolves against them: the article is deleted or merged, their labor is unrecouped, and they must either appeal through deletion review, wait for the boundary to move, or abandon the topic. From this seat the process looks less like negotiation and more like a verdict they did not get to co-author on equal footing with experienced AfD regulars.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, marginal_topic_creators, payer,
    powerless, biographical, constrained, global).

% Editors organized around niche topic areas (regional culture, genre fiction, non-Western biography) often have domain expertise about a subject's significance but limited standing in general AfD discourse, which runs on Wikipedia-wide sourcing norms rather than domain-specific expertise; they show up to argue but are frequently outnumbered by generalist participants applying the guideline more literally.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, subject_matter_specialist_wikiprojects, excluded,
    moderate, biographical, constrained, global).

% Provides the technical and legal infrastructure within which the community self-governs notability but does not itself adjudicate individual AfD outcomes or set the substantive content of WP:N, treating the deliberative process as a matter of community self-governance it declines to override.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wikimedia_foundation, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__deliberative_reading, diffuse).
narrative_ontology:fixing_cost_class(notability_guidelines__deliberative_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem of keeping a boundary concept (what merits independent encyclopedic treatment) responsive to changing sourcing landscapes and topic areas without requiring a formal amendment process for every shift — AfD functions as a distributed, continuously running boundary-update mechanism rather than a one-time rule.
% TRANSFER_FUNCTION: Moves editorial attention and inclusion/exclusion decisions from any single author's unilateral judgment to a rotating deliberative body of participants; costs (deleted labor, unrecouped article-creation effort) land disproportionately on editors whose topics sit near the current boundary, especially those without sustained AfD experience.
% ABSENT_VOICES: Subject-matter WikiProjects with topic expertise but weak general-discourse standing, and the subjects of articles themselves (who typically have no standing to participate in a discussion about their own notability), are structurally underrepresented relative to editors fluent in AfD's argumentative conventions.
% DISAPPEARANCE_RATIONALE: If AfD deliberation vanished overnight, notability determinations would revert to unilateral editor judgment or a static, unrevisable rule; the boundary would either freeze at its current position or fragment into inconsistent local practice across topic areas, and the self-correcting mechanism that currently absorbs disputed cases would disappear.
% FOUNDING_PROBLEM: Early Wikipedia needed some non-arbitrary way to decide which topics warranted a standalone article, since neither 'anything anyone writes about' nor a single editor's fiat scaled to a mass-collaborative project; AfD was built to let the community resolve these questions case by case rather than freeze a rule in advance.
% FOUNDING_PROBLEM_CORROBORATION: Academic studies of Wikipedia governance (e.g. peer-reviewed work on Wikipedia's bureaucratic evolution) and outside journalistic coverage of AfD controversies attest that the boundary-negotiation function remains actively exercised and contested, not merely nominal; this corroboration comes from researchers and reporters outside the editing community itself, not solely from AfD participants describing their own process.
narrative_ontology:disappearance_verdict(notability_guidelines__deliberative_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deliberative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deliberative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(notability_guidelines__deliberative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deliberative_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__deliberative_reading_tests).
:- end_tests(notability_guidelines__deliberative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28) because this reading holds that the process genuinely re-derives the boundary rather than merely applying a fixed rule to extract exclusion — the cost borne by marginal-topic creators is real but is a byproduct of the negotiation mechanism functioning, not the mechanism's purpose. Suppression is moderate (0.32): AfD does not suppress alternatives to itself (editors can appeal, relist, or wait for consensus to shift) but does suppress unilateral override of deliberative outcomes. Accessibility collapse is moderate (0.35) — once a topic is deleted, recreating it requires clearing a real, if not impossible, procedural bar (deletion review, WP:REFUND, or demonstrating changed circumstances). Resistance is moderate (0.4): creators of deleted articles do contest outcomes, but the process absorbs contestation as part of its normal operation rather than requiring it be crushed.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter/AfD-participant seat, each individual close looks like a fair application of evolving, jointly-authored consensus. From the marginal_topic_creator seat, the same close can look like an ex post application of a standard that shifted after they invested labor, with no meaningful opportunity to have shaped the standard before it was applied to them. The engine's per-seat computation should reflect this: the coordinating seats see low extraction because they experience the process as negotiation they are part of; the payer seat sees higher effective extraction because the same process, from that vantage, functions as a verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   AfD participants and the editing community sit near the beneficiary end: they run the process and gain a self-correcting standard. Marginal topic creators sit toward the target end: their labor is the input that gets tested and sometimes discarded by a process they did not design and often cannot fully navigate. Subject-matter specialists sit in between — they participate but with attenuated influence relative to generalist AfD regulars, which is a directionality fact about representation within the deliberative body itself, not merely about outsider/insider status.
 *
 * MANDATROPHY ANALYSIS:
 *   The deliberative reading exists specifically to prevent WP:N from being mislabeled as either pure Mountain (a fixed, natural boundary — the deletionist error under this reading's lights) or pure Snare (naked exclusionary gatekeeping — the inclusionist error under this reading's lights). By declaring has_sunset_clause on each specific guideline formulation (any given notability threshold is explicitly revisable and routinely revised) and requiring active enforcement (AfD closes are enforced against unilateral override), this reading captures the Scaffold structure: coordination whose justification is the transition itself — the guideline's specific wording is never meant to be the permanent settlement, only the current working draft the community is entitled to revise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    process_vs_outcome_notability,
    'Is notability genuinely re-derived by each AfD discussion (a live negotiation), or does accumulated precedent function as a de facto fixed rule that discussions merely apply while performing deliberation?',
    'Longitudinal analysis of AfD outcome consistency: if outcomes on structurally similar articles vary significantly over time in ways traceable to explicit argument (not just participant composition), the deliberative reading is supported; if outcomes are highly predictable from guideline text and precedent alone regardless of discussion content, the process is closer to rule-application theater.',
    'If discussions are largely theater over a settled rule, this reading''s low theater_ratio and scaffold classification would be undermined and the constraint would look more like the deletionist reading''s Mountain framing wearing a deliberative costume — a piton candidate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(process_vs_outcome_notability, empirical, 'Whether AfD is genuine ongoing negotiation or precedent-application theater.').

omega_variable(
    representation_within_deliberation,
    'Does the deliberative body (AfD regulars) adequately represent the interests of infrequent participants and subject-matter specialists, or does experience with AfD''s argumentative conventions itself function as an informal power asymmetry within the ''coordination''?',
    'Participation studies measuring AfD regular vs. first-time/infrequent participant argument success rates, controlling for the substantive strength of sourcing arguments made.',
    'If experienced participants systematically prevail independent of argument quality, the ''deliberation'' partially converts into an insider/outsider extraction structure even under this reading''s own terms, raising the effective ε above what the surface metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(representation_within_deliberation, empirical, 'Whether AfD''s deliberative structure conceals an experience-based power asymmetry.').

omega_variable(
    kernel_framing_underdetermination,
    'Given that all three readings (deletionist, deliberative, inclusionist) describe the same textual kernel, which framing should govern policy discussion about reforming WP:N — and does the choice of framing itself determine which reforms appear legitimate?',
    'Track which framing dominates in actual Wikipedia policy RfCs about notability reform; a framing that consistently wins agenda-setting power over the others is functioning as the operative kernel reading regardless of formal neutrality.',
    'If the deliberative framing is invoked mainly to legitimate outcomes that the inclusionist reading would flag as exclusionary, the deliberative reading risks functioning as a legitimating narrative for the inclusionist reading''s Snare, rather than as an independent structural fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the deliberative framing itself is contested terrain in policy discourse, not a neutral description.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deliberative_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deliberative_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nota_tr_t4, notability_guidelines__deliberative_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement(nota_tr_t8, notability_guidelines__deliberative_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(nota_tr_t12, notability_guidelines__deliberative_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(nota_tr_t16, notability_guidelines__deliberative_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__deliberative_reading, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deliberative_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(nota_be_t4, notability_guidelines__deliberative_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(nota_be_t8, notability_guidelines__deliberative_reading, base_extractiveness, 8, 0.23).
narrative_ontology:measurement(nota_be_t12, notability_guidelines__deliberative_reading, base_extractiveness, 12, 0.25).
narrative_ontology:measurement(nota_be_t16, notability_guidelines__deliberative_reading, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__deliberative_reading, base_extractiveness, 20, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__deliberative_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(nota_su_t4, notability_guidelines__deliberative_reading, suppression_requirement, 4, 0.27).
narrative_ontology:measurement(nota_su_t8, notability_guidelines__deliberative_reading, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(nota_su_t12, notability_guidelines__deliberative_reading, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(nota_su_t16, notability_guidelines__deliberative_reading, suppression_requirement, 16, 0.31).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__deliberative_reading, suppression_requirement, 20, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deliberative_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(notability_guidelines__deliberative_reading, 0.1).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, notability_guidelines__deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, notability_guidelines__inclusionist_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the natural-language label 'WP:N' into structurally distinct claims sharing one kernel (notability_guidelines): deletionist_reading (near-Mountain, low ε, quality-filter function), deliberative_reading (this story; Scaffold, moderate ε, boundary-negotiation function), and inclusionist_reading (Tangled Rope/Snare, high ε, gatekeeping-apparatus function with named excluded-knowledge victims). Each carries its own claimed_type, metrics, and stakeholder set per the ε-invariance principle; they are linked bidirectionally via affects_constraints because policy shifts in AfD deliberative practice (this story) structurally influence both the deletionist reading's claim to settledness and the inclusionist reading's evidentiary basis for claiming systematic exclusion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
