% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__legitimacy_erosion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__legitimacy_erosion_reading, []).

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
 *   constraint_id: ietf_openness_commitment__legitimacy_erosion_reading
 *   human_readable: IETF Rough Consensus as Legitimacy Erosion
 *   domain: technology_governance/internet_standards
 *
 * SUMMARY:
 *   The IETF's rough consensus mechanism is formally an open, non-voting
 *   decision procedure intended to prevent centralized control over Internet
 *   standards. In this legitimacy_erosion_reading, the mechanism has become
 *   contested terrain: well-resourced incumbent vendors and platform
 *   operators sustain continuous working-group participation, draft
 *   authorship, and procedural engagement at scales unavailable to
 *   independent contributors. They extract procedural legitimacy from the
 *   rough consensus process to ratify standards that entrench their existing
 *   market positions, technical assumptions, and barrier structures. The
 *   result is a tangled rope: the coordination function (interoperable
 *   standards without single-entity control) remains partially real, but the
 *   same structure asymmetrically extracts legitimacy and encodes
 *   gatekeeping. The concrete costs are borne by independent technical
 *   contributors and non-incumbent implementers who are procedurally
 *   outlasted or overridden.
 *
 * KEY AGENTS:
 *   - well_resourced_incumbents: Primary beneficiary (powerful/mobile) â extracts procedural legitimacy to ratify self-serving standards
 *   - independent_technical_contributors: Primary target (moderate/constrained) â technical input overridden by volume and persistence
 *   - ietf_process_administrators: Agenda setter (institutional/constrained) â judges consensus under pressure from dominant voices
 *   - non_incumbent_implementers: Secondary target (moderate/constrained) â bears implementation costs of incumbent-favorable complexity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, 0.72).
domain_priors:suppression_score(ietf_openness_commitment__legitimacy_erosion_reading, 0.68).
domain_priors:theater_ratio(ietf_openness_commitment__legitimacy_erosion_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__legitimacy_erosion_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__legitimacy_erosion_reading, "IETF Rough Consensus as Legitimacy Erosion").
narrative_ontology:topic_domain(ietf_openness_commitment__legitimacy_erosion_reading, "technology_governance/internet_standards").

domain_priors:requires_active_enforcement(ietf_openness_commitment__legitimacy_erosion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__legitimacy_erosion_reading, '9e996a4b-fb80-4079-aa1a-66f4a040e4ed').
narrative_ontology:cs_kernel_codification('9e996a4b-fb80-4079-aa1a-66f4a040e4ed', distributed).
narrative_ontology:cs_authority_grounding('9e996a4b-fb80-4079-aa1a-66f4a040e4ed', practice).
narrative_ontology:cs_interpretation_layer_present('9e996a4b-fb80-4079-aa1a-66f4a040e4ed').
narrative_ontology:cs_reading_relation('9e996a4b-fb80-4079-aa1a-66f4a040e4ed', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('9e996a4b-fb80-4079-aa1a-66f4a040e4ed', ietf_openness_commitment__capture_substrate_reading, coexists_with).
narrative_ontology:cs_axiom('9e996a4b-fb80-4079-aa1a-66f4a040e4ed', foundational, procedural_openness_is_capture_vulnerable).
narrative_ontology:cs_axiom_status(procedural_openness_is_capture_vulnerable, holdable).
narrative_ontology:cs_axiom_grounding('9e996a4b-fb80-4079-aa1a-66f4a040e4ed', procedural_openness_is_capture_vulnerable, empirically_contingent).
narrative_ontology:cs_axiom('9e996a4b-fb80-4079-aa1a-66f4a040e4ed', foundational, legitimacy_commons_is_extractable).
narrative_ontology:cs_axiom_status(legitimacy_commons_is_extractable, holdable).
narrative_ontology:cs_axiom_grounding('9e996a4b-fb80-4079-aa1a-66f4a040e4ed', legitimacy_commons_is_extractable, empirically_contingent).
narrative_ontology:cs_reference_frame('9e996a4b-fb80-4079-aa1a-66f4a040e4ed', open_participatory_governance).
narrative_ontology:cs_drift_state('9e996a4b-fb80-4079-aa1a-66f4a040e4ed', post_commercialization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9e996a4b-fb80-4079-aa1a-66f4a040e4ed', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_incumbents).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, independent_technical_contributors).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, non_incumbent_implementers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy large engineering staffs to IETF working groups, authoring drafts, attending interim meetings, and sustaining procedural engagement at scales smaller actors cannot match. They leverage rough consensus informality to shepherd standards that entrench existing product architectures, patent positions, and operational assumptions, extracting procedural legitimacy as a resource that ratifies self-serving outcomes.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_incumbents, beneficiary,
    powerful, generational, mobile, global).

% Working group chairs and area directors responsible for judging rough consensus, managing mailing lists, setting agendas, and declaring when objections have been adequately addressed. They are structurally dependent on participant density and face implicit pressure to declare consensus when dominant organizational voices align, while formally adhering to open-process norms.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, ietf_process_administrators, agenda_setter,
    institutional, generational, constrained, global).

% Individual engineers, academics, and open-source maintainers who contribute technical analysis to standards discussions on personal or small-project time. Their objections are procedurally acknowledged but often overridden by incumbent volume, persistent re-submission, or chair judgments that the working group has moved on, leaving them with the choice of exhausted persistence or silent acceptance.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, independent_technical_contributors, payer,
    moderate, biographical, constrained, global).

% Small companies, startups, and open-source projects that must implement IETF standards for interoperability. They bear the cost when captured standards embed incumbent-favored complexity, scaled operational assumptions, or implicit patent licensing structures that reduce competitive viability and raise barrier to entry.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, non_incumbent_implementers, payer,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_incumbents).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__legitimacy_erosion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a distributed, non-voting decision procedure for Internet technical standards that avoids single-entity or governmental control and seeks to incorporate diverse technical input into interoperable specifications.
% TRANSFER_FUNCTION: Moves procedural legitimacy and ratification authority from a diffuse technical community to well-resourced organizational actors who can sustain continuous working-group participation, converting the consensus mechanism into a legitimacy engine for incumbent-blessed standardization.
% ABSENT_VOICES: Independent contributors from developing economies who cannot afford continuous travel and meeting attendance; civil society and public-interest technologists who would raise competition and equity concerns but are procedurally framed as out-of-scope; end-users who experience downstream effects of captured standards but have no seat in the process.
% DISAPPEARANCE_RATIONALE: If the rough consensus mechanism and its procedural safeguards vanished, standards development would fragment across proprietary consortia, regional regulatory bodies, and corporate-controlled forums. Incumbents would lose a key legitimacy engine, while independent actors would lose a nominally open venue that at least affords theoretical participation, and the Internet protocol stack would face balkanization.
% FOUNDING_PROBLEM: Preventing centralized or governmental control over Internet protocol standards; enabling voluntary interoperability across heterogeneous networks without requiring unanimous consent.
% FOUNDING_PROBLEM_CORROBORATION: Internet historians and early IETF participants attest the anti-centralization motive. Contemporary critical infrastructure scholars and some former IETF area directors corroborate that the threat model has shifted from government capture to corporate capture, and that the rough consensus mechanism no longer effectively safeguards against concentrated private power.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__legitimacy_erosion_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__legitimacy_erosion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__legitimacy_erosion_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ietf_openness_commitment__legitimacy_erosion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__legitimacy_erosion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__legitimacy_erosion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the mechanism converts diffuse technical participation into ratified standards that systematically reflect the preferences of actors who can afford sustained engagement. Suppression (0.68) is substantial: procedural informality is actively enforced through chair authority, mailing-list moderation, and the humming convention, which together suppress structured dissent that cannot match incumbent repetition. Theater ratio (0.55) reflects that a growing share of process activity maintains the appearance of open consensus while substantive outcomes are prefigured by concentrated resources. Accessibility collapse (0.60) captures that alternative standards-development venues exist but lack the IETF's accumulated legitimacy, making exit costly. Resistance (0.45) is moderate: criticism is visible but fragmented and rarely overrides incumbent momentum. The measurement series share a single time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (process administrators) experiences the constraint as a delicate coordination mechanism under genuine technical complexity, requiring active management to reach any decision at all. The beneficiary seat (incumbents) experiences it as a legitimate venue that happens to validate their engineering preferences. The payer seats (independent contributors and smaller implementers) experience the same structure as a procedural ratchet where sustained resource advantage is laundered into technical consensus. The engine computes these divergences from the structural data; the author does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to well_resourced_incumbents, who collect legitimacy and market-entrenching standards from the process â their directionality sits near the beneficiary pole. Victim declarations map to independent_technical_contributors and non_incumbent_implementers, who bear the costs of reduced influence and incumbent-favored complexity â their directionality sits near the target pole. Ietf_process_administrators are agenda_setters without direct rent collection; their structural directionality is closer to symmetric but slightly beneficiary-adjacent because their institutional authority depends on the process appearing successful. The extraction is identity-locked for administrators (professional identity fused to the IETF process) and constrained for contributors.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the rough consensus mechanism as a pure rope by requiring both beneficiaries and victims for tangled_rope certification. A pure rope reading would predict no concentrated victim group and no asymmetric extraction; the presence of identifiable payers whose objections are procedurally diluted, combined with incumbents who systematically capture outcomes, blocks that classification. Conversely, the remaining genuine coordination function â the IETF does produce interoperable standards that no single government controls â prevents snare classification, which would require the coordination story to be cover. The mechanism is hybrid: it coordinates and extracts through the same structural channel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ietf_kernel_reading_contest,
    'This constraint is the legitimacy_erosion_reading of the ietf_openness_commitment kernel. Would adopting the commons_stewardship_reading or capture_substrate_reading reclassify the mechanism as rope or snare respectively?',
    'Comparative analysis of the sibling constraints'' structural data, metric profiles, and stakeholder directionality computed by the engine.',
    'If sibling readings produce different computed types, the kernel is genuinely contested structurally; if all readings converge on tangled_rope, the disagreement is normative rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ietf_kernel_reading_contest, conceptual, 'Contested kernel reading divergence for IETF openness commitment').

omega_variable(
    procedural_neutrality,
    'Does the informalism of rough consensus structurally advantage well-resourced repeat players who can sustain continuous participation, or is the observed capture an incidental and remediable artifact?',
    'Demographic and participation-pattern analysis of working-group attendance, draft authorship, and objection outcomes by organizational size and funding level.',
    'If confirmed, the coordination function is inseparable from the extraction function and tangled_rope holds; if disconfirmed, the mechanism may be a rope with incidental inequality that could be repaired.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_neutrality, empirical, 'Whether rough consensus informalism inherently advantages incumbents').

omega_variable(
    legitimacy_vs_actor_extraction,
    'Is the primary extraction target the abstract legitimacy commons itself, or does the mechanism extract directly from identifiable actor classes with legitimacy erosion as a side effect?',
    'Trace the causal chain of specific standardization outcomes to measure whether incumbent gains are larger than, equal to, or smaller than the costs imposed on independent contributors and implementers.',
    'If legitimacy is the primary target, the constraint''s effective scope is broader than its stakeholder list suggests; if actors are the primary targets, the legitimacy erosion is a downstream epiphenomenon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_vs_actor_extraction, conceptual, 'Whether extraction targets legitimacy commons or actor welfare primarily').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__legitimacy_erosion_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ietf_tr_t5, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(ietf_tr_t10, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(ietf_tr_t15, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(ietf_tr_t25, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(ietf_tr_t30, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(ietf_be_t5, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(ietf_be_t10, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(ietf_be_t15, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ietf_be_t25, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(ietf_be_t30, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(ietf_su_t5, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 5, 0.25).
narrative_ontology:measurement(ietf_su_t10, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(ietf_su_t15, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(ietf_su_t25, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(ietf_su_t30, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__legitimacy_erosion_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, capture_substrate_reading).

% DUAL FORMULATION NOTE:
% This constraint is the legitimacy_erosion_reading of the ietf_openness_commitment kernel. The kernel decomposes into three structurally distinct claims: commons_stewardship_reading (predominantly coordination, rope-like), capture_substrate_reading (predominantly extraction, snare-like), and legitimacy_erosion_reading (hybrid coordination/extraction, tangled_rope). Each reading carries a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
