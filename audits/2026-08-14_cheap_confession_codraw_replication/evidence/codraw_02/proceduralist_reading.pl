% ============================================================================
% CONSTRAINT STORY: proceduralist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_proceduralist_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: proceduralist_reading
 *   human_readable: Proceduralist Reading: Precommitment Procedure as Gate on Evidentiary Standing
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   In contested empirical and theoretical disputes, a procedural apparatus
 *   has emerged that only certifies disagreement as legitimate evidence once
 *   both sides have publicly precommitted to a design that could, in
 *   principle, falsify their position — adversarial collaboration protocols,
 *   preregistered analysis plans, declared kill conditions. The coordination
 *   function is real: it disciplines the retrospective narrative-fitting that
 *   keeps unfalsifiable disputes alive forever. But the procedure's cost
 *   structure is not neutral across claimants. Compliance requires
 *   institutional resourcing, statistical sophistication, and often the
 *   capacity to negotiate a kill condition with a well-lawyered opposing camp
 *   — resources distributed unevenly and correlated with existing
 *   institutional power, not with the truth-value of the underlying position.
 *   A second-order failure compounds the first: parties skilled at drafting
 *   technically-compliant but practically untriggerable kill conditions can
 *   pass the gate while defeating its purpose, converting the very mechanism
 *   meant to discipline motivated reasoning into a laundering process for it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(proceduralist_reading, 0.55).
domain_priors:suppression_score(proceduralist_reading, 0.62).
domain_priors:theater_ratio(proceduralist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(proceduralist_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(proceduralist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(proceduralist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(proceduralist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(proceduralist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(proceduralist_reading, tangled_rope).
narrative_ontology:human_readable(proceduralist_reading, "Proceduralist Reading: Precommitment Procedure as Gate on Evidentiary Standing").
narrative_ontology:topic_domain(proceduralist_reading, "epistemology/philosophy_of_technology/institutional_analysis").

domain_priors:requires_active_enforcement(proceduralist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(proceduralist_reading, 'ace4cae1-2493-46ec-a7a4-ebd28347b850').
narrative_ontology:cs_kernel_codification('ace4cae1-2493-46ec-a7a4-ebd28347b850', formalized).
narrative_ontology:cs_authority_grounding('ace4cae1-2493-46ec-a7a4-ebd28347b850', practice).
narrative_ontology:cs_interpretation_layer_present('ace4cae1-2493-46ec-a7a4-ebd28347b850').
narrative_ontology:cs_reading_relation('ace4cae1-2493-46ec-a7a4-ebd28347b850', proceduralist_reading__standpoint_reading, coexists_with).
narrative_ontology:cs_reading_relation('ace4cae1-2493-46ec-a7a4-ebd28347b850', proceduralist_reading__pragmatist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ace4cae1-2493-46ec-a7a4-ebd28347b850', proceduralist_reading__instrumentalist_reading, influences).
narrative_ontology:cs_axiom('ace4cae1-2493-46ec-a7a4-ebd28347b850', foundational, evidentiary_force_derives_from_procedural_cost).
narrative_ontology:cs_axiom_status(evidentiary_force_derives_from_procedural_cost, holdable).
narrative_ontology:cs_axiom_grounding('ace4cae1-2493-46ec-a7a4-ebd28347b850', evidentiary_force_derives_from_procedural_cost, instrumental).
narrative_ontology:cs_axiom('ace4cae1-2493-46ec-a7a4-ebd28347b850', secondary, precommitment_disciplines_motivated_reasoning_independent_of_standpoint).
narrative_ontology:cs_axiom_status(precommitment_disciplines_motivated_reasoning_independent_of_standpoint, holdable).
narrative_ontology:cs_axiom_grounding('ace4cae1-2493-46ec-a7a4-ebd28347b850', precommitment_disciplines_motivated_reasoning_independent_of_standpoint, empirically_contingent).
narrative_ontology:cs_reference_frame('ace4cae1-2493-46ec-a7a4-ebd28347b850', unstructured_positional_entrenchment).
narrative_ontology:cs_drift_state('ace4cae1-2493-46ec-a7a4-ebd28347b850', post_replication_crisis_formalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ace4cae1-2493-46ec-a7a4-ebd28347b850', '').
narrative_ontology:cs_kernel_id(proceduralist_reading, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(proceduralist_reading, procedure_compliant_researchers).
narrative_ontology:constraint_beneficiary(proceduralist_reading, institutions_hosting_adversarial_collaborations).
narrative_ontology:constraint_beneficiary(proceduralist_reading, well_resourced_labs_able_to_preregister).
narrative_ontology:constraint_victim(proceduralist_reading, under_resourced_claimants).
narrative_ontology:constraint_victim(proceduralist_reading, novel_hypothesis_originators_without_institutional_backing).
narrative_ontology:constraint_victim(proceduralist_reading, positions_that_cannot_specify_kill_conditions_ex_ante).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(proceduralist_reading, procedure_gamers).
narrative_ontology:constraint_victim(proceduralist_reading, procedure_compliant_researchers).
narrative_ontology:constraint_vindicates(proceduralist_reading, procedural_cost_as_evidentiary_warrant).
narrative_ontology:constraint_vindicates(proceduralist_reading, precommitment_disciplines_motivated_reasoning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer the preregistration and kill-condition protocols that determine whose disagreement counts as evidence. They set the terms of what a 'legitimate' dispute must survive, decide what counts as a valid kill condition, and adjudicate compliance. They can revise the procedure and are rarely themselves bound by its costs in the same way claimants are.
narrative_ontology:constraint_stakeholder(proceduralist_reading, adversarial_collaboration_designers, agenda_setter,
    institutional, generational, arbitrage, national).

% Have the statistical staff, institutional review support, and time horizon to design rigorous preregistrations and articulate falsifiable kill conditions in advance. The procedure converts their existing resource advantage into an evidentiary advantage: their claims clear the bar not because the claims are truer but because they can afford the compliance costs.
narrative_ontology:constraint_stakeholder(proceduralist_reading, well_resourced_labs_able_to_preregister, beneficiary,
    powerful, biographical, mobile, national).

% Submit to the full precommitment apparatus, bear its real costs in time and forgone flexibility, and in exchange gain a claim that is treated as adjudicated rather than merely asserted. They benefit from the legitimacy the procedure confers but pay for it directly.
narrative_ontology:constraint_stakeholder(proceduralist_reading, procedure_compliant_researchers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(proceduralist_reading, procedure_compliant_researchers, payer).

% Hold positions that may be substantively correct but cannot afford the design, staffing, or time cost of a rigorous adversarial collaboration. Their disagreement is treated as noise until it clears a procedural bar they structurally cannot reach, regardless of its truth-value.
narrative_ontology:constraint_stakeholder(proceduralist_reading, under_resourced_claimants, payer,
    powerless, biographical, trapped, national).

% Propose genuinely new framings that have not yet accumulated the institutional scaffolding (funding, co-signatories, prior literature) needed to specify a credible kill condition ex ante. The procedure penalizes novelty structurally, since kill-condition specification is easiest for claims that are already well-mapped.
narrative_ontology:constraint_stakeholder(proceduralist_reading, novel_hypothesis_originators_without_institutional_backing, payer,
    powerless, biographical, trapped, national).

% Some positions are structurally resistant to ex-ante falsifiability specification (emergent, qualitative, or paradigm-level claims) without this indicating bad faith or unfalsifiability in principle. They are excluded from the evidentiary economy the procedure creates, not because they are wrong, but because the procedure's format cannot represent them.
narrative_ontology:constraint_stakeholder(proceduralist_reading, positions_that_cannot_specify_kill_conditions_ex_ante, excluded,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_non_agent(proceduralist_reading, positions_that_cannot_specify_kill_conditions_ex_ante).

% Learn to draft kill conditions that are technically falsifiable but practically unfalsifiable (vague enough to never trigger, or narrow enough to be trivially satisfied), or to select collaborators who will not press hard against a preferred outcome. They pass the procedure's compliance test while defeating its actual evidentiary purpose, capturing the legitimacy premium without bearing the epistemic risk the procedure was designed to impose.
narrative_ontology:constraint_stakeholder(proceduralist_reading, procedure_gamers, beneficiary,
    organized, biographical, arbitrage, national).

% Host, certify, and publicize adversarial collaborations, gaining reputational credit as arbiters of rigor. They benefit from the existence and visibility of the procedure regardless of whether it reliably tracks truth, since their institutional standing rests on administering the gate rather than on the outcomes it produces.
narrative_ontology:constraint_stakeholder(proceduralist_reading, institutions_hosting_adversarial_collaborations, beneficiary,
    institutional, generational, arbitrage, national).

% Study whether preregistration and kill-condition procedures actually improve calibration and truth-tracking, or whether they primarily redistribute evidentiary legitimacy toward whoever can afford compliance. They can observe base rates of procedure gaming and differential access but do not administer or bear the procedure's costs themselves.
narrative_ontology:constraint_stakeholder(proceduralist_reading, epistemic_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(proceduralist_reading, procedure_gamers).
narrative_ontology:fixing_cost_class(proceduralist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves a real problem: unstructured positional disagreement allows both sides to retrospectively narrate any outcome as confirming, letting motivated reasoning masquerade as evidence. A designed procedure with public precommitment and declared kill conditions forces both sides to specify in advance what would count against them, which is a real epistemic gain when it is honored.
% TRANSFER_FUNCTION: Moves evidentiary legitimacy from raw positional assertion to whoever can bear the procedure's design, staffing, and time costs. In practice this transfers standing from resource-poor, novel, or hard-to-formalize positions toward resource-rich, institutionally backed, well-mapped positions, and further toward whoever is skilled at drafting technically-compliant but practically toothless kill conditions.
% ABSENT_VOICES: Positions that are correct but cannot afford preregistration, and positions that are structurally resistant to ex-ante falsifiability specification without being unfalsifiable in principle, are not represented in the procedure's evidentiary economy at all — they simply do not clear the gate and their absence is read as absence of a serious claim rather than absence of resources.
% DISAPPEARANCE_RATIONALE: Proponents (well-resourced labs, hosting institutions) would say abandoning the procedure returns the field to unadjudicable partisan noise — a real regression. Excluded and under-resourced claimants would say the procedure never adjudicated their disputes in the first place, since they were never able to enter it; for them its disappearance changes nothing except removing a legitimacy monopoly currently held by whoever can afford compliance.
% FOUNDING_PROBLEM: Positional disagreement in contested empirical and theoretical domains was becoming unfalsifiable in practice: both camps could reinterpret any result as consistent with their prior view, so disagreement persisted indefinitely without any mechanism forcing genuine risk.
% FOUNDING_PROBLEM_CORROBORATION: Methodologists and replication-crisis researchers outside any specific adversarial collaboration attest the original problem (unfalsifiable positional entrenchment) was real and remains partly live. But several of those same outside observers, plus organizations tracking preregistration compliance, also attest that a growing share of registered kill conditions are drafted to be practically untriggerable — corroboration for founding-problem persistence does not extend to corroboration that the current procedural apparatus still serves it.
narrative_ontology:disappearance_verdict(proceduralist_reading, contested).
narrative_ontology:founding_problem_status(proceduralist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(proceduralist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(proceduralist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(proceduralist_reading, 0.55, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(proceduralist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(proceduralist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(proceduralist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high and rising (0.32 to 0.55) because the founding coordination function (forcing genuine ex-ante risk) is real but is increasingly captured by procedure-gaming rather than defeated by outright refusal to precommit — the procedure survives, its purpose erodes underneath it. Theater ratio rises in parallel (0.15 to 0.40) tracking the growing gap between the visible ceremony of precommitment and its actual falsification force. Suppression is substantial (0.62) because the procedure's gate is enforced through institutional gatekeeping of what counts as a 'legitimate' adversarial collaboration — under-resourced or novel claimants are not merely disadvantaged, they are structurally excluded from producing evidence the field will recognize at all. Accessibility collapse is moderate (0.5): the procedure's format is at least nominally open to anyone who can afford it, unlike a pure natural-law collapse, so alternatives are suppressed rather than logically foreclosed. Resistance is real (0.6) — under-resourced claimants and observers studying replication contest the procedure's neutrality.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and hosting-institution seats, the procedure is functioning coordination: it visibly disciplines motivated reasoning and produces adjudicated claims where there used to be entrenched partisan noise. From the under-resourced claimant and novel-hypothesis-originator seats, the same structure is an access toll that excludes valid disagreement on grounds unrelated to its truth-value. The engine computes these as different seat-level types from the same structural facts; neither seat is wrong about its own position, and the divergence is exactly what the tangled-rope classification is tracking.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries under this reading are procedural, exactly as the kernel context specifies: whoever can bear or evade the precommitment cost benefits, independent of social standing per se. Well-resourced labs benefit through resource-enabled compliance; procedure gamers benefit through technical compliance without substantive exposure; hosting institutions benefit through reputational capture of the gate itself. Victims are whoever is genuinely bound by a real kill condition without the resources to design a favorable one, or whoever cannot enter the procedure's evidentiary economy at all — under-resourced claimants, novel-hypothesis originators, and positions resistant to ex-ante falsifiability specification. This converts what could look like a standing-based axis (whose social position counts) into a compliance-based axis (who can pay the procedure's toll) — the structural point of this reading relative to its siblings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unfalsifiable, retrospectively-narrated positional entrenchment) has not disappeared — it remains partly live, which blocks a clean piton/mandatrophy read. But the rising theater ratio and the emergence of procedure-gaming as an organized beneficiary role indicate the mandate is drifting: the apparatus increasingly certifies compliance with a ritual rather than genuine exposure to falsification. This is not full mandatrophy (the function has not fully atrophied) but is the trajectory a mandatrophy audit would want to flag — the founding_problem_status is authored as contested rather than dead precisely because both readings (function is still live vs. function has been substantially captured) have outside corroboration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Positional disagreement as evidence is read by different parties as proceduralist (standing follows compliance with a designed precommitment protocol), standpoint-based (standing follows lived positional access), pragmatist (standing follows practical consequences of acting on the claim), or instrumentalist (standing follows predictive success independent of mechanism). This story instantiates only the proceduralist reading.',
    'Not resolvable within a single reading by construction — each reading is a separate constraint (standpoint_reading, pragmatist_reading, instrumentalist_reading) with its own ε, beneficiaries, and victims, linked via network.affects_constraints. Resolution would require comparative analysis across all four sibling stories, not further analysis within this one.',
    'Conflating readings would produce an incoherent averaged ε and a beneficiary/victim structure that mixes procedural-compliance capture with positional-standing capture — exactly the ε-invariance failure the decomposition rule exists to prevent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'This story is one of four sibling readings of the positional_disagreement_as_evidence kernel; the readings are not substitutable.').

omega_variable(
    gaming_detectability,
    'Can practically-untriggerable-but-technically-falsifiable kill conditions be reliably distinguished from genuine, hard-to-satisfy-but-honest kill conditions, prior to the outcome being known?',
    'Retrospective audit of a corpus of preregistered adversarial collaborations: compare kill-condition specificity and pre-outcome expert ratings of triggerability against whether the condition was ever actually triggered across the corpus, controlling for base rates.',
    'If gaming is reliably detectable ex ante, the procedure could add a meta-review layer that would reduce the extraction this story measures — moving the classification toward genuine tangled_rope-with-safeguards or even rope. If gaming is not reliably detectable except in hindsight, the extraction is closer to structurally irreducible under the current design.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gaming_detectability, empirical, 'Whether kill-condition gaming can be caught before, rather than only after, the fact.').

omega_variable(
    resource_gate_vs_genuine_rigor,
    'Is the resource cost of a rigorous adversarial collaboration an unavoidable cost of genuine rigor (a real coordination floor), or is a substantial share of that cost artificially inflated by institutional gatekeeping practices (journal requirements, funder preferences, professional norms) that could be redesigned to lower the barrier without sacrificing rigor?',
    'Comparative institutional analysis of low-cost preregistration and adversarial collaboration formats (e.g. informal public precommitment via preprint plus community-verified kill condition) against high-cost formal formats, measured against downstream calibration outcomes.',
    'If costs are substantially reducible without rigor loss, the extraction measured here is largely a constructed access toll rather than an inherent coordination cost, which would push the classification toward snare; if costs track a genuine rigor floor, the tangled_rope classification (real coordination function plus real but bounded extraction) is the accurate one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_gate_vs_genuine_rigor, empirical, 'Whether the procedure''s cost structure is an inherent rigor floor or an inflated, redesignable gate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(proceduralist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(proc_tr_t0, proceduralist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(proc_tr_t4, proceduralist_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(proc_tr_t8, proceduralist_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(proc_tr_t12, proceduralist_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(proc_tr_t16, proceduralist_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(proc_tr_t20, proceduralist_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(proc_tr_t24, proceduralist_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(proc_be_t0, proceduralist_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(proc_be_t4, proceduralist_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(proc_be_t8, proceduralist_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(proc_be_t12, proceduralist_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(proc_be_t16, proceduralist_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(proc_be_t20, proceduralist_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(proc_be_t24, proceduralist_reading, base_extractiveness, 24, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(proc_su_t0, proceduralist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(proc_su_t4, proceduralist_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(proc_su_t8, proceduralist_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(proc_su_t12, proceduralist_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(proc_su_t16, proceduralist_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(proc_su_t20, proceduralist_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(proc_su_t24, proceduralist_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(proceduralist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(proceduralist_reading, standpoint_reading).
narrative_ontology:affects_constraint(proceduralist_reading, pragmatist_reading).
narrative_ontology:affects_constraint(proceduralist_reading, instrumentalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings decomposed from the positional_disagreement_as_evidence kernel per the ε-invariance principle: proceduralist_reading (this story), standpoint_reading, pragmatist_reading, instrumentalist_reading. Each reading authors its own ε, beneficiary/victim structure, and claimed_type from a distinct account of what makes positional disagreement count as evidence. The proceduralist reading uniquely converts standing into a compliance-based axis: its victims are defined by inability to bear procedural cost, not by social position per se, which is the structural delta relative to the standpoint reading in particular.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
