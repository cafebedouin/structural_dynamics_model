% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__treaty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__treaty_primacy_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__treaty_primacy_reading
 *   human_readable: Treaty Primacy Reading of Secession Legitimacy
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This story instantiates one reading of the secession_legitimacy_boundary
 *   kernel: the claim that Indigenous treaty rights predate and structurally
 *   supersede both federal and provincial sovereignty claims, such that no
 *   secession is legitimate without treaty holder consent. This is a distinct
 *   constraint from sibling readings — constitutional_impossibility_reading
 *   (which grounds illegitimacy in constitutional amendment procedure, not
 *   treaty priority), popular_sovereignty_reading (which locates ultimate
 *   legitimacy in provincial referendum majorities and would treat treaty
 *   consent as merely one negotiated input, not a veto), and
 *   grievance_threshold_reading (which locates legitimacy in accumulated
 *   injustice, largely orthogonal to treaty temporal priority). Each reading
 *   has a different beneficiary/victim structure and a different ε; they are
 *   linked via network.affects_constraints, not merged into one story.
 *
 * KEY AGENTS:
 *   - treaty_nations_with_recognized_title: primary beneficiary/agenda-setter — holds the consent veto this reading establishes
 *   - federal_crown_as_treaty_counterparty: secondary beneficiary — gains standing as necessary intermediary
 *   - separatist_provincial_movements: primary target — faces a legitimacy bar this reading imposes on top of referendum success
 *   - treaty_nations_without_recognized_title: excluded victim class — equally old claims, but structurally unprotected by the recognition threshold this reading relies on
 *   - constitutional_courts: analytical observer — would determine whether the veto is binding law or persuasive norm
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, 0.42).
domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, 0.58).
domain_priors:theater_ratio(secession_legitimacy_boundary__treaty_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__treaty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__treaty_primacy_reading, "Treaty Primacy Reading of Secession Legitimacy").
narrative_ontology:topic_domain(secession_legitimacy_boundary__treaty_primacy_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__treaty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__treaty_primacy_reading, '221bf5bc-cf93-43c1-ae5e-7a78df95b237').
narrative_ontology:cs_kernel_codification('221bf5bc-cf93-43c1-ae5e-7a78df95b237', distributed).
narrative_ontology:cs_authority_grounding('221bf5bc-cf93-43c1-ae5e-7a78df95b237', distributed).
narrative_ontology:cs_reading_relation('221bf5bc-cf93-43c1-ae5e-7a78df95b237', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('221bf5bc-cf93-43c1-ae5e-7a78df95b237', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('221bf5bc-cf93-43c1-ae5e-7a78df95b237', secession_legitimacy_boundary__grievance_threshold_reading, influences).
narrative_ontology:cs_axiom('221bf5bc-cf93-43c1-ae5e-7a78df95b237', foundational, treaty_relationships_predate_settler_sovereignty_division).
narrative_ontology:cs_axiom_status(treaty_relationships_predate_settler_sovereignty_division, holdable).
narrative_ontology:cs_axiom_grounding('221bf5bc-cf93-43c1-ae5e-7a78df95b237', treaty_relationships_predate_settler_sovereignty_division, deontological).
narrative_ontology:cs_axiom('221bf5bc-cf93-43c1-ae5e-7a78df95b237', foundational, consent_of_prior_rights_holder_is_precondition_not_negotiable_input).
narrative_ontology:cs_axiom_status(consent_of_prior_rights_holder_is_precondition_not_negotiable_input, holdable).
narrative_ontology:cs_axiom_grounding('221bf5bc-cf93-43c1-ae5e-7a78df95b237', consent_of_prior_rights_holder_is_precondition_not_negotiable_input, deontological).
narrative_ontology:cs_reference_frame('221bf5bc-cf93-43c1-ae5e-7a78df95b237', pre_confederation_nation_to_nation_treaty_relationship).
narrative_ontology:cs_drift_state('221bf5bc-cf93-43c1-ae5e-7a78df95b237', contemporary_secession_movement_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('221bf5bc-cf93-43c1-ae5e-7a78df95b237', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, treaty_nations_with_recognized_title).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, federal_crown_as_treaty_counterparty).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, separatist_provincial_movements).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, treaty_nations_without_recognized_title).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds treaty rights predating both federal and provincial constitutional orders; under this reading, any secession attempt affecting their territory requires their consent, giving them an effective veto. They administer this claim through litigation, direct negotiation, and international forums. Exit from the settler-state framework entirely is not available to them because their land and jurisdiction are physically embedded within the contested territory; their leverage comes from the priority of their claim, not from mobility.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, treaty_nations_with_recognized_title, beneficiary,
    organized, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, treaty_nations_with_recognized_title, agenda_setter).

% As the treaty signatory, the federal government benefits structurally from a reading that requires treaty holder consent for secession, because it forecloses provincial unilateralism and preserves federal standing as the necessary intermediary. It can invoke treaty primacy selectively, deploying it against separatist claims while remaining otherwise inconsistent in honoring the same treaties in resource and land disputes.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_crown_as_treaty_counterparty, beneficiary,
    institutional, generational, mobile, national).

% Seeks to leave the federation on the basis of a provincial referendum majority. Under this reading, that mandate is insufficient and possibly void without treaty holder consent, since much of the province's territory and resource base sits within treaty lands the movement would need to govern. Their exit path is blocked entirely at the legitimacy stage, not merely made costly — the reading treats their claim as structurally incomplete regardless of vote share.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, separatist_provincial_movements, payer,
    organized, biographical, trapped, regional).

% Nations whose treaty or title claims remain unsettled, contested, or unrecognized by courts and governments fall outside the protective structure this reading offers to nations with adjudicated title. If secession proceeds, their consent is not sought because their claim has not been formally recognized, even though their territory and rights may be equally at stake. They bear the risk of exclusion from a framework that only protects recognized claimants.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, treaty_nations_without_recognized_title, payer,
    powerless, generational, trapped, regional).

% Would prefer a reading of secession legitimacy grounded purely in provincial constitutional procedure or popular sovereignty, where treaty rights are a negotiated matter subordinate to provincial jurisdiction over its own territory. Under treaty primacy, its authority to negotiate exit terms is structurally subordinated to a third party's consent, a position it did not choose and cannot easily contest without appearing to attack Indigenous rights directly.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_government_incumbent, excluded,
    institutional, biographical, constrained, regional).

% Would be called upon to adjudicate whether treaty primacy actually operates as a legal veto or as a political norm without binding force. Their eventual ruling would determine whether this reading operates as enforceable law or as a persuasive but non-binding claim in secession negotiations.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__treaty_primacy_reading, treaty_nations_with_recognized_title).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__treaty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for treaty nations to prevent unilateral redrawing of jurisdictional boundaries that were never party to their original agreements — coordinating around the principle that sovereignty transfers require consent from all parties whose rights predate the transferring authorities.
% TRANSFER_FUNCTION: Moves veto power and negotiating leverage over secession outcomes from provincial electorates and federal legislatures to treaty nations with recognized title; correspondingly moves the cost of achieving any secession from a simple majority threshold to a multi-party consent requirement.
% ABSENT_VOICES: Treaty nations without formally recognized or adjudicated title are structurally present in the territory but functionally absent from the consent requirement as this reading operationalizes it — their claims are equally old but institutionally unrecognized, so the veto mechanism does not extend to them.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned, separatist movements would face one fewer legitimacy obstacle and could proceed on constitutional-procedure or referendum grounds alone; treaty nations would lose their strongest legal lever against being reassigned to a new sovereign without consent. Whether the world 'rearranges' depends on whether courts ever actually enforced the veto or whether it functioned mainly as leverage in negotiation — a live dispute among the parties themselves.
% FOUNDING_PROBLEM: Colonial-era and post-colonial treaties were negotiated between Indigenous nations and the Crown, predating the federal-provincial division of powers; the founding problem is that no mechanism existed to prevent settler-state successors from unilaterally reassigning treaty obligations when internal boundaries shift, effectively treating Indigenous consent as irrelevant to sovereignty transfers over their own territory.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars working outside any government or Indigenous-nation payroll, and multiple United Nations special rapporteur reports on Indigenous rights, corroborate that the underlying problem of consent-exclusion in sovereignty transfers remains unresolved in domestic law; this corroboration comes from outside both the treaty nations asserting the claim and the federal government invoking it selectively.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__treaty_primacy_reading, contested).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__treaty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__treaty_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__treaty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).
:- end_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) and rising: as separatist movements gain political momentum over the measured interval, the cost this reading imposes on them (having to secure treaty holder consent on top of a democratic mandate) grows more consequential, and the reading's suppressive force against unilateral secession attempts intensifies correspondingly (suppression_requirement rising from 0.40 to 0.58). Theater ratio is low-moderate and rising slowly (0.15 to 0.28) — the treaty-consent requirement is substantively invoked in litigation and negotiation, not merely ceremonial, though performative citation of treaty primacy by federal actors who do not otherwise honor treaty obligations in other domains does introduce a growing theatrical component. Accessibility collapse is moderate (0.45): separatist movements retain other legitimacy pathways (constitutional amendment, negotiated settlement) even where this specific reading blocks unilateral action, so alternatives are not fully foreclosed. Resistance is high (0.72) because separatist and provincial actors actively contest the treaty-primacy framing as inserting an unelected third party into a democratic self-determination question.
 *
 * PERSPECTIVAL GAP:
 *   From the treaty nations' seat, this reading operationalizes a genuinely prior legal claim being properly respected — a coordination function correcting a historical exclusion. From the separatist movement's seat, the same structure functions as an externally imposed veto that nullifies a democratic mandate. The federal government's seat is doubly asymmetric: it benefits from treaty primacy as a check on provincial unilateralism while facing no equivalent constraint on its own capacity to unilaterally alter treaty terms outside the secession context — this is the seat divergence the engine should register structurally.
 *
 * DIRECTIONALITY LOGIC:
 *   Treaty nations with recognized title are declared beneficiaries because the reading directly manufactures their veto power; their exit options are constrained rather than mobile because their claim's force depends on continued embeddedness in the contested territory, not on relocation. Separatist movements are declared victims/payers because the reading imposes a consent cost on their preferred exit path with no reciprocal benefit; their exit options are trapped at the legitimacy-determination stage specifically because this reading treats their referendum mandate as necessary but not sufficient. Treaty nations without recognized title are also victims despite sharing the same underlying claim type as the beneficiary group — their powerlessness arises from non-recognition, not from a different quality of claim, which is the asymmetry the recognized/unrecognized split in this reading produces.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that sovereignty transfers among settler successor governments could otherwise proceed with zero regard for pre-existing treaty relationships — remains live rather than resolved, per corroboration from international legal scholarship outside both benefiting parties. This blocks a mandatrophy read: the constraint is not a vestigial mandate persisting past its function, but the tangled-rope structure (genuine coordination function for recognized treaty nations, real cost imposed on separatist movements and unrecognized nations) means it cannot be waved through as a pure rope either. The requires_active_enforcement flag and dual beneficiary/victim declaration keep the classification honest to that hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_or_negotiating_leverage,
    'Does treaty holder consent function as a binding legal veto enforceable by courts, or as a strong political/moral claim that shapes negotiation but could ultimately be overridden by sufficiently determined federal and provincial action?',
    'A definitive appellate or supreme court ruling directly addressing whether treaty rights create an enforceable consent requirement in a secession scenario (as opposed to the resource/land-use contexts where such rulings already exist) would resolve this; absent such a ruling, the question remains a matter of unresolved constitutional interpretation.',
    'If binding veto, this reading operates with much lower theater_ratio and functions as tangled_rope with strong enforcement; if merely persuasive leverage, the theater_ratio should be revised upward and the classification drifts toward scaffold or piton (a claim maintained rhetorically without binding force).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_or_negotiating_leverage, empirical, 'Whether treaty-consent operates as enforceable law or as negotiating leverage.').

omega_variable(
    recognition_threshold_arbitrariness,
    'Is the line between ''recognized title'' and ''unrecognized title'' treaty nations a principled legal distinction, or an artifact of which claims happened to be litigated and adjudicated first, meaning the exclusion of unrecognized-title nations from this reading''s protective structure is itself an extractive byproduct of an arbitrary administrative history?',
    'Comparative analysis of the legal merits of recognized versus unrecognized claims — do unrecognized claims fail on substance, or merely on procedural exhaustion, funding for litigation, or historical timing of court dockets?',
    'If the distinction is largely arbitrary, the victim status of treaty_nations_without_recognized_title is more clearly an artifact of this reading''s operationalization rather than a neutral legal boundary, strengthening the case that the reading itself, not just its application, produces an extractive exclusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recognition_threshold_arbitrariness, conceptual, 'Whether the recognized/unrecognized title boundary is principled or an artifact of adjudicative history.').

omega_variable(
    sibling_reading_selection_pressure,
    'Which of the four sibling readings of the secession_legitimacy_boundary kernel actually governs a live secession attempt is not determined by legal text alone — it is determined by which courts, legislatures, and international bodies are asked to rule, and in what order. Is there a structural reason treaty_primacy_reading would be invoked before or after popular_sovereignty_reading in an actual crisis?',
    'Case-study analysis of actual or near-secession events where multiple legitimacy readings were invoked in sequence, tracking which reading was raised first and by which party, and whether procedural sequencing determined the eventual outcome independent of the substantive merits of each reading.',
    'If procedural sequencing dominates substantive merit, then this reading''s practical force depends heavily on which actor gets to frame the legal question first — treaty nations invoking it early versus a separatist movement securing a referendum result and international recognition before treaty claims are litigated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_selection_pressure, conceptual, 'Whether reading-selection in an actual crisis is governed by substantive merit or procedural sequencing among the four kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__treaty_primacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sece_tr_t8, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(sece_tr_t16, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(sece_tr_t32, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sece_be_t8, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(sece_be_t16, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(sece_be_t32, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sece_su_t8, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(sece_su_t16, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(sece_su_t32, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__treaty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__grievance_threshold_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the secession_legitimacy_boundary kernel. Each reading is authored as its own ε-invariant constraint with its own beneficiary/victim structure: treaty_primacy_reading (this story) makes treaty nations the primary beneficiary and separatist movements a primary victim; constitutional_impossibility_reading centers federal/provincial constitutional actors; popular_sovereignty_reading centers provincial electorates as beneficiaries and would treat treaty consent as subordinate; grievance_threshold_reading centers aggrieved populations and treats both federal and provincial authority as potential victims/targets depending on grievance direction. The four are linked bidirectionally via affects_constraints; none should be read as a parameter of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
