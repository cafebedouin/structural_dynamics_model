% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: Winnability Rhetorical Taboo / Operational Persistence
 *   domain: strategic/military/international_relations
 *
 * SUMMARY:
 *   After 1945, the winnability of great-power war became structurally
 *   ambiguous. Nuclear weapons made total victory indistinguishable from
 *   mutual suicide, yet military planners could not abandon contingency
 *   planning. The constraint that emerged: winnability became
 *   rhetorical-taboo (publicly unsayable, treated as incoherent by
 *   declaratory doctrine) while operationally persisting (classified plans
 *   premised constrained-but-achievable victory, damage-limitation scenarios,
 *   selective targeting). This constraint is ONE READING of the contested
 *   kernel 'war_winnability_post_1945': this reading emphasizes the SPLIT
 *   between rhetorical contraction (winnability banished from public
 *   discourse) and strategic persistence (winnability-planning continuing in
 *   classified channels). The beneficiaries are strategic planners and the
 *   executive apparatus (who gain operational flexibility without public
 *   accountability). The victims are democratic publics and their legislative
 *   representatives (who bear the risk and pay the budget but cannot inform
 *   themselves about the strategic premises). The rhetoric-operations split
 *   makes oversight impossible: the taboo disables public debate, suppressing
 *   the very questions that would make informed oversight possible.
 *
 * KEY AGENTS:
 *   - military_strategic_planners: Institutional power, generational time horizon, constrained exit. Maintain classified winnability-planning while publicly disavowing winnability.
 *   - executive_security_apparatus: Institutional power, generational time horizon, constrained exit. Enforce the rhetorical boundary through classification; manage the perimeter of what can be said; retain unilateral decision authority.
 *   - democratic_publics: Organized power, biographical time horizon, identity-locked exit. Nominal sovereigns bearing the risk and cost, prevented from knowing or debating the operational premises.
 *   - legislative_oversight_bodies: Powerful institutional seats, biographical horizon, constrained exit. Hold formal authority but are structurally prevented from exercising it — access to classified plans is limited; public debate is held under the taboo.
 *   - peer_adversary_states: Institutional power, generational horizon, trapped exit. Targets of the plans, kept in uncertainty about whether the plans premise winnability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.68).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.79).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "Winnability Rhetorical Taboo / Operational Persistence").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic/military/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, '5a60a319-88dc-41d5-8e91-9ffb91e9156a').
narrative_ontology:cs_kernel_codification('5a60a319-88dc-41d5-8e91-9ffb91e9156a', fixed_text).
narrative_ontology:cs_authority_grounding('5a60a319-88dc-41d5-8e91-9ffb91e9156a', extraction).
narrative_ontology:cs_interpretation_layer_present('5a60a319-88dc-41d5-8e91-9ffb91e9156a').
narrative_ontology:cs_reading_relation('5a60a319-88dc-41d5-8e91-9ffb91e9156a', war_winnability_post_1945__countervailing_thinkable, coexists_with).
narrative_ontology:cs_reading_relation('5a60a319-88dc-41d5-8e91-9ffb91e9156a', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_axiom('5a60a319-88dc-41d5-8e91-9ffb91e9156a', foundational, winnability_rhetorical_erasure_enables_apparatus_autonomy).
narrative_ontology:cs_axiom_status(winnability_rhetorical_erasure_enables_apparatus_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('5a60a319-88dc-41d5-8e91-9ffb91e9156a', winnability_rhetorical_erasure_enables_apparatus_autonomy, instrumental).
narrative_ontology:cs_axiom('5a60a319-88dc-41d5-8e91-9ffb91e9156a', foundational, operational_winnability_planning_requires_secrecy).
narrative_ontology:cs_axiom_status(operational_winnability_planning_requires_secrecy, holdable).
narrative_ontology:cs_axiom_grounding('5a60a319-88dc-41d5-8e91-9ffb91e9156a', operational_winnability_planning_requires_secrecy, empirically_contingent).
narrative_ontology:cs_reference_frame('5a60a319-88dc-41d5-8e91-9ffb91e9156a', deterrence_by_uncertain_retaliation).
narrative_ontology:cs_drift_state('5a60a319-88dc-41d5-8e91-9ffb91e9156a', contemporary_post_cold_war, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5a60a319-88dc-41d5-8e91-9ffb91e9156a', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, military_strategic_planners).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, executive_security_apparatus).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, democratic_publics).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, legislative_oversight_bodies).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, mutual_assured_destruction_axiom).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, deterrence_stability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain classified war plans (SIOP, warfighting strategies, damage-limitation scenarios) that premise winnability — constrained but achievable victory through selective targeting, escalation control, or first-strike capability. Publicly disavow winnability as taboo while operationally planning for scenarios where war could be won at acceptable cost. The taboo provides cover for continued planning that would face public pressure if disclosed. Their planning flexibility depends on this rhetorical-operational split.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, military_strategic_planners, agenda_setter,
    institutional, generational, constrained, global).

% Enforces the rhetorical boundary through classification authority, manages the perimeter of what can be said in public forums, and maintains the taboo as structural policy. Protects planners from accountability by keeping war-planning assumptions secret. Benefits from the constraint by retaining unilateral decision authority over deterrence and escalation — public discourse cannot question what it does not know exists.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, executive_security_apparatus, agenda_setter,
    institutional, generational, constrained, global).

% Are the nominal sovereigns authorizing the strategic apparatus, yet are prevented from knowing or debating the operational premises of the plans made in their name. They bear the risk (nuclear annihilation) and furnish the resources (military budget, technological capacity) but cannot question whether the underlying strategic assumptions are defensible because the assumptions are classified and the public discourse declares winnability unthinkable. Their exit is identity-locked: they cannot leave or exit the security state; they are constitutively inside it through citizenship.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, democratic_publics, payer,
    organized, biographical, identity_locked, global).

% Hold nominal authority to authorize military budgets and strategic doctrine, but are structurally prevented from exercising it. Access to classified plans is limited to select committees under non-disclosure constraints; public debate (in which legislatures are accountable to constituents) is held under the winnability taboo, preventing informed legislative choice. If legislators attempted to debate operational winnability assumptions, they would violate classification rules and face legal consequences. The constraint splits their formal authority from their operative capacity.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, legislative_oversight_bodies, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, legislative_oversight_bodies, excluded).

% Are the presumed targets of the plans, yet are kept in uncertainty about whether the plans premise winnability. The rhetorical taboo obscures strategic intent; adversaries must infer whether deterrence is 'assured destruction' (no winnability) or 'war-fighting with damage limitation' (winnability constrained but achievable). This ambiguity is functionally useful for deterrence but prevents explicit negotiation about strategic stability and reduces the possibility of mutual arms-control agreements based on shared understanding of the constraints.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, peer_adversary_states, excluded,
    institutional, generational, trapped, global).

% Attempt to reason about nuclear strategy in published academic and policy journals, but operate under the rhetorical taboo: formal public argument treats winnability as incoherent or immoral, while classified-world assumptions treat it as operationally constrained but real. The taboo creates a two-world epistemology: public intellectuals reason from premises (deterrence, mutual vulnerability) that official planners treat as constraints, not as axioms. Their analysis oscillates between the two worlds without resolving the underlying split.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_intellectuals_public_sphere, observer,
    moderate, generational, mobile, global).

% Campaign against nuclear weapons on the premise that winnability is (and should be) unthinkable — that the taboo expresses a moral truth. They would mobilize against operational winnability assumptions if those assumptions were public. Their exclusion is functional: the taboo makes them appear to have won a rhetorical victory (winnability is unsayable) while the operational premises persist unchanged. The constraint absorbs their activism as evidence of its own success.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, deterrence_advocacy_movements, excluded,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__rhetorical_contraction, military_strategic_planners).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__rhetorical_contraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves deterrence stability by publicly declaring war unwinnable (foreclosing first-strike temptation and reassuring adversaries that escalation will not be pursued rationally), while operationally maintaining planning flexibility to respond if deterrence fails. The coordination problem solved: how to maintain both (a) credible no-first-use commitment (communicated by rhetorical winnability-taboo) and (b) capacity to limit damage if war occurs (operationally pursued through winnability-constrained planning). The taboo coordinates the deterrence messaging, the plans coordinate the operational response.
% TRANSFER_FUNCTION: Moves decision authority and accountability away from democratic publics and their representatives, toward the executive security apparatus and military planners. The transfer is mediated through the taboo: public debate is disabled (winnability cannot be discussed), so publics cannot exercise informed oversight. The apparatus retains unilateral authority to plan for scenarios it publicly declares unthinkable. The flow is one-directional: from publics (who bear the risk and pay the budget) to planners (who design strategy in secret).
% ABSENT_VOICES: Democratic constituencies who would contest the winnability-planning assumption if it were public; strategic theorists whose public academic work treats winnability as a real option constrained by vulnerability; adversary-state strategists who would negotiate arms-control agreements based on mutual understanding of the winnability-impossibility boundary; whistleblowers and investigative journalists who attempt to make the planning assumptions public but face classification enforcement. These voices are structurally excluded from the decision-making perimeter.
% DISAPPEARANCE_RATIONALE: Deterrence-theorists argue the world would rearrange if the taboo lifted: public debate of winnability would undermine psychological deterrence, invite adversary miscalculation about U.S. willingness to escalate, and weaken the declaratory doctrine that makes deterrence credible. Abolitionists and accountability advocates argue the world would rearrange by becoming more transparent: if the operational plans and their winnability assumptions were public, legislatures could debate and potentially constrain them, democratic publics could exercise informed consent to the risks, and international negotiations could address mutual vulnerabilities more honestly. The disagreement is structural, not factual.
% FOUNDING_PROBLEM: After 1945, great-power war became potentially annihilatory: nuclear weapons made total victory indistinguishable from mutual suicide. Yet military planners could not simply declare all war-planning obsolete — they needed operational contingencies for scenarios where deterrence failed. The founding problem: how to maintain deterrence credibility (which requires the belief that escalation will not be pursued rationally) while preserving the capacity to plan for limited war if deterrence failed (which requires believing winnability is constrained but achievable)? The rhetoric-operations split emerged as a solution: declare winnability unthinkable in public discourse (reinforcing deterrence), while operationally planning for constrained winnability in classified channels (preserving flexibility).
% FOUNDING_PROBLEM_CORROBORATION: Deterrence-doctrine authorities (military strategists, arms-control theorists within the security establishment) attest the founding problem is live: winnability remains a necessary planning assumption because deterrence could fail and response options must be preserved. Democratic-accountability advocates and international-relations theorists outside the security apparatus attest the founding problem has been distorted: the split between rhetorical winnability-denial and operational winnability-planning was never a necessity — it is a choice by the apparatus to retain unilateral authority. Declassified planning documents from RAND, the Strategic Air Command, and presidential administrations (available through FOIA and the National Security Archive) corroborate that operational plans explicitly premised winnability and damage-limitation scenarios; public policy statements and declaratory doctrine explicitly denied winnability. The corroboration comes from documentary evidence outside the apparatus's current justifications.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__rhetorical_contraction, contested).
narrative_ontology:founding_problem_status(war_winnability_post_1945__rhetorical_contraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__rhetorical_contraction, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.42 to 0.68 over the interval (t=0 to t=80 years post-1945) because the apparatus progressively calcifies the taboo and invests in suppression infrastructure (classification authority, intelligence compartmentalization, legal enforcement against disclosure). The taboo was initially (1945–1960) a somewhat informal rhetorical norm; by 1980 it had hardened into institutional doctrine, training curricula, secrecy law, and professional enforcement. Theater ratio climbs from 0.38 to 0.61 because the constraint's function increasingly consists in maintaining appearances rather than coordinating genuine action. Early on, the winnability-taboo did coordinate deterrence signaling (public non-winnability messaging was functionally coupled to operational flexibility). Over time, the taboo became decoupled from deterrence stability and became instead a tool for shielding planners from accountability — the performative cost rose while the functional return diminished. Suppression rises from 0.62 to 0.79 because enforcement intensifies: classification expands, whistleblower prosecutions increase, FOIA restrictions tighten, intelligence agencies develop capabilities to monitor domestic discourse about nuclear strategy. Accessibility collapse is lower at the individual level (people can still think privately about winnability) but high at the structural level (the apparatus prevents the thought from entering institutional decision-making). Resistance is initially higher at the class level (peace movements, church opposition, academic dissidents) but organizational resistance declines over time as the taboo becomes naturalized and dissent becomes professionally costly.
 *
 * PERSPECTIVAL GAP:
 *   Why does the payer seat compute differently from the agenda-setter seat? The payer (democratic public) perceives this constraint as extraction disguised as coordination: the winnability-taboo looks like mutual agreement that war is unthinkable (coordination), but it functions to suppress the very debate that would enable informed consent. The agenda-setter (planner) perceives the constraint as necessary coordination: winnability must be taboo in order for deterrence to work; the taboo and the operational planning are two parts of a coherent strategy for maintaining peace while preserving response options. These are not different measurements of the same thing; they are different structural experiences. The agenda-setter has information access and decision authority; the payer has neither. This asymmetry is the definitional feature of a tangled_rope from one seat's perspective and a snare from the other's.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim split is stark because the constraint's primary function is access to information and decision authority. Strategic planners and the security apparatus benefit because they retain unilateral authority to plan and execute strategy without public challenge. Democratic publics and their representatives suffer because they are kept ignorant of the operational premises on which nuclear deterrence rests, preventing them from exercising informed oversight. The rhetoric-operations split is the mechanism: the taboo disables public debate, so the publics are prevented from asking questions even if they wanted to. Exit options reinforce the asymmetry: planners can stay inside the classified system and prosper; publics are identity-locked inside a security state they cannot exit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy question by documenting the founding problem and tracking its evolution. The founding problem (t=0, ~1945) was genuine: nuclear weapons created a true coordination problem — how to maintain both deterrence credibility (which requires convincing adversaries that war will not be pursued rationally) and response capacity (which requires planning for scenarios where deterrence fails). The rhetoric-operations split emerged as one possible solution: public non-winnability rhetoric coordinates deterrence signaling; operational winnability-planning preserves flexibility. Over 80 years, the founding problem has shifted from 'live' to 'contested' and the constraint has increasingly become a mechanism for shielding planners from accountability rather than coordinating deterrence. The theater_ratio climbs from 0.38 to 0.61 because the constraint's performative function (maintaining appearances) has grown while its coordination function (deterring great-power war) has become decoupled from the actual mechanism. If deterrence stability depends on convincing adversaries that war is unthinkable, then the winnability-taboo made sense. But if deterrence stability now depends on making the operation stable through arms-control treaties, mutual vulnerability acknowledgment, and transparent strategic dialogue, then the taboo becomes dysfunctional — it prevents the very negotiations that would stabilize deterrence. The mandatrophy flag fires not on the founding problem per se but on the divergence: the constraint persists as a mechanism for shielding planners (extractive function, benefiting agenda-setters), while its original coordination function (deterring nuclear war through public non-winnability messaging) has become attenuated or decoupled from the actual mechanism. The ascending theater ratio is the diagnostic: the constraint is becoming increasingly theatrical — maintaining appearances of coherence while the functional coupling has weakened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    winnability_operational_status,
    'Are classified war plans actually premised on constrained winnability, or have they evolved toward pure deterrence-by-denial (mutual vulnerability with no winnability pathway)?',
    'Declassification of SIOP documents, warfighting strategy papers, and damage-limitation planning from different eras (1960s, 1980s, 2000s, 2020s) to establish whether operational plans consistently premised winnability or whether the assumption has changed.',
    'If operational plans have already shifted to pure deterrence (no winnability pathway), the constraint is increasingly theatrical — the rhetoric-operations split persists as institutional inertia rather than functional necessity. This would reclassify toward piton. If operational plans continue to premise winnability, the constraint remains tangled_rope but the beneficiary/victim structure becomes clearer: planners benefit from operational flexibility, publics suffer from prevented accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(winnability_operational_status, empirical, 'Whether classified operational plans actually premise winnability or have shifted to pure deterrence.').

omega_variable(
    rhetorical_necessity_vs_apparatus_preference,
    'Is the winnability taboo necessary for deterrence stability, or is it a choice by the apparatus to retain unilateral authority?',
    'Comparative analysis of strategic doctrines that do NOT employ the taboo (e.g., Russian/Chinese declaratory doctrine, which openly discusses limited nuclear war scenarios); assessment of whether deterrence stability differs between regimes with and without the taboo; negotiated arms-control treaties in scenarios where winnability assumptions were made explicit and jointly acknowledged.',
    'If deterrence stability is maintained WITHOUT the taboo (as suggested by some peer-adversary doctrines), then the taboo is revealed as a choice for apparatus autonomy rather than a necessity for stability — this shifts the constraint from ''necessary coordination'' toward ''apparatus preference disguised as necessity.'' If stability requires the taboo, then the constraint remains justifiable as coordination. The impact directly affects the mandate evaluation: does the constraint still solve its founding problem, or has it become decoupled from that problem?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rhetorical_necessity_vs_apparatus_preference, conceptual, 'Whether the winnability taboo is necessary for deterrence or is a choice for apparatus autonomy.').

omega_variable(
    founding_problem_status_mutation,
    'Has the founding problem (coordinating deterrence signaling with response capacity) remained stable, or has it shifted into a different problem (apparatus autonomy from democratic oversight)?',
    'Genealogical analysis of official strategic doctrine and classified planning assumptions from different eras; interviews with retired officials; analysis of whistleblower disclosures (Pentagon Papers, Snowden materials, etc.); legislative testimony from oversight committees; comparative analysis of the rhetoric-operations split''s centrality to strategic planning (does it remain justified by deterrence logic, or has deterrence justification become secondary to apparatus autonomy?)',
    'If the founding problem has mutated, the constraint is a candidate for mandatrophy: it persists as institutional inertia (the rhetoric-operations split is how the apparatus operates) rather than as a solution to the founding problem. This would place it on the piton/theater trajectory. If the founding problem remains live and the constraint still solves it, the constraint remains tangled_rope but with declining functional necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_status_mutation, empirical, 'Whether the founding problem has remained stable or has mutated into apparatus autonomy.').

omega_variable(
    reading_foreclosure_and_coexistence,
    'Do the sibling readings (countervailing_thinkable, deterrence_unthinkable) logically foreclose this reading (rhetorical_contraction), or do they coexist as alternative positions held by different institutional actors?',
    'Analysis of strategic doctrine and planning across time and institutional actors. If countervailing_thinkable and deterrence_unthinkable are held by mutually exclusive institutional factions (e.g., Strategic Air Command vs. policy civilians), they coexist. If one reading has progressively dominated and the others are archival or theoretical only, foreclosure may apply. Assess whether the Air Force (historically countervailing-thinkable) has been overtaken by civilian policy (deterrence-unthinkable) or whether both doctrines continue to be operationally instantiated.',
    'If readings coexist, the kernel is contested and all three readings remain valid constraint stories, linked by network.affects_constraints. If one reading forecloses another, the classification of the foreclosed reading shifts toward mountain (naturalized, no longer chosen). This affects how the constraint family is modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_and_coexistence, empirical, 'Whether sibling readings foreclose each other or coexist as live positions.').

omega_variable(
    identity_lock_mechanism_suppression,
    'Is the ''identity-locked'' exit classification for democratic publics structural (economic/political dependence on the security state) or internalized (the public''s identity is fused with the nation-state such that exit is unthinkable)?',
    'Comparative analysis of how different publics respond to revelations of classified planning (e.g., the Vietnam War revelations, the Pentagon Papers, Snowden disclosures). If publics attempt to exit or change the constraint when informed, exit is identity-locked but suppression is partly internalized; they can overcome it. If publics internalize the taboo even after disclosure (treating winnability-planning as necessary or unquestionable), suppression is internalized and the identity-lock is deep.',
    'If identity-lock is structural, the constraint is more classifiable as tangled_rope (the structure forces cooperation, not internalization). If identity-lock is internalized, the constraint approaches snare (the target has internalized the extraction and defends it). This affects the classification and the strategic implications for constraint reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_suppression, empirical, 'Whether identity-lock in the democratic public is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(war__tr_t0, observed).
narrative_ontology:measurement(war__tr_t10, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(war__tr_t10, observed).
narrative_ontology:measurement(war__tr_t20, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 20, 0.44).
narrative_ontology:measurement_basis(war__tr_t20, observed).
narrative_ontology:measurement(war__tr_t30, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(war__tr_t30, observed).
narrative_ontology:measurement(war__tr_t40, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 40, 0.53).
narrative_ontology:measurement_basis(war__tr_t40, observed).
narrative_ontology:measurement(war__tr_t50, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 50, 0.57).
narrative_ontology:measurement_basis(war__tr_t50, observed).
narrative_ontology:measurement(war__tr_t60, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 60, 0.61).
narrative_ontology:measurement_basis(war__tr_t60, observed).
narrative_ontology:measurement(war__tr_t70, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 70, 0.61).
narrative_ontology:measurement_basis(war__tr_t70, projected).
narrative_ontology:measurement(war__tr_t80, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 80, 0.61).
narrative_ontology:measurement_basis(war__tr_t80, projected).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(war__be_t0, observed).
narrative_ontology:measurement(war__be_t10, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(war__be_t10, observed).
narrative_ontology:measurement(war__be_t20, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 20, 0.54).
narrative_ontology:measurement_basis(war__be_t20, observed).
narrative_ontology:measurement(war__be_t30, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(war__be_t30, observed).
narrative_ontology:measurement(war__be_t40, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 40, 0.65).
narrative_ontology:measurement_basis(war__be_t40, observed).
narrative_ontology:measurement(war__be_t50, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 50, 0.67).
narrative_ontology:measurement_basis(war__be_t50, observed).
narrative_ontology:measurement(war__be_t60, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(war__be_t60, observed).
narrative_ontology:measurement(war__be_t70, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 70, 0.68).
narrative_ontology:measurement_basis(war__be_t70, projected).
narrative_ontology:measurement(war__be_t80, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 80, 0.68).
narrative_ontology:measurement_basis(war__be_t80, projected).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(war__su_t0, observed).
narrative_ontology:measurement(war__su_t10, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(war__su_t10, observed).
narrative_ontology:measurement(war__su_t20, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(war__su_t20, observed).
narrative_ontology:measurement(war__su_t30, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 30, 0.74).
narrative_ontology:measurement_basis(war__su_t30, observed).
narrative_ontology:measurement(war__su_t40, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 40, 0.76).
narrative_ontology:measurement_basis(war__su_t40, observed).
narrative_ontology:measurement(war__su_t50, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 50, 0.78).
narrative_ontology:measurement_basis(war__su_t50, observed).
narrative_ontology:measurement(war__su_t60, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 60, 0.79).
narrative_ontology:measurement_basis(war__su_t60, observed).
narrative_ontology:measurement(war__su_t70, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 70, 0.79).
narrative_ontology:measurement_basis(war__su_t70, projected).
narrative_ontology:measurement(war__su_t80, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 80, 0.79).
narrative_ontology:measurement_basis(war__su_t80, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=80
narrative_ontology:measurement(war__grid_01, war_winnability_post_1945__rhetorical_contraction, accessibility_collapse(class), 0, 0.31).
narrative_ontology:measurement(war__grid_02, war_winnability_post_1945__rhetorical_contraction, accessibility_collapse(class), 80, 0.38).
narrative_ontology:measurement(war__grid_03, war_winnability_post_1945__rhetorical_contraction, accessibility_collapse(individual), 0, 0.22).
narrative_ontology:measurement(war__grid_04, war_winnability_post_1945__rhetorical_contraction, accessibility_collapse(individual), 80, 0.29).
narrative_ontology:measurement(war__grid_05, war_winnability_post_1945__rhetorical_contraction, accessibility_collapse(organizational), 0, 0.38).
narrative_ontology:measurement(war__grid_06, war_winnability_post_1945__rhetorical_contraction, accessibility_collapse(organizational), 80, 0.36).
narrative_ontology:measurement(war__grid_07, war_winnability_post_1945__rhetorical_contraction, accessibility_collapse(structural), 0, 0.55).
narrative_ontology:measurement(war__grid_08, war_winnability_post_1945__rhetorical_contraction, accessibility_collapse(structural), 80, 0.48).
narrative_ontology:measurement(war__grid_09, war_winnability_post_1945__rhetorical_contraction, resistance(class), 0, 0.54).
narrative_ontology:measurement(war__grid_10, war_winnability_post_1945__rhetorical_contraction, resistance(class), 80, 0.62).
narrative_ontology:measurement(war__grid_11, war_winnability_post_1945__rhetorical_contraction, resistance(individual), 0, 0.45).
narrative_ontology:measurement(war__grid_12, war_winnability_post_1945__rhetorical_contraction, resistance(individual), 80, 0.51).
narrative_ontology:measurement(war__grid_13, war_winnability_post_1945__rhetorical_contraction, resistance(organizational), 0, 0.41).
narrative_ontology:measurement(war__grid_14, war_winnability_post_1945__rhetorical_contraction, resistance(organizational), 80, 0.38).
narrative_ontology:measurement(war__grid_15, war_winnability_post_1945__rhetorical_contraction, resistance(structural), 0, 0.28).
narrative_ontology:measurement(war__grid_16, war_winnability_post_1945__rhetorical_contraction, resistance(structural), 80, 0.19).
narrative_ontology:measurement(war__grid_17, war_winnability_post_1945__rhetorical_contraction, stakes_inflation(class), 0, 0.64).
narrative_ontology:measurement(war__grid_18, war_winnability_post_1945__rhetorical_contraction, stakes_inflation(class), 80, 0.68).
narrative_ontology:measurement(war__grid_19, war_winnability_post_1945__rhetorical_contraction, stakes_inflation(individual), 0, 0.58).
narrative_ontology:measurement(war__grid_20, war_winnability_post_1945__rhetorical_contraction, stakes_inflation(individual), 80, 0.62).
narrative_ontology:measurement(war__grid_21, war_winnability_post_1945__rhetorical_contraction, stakes_inflation(organizational), 0, 0.71).
narrative_ontology:measurement(war__grid_22, war_winnability_post_1945__rhetorical_contraction, stakes_inflation(organizational), 80, 0.72).
narrative_ontology:measurement(war__grid_23, war_winnability_post_1945__rhetorical_contraction, stakes_inflation(structural), 0, 0.89).
narrative_ontology:measurement(war__grid_24, war_winnability_post_1945__rhetorical_contraction, stakes_inflation(structural), 80, 0.88).
narrative_ontology:measurement(war__grid_25, war_winnability_post_1945__rhetorical_contraction, suppression(class), 0, 0.48).
narrative_ontology:measurement(war__grid_26, war_winnability_post_1945__rhetorical_contraction, suppression(class), 80, 0.61).
narrative_ontology:measurement(war__grid_27, war_winnability_post_1945__rhetorical_contraction, suppression(individual), 0, 0.35).
narrative_ontology:measurement(war__grid_28, war_winnability_post_1945__rhetorical_contraction, suppression(individual), 80, 0.42).
narrative_ontology:measurement(war__grid_29, war_winnability_post_1945__rhetorical_contraction, suppression(organizational), 0, 0.58).
narrative_ontology:measurement(war__grid_30, war_winnability_post_1945__rhetorical_contraction, suppression(organizational), 80, 0.74).
narrative_ontology:measurement(war__grid_31, war_winnability_post_1945__rhetorical_contraction, suppression(structural), 0, 0.62).
narrative_ontology:measurement(war__grid_32, war_winnability_post_1945__rhetorical_contraction, suppression(structural), 80, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__rhetorical_contraction, 0.12).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, nuclear_deterrence_stability_doctrine).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, classification_authority_and_accountability).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'war_winnability_post_1945'. All three readings share the same referent (the standing arrangement of nuclear deterrence post-1945) but disagree on what winnability became. Rhetorical_contraction emphasizes the SPLIT between public taboo and operational persistence. Countervailing_thinkable emphasizes that winnability remains strategically real despite constraints. Deterrence_unthinkable emphasizes that winnability became rationally incoherent. Each reading has its own ε, stakeholder structure, and classification. They are linked because the choice between readings determines the classification of the entire deterrence system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__rhetorical_contraction, powerless, 0.85).
constraint_indexing:directionality_override(war_winnability_post_1945__rhetorical_contraction, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
