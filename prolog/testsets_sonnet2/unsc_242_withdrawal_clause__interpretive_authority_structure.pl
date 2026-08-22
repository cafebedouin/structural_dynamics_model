% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__interpretive_authority_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__interpretive_authority_structure, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__interpretive_authority_structure
 *   human_readable: Contested Authority to Interpret UNSC Resolution 242's Withdrawal Clause
 *   domain: international_law/diplomatic_history
 *
 * SUMMARY:
 *   This story instantiates the interpretive_authority_structure reading of
 *   the UNSC 242 withdrawal clause kernel: it is not about which substantive
 *   reading of the withdrawal clause is correct (that is the subject of the
 *   sibling maximal_withdrawal_reading and partial_withdrawal_reading
 *   constraints), but about the second-order dispute over WHO has the
 *   authority to settle that question. The ICJ asserts judicial-interpretive
 *   competence under customary treaty construction; the original drafting
 *   states assert that their own recollected intent controls; the occupying
 *   state asserts that decades of subsequent practice have themselves become
 *   authoritative. Because no single one of these claims can be enforced
 *   against the others, the meta-dispute is never resolved, and its
 *   non-resolution is precisely what keeps both substantive readings alive as
 *   live options for over half a century. This is a structurally distinct,
 *   ε-stable claim from either substantive reading and is linked to them via
 *   network.affects_constraints rather than folded into either.
 *
 * KEY AGENTS:
 *   - occupying_state_government: primary beneficiary (powerful/arbitrage) — uses customary-practice claim to consolidate facts on the ground while treaty status remains open
 *   - drafting_states_bloc: agenda-setter and secondary beneficiary (institutional/constrained) — preserves diplomatic flexibility by declining to endorse a single interpretive method
 *   - icj_and_international_legal_bodies: excluded analytical authority (institutional/analytical) — claims the legitimate interpretive method but lacks compulsory jurisdiction to enforce it
 *   - displaced_populations_seeking_status_resolution: primary victim (powerless/trapped) — bears the accumulated cost of unresolved legal status across generations
 *   - unsc_permanent_members_with_veto: structural beneficiary (institutional/arbitrage) — protects its own future interpretive latitude by blocking any move to designate one authoritative method
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.81).
domain_priors:suppression_score(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.72).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, extractiveness, 0.81).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__interpretive_authority_structure, snare).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__interpretive_authority_structure, "Contested Authority to Interpret UNSC Resolution 242's Withdrawal Clause").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__interpretive_authority_structure, "international_law/diplomatic_history").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__interpretive_authority_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__interpretive_authority_structure, '841eda9b-c3dc-436c-89d0-0d6c20e0b0e9').
narrative_ontology:cs_kernel_codification('841eda9b-c3dc-436c-89d0-0d6c20e0b0e9', fixed_text).
narrative_ontology:cs_authority_grounding('841eda9b-c3dc-436c-89d0-0d6c20e0b0e9', distributed).
narrative_ontology:cs_reading_relation('841eda9b-c3dc-436c-89d0-0d6c20e0b0e9', unsc_242_withdrawal_clause__maximal_withdrawal_reading, influences).
narrative_ontology:cs_reading_relation('841eda9b-c3dc-436c-89d0-0d6c20e0b0e9', unsc_242_withdrawal_clause__partial_withdrawal_reading, influences).
narrative_ontology:cs_axiom('841eda9b-c3dc-436c-89d0-0d6c20e0b0e9', foundational, no_single_body_holds_exclusive_interpretive_competence).
narrative_ontology:cs_axiom_status(no_single_body_holds_exclusive_interpretive_competence, holdable).
narrative_ontology:cs_axiom_grounding('841eda9b-c3dc-436c-89d0-0d6c20e0b0e9', no_single_body_holds_exclusive_interpretive_competence, conventional).
narrative_ontology:cs_axiom('841eda9b-c3dc-436c-89d0-0d6c20e0b0e9', secondary, subsequent_practice_can_independently_generate_authority).
narrative_ontology:cs_axiom_status(subsequent_practice_can_independently_generate_authority, holdable).
narrative_ontology:cs_axiom_grounding('841eda9b-c3dc-436c-89d0-0d6c20e0b0e9', subsequent_practice_can_independently_generate_authority, conventional).
narrative_ontology:cs_reference_frame('841eda9b-c3dc-436c-89d0-0d6c20e0b0e9', unsc_resolution_as_negotiated_compromise_text).
narrative_ontology:cs_drift_state('841eda9b-c3dc-436c-89d0-0d6c20e0b0e9', post_vienna_convention_customary_codification, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('841eda9b-c3dc-436c-89d0-0d6c20e0b0e9', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_government).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_permanent_members_with_veto).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, displaced_populations_seeking_status_resolution).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, neighboring_states_seeking_border_certainty).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, international_law_practitioners_seeking_precedent).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states_bloc).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__interpretive_authority_structure, sovereign_equality_of_interpretive_claims).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__interpretive_authority_structure, resolution_242_as_living_instrument).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invokes customary practice and subsequent state conduct as the controlling interpretive method, arguing that decades of administration and negotiated partial withdrawals constitute authoritative practice-based meaning. Because no single body can compel it to accept a rival interpretive method, it can indefinitely defer resolution while consolidating facts on the ground. Its exit option from any unfavorable interpretive ruling is simply non-recognition of that body's authority to rule.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_government, beneficiary,
    powerful, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_government, agenda_setter).

% The United States and United Kingdom, as principal drafters, assert that authorial intent (their own recollected negotiating history) should control interpretation, and periodically issue statements reaffirming this. They benefit from the ambiguity insofar as it preserves diplomatic flexibility and avoids forcing an ally into a legal posture, but they are constrained by the need to maintain credibility as neutral drafters and by pressure from other UN members to endorse a definitive reading.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states_bloc, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states_bloc, beneficiary).

% Claims that judicial interpretation under customary rules of treaty construction (as later codified in the Vienna Convention) is the legitimate method for resolving the ambiguity, and has issued advisory opinions touching on related territorial questions. It has no enforcement mechanism against a permanent Security Council member's non-cooperation and is structurally excluded from binding adjudication because none of the interested parties has submitted the underlying dispute to compulsory jurisdiction.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, icj_and_international_legal_bodies, excluded,
    institutional, civilizational, analytical, global).

% Live under conditions whose legal character (occupied, disputed, or otherwise) depends entirely on which interpretive authority ultimately controls, but no authority is able to render a binding determination. They bear the accumulated cost of the unresolved status across generations — statelessness, restricted movement, unresolved property and return claims — with no venue capable of closing the question.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, displaced_populations_seeking_status_resolution, payer,
    powerless, civilizational, trapped, regional).

% Require a settled boundary to plan security arrangements, water rights, and economic development, but every negotiation restarts from the unresolved question of whose interpretive method governs. Their exit options are limited to unilateral security measures or prolonged diplomatic stalemate, since no adjudicative body can bind the occupying state.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, neighboring_states_seeking_border_certainty, payer,
    moderate, generational, constrained, regional).

% Scholars, tribunals, and legal advisers who need a settled precedent on how to allocate interpretive authority over ambiguous Security Council resolutions more broadly. The unresolved meta-dispute over 242 becomes a recurring citation of institutional failure, undermining confidence that similarly drafted resolutions can ever be authoritatively construed.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, international_law_practitioners_seeking_precedent, payer,
    analytical, civilizational, analytical, global).

% Retain the ability to block any Security Council resolution or referral that would designate a single authoritative interpretive method, since doing so would constrain their own future flexibility in other ambiguous drafting situations. They have no incentive to resolve the meta-dispute because an unresolved precedent protects their interpretive latitude on unrelated resolutions.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_permanent_members_with_veto, beneficiary,
    institutional, civilizational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__interpretive_authority_structure, diffuse).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__interpretive_authority_structure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, the arrangement could coordinate expectations about which body's reading of an ambiguously drafted resolution is binding, allowing all parties to plan around a single authoritative interpretation rather than perpetually relitigating meaning.
% TRANSFER_FUNCTION: Moves the cost of legal uncertainty from the parties capable of exploiting the ambiguity (the occupying state, the drafting powers, and veto-holding Council members) onto parties who need closure but cannot compel it (displaced populations, neighboring states, and the broader body of international legal practice).
% ABSENT_VOICES: The displaced populations most directly governed by whatever the withdrawal clause is eventually read to mean have no seat at the interpretive-authority table at all — they are neither the ICJ, nor a drafting state, nor the occupying state, and no forum exists in which their view of who should decide is solicited.
% DISAPPEARANCE_RATIONALE: If the meta-dispute over interpretive authority were resolved — if one body's method were accepted as controlling by all relevant parties — the underlying substantive dispute (maximal vs. partial withdrawal) would have to be adjudicated on the merits rather than perpetually deferred, forcing a determinate territorial and legal outcome that would reorganize negotiations, security postures, and population status determinations across the region.
% FOUNDING_PROBLEM: Resolution 242 was drafted with deliberately ambiguous language (the missing definite article in the English text) precisely to secure passage among states with irreconcilable positions on withdrawal scope, deferring the interpretive question rather than resolving it at drafting time.
% FOUNDING_PROBLEM_CORROBORATION: Independent treaty-law scholars outside any of the interested governments (writing in venues such as the American Journal of International Law and the British Yearbook of International Law) have documented the drafting ambiguity as intentional and have separately noted that no compulsory jurisdiction clause was ever accepted by the parties, corroborating that the authority gap is original to the instrument rather than a later invention by any single benefiting party.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__interpretive_authority_structure, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__interpretive_authority_structure, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__interpretive_authority_structure, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81 by 2024) because the unresolved authority question is not a passive gap but an actively exploited one: parties invoke whichever interpretive method currently favors their position, and none can be forced to accept a rival's method. Suppression is authored substantial (0.72) because the absence of compulsory jurisdiction is itself an enforced condition — no party with the power to create binding adjudication (a Security Council referral, an ICJ compulsory-jurisdiction declaration) has done so, and that non-creation is maintained by veto-capable actors. Theater ratio is authored moderate-to-rising (0.58) because substantial diplomatic and legal-scholarly activity around the resolution's meaning increasingly functions as performance — restating positions rather than moving toward closure — while the underlying authority question remains untouched.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying state and the veto-holding Council members are beneficiaries because the unresolved meta-dispute preserves their room to maneuver: as long as no authority is binding, no ruling can bind them either. The drafting states occupy an agenda-setting but partially self-interested position: they benefit from flexibility but are constrained by legitimacy costs. Displaced populations, border-seeking neighboring states, and the international legal community itself are victims not because a substantive judgment has gone against them, but because the absence of ANY binding judgment imposes the cost of permanent uncertainty on those least able to generate their own authoritative closure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing passage of a resolution among states with genuinely irreconcilable withdrawal positions in 1967 — was arguably live and even functional as short-term coordination at drafting time. But the founding_problem_status is authored as 'live' rather than 'dead' deliberately: the underlying territorial dispute the ambiguity was meant to paper over has never been resolved, so the drafting-era ambiguity has not become obsolete scaffolding — it has instead calcified into a permanent authority vacuum that outlived any plausible transitional justification. This is not mandatrophy resolved by disuse; it is mandatrophy actively exploited by parties who now prefer the vacuum to any determinate outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    which_interpretive_method_is_correct,
    'Among judicial interpretation (ICJ/VCLT customary rules), authorial intent (drafting-state testimony), and subsequent state practice (occupying-state conduct), is there a principled basis in general international law for treating one as controlling over the others for a pre-VCLT Security Council resolution?',
    'A binding ICJ advisory opinion accepted by all interested parties, or a Security Council resolution expressly designating an interpretive method and securing compliance from the occupying state and drafting powers.',
    'If one method were established as controlling, the underlying maximal/partial withdrawal dispute could be resolved on the merits by that method, collapsing this constraint''s extractive function; absent resolution, the ambiguity self-perpetuates indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_interpretive_method_is_correct, conceptual, 'Whether a principled hierarchy among competing interpretive authorities exists for pre-VCLT Security Council resolutions.').

omega_variable(
    authority_contest_as_deliberate_vs_emergent,
    'Was the current three-way authority contest (ICJ, drafters, occupying state) deliberately cultivated by beneficiary parties, or did it emerge organically from the absence of a compulsory jurisdiction clause in the original resolution?',
    'Archival review of internal diplomatic correspondence from the drafting states and the occupying state across the interval to determine whether interpretive-authority ambiguity was strategically reinforced (e.g., through deliberate non-referral to the ICJ) versus simply never addressed.',
    'Deliberate cultivation would support classifying this constraint as snare with clear intent; organic emergence would support a piton-adjacent reading where the extraction is now opportunistic rather than originally designed — though even in the latter case, sustained non-referral by capable parties constitutes ongoing active maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_contest_as_deliberate_vs_emergent, empirical, 'Whether the authority vacuum was strategically constructed or emerged from drafting omission and is now opportunistically exploited.').

omega_variable(
    sibling_reading_dependency,
    'If this meta-dispute over interpretive authority were resolved, would the maximal_withdrawal_reading or partial_withdrawal_reading sibling constraints become moot, or would the meta-resolution simply relocate contestation to the substantive level under the newly authoritative method?',
    'Comparative case study of other ambiguous Security Council resolutions where interpretive authority was later clarified (e.g., through ICJ advisory opinion) to observe whether substantive disputes resolved cleanly or persisted in modified form.',
    'If substantive contestation would simply relocate, this constraint''s extraction is partly irreducible to the authority question alone and the family of three readings may share deeper interdependence than the network edges currently capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_dependency, conceptual, 'Whether resolving the meta-authority dispute would resolve or merely relocate the underlying substantive withdrawal-scope dispute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__interpretive_authority_structure, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1967, 0.3).
narrative_ontology:measurement(unsc_tr_t1979, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1979, 0.38).
narrative_ontology:measurement(unsc_tr_t1991, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1991, 0.45).
narrative_ontology:measurement(unsc_tr_t2003, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2003, 0.5).
narrative_ontology:measurement(unsc_tr_t2014, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2014, 0.55).
narrative_ontology:measurement(unsc_tr_t2024, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement(unsc_be_t1979, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1979, 0.62).
narrative_ontology:measurement(unsc_be_t1991, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1991, 0.68).
narrative_ontology:measurement(unsc_be_t2003, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2003, 0.74).
narrative_ontology:measurement(unsc_be_t2014, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2014, 0.78).
narrative_ontology:measurement(unsc_be_t2024, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement(unsc_su_t1979, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1979, 0.58).
narrative_ontology:measurement(unsc_su_t1991, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1991, 0.63).
narrative_ontology:measurement(unsc_su_t2003, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2003, 0.67).
narrative_ontology:measurement(unsc_su_t2014, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2014, 0.7).
narrative_ontology:measurement(unsc_su_t2024, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, partial_withdrawal_reading).

% DUAL FORMULATION NOTE:
% This constraint, together with maximal_withdrawal_reading and partial_withdrawal_reading, forms the unsc_242_withdrawal_clause kernel family. This story is the meta-level reading: it does not adjudicate withdrawal scope but instead characterizes the contested authority to adjudicate it, and that contest is what allows the two substantive readings to remain simultaneously live rather than one displacing the other. The substantive readings each carry their own ε reflecting the extractive weight of their preferred interpretation being blocked or advanced; this story's ε reflects the extractive weight of the authority vacuum itself, which is structurally upstream of both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
