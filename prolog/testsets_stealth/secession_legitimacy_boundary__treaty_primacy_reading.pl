% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__treaty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: secession_legitimacy_boundary__treaty_primacy_reading
 *   human_readable: Secession Legitimacy Boundary — Treaty Primacy Reading
 *   domain: political economy/federalism/resource politics
 *
 * SUMMARY:
 *   This story instantiates the treaty_primacy_reading of the
 *   secession_legitimacy_boundary kernel: the claim that Indigenous treaty
 *   rights predate and supersede both federal and provincial authority, so
 *   that no secession is legitimate without treaty-holder consent. Following
 *   the ε-referent rule for kernel-reading stories, ε is authored for the
 *   standing arrangement under contest — the actually operating
 *   secession-legitimacy framework (clear-question referendum, bilateral
 *   federal-provincial negotiation, Indigenous agreement sought through
 *   consultation and accommodation but required at no step) — as this reading
 *   assesses it, never for the consent-gated regime the reading asserts to be
 *   the true boundary. Through treaty-primacy lights the standing framework
 *   is substantially extractive: it processes the disposition of treaty
 *   territories while rendering the prior sovereigns' agreement advisory.
 *   Constraint family note (ε-invariance decomposition): the colloquial label
 *   'secession legitimacy' covers four structurally distinct boundary claims,
 *   one per sibling reading, each a separate story with its own ε over the
 *   shared referent. This reading authors high ε because it locates a consent
 *   gate the framework omits; popular_sovereignty_reading authors low ε (the
 *   referendum is self-legitimating, nothing is missing);
 *   constitutional_impossibility_reading authors moderate ε (a gate exists
 *   but is federal-provincial rather than popular);
 *   grievance_threshold_reading authors conditionally low ε (legitimacy
 *   tracks federal conduct, not consent). Family edges are declared in
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - indigenous_treaty_nations: Primary target (organized/trapped) — bears the framework's costs; agreement advisory at every step; assertion inside the framework is the only lever
 *   - provincial_secession_movements: Primary beneficiary (powerful/constrained) — receives an orderly channel with no third-party gate over the outcome
 *   - federal_government: Dual-positioned beneficiary (institutional/constrained) — collects territorial integrity and the exclusive counterparty position; its own sovereignty claims bounded by the assertions it benefits from
 *   - provincial_governments: Beneficiary with payer costs (institutional/constrained) — provincial envelope treated as the negotiating unit; accommodation duties on the other side of the ledger
 *   - supreme_court_judiciary: Agenda-setter (institutional/analytical) — authored the framework's terms in the Secession Reference and frames restructuring questions via reference jurisdiction
 *   - metis_and_non_status_peoples: Excluded (organized/trapped) — would object to any rearrangement but holds no historic treaty standing in either the framework or the corrective claim
 *   - international_treaty_bodies: Analytical observer (institutional/analytical) — UNDRIP monitoring and FPIC observations; normative pressure without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, 0.68).
domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, 0.6).
domain_priors:theater_ratio(secession_legitimacy_boundary__treaty_primacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__treaty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__treaty_primacy_reading, "Secession Legitimacy Boundary — Treaty Primacy Reading").
narrative_ontology:topic_domain(secession_legitimacy_boundary__treaty_primacy_reading, "political economy/federalism/resource politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__treaty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__treaty_primacy_reading, 'ff8b42e1-fb8b-4c07-8e18-442e11129df9').
narrative_ontology:cs_kernel_codification('ff8b42e1-fb8b-4c07-8e18-442e11129df9', formalized).
narrative_ontology:cs_authority_grounding('ff8b42e1-fb8b-4c07-8e18-442e11129df9', lineage).
narrative_ontology:cs_interpretation_layer_present('ff8b42e1-fb8b-4c07-8e18-442e11129df9').
narrative_ontology:cs_reading_relation('ff8b42e1-fb8b-4c07-8e18-442e11129df9', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('ff8b42e1-fb8b-4c07-8e18-442e11129df9', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff8b42e1-fb8b-4c07-8e18-442e11129df9', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('ff8b42e1-fb8b-4c07-8e18-442e11129df9', foundational, treaty_sovereignty_precedes_state_authority).
narrative_ontology:cs_axiom_status(treaty_sovereignty_precedes_state_authority, holdable).
narrative_ontology:cs_axiom_grounding('ff8b42e1-fb8b-4c07-8e18-442e11129df9', treaty_sovereignty_precedes_state_authority, deontological).
narrative_ontology:cs_axiom('ff8b42e1-fb8b-4c07-8e18-442e11129df9', secondary, secession_requires_prior_sovereign_consent).
narrative_ontology:cs_axiom_status(secession_requires_prior_sovereign_consent, holdable).
narrative_ontology:cs_axiom_grounding('ff8b42e1-fb8b-4c07-8e18-442e11129df9', secession_requires_prior_sovereign_consent, deontological).
narrative_ontology:cs_reference_frame('ff8b42e1-fb8b-4c07-8e18-442e11129df9', prior_sovereignty_treaty_order).
narrative_ontology:cs_drift_state('ff8b42e1-fb8b-4c07-8e18-442e11129df9', contemporary_post_clarity_act_framework, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ff8b42e1-fb8b-4c07-8e18-442e11129df9', '2026-08-04T00:00:00Z').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, provincial_secession_movements).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, federal_government).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, federal_government).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__treaty_primacy_reading, negotiated_secession_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nations holding treaties signed with the Crown before and against the formation of the federal and provincial governments, covering territories that lie inside provincial boundaries. When secession politics move, the operating framework seeks their views through consultation and accommodation but contains no step at which their agreement can stop the process; their protection consists of litigation, their own organized referendum participation, and advocacy through international bodies. They cannot relocate away from the territories at stake, and the treaty relationship they defend is framed in their own traditions as lasting indefinitely.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_nations, payer,
    organized, civilizational, trapped, national).

% Political movements organizing a province's exit from the federation through a referendum mandate followed by negotiation. The framework gives them a defined route: a clear question and a clear majority oblige the federation to negotiate, and no third party holds a formal gate over the outcome. A unilateral declaration is foreclosed by federal enforcement and by the absence of international recognition, so the negotiated route is the only one open to them.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_secession_movements, beneficiary,
    powerful, biographical, constrained, regional).

% Govern the province whose borders and internal treaty territories any secession would rearrange. The framework treats the provincial envelope as the negotiating unit and treaty lands as items within it, which serves their jurisdictional claims; at the same time they carry consultation and accommodation duties on resource development and face the treaty nations' assertions in any negotiation they enter, which bounds what they can do unilaterally on treaty territories.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments, payer).

% Enforces the rule that a province leaves only through the negotiated channel, codified in statute after the Supreme Court's reference, and sits as the exclusive counterparty to any seceding province. It gains the preservation of territorial integrity that the treaty nations' public assertions reinforce, and it operates a framework it authored; at the same time its claimed authority over treaty matters is bounded by those same assertions and by consultation duties it cannot unilaterally discharge, and it cannot alter treaty relationships on its own motion.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_government, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, federal_government, payer).

% Authored the framework's terms in the Secession Reference: unilateral secession is unconstitutional, a clear referendum majority triggers an obligation to negotiate, and the rights of Indigenous peoples are named as central but routed into the negotiation rather than erected as a gate over it. Its reference jurisdiction lets it define how constitutional restructuring questions are framed, and it adjudicates disputes over the framework's application.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, supreme_court_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Assert Indigenous nationhood and would bear the consequences of any territorial rearrangement, but hold no historic treaty instrument of the kind both the operating framework and the treaty-primacy claim key on. They litigate for recognition of federal responsibility and demand seats in any negotiation over the territorial envelope; as of now the consent conversation runs over their heads in both its state version and its corrective version.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, metis_and_non_status_peoples, excluded,
    organized, generational, trapped, regional).

% Monitor state conduct against the UN Declaration on the Rights of Indigenous Peoples and treaty commitments, issue observations on free, prior and informed consent, and possess no direct enforcement power over the domestic secession framework. Their findings shift the normative ground on which domestic actors argue and give the treaty nations' assertions an external forum.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, international_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__treaty_primacy_reading, federal_government).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__treaty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single orderly channel for processing a provincial exit from the federation: a clear referendum question and majority create an obligation to negotiate, replacing unilateral declaration with a structured bilateral process and preserving a unified framework in which the federation's territorial integrity and the province's claim are adjudicated together.
% TRANSFER_FUNCTION: Moves disposition power over treaty territories and the terms of constitutional restructuring to the federal and provincial state actors who negotiate, and confers legitimacy on the outcome from referendum majorities rather than from the prior sovereigns whose territories are at stake; correspondingly it moves the risk of territorial transfer and treaty alteration onto Indigenous treaty holders, whose agreement is consultable at every step but required at none.
% ABSENT_VOICES: Métis nations and non-status Indigenous peoples would object that any rearrangement of the territorial envelope binds them too, but they hold no historic treaty instrument and so have no seat in either the operating framework's negotiation or the treaty-primacy claim's consent gate. Provincial voters who regard the referendum as self-legitimating are likewise outside the treaty frame. Both groups sit in litigation and political advocacy outside the constitutional conversation.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, secession politics would reorganize around unilateral assertion: a referendum majority would be treated as self-executing by its proponents, the federation would respond with enforcement or capitulation, and Indigenous nations would face territorial transfer with no structured channel in which their assertion operates. The Cree and Inuit self-organized referendum participation of 1995 shows the shape of that world — protection purchased assertion-by-assertion with no framework to carry it.
% FOUNDING_PROBLEM: After the 1995 Quebec referendum came within a point of a secession mandate, the constitutional order had no settled answer to how a federation processes a provincial exit claim: unilateral declaration threatened chaos, and the Supreme Court's 1998 reference and the 2000 Clarity Act built the channel — clear question, clear majority, obligation to negotiate — that now constitutes the standing framework.
% FOUNDING_PROBLEM_CORROBORATION: The secession movement itself corroborates the founding problem — it litigated and campaigned for a clear channel after 1995 — and Indigenous nations' interventions in the reference process attest it from outside the benefiting state parties, as does the constitutional-law literature treating the Reference/Clarity Act sequence as a direct response to the 1995 near-miss. No party denies the framework was built to channel secession claims; the contest between the readings is over whose consent the channel requires.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__treaty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__treaty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__treaty_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__treaty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   ε is reading-indexed (per OQ-26 the reading authors ε over the fixed referent): through treaty-primacy lights the framework's rendering of prior-sovereign consent advisory while it processes territorial disposition measures 0.68 — substantially extractive, short of snare-level because the framework does acknowledge the nations (the Reference names their rights central, consultation duties are real law, modern treaties operate). Suppression 0.60 is a raw structural property, unscaled by power or scope: the framework forecloses in two directions at once — unilateral secession (statutory enforcement, reference jurisdiction, international non-recognition) and hardening of the treaty-holder gate (no instrument entrenches it; each assertion must be re-made). Theater 0.48 sits just under the substitution line: the consultation-and-accommodation layer is heavily ritual (processes that convene after structural decisions are made, honor-of-the-Crown rhetoric without entrenchment) while the coordination core — clarity rules and the negotiation obligation — is functional. Accessibility collapse 0.62: for the treaty nations alternatives collapse nearly completely (no territorial exit, no parallel sovereignty channel), while secessionists retain the negotiated route. Resistance 0.58 reflects continuous two-sided pressure: the nations' litigation, referendum mobilization, and UNDRIP advocacy, and secessionist challenges to the framework from the other side. claimed_type is authored independently as tangled_rope: who is coordinated — the federal and provincial state parties and the secessionist movements, routed into a single bilateral channel; who pays — the treaty nations, whose consent the channel renders advisory. The metrics were not tuned to the claim. All three tracked series run on one shared grid (1995, 2000, 2005, 2010, 2015, 2020, 2025): base_extractiveness rises as the framework hardens (0.58 to 0.65), dips with UNDRIP and Truth-and-Reconciliation normative pressure (0.63 in 2015), and resumes rising as implementation stalls (0.68); theater_ratio peaks with consultation ritualism in 2010 (0.50) and regrows after 2020; suppression_requirement tracks enforcement-machinery build-up after the 1995 near-miss (0.55 to 0.64) then decay as secession urgency wanes (0.58), with slight renewal by 2025.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute different types from the same structural data. From the indigenous_treaty_nations seat the framework operates as enforced extraction: their consent is consultable, their territories are negotiable, their exit is nil — the engine should compute high effective extraction and a snare-flavored verdict at that seat. From the provincial_secession_movements and federal_government seats the same framework is a coordination structure they built and staffed — rope-flavored. The federal seat is genuinely dual: it collects territorial integrity and the negotiation monopoly while its sovereignty claims are bounded by the assertions it benefits from; the derivation keys it to the beneficiary end through the beneficiaries declaration, and its payer costs are carried by secondary_role rather than an override, because an override on the institutional power atom would also strike the judiciary and provincial seats, where no such dual position exists. The excluded Métis seat feeds no directionality — exclusion is commentary-grade — which is itself the structural fact omega treaty_frame_internal_exclusion records.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: provincial_secession_movements (receive the orderly channel), provincial_governments (the provincial envelope treated as the negotiating unit), federal_government (territorial integrity plus exclusive counterparty). Victim declared: indigenous_treaty_nations (agreement advisory, transfer risk borne, no exit). The declarations map to real flows: the framework moves disposition power and legitimacy-conferral to the state negotiating parties and moves transfer risk onto the treaty holders. The federal seat's gain is the most concentrated and unconditional — it accrues whether or not secession proceeds — so gain_flow names federal_government. The treaty nations' seat sits at the full-target end, amplified by trapped exit and identity-lock: the nations cannot exit the territory or the order without losing the very relationship the assertion defends, so the assertion is the only protection and must be continuously re-made. Secessionist and federal seats sit near the beneficiary end; provincial governments sit between, their benefit conditional on secession actually proceeding while their accommodation costs are immediate. Fixing the framework — entrenching the consent gate — is prohibitive for the seat that could do it: it requires reopening the constitutional settlement against the resistance of every benefiting party, which is why fixing_cost is authored prohibitive alongside a named capturer rather than a diffuse receipt. No directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the right relationships, and the one genuinely dual seat is handled structurally rather than by an override that would leak across the institutional atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how a federation processes a provincial exit claim without chaos — is live: secession movements persist and the framework is in active use, so no mandatrophy declaration is authored and no sunset applies. The tangled_rope claim is what prevents mislabeling in both directions: a pure-coordination reading would erase the omitted consent gate (the framework's central defect through this reading's lights), and a pure-extraction reading would erase the genuine orderly-channel function that even the treaty nations used when they intervened in the reference process. The framework is both at once, and the classification holds the two together. The R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no zombie flag — the arrangement's persistence tracks its founding problem, though the content of the founding answer is exactly what the four readings contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_bearer_location,
    'Whose consent constitutes the secession-legitimacy boundary — the treaty holder''s (this reading), the provincial majority''s, the federation''s amendment formula, or none absent a threshold of federal injustice?',
    'Comparative analysis across the four sibling readings'' institutional carriers: which seat each reading empowers to grant or withhold the boundary''s consent, and which courts or bodies each reading recognizes as competent to adjudicate it.',
    'This story is the treaty_primacy_reading of kernel secession_legitimacy_boundary; a sibling reading relocates the gate — popular sovereignty dissolves the treaty-holder gate entirely, constitutional impossibility replaces it with a federal-provincial monopoly, grievance threshold makes it conditional — changing the beneficiary/victim structure and the computed per-seat classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_bearer_location, conceptual, 'Which reading of the secession-legitimacy kernel this story instantiates and what each sibling would change structurally.').

omega_variable(
    consent_gate_entrenchment_status,
    'Is treaty-holder consent already binding law that the operating framework violates, or a corrective norm requiring new entrenchment?',
    'A court squarely holding that provincial secession requires treaty-holder consent — or refusing to so hold — would settle whether the standing framework operates in open violation of an existing boundary or within an acknowledged gap.',
    'If already binding, the framework''s extraction is open violation and the victim seat''s effective extraction is amplified; if corrective, the extraction is the absence of a gate the law has not yet erected, and the framework''s defenders gain a legality argument the reading must answer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_gate_entrenchment_status, empirical, 'Whether the consent gate is violated existing law or an unentrenched corrective claim.').

omega_variable(
    fpic_veto_or_consultation,
    'Does free, prior, and informed consent under UNDRIP confer a veto over territorial disposition, or a heightened consultation duty?',
    'Domestic implementation record: UNDRIP Act action plans, provincial alignment statutes, and litigation testing FPIC''s force against the negotiation framework.',
    'A veto reading hardens the gate and dates the framework''s divergence from the treaty order as violation; a consultation reading confirms the advisory structure this reading contests and keeps the theater_ratio trajectory on its current path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fpic_veto_or_consultation, conceptual, 'Whether FPIC operates as a gate or as ritual within the secession framework.').

omega_variable(
    federal_byproduct_dependence,
    'Does the federal state''s territorial-integrity benefit from Indigenous nations'' assertions make the nations'' enforcement of the boundary structurally dependent on, or co-opted by, federal interests?',
    'Motivation analysis of the nations'' own assertions — the Cree and Inuit 1995 participation was self-authored and pre-dated federal adoption of the framework — plus the counterfactual of whether the assertion would persist if federal territorial integrity were not at stake.',
    'If the assertion is functionally federal-subsidized, the gate serves federal ends and the beneficiary structure shifts toward capture; if independent, the nations'' enforcement is self-standing prior-sovereignty defense and the federal seat''s gain is a pure byproduct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federal_byproduct_dependence, empirical, 'Whether the consent gate''s enforcement primarily serves the nations'' interests or the federation''s.').

omega_variable(
    treaty_frame_internal_exclusion,
    'Does the treaty-primacy frame''s own gate on historic treaties exclude Indigenous claimants (Métis, non-status peoples) from the very consent conversation it demands for others?',
    'Legal development: Daniels-line recognition of federal responsibility, admission of modern treaties, and self-government agreements extending standing beyond historic treaty holders.',
    'If the frame structurally excludes internal claimants, the corrective reading reproduces a boundary-exclusion pattern and its victim set is understated; if modern instruments extend standing, the frame''s gate widens without dissolving and the excluded seat converts to a payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_frame_internal_exclusion, conceptual, 'Whether the treaty-primacy frame''s historic-treaty gate excludes internal Indigenous claimants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__treaty_primacy_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(secession_treaty_primacy_tr_t1995, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(secession_treaty_primacy_tr_t2000, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(secession_treaty_primacy_tr_t2005, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 2005, 0.45).
narrative_ontology:measurement(secession_treaty_primacy_tr_t2010, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 2010, 0.5).
narrative_ontology:measurement(secession_treaty_primacy_tr_t2015, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 2015, 0.46).
narrative_ontology:measurement(secession_treaty_primacy_tr_t2020, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement(secession_treaty_primacy_tr_t2025, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(secession_treaty_primacy_be_t1995, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(secession_treaty_primacy_be_t2000, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(secession_treaty_primacy_be_t2005, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement(secession_treaty_primacy_be_t2010, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(secession_treaty_primacy_be_t2015, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement(secession_treaty_primacy_be_t2020, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(secession_treaty_primacy_be_t2025, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(secession_treaty_primacy_su_t1995, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(secession_treaty_primacy_su_t2000, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(secession_treaty_primacy_su_t2005, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2005, 0.64).
narrative_ontology:measurement(secession_treaty_primacy_su_t2010, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement(secession_treaty_primacy_su_t2015, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(secession_treaty_primacy_su_t2020, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement(secession_treaty_primacy_su_t2025, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2025, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__treaty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__grievance_threshold_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'secession legitimacy' decomposes, per the ε-invariance principle, into four structurally distinct boundary claims — one per sibling reading — each a separate story with its own ε, beneficiary/victim structure, and classification. This file instantiates the treaty_primacy_reading. The upstream/downstream structure: the constitutional_impossibility_reading is the state framework's own articulation and is cited against this reading; this reading exerts repudiation pressure on it while structurally depending on the negotiation channel it contests. Edges here link the constraint family for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
