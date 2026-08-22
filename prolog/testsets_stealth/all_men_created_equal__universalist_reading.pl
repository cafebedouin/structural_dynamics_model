% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__universalist_reading, []).

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
 *   constraint_id: all_men_created_equal__universalist_reading
 *   human_readable: Universalist Reading of the Equality Clause: Iterative Expansion Mandate
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   A single sentence written in 1776 — that all men are created equal — is
 *   treated by this reading as a binding universal maxim whose reach is fixed
 *   by its content rather than by the intentions or conduct of its authors.
 *   On this reading the clause obliges each generation to labor for its
 *   fuller realization: extending equal status to those the founding order
 *   excluded, dismantling the legal architectures of prior hierarchies as
 *   claims arrive, and absorbing the costs displacement imposes. The
 *   arrangement coordinates the polity around one expandable standard of
 *   membership while transferring status, power, and enforcement burdens in
 *   each round; its costs fall on holders of displaced privilege, on states
 *   and localities precluded from ordering their own hierarchies, and on
 *   consciences that conflict with newly mandated equal treatment. Per the
 *   kernel rules, this file instantiates ONLY the universalist reading as a
 *   clean, epsilon-invariant constraint — the originalist and
 *   textualist-paradox readings are separate files, and the committer
 *   structure is routed to omega variables. KEY AGENTS (by structural
 *   relationship): - marginalized_inclusion_claimants: Primary beneficiary
 *   (powerless/trapped) — invoke the clause to claim inclusion; cannot exit
 *   the jurisdiction that excludes them - social_movement_organizations:
 *   Beneficiary and agenda-driver (organized/constrained) — convert the maxim
 *   into litigation and legislation each round - federal_judiciary:
 *   Agenda-setter (institutional/identity_locked) — administers the reading
 *   and accumulates the canon; cannot exit its own precedent structure -
 *   displaced_privilege_holders: Primary target (powerful/constrained) — lose
 *   property, institutional power, or practice autonomy as each round lands -
 *   state_autonomy_defenders: Secondary target (organized/constrained) — bear
 *   federal preclusion of locally ordered hierarchies -
 *   conscience_objectors_to_expansion: Marginal target (moderate/constrained)
 *   — bear compliance costs where conviction conflicts with new mandates -
 *   future_generations_of_claimants: Designed constituency
 *   (powerless/trapped, civilizational horizon) — inherit an expanded
 *   baseline and the mandate to extend it -
 *   comparative_constitutional_scholars: Analytical observer
 *   (analytical/analytical) — trace the reading's travel and the
 *   proclamation-practice gap
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, 0.52).
domain_priors:suppression_score(all_men_created_equal__universalist_reading, 0.58).
domain_priors:theater_ratio(all_men_created_equal__universalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "Universalist Reading of the Equality Clause: Iterative Expansion Mandate").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, '3964e9a8-1199-4b50-9c98-901c503c088d').
narrative_ontology:cs_kernel_codification('3964e9a8-1199-4b50-9c98-901c503c088d', fixed_text).
narrative_ontology:cs_authority_grounding('3964e9a8-1199-4b50-9c98-901c503c088d', lineage).
narrative_ontology:cs_interpretation_layer_present('3964e9a8-1199-4b50-9c98-901c503c088d').
narrative_ontology:cs_reading_relation('3964e9a8-1199-4b50-9c98-901c503c088d', all_men_created_equal__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('3964e9a8-1199-4b50-9c98-901c503c088d', all_men_created_equal__textualist_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('3964e9a8-1199-4b50-9c98-901c503c088d', foundational, universal_content_governs_scope_over_founder_intent).
narrative_ontology:cs_axiom_status(universal_content_governs_scope_over_founder_intent, holdable).
narrative_ontology:cs_axiom_grounding('3964e9a8-1199-4b50-9c98-901c503c088d', universal_content_governs_scope_over_founder_intent, deontological).
narrative_ontology:cs_axiom('3964e9a8-1199-4b50-9c98-901c503c088d', foundational, iterative_expansion_is_intergenerational_obligation).
narrative_ontology:cs_axiom_status(iterative_expansion_is_intergenerational_obligation, holdable).
narrative_ontology:cs_axiom_grounding('3964e9a8-1199-4b50-9c98-901c503c088d', iterative_expansion_is_intergenerational_obligation, deontological).
narrative_ontology:cs_reference_frame('3964e9a8-1199-4b50-9c98-901c503c088d', declaration_as_binding_universal_maxim).
narrative_ontology:cs_drift_state('3964e9a8-1199-4b50-9c98-901c503c088d', contemporary_interpretive_contest, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('3964e9a8-1199-4b50-9c98-901c503c088d', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, marginalized_inclusion_claimants).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, future_generations_of_claimants).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, displaced_privilege_holders).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, state_autonomy_defenders).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, conscience_objectors_to_expansion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, social_movement_organizations).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, living_constitution_doctrine).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, judicial_review_expansive_construction).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, self_evident_truth_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons excluded from full legal status under prior arrangements — the enslaved, disenfranchised women, segregated minorities, unrecognized families — who invoke the equality clause to claim inclusion. They cannot exit the jurisdiction that excludes them, and their claims have force only through the principle's authority; until a round lands, they bear the exclusion the reading condemns.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, marginalized_inclusion_claimants, beneficiary,
    powerless, generational, trapped, national).

% Abolition societies, suffrage associations, and civil rights organizations that convert the maxim into litigation, legislation, and mobilization each round. They set the political agenda for expansion and collect standing, vindication, and institutional survival when claims succeed; dissolving would forfeit their purpose, so exit is effectively unavailable.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, social_movement_organizations, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, social_movement_organizations, agenda_setter).

% Courts administer the reading: deciding which unequal treatments the principle reaches, striking contrary arrangements, and accumulating the equal-protection canon. The institution's authority is fused with its interpretive role — it has become the thing that construes the maxim — and it cannot exit its own precedent structure without dissolving the basis of its power.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Holders of legally sanctioned superiority under prior rounds — slaveholders whose bondages were voided, segregationist institutions, employers and landlords brought under anti-discrimination mandate — who lose property, institutional power, or practice autonomy as each expansion lands. Their exits are relocation, resistance, or absorption of the loss; all are costly, and their legal status cannot be carried elsewhere.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, displaced_privilege_holders, payer,
    powerful, biographical, constrained, national).

% States and localities asserting authority to order their own school systems, voting rules, and family-law regimes, who bear federal preclusion when the reading reaches their arrangements. Exit means constitutional confrontation they historically lose or compliance they resent; their scope is regional but the precluding authority is national.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, state_autonomy_defenders, payer,
    organized, generational, constrained, regional).

% Individuals and institutions whose religious or philosophical convictions conflict with newly mandated equal treatment — compelled service, recognition, or association cases — who bear compliance costs or penalties as the frontier advances. Their exits are narrowing their own practice or litigating for exemptions; both are partial and uncertain.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, conscience_objectors_to_expansion, payer,
    moderate, biographical, constrained, national).

% Persons not yet born or not yet aggrieved whose future claims the reading pre-authorizes. They hold no present power and cannot speak, but they are the reading's designed constituency: each generation inherits an expanded baseline of equal status together with the mandate to extend it further.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, future_generations_of_claimants, beneficiary,
    powerless, civilizational, trapped, national).

% Researchers tracing how the reading travels — foreign constitutions borrowing the expansion pattern, longitudinal studies of the gap between proclamation and practice. They collect no rents from the arrangement and bear none of its costs; their seat is analytical.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__universalist_reading, marginalized_inclusion_claimants).
narrative_ontology:fixing_cost_class(all_men_created_equal__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single shared standard of political membership — the equal status of all persons — that successive generations use to adjudicate inclusion claims without renegotiating first principles. It converts episodic exclusion disputes into iterations of one authorizing text, keeping the founding document authoritative while its application expands.
% TRANSFER_FUNCTION: Moves recognized legal status and power from holders of prior hierarchies to newly included groups in each round; moves enforcement and compliance costs onto institutions, states, and consciences that resist; and concentrates interpretive authority in the institutions that administer the maxim.
% ABSENT_VOICES: Future claimants whose categories of exclusion have not yet crystallized cannot speak — the reading claims to speak for them by design, which is exactly what its critics deny it may do. Displaced parties typically enter the process only as defendants after a round lands. The founders themselves are an absent voice by the reading's own choice: their understanding is discounted as evidence, which the originalist sibling treats as disqualifying and this reading treats as the point.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand every pending and future inclusion claim without its authorizing instrument, unravel the equal-protection canon built on the reading, and force each still-standing hierarchy to be renegotiated on non-foundational ground. The constitutional order would rearrange around whatever replacement mechanism emerged, at the cost of a legitimacy crisis in the interim.
% FOUNDING_PROBLEM: A universal principle was proclaimed by a founding generation that practiced chattel slavery and restricted franchise. This arrangement was built to resolve that contradiction by making the proclaimed principle govern regardless of the proclaimers' intent or conduct.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the eventual beneficiary set: Lincoln's 1857 Springfield speech — delivered before inclusion was won — framed the clause as a standard maxim to be constantly labored for; Alexander Stephens' 1861 Cornerstone Speech attests from the opposing camp that the universalist reading was a live and feared force; Tocqueville, observing from outside the American claimant set, recorded the principle's apparently uncontrollable expansive logic. Contemporary corroboration is weaker and stated plainly: much attestation now comes from claimant communities themselves, and originalist scholars corroborate only the text's universality while disputing this reading — partial corroboration, honestly flagged.
narrative_ontology:disappearance_verdict(all_men_created_equal__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__universalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(all_men_created_equal__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__universalist_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.52 at interval end): each expansion round genuinely transfers status and power away from prior hierarchies, and the coordination costs of continuous expansion — litigation, compliance, cultural conflict — are real; but the costs are overt, justified by the reading's own lights, and paired with a functioning coordination output, which is why the claimed type is tangled_rope rather than snare. Suppression (0.58) reflects an enforcement apparatus that is real but partly consensual: federal preclusion and judicial orders coerce the resistant minority while most institutions comply voluntarily. Suppression is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope in the engine's computation. Theater is low (0.22): ceremonial invocations of the clause exist, but enforcement and doctrinal activity are predominantly functional. Accessibility collapse is moderate-low (0.40): rival interpretive communities remain live exits, so the reading does not close the space of alternatives the way a natural law would. Resistance is high (0.65): every round has met organized counter-mobilization. The three measurement series share one six-point grid (t=0..250, all metrics authored at every point); the base_extractiveness and suppression_requirement trajectories are deliberately non-monotonic — Reconstruction-era peaks, post-Redemption collapse, mid-twentieth-century ratchet — and the oscillation is investigated under the backlash_cycle_endogeneity omega rather than assumed benign.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical text. From the federal_judiciary seat the reading is a vocation: the institution's authority is constituted by administering the maxim, so it experiences the arrangement as legitimate self-executing principle. From the displaced_privilege_holders seat the same structure is confiscation: property and status taken by interpretive development they never consented to, with exit limited to relocation or resistance. From the marginalized_inclusion_claimants seat it is the only available instrument: a trapped population with no other authorizing text. Same kernel, same interval, different experienced types — the divergence is computed by the engine from power, exit, and directional position, not asserted by the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: marginalized_inclusion_claimants and future_generations_of_claimants sit near the beneficiary end (low d) — the arrangement subsidizes their claims; social_movement_organizations collect standing and vindication while also setting the agenda, a genuinely dual position carried by secondary_role. Targets: displaced_privilege_holders carry the direct transfer (high d, amplified by constrained exit — their legal status cannot be carried elsewhere); state_autonomy_defenders bear preclusion of their preferred arrangements; conscience_objectors_to_expansion bear marginal-round compliance costs. The federal_judiciary administers rather than collects: it gains authority incidentally, keeping it nearer symmetric than beneficiary. Receipt surface: the transferred status demonstrably lands on marginalized_inclusion_claimants, so gain_flow names that seat rather than 'diffuse' — the polity-wide legitimacy dividend is real but diffuse, and the direct transfer is not.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a professed universal principle enacted amid exclusion — remains live: each round of expansion has surfaced new exclusions, and claims not yet recognized continue to arrive, so the mandate has not outlived its function and no mandatrophy is declared. The classification disciplines both mislabelings: calling the reading pure coordination ignores the real, recurring costs imposed on displaced parties each round; calling it pure extraction ignores that the costs are the price of a coordination function the payers' own preferred arrangements made necessary — the exclusionary orders being dismantled are what generate each round's claims. The tangled_rope verdict holds both facts in one structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the kernel all_men_created_equal — the universalist_reading. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Compare the compiled sibling stories: the originalist_reading authors epsilon over the intent-bounded arrangement (its victim set is those the 18th-century taxonomy excluded, with the taxonomy itself functioning as the scope-fixing device); the textualist_paradox_reading authors epsilon over the founding act''s coherence (the kernel itself is the contested object). The disagreement is located at the scope-determinant: whether founder intent fixes the principle''s reach, whether the text self-refutes, or whether universal content governs regardless of intent.',
    'If the originalist reading prevails institutionally, this constraint''s victim set contracts sharply (bounded scope displaces fewer holders per round) and its coordination function narrows to period-specific membership; if the textualist paradox prevails, the kernel loses authority altogether and all three readings dissolve into a legitimacy crisis rather than competing classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer routing: one reading of a three-reading kernel; disagreement located at the scope-determinant.').

omega_variable(
    natural_law_vs_political_instrument,
    'Is the universalist reading the discovery of a self-evident moral fact that constrains all polities, or a constructed political instrument whose necessity is maintained by the institutions that wield it?',
    'Cross-cultural comparison of expansion dynamics in polities with and without an equality-clause kernel; if comparable expansion occurs without the clause, the reading is instrument rather than discovery.',
    'Discovery would push the classification toward mountain-like immunity (negligible effective extraction, alternatives genuinely collapsed); instrument would confirm tangled_rope with enforceable, revisable structure and identifiable maintainers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_political_instrument, conceptual, 'Whether the reading is a discovered moral limit or a maintained political construction.').

omega_variable(
    expansion_frontier_terminus,
    'Does iterative expansion have a terminus — a state in which all equal-status claims are satisfied — or is the frontier unbounded, with each expansion generating new claimants and a correspondingly expanding set of displaced parties?',
    'Track the claim-generation rate across rounds: if new claim categories emerge faster than old ones resolve, the frontier is unbounded and the reading''s costs never asymptote.',
    'A bounded frontier implies eventual convergence toward pure coordination with residual costs; an unbounded frontier implies a permanently hybrid structure with recurring extraction rounds and recurring backlash exposure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(expansion_frontier_terminus, empirical, 'Whether the expansion mandate converges or runs without terminus.').

omega_variable(
    backlash_cycle_endogeneity,
    'Are the resistance cycles visible in the measurement series (Reconstruction-era peak and collapse, mid-century ratchet and reaction) endogenous to the mechanism — intermittent reinforcement that sustains the reading by alternating threat and concession — or exogenous shocks from war, economics, and demography?',
    'Comparative timing analysis correlating expansion rounds and backlash intensity with the reading''s own enforcement actions versus external events, across multiple polities carrying the same kernel.',
    'Endogenous cycling would mean the oscillation is itself part of the arrangement''s persistence machinery, raising the honest effective-suppression picture above the scalar; exogenous cycling would leave the scalar suppression measure as the accurate account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(backlash_cycle_endogeneity, empirical, 'Whether the observed backlash oscillation is generated by the mechanism or impressed on it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(amce_universalist_tr_t0, all_men_created_equal__universalist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(amce_universalist_tr_t0, observed).
narrative_ontology:measurement(amce_universalist_tr_t50, all_men_created_equal__universalist_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement_basis(amce_universalist_tr_t50, observed).
narrative_ontology:measurement(amce_universalist_tr_t100, all_men_created_equal__universalist_reading, theater_ratio, 100, 0.2).
narrative_ontology:measurement_basis(amce_universalist_tr_t100, observed).
narrative_ontology:measurement(amce_universalist_tr_t150, all_men_created_equal__universalist_reading, theater_ratio, 150, 0.45).
narrative_ontology:measurement_basis(amce_universalist_tr_t150, observed).
narrative_ontology:measurement(amce_universalist_tr_t200, all_men_created_equal__universalist_reading, theater_ratio, 200, 0.18).
narrative_ontology:measurement_basis(amce_universalist_tr_t200, observed).
narrative_ontology:measurement(amce_universalist_tr_t250, all_men_created_equal__universalist_reading, theater_ratio, 250, 0.22).
narrative_ontology:measurement_basis(amce_universalist_tr_t250, observed).

% Extraction over time
narrative_ontology:measurement(amce_universalist_be_t0, all_men_created_equal__universalist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(amce_universalist_be_t0, observed).
narrative_ontology:measurement(amce_universalist_be_t50, all_men_created_equal__universalist_reading, base_extractiveness, 50, 0.3).
narrative_ontology:measurement_basis(amce_universalist_be_t50, observed).
narrative_ontology:measurement(amce_universalist_be_t100, all_men_created_equal__universalist_reading, base_extractiveness, 100, 0.48).
narrative_ontology:measurement_basis(amce_universalist_be_t100, observed).
narrative_ontology:measurement(amce_universalist_be_t150, all_men_created_equal__universalist_reading, base_extractiveness, 150, 0.42).
narrative_ontology:measurement_basis(amce_universalist_be_t150, observed).
narrative_ontology:measurement(amce_universalist_be_t200, all_men_created_equal__universalist_reading, base_extractiveness, 200, 0.62).
narrative_ontology:measurement_basis(amce_universalist_be_t200, observed).
narrative_ontology:measurement(amce_universalist_be_t250, all_men_created_equal__universalist_reading, base_extractiveness, 250, 0.52).
narrative_ontology:measurement_basis(amce_universalist_be_t250, observed).

% Suppression requirement over time
narrative_ontology:measurement(amce_universalist_su_t0, all_men_created_equal__universalist_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(amce_universalist_su_t0, observed).
narrative_ontology:measurement(amce_universalist_su_t50, all_men_created_equal__universalist_reading, suppression_requirement, 50, 0.15).
narrative_ontology:measurement_basis(amce_universalist_su_t50, observed).
narrative_ontology:measurement(amce_universalist_su_t100, all_men_created_equal__universalist_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement_basis(amce_universalist_su_t100, observed).
narrative_ontology:measurement(amce_universalist_su_t150, all_men_created_equal__universalist_reading, suppression_requirement, 150, 0.25).
narrative_ontology:measurement_basis(amce_universalist_su_t150, observed).
narrative_ontology:measurement(amce_universalist_su_t200, all_men_created_equal__universalist_reading, suppression_requirement, 200, 0.7).
narrative_ontology:measurement_basis(amce_universalist_su_t200, observed).
narrative_ontology:measurement(amce_universalist_su_t250, all_men_created_equal__universalist_reading, suppression_requirement, 250, 0.58).
narrative_ontology:measurement_basis(amce_universalist_su_t250, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__universalist_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'all men are created equal' decomposes into three structurally distinct constraints per the epsilon-invariance principle — the originalist reading (epsilon authored over the intent-bounded arrangement; the 18th-century taxonomy functions as the scope-fixing device), the textualist paradox reading (epsilon authored over the founding act's coherence; the kernel itself is the contested object), and this universalist reading (epsilon 0.52 authored over the expansion regime the reading institutes). Each file carries its own beneficiaries, victims, and claimed type; upstream/downstream pressure runs from whichever reading holds institutional power to the others' operating environment. This file links both siblings via affects_constraints; an orphan member would break the family invariant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
