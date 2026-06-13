% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__withdrawal_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__withdrawal_sovereignty_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: npt_treaty_1970__withdrawal_sovereignty_reading
 *   human_readable: NPT Article X Withdrawal Right (Sovereignty Reading)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   Under the withdrawal sovereignty reading, Article X of the Nuclear
 *   Nonproliferation Treaty is interpreted as a legitimate exercise of state
 *   sovereignty — any signatory may withdraw if it judges the security
 *   environment has changed in ways that threaten its vital interests. This
 *   reading stands in contrast to the oligopoly enforcement reading (which
 *   treats Articles I-II as primary binding obligations and Article VI as
 *   contingent) and the reciprocal disarmament reading (which treats Article
 *   VI as a binding legal obligation with reciprocal force). The sovereignty
 *   reading does NOT claim withdrawal is exercised casually; rather, it
 *   claims withdrawal is a justified and available response when security
 *   guarantees fail or when NWS demonstrate they will not disarm. This
 *   reading emerges from threshold states' legal arguments, from non-aligned
 *   movement critiques of the treaty's asymmetry, and from NWS's own framing
 *   that the treaty is consensual rather than coercive. The constraint
 *   studied here is the institutional effect of that reading: once withdrawal
 *   is accepted as sovereign choice, treaty obligations become contingent
 *   rather than binding, regime stability is undermined as a beneficiary of
 *   the regime itself, and threshold states gain option value that alters the
 *   renegotiation equilibrium.
 *
 * KEY AGENTS:
 *   - Threshold states (Japan, South Korea, Iran, Saudi Arabia, Turkey): structurally shift from locked-in non-NWS to contingent signatories with withdrawal option; exit_options move from 'trapped' to 'arbitrage'.
 *   - Nuclear weapon states (US, Russia, UK, France, China): benefit from threshold compliance while retaining symmetric withdrawal right; frame the right as proof the treaty is legitimate, not coercive.
 *   - Non-nuclear signatories that have given up weapons: pay opportunity costs of foresworn deterrence; bear the regime stability cost when the withdrawal right makes their commitment appear revocable.
 *   - Regime stability norm (pacta sunt servanda): non-agent, but enters victim set under this reading because credible exit threats undermine the norm's binding force.
 *   - Security guarantee providers: use the withdrawal right as a renegotiation lever; gain bargaining power from threshold states' threat to exit.
 *   - IAEA inspectorate: observes but does not control; their authority is contingent on treaty's continuance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.62).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.41).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal Right (Sovereignty Reading)").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__withdrawal_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, '1f5de49a-409f-4930-9884-d90fa356ce25').
narrative_ontology:cs_kernel_codification('1f5de49a-409f-4930-9884-d90fa356ce25', fixed_text).
narrative_ontology:cs_authority_grounding('1f5de49a-409f-4930-9884-d90fa356ce25', extraction).
narrative_ontology:cs_interpretation_layer_present('1f5de49a-409f-4930-9884-d90fa356ce25').
narrative_ontology:cs_reading_relation('1f5de49a-409f-4930-9884-d90fa356ce25', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f5de49a-409f-4930-9884-d90fa356ce25', npt_treaty_1970__reciprocal_disarmament_reading, influences).
narrative_ontology:cs_axiom('1f5de49a-409f-4930-9884-d90fa356ce25', foundational, withdrawal_as_sovereign_right).
narrative_ontology:cs_axiom_status(withdrawal_as_sovereign_right, holdable).
narrative_ontology:cs_axiom_grounding('1f5de49a-409f-4930-9884-d90fa356ce25', withdrawal_as_sovereign_right, deontological).
narrative_ontology:cs_axiom('1f5de49a-409f-4930-9884-d90fa356ce25', secondary, obligations_contingent_on_security_environment).
narrative_ontology:cs_axiom_status(obligations_contingent_on_security_environment, holdable).
narrative_ontology:cs_axiom_grounding('1f5de49a-409f-4930-9884-d90fa356ce25', obligations_contingent_on_security_environment, instrumental).
narrative_ontology:cs_reference_frame('1f5de49a-409f-4930-9884-d90fa356ce25', state_sovereignty_supremacy).
narrative_ontology:cs_drift_state('1f5de49a-409f-4930-9884-d90fa356ce25', contemporary_proliferation_environment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1f5de49a-409f-4930-9884-d90fa356ce25', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states_with_exit_option).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_signatories_dependent_on_nonnuclear_security_guarantees).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_1970__withdrawal_sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end, rising from 0.48 at start) because the withdrawal right creates asymmetric option value: threshold states gain renegotiation leverage; non-NWS lose the assumption that the treaty is permanent; NWS gain the appearance of consensuality while keeping weapons. Suppression is moderate (0.41) because the withdrawal right is openly available in the text and exercisable without permission — there is no hidden enforcement mechanism. However, suppression is nonzero because the IAEA inspections regime and export control cartels (Zangger Committee, Nuclear Suppliers Group) create real costs to withdrawal (isolation, sanctions, loss of supply). Theater is low-moderate (0.22) because the withdrawal right is invoked rarely (North Korea 2003, Iran's periodic threats are the primary cases) and mostly as a negotiation tool rather than actual exit; the coordination function (preventing proliferation cascade) is real but the withdrawal threat's credible existence degrades regime stability by making the prohibition revocable. The measurement series shows extractiveness rising through the 1990s-2010s (as threshold states' technical capacity and security concerns both grew) and stabilizing post-2010. Theater rises as withdrawal threats become less emergency-rare and more strategic lever. Suppression stays stable because the enforcement architecture (inspections, export controls) does not intensify materially — suppression measures what has to be actively enforced to hold threshold states in; it does not change with the theoretical availability of exit.
 *
 * PERSPECTIVAL GAP:
 *   Threshold states and non-nuclear signatories compute the constraint type very differently. From the threshold state perspective (powerful, arbitrage exit), the withdrawal right is a coordination safety valve — a rope with escape clause. From the non-nuclear signatory perspective (organized, constrained exit), the same withdrawal right is a snare cover story: they gave up deterrence forever while the NWS retained the option to exit, which is asymmetrically extractive. The NWS see themselves as cooperative (offering the right as evidence they are not imposing a coercive regime); threshold states see themselves as holding a credible renegotiation threat; non-NWS see themselves as locked in while NWS are free. The engine computes these per-seat from the structural data (power, exit, beneficiary/victim declarations); the computed divergence is exactly the measurement this story takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold states are the structural beneficiaries of the withdrawal right (d near beneficiary end, ~0.2-0.3) because they gain option value and renegotiation leverage. NWS are at near-symmetric (d ~0.45) because they benefit from the legitimacy the right provides but also pay the cost of threshold states' increased credible exit threat. Non-nuclear signatories and the regime stability norm are the victims (d near target end, ~0.75-0.85) because the withdrawal right's existence and credibility undermine their ability to signal binding commitment and their expectation that the treaty provides permanent security. Directionality overrides are not necessary here; the structural derivation from beneficiary/victim + exit options produces the correct d values. The key is that regime_stability_norm enters victims[], which is unusual (it is a non-agent): this records the fact that one of the constraint's extractive consequences is the weakening of the norm that treaties are binding.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prevent proliferation cascade) is still live (threshold states are still tempted to acquire weapons), but the withdrawal right's treatment as legitimate sovereignty exercise has shifted the regime from binding constraint to contingent coordination with escape clause. This is not mandatrophy in the classical sense (atrophied function maintained theatrically), but rather a reading shift that reframes how the same institutional text (Article X) operates. The welfare analysis: NWS get legitimacy (consent narrative) + threshold states get option value (renegotiation leverage) + non-NWS get undermined security (regime stability cost). The constraint is not a Piton (theater_ratio stays low, function is not purely theatrical) — it is a Tangled Rope under the sovereignty reading, with genuine coordination (non-proliferation pledge) and genuine extraction (threshold states' option value shifts the renegotiation equilibrium away from non-NWS). The classification depends on which reading the observer holds; under the oligopoly enforcement reading (Articles I-II binding, Article VI contingent), the constraint would classify as a Snare; under the reciprocal disarmament reading, it would be a different Tangled Rope (asymmetry between NWS and non-NWS disarmament obligations, not between withdrawal rights). The contested reading produces contested classification — which is appropriate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contingency_vs_binding_interpretation,
    'Is the withdrawal right primarily a safety valve for legitimate sovereignty in emergencies (the withdrawal reading''s framing), or primarily evidence that the treaty is contingent rather than binding (non-NWS critique)?',
    'Discourse analysis of state practice in withdrawal threats (North Korea, Iran): if withdrawal is invoked only in genuine security crises, the safety-valve framing holds; if invoked routinely or threatened as negotiation leverage even absent security change, the contingency framing holds.',
    'If withdrawal is rare emergency measure, the constraint is a Rope with escape clause (coordination with restoration path). If withdrawal is normalized as negotiation leverage, the constraint is a Snare (contingent obligations that favor threshold states). Classification could shift to Snare-trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contingency_vs_binding_interpretation, empirical, 'Whether withdrawal is emergency-rare or routine-strategic.').

omega_variable(
    regime_stability_norm_victim_status,
    'Does the credible withdrawal threat genuinely undermine the norm that treaties are binding (pacta sunt servanda), or is the norm robust enough that occasional withdrawal invocations do not shift state behavior on other treaties?',
    'Comparative institutional analysis: track whether threshold states'' withdrawal threats correlate with increased defection on other international commitments, and whether non-NWS signatories cite the withdrawal right as evidence for contingency in other treaty negotiations.',
    'If withdrawal threat induces defection cascades on other treaties, regime_stability_norm is genuinely victimized and the constraint''s extractive effect is system-level. If the norm holds despite NPT withdrawal threats, the regime_stability_norm victim declaration is over-stated and extraction is more localized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_stability_norm_victim_status, empirical, 'System-level impact of withdrawal threats on treaty-binding norms.').

omega_variable(
    reading_identity_dependence,
    'Is this constraint''s classification dependent on which reading of the kernel the observer holds, or is there a reading-independent structural fact that determines classification?',
    'Test whether the oligopoly enforcement reading and the reciprocal disarmament reading, when instantiated as separate constraint stories, produce divergent classifications for the same Article X institutional text. If they do, classification is reading-dependent (conceptual).',
    'If reading-dependent, the constraint''s type is a hermeneutic fact, not a structural fact — the divergence is feature, not bug, and the Omega documents the reader-dependence. If reading-independent, the sovereignty reading''s classification is the correct one regardless of interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_dependence, conceptual, 'Whether constraint type is reading-invariant or depends on the kernel interpretation the observer holds.').

omega_variable(
    asymmetry_in_withdrawal_credibility,
    'Do NWS and threshold states face equal credibility costs in threatening withdrawal, or does the asymmetry in consequences make NWS withdrawal threats less credible and threshold-state threats more credible?',
    'Political-economy analysis of withdrawal consequences: NWS withdrawal would trigger sanctions, treaty collapse, and arms race; threshold state withdrawal triggers isolation but is survivable. The asymmetry in consequence makes the option value asymmetric — threshold states'' withdrawal threat is more credible. This may deserve a directionality override to reflect the structural difference.',
    'If the asymmetry is large enough, threshold_states'' directionality (d) should be lower (more beneficiary-like) and non_nuclear_signatories'' directionality should be higher (more target-like). This could justify a directionality override to separate structural beneficiaries (threshold states with credible exit) from structural targets (non-NWS with zero exit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetry_in_withdrawal_credibility, empirical, 'Asymmetric withdrawal credibility between NWS and threshold states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__withdrawal_sovereignty_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(npt__tr_t8, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement(npt__tr_t16, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(npt__tr_t24, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(npt__tr_t32, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 32, 0.22).
narrative_ontology:measurement(npt__tr_t40, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(npt__tr_t50, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 50, 0.22).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(npt__be_t8, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(npt__be_t16, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(npt__be_t24, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(npt__be_t32, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(npt__be_t40, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(npt__be_t50, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(npt__su_t8, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(npt__su_t16, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(npt__su_t24, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(npt__su_t32, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 32, 0.41).
narrative_ontology:measurement(npt__su_t40, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement(npt__su_t50, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 50, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__withdrawal_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__reciprocal_disarmament_reading).

% DUAL FORMULATION NOTE:
% The NPT (kernel: npt_treaty_1970) decomposes into three structurally distinct constraint stories, one per reading. Each reading instantiates a different ε and different classification. The sovereignty_reading (this file) treats Article X withdrawal as legitimate sovereignty exercise, making obligations contingent. The oligopoly_enforcement_reading treats Articles I-II as binding and Article VI as contingent/aspirational. The reciprocal_disarmament_reading treats Article VI as binding and symmetrical with I-II. These are not three perspectives on one constraint; they are three different constraints that happen to be called 'the NPT' colloquially. Each reading captures what would be lost and what would be gained if that interpretation were rejected. All three stories must exist in the corpus for the cross-reading divergence to be detectable by consumers. The network edges record the kernel membership and reading kinship; the specific structural relationships (forecloses, coexists_with, influences) are declared in cs_structure.reading_relations within each story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_1970__withdrawal_sovereignty_reading, powerful, 0.28).
constraint_indexing:directionality_override(npt_treaty_1970__withdrawal_sovereignty_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
