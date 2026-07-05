% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: The Nuclear Taboo: Normative Prohibition of Total War Independent of Material Capability
 *   domain: international_relations/security_studies/institutional_history
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   `total_war_possibility_space`: the claim that total war (specifically,
 *   nuclear first use) has become normatively unthinkable through a
 *   constructed taboo — a discursive and institutional achievement
 *   independent of the underlying material capability, which remains fully
 *   present. This is distinct from the sibling reading that total war is
 *   merely deterred by mutual vulnerability (material calculation doing all
 *   the work, taboo as post-hoc narrative) and from the sibling reading that
 *   nuclear weapons removed total war from the strategically thinkable
 *   altogether (a stronger claim about cognitive/strategic space contraction,
 *   not normative prohibition layered on an unchanged possibility space).
 *   Under this reading, war remains materially possible — arsenals exist,
 *   delivery systems exist, the physics is unchanged — but crossing the
 *   threshold is normatively foreclosed by an internalized prohibition that
 *   non-proliferation institutions, no-first-use pledges, and
 *   norm-entrepreneur advocacy actively construct and maintain. The ε for
 *   this reading is moderate: coordination value from crisis stability is
 *   real, but the taboo's enforcement machinery also does significant
 *   extractive work — legitimizing the arsenals of existing powers while
 *   imposing costs on threshold states and displacing violence downward onto
 *   conventional theaters.
 *
 * KEY AGENTS:
 *   - existing_nuclear_weapon_states: primary beneficiary — retain arsenals under taboo's protective legitimacy
 *   - norm_entrepreneur_ngos: agenda-setters who construct and sustain the discourse of unthinkability
 *   - non_proliferation_treaty_secretariat: institutional administrator of the taboo's verification apparatus
 *   - threshold_states_denied_arsenals: primary target — bear the taboo's exclusionary cost
 *   - populations_in_conventional_war_zones_below_the_taboo_threshold: bear displaced violence the taboo does not touch
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.42).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.55).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "The Nuclear Taboo: Normative Prohibition of Total War Independent of Material Capability").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/security_studies/institutional_history").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '87c8396f-4584-40e9-9e2b-b78ed0f23009').
narrative_ontology:cs_kernel_codification('87c8396f-4584-40e9-9e2b-b78ed0f23009', distributed).
narrative_ontology:cs_authority_grounding('87c8396f-4584-40e9-9e2b-b78ed0f23009', practice).
narrative_ontology:cs_interpretation_layer_present('87c8396f-4584-40e9-9e2b-b78ed0f23009').
narrative_ontology:cs_reading_relation('87c8396f-4584-40e9-9e2b-b78ed0f23009', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('87c8396f-4584-40e9-9e2b-b78ed0f23009', total_war_possibility_space__space_contraction_reading, influences).
narrative_ontology:cs_axiom('87c8396f-4584-40e9-9e2b-b78ed0f23009', foundational, prohibition_causally_independent_of_capability).
narrative_ontology:cs_axiom_status(prohibition_causally_independent_of_capability, holdable).
narrative_ontology:cs_axiom_grounding('87c8396f-4584-40e9-9e2b-b78ed0f23009', prohibition_causally_independent_of_capability, empirically_contingent).
narrative_ontology:cs_axiom('87c8396f-4584-40e9-9e2b-b78ed0f23009', secondary, norm_entrepreneurship_constructs_rather_than_reflects_restraint).
narrative_ontology:cs_axiom_status(norm_entrepreneurship_constructs_rather_than_reflects_restraint, holdable).
narrative_ontology:cs_axiom_grounding('87c8396f-4584-40e9-9e2b-b78ed0f23009', norm_entrepreneurship_constructs_rather_than_reflects_restraint, conventional).
narrative_ontology:cs_reference_frame('87c8396f-4584-40e9-9e2b-b78ed0f23009', post_hiroshima_normative_vacuum).
narrative_ontology:cs_drift_state('87c8396f-4584-40e9-9e2b-b78ed0f23009', post_cold_war_proliferation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('87c8396f-4584-40e9-9e2b-b78ed0f23009', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, existing_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_ngos).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_treaty_secretariat).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, threshold_states_denied_arsenals).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states_under_extended_deterrence).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, populations_in_conventional_war_zones_below_the_taboo_threshold).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states_under_extended_deterrence).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, constructivist_norm_theory).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, taboo_as_independent_causal_variable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess arsenals grandfathered under the Non-Proliferation Treaty and lead the diplomatic apparatus that maintains and polices the taboo — sponsoring no-first-use pledges, funding disarmament rhetoric, and enforcing non-proliferation against others while retaining their own stockpiles. The taboo's persistence locks in their relative position without requiring them to disarm.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, existing_nuclear_weapon_states, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, existing_nuclear_weapon_states, agenda_setter).

% Anti-nuclear activist networks, arms-control think tanks, and Nobel-adjacent campaign organizations who built and sustain the discourse that use is unthinkable rather than merely disadvantageous. Their institutional survival and funding depend on the taboo's continued salience as a live moral question; they can shift focus (mobile exit) if the issue loses public traction.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_ngos, agenda_setter,
    organized, generational, mobile, global).

% The IAEA and associated treaty bodies administer inspection regimes and safeguard agreements that operationalize the taboo into verification bureaucracy. Their institutional mandate and budget depend on the taboo remaining a live, defensible international norm; they cannot easily exit the regime they staff.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_treaty_secretariat, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_treaty_secretariat, agenda_setter).

% States with the technical capacity to build weapons but who face sanctions, diplomatic isolation, and military threat if they cross the threshold. They bear the security cost of the taboo — denied the deterrent that existing powers already hold — while the normative frame declares their pursuit illegitimate regardless of the material symmetry with existing arsenals.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, threshold_states_denied_arsenals, payer,
    moderate, generational, trapped, national).

% Allied states that rely on a nuclear patron's umbrella instead of their own arsenal. They benefit from the taboo's stabilizing effect on great-power conflict but pay through subordination to patron foreign policy and the residual risk that the patron's own use would be visited on their territory as a staging or retaliation target.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states_under_extended_deterrence, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states_under_extended_deterrence, beneficiary).

% Civilians in conflicts that stay conventional precisely because total war is normatively foreclosed at the nuclear tier — but the taboo does nothing to constrain the intensity or duration of conventional warfare beneath it. They absorb violence that the taboo redirects downward rather than eliminates.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, populations_in_conventional_war_zones_below_the_taboo_threshold, payer,
    powerless, immediate, trapped, regional).

% Academic researchers who study whether the seven-decade non-use record reflects genuine normative internalization or merely successful deterrence dressed in moral language. Their analysis feeds directly into the sibling readings of this same kernel.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, strategic_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__nuclear_taboo_reading, existing_nuclear_weapon_states).
narrative_ontology:fixing_cost_class(total_war_possibility_space__nuclear_taboo_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, internalized prohibition that lets states forgo nuclear use even in extremis without appearing weak — a reputational shield that substitutes for continuous material deterrence calculation and reduces first-use temptation during crises.
% TRANSFER_FUNCTION: Moves security guarantees and international legitimacy from threshold and non-nuclear states toward existing nuclear powers, who retain arsenals while imposing the taboo's costs (sanctions, isolation, subordination) on those denied entry to the club.
% ABSENT_VOICES: Threshold states and populations in conventional conflict zones would object that the taboo protects the position of existing arsenal-holders under moral cover, but they are not seated in the bodies (UN Security Council, NPT review conferences) that adjudicate legitimate possession.
% DISAPPEARANCE_RATIONALE: Norm entrepreneurs and the secretariat would say the world rearranges catastrophically — use becomes thinkable again, arms races accelerate. Deterrence-equilibrium theorists would say little changes because material MAD calculations, not the taboo, were doing the restraining work all along. This exact disagreement is why the kernel decomposes into separate readings.
% FOUNDING_PROBLEM: After Hiroshima and Nagasaki, the international system needed some mechanism to prevent nuclear use from becoming a normal instrument of statecraft as artillery had been for conventional war — the founding problem was establishing that crossing the nuclear threshold was categorically different from any prior escalation.
% FOUNDING_PROBLEM_CORROBORATION: Constructivist IR scholars (Tannenwald and successors) attest from outside the beneficiary set that the taboo functions as an independent normative variable distinct from material deterrence. Realist scholars and threshold-state diplomats counter that the 'taboo' is retrospective narrative laid over material deterrence outcomes that would have held without it — this is the live scholarly contest the kernel decomposition exists to represent, not a settled genealogy.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, contested).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).
:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) sits at the moderate tangled-rope range: the taboo genuinely reduces first-use risk (real coordination value) but also functions to legitimize an asymmetric possession regime — existing powers keep arsenals, others are barred, with the moral frame doing legitimation work material symmetry alone would not support. Suppression (0.55) reflects the sanctions, inspections regimes, and diplomatic isolation used against threshold states to enforce the norm; this has intensified over the measured interval as the non-proliferation regime institutionalized (rising suppression_requirement from 0.30 to 0.55). Theater ratio rises moderately (0.15 to 0.38) as disarmament rhetoric from existing powers increasingly substitutes for actual reduction — NPT Article VI's stockpile-reduction commitment is honored more in ceremony than arsenal count. Accessibility collapse (0.60) is substantial but not total: the alternative (return to fully material deterrence calculation without taboo) remains conceivable and is precisely what the sibling deterrence-equilibrium reading asserts was happening all along.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (existing powers, NPT secretariat), this looks like genuine, hard-won coordination — a moral achievement that stabilizes an otherwise catastrophic possibility space. From the threshold-state payer seat, the same structure looks like extraction: a normative frame invented after the fact by those who already possess the capability, used to bar late entrants from the same option. The engine computes both from the same structural data; the divergence is the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing nuclear weapon states sit near the beneficiary end: the taboo lets them retain deterrent value and moral high ground simultaneously, without requiring disarmament. Norm-entrepreneur NGOs benefit institutionally from the taboo's salience but have mobile exit if the issue loses relevance. Threshold states sit near the full-target end: trapped by the combination of sanctions regimes and the reputational cost of appearing to defy an internalized global norm rather than merely a material power imbalance. Populations in conventional conflict zones are powerless and trapped — the taboo's redirection of violence downward is not something they can arbitrage or exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing normalization of nuclear use) is genuinely contested as live or dead: proliferation risk from new state and potentially non-state actors keeps the problem live in some corridors, while the non-use record since 1945 is cited by beneficiaries as evidence the mandate has succeeded and could be relaxed. The taboo has not been declared mandatrophy-resolved because the enforcement apparatus (sanctions, inspections, diplomatic isolation of threshold states) shows no sign of standing down — if anything, suppression_requirement is still rising, which argues against treating the founding problem as dead even though non-use duration is often cited as evidence it is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_versus_deterrence_causal_priority,
    'Is non-use since 1945 caused by an internalized normative prohibition independent of material capability, or is the ''taboo'' a retrospective narrative laid over outcomes that mutual assured destruction calculations alone would have produced?',
    'Comparative case analysis of near-use crises (Cuban Missile Crisis, Able Archer, India-Pakistan Kargil) examining whether decision-makers'' own contemporaneous reasoning invoked normative unthinkability or material deterrence calculus — declassified deliberation records are the primary evidence class.',
    'If deterrence calculation was doing the restraining work throughout, this reading collapses into the deterrence_equilibrium_reading and the taboo becomes epiphenomenal narrative rather than an independent causal constraint — the tangled_rope classification would weaken toward rope (less extraction, since there would be no independently constructed exclusionary mechanism, just material asymmetry described in moral language).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_versus_deterrence_causal_priority, conceptual, 'Core kernel-contest ambiguity: whether the taboo is causally independent of material deterrence or a narrative gloss on it.').

omega_variable(
    taboo_versus_thinkability_contraction,
    'Does the sibling space_contraction_reading''s claim — that nuclear weapons removed total war from the strategically THINKABLE, not merely the normatively preferable — describe a different mechanism than this reading''s normative-prohibition claim, or are they the same phenomenon at different levels of description?',
    'Elicitation studies or historical-cognitive analysis of whether strategic planners in nuclear states actually model total war as a live option weighed against a taboo, versus not modeling it as an option at all (a category absent from the decision space rather than a rejected option within it).',
    'If total war has genuinely exited the thinkable strategic space rather than remaining a rejected-but-considered option, this reading''s premise (a live normative prohibition operating on a materially unchanged possibility space) would be structurally wrong for the relevant actors, and the space_contraction_reading would be the more accurate constraint for those seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taboo_versus_thinkability_contraction, conceptual, 'Whether normative prohibition and cognitive/strategic unthinkability name the same underlying mechanism or two distinct kernel readings.').

omega_variable(
    beneficiary_capture_of_norm_entrepreneurship,
    'To what extent have existing nuclear weapon states captured or co-opted the norm-entrepreneur apparatus (funding disarmament NGOs, sponsoring taboo-reinforcing scholarship) such that the ''independent'' normative construction is itself partly a product of beneficiary agenda-setting?',
    'Funding-flow analysis of major arms-control NGOs and academic centers tracing state and state-adjacent foundation money; comparison of policy positions taken by independently-funded versus state-adjacent-funded norm entrepreneurs on disarmament timelines and threshold-state sanctions.',
    'Substantial capture would push this reading further toward snare (extraction dressed as coordination, with even the norm-construction machinery serving beneficiary interests) rather than the tangled_rope''s genuine-coordination-plus-extraction hybrid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_norm_entrepreneurship, empirical, 'Whether the taboo''s construction was independently driven or substantially shaped by the states who benefit from it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(tota_tr_t0, observed).
narrative_ontology:measurement(tota_tr_t16, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement_basis(tota_tr_t16, observed).
narrative_ontology:measurement(tota_tr_t32, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement_basis(tota_tr_t32, observed).
narrative_ontology:measurement(tota_tr_t48, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 48, 0.3).
narrative_ontology:measurement_basis(tota_tr_t48, observed).
narrative_ontology:measurement(tota_tr_t64, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 64, 0.34).
narrative_ontology:measurement_basis(tota_tr_t64, observed).
narrative_ontology:measurement(tota_tr_t80, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement_basis(tota_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(tota_be_t0, observed).
narrative_ontology:measurement(tota_be_t16, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 16, 0.28).
narrative_ontology:measurement_basis(tota_be_t16, observed).
narrative_ontology:measurement(tota_be_t32, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 32, 0.33).
narrative_ontology:measurement_basis(tota_be_t32, observed).
narrative_ontology:measurement(tota_be_t48, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 48, 0.37).
narrative_ontology:measurement_basis(tota_be_t48, observed).
narrative_ontology:measurement(tota_be_t64, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 64, 0.4).
narrative_ontology:measurement_basis(tota_be_t64, observed).
narrative_ontology:measurement(tota_be_t80, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 80, 0.42).
narrative_ontology:measurement_basis(tota_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(tota_su_t0, observed).
narrative_ontology:measurement(tota_su_t16, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement_basis(tota_su_t16, observed).
narrative_ontology:measurement(tota_su_t32, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 32, 0.44).
narrative_ontology:measurement_basis(tota_su_t32, observed).
narrative_ontology:measurement(tota_su_t48, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 48, 0.48).
narrative_ontology:measurement_basis(tota_su_t48, observed).
narrative_ontology:measurement(tota_su_t64, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 64, 0.52).
narrative_ontology:measurement_basis(tota_su_t64, observed).
narrative_ontology:measurement(tota_su_t80, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement_basis(tota_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__nuclear_taboo_reading, 0.12).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, space_contraction_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_treaty_regime).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the kernel total_war_possibility_space. deterrence_equilibrium_reading holds that material mutual vulnerability, not norm construction, restrains total war; space_contraction_reading holds that nuclear weapons contracted the strategically thinkable space itself rather than leaving it materially intact but normatively foreclosed. This reading (nuclear_taboo_reading) claims the intermediate position: capability is unchanged, thinkability is unchanged, but use has become normatively prohibited through constructed discourse and institutions. Each sibling carries its own ε, beneficiary/victim structure, and classification; they are linked here rather than merged into one story per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
