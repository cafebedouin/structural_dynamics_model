% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Nuclear Taboo: Constructed Normative Prohibition on Total War
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This story instantiates the nuclear-taboo reading of the
 *   total_war_possibility_space kernel: total war is materially possible but
 *   has become normatively foreclosed through a constructed, discursively
 *   maintained prohibition, causally independent of the material capability
 *   that deterrence-equilibrium accounts foreground. The taboo generates its
 *   own institutional apparatus — the NPT regime, no-first-use pledges, IAEA
 *   verification, humanitarian-impact disarmament campaigns — which is the
 *   reading's structural signature: norm-enforcement infrastructure that
 *   would not exist, or would look very different, if restraint were purely a
 *   function of mutual vulnerability. This reading is deliberately NOT
 *   reconciled with the deterrence_equilibrium_reading or the
 *   space_contraction_reading; those are separate constraints in the same
 *   kernel family, sharing the topic label 'why total war hasn't happened
 *   since 1945' but authoring different mechanisms, different ε, and
 *   different beneficiary/victim structures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.42).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.58).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear Taboo: Constructed Normative Prohibition on Total War").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '946e644b-a2dc-4877-a4ab-869ebe42da1d').
narrative_ontology:cs_kernel_codification('946e644b-a2dc-4877-a4ab-869ebe42da1d', distributed).
narrative_ontology:cs_authority_grounding('946e644b-a2dc-4877-a4ab-869ebe42da1d', distributed).
narrative_ontology:cs_reading_relation('946e644b-a2dc-4877-a4ab-869ebe42da1d', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('946e644b-a2dc-4877-a4ab-869ebe42da1d', total_war_possibility_space__space_contraction_reading, influences).
narrative_ontology:cs_axiom('946e644b-a2dc-4877-a4ab-869ebe42da1d', foundational, restraint_is_normatively_constructed_not_materially_determined).
narrative_ontology:cs_axiom_status(restraint_is_normatively_constructed_not_materially_determined, holdable).
narrative_ontology:cs_axiom_grounding('946e644b-a2dc-4877-a4ab-869ebe42da1d', restraint_is_normatively_constructed_not_materially_determined, empirically_contingent).
narrative_ontology:cs_axiom('946e644b-a2dc-4877-a4ab-869ebe42da1d', secondary, taboo_erodes_under_norm_entrepreneur_withdrawal).
narrative_ontology:cs_axiom_status(taboo_erodes_under_norm_entrepreneur_withdrawal, holdable).
narrative_ontology:cs_axiom_grounding('946e644b-a2dc-4877-a4ab-869ebe42da1d', taboo_erodes_under_norm_entrepreneur_withdrawal, empirically_contingent).
narrative_ontology:cs_reference_frame('946e644b-a2dc-4877-a4ab-869ebe42da1d', post_hiroshima_moral_prohibition_baseline).
narrative_ontology:cs_drift_state('946e644b-a2dc-4877-a4ab-869ebe42da1d', post_cold_war_proliferation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('946e644b-a2dc-4877-a4ab-869ebe42da1d', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, arms_control_epistemic_community).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_signatory_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, populations_under_extended_deterrence_umbrellas).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, constructivist_norm_theory).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, taboo_as_causally_independent_of_material_capability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold nuclear arsenals and administer the non-proliferation regime through the NPT's recognized-weapon-state carve-out. They author and re-author no-first-use pledges, treaty language, and export-control lists. They benefit from the taboo insofar as it locks in their own arsenals as the permanent exception while foreclosing new entrants; they can exit specific commitments (test moratoria, ABM treaties) with limited consequence given their structural position.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states, beneficiary).

% Middle powers and coalitions (e.g., non-nuclear NATO members, humanitarian disarmament coalitions) who invest diplomatic capital in advancing and policing the taboo — treaty conferences, humanitarian-impact campaigns, moral condemnation of use or threat of use. They gain reputational and institutional standing from being taboo custodians; their exit from advocacy would not itself change the material balance but would visibly weaken enforcement.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_states, beneficiary,
    powerful, generational, mobile, global).

% Academics, think-tank analysts, and international-organization staff whose careers, funding, and institutional relevance depend on the taboo being real and worth studying, verifying, and defending. They produce the discourse that constitutes the norm as a norm; without a contested taboo their analytical field contracts.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, arms_control_epistemic_community, beneficiary,
    organized, civilizational, constrained, global).

% States that renounced nuclear acquisition under the NPT bargain, accepting inspection regimes and forgoing the deterrent capability the weapon states retain. They bear the taboo's enforcement costs (verification intrusiveness, sanctions exposure for suspected proliferation) without ever holding the exception; their exit option (withdrawal, as with the DPRK precedent) triggers severe sanctions and isolation, making it trapped rather than merely constrained.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_signatory_states, payer,
    moderate, generational, trapped, national).

% Civilian populations in allied states whose security policy is set by a nuclear patron's taboo-compliant doctrine. They carry the residual catastrophic risk that the taboo manages but does not eliminate, and have no institutional voice in how no-first-use pledges or alert postures are negotiated; they cannot exit the geography of the umbrella.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, populations_under_extended_deterrence_umbrellas, payer,
    powerless, biographical, trapped, regional).

% States that judge the taboo as a device that freezes the current distribution of capability under moral cover. They are structurally excluded from the norm-setting conversation — sanctioned and diplomatically isolated the moment they act on the objection — even though their argument (the taboo enforces stratification, not disarmament) is a serious structural claim the regime does not engage.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, aspiring_proliferators, excluded,
    moderate, generational, constrained, national).

% Administer inspections, safeguards, and compliance reporting. They are formally neutral verifiers but structurally dependent on member-state funding and political will, giving them a dual seat: they both observe compliance and actively set the operational agenda of what counts as a violation.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, iaea_and_treaty_verification_bodies, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, iaea_and_treaty_verification_bodies, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(total_war_possibility_space__nuclear_taboo_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, publicly legible standard — 'total war via nuclear weapons is unthinkable/illegitimate' — that lets states coordinate on non-use, non-testing (in most cases), and non-transfer without requiring continuous re-negotiation of the same commitment; this genuinely lowers the risk of miscalculation and arms racing relative to a world with no shared normative baseline at all.
% TRANSFER_FUNCTION: Moves security burden and sovereignty constraint from nuclear-weapon states (who retain full-spectrum deterrent capability under the taboo's exception) to non-nuclear signatories (who accept inspection, forgo acquisition, and depend on extended deterrence) and to populations under those umbrellas (who bear residual catastrophic risk without decision authority).
% ABSENT_VOICES: Aspiring proliferators and states outside the NPT framework (or that view the regime as a stratification device) are not seated at the norm-construction table; their structural objection — that the taboo is applied selectively to preserve an existing five-power hierarchy rather than to eliminate total-war risk as such — surfaces mainly as accusation, not as recognized argument within the regime's own deliberative venues.
% DISAPPEARANCE_RATIONALE: Norm entrepreneurs and the epistemic community would say the world rearranges catastrophically — restraint on first use and testing would erode, proliferation would accelerate, and the moral cost of employment would fall, changing behavior even absent any change in arsenals. Deterrence-equilibrium theorists (a sibling reading) would say the world stays materially unchanged because material mutual vulnerability, not the taboo, is what has actually been doing the restraining work — the taboo is a legitimating story layered atop a deterrence equilibrium that would hold with or without it. The dispute over which account is correct is exactly the kernel contest this story is one reading of.
% FOUNDING_PROBLEM: After Hiroshima and Nagasaki, and accelerating through the Cold War, policymakers and publics needed a way to render first use and further total-war escalation not merely strategically inadvisable but morally and diplomatically unthinkable — a problem material deterrence alone does not solve, since deterrence permits use in extremis while the taboo forecloses it categorically.
% FOUNDING_PROBLEM_CORROBORATION: Norm-entrepreneur states and the epistemic community (constructivist IR scholars, ICAN and similar advocacy networks) attest the founding problem remains live and the taboo is doing independent causal work — citing the 75+ year non-use record as evidence the norm, not merely deterrence, is operative. Realist and deterrence-theory scholars outside the beneficiary set (a corroborating source external to the norm-entrepreneur coalition) dispute this, arguing the non-use record is equally consistent with pure material deterrence and that the taboo's discourse function is to launder an unequal arsenal distribution as a moral achievement rather than to solve an independent problem.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, contested).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored moderate (0.42 at present) and rising slowly: the taboo's coordination function (avoiding miscalculation, providing a shared restraint vocabulary) is genuine, but its enforcement machinery increasingly serves to freeze an unequal five-power exception rather than to advance toward the elimination the NPT's Article VI nominally commits to. Suppression rises faster (0.58) and further than extraction because the apparatus that enforces the taboo — sanctions regimes, export-control cartels, diplomatic isolation of suspected proliferators — has hardened over 80 years into a machinery whose primary observable function is policing the boundary of the exception, not eliminating the underlying capability. Theater ratio is moderate and rising (0.31): a meaningful share of taboo-maintenance activity (disarmament conferences that produce no arsenal reductions, review-conference communiqués reaffirming commitments already made) is performative reaffirmation rather than functional change.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear-weapon-state seat, the taboo looks like coordination they lead and legitimately administer — a genuine achievement of restraint discourse. From the non-nuclear-signatory seat, the same structure looks like enforced asymmetry: they carry the inspection burden and the risk while never holding the exception the taboo protects. The engine computes these as structurally different seat classifications from the same authored data; this divergence is the point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states and norm-entrepreneur states are declared beneficiaries: the former retain the exception the taboo institutionalizes as permanent rather than transitional, and the latter accrue diplomatic standing from taboo custodianship. The epistemic community benefits by association — the taboo is the field's object of study and its funding rationale. Non-nuclear signatories and populations under extended-deterrence umbrellas are declared victims: they accept the treaty's constraints (inspection, forgone acquisition, residual catastrophic risk) without ever holding the exception, and their exit option from the regime (withdrawal) triggers costs disproportionate to the states that never joined it. This maps directly onto the derived directionality: nuclear-weapon-state d sits near the beneficiary end (arbitrage exit, institutional power), non-nuclear-signatory d sits near the target end (trapped exit, moderate power).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rendering total-war escalation categorically unthinkable rather than merely inadvisable — is authored as contested rather than resolved-dead or clearly-live, because the corroboration itself splits along the kernel's fault line: norm entrepreneurs and constructivist scholars treat the problem as live and the taboo as doing real independent work, while deterrence theorists (from outside the beneficiary coalition) read the same non-use record as consistent with pure material restraint, making the taboo's apparatus a legitimating overlay on a deterrence equilibrium that does the actual causal work. Classifying this as tangled_rope rather than snare or pure rope respects that ambiguity: there IS a genuine coordination function (shared normative baseline reduces miscalculation risk) that a pure-snare reading would deny, and there ARE identifiable beneficiaries/victims that a pure-rope reading would deny. The reading predicts the taboo weakens specifically if norm entrepreneurs exit advocacy — which is a testable, falsifiable claim distinguishing this reading from the deterrence_equilibrium sibling (which predicts no change from entrepreneur exit, since material vulnerability, not normative custodianship, is doing the work).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_vs_deterrence_causal_priority,
    'Is the 80-year non-use record actually caused by the constructed normative taboo, or is the taboo discourse an epiphenomenal legitimation of restraint that mutual material vulnerability (the deterrence_equilibrium sibling reading) would have produced anyway?',
    'Natural-experiment analysis of near-use crises (Cuban Missile Crisis, 1969 Sino-Soviet border tension, 1983 Able Archer) coded for whether decision-makers cited normative prohibition versus material/retaliatory calculation as the operative restraint; divergence in cited reasoning across cases with comparable material stakes would favor the taboo reading.',
    'If deterrence equilibrium is doing all the causal work, this constraint''s coordination-function claim collapses and the story is better read as pure extraction (a snare maintaining hierarchy under moral cover) rather than tangled_rope with genuine coordination benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taboo_vs_deterrence_causal_priority, conceptual, 'Whether the taboo has independent causal force or is discourse layered atop deterrence.').

omega_variable(
    norm_entrepreneur_exit_sensitivity,
    'If norm-entrepreneur states materially disinvested from taboo advocacy (withdrew funding from disarmament diplomacy, stopped convening review conferences), would the observed non-use rate change measurably within a generation?',
    'Track advocacy investment levels against proliferation attempts, near-miss incidents, and rhetorical normalization of nuclear use in declared doctrine over the coming decades; a measurable correlation would support the taboo reading''s falsifiable prediction.',
    'A positive finding would strongly corroborate this reading over the deterrence_equilibrium sibling; a null finding would support reclassifying this constraint''s coordination claim as largely theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_entrepreneur_exit_sensitivity, empirical, 'The reading''s own falsifiable prediction: entrepreneur exit should weaken the taboo if the reading is correct.').

omega_variable(
    selective_enforcement_as_stratification_device,
    'Does the taboo apparatus function primarily to prevent total war, or primarily to freeze the current five-power capability hierarchy under the cover of a universal moral prohibition?',
    'Compare enforcement intensity and diplomatic cost imposed on suspected proliferators outside the recognized five versus any comparable normative pressure applied to recognized nuclear-weapon-state modernization programs.',
    'If enforcement asymmetry is severe and consistent, the beneficiary/victim structure authored here understates extraction and the constraint sits closer to snare; if enforcement is genuinely symmetric in practice, the tangled_rope classification with real coordination benefit is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_as_stratification_device, empirical, 'Whether the taboo''s enforcement pattern reveals a stratification function distinct from its stated universal purpose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement(tota_tr_t1975, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(tota_tr_t1995, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1995, 0.24).
narrative_ontology:measurement(tota_tr_t2010, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2025, 0.31).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1945, 0.18).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1962, 0.28).
narrative_ontology:measurement(tota_be_t1975, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1975, 0.34).
narrative_ontology:measurement(tota_be_t1995, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(tota_be_t2010, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement(tota_su_t1962, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1962, 0.38).
narrative_ontology:measurement(tota_su_t1975, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1975, 0.46).
narrative_ontology:measurement(tota_su_t1995, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1995, 0.51).
narrative_ontology:measurement(tota_su_t2010, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(tota_su_t2025, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__nuclear_taboo_reading, 0.12).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, space_contraction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'why hasn't total war recurred since 1945' per the ε-invariance principle. Each sibling authors a structurally distinct mechanism: nuclear_taboo_reading (this story) attributes restraint to constructed normative prohibition and predicts erosion under norm-entrepreneur exit; deterrence_equilibrium_reading attributes restraint to mutual material vulnerability and predicts no change from entrepreneur exit; space_contraction_reading attributes restraint to total war's removal from the strategically thinkable option-space altogether, a cognitive/epistemic claim distinct from both normative and material-deterrence claims. The three share a topic label but not an ε — do not average or reconcile across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
