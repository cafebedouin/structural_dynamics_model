% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__space_contraction_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: total_war_possibility_space__space_contraction_reading
 *   human_readable: Nuclear Weapons as Cognitive-Strategic Absolute: Total War Exits the Thinkable
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint instantiates the space_contraction_reading of the
 *   total_war_possibility_space kernel. It holds that nuclear weapons did not
 *   merely raise the cost of total war (deterrence equilibrium) or construct
 *   a normative prohibition (nuclear taboo), but removed total war from the
 *   realm of strategic cognition entirely. The constraint operates as an
 *   epistemic-institutional absolute: planning apparatus atrophied, general
 *   staffs ceased war-gaming great-power total war, and strategic studies
 *   shifted to sub-nuclear and deterrence domains. It presents itself as a
 *   natural feature of the nuclear ageâa Mountain of strategic
 *   reasonâwhile benefiting identifiable institutional complexes. The
 *   authored metrics and beneficiaries are deliberately divergent from the
 *   mountain claim to enable false-summit detection.
 *
 * KEY AGENTS:
 *   - nuclear_deterrence_establishment: Primary agenda-setter (institutional/identity_locked) â administers and benefits from the unthinkability axiom.
 *   - conventional_mobilization_constituency: Primary target (organized/constrained) â bears extraction through institutional irrelevance.
 *   - status_quo_nuclear_powers: Primary beneficiary (institutional/constrained) â systemic stability from foreclosed total war.
 *   - revisionist_state_elites: Excluded payer (powerful/constrained) â strategic option space foreclosed.
 *   - nuclear_strategy_academia: Secondary beneficiary (organized/identity_locked) â careers and field built on the axiom.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.68).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.58).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Nuclear Weapons as Cognitive-Strategic Absolute: Total War Exits the Thinkable").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '6cbbfc1e-c77e-498b-8560-e86b19be2eaa').
narrative_ontology:cs_kernel_codification('6cbbfc1e-c77e-498b-8560-e86b19be2eaa', implicit).
narrative_ontology:cs_authority_grounding('6cbbfc1e-c77e-498b-8560-e86b19be2eaa', practice).
narrative_ontology:cs_interpretation_layer_present('6cbbfc1e-c77e-498b-8560-e86b19be2eaa').
narrative_ontology:cs_reading_relation('6cbbfc1e-c77e-498b-8560-e86b19be2eaa', total_war_possibility_space__deterrence_equilibrium_reading, forecloses).
narrative_ontology:cs_reading_relation('6cbbfc1e-c77e-498b-8560-e86b19be2eaa', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('6cbbfc1e-c77e-498b-8560-e86b19be2eaa', foundational, total_war_categorically_unthinkable).
narrative_ontology:cs_axiom_status(total_war_categorically_unthinkable, holdable).
narrative_ontology:cs_axiom_grounding('6cbbfc1e-c77e-498b-8560-e86b19be2eaa', total_war_categorically_unthinkable, empirically_contingent).
narrative_ontology:cs_axiom('6cbbfc1e-c77e-498b-8560-e86b19be2eaa', secondary, planning_absence_as_possibility_evidence).
narrative_ontology:cs_axiom_status(planning_absence_as_possibility_evidence, holdable).
narrative_ontology:cs_axiom_grounding('6cbbfc1e-c77e-498b-8560-e86b19be2eaa', planning_absence_as_possibility_evidence, empirically_contingent).
narrative_ontology:cs_reference_frame('6cbbfc1e-c77e-498b-8560-e86b19be2eaa', total_war_planning_norm).
narrative_ontology:cs_drift_state('6cbbfc1e-c77e-498b-8560-e86b19be2eaa', contemporary_nuclear_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('6cbbfc1e-c77e-498b-8560-e86b19be2eaa', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, nuclear_deterrence_establishment).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, status_quo_nuclear_powers).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, nuclear_strategy_academia).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, conventional_mobilization_constituency).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, revisionist_state_elites).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, nuclear_revolution_theory).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, strategic_irrelevance_of_total_war).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls nuclear strategy, doctrine, and force posture; maintains the framework that total war is categorically outside strategic thought. Derives budget, legitimacy, and institutional mission from the permanence of nuclear deterrence. Exit would require dismantling the strategic identity of the organization.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_deterrence_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Military branches and legacy general-staff institutions built around mass mobilization and total-war planning. Lost doctrinal authority and budget share to nuclear forces after 1945. Career paths and institutional culture are trapped in a planning paradigm that the strategic ecosystem no longer validates.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, conventional_mobilization_constituency, payer,
    organized, biographical, constrained, continental).

% Great powers possessing secure second-strike capabilities whose territorial and systemic position is stabilized by the absence of total-war planning. They fund and legitimate the deterrence establishment. Exit would mean voluntarily reintroducing existential risk.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, status_quo_nuclear_powers, beneficiary,
    institutional, generational, constrained, global).

% State elites seeking systemic revision who would prefer large-scale conventional or total-war pathways. Their strategic option space is foreclosed not by battle outcomes but by the consensus that such wars cannot be coherently planned. They are absent from deterrence-theory forums.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, revisionist_state_elites, excluded,
    powerful, biographical, constrained, national).

% University departments, journals, and think tanks organized around nuclear deterrence theory. Careers, citation networks, and conference circuits depend on the unthinkability axiom. Total-war scholarship is marginalized as archaic or strategically illiterate.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_strategy_academia, beneficiary,
    organized, biographical, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__space_contraction_reading, nuclear_deterrence_establishment).
narrative_ontology:fixing_cost_class(total_war_possibility_space__space_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Eliminates the security dilemma spiral toward great-power total war by collapsing the strategic imagination that renders total war a coherent option, thereby ending mobilization-based security competition.
% TRANSFER_FUNCTION: Moves institutional resources, doctrinal authority, and planning capacity from conventional mass-mobilization forces to nuclear-deterrence infrastructure; moves academic legitimacy from war-winning studies to deterrence theory.
% ABSENT_VOICES: Conventional war-fighting advocates and total-war historians are structurally absent from mainstream strategic discourse; their objections are filtered as pre-nuclear anachronism. Revisionist state elites who might pursue territorial change through mass mobilization have no voice in deterrence-theory forums.
% DISAPPEARANCE_RATIONALE: If total war re-entered the strategically thinkable, general staffs would rebuild mobilization doctrines, defense budgets would rebalance toward conventional forces, strategic studies would reconstitute war-winning and total-war departments, and revisionist powers would reopen planning pathways that are now cognitively foreclosed.
% FOUNDING_PROBLEM: The industrialization of warfare in the first half of the twentieth century produced wars of annihilation that threatened state survival; the nuclear completion of this trend made total war self-defeating and necessitated its cognitive elimination from strategy.
% FOUNDING_PROBLEM_CORROBORATION: Conventional military historians and offensive realists attest from outside the beneficiary set that the founding problem persists and the unthinkability claim is contested; no party outside the beneficiary set corroborates the claim that total war is categorically unthinkable as opposed to merely deterred.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__space_contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_possibility_space__space_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Accessibility collapse is very high (0.85) because once the nuclear-revolution framework is accepted, total-war alternatives vanish from the strategic imagination almost completely; this is the signature of a mountain claim. Resistance is moderate (0.35) because the framework is contested by offensive realists and conventional military historians, though they are marginalized. Extractiveness (0.68) is substantial because the constraint channels massive institutional resources into nuclear deterrence and away from conventional preparedness. Theater ratio (0.48) is moderate-to-high: much contemporary deterrence discourse is performative maintenance of an atrophied planning logic. Suppression (0.58) reflects the professional exclusion of total-war scholarship and planning. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The nuclear deterrence establishment and associated academia experience this constraint as a genuine structural feature of strategy (low d, near-beneficiary); the conventional mobilization constituency experiences it as institutional extraction and epistemic violence (high d, full target). The engine will compute divergent seat classifications from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the nuclear deterrence establishment, status quo nuclear powers, and nuclear strategy academia: they collect legitimacy, budget, and careers from the unthinkability axiom, giving them low directionality. Victims are the conventional mobilization constituency (lost institutional relevance) and revisionist state elites (foreclosed strategic pathway), giving them high directionality. The divergence is stark because the constraint reallocates the same planning space from one group to another.
 *
 * MANDATROPHY ANALYSIS:
 *   The space contraction reading prevents mislabeling by requiring the R5 genealogy interview: the founding problem (total war annihilation) is contested in its status. If the problem is dead but the arrangement persists, the constraint is a zombie piton. If the problem is live, it may be a scaffold or rope. Here, the metrics (substantial extraction, rising theater, moderate suppression) combined with contested founding-problem status suggest the constraint is drifting toward piton/snare territory despite its mountain claim. The mandatrophy check catches the divergence between the claim of categorical impossibility and the institutional incentives required to maintain the absence of planning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_discursive_construct,
    'Is the unthinkability of total war a material-technical necessity of nuclear physics, or a discursive construct maintained by institutional gatekeeping and professional socialization?',
    'Comparative analysis of strategic planning documents across nuclear and non-nuclear eras; assessment of whether total-war planning could be revived under changed political conditions without technological change.',
    'If constructed, the constraint is a false summit mountain reclassifying to tangled_rope or snare; if material, it retains mountain classification despite beneficiary presence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_discursive_construct, conceptual, 'Natural law versus constructed constraint ambiguity').

omega_variable(
    institutional_atrophy_reversibility,
    'Does the institutional atrophy of total-war planning reflect irreversible cognitive elimination, or neglect that could be reversed by political demand?',
    'Historical cases of institutional revival (e.g., conscription reinstatement) and war-game reintroduction of total-war scenarios against peer adversaries.',
    'If reversible, the unthinkability is performative theater rather than structural impossibility, raising theater_ratio and lowering accessibility_collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_atrophy_reversibility, empirical, 'Whether planning atrophy is reversible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twps_space_tr_t0, total_war_possibility_space__space_contraction_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(twps_space_tr_t10, total_war_possibility_space__space_contraction_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(twps_space_tr_t20, total_war_possibility_space__space_contraction_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(twps_space_tr_t30, total_war_possibility_space__space_contraction_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(twps_space_tr_t40, total_war_possibility_space__space_contraction_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(twps_space_tr_t50, total_war_possibility_space__space_contraction_reading, theater_ratio, 50, 0.44).
narrative_ontology:measurement(twps_space_tr_t60, total_war_possibility_space__space_contraction_reading, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(twps_space_be_t0, total_war_possibility_space__space_contraction_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(twps_space_be_t10, total_war_possibility_space__space_contraction_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(twps_space_be_t20, total_war_possibility_space__space_contraction_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(twps_space_be_t30, total_war_possibility_space__space_contraction_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(twps_space_be_t40, total_war_possibility_space__space_contraction_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(twps_space_be_t50, total_war_possibility_space__space_contraction_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(twps_space_be_t60, total_war_possibility_space__space_contraction_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(twps_space_su_t0, total_war_possibility_space__space_contraction_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(twps_space_su_t10, total_war_possibility_space__space_contraction_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(twps_space_su_t20, total_war_possibility_space__space_contraction_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(twps_space_su_t30, total_war_possibility_space__space_contraction_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(twps_space_su_t40, total_war_possibility_space__space_contraction_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(twps_space_su_t50, total_war_possibility_space__space_contraction_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(twps_space_su_t60, total_war_possibility_space__space_contraction_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel total_war_possibility_space. It decomposes the colloquial claim that 'nuclear weapons made total war obsolete' into three structurally distinct constraints: this reading (categorical unthinkability), deterrence_equilibrium_reading (high-cost equilibrium), and nuclear_taboo_reading (normative prohibition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
