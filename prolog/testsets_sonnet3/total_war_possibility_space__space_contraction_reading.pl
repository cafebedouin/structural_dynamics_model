% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: total_war_possibility_space__space_contraction_reading
 *   human_readable: Nuclear-Induced Contraction of the Total-War Planning Space
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'total_war_possibility_space': the claim that nuclear weapons did not
 *   merely make great-power total war costlier or normatively disfavored, but
 *   removed it categorically from the set of strategically thinkable options
 *   — a contraction of possibility space rather than a raising of its price
 *   or a moral prohibition layered on top of an unchanged possibility set.
 *   Under this reading, general staffs did not choose to deprioritize
 *   total-war mobilization doctrine as one option among several; the option
 *   itself exited the planning space, producing genuine institutional atrophy
 *   (war colleges stopped war-gaming continental conquest, mobilization
 *   doctrine was not maintained, strategic-studies funding and careers
 *   migrated to sub-nuclear and gray-zone domains). The rising theater_ratio
 *   in the measurement series reflects the reading's own prediction: what
 *   military planning apparatus retains around 'total war' scenarios by 2025
 *   is increasingly performative (legacy doctrine documents, ceremonial
 *   contingency staffs) rather than functional, because the underlying
 *   category has left the live planning space. Extraction is authored low and
 *   rising only modestly, consistent with a mountain-type reading whose faint
 *   upward drift is fully accounted for by the FSM beneficiary declaration
 *   below, not by any hidden coercive mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.28).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.35).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Nuclear-Induced Contraction of the Total-War Planning Space").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '4ccd1443-43aa-45c9-a12e-c797cba031db').
narrative_ontology:cs_kernel_codification('4ccd1443-43aa-45c9-a12e-c797cba031db', distributed).
narrative_ontology:cs_authority_grounding('4ccd1443-43aa-45c9-a12e-c797cba031db', distributed).
narrative_ontology:cs_reading_relation('4ccd1443-43aa-45c9-a12e-c797cba031db', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ccd1443-43aa-45c9-a12e-c797cba031db', total_war_possibility_space__nuclear_taboo_reading, influences).
narrative_ontology:cs_axiom('4ccd1443-43aa-45c9-a12e-c797cba031db', foundational, total_war_exits_possibility_space_categorically).
narrative_ontology:cs_axiom_status(total_war_exits_possibility_space_categorically, holdable).
narrative_ontology:cs_axiom_grounding('4ccd1443-43aa-45c9-a12e-c797cba031db', total_war_exits_possibility_space_categorically, empirically_contingent).
narrative_ontology:cs_axiom('4ccd1443-43aa-45c9-a12e-c797cba031db', secondary, planning_apparatus_atrophy_is_diagnostic_of_categorical_exit).
narrative_ontology:cs_axiom_status(planning_apparatus_atrophy_is_diagnostic_of_categorical_exit, holdable).
narrative_ontology:cs_axiom_grounding('4ccd1443-43aa-45c9-a12e-c797cba031db', planning_apparatus_atrophy_is_diagnostic_of_categorical_exit, empirically_contingent).
narrative_ontology:cs_reference_frame('4ccd1443-43aa-45c9-a12e-c797cba031db', pre_nuclear_total_war_baseline).
narrative_ontology:cs_drift_state('4ccd1443-43aa-45c9-a12e-c797cba031db', post_cold_war_unipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4ccd1443-43aa-45c9-a12e-c797cba031db', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, existing_great_power_order).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, strategic_studies_discipline).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, non_nuclear_states).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, categorical_impossibility_of_great_power_total_war).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the capability whose existence is claimed to have removed great-power total war from the set of strategically conceivable options. Their military planning apparatus, force posture, and diplomatic doctrine are all organized around this removal being real and permanent. They benefit from a strategic environment in which the costliest, most destabilizing category of conflict is treated as off the table rather than merely unattractive.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_weapon_states, beneficiary,
    institutional, civilizational, arbitrage, global).

% Administer the actual planning apparatus — war colleges, mobilization doctrine, contingency staffs. Under this reading their institutional trajectory is to let total-war planning atrophy: mobilization doctrine is not maintained, general-staff war-gaming for great-power conflict ceases, and institutional energy redirects toward sub-nuclear, gray-zone, and proxy domains. They administer the contraction but did not create the underlying physical fact.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, general_staffs_and_defense_planners, agenda_setter,
    institutional, generational, constrained, national).

% Operate inside a strategic order in which total war among the great powers is treated as foreclosed, but conventional, proxy, and sub-threshold conflict is displaced onto their territories and populations instead. They bear the downstream cost of the contraction: the violence that would have been absorbed into (or deterred by) a live total-war category migrates to sub-nuclear theaters where they are the battleground, without having any capability-based claim on the contraction's protective effect.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, non_nuclear_states, payer,
    moderate, generational, trapped, regional).

% The academic and think-tank field reorganizes its research agenda, funding streams, and career paths around the premise that great-power total war is no longer a live planning category. Scholars who build careers on sub-nuclear conflict, limited war, and deterrence theory benefit from the space-contraction premise being treated as settled; the discipline's institutional shape is itself evidence for (or artifact of) the reading.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_studies_discipline, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__space_contraction_reading, strategic_studies_discipline, agenda_setter).

% Study the mobilization, doctrine, and general-staff planning apparatus of the pre-1945 total-war era. Their perspective — that total war was itself a historically bounded and recent category, not a permanent baseline that nuclear weapons then removed — is rarely centered in strategic-studies discourse, which tends to treat the nuclear-era contraction as the interesting event rather than asking whether the prior expansion was itself contingent.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, historians_of_pre_nuclear_total_war, excluded,
    moderate, civilizational, analytical, global).

% Attempt to adjudicate whether total war has become categorically unthinkable (this reading), merely deterred (the equilibrium reading), or normatively tabooed (the taboo reading) by examining planning documents, budget allocations, and doctrine revisions across nuclear states over time.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, counterfactual_war_theorists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the coordination sense — this reading holds that the contraction is a structural fact about strategic possibility space imposed by weapons physics, not an arrangement that solves a collective-action problem for the parties inside it. To the extent institutions coordinate anything, they coordinate around adapting planning apparatus to a changed constraint, not around jointly producing the constraint.
% TRANSFER_FUNCTION: The reading holds nothing is transferred by the constraint itself; what moves is institutional attention and conflict incidence — away from total-war planning and toward sub-nuclear and proxy domains, redistributing the locus of violence onto states outside the nuclear club rather than redistributing rents between named parties.
% ABSENT_VOICES: Historians who would ask whether 'total war' was ever the stable baseline this reading presumes (rather than a brief early-20th-century configuration) are largely absent from strategic-studies discourse; also absent are the populations in non-nuclear regions who absorb the displaced violence and who have no seat in nuclear-doctrine deliberations.
% DISAPPEARANCE_RATIONALE: If nuclear weapons were removed overnight, whether great-power total war re-enters the thinkable set is exactly the question this reading answers in the affirmative and the deterrence-equilibrium reading treats as a live empirical bet rather than a certainty; the parties dispute what would rearrange because they dispute what kind of fact the contraction is.
% FOUNDING_PROBLEM: The felt problem, from inside this reading, is explaining why great-power general staffs stopped war-gaming continental conquest and mobilization-for-total-war doctrine largely vanished after 1945 — the reading answers that the option left the possibility space entirely, not merely that it became unattractive.
% FOUNDING_PROBLEM_CORROBORATION: Some declassified planning archives and doctrine-revision histories (cited by historians outside the nuclear-weapon states' own institutions) are read by proponents of this reading as showing genuine atrophy of total-war planning capacity, not just its deprioritization; critics from the deterrence-equilibrium camp point to continued nuclear force modernization and war-gaming of escalation scenarios as evidence the option was suppressed, not erased — corroboration is contested precisely because the two readings interpret the same archival record differently.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, contested).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__space_contraction_reading_tests).

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
 *   accessibility_collapse is authored high (0.88) because this reading's defining claim IS that the alternative (total war as a live planning option) has collapsed almost completely from the strategic imagination of nuclear-armed general staffs — that is the reading's content, not an artifact of measurement. resistance is authored low (0.22) because, within this reading's own terms, there is little active contestation of the underlying physical fact of mutual destructive capacity; what resistance exists is contestation of the INTERPRETATION (whether the option is truly gone vs. merely suppressed), which belongs to the sibling readings, not to this reading's internal metrics. theater_ratio rises across the interval because as decades pass without total-war planning being exercised, whatever institutional shell remains (legacy contingency documents, ceremonial general-staff functions) increasingly performs a planning function it no longer substantively executes.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear-weapon-state seat, the contraction reads as an settled structural fact underwriting stable planning doctrine. From the non-nuclear-state seat, the same 'contraction' looks like a redistribution of conflict risk onto their territories, dressed in the vocabulary of strategic impossibility rather than acknowledged as a policy choice by the powers who retain nuclear arsenals. The engine's per-seat computation is expected to diverge along exactly this line.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states and the strategic-studies discipline are declared beneficiaries because their institutional order, funding structures, and doctrinal legitimacy are built on the premise that this contraction is real and stable — d sits near the beneficiary end for them. Non-nuclear states are declared payers: under this reading, displaced conflict energy migrates to sub-nuclear and proxy theaters where they, not the nuclear powers, bear the costs, despite having no capability-based claim on the contraction's protective effects — their exit is trapped because they cannot opt out of the great-power order's downstream conflict displacement. General staffs and defense planners are agenda-setters who administer institutional adaptation to the reading's claimed fact without having created the underlying physical constraint themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   This is authored as a mountain (a claim about a structural feature of strategic reality created by weapons physics) that nonetheless carries declared beneficiaries — this is a deliberate FSM (false-summit) candidate. The omega variables document the live ambiguity: is the contraction a genuine emergent structural fact (mountain), or is 'categorical impossibility' itself a constructed framing that benefits nuclear-armed incumbents and the strategic-studies apparatus built around them by foreclosing debate about whether total war remains a live, merely-suppressed option (which would push this toward tangled_rope)? The engine's FSM signature is expected to evaluate this tension directly from the beneficiary declaration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    possibility_space_vs_deterred_option,
    'Is great-power total war genuinely absent from the strategic possibility space (this reading), or does it remain a live, continuously-suppressed option whose absence from visible planning documents reflects successful deterrence rather than categorical exit (the sibling deterrence_equilibrium_reading)?',
    'Examine classified contingency planning, war-gaming exercises, and force-posture documents across nuclear states over the interval: if total-war scenarios continue to be actively gamed and budgeted for even as public doctrine shifts toward sub-nuclear domains, that favors the deterrence reading; if such planning demonstrably atrophies and institutional capacity to conduct it degrades irreversibly, that favors this reading.',
    'If the deterrence reading is correct, this story''s claimed mountain status is wrong — the constraint is better modeled as a very costly but live option (arguably tangled_rope or scaffold), and the institutional atrophy this reading predicts would instead represent dangerous capability erosion rather than a genuine structural feature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(possibility_space_vs_deterred_option, conceptual, 'Whether the contraction is categorical exit or continuous deterred suppression.').

omega_variable(
    fsm_beneficiary_or_genuine_natural_fact,
    'Do nuclear weapon states and the strategic-studies discipline benefit from PROMOTING the space-contraction framing regardless of whether it is true, such that the framing itself is partly a constructed narrative serving incumbent interests rather than a purely emergent physical fact?',
    'Compare doctrinal rhetoric and funding patterns in nuclear states against actual retained total-war contingency capacity; if states that publicly endorse the ''total war is unthinkable'' framing privately maintain substantial total-war planning infrastructure, that supports a constructed-narrative reading over a pure natural-fact reading.',
    'If the framing is substantially self-serving, this constraint should be reclassified via the false_summit_mountain signature toward tangled_rope, with nuclear-weapon states as active beneficiaries of a narrative that forecloses debate about arsenal reduction or alternative security architectures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsm_beneficiary_or_genuine_natural_fact, conceptual, 'Whether declared beneficiaries indicate a false summit over a constructed framing.').

omega_variable(
    displaced_conflict_measurement,
    'Is the claimed transfer of conflict risk onto non-nuclear states (via proxy and sub-nuclear conflict displacement) a real, measurable phenomenon attributable to the total-war contraction, or a confound of other 20th/21st century trends (decolonization, regional rivalries, resource competition)?',
    'Comparative conflict-incidence analysis across nuclear-armed-region versus non-nuclear-region conflicts pre- and post-1945, controlling for decolonization and regional dynamics.',
    'If displacement is not attributable to the nuclear contraction specifically, the payer designation for non_nuclear_states weakens and the story''s extractiveness score should be revised downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_conflict_measurement, empirical, 'Whether displaced conflict is caused by the contraction or by confounding historical trends.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__space_contraction_reading, theater_ratio, 1962, 0.12).
narrative_ontology:measurement(tota_tr_t1975, total_war_possibility_space__space_contraction_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(tota_tr_t1991, total_war_possibility_space__space_contraction_reading, theater_ratio, 1991, 0.3).
narrative_ontology:measurement(tota_tr_t2005, total_war_possibility_space__space_contraction_reading, theater_ratio, 2005, 0.36).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__space_contraction_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.12).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1962, 0.15).
narrative_ontology:measurement(tota_be_t1975, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1975, 0.18).
narrative_ontology:measurement(tota_be_t1991, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1991, 0.2).
narrative_ontology:measurement(tota_be_t2005, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2005, 0.24).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2025, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_possibility_space__space_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the colloquial 'nuclear weapons ended total war' claim under the kernel total_war_possibility_space. space_contraction_reading (this story) claims a categorical/ontological change in the thinkable option set. deterrence_equilibrium_reading claims the option remains live but is deterred by mutual vulnerability (a cost/equilibrium claim). nuclear_taboo_reading claims a normative prohibition constructed independent of material capability. Each carries its own ε and stakeholder structure per the ε-invariance principle; they are linked here rather than merged because measuring 'has total war become impossible' by different lights (ontological possibility vs. strategic equilibrium vs. normative status) yields materially different ε and beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
