% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__deterrence_equilibrium_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Total War Deterrence Through Mutual Vulnerability
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This story instantiates the DETERRENCE_EQUILIBRIUM reading of the
 *   contested kernel 'total_war_possibility_space'. The reading asserts:
 *   total war remains strategically reachable and is deterred by mutual
 *   vulnerability—a cost-benefit calculation where rational actors choose not
 *   to escalate because the costs of total war exceed the benefits. This
 *   reading generates continuous investment in war-fighting capability as
 *   deterrent signal, doctrine development around counterforce targeting, and
 *   the persistence of escalation ladders in military planning. The
 *   constraint is CLAIMED as tangled_rope because it coordinates
 *   non-escalation (genuine coordination function) while simultaneously
 *   transferring strategic authority over war-possibility judgment to
 *   nuclear-armed states (asymmetric extraction). The measured extractiveness
 *   (0.68) and suppression (0.72) reflect that transfer of authority and the
 *   active enforcement (exclusion of non-escalation advocates from defense
 *   planning) required to maintain the framework. The theater_ratio (0.41) is
 *   moderate-high because deterrence doctrine requires performative
 *   maintenance—continuous signaling of deterrent intent—even as the
 *   underlying functional need (preventing escalation) may be satisfied by
 *   simpler mechanisms. The accessibility_collapse (0.38) is below typical
 *   rope values because alternatives to deterrence (taboo, structural
 *   foreclosure, arms reduction) remain theoretically available; they are
 *   suppressed but not collapsed. The constraint family links
 *   deterrence_equilibrium to its sibling readings: each produces a different
 *   classification, different directionalities, different beneficiary/victim
 *   structures.
 *
 * KEY AGENTS:
 *   - nuclear_armed_states: institutional agenda_setters, trapped in deterrence framework they authored, beneficiaries of the authority structure
 *   - non_nuclear_states: constrained payers, bearing escalation risk not of their choosing, excluded from deterrence doctrine revision
 *   - escalation_risk_bearing_populations: powerless payers, experiencing standing risk of total war, no institutional seat in framework revision
 *   - military strategists: beneficiaries through identity_lock to doctrine, institutional authority dependent on framework persistence
 *   - non_escalation doctrine proponents: beneficiaries whose policy recommendations rest on framework credibility
 *   - competing reading advocates: excluded, moderately powerful, pushed to margins by framework dominance
 *   - competing powers: observers, institutional, analytically positioned, contesting framework application
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.68).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.72).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Total War Deterrence Through Mutual Vulnerability").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, '268f0e15-fce2-4710-a998-e1e154fd4529').
narrative_ontology:cs_kernel_codification('268f0e15-fce2-4710-a998-e1e154fd4529', formalized).
narrative_ontology:cs_authority_grounding('268f0e15-fce2-4710-a998-e1e154fd4529', extraction).
narrative_ontology:cs_interpretation_layer_present('268f0e15-fce2-4710-a998-e1e154fd4529').
narrative_ontology:cs_reading_relation('268f0e15-fce2-4710-a998-e1e154fd4529', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_reading_relation('268f0e15-fce2-4710-a998-e1e154fd4529', total_war_possibility_space__space_contraction_reading, influences).
narrative_ontology:cs_axiom('268f0e15-fce2-4710-a998-e1e154fd4529', foundational, mutual_vulnerability_produces_rational_deterrence).
narrative_ontology:cs_axiom_status(mutual_vulnerability_produces_rational_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('268f0e15-fce2-4710-a998-e1e154fd4529', mutual_vulnerability_produces_rational_deterrence, instrumental).
narrative_ontology:cs_axiom('268f0e15-fce2-4710-a998-e1e154fd4529', foundational, total_war_remains_strategically_reachable).
narrative_ontology:cs_axiom_status(total_war_remains_strategically_reachable, holdable).
narrative_ontology:cs_axiom_grounding('268f0e15-fce2-4710-a998-e1e154fd4529', total_war_remains_strategically_reachable, empirically_contingent).
narrative_ontology:cs_reference_frame('268f0e15-fce2-4710-a998-e1e154fd4529', mutual_vulnerability_equilibrium).
narrative_ontology:cs_drift_state('268f0e15-fce2-4710-a998-e1e154fd4529', contemporary_multipolarity, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('268f0e15-fce2-4710-a998-e1e154fd4529', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_armed_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, non_escalation_doctrine_proponents).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, escalation_risk_bearing_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, military_strategists_and_doctrine_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain strategic doctrine that total war is deterred by mutual vulnerability and second-strike capacity. Set the terms of what counts as 'rational' strategy, structure military planning around counterforce doctrine, and continuously invest in war-fighting capability to credibly signal deterrent intent. Benefit from a world order where their nuclear arsenals are treated as the primary structural constraint on conflict escalation. Their exit from the deterrence framework means nuclear disarmament, which they treat as strategically unthinkable given continuing adversary arsenals.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_armed_states, agenda_setter,
    institutional, civilizational, trapped, global).

% Exist under an umbrella of strategic uncertainty: the deterrence framework assumes total war is averted, but the basis of avoidance is cost-benefit calculation at a threshold of catastrophic harm. Non-nuclear states either align with a nuclear power (accepting exposure to escalation dynamics they do not control) or remain outside alliance structures (bearing isolation risk). Their options for influencing the deterrence threshold are limited; escalation dynamics are set by nuclear powers' doctrinal choices, not by their preference or capacity.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states, payer,
    moderate, generational, constrained, global).

% Bear the actual risk of total war: civilians in all territories, including nuclear-armed and non-nuclear states. The deterrence framework is presented as protecting them from total war; simultaneously, the continuous maintenance of war-fighting capability and counterforce doctrine means they live under a standing technical possibility of escalation. They have no institutional seat at the table where deterrence doctrine is revised.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, escalation_risk_bearing_populations, payer,
    powerless, immediate, trapped, global).

% Maintain careers and institutional authority through the continuous development and refinement of deterrence doctrine, counterforce targeting theory, and escalation ladders. The deterrence framework legitimates their expertise and justifies military research budgets. Their professional identity is fused with the framework: moving outside deterrence theory is to cease being a strategist in the operational sense.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, military_strategists_and_doctrine_community, beneficiary,
    institutional, biographical, identity_locked, global).

% Advocates, scholars, and some policymakers who benefit from the deterrence framework's credibility: their policy recommendations (maintain deployments, invest in counterforce, avoid provocative signals) are treated as strategically sound precisely because the framework treats total war as averted through mutual vulnerability. Their alternative (constructing non-military deterrence or accepting war as structurally impossible) would require institutional reorganization they do not control.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, non_escalation_doctrine_proponents, beneficiary,
    powerful, biographical, mobile, global).

% Scholars and advocates of the nuclear_taboo_reading or space_contraction_reading: they argue total war is normatively prohibited or structurally unthinkable, not merely deterred by cost-benefit. Their institutional presence is weaker in military strategy; they are systematically excluded from defense policy planning by the dominance of deterrence reasoning. Including them would require reopening the kernel question of what constrains total war.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, competing_reading_advocates, excluded,
    moderate, biographical, constrained, global).

% Rising or competing nuclear-armed states that view the incumbent deterrence framework as authored to lock in the current power distribution. They observe the framework but contest its application to new multipolar configurations or emerging technologies. Their position is structurally ambiguous: they partially operate within deterrence logic while seeking to shift its terms.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, competing_powers_outside_framework, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_armed_states).
narrative_ontology:fixing_cost_class(total_war_possibility_space__deterrence_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents total war escalation by embedding war-fighting capability as a credible signal that unacceptable costs attend escalation beyond declared thresholds. Solves the coordination problem: how do nuclear powers signal non-hostile intent while maintaining military readiness? Answer: through doctrinal statements that total war is deterred by mutual vulnerability, not chosen.
% TRANSFER_FUNCTION: Transfers strategic authority over the possibility of total war to nuclear-armed states and their military-strategic communities. Non-nuclear states and non-combatant populations receive a standing claim on deterrence (your safety is purchased by the threat structure) in exchange for accepting that the threshold of total war is set by nuclear powers' cost-benefit calculations, not by universal agreement or non-military institutions.
% ABSENT_VOICES: Populations bearing escalation risk have no voice in deterrence doctrine revision. Non-nuclear states that would advocate for alternative war-prevention mechanisms (non-military security cooperation, arms reduction) are structurally excluded from defense planning conversations. Advocates of competing readings (taboo or structural foreclosure) are marginalized in defense policy. No deliberative process asks whether the framework is the only way to prevent total war; the framework itself answers the question by defining the field of strategic possibility.
% DISAPPEARANCE_RATIONALE: If the deterrence framework disappeared, military planning would reorganize immediately: without the shared understanding that total war is averted through mutual vulnerability, states would face an open question about whether total war is strategically possible. Defense budgets, targeting doctrine, alliance structures, and technology development would shift to address that new uncertainty. The absence of the framework does not eliminate total war as a possibility; it removes the shared cost-benefit story that treats it as averted.
% FOUNDING_PROBLEM: Total war became technically possible (WWI industrialization, then atomic weapons) but remained strategically catastrophic for all parties. How can a stable order persist when total war is both possible and mutually unacceptable? The deterrence framework answers: through mutual vulnerability and credible second-strike capacity, rational actors will choose not to escalate beyond the war-fighting threshold.
% FOUNDING_PROBLEM_CORROBORATION: Military strategists and nuclear-armed states attest the founding problem is live: deterrence doctrine remains necessary because adversary arsenals persist and the threat of escalation remains real. Non-escalation scholars and some arms-reduction advocates attest the problem has been partially or wholly solved by normative shifts and institutional development (arms control treaties, non-proliferation regimes, taboo against nuclear use). Competing readings documented in network links (nuclear_taboo_reading, space_contraction_reading) argue the strategic problem itself has shifted from how to prevent total war (founding problem) to how to prevent ANY war or how to ensure total war is structurally unthinkable.
narrative_ontology:disappearance_verdict(total_war_possibility_space__deterrence_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__deterrence_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__deterrence_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures the degree to which the deterrence framework transfers strategic decision-making authority from distributed actors (states, populations) to nuclear-armed powers and their strategic communities. At 0.68, this reflects: (1) non-nuclear states' constrained options (accept umbrella alignment or isolation); (2) populations' lack of voice in deterrence threshold-setting; (3) military strategists' monopoly on war-possibility judgment. The measurement series show extractiveness rising from 1945 (0.45—early deterrence formation, competing frameworks still present) through the Cold War peak (1962: 0.58 during Cuban Missile Crisis when deterrence doctrine was most assertive), maintaining through the post-Cold War period with a dip in 1990 (0.55—brief moment of nuclear superpower cooperation and non-proliferation hopes) before rising again (2010: 0.64; 2025: 0.68 as multipolarity creates new deterrence complexity). Suppression follows a similar arc: the framework suppresses alternative war-prevention narratives and excludes competing readings through institutional dominance of deterrence reasoning in defense planning. Theater_ratio rises from 1945 (0.22—deterrence was new, functionality still primary) through the Cold War (1962–1975: 0.31–0.38 as deterrence doctrine became increasingly elaborate and ceremonial) and stabilizes at moderate levels (0.35–0.41 from 1990 onward) because the doctrine continues to perform its signaling function even as the underlying strategic problem may have shifted. Accessibility_collapse at 0.38 reflects the persistence of alternatives: populations and non-nuclear states are not locked into total cognitive collapse about alternatives; they simply lack institutional power to enforce alternatives. Resistance at 0.52 reflects sustained challenge from non-escalation advocates, arms-control movements, and competing readings—the framework faces real resistance but manages to persist through institutional dominance and the credibility of mutual vulnerability as a mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear-armed state seat, the deterrence framework is a stability mechanism they maintain through doctrine and force posture; from their position, total war is averted through rational cost-benefit calculation and deterrent capability is the proof. From the non-nuclear state seat, the same framework appears as constraints on their strategic options and exposure to escalation risk they do not control; their exit from the arrangement means vulnerability or isolation. From the population seat, deterrence appears as a standing threat—the promise of safety purchased by the threat of mutual destruction. From the military strategist seat, deterrence is the primary strategic reality: their professional identity and career are organized around its perpetuation. From the competing reading advocate seat, the framework is a false naturalness imposed on what is actually a normative taboo or structural foreclosure. The engine computes per-seat classifications from this structural data: nuclear-armed states likely compute tangled_rope or rope (beneficiaries coordinating escalation prevention); non-nuclear states likely compute snare (victims of asymmetric authority transfer); populations compute snare (targets of suppression through excluded voice); strategists compute rope (beneficiaries without overt extraction); competing advocates compute snare (excluded, paying the cost of framework dominance). These divergences are the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-armed states have directionality near beneficiary (0.0–0.2): they set the framework, control its terms, benefit from its legitimation of their arsenals and authority over war-possibility judgment. Non-nuclear states have directionality near target (0.75–1.0): they bear constraints (limited strategic options, escalation exposure) and lack control over the framework. Populations have directionality near full-target (0.85–1.0): they bear maximum risk and have no voice. Military strategists have directionality near beneficiary (0.15–0.35): they benefit from the framework's institutional authority but are not fully immune—if the framework collapses, their expertise evaporates. Competing reading advocates have directionality at target (0.7–0.9): they are excluded and bear the cost of marginalization. No directionality overrides are needed for this story; the structural derivation from beneficiary/victim data + exit options produces the expected directional spread.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to prevent total war when it is both possible and mutually unacceptable) was live in 1945–1962 (period of doctrine development, Cuban Missile Crisis confirmation). By 2025, the problem has shifted: non-proliferation and arms-control regimes have institutionalized, the norm against nuclear use has strengthened, and competing readings argue the problem itself has been solved (either through taboo or through structural foreclosure). The measured high extractiveness (0.68) and moderate theater (0.41) at 2025 suggest mandate creep: the original problem (prevent escalation through deterrence) has been partially solved, but the framework persists and has extracted additional value (authority transfer, military doctrine dominance, institutional entrenchment). The constraint shows mandatrophy symptoms: the coordination function (preventing escalation) remains genuine, but the extraction function (transferring authority to nuclear powers) has become a primary purpose. The framework could, in principle, be replaced by arms reduction, normative prohibition, or structural foreclosure—but doing so would require dismantling the institutional structures that now benefit from the constraint's persistence. The disappearance_verdict (world_rearranges) and founding_problem_status (contested) jointly signal mandatrophy: the problem is partially solved but contested, yet the framework persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_vs_taboo_independence,
    'Does deterrence_equilibrium reading stand independently from nuclear_taboo_reading, or is deterrence empirically dependent on normativity constructed through taboo?',
    'Historical-counterfactual: construct a scenario with mutual vulnerability but absent taboo (hypothetical: nuclear use is not prohibited normatively, only deterred by cost-benefit). Would deterrence persist? Alternatively: find empirical cases where mutual vulnerability existed without taboo and measure deterrence stability. Or: historical analysis of whether deterrence doctrine explicitly acknowledges normative constraint or claims pure cost-benefit operation.',
    'If independent: the readings are genuinely coexistent and the deterrence_equilibrium reading''s classification holds separately from the taboo reading''s classification. If dependent: the deterrence reading describes a mechanism that requires taboo to operate; they are not two readings of the same kernel but rather one is foundational to the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_vs_taboo_independence, empirical, 'Whether deterrence through mutual vulnerability operates independently of normative taboo.').

omega_variable(
    escalation_ladder_rationality,
    'Do the escalation ladders and counterforce doctrines that the deterrence framework generates constitute rational deterrent signaling (as deterrence_equilibrium asserts) or constitute dangerous brinkmanship that increases escalation risk (as space_contraction and taboo readings suggest)?',
    'Analysis of doctrine-practice alignment: do military exercises and force posture actually implement the escalation ladders theory, or are they cosmetic? Historical incidents: when doctrine was tested (Cuban Missile Crisis, Kargil, Taiwan Strait), did the escalation ladder mechanism work as predicted (deterrence held) or did ad-hoc diplomacy and luck prevent disaster (suggesting mechanism is unreliable)?',
    'If rational signaling: the constraint coordinates non-escalation as claimed; extracted authority is the price of coordination. If brinkmanship: the framework is primarily extractive (transfers authority to strategists who benefit from crisis) with coordination as accidental byproduct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escalation_ladder_rationality, empirical, 'Whether escalation ladders function as deterrent signals or as mechanisms that increase escalation risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1945, 0.22).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1962, 0.31).
narrative_ontology:measurement(tota_tr_t1975, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1975, 0.38).
narrative_ontology:measurement(tota_tr_t1990, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(tota_tr_t2010, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2010, 0.41).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2025, 0.41).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1945, 0.45).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1962, 0.58).
narrative_ontology:measurement(tota_be_t1975, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1975, 0.62).
narrative_ontology:measurement(tota_be_t1990, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(tota_be_t2010, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1945, 0.52).
narrative_ontology:measurement(tota_su_t1962, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1962, 0.68).
narrative_ontology:measurement(tota_su_t1975, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1975, 0.71).
narrative_ontology:measurement(tota_su_t1990, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(tota_su_t2010, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(tota_su_t2025, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__deterrence_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__deterrence_equilibrium_reading, 0.12).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__nuclear_taboo_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__space_contraction_reading).

% DUAL FORMULATION NOTE:
% The kernel 'total_war_possibility_space' decomposes into three readings, each instantiating a different constraint with different ε values and beneficiary/victim structures. deterrence_equilibrium_reading asserts cost-benefit deterrence (high extractiveness, continuous doctrine); nuclear_taboo_reading asserts normative prohibition (lower extractiveness, doctrine as performance); space_contraction_reading asserts structural foreclosure (minimal extractiveness, planning ceases). Each reading is a distinct constraint in the Deferential Realism framework. The three are linked by kernel identity and reading-relational structure (reading_relations array in cs_structure). They are NOT framings of a single underlying constraint—they have different ε values, different beneficiary structures, and different structural predictions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
