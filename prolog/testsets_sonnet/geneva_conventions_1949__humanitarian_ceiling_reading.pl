% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions 1949 — Humanitarian Ceiling Reading (Common Article 3 / GC IV Absolute Minimums)
 *   domain: international_law/law_of_armed_conflict/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the humanitarian-ceiling reading of the Geneva
 *   Conventions 1949 kernel: the claim that Common Article 3 and the
 *   civilian/detainee protections establish an absolute floor of humane
 *   treatment that binds state parties regardless of adversary reciprocity or
 *   the operational character of the conflict. This is distinct from the
 *   conditional-reciprocity reading (protections scale with adversary
 *   compliance) and the security-maximization reading (protections yield to
 *   operational necessity in asymmetric conflict) — those are separate
 *   constraints with different beneficiary/victim structures and different
 *   epsilon values, linked here via network.affects_constraints, not blended
 *   into this one. The ceiling reading is the interpretive position most
 *   associated with the ICRC, international humanitarian law scholarship, and
 *   post-WWII tribunal jurisprudence; it treats the founding atrocities of
 *   WWII as evidence that reciprocity-conditioned or necessity-conditioned
 *   protections collapse exactly when protection is most needed.
 *
 * KEY AGENTS:
 *   - state_military_operational_commanders: bear the ceiling's operational cost without reciprocal guarantee from irregular adversaries
 *   - detainees_and_pow_status_ineligible_combatants: primary beneficiaries of the floor irrespective of their own conduct or legal status
 *   - icrc_and_monitoring_bodies: institutional agenda-setter and interpretive authority sustaining the ceiling reading
 *   - civilian_populations_in_conflict_zones: powerless beneficiaries with no direct enforcement capacity
 *   - national_security_agencies_facing_irregular_adversaries: seek exit via status-redefinition, which the ceiling reading treats as impermissible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.31).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.72).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions 1949 — Humanitarian Ceiling Reading (Common Article 3 / GC IV Absolute Minimums)").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "international_law/law_of_armed_conflict/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, 'a6f01398-edff-4cca-afc2-a5e06f859f87').
narrative_ontology:cs_kernel_codification('a6f01398-edff-4cca-afc2-a5e06f859f87', fixed_text).
narrative_ontology:cs_authority_grounding('a6f01398-edff-4cca-afc2-a5e06f859f87', lineage).
narrative_ontology:cs_interpretation_layer_present('a6f01398-edff-4cca-afc2-a5e06f859f87').
narrative_ontology:cs_reading_relation('a6f01398-edff-4cca-afc2-a5e06f859f87', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6f01398-edff-4cca-afc2-a5e06f859f87', geneva_conventions_1949__security_maximization_reading, forecloses).
narrative_ontology:cs_axiom('a6f01398-edff-4cca-afc2-a5e06f859f87', foundational, humanitarian_minimums_are_non_derogable).
narrative_ontology:cs_axiom_status(humanitarian_minimums_are_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('a6f01398-edff-4cca-afc2-a5e06f859f87', humanitarian_minimums_are_non_derogable, deontological).
narrative_ontology:cs_axiom('a6f01398-edff-4cca-afc2-a5e06f859f87', foundational, protection_status_independent_of_adversary_conduct).
narrative_ontology:cs_axiom_status(protection_status_independent_of_adversary_conduct, holdable).
narrative_ontology:cs_axiom_grounding('a6f01398-edff-4cca-afc2-a5e06f859f87', protection_status_independent_of_adversary_conduct, deontological).
narrative_ontology:cs_axiom('a6f01398-edff-4cca-afc2-a5e06f859f87', secondary, operational_necessity_cannot_suspend_the_floor).
narrative_ontology:cs_axiom_status(operational_necessity_cannot_suspend_the_floor, holdable).
narrative_ontology:cs_axiom_grounding('a6f01398-edff-4cca-afc2-a5e06f859f87', operational_necessity_cannot_suspend_the_floor, conventional).
narrative_ontology:cs_reference_frame('a6f01398-edff-4cca-afc2-a5e06f859f87', post_1949_absolute_floor_consensus).
narrative_ontology:cs_drift_state('a6f01398-edff-4cca-afc2-a5e06f859f87', post_9_11_asymmetric_warfare_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a6f01398-edff-4cca-afc2-a5e06f859f87', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, detainees_and_pow_status_ineligible_combatants).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, icrc_and_monitoring_bodies).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, post_conflict_reconciliation_processes).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_military_operational_commanders).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, counterinsurgency_forces).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, national_security_agencies_facing_irregular_adversaries).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, human_dignity_is_non_derogable).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, jus_cogens_prohibition_on_torture).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, distinction_principle_universality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must extend humanitarian minimums (no torture, no summary execution, humane treatment, access to protections) to captured irregular fighters even when those fighters wear no uniform, hide among civilians, and their own side executes captured state soldiers. Commanders cannot suspend the ceiling to match adversary conduct; violations by the enemy do not licence reciprocal violations. This is experienced as an asymmetric operational burden — legal exposure and battlefield risk rise together, and the exit option (simply not complying) exists in practice but carries war-crimes liability and political cost that most commanders will not accept.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, state_military_operational_commanders, payer,
    institutional, immediate, constrained, national).

% Ground units conducting detention and interrogation operations against non-state armed groups are bound by Common Article 3 minimums regardless of whether the adversary honors any comparable restraint. They bear the direct tactical cost of the ceiling — intelligence-gathering methods are constrained, detention timelines are constrained, and adversaries who ignore the Conventions entirely face no matching consequence from the ceiling reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, counterinsurgency_forces, payer,
    organized, immediate, constrained, regional).

% Interrogation and detention policy is constrained by the absolute floor even in asymmetric conflicts where the agency judges the threat to justify harsher measures. The agency can lobby to redefine the conflict's legal character (e.g., 'unlawful combatant' status) to escape the ceiling, but the humanitarian-ceiling reading treats such redefinition as impermissible evasion, not a legitimate exit.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, national_security_agencies_facing_irregular_adversaries, payer,
    institutional, biographical, constrained, national).

% Receive protection from targeting, collective punishment, and abuse regardless of which side is 'winning' the compliance contest. They have no capacity to enforce the ceiling themselves and depend entirely on third-party monitoring and the state's own restraint; their only leverage is that violations create legal and reputational liability for the violating state.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations_in_conflict_zones, beneficiary,
    powerless, immediate, trapped, regional).

% Captured fighters who do not qualify for full POW status under the conditional-reciprocity or security-maximization readings retain humane-treatment guarantees under this reading purely by virtue of being human beings in detention, not by virtue of their side's conduct or legal category. They cannot negotiate or improve their position; the ceiling is their only protection and it operates independent of anything they or their side did.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, detainees_and_pow_status_ineligible_combatants, beneficiary,
    powerless, immediate, trapped, national).

% Interprets and advocates for the absolute-floor reading, conducts detention visits, and publicly documents violations without regard to reciprocity claims by either side. Administers no enforcement mechanism beyond exposure and diplomatic pressure, but its interpretive authority is what keeps the ceiling reading institutionally alive against the competing readings.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, icrc_and_monitoring_bodies, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__humanitarian_ceiling_reading, icrc_and_monitoring_bodies, observer).

% War crimes tribunals, truth commissions, and reconciliation frameworks rely on a settled, non-negotiable baseline of what conduct was impermissible during the conflict. If the ceiling had been conditional on reciprocity, post-conflict accountability would collapse into competing tu quoque defenses; the absolute-minimum reading gives these processes a fixed reference point.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, post_conflict_reconciliation_processes, beneficiary,
    moderate, generational, analytical, national).

% Bound by the same minimums when they hold detainees, but rarely party to the treaty-drafting or interpretive process that sets the ceiling's content; their voice enters only through customary-law arguments and post-hoc invocation by advocates, not through direct participation in defining what the floor requires.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_armed_groups, excluded,
    powerless, immediate, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__humanitarian_ceiling_reading, diffuse).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__humanitarian_ceiling_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, non-negotiable floor of humane treatment that all parties to a conflict are held to, removing treatment-of-persons from the bargaining table entirely so that battlefield reciprocity disputes cannot be used to justify atrocity.
% TRANSFER_FUNCTION: Moves operational latitude and interrogation/detention discretion away from state military and security institutions and toward captured persons and civilian populations, in the form of guaranteed minimum treatment that cannot be traded away regardless of battlefield conduct.
% ABSENT_VOICES: Irregular armed groups and non-state actors are bound by the ceiling but were not signatories to the 1949 Conventions and have limited institutional voice in defining its content; some state military establishments argue their operational perspective is treated as inherently suspect by the ceiling reading's interpretive community.
% DISAPPEARANCE_RATIONALE: If the absolute-ceiling reading were replaced overnight by the conditional-reciprocity or security-maximization readings, detainee treatment in asymmetric conflicts would become explicitly contingent on adversary conduct, war crimes tribunals would lose their non-negotiable baseline, and the legal exposure currently borne by commanders and interrogators would largely evaporate — the practical conduct of counterinsurgency and detention operations would visibly change.
% FOUNDING_PROBLEM: Post-WWII drafters sought to prevent the recurrence of treatment atrocities (against POWs, civilians, and detainees) that had been rationalized during the war by claims of military necessity, adversary non-compliance, or the enemy's supposed forfeiture of protected status.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC and independent international law scholars outside any single state's military establishment attest that irregular and asymmetric warfare has made treatment-status disputes (uniform requirements, unlawful-combatant designations) the primary mechanism by which states have historically sought to evade the floor — corroborating that the founding problem (rationalized departure from minimums under claimed necessity or non-reciprocity) remains active rather than historical. State security agencies dispute this characterization and argue the founding context (interstate war between uniformed militaries) no longer matches contemporary irregular conflict, which is itself evidence for the founding-problem contest rather than its resolution.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__humanitarian_ceiling_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__humanitarian_ceiling_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 0.31, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).
:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.31) rather than low, because the ceiling reading does impose a real, asymmetric operational cost transfer from state militaries to protected persons — commanders and interrogators lose latitude and bear legal exposure that does not depend on their own security assessment. It is not authored high because the transfer funds a genuine, non-trivial coordination good (a stable floor that prevents mutual-atrocity spirals) rather than pure rent extraction. Suppression is authored high (0.72) because the reading is actively and increasingly enforced against operational-necessity arguments — the entire point of the 'absolute' framing is to suppress the security-maximization rationale as a valid exit, and that suppressive function has hardened over the post-Cold War period as asymmetric conflict became the dominant conflict form. Theater ratio is kept low-moderate (0.22): monitoring and reporting mechanisms (ICRC visits, tribunal proceedings) are largely functional rather than performative, though a growing share of state compliance messaging is arguably reputational theater layered on top of substantive constraint. Accessibility collapse (0.58) and resistance (0.68) reflect that operational-necessity arguments for departing from the floor remain live and actively pressed by security establishments — this is not a settled natural-law-like constraint; it is a contested normative commitment defended against continuous resistance from exactly the actors it binds.
 *
 * PERSPECTIVAL GAP:
 *   From the operational-commander seat, the ceiling reading computes as a burden without reciprocal benefit — a tangled-rope structure where they are coordinated into restraint that the adversary need not match, with real enforcement (war crimes liability) behind it. From the detainee or civilian seat, the same structure computes as close to a pure protective floor with minimal cost to them. From the ICRC's analytical seat, it appears as the coordination mechanism functioning as designed: the very fact that it is costly to the powerful party is what makes it a meaningful constraint rather than an empty gesture. This divergence is exactly what the tangled-rope classification is meant to capture — genuine coordination function (preventing mutual atrocity spirals) coexisting with asymmetric extraction (state militaries pay disproportionately, adversaries who defect pay nothing under this reading specifically).
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations and detainees are near the full-beneficiary end: they receive a guaranteed floor of treatment they did nothing to earn and cannot bargain away, and they bear essentially none of the compliance cost. State military commanders, counterinsurgency forces, and national security agencies sit near the full-target end: the ceiling constrains their operational discretion specifically and asymmetrically, without a matching constraint on irregular adversaries who may not honor the Conventions at all. The ICRC occupies an agenda-setter position with analytical exit (it does not fight the wars it monitors) but real institutional stakes in the ceiling reading's survival, since the reading is largely coextensive with its own interpretive mandate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rationalized departure from humane treatment under claims of necessity or enemy non-compliance) is authored as live, not dead — asymmetric and irregular conflict has, if anything, made status-redefinition and necessity claims more common as evasion mechanisms, not less. This blocks a mandatrophy misreading in either direction: the ceiling is not obsolete scaffolding whose sunset has passed (there is no sunset clause; it was drafted as permanent), and it is not being kept alive as pure institutional inertia — the ICRC's continued monitoring activity and the still-contested status of 'unlawful combatant' doctrine indicate the underlying dispute the Conventions were built to settle remains actively fought over, not resolved and forgotten.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ceiling_versus_reciprocity_as_the_true_kernel_reading,
    'Does the 1949 Conventions text and its travaux préparatoires support the humanitarian-ceiling reading as the drafters'' intended meaning, or is the ceiling reading itself a later interpretive expansion (largely post-1977 Additional Protocols and subsequent tribunal jurisprudence) grafted onto a text that was originally understood in more conditional/reciprocal terms?',
    'Historical-legal analysis of the 1949 diplomatic conference records, state reservations at ratification, and the doctrinal trajectory from 1949 through the 1977 Additional Protocols and ICTY/ICTR jurisprudence establishing Common Article 3 as customary international law binding all parties.',
    'If the ceiling reading is a later interpretive layer rather than the original textual meaning, this constraint''s claimed_type and its foreclosure relationship to the security-maximization reading would need re-evaluation — the reading might be better modeled as a scaffold interpretation that hardened into a claimed absolute over time, rather than the kernel''s original content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceiling_versus_reciprocity_as_the_true_kernel_reading, conceptual, 'Whether the absolute-ceiling reading is the kernel''s original meaning or a later interpretive accretion.').

omega_variable(
    asymmetric_burden_versus_genuine_natural_right,
    'Is the asymmetric operational burden borne by state militaries under this reading a cost of genuine moral/legal coordination (preventing atrocity spirals is worth the asymmetry), or does the asymmetry itself undermine the reading''s claim to be a neutral humanitarian floor rather than a de facto disarmament of state security response relative to irregular adversaries who face no matching constraint?',
    'Comparative analysis of conflict outcomes and atrocity rates in conflicts where the ceiling reading was rigorously applied versus conflicts where security-maximization or conditional-reciprocity approaches dominated, controlling for conflict type and adversary characteristics.',
    'If rigorous ceiling application correlates with reduced overall atrocity (including by irregular forces, via norm diffusion or reduced escalation), the tangled-rope classification''s coordination component is strongly supported. If it correlates with no measurable protective benefit but real operational cost, the classification should weight toward snare from the state-military seat specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asymmetric_burden_versus_genuine_natural_right, empirical, 'Whether the ceiling''s asymmetric burden produces genuine protective coordination value or is extraction dressed as humanitarianism.').

omega_variable(
    icrc_interpretive_authority_versus_institutional_self_interest,
    'Is the ICRC''s advocacy for the ceiling reading a disinterested application of its founding humanitarian mandate, or does the organization have an institutional interest in the ceiling reading''s dominance (funding, mandate scope, moral authority) that should be weighed when assessing its role as agenda_setter rather than pure observer?',
    'Examination of ICRC funding structures, mandate-renewal dependencies, and comparison of ICRC public positions with positions of humanitarian law scholars institutionally independent of the ICRC.',
    'If the ICRC has material institutional stakes in the ceiling reading''s continued dominance, its dual agenda_setter/observer role should be weighted toward agenda_setter with corresponding directionality implications, rather than treated as a neutral analytical seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(icrc_interpretive_authority_versus_institutional_self_interest, conceptual, 'Whether the ICRC''s interpretive advocacy is disinterested or institutionally self-reinforcing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(gene_tr_t0, observed).
narrative_ontology:measurement(gene_tr_t15, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement_basis(gene_tr_t15, observed).
narrative_ontology:measurement(gene_tr_t30, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(gene_tr_t30, observed).
narrative_ontology:measurement(gene_tr_t45, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 45, 0.18).
narrative_ontology:measurement_basis(gene_tr_t45, observed).
narrative_ontology:measurement(gene_tr_t60, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement_basis(gene_tr_t60, observed).
narrative_ontology:measurement(gene_tr_t75, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 75, 0.22).
narrative_ontology:measurement_basis(gene_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(gene_be_t0, observed).
narrative_ontology:measurement(gene_be_t15, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 15, 0.2).
narrative_ontology:measurement_basis(gene_be_t15, observed).
narrative_ontology:measurement(gene_be_t30, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement_basis(gene_be_t30, observed).
narrative_ontology:measurement(gene_be_t45, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 45, 0.29).
narrative_ontology:measurement_basis(gene_be_t45, observed).
narrative_ontology:measurement(gene_be_t60, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 60, 0.3).
narrative_ontology:measurement_basis(gene_be_t60, observed).
narrative_ontology:measurement(gene_be_t75, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 75, 0.31).
narrative_ontology:measurement_basis(gene_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(gene_su_t0, observed).
narrative_ontology:measurement(gene_su_t15, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(gene_su_t15, observed).
narrative_ontology:measurement(gene_su_t30, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(gene_su_t30, observed).
narrative_ontology:measurement(gene_su_t45, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 45, 0.66).
narrative_ontology:measurement_basis(gene_su_t45, observed).
narrative_ontology:measurement(gene_su_t60, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement_basis(gene_su_t60, observed).
narrative_ontology:measurement(gene_su_t75, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement_basis(gene_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__humanitarian_ceiling_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__security_maximization_reading).

% DUAL FORMULATION NOTE:
% These three constraints are sibling readings of the geneva_conventions_1949 kernel, not three measurements of one constraint. Each reading produces a distinct epsilon, distinct beneficiary/victim structure, and distinct classification: the humanitarian_ceiling_reading (this story) computes as tangled_rope with moderate extraction (0.31) and high suppression (0.72) of security-necessity rationales; conditional_reciprocity_reading is expected to show lower suppression and a beneficiary structure contingent on adversary conduct; security_maximization_reading is expected to show high extraction from civilian/detainee populations with the state military recast as beneficiary rather than payer. The three should never be merged or averaged — per the ε-invariance principle, each is a structurally distinct constraint sharing only the treaty text as a common kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
