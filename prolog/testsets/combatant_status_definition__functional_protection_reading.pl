% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__functional_protection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__functional_protection_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: combatant_status_definition__functional_protection_reading
 *   human_readable: Common Article 3 Minimum Protections Regardless of Combatant Status
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   International humanitarian law governing armed conflicts recognizes three
 *   distinct readings of how combatant status determines detention
 *   protections. This constraint story instantiates the FUNCTIONAL PROTECTION
 *   READING: a reading that decouples minimum humane treatment from formal
 *   combatant classification. Under this reading, all detained persons
 *   receive Common Article 3 baseline protections (prohibition of torture,
 *   cruel treatment, humiliation, and arbitrary killing; access to medical
 *   care; fair trial guarantees) IMMEDIATELY UPON DETENTION, regardless of
 *   whether their combatant status has been determined, established, or
 *   contested. The functional reading prioritizes protection floors over
 *   status gates. This reading does NOT answer whether non-state armed groups
 *   can BE combatants (the state-centric reading says no; the
 *   national-liberation reading says yes under specific conditions). It
 *   answers only whether detention authority can postpone minimum protections
 *   pending status determination — the functional reading says no. Status
 *   ambiguity does not suspend Article 3. Extractiveness is low (0.18)
 *   because the constraint creates no asymmetric transfer — it mandates equal
 *   baseline treatment, not distribution of goods. Suppression is low (0.22)
 *   because the constraint's enforcement depends on state compliance with
 *   obligations they have voluntarily undertaken in treaties; there is
 *   minimal active coercion required to hold it. Theater is very low (0.08)
 *   because the constraint's function is transparent and verifiable (either
 *   humane treatment occurs or it does not). Measurement drift is minimal
 *   over the 40-year interval, indicating the constraint's function is
 *   stable; slight uptick in suppression reflects increasing ICC monitoring
 *   and state-practice pressure around detention protocols.
 *
 * KEY AGENTS:
 *   - detained_persons_universal (beneficiary, trappped exit): all persons deprived of liberty in armed conflict, regardless of classification; receive immediate Article 3 protections without precondition
 *   - detaining_states (payer/agenda-setter, institutional power): maintain obligation to provide baseline protections but lose discretionary power to classify first and protect based on status; must invest in immediate-protection protocols
 *   - international_humanitarian_legal_system (beneficiary, institutional): gains coherence and predictability from universal baseline; reduces status litigation and classification delays
 *   - national_military_command (agenda-setter, institutional): implements Article 3 protocols and faces operational cost of classifying/identifying combatants AFTER extending protections rather than conditioning protection on classification
 *   - international_committee_red_cross_icrc (observer/monitor, institutional): gains access rights to all detained persons under Article 3; monitors implementation; provides independent verification
 *   - state_centric_reading_adherents (excluded): would argue status determination is prerequisite for protection level; are excluded from the functional reading's mandate-setting process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.18).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.22).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, rope).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Common Article 3 Minimum Protections Regardless of Combatant Status").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__functional_protection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, '1d2355f2-a293-4b45-ab32-2a6dbd931a87').
narrative_ontology:cs_kernel_codification('1d2355f2-a293-4b45-ab32-2a6dbd931a87', formalized).
narrative_ontology:cs_authority_grounding('1d2355f2-a293-4b45-ab32-2a6dbd931a87', lineage).
narrative_ontology:cs_interpretation_layer_present('1d2355f2-a293-4b45-ab32-2a6dbd931a87').
narrative_ontology:cs_reading_relation('1d2355f2-a293-4b45-ab32-2a6dbd931a87', combatant_status_definition__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d2355f2-a293-4b45-ab32-2a6dbd931a87', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_axiom('1d2355f2-a293-4b45-ab32-2a6dbd931a87', foundational, human_dignity_precedes_status_classification).
narrative_ontology:cs_axiom_status(human_dignity_precedes_status_classification, holdable).
narrative_ontology:cs_axiom_grounding('1d2355f2-a293-4b45-ab32-2a6dbd931a87', human_dignity_precedes_status_classification, deontological).
narrative_ontology:cs_axiom('1d2355f2-a293-4b45-ab32-2a6dbd931a87', foundational, detention_protection_status_independence).
narrative_ontology:cs_axiom_status(detention_protection_status_independence, holdable).
narrative_ontology:cs_axiom_grounding('1d2355f2-a293-4b45-ab32-2a6dbd931a87', detention_protection_status_independence, deontological).
narrative_ontology:cs_reference_frame('1d2355f2-a293-4b45-ab32-2a6dbd931a87', universal_baseline_protection_framework).
narrative_ontology:cs_drift_state('1d2355f2-a293-4b45-ab32-2a6dbd931a87', post_rome_statute_enforcement_era, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('1d2355f2-a293-4b45-ab32-2a6dbd931a87', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, detained_persons_universal).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, international_humanitarian_legal_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, detaining_states).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, human_dignity_universal).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, protection_precedes_classification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All persons deprived of liberty in armed conflict (captured combatants, suspected combatants, civilians mistaken for combatants, unclassified detainees). Under the functional reading, they receive immediate access to medical care, prohibition on torture, humane treatment, and fair trial rights — without waiting for status determination. Trapped by the fact of detention; their only exit is release or status clarification, neither of which the functional reading controls. Their power is minimal; they depend on humanitarian protections for survival. Time horizon is biographical — their situation is acute during armed conflict, often years or decades in unresolved cases.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, detained_persons_universal, beneficiary,
    powerless, biographical, trapped, global).

% States that conduct detentions in armed conflict. They set and enforce detention policy; they voluntarily undertake Geneva Convention obligations, including Article 3. The functional reading requires them to implement Article 3 protections immediately, regardless of status determination timelines. They bear the compliance cost: training personnel, monitoring conditions, allowing ICRC access, documenting detention decisions. They cannot exit the functional reading's obligations without violating treaty commitments and facing ICC prosecution for command responsibility. Their power is institutional (they write the rules), but constrained by international law's formal structure and ICC jurisdiction.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, detaining_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__functional_protection_reading, detaining_states, payer).

% The ICRC monitors detention conditions and verifies Article 3 compliance. The functional reading automatically triggers ICRC access rights — the ICRC gains leverage to enter detention facilities and interview detainees because the functional reading makes baseline protections a universal obligation, not a status-contingent one. The ICRC observes state compliance and reports violations to the humanitarian system and ICC. Mobile exit: the ICRC's mandate is self-imposed, and it could theoretically withdraw from monitoring specific conflicts, though reputational cost makes this rare.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, international_committee_red_cross_icrc, observer,
    organized, generational, mobile, global).

% The corpus of treaty law, case law, and customary international law governing armed conflict protections. The functional reading strengthens this system's internal coherence by removing a major source of litigation: whether detained persons can be denied Article 3 protections pending status determination. The system benefits from the clarity and predictability the functional reading provides. As a doctrine rather than an agent, it collects rents (legitimacy, institutional authority) from the functional reading's adoption, which is why it appears in beneficiaries and vindicated_propositions.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, international_humanitarian_legal_system, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(combatant_status_definition__functional_protection_reading, international_humanitarian_legal_system).

% States and military authorities that hold the state-centric reading (combatant status requires formal state military organization; non-state actors are not entitled to combatant status). These actors would argue that the functional reading's baseline protections apply to all detainees, they lose the leverage to condition stronger protections on formal status recognition. They are excluded from the functional reading's mandate-setting process — they would object that immediate, unconditional Article 3 protections for non-state actors blur the status distinction and reduce incentives to organize formal state militaries. Their exclusion is structural: the functional reading's adoption means the legal system has chosen the functional frame over the state-centric frame for detention protections, at least at the baseline level.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, state_centric_reading_adherents, excluded,
    institutional, generational, constrained, global).

% Military commanders implementing detention policy at the operational level. They must classify combatants and manage detention facilities. The functional reading requires them to provide Article 3 protections at point of capture, before classification is complete. This changes operational procedure: they must allocate resources to immediate-protection protocols (medical care access, interrogation safeguards, documentation) in parallel with classification, rather than sequentially. They are constrained by the functional reading's mandate and by threatened ICC prosecution for command responsibility if violations occur. Their power is operational but institutionally mediated through state policy.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, national_military_command, agenda_setter,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__functional_protection_reading, diffuse).
narrative_ontology:fixing_cost_class(combatant_status_definition__functional_protection_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal baseline of humane treatment and basic procedural rights for all detained persons in armed conflict, preventing status ambiguity from being used as justification for torture, summary killing, or denial of medical care. Solves the coordination problem: without the baseline, detaining states have incentive to delay status determination to avoid triggering higher protections; the functional reading removes that incentive by applying the floor immediately. Also coordinates the international humanitarian system around a common standard that eliminates status-shopping (forum selection to avoid protections).
% TRANSFER_FUNCTION: Transfers compliance costs from detained persons (who would otherwise bear torture, humiliation, denial of medical care pending status determination) to detaining states (which bear the cost of implementing Article 3 protections immediately). The transfer is not a zero-sum extraction — it redistributes authority such that detaining states lose the discretionary power to deny baseline standards, but gain clarity on their obligations. Humanitarian actors (ICRC) gain access and monitoring rights.
% ABSENT_VOICES: State military commands in ongoing conflicts often resist immediate protection mandates, arguing they slow operations and complicate classification. Their resistance is structural (excluded from the mandate-setting table once the functional reading is adopted by the international legal system) rather than voiced and incorporated. Detained persons themselves are trapped and cannot voice counterarguments; humanitarian advocates speak for their interests. Non-state armed groups that want combatant status (national-liberation reading adherents) are also excluded from the functional reading's mandate-setting, though the functional reading does not foreclose their status claim — it only mandates baseline protections regardless of status outcome.
% DISAPPEARANCE_RATIONALE: If the functional reading disappeared — if the international legal system reverted to the state-centric reading where status determination is a precondition for Article 3 protections — detention practices would shift dramatically. Detaining states would delay status decisions to avoid triggering higher protections; torture and coercive interrogation would increase; ICRC access would become conditional on status determination; humanitarian advocacy would lose its normative anchor. Detained persons would face immediate risk of torture pending classification. The functional reading's disappearance would rearrange the incentive structure such that status ambiguity becomes an asset (for states avoiding higher protections) rather than a problem (under the functional reading, a violation of the baseline).
% FOUNDING_PROBLEM: Armed conflicts create situations where combatant status is genuinely ambiguous at the moment of detention: fighters captured in civilian areas, persons with dual roles (part-time militias), individuals whose allegiances are unclear. Historically, detaining authorities exploited this ambiguity, denying protections to detainees under the guise that status was not yet determined. Torture, coercive interrogation, and summary execution were justified as preconditioned on status determination. The functional reading was developed to block this justification: status ambiguity cannot suspend baseline humanity.
% FOUNDING_PROBLEM_CORROBORATION: Amnesty International, Human Rights Watch, and UN fact-finding missions in Syria (2016–2020), Yemen (ongoing), and Ethiopia (2021–2023) document that detaining authorities regularly invoke status ambiguity as justification for torture, denial of medical care, and coercive interrogation. ICRC field reports and International Criminal Court case law (Kunarac, Akayesu, Al-Mahdi) establish that detaining states continue to exploit status ambiguity to avoid baseline protections. These sources are external to the beneficiaries of the functional reading (ICRC and the humanitarian system); they represent empirical observation from independent monitors and prosecutors. The founding problem is corroborated as live — status ambiguity is still used to justify abuse.
narrative_ontology:disappearance_verdict(combatant_status_definition__functional_protection_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__functional_protection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__functional_protection_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(combatant_status_definition__functional_protection_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__functional_protection_reading_tests).
:- end_tests(combatant_status_definition__functional_protection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because the functional reading removes a gate (status-conditional protection) rather than imposing a transfer. No party 'collects' from the universal baseline; instead, the reading redistributes authority: status-determination power moves from detaining authority (who could use status ambiguity to justify harsh treatment) to the humanitarian system (which mandates treatment regardless). Suppression is low (0.22) — article 3 protections are voluntary treaty commitments, not externally imposed coercion. The suppression value reflects the effort required to enforce baseline standards against actors who defect, not the effort to impose an unwanted condition. Theater is very low (0.08) because compliance with 'no torture,' 'access to medical care,' and 'fair trial' is objectively verifiable by ICRC monitors and post-conflict tribunals; there is minimal scope for performative compliance. The measurement series shows stability because the functional reading's core claim (universal baseline) has been relatively durable internationally since the 1949 Geneva Conventions and reinforced by Rome Statute. Slight uptick in suppression after year 32 reflects post-ICC era state practice, where the threat of prosecution has created higher compliance burden. The readings DO coexist with the state-centric and national-liberation readings — this story makes no claim about WHO can be a combatant, only about what BASELINE protections apply to detained persons. The three readings form a constraint family related through the combatant_status_definition kernel: state-centric reading answers 'who qualifies as combatant' via formal criteria; national-liberation reading answers the same question via command-and-control criteria; functional reading answers 'what minimum applies to detained persons' independently of status clarity.
 *
 * PERSPECTIVAL GAP:
 *   State military commanders experience this reading as imposing a compliance cost — they must allocate resources to verify detention conditions and manage status classification in parallel with protection delivery, rather than sequentially (classify, then protect per tier). They may compute the constraint as moderately extractive due to operational overhead. Detained persons and the humanitarian community experience it as protective, non-extractive. The ICRC experiences it as enabling its access mandate — Article 3 automatically triggers ICRC monitoring rights. This perspectival gap should resolve in the engine's per-seat computation: a state institutional seat with constrained exit (cannot withdraw from Geneva Conventions without reputational cost) will show higher d (~0.4–0.5) than a detained-persons seat (d near 0.0, pure beneficiary), reflecting their opposed structural positions on the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The functional reading's beneficiary set is universal detained persons (all power levels, trapped or identity-locked exit) — they receive baseline protections without paying for them. The reading also vindicates the proposition 'human_dignity_universal' (a doctrine, not an agent, compiled to vindicated_propositions, not beneficiaries). Detaining states are the payers — they bear the compliance cost of ensuring Article 3 standards. No 'victims' are declared because the reading does not impose asymmetric harm; it removes a harm-enabling gate (status-conditional protection). Status ambiguity no longer suspends protections, so states cannot use classification delays as cover for abuse. The reading has no identified victims because all parties either benefit (detained persons) or bear legitimate compliance costs (states), not exploitation. Directionality should derive automatically: beneficiaries (detained persons, all power levels) → d near 0.0; payer (detaining states, institutional power) → d around 0.4–0.5 (cost imposed but no asymmetric extraction, legitimate duty). No overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The functional reading avoids false Rope/Tangled Rope collapse by distinguishing protection (what the reading guarantees) from status (what the reading does not address). The founding problem — arbitrary classification used to justify torture — is LIVE: contemporary armed conflicts (Yemen, Syria, Ukraine, Ethiopia) show detaining authorities still invoking status ambiguity to justify coercive interrogation and harsh detention. The functional reading persists because the mandate it addresses has not been solved. However, there is a secondary mandatrophy risk: if status ambiguity becomes actually clarified quickly via modern biometric / intelligence systems, the functional reading's main utility (protecting against classification delays) diminishes. This risk is captured in omega status_determination_precondition_ambiguity: if states develop reliable instant-classification protocols, the functional reading may devolve from genuine coordination (baseline protection is necessary because status delays are inevitable) to theater (baseline protection is symbolic, classification is instant). Measurement data would show rising theater_ratio if this occurred. The 40-year interval shows no such rise, indicating the founding problem (classification delays exploited for abuse) remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    status_determination_precondition_ambiguity,
    'Can a detention authority invoke status uncertainty as justification for withholding Article 3 protections pending classification, or does the functional reading require minimum protections to apply immediately upon detention regardless of status clarity?',
    'International Criminal Court jurisprudence on temporal scope of Article 3 obligations; state practice in conflict zones regarding immediate protection protocols at point of capture; treaty body general comments clarifying Article 3 applicability thresholds.',
    'If immediate application is required, the functional reading forecloses postponement-pending-classification and converts status ambiguity into a structural obligation to protect first. If status determination can precede protection, the readings coexist with different state practice. This omega directly determines whether the functional reading''s core premise (status-independent baseline) actually constrains state action or merely restates aspirational language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_determination_precondition_ambiguity, empirical, 'Temporal sequence: does status determination precede or follow minimum protection obligation?').

omega_variable(
    state_centric_authority_persistence,
    'Does the state-centric reading retain legitimacy authority in jurisdictions that ratified Additional Protocol II (AP II, extending protections to non-international armed conflicts) and later the Rome Statute (establishing universal jurisdiction), or has the functional reading''s axiom of universal human dignity overridden the foundational state-monopoly premise?',
    'Analysis of treaty succession and interpretation: did states that adopted AP II and Rome Statute formally abandon the state-monopoly premise, or merely layered non-state protections alongside it? Comparative constitutional jurisprudence from national courts interpreting IHL domestically.',
    'If the state-centric axiom remains overridden (not just supplemented), the functional reading has foreclosed the state-centric reading within the legal system that adopted AP II/Rome Statute. If both premises coexist (states retain combatant-status gatekeeping while non-state fighters get parallel protections), the readings truly coexist rather than the functional reading foreclosing. The classification matters for understanding whether the kernel genuinely permits multiple readings or whether formal law has settled to functional dominance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_centric_authority_persistence, empirical, 'Has the state-monopoly premise been overridden by treaty succession or merely supplemented?').

omega_variable(
    national_liberation_boundary_case,
    'Does the functional reading''s logic (minimum protections apply to detained persons regardless of formal status) extend to non-state armed groups fighting colonial, occupation, or racist regimes under AP I Article 1(4), or does the functional reading remain neutral on whether combatant status is granted to non-state fighters at all?',
    'Close reading of the functional reading''s foundational axioms: if ''universal dignity + immediate protection'' is the core, it says nothing about WHETHER non-state actors can BE combatants (that is the state-centric / national-liberation distinction). If the functional reading is only about BASELINE protections for whoever is detained, it coexists with both the national-liberation and state-centric readings on the status question itself.',
    'This omega clarifies scope: the functional reading may not actually resolve the status question but only the protection question. If so, the functional reading does not foreclose the national-liberation reading — it merely requires that even if national-liberation combatant status is granted, minimum protections apply. This omega determines whether the functional reading is genuinely a third way or a narrower constraint focused only on protection floors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_liberation_boundary_case, conceptual, 'Does the functional reading''s axiom address status eligibility (who can be combatants) or only protection floors (what minimum applies once detained)?').

omega_variable(
    enforcement_mechanism_authority_ambiguity,
    'Who enforces the functional reading''s baseline protections — the detaining authority itself (self-regulation), International Committee of the Red Cross (ICRC) oversight), state peer review (reciprocity machinery), or International Criminal Court prosecution (accountability after the fact)? Does the reading specify an enforcement posture, or is enforcement authority contestable between readings?',
    'Historical practice in armed conflicts: which enforcement mechanism has been most effective at securing Article 3 compliance? Comparative analysis of enforcement under the state-centric reading (state reciprocity + ICRC access) versus functional reading (immediate duty + independent oversight). Empirical data on detention conditions before and after Rome Statute conferral of ICC jurisdiction.',
    'If the functional reading requires real-time independent verification (ICRC access, neutral inspectors), it creates higher enforcement costs than the state-centric reading''s reciprocity model. This cost asymmetry may determine whether the functional reading''s universality is durable or becomes theater (high theater_ratio). The enforcement mechanism choice reshapes the constraint''s extractiveness profile: self-reporting compliance is cheaper but unreliable; independent monitoring is more costly but more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_authority_ambiguity, empirical, 'Which enforcement mechanism realizes the functional reading''s protections?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t0, combatant_status_definition__functional_protection_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(comb_tr_t0, observed).
narrative_ontology:measurement(comb_tr_t8, combatant_status_definition__functional_protection_reading, theater_ratio, 8, 0.06).
narrative_ontology:measurement_basis(comb_tr_t8, observed).
narrative_ontology:measurement(comb_tr_t16, combatant_status_definition__functional_protection_reading, theater_ratio, 16, 0.07).
narrative_ontology:measurement_basis(comb_tr_t16, observed).
narrative_ontology:measurement(comb_tr_t24, combatant_status_definition__functional_protection_reading, theater_ratio, 24, 0.08).
narrative_ontology:measurement_basis(comb_tr_t24, observed).
narrative_ontology:measurement(comb_tr_t32, combatant_status_definition__functional_protection_reading, theater_ratio, 32, 0.09).
narrative_ontology:measurement_basis(comb_tr_t32, observed).
narrative_ontology:measurement(comb_tr_t40, combatant_status_definition__functional_protection_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(comb_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comb_be_t0, combatant_status_definition__functional_protection_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement_basis(comb_be_t0, observed).
narrative_ontology:measurement(comb_be_t8, combatant_status_definition__functional_protection_reading, base_extractiveness, 8, 0.17).
narrative_ontology:measurement_basis(comb_be_t8, observed).
narrative_ontology:measurement(comb_be_t16, combatant_status_definition__functional_protection_reading, base_extractiveness, 16, 0.18).
narrative_ontology:measurement_basis(comb_be_t16, observed).
narrative_ontology:measurement(comb_be_t24, combatant_status_definition__functional_protection_reading, base_extractiveness, 24, 0.19).
narrative_ontology:measurement_basis(comb_be_t24, observed).
narrative_ontology:measurement(comb_be_t32, combatant_status_definition__functional_protection_reading, base_extractiveness, 32, 0.2).
narrative_ontology:measurement_basis(comb_be_t32, observed).
narrative_ontology:measurement(comb_be_t40, combatant_status_definition__functional_protection_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement_basis(comb_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t0, combatant_status_definition__functional_protection_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(comb_su_t0, observed).
narrative_ontology:measurement(comb_su_t8, combatant_status_definition__functional_protection_reading, suppression_requirement, 8, 0.21).
narrative_ontology:measurement_basis(comb_su_t8, observed).
narrative_ontology:measurement(comb_su_t16, combatant_status_definition__functional_protection_reading, suppression_requirement, 16, 0.22).
narrative_ontology:measurement_basis(comb_su_t16, observed).
narrative_ontology:measurement(comb_su_t24, combatant_status_definition__functional_protection_reading, suppression_requirement, 24, 0.23).
narrative_ontology:measurement_basis(comb_su_t24, observed).
narrative_ontology:measurement(comb_su_t32, combatant_status_definition__functional_protection_reading, suppression_requirement, 32, 0.24).
narrative_ontology:measurement_basis(comb_su_t32, observed).
narrative_ontology:measurement(comb_su_t40, combatant_status_definition__functional_protection_reading, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(comb_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__functional_protection_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(combatant_status_definition__functional_protection_reading, 0.12).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__national_liberation_reading).

% DUAL FORMULATION NOTE:
% The combatant_status_definition kernel family comprises three constraint stories: state_centric_reading (combatant status requires state military organization), national_liberation_reading (status extends to non-state fighters meeting command-control criteria), and functional_protection_reading (minimum protections apply regardless of status clarity). Each story has distinct ε, beneficiary/victim structure, and claimed type. They are linked by network.affects_constraints because the legal system holds all three readings simultaneously in tension; the functional reading's adoption in Rome Statute influences how states interpret the state-centric and national-liberation readings' scope. The readings form a constraint family because their core dispute (what combatant status is, and what it determines) is a single kernel, but their structural answers are incompatible within a single constraint story — hence three separate stories with ε-invariant metrics specific to each reading's functional claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
