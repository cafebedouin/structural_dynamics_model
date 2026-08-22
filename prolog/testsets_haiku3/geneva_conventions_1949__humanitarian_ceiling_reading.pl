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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Geneva Conventions 1949 — Humanitarian Ceiling Reading
 *   domain: political/legal/humanitarian
 *
 * SUMMARY:
 *   The humanitarian ceiling reading of the 1949 Geneva Conventions asserts
 *   that state militaries are bound by absolute, non-negotiable humanitarian
 *   minimums regardless of adversary conduct, combatant status
 *   classification, or security threat. Civilians must not be targeted;
 *   detainees must receive medical care and humane treatment; irregular
 *   combatants retain baseline protections even without POW status. This
 *   reading suppresses security-maximization and conditional-reciprocity
 *   rationales: no circumstance — not asymmetric warfare, not WMD threat, not
 *   attacks on civilians by the adversary — lawfully permits suspension of
 *   the humanitarian floor. The reading is contested: military establishments
 *   and security doctrine communities argue necessity should permit
 *   flexibility; this reading rejects that logic entirely. The claim/metric
 *   gap is deliberate: extractiveness is moderate (0.38) because the reading
 *   imposes real costs on state militaries but does not eliminate their
 *   discretion in how they deploy force; suppression is high (0.71) because
 *   enforcement depends on actively suppressing security rationales and
 *   institutional pressure to reinterpret protections. Theater is moderate
 *   (0.42): humanitarian bodies perform monitoring, states perform compliance
 *   theater, and the actual constraint is routinely evaded in conflicts while
 *   formally maintained in law.
 *
 * KEY AGENTS:
 *   - state_militaries: institutional power, trapped exit, bear restraint burden
 *   - protected_civilians_and_detainees: powerless, trapped in theaters, beneficiaries of the floor
 *   - irregular_combatants: moderate power, constrained exit, gain protections even outside POW status — most contested provision
 *   - international_humanitarian_bodies: analytical/observer seat, interpret and monitor the ceiling
 *   - security_doctrine_establishments: excluded seat that contests the reading from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.38).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.71).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions 1949 — Humanitarian Ceiling Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "political/legal/humanitarian").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, '5f2fe608-908f-4968-8d25-4a3db30eaf96').
narrative_ontology:cs_kernel_codification('5f2fe608-908f-4968-8d25-4a3db30eaf96', formalized).
narrative_ontology:cs_authority_grounding('5f2fe608-908f-4968-8d25-4a3db30eaf96', lineage).
narrative_ontology:cs_interpretation_layer_present('5f2fe608-908f-4968-8d25-4a3db30eaf96').
narrative_ontology:cs_reading_relation('5f2fe608-908f-4968-8d25-4a3db30eaf96', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_reading_relation('5f2fe608-908f-4968-8d25-4a3db30eaf96', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('5f2fe608-908f-4968-8d25-4a3db30eaf96', foundational, humanitarian_protection_non_reciprocal).
narrative_ontology:cs_axiom_status(humanitarian_protection_non_reciprocal, holdable).
narrative_ontology:cs_axiom_grounding('5f2fe608-908f-4968-8d25-4a3db30eaf96', humanitarian_protection_non_reciprocal, deontological).
narrative_ontology:cs_axiom('5f2fe608-908f-4968-8d25-4a3db30eaf96', foundational, irregular_combatant_baseline_protection).
narrative_ontology:cs_axiom_status(irregular_combatant_baseline_protection, holdable).
narrative_ontology:cs_axiom_grounding('5f2fe608-908f-4968-8d25-4a3db30eaf96', irregular_combatant_baseline_protection, deontological).
narrative_ontology:cs_reference_frame('5f2fe608-908f-4968-8d25-4a3db30eaf96', universal_humanitarian_minimum).
narrative_ontology:cs_drift_state('5f2fe608-908f-4968-8d25-4a3db30eaf96', asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5f2fe608-908f-4968-8d25-4a3db30eaf96', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, protected_civilians).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, detainees_and_prisoners).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the operational framework within which the conventions are implemented (or evaded). Bear the heaviest compliance burden: must maintain POW treatment, medical care, and civilian protections regardless of adversary conduct. Face pressure from security doctrine and irregular opponents to suspend or reinterpret the constraint. Cannot exit without international sanction and institutional delegitimation.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries, agenda_setter,
    institutional, generational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries, payer).

% Protected by prohibitions on targeting, sexual violence, forced displacement, collective punishment, and requisitioning beyond necessity. In conflict zones, the protection is often nominal (enforcement weak, militaries incentivized to dismiss civilian status claims). Trapped in theater, no exit option available. Gain protection under this reading even when adversaries disregard it.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, protected_civilians, beneficiary,
    powerless, immediate, trapped, universal).

% Entitled to medical care, dignity, safe custody, and family contact regardless of status classification or adversary conduct. This reading extends protection even to irregular combatants who lack POW status — the most contested provision. State militaries argue irregular status voids protection; this reading asserts the humanitarian floor applies universally.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, detainees_and_prisoners, beneficiary,
    powerless, immediate, trapped, universal).

% Under this reading, retain baseline humanitarian protections (medical care, humane treatment, no torture) even without POW classification. This is the reading's highest-conflict provision: state militaries claim irregular status (unlawful combatancy, failure to distinguish) voids protections; this reading asserts absolute humanitarian minimums persist. Combatants can be detained but not tortured, executed, or abused.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_combatants, beneficiary,
    moderate, immediate, constrained, universal).

% ICRC, UN human rights mechanisms, and treaty bodies interpret and monitor compliance with the conventions under this reading. They document violations, issue advisory opinions, and provide institutional evidence of whether the humanitarian ceiling is being held or eroded. Their authority to name violations is derived from the reading itself.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, international_humanitarian_bodies, observer,
    organized, generational, analytical, universal).

% Military legal advisors and doctrine communities that argue necessity and asymmetric-threat rationales should permit suspension or reinterpretation of protections. They are structurally excluded from this reading's decision frame — the reading asserts the humanitarian floor is not negotiable on security grounds. They contest the reading from outside, proposing the conditional_reciprocity or security_maximization readings.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, security_doctrine_establishments, excluded,
    institutional, biographical, constrained, national).

% Bound by the conventions they ratified. Under this reading, the binding is absolute: they cannot condition protections on adversary conduct or invoke emergency as grounds for suspension. They administer the constraint through military command structure and legal review. Constrained exit: formal withdrawal from treaties is possible but carries diplomatic and reputational costs; de facto non-compliance is common but undeclared.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, signatory_states, agenda_setter,
    institutional, generational, constrained, national).

% Often cited (by security doctrine) as justification for degrading protections: when adversaries violate conventions, signatories argue they should be released from reciprocal obligation. This reading excludes that rationale — it asserts protections are independent of adversary conduct. Adversary violations do not trigger lawful retaliation or suspension under this reading; they remain violations themselves.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, adversary_states_and_irregular_forces, excluded,
    powerful, immediate, mobile, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__humanitarian_ceiling_reading, diffuse).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__humanitarian_ceiling_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared framework for humanitarian conduct in armed conflict that applies universally, regardless of whether all parties honor it. Solves the coordination problem: absent universal minimums, each state would rationalize escalation ('the enemy violates, so we must too') and protections would spiral downward. The ceiling establishes what no state should do, even unilaterally.
% TRANSFER_FUNCTION: Transfers constraints (limitations on state violence) away from state militaries and toward protected civilian and detainee populations. State militaries bear the burden of restraint; protected groups receive the benefit of protected status. Under this reading, the constraint is asymmetric by design: only state actors have the power to comply; non-state actors cannot owe the same compliance because they lack the institutional capacity.
% ABSENT_VOICES: Military commands advocating security-maximization rationales are structurally excluded — this reading's core premise suppresses their claim that security necessity overrides humanitarian minimums. Irregular combatants who oppose the POW status threshold are excluded from the decision frame (they benefit from the protection but do not seat in governance of what protections entail). Third parties and subsequent generations who would pay opportunity costs from military constraint are unrepresented.
% DISAPPEARANCE_RATIONALE: If this reading of the conventions vanished (replaced by conditional reciprocity or security maximization), state violence against civilians and detainees would escalate immediately. States would cite adversary violations to justify suspension of medical care, targeting shifts, interrogation methods, and detention without trial. The institutional floor that inhibits this escalation would collapse; humanitarian protections would become contingent, renegotiated, and progressively degraded in each conflict.
% FOUNDING_PROBLEM: Mid-20th-century recognition that warfare had become total, targeting entire populations; the problem was the systematic erosion of protections during WWII and earlier conflicts. The founding problem: absent universal, non-reciprocal minimums, states use adversary conduct to justify unlimited violence, and protections collapse entirely.
% FOUNDING_PROBLEM_CORROBORATION: Violations documented by ICRC and UN human rights bodies in every major conflict since 1949 show that enforcement is weak, but the problem remains live: when states invoke security rationales to degrade protections, humanitarian bodies cite the conventions' absolute language to resist the reinterpretation. Academic and activist testimony outside the military establishment (Amnesty International, Human Rights Watch, legal scholars) attests the problem is live because the assault on the humanitarian ceiling never stops.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__humanitarian_ceiling_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__humanitarian_ceiling_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The humanitarian ceiling reading extracts constraints on state violence (the burden falls on state militaries). Extractiveness plateaus around 0.38 because: (1) the constraint is real — it limits tactical choices and precludes certain weapons/methods; (2) but state militaries retain substantial discretion in prosecution (rules of engagement, targeting, detention practices) so the extraction is not total. Suppression is high (0.71) because: (1) enforcement requires actively suppressing the security-maximization argument (institutional pressure from doctrine communities, political leaders, military commands seeking flexibility); (2) the alternatives to this reading (conditional_reciprocity, security_maximization) are live, powerful, and constantly advocated; (3) maintaining the ceiling requires sustained institutional work (ICRC presence, monitoring, legal review, diplomatic pressure) to prevent drift. Theater is moderate (0.42) because: (1) humanitarian operations are real (medical care, detention protocols, investigations of violations); (2) but increasing share of reported compliance is performative (states announce commitment while violating in practice, ICRC documents violations but enforcement remains weak, legal review finds compliance while operations proceed differently). The measurement trajectory shows suppression stabilizing at 0.71 after reaching it by t=50 — the ceiling requires sustained effort but has settled into a maintenance regime; theater rises and plateaus (the performative component grows but then stabilizes around 0.42, suggesting the system has achieved a quasi-stable theatrical equilibrium).
 *
 * PERSPECTIVAL GAP:
 *   The state military seat and the protected-population seat should compute radically differently. From the state seat: the constraint imposes real restrictions on force projection, rules out certain tactics, and requires resource allocation to compliance (legal review, medical care infrastructure, detention management). From the protected-population seat: the constraint offers nominal protection that is often breached in practice, but the existence of the ceiling as a legal and institutional framework creates at least some accountability mechanism and prevents total abandonment of protections. From the security doctrine seat (excluded): the constraint is irrational overreach — it privileges enemy combatants and civilian shield populations over state security. The engine computes this divergence from structural data (power, exit options, roles); the reading does not adjudicate which seat's experience is correct, only that they diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries (institutional power, trapped exit, agenda-setter role) are the structural target: d → 0.9 (high target end). They bear the burden of implementing and defending the ceiling against operational pressure. Protected civilians and detainees (powerless, trapped, beneficiary role) are at the low d end (~0.1-0.15) — they collect protection without bearing cost. Irregular combatants (moderate power, constrained exit, beneficiary role) sit intermediate (~0.35-0.45) — they benefit from protection extension but also constrain their own tactical choices by accepting combatant status claims. International humanitarian bodies (analytical power, analytical exit, observer role) sit at d=0.5 (symmetric, neutral). Security doctrine establishments (excluded) do not receive a d value in this reading — their exclusion is the point. The reading's own logic suppresses their rationale, so deriving d for them would misrepresent the structural relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The humanitarian ceiling reading does NOT exhibit mandatrophy in the classical sense (founding problem solved, constraint persists). Instead, it exhibits a different pattern: founding problem (mid-20th century total war, systematic erosion of protections) remains LIVE, but institutional enforcement of the ceiling has plateaued. The constraint persists because the problem is live, but the Theater ratio rising to 0.42 signals that an increasing share of stated compliance is performative — states announce commitment while practice diverges. This is not mandatrophy (the mandate hasn't expired), but it IS degradation: the constraint's real-world force is diminishing even as its formal authority remains. The measurements support this: extractiveness plateaus (the ceiling holds as a legal matter but operational impact stabilizes), suppression plateaus (the effort to suppress security-maximization arguments remains constant), and theater rises then stabilizes (more performance, less real function). The constraint is not a piton yet (it still coordinates genuine humanitarian protections, and violations remain contested rather than accepted), but it is moving toward piton-state if theater continues rising and real enforcement capacity continues declining.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_capacity_legitimacy,
    'As enforcement of the humanitarian ceiling weakens (rising theater_ratio, low conviction rates, institutional inadequacy), does the ceiling''s legitimacy persist or erode?',
    'Monitor whether state compliance with conventions in new conflicts deteriorates (empirical path), or whether humanitarian bodies and activist pressure maintain resistance to reinterpretation (institutional path). Survey legal scholars and state legal advisors on whether weak enforcement shifts their compliance calculus.',
    'If legitimacy erodes with enforcement capacity, the constraint shifts toward piton (institutionally maintained performance, reduced real function). If legitimacy persists despite weak enforcement, the constraint remains anchored in principle even as practice diverges — a different failure mode (hypocrisy rather than abandonment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_legitimacy, empirical, 'Whether the humanitarian ceiling''s normative authority depends on enforcement capacity or stands independent.').

omega_variable(
    irregular_combatant_protection_scope,
    'What is the boundary of ''irregular combatant'' status? At what point does a non-state actor fall so far outside combat norms that humanitarian protections no longer apply under this reading?',
    'Test cases: tribunals and legal bodies classify actors at the margin (terrorist cells, militia groups, cybercombatants, drone operators) and establish where the ceiling''s protections extend. Compare across jurisdictions to identify whether consensus emerges or divergence persists.',
    'If the boundary is narrow and stable, the reading''s protection of irregular combatants is clear and the constraint holds as stated. If the boundary erodes (actors increasingly classified as outside the law), the ceiling effectively narrows and security-maximization rationales gain ground despite the reading''s formal absoluteness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irregular_combatant_protection_scope, conceptual, 'How the humanitarian ceiling defines the scope of protected persons.').

omega_variable(
    absolute_vs_contextual_interpretation,
    'Is the humanitarian ceiling truly non-negotiable, or does this reading''s own language contain implicit contextual limits (proportionality, military necessity, distinction doctrine) that permit state reinterpretation?',
    'Comparative jurisprudence: study how state legal commands and tribunals interpret the conventions. If nearly all invoked reinterpretations can be fit within proportionality/necessity doctrines, the ceiling is effectively contextual despite this reading''s absoluteness language.',
    'If the ceiling is effectively contextual, the conditional_reciprocity reading becomes observationally indistinguishable from this reading — enforcement apparatus produces similar results. If the ceiling is genuinely non-negotiable, violations stand as violations regardless of contextual invocation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolute_vs_contextual_interpretation, conceptual, 'Whether the humanitarian ceiling reading''s absoluteness is durable or subject to legitimate reinterpretation.').

omega_variable(
    reading_sibling_boundary,
    'This reading coexists with conditional_reciprocity and security_maximization readings. What institutional or normative event would constitute a genuine boundary between them — i.e., would cause this reading to foreclose one of its siblings?',
    'Trace the genealogy of when readings have shifted dominance historically (e.g., post-WWII dominance of humanitarian ceiling vs. post-9/11 drift toward security-maximization). Identify what triggered institutional/legal recalibration.',
    'If no boundary exists, the readings remain permanently coexistent, each claiming legitimacy depending on political moment and conflict type. If a boundary can be identified (e.g., UN General Assembly resolution, Rome Statute amendment), the structural possibility of foreclosure becomes clear and future policy can target it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_sibling_boundary, conceptual, 'Whether the readings represent permanently coexistent positions or whether structural events could force a winner.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(gene_tr_t12, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(gene_tr_t25, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement(gene_tr_t37, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 37, 0.38).
narrative_ontology:measurement(gene_tr_t50, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(gene_tr_t62, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 62, 0.43).
narrative_ontology:measurement(gene_tr_t75, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 75, 0.42).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(gene_be_t12, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 12, 0.31).
narrative_ontology:measurement(gene_be_t25, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 25, 0.35).
narrative_ontology:measurement(gene_be_t37, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 37, 0.37).
narrative_ontology:measurement(gene_be_t50, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(gene_be_t62, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 62, 0.39).
narrative_ontology:measurement(gene_be_t75, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 75, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gene_su_t12, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(gene_su_t25, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 25, 0.66).
narrative_ontology:measurement(gene_su_t37, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 37, 0.7).
narrative_ontology:measurement(gene_su_t50, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(gene_su_t62, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 62, 0.71).
narrative_ontology:measurement(gene_su_t75, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 75, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__humanitarian_ceiling_reading, 0.18).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__security_maximization_reading).

% DUAL FORMULATION NOTE:
% The 1949 Geneva Conventions are a contested kernel with three structurally distinct readings: humanitarian_ceiling (this story), conditional_reciprocity, and security_maximization. Each reading instantiates different ε, different beneficiary/victim sets, and different mechanisms for handling adversary non-compliance. They are not perspectives on the same constraint; they are three separate constraints that compete over authority to interpret the same formalized text. This reading asserts absolute humanitarian minimums regardless of adversary conduct. Sibling readings deny that absoluteness by conditioning compliance on reciprocity (conditional_reciprocity) or necessity (security_maximization). All three are live positions; none has been formally foreclosed within the international legal system, though their relative dominance shifts across conflicts and historical periods.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__humanitarian_ceiling_reading, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
