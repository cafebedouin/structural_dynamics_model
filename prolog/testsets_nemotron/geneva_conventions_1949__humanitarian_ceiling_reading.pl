% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   domain: international_law/armed_conflict
 *
 * SUMMARY:
 *   This reading of the 1949 Geneva Conventions instantiates the
 *   'humanitarian ceiling' interpretation: the Conventions establish absolute
 *   humanitarian minimums that bind states regardless of adversary conduct,
 *   reciprocity, or operational convenience. The reading holds that Common
 *   Article 3, the grave breaches regime, and the fundamental guarantees of
 *   the Fourth Convention create a non-derogable floor of protection for
 *   civilians, detainees, and persons hors de combat — including irregular
 *   fighters who do not qualify for POW status. State militaries and command
 *   structures bear the asymmetric burden of compliance even when facing
 *   adversaries who systematically violate the same norms. The constraint
 *   operates as a tangled rope: it solves a genuine coordination problem
 *   (limiting the horrors of war, enabling humanitarian access, providing a
 *   shared normative language for accountability) while extracting
 *   substantial compliance costs from state security apparatuses and
 *   suppressing security-rationality arguments that would justify relaxing
 *   protections. The extractiveness measured here is the structural cost of
 *   maintaining this ceiling against the persistent pressure of asymmetric
 *   warfare, counterinsurgency, and terrorism — not the reading's normative
 *   aspiration.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.38).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.72).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions 1949 — Humanitarian Ceiling Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "international_law/armed_conflict").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, '171ea97a-e346-4a0e-a5d5-a1512e05b76d').
narrative_ontology:cs_kernel_codification('171ea97a-e346-4a0e-a5d5-a1512e05b76d', formalized).
narrative_ontology:cs_authority_grounding('171ea97a-e346-4a0e-a5d5-a1512e05b76d', lineage).
narrative_ontology:cs_interpretation_layer_present('171ea97a-e346-4a0e-a5d5-a1512e05b76d').
narrative_ontology:cs_reading_relation('171ea97a-e346-4a0e-a5d5-a1512e05b76d', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_reading_relation('171ea97a-e346-4a0e-a5d5-a1512e05b76d', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('171ea97a-e346-4a0e-a5d5-a1512e05b76d', foundational, humanitarian_floor_non_derogable).
narrative_ontology:cs_axiom_status(humanitarian_floor_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('171ea97a-e346-4a0e-a5d5-a1512e05b76d', humanitarian_floor_non_derogable, deontological).
narrative_ontology:cs_axiom('171ea97a-e346-4a0e-a5d5-a1512e05b76d', foundational, protection_independent_of_reciprocity).
narrative_ontology:cs_axiom_status(protection_independent_of_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('171ea97a-e346-4a0e-a5d5-a1512e05b76d', protection_independent_of_reciprocity, deontological).
narrative_ontology:cs_axiom('171ea97a-e346-4a0e-a5d5-a1512e05b76d', secondary, common_article_3_as_minimum_standard).
narrative_ontology:cs_axiom_status(common_article_3_as_minimum_standard, holdable).
narrative_ontology:cs_axiom_grounding('171ea97a-e346-4a0e-a5d5-a1512e05b76d', common_article_3_as_minimum_standard, conventional).
narrative_ontology:cs_reference_frame('171ea97a-e346-4a0e-a5d5-a1512e05b76d', id_1949_convention_text_as_humanitarian_ceiling).
narrative_ontology:cs_drift_state('171ea97a-e346-4a0e-a5d5-a1512e05b76d', post_911_asymmetric_conflict_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('171ea97a-e346-4a0e-a5d5-a1512e05b76d', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, detained_persons).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_combatants).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, medical_personnel).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, humanitarian_organizations).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, command_structures).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, national_security_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Protected from direct attack, collective punishment, displacement, and starvation as a method of warfare. They do not choose this protection — it is imposed on belligerents. In asymmetric conflict, they are often the primary terrain of war; their 'exit' from the constraint's effects is impossible (they cannot opt out of being civilians). The constraint subsidizes their survival.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations, beneficiary,
    powerless, biographical, trapped, global).

% Includes POWs, civilian internees, and detainees in non-international conflict. The constraint mandates humane treatment, judicial guarantees, ICRC access, and protection from torture and summary execution. Their exit from the constraint's protection is structurally impossible — they are in the detaining power's physical control. The constraint is their only structural shield.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, detained_persons, beneficiary,
    powerless, biographical, trapped, global).

% Fighters who do not meet GC III Art. 4 criteria (no fixed distinctive sign, no open carrying of arms, no responsible command). Under this reading, they retain Common Article 3 protections and fundamental guarantees regardless of status. They cannot exit the constraint's protection (it attaches to their person), but they also cannot claim its full benefits (no POW status). They occupy the structural border where the ceiling's claim is most contested.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_combatants, beneficiary,
    powerless, biographical, trapped, global).

% Protected in their medical functions; entitled to operate without interference. Their protection is conditional on exclusively medical duties — a structural limit. They can exit by ceasing medical work, but professional identity and ethical obligation make exit costly. The constraint enables their operational space but does not guarantee safety in asymmetric conflict where medical markers are targeted.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, medical_personnel, beneficiary,
    moderate, biographical, constrained, global).

% ICRC and impartial humanitarian bodies gain access rights, protected status for personnel/transports, and a legal basis for negotiation with belligerents. They are institutional beneficiaries — the constraint creates their operational mandate. Exit means withdrawing from conflict zones, which abandons their mandate. They bear reputational and operational costs when the ceiling is violated but lack enforcement power.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, humanitarian_organizations, beneficiary,
    organized, generational, constrained, global).

% Bear the compliance burden: legal training, rules of engagement, targeting review, detention infrastructure, investigation/prosecution of violations, strategic foreclosure of tactics (area bombardment, hostage-taking, reprisals against protected persons). In asymmetric conflict, they fight adversaries who systematically violate the same norms without reciprocal cost. They cannot exit the constraint (treaty obligation, domestic law, international legitimacy) but can mitigate through legal interpretation, operational workarounds, and non-compliance. The constraint extracts their operational freedom and resources.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries, payer,
    institutional, generational, constrained, global).

% Senior military and civilian leadership bear command responsibility for violations. The constraint imposes personal legal liability (ICC, universal jurisdiction), political risk, and the burden of enforcing compliance down the chain. They are the primary extraction point — the constraint's suppression targets their security-rationality decision calculus. Exit means resignation or regime change; constrained by institutional role and personal liability.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, command_structures, payer,
    powerful, biographical, constrained, global).

% Intelligence agencies, special operations, and security bureaucracies face the sharpest extraction: the constraint forbids torture, enforced disappearance, secret detention, and extrajudicial killing — tools they view as essential in asymmetric conflict. They bear the cost of foregone intelligence, operational complexity, and legal exposure. Their exit is constrained by democratic oversight (where it exists) and international law; in authoritarian contexts, they are the constraint's primary active resistance.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, national_security_apparatus, payer,
    institutional, generational, constrained, national).

% States, legal advisers, and scholars who argue the Conventions' protections are reciprocal and degradable upon adversary non-compliance. They are excluded from this reading's framework — their position is treated as legally foreclosed by the humanitarian ceiling. They would object that the reading imposes unilateral compliance on states facing existential asymmetric threats. They operate in parallel legal/policy spaces (military manuals, national legislation, diplomatic positions).
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, conditional_reciprocity_advocates, excluded,
    organized, biographical, mobile, global).

% Military establishments, intelligence communities, and realist scholars who argue Conventions must yield to operational necessity in asymmetric conflict. They are excluded from this reading — their position is treated as legally and morally illegitimate. They would argue the ceiling reading gets soldiers killed and wars lost. They exercise influence through classified legal opinions, operational practice, and political pressure on civilian leadership.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, security_maximization_advocates, excluded,
    institutional, biographical, mobile, global).

% ICTY, ICTR, ICC, and hybrid courts interpret and apply the Conventions. They adjudicate between the readings in concrete cases — their jurisprudence shapes which reading becomes operationally dominant. They are analytical observers of the kernel's instantiation, not parties to the contest, though their rulings structurally favor the humanitarian ceiling reading's expansive protections.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, international_tribunals, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared normative floor that limits the horrors of war, enables humanitarian access and negotiation, creates a common language for accountability, and coordinates state behavior around minimum standards of humanity — even (especially) when adversaries violate those standards.
% TRANSFER_FUNCTION: Transfers operational freedom, tactical options, and resource allocation from state militaries and security apparatuses to protected persons (civilians, detainees, irregular combatants, medical personnel) and humanitarian organizations. The transfer is asymmetric: states pay regardless of adversary compliance.
% ABSENT_VOICES: The conditional reciprocity and security maximization readings' proponents — military legal advisers who argue for reciprocity-based degradation, intelligence agencies that view the ceiling as operationally suicidal, and states facing existential asymmetric threats — are structurally excluded from this reading's framework. Their objections are treated as legally foreclosed, not as legitimate interpretive positions.
% DISAPPEARANCE_RATIONALE: If the humanitarian ceiling vanished overnight, states in asymmetric conflict would immediately expand targeting, detention, and interrogation practices toward security-maximization norms. Civilian casualties would rise, detainee protections would collapse, humanitarian access would be denied, and the legal basis for war crimes prosecution would erode. The world would rearrange toward the security-maximization reading's logic.
% FOUNDING_PROBLEM: The 1949 Conventions were built to solve the horror of total war: uncontrolled targeting of civilians, starvation as policy, torture and summary execution of prisoners, and the absence of any legal floor for humanitarian protection in both international and (via Common Article 3) non-international conflict.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC and humanitarian organizations attest the founding problem remains live — civilian harm, detainee abuse, and asymmetric warfare persist. Military establishments and security-maximization advocates attest the problem has mutated: non-state actors who reject IHL's premises make the original solution operationally suicidal. Conditional reciprocity advocates attest the problem is solvable only through mutual restraint. No single corroborating source outside the beneficiary set commands consensus.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__humanitarian_ceiling_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__humanitarian_ceiling_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.38) reflects the substantial but bounded compliance burden on state militaries: training, legal review, detention infrastructure, targeting restrictions, and the strategic foreclosure of certain operational options. Suppression (0.72) is high because the reading actively forecloses security-necessity defenses — the constraint's persistence depends on suppressing the argument that 'the enemy does worse' or 'operational necessity requires it.' Theater ratio (0.28) captures the gap between formal compliance rituals (legal advisers in targeting cells, detention review boards) and the constraint's actual bite in high-intensity asymmetric conflict — some compliance activity is performative signaling to domestic and international audiences. Accessibility collapse (0.42) is moderate: alternatives (reciprocity-based frameworks, security-maximization doctrines) remain conceptually available and are actively advocated by the security-maximization reading, but the humanitarian ceiling reading treats them as legally foreclosed. Resistance (0.58) reflects sustained pushback from military establishments, state legal advisers, and the security-maximization reading's proponents — the constraint is contested, not accepted.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations, detainees, irregular combatants, medical personnel, and humanitarian organizations are structural beneficiaries: the constraint directly subsidizes their protection and enables their operations. State militaries, command structures, and national security apparatuses are structural victims: they bear the compliance costs, operational constraints, and legal exposure without reciprocal relief when adversaries violate. The directionality derivation from these declarations yields low d for beneficiaries (constraint subsidizes them) and high d for victims (constraint extracts from them), which the engine amplifies into effective extraction asymmetry. The asymmetry is the structural signature of the tangled rope: genuine coordination function (the humanitarian floor) coexists with asymmetric extraction (the state bears the cost unilaterally).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — limiting the horrors of interstate war and protecting non-combatants — remains live (founding_problem_status: contested). The humanitarian ceiling reading insists the problem has not changed: civilians still need protection, detainees still face abuse, irregular warfare still generates humanitarian crises. The security-maximization reading argues the problem has mutated: asymmetric conflict with non-state actors who reject the Conventions' premises makes the ceiling operationally suicidal. The conditional reciprocity reading argues the problem is solvable only through mutual restraint. The constraint persists because no single reading has displaced the others; the Geneva framework survives as a contested coordination mechanism with extraction layered onto it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ceiling_vs_floor_ambiguity,
    'Does the humanitarian ceiling reading describe a genuine non-derogable minimum, or does it functionally operate as an aspirational ceiling that states violate when existential threats arise?',
    'Track state practice in existential asymmetric conflicts (e.g., counterterrorism operations against non-state actors who reject IHL): if states consistently violate the ceiling while invoking the reading''s language, the reading is aspirational cover; if states accept strategic costs to maintain compliance, the ceiling is structurally binding.',
    'If aspirational, the constraint''s extractiveness is lower than measured (states don''t actually bear the full cost) and its suppression is performative; if binding, the measured extractiveness and suppression are accurate and the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceiling_vs_floor_ambiguity, empirical, 'Whether the humanitarian ceiling is a binding constraint or aspirational rhetoric in practice').

omega_variable(
    irregular_combatant_protection_scope,
    'How far do ''basic humanitarian protections'' extend for irregular combatants who do not qualify for POW status — and does this reading require states to extend protections that the Convention text does not explicitly guarantee?',
    'Analyze the divergence between Common Article 3''s ''fundamental guarantees'' and the full POW protections of GC III; track how international tribunals and state practice have interpreted the scope for unprivileged belligerents.',
    'If the reading extends protections beyond Common Article 3 into de facto POW-equivalent treatment, extractiveness on state militaries rises substantially (detention, trial, repatriation obligations); if limited to CA3 minimums, extractiveness is lower and the coordination function more defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irregular_combatant_protection_scope, conceptual, 'Scope of protections for irregular combatants under the humanitarian ceiling reading').

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s instantiation of the Geneva Conventions kernel differ structurally from the conditional_reciprocity_reading and security_maximization_reading in terms of beneficiary/victim structure, suppression targets, and enforcement demands?',
    'Compare the three readings'' constraint stories: map each reading''s beneficiaries, victims, suppression mechanisms, and claimed types. The structural delta is the committer content.',
    'Documents the kernel''s internal contestation as structural divergence between constraint instantiations, not as interpretive disagreement within one constraint. Enables cross-reading contamination analysis via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'This reading''s structural distinction from sibling readings of the same kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1949, 0.12).
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1977, 0.18).
narrative_ontology:measurement(gene_tr_t1998, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1998, 0.22).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2001, 0.26).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2010, 0.27).
narrative_ontology:measurement(gene_tr_t2025, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1949, 0.22).
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1977, 0.28).
narrative_ontology:measurement(gene_be_t1998, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1998, 0.31).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2001, 0.35).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2010, 0.36).
narrative_ontology:measurement(gene_be_t2025, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1949, 0.55).
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1977, 0.62).
narrative_ontology:measurement(gene_su_t1998, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1998, 0.67).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2001, 0.71).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(gene_su_t2025, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__humanitarian_ceiling_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__security_maximization_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, icc_rome_statute_grave_breaches).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, customary_ihl_fundamental_guarantees).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the geneva_conventions_1949 kernel. The humanitarian ceiling reading (this file) treats the Conventions as establishing a non-derogable humanitarian floor. The conditional reciprocity reading treats them as reciprocal restraints degradable upon adversary non-compliance. The security maximization reading treats them as peacetime aspirations yielding to operational necessity. Each reading instantiates a different constraint with different ε, beneficiaries, victims, and suppression targets. They are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__humanitarian_ceiling_reading, institutional, 0.85).
constraint_indexing:directionality_override(geneva_conventions_1949__humanitarian_ceiling_reading, powerful, 0.8).
constraint_indexing:directionality_override(geneva_conventions_1949__humanitarian_ceiling_reading, powerless, 0.05).
constraint_indexing:directionality_override(geneva_conventions_1949__humanitarian_ceiling_reading, organized, 0.15).
constraint_indexing:directionality_override(geneva_conventions_1949__humanitarian_ceiling_reading, moderate, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
