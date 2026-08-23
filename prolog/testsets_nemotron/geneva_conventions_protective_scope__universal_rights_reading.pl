% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__universal_rights_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: geneva_conventions_protective_scope__universal_rights_reading
 *   human_readable: Geneva Protective Scope — Universal Rights Reading
 *   domain: legal/international/humanitarian
 *
 * SUMMARY:
 *   This constraint story captures the universal rights reading of Geneva
 *   protective scope: the position that Common Article 3, read together with
 *   continuing human rights law obligations, creates a single universal
 *   protection floor applying to all persons in all armed conflicts
 *   regardless of combatant status or conflict classification. This reading
 *   emerged through ICRC commentary, ICTY/ICTR jurisprudence, and human
 *   rights treaty body interpretation — expanding protections beyond the 1949
 *   treaty's explicit categorical structure. The constraint operates as a
 *   tangled rope: it performs genuine coordination (universal behavioral
 *   floor solving status-contestation) while extracting operational latitude
 *   from state military commands and national security establishments through
 *   active enforcement (international courts, UN mechanisms, diplomatic
 *   pressure). The claim/metric gap is deliberate: the reading is claimed as
 *   tangled rope (coordination + extraction acknowledged) while metrics
 *   describe the degree of each.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.68).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.72).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Geneva Protective Scope — Universal Rights Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "legal/international/humanitarian").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, 'e5ef7073-2bfd-46c1-b2e1-217295e6e912').
narrative_ontology:cs_kernel_codification('e5ef7073-2bfd-46c1-b2e1-217295e6e912', formalized).
narrative_ontology:cs_authority_grounding('e5ef7073-2bfd-46c1-b2e1-217295e6e912', lineage).
narrative_ontology:cs_interpretation_layer_present('e5ef7073-2bfd-46c1-b2e1-217295e6e912').
narrative_ontology:cs_reading_relation('e5ef7073-2bfd-46c1-b2e1-217295e6e912', geneva_conventions_protective_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5ef7073-2bfd-46c1-b2e1-217295e6e912', geneva_conventions_protective_scope__hybrid_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('e5ef7073-2bfd-46c1-b2e1-217295e6e912', foundational, common_article_3_universal_human_rights_floor).
narrative_ontology:cs_axiom_status(common_article_3_universal_human_rights_floor, holdable).
narrative_ontology:cs_axiom_grounding('e5ef7073-2bfd-46c1-b2e1-217295e6e912', common_article_3_universal_human_rights_floor, deontological).
narrative_ontology:cs_axiom('e5ef7073-2bfd-46c1-b2e1-217295e6e912', secondary, human_rights_law_non_derogation_in_armed_conflict).
narrative_ontology:cs_axiom_status(human_rights_law_non_derogation_in_armed_conflict, holdable).
narrative_ontology:cs_axiom_grounding('e5ef7073-2bfd-46c1-b2e1-217295e6e912', human_rights_law_non_derogation_in_armed_conflict, empirically_contingent).
narrative_ontology:cs_reference_frame('e5ef7073-2bfd-46c1-b2e1-217295e6e912', common_article_3_minimum_standards_1949).
narrative_ontology:cs_drift_state('e5ef7073-2bfd-46c1-b2e1-217295e6e912', post_911_counterterrorism_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e5ef7073-2bfd-46c1-b2e1-217295e6e912', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, detainees_all_categories).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, internally_displaced_persons).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_commands).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, national_security_establishments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, common_article_3_customary_universal_application).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, human_rights_law_continues_in_armed_conflict).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, non_derogable_rights_apply_to_all_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce rules of engagement, targeting policies, and detention frameworks. Bear the operational cost of expanded protections — more restrictive targeting, enhanced due process for detainees, limits on interrogation methods. Exit from the constraint would mean withdrawing from treaty obligations or denying conflict classification, both carrying severe diplomatic and legal consequences.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_military_commands, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__universal_rights_reading, state_military_commands, payer).

% Design intelligence and counterterrorism frameworks. Bear costs through restricted intelligence gathering (coercive interrogation limits), expanded review procedures, and reduced operational flexibility against non-state actors. Cannot exit without dismantling post-9/11 legal architectures.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, national_security_establishments, payer,
    institutional, generational, constrained, national).

% Receive protection from indiscriminate attacks, collective punishment, and arbitrary detention. Have no exit from conflict zones; their protection depends entirely on the constraint's enforcement by parties they cannot influence.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations, beneficiary,
    powerless, biographical, trapped, local).

% Gain legal recognition of protections (humane treatment, fair trial guarantees) previously denied as 'unprivileged belligerents.' Simultaneously bound by Common Article 3 obligations — cannot selectively claim protections while ignoring reciprocal duties. Exit means ceasing armed operations.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups, payer).

% Receive judicial guarantees, prohibition of torture, and humane detention conditions regardless of capture status (POW, security detainee, criminal suspect). No exit from detention; protection is entirely dependent on captor compliance.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, detainees_all_categories, beneficiary,
    powerless, immediate, trapped, local).

% Protected from forced displacement, starvation as method of warfare, and denial of humanitarian access. Trapped in conflict zones with no legal exit pathway; constraint is their primary legal shield.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, internally_displaced_persons, beneficiary,
    powerless, biographical, trapped, local).

% Monitor compliance, document violations, and advocate for enforcement. Their operational access depends on the constraint's universal scope — narrower readings restrict their mandate. Can relocate operations but lose institutional credibility.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_humanitarian_organizations, observer,
    organized, generational, mobile, global).

% Prosecutes war crimes and crimes against humanity. Universal reading expands jurisdictional reach to non-international conflicts and all person categories. State-centric reading would shrink docket significantly. Institutional mandate prevents exit.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_criminal_court, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a universal behavioral floor for all armed conflict parties: Common Article 3's minimum standards plus continuing human rights obligations establish baseline protections that apply regardless of conflict classification or combatant status, solving the coordination problem of 'which rules apply when status is contested.'
% TRANSFER_FUNCTION: Transfers operational latitude from state military commands and national security establishments to protected persons (civilians, detainees, non-state fighters). States lose freedom to define protection scope narrowly; protected persons gain enforceable claims to humane treatment, fair process, and protection from arbitrary violence.
% ABSENT_VOICES: Victims in conflicts where states deny applicability of Geneva Conventions (e.g., 'counterterrorism operations' classified as law enforcement, not armed conflict). Also absent: future generations who will inherit precedent set by current classification decisions. They are in conflict zones under state control, in secret detention, or not yet born.
% DISAPPEARANCE_RATIONALE: If this reading vanished, states would revert to status-based protection frameworks (state_centric_reading) or conflict-type frameworks (hybrid_proportionality_reading), immediately shrinking protected categories. Non-state actors would lose Common Article 3 protections; detainees in 'non-international' conflicts would lose fair trial guarantees; civilians in 'counterterrorism' operations would lose IHL protections entirely. The legal landscape would reorganize around state-defined thresholds.
% FOUNDING_PROBLEM: The 1949 Conventions created protection gaps: Common Article 3 applied only to non-international conflicts; Articles 2-3 thresholds excluded many conflicts; 'unprivileged belligerent' category left fighters without protections. Post-WWII decolonization wars and Cold War proxy conflicts exposed these gaps — persons fell through categorical cracks.
% FOUNDING_PROBLEM_CORROBORATION: ICRC Commentary (2016) attests the gaps were real and the universal reading addresses them. State parties (US, Israel, Russia) formally object that universal reading exceeds treaty text — the 1949 drafters did not intend Common Article 3 plus human rights law to create a single universal floor. Academic commentators (Sassòli, Dörmann, Melzer) split: some corroborate universal reading as necessary evolution; others attest it rewrites rather than interprets.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__universal_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__universal_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(geneva_conventions_protective_scope__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__universal_rights_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the significant operational latitude transferred from states to protected persons — targeting restrictions, detention review requirements, interrogation prohibitions. Suppression (0.72) is high because the constraint's persistence depends on active international enforcement: ICC jurisdiction, universal jurisdiction claims, treaty body reviews, diplomatic conditionality. Theater (0.28) is moderate: states perform compliance (ratification, military manuals) while maintaining parallel frameworks (counterterrorism law, national security exceptions) that functionally narrow the constraint. Accessibility collapse (0.42) and resistance (0.55) reflect that alternatives (status-based, conflict-type frameworks) remain live and actively advocated by powerful states — the universal reading has not collapsed the field.
 *
 * PERSPECTIVAL GAP:
 *   From state military commands' seat, this constraint is experienced as enforced extraction — operational restrictions imposed by external legal interpretation. From civilian/detainee seats, it is experienced as coordination — the only mechanism making their protection claims enforceable. From non-state armed groups' seat, it is a double-edged coordination: they gain protections but accept binding obligations. The engine computes this divergence from the structural data; the universal reading does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   State military commands and national security establishments are structural payers (d near 1.0): they bear operational costs, face prosecution risk, and cannot exit treaty frameworks. Civilian populations, detainees, and IDPs are structural beneficiaries (d near 0.0): they receive protections with no capacity to enforce and no exit. Non-state armed groups are dual-positioned: beneficiaries of protections they previously lacked, but payers of reciprocal obligations under Common Article 3. IHL organizations and ICC are analytical observers (d=0.5): they monitor but neither collect nor pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (categorical protection gaps in 1949 treaty structure) remains contested: universal reading proponents argue gaps persist in modern asymmetric conflicts; state-centric proponents argue treaty text never authorized this expansion. The constraint is not a piton — it actively reshapes state behavior and generates prosecutions. It is not a scaffold — no sunset clause, no declared transitional purpose. Tangled rope is the honest classification: real coordination function (universal floor) + real asymmetric extraction (state operational latitude) + active enforcement required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_reading_vs_treaty_text,
    'Does the universal reading faithfully interpret the 1949 Conventions and 1977 Protocols, or does it substantively rewrite them through evolutive interpretation?',
    'Comparative analysis of travaux préparatoires, subsequent state practice, and ICJ/ICC jurisprudence on treaty interpretation (VCLT Articles 31-33). The 2016 ICRC Commentary update is a key document.',
    'If faithful interpretation, the constraint is a rope (coordination implementing treaty intent). If substantive rewrite, it is a tangled rope or snare (new obligations imposed without state consent). Classification shifts on this axis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_reading_vs_treaty_text, conceptual, 'Whether the universal reading is interpretive or constitutive.').

omega_variable(
    human_rights_law_continuation_scope,
    'To what extent does human rights law ''continue to apply'' in armed conflict in a way that creates a universal floor, versus being displaced by lex specialis (IHL)?',
    'Resolution of the ICJ Nuclear Weapons Advisory Opinion (1996) ''lex specialis'' formulation through subsequent jurisprudence (Wall Advisory, DRC v Uganda, Al-Jedda, Hassan v UK). State practice on derogations in NIAC.',
    'If HRL continuation is robust, universal floor is legally solid (lower extractiveness on states — they consented to HRL treaties). If HRL is largely displaced, universal reading imposes non-consented obligations (higher extractiveness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_rights_law_continuation_scope, empirical, 'Scope of human rights law continuation in armed conflict.').

omega_variable(
    non_state_actor_reciprocity_enforcement,
    'Can Common Article 3 obligations be effectively enforced against non-state armed groups, or does the universal reading create asymmetric obligations (states bound, non-state actors unbound)?',
    'Empirical study of non-state armed group compliance with CA3, engagement with Geneva Call, ICC prosecutions of non-state actors, and UN monitoring mechanisms.',
    'If enforcement is asymmetric, the constraint extracts from states while failing to coordinate non-state behavior — moves toward snare. If reciprocal, coordination function is genuine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_state_actor_reciprocity_enforcement, empirical, 'Reciprocity of enforcement against non-state armed groups.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the universal rights reading''s core premise (single universal floor for all persons) logically foreclose the state-centric reading (status-based protections) within any single legal framework, or do they coexist as competing interpretive positions?',
    'Analysis of whether a state can simultaneously maintain: (a) universal CA3+HRL floor for all persons, AND (b) status-based combatant privileges/immunities under Article 4. The ICRC''s ''cumulative application'' doctrine vs. state practice of mutual exclusivity.',
    'If forecloses: the readings are structurally incompatible — adoption of universal reading requires abandonment of state-centric framework. If coexists_with: states can selectively invoke each reading in different forums (e.g., universal reading in human rights bodies, state-centric in military operations).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Logical relationship between universal and state-centric readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1977, 0.15).
narrative_ontology:measurement(gene_tr_t1993, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1993, 0.18).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(gene_tr_t2006, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2006, 0.25).
narrative_ontology:measurement(gene_tr_t2016, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2016, 0.27).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1949, 0.35).
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1977, 0.42).
narrative_ontology:measurement(gene_be_t1993, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement(gene_be_t2006, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2006, 0.62).
narrative_ontology:measurement(gene_be_t2016, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2016, 0.65).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1949, 0.4).
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1977, 0.5).
narrative_ontology:measurement(gene_su_t1993, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1993, 0.58).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2001, 0.65).
narrative_ontology:measurement(gene_su_t2006, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2006, 0.68).
narrative_ontology:measurement(gene_su_t2016, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2016, 0.7).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__universal_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__universal_rights_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, icc_jurisdiction_non_international_conflicts).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, universal_jurisdiction_war_crimes).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, ihl_human_rights_law_interplay).

% DUAL FORMULATION NOTE:
% This constraint and its two siblings form a constraint family decomposing the 'Geneva protective scope' label. The universal reading raises ε on states (restricts operations); the state-centric reading minimizes ε on states but creates protection gaps (high ε on excluded persons); the hybrid reading creates a dual-track system with its own coordination/extraction profile. All three share the same kernel but instantiate different constraints with different ε, different beneficiary/victim structures, and different types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_protective_scope__universal_rights_reading, institutional, 0.85).
constraint_indexing:directionality_override(geneva_conventions_protective_scope__universal_rights_reading, powerless, 0.05).
constraint_indexing:directionality_override(geneva_conventions_protective_scope__universal_rights_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
