% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__abolition_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: state_killing_legitimacy__abolition_reading
 *   human_readable: Categorical Prohibition of State Killing (Abolition Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   The abolition reading of the state-killing-legitimacy kernel instantiates
 *   a categorical prohibition: state killing violates human dignity
 *   regardless of desert or utility. This reading treats the condemned person
 *   as a rights-bearer whose protection is the constraint's beneficiary, and
 *   treats the state's killing power (and its justificatory frameworks) as
 *   the extractive target. The constraint claims mountain status — a
 *   natural-law-like prohibition that emerges from the structure of dignity
 *   itself — while the metrics describe a constraint that has been built,
 *   expanded, and maintained through active struggle (high resistance,
 *   declining theater, declining extraction over 260 years). The engine will
 *   compute per-seat types from this structural data; the authored claim and
 *   metrics are independent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, 0.15).
domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, 0.05).
domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__abolition_reading, mountain).
narrative_ontology:human_readable(state_killing_legitimacy__abolition_reading, "Categorical Prohibition of State Killing (Abolition Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__abolition_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:emerges_naturally(state_killing_legitimacy__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__abolition_reading, '2cf8e888-e5fe-4467-be69-07cc6df9b8af').
narrative_ontology:cs_kernel_codification('2cf8e888-e5fe-4467-be69-07cc6df9b8af', formalized).
narrative_ontology:cs_authority_grounding('2cf8e888-e5fe-4467-be69-07cc6df9b8af', lineage).
narrative_ontology:cs_interpretation_layer_present('2cf8e888-e5fe-4467-be69-07cc6df9b8af').
narrative_ontology:cs_reading_relation('2cf8e888-e5fe-4467-be69-07cc6df9b8af', state_killing_legitimacy__deterrence_reading, forecloses).
narrative_ontology:cs_reading_relation('2cf8e888-e5fe-4467-be69-07cc6df9b8af', state_killing_legitimacy__retributive_reading, forecloses).
narrative_ontology:cs_axiom('2cf8e888-e5fe-4467-be69-07cc6df9b8af', foundational, human_dignity_inalienable).
narrative_ontology:cs_axiom_status(human_dignity_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('2cf8e888-e5fe-4467-be69-07cc6df9b8af', human_dignity_inalienable, deontological).
narrative_ontology:cs_axiom('2cf8e888-e5fe-4467-be69-07cc6df9b8af', foundational, right_to_life_absolute).
narrative_ontology:cs_axiom_status(right_to_life_absolute, holdable).
narrative_ontology:cs_axiom_grounding('2cf8e888-e5fe-4467-be69-07cc6df9b8af', right_to_life_absolute, deontological).
narrative_ontology:cs_axiom('2cf8e888-e5fe-4467-be69-07cc6df9b8af', secondary, state_killing_categorically_violates_dignity).
narrative_ontology:cs_axiom_status(state_killing_categorically_violates_dignity, holdable).
narrative_ontology:cs_axiom_grounding('2cf8e888-e5fe-4467-be69-07cc6df9b8af', state_killing_categorically_violates_dignity, deontological).
narrative_ontology:cs_reference_frame('2cf8e888-e5fe-4467-be69-07cc6df9b8af', pre_abolition_sovereign_violence).
narrative_ontology:cs_drift_state('2cf8e888-e5fe-4467-be69-07cc6df9b8af', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2cf8e888-e5fe-4467-be69-07cc6df9b8af', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__abolition_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, condemned_persons).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, human_rights_advocates).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, abolitionist_organizations).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, state_killing_power).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, retributive_justification).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, deterrence_justification).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__abolition_reading, human_dignity_inalienable).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__abolition_reading, right_to_life_absolute).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__abolition_reading, state_killing_categorically_violates_dignity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons under sentence of death whose lives are protected by the categorical prohibition. They bear no cost of the constraint; the constraint exists to shield them from state killing. Their exit from the protected class is impossible — they are the rights-bearers for whom the prohibition is the floor.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, condemned_persons, beneficiary,
    powerless, immediate, trapped, universal).

% Advocacy organizations and legal practitioners who build jurisprudence, litigate stays, and mobilize international pressure against state killing. They benefit professionally and morally from the prohibition's expansion; their work is the active maintenance of the mountain. They can shift forums (domestic courts, regional systems, UN mechanisms) — arbitrage-grade exit across institutional venues.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, human_rights_advocates, beneficiary,
    organized, generational, arbitrage, global).

% The organized abolitionist movement (Amnesty International, World Coalition Against the Death Penalty, national campaigns) that sets the normative agenda, drafts treaty language, and coordinates strategic litigation. They administer the prohibition's expansion through institutional channels. They do not pay the cost of the constraint — they profit from its extension in legitimacy and resources.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, abolitionist_organizations, agenda_setter,
    institutional, civilizational, analytical, global).

% The state's institutional capacity to execute — the execution chambers, protocols, personnel, and legal machinery. The categorical prohibition extracts from this power by rendering its core function illegitimate. It cannot easily exit: the state's monopoly on violence is constitutionally entrenched, and the prohibition targets its most extreme expression. The cost is the loss of a sovereign prerogative.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, state_killing_power, payer,
    institutional, generational, constrained, national).

% The normative framework that justifies killing as proportional desert (lex talionis). Its adherents — prosecutors, victims' families oriented toward retribution, conservative legal theorists — are identity-locked: the justification is constitutive of their moral worldview. The prohibition does not merely inconvenience them; it declares their founding premise illegitimate. Exit requires abandoning the identity.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, retributive_justification, payer,
    organized, civilizational, identity_locked, universal).

% The empirical-moral framework that justifies killing as a rational signal preventing future murders. Its adherents — criminologists, policymakers, law-enforcement leadership — can shift to alternative crime-control strategies (constrained exit), but the prohibition removes their strongest rhetorical and institutional tool. The cost is the loss of the ultimate sanction.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, deterrence_justification, payer,
    organized, civilizational, constrained, universal).

% Families of murder victims who seek execution as closure or justice. They are excluded from the abolitionist reading's beneficiary set — the reading treats their desire for retribution as a preference that must yield to the condemned person's inalienable right. They would object if present; their exclusion is structural to the reading's categorical form.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, victims_families_retributive, excluded,
    moderate, biographical, identity_locked, local).

% Scholars who analyze the prohibition's coherence, its genealogy, its relationship to competing justifications, and its empirical effects on state behavior. They neither collect nor pay; they map the structure.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, legal_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a universal floor: no state may kill its captives, regardless of crime, deterrence calculus, or retributive sentiment. Solves the collective-action problem of states racing to the bottom on sovereign violence by making the prohibition a condition of legitimacy in the international order.
% TRANSFER_FUNCTION: Transfers the power to kill from the state to a categorical prohibition. The state loses a sovereign prerogative (the ultimate sanction); the condemned person gains an inalienable protection. The transfer is not of resources but of authority — the authority to decide life and death is removed from the state's discretion.
% ABSENT_VOICES: Victims' families who experience the prohibition as a denial of justice; prosecutors who lose their strongest leverage; publics in retentionist states who support the death penalty and see the prohibition as foreign imposition. They are structurally excluded because the reading defines their preferences as irrelevant to the rights-bearer's claim.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished overnight, retentionist states would resume or expand executions immediately; abolitionist states would face domestic pressure to reinstate; the international human rights architecture built on the right to life would lose its keystone. The world rearranges because the prohibition is the only thing preventing state killing in abolitionist jurisdictions and the only brake in retentionist ones.
% FOUNDING_PROBLEM: The historical problem of sovereign arbitrariness: states killing captives for political dissent, religious heresy, minor theft, and identity — with no principle limiting the power. The abolition reading was built to solve this by removing the power entirely, not regulating its exercise.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by the historical record of abolitionist movements (Beccaria, Hugo, Amnesty International's founding, the UN moratorium campaigns) and by contemporary retentionist practices (political executions in Iran, China, Saudi Arabia; drug offenses in Singapore, Indonesia) — sources outside the abolitionist movement confirm the founding problem persists. The state's killing power remains arbitrary where it exists.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__abolition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__abolition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(state_killing_legitimacy__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__abolition_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__abolition_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, ExtMetricName, E),
    domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(state_killing_legitimacy__abolition_reading),
    narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(state_killing_legitimacy__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.15) at interval end because the prohibition extracts little from those it governs — it removes a power from the state rather than transferring resources to a beneficiary. Suppression is near-zero (0.05) because the prohibition does not coerce compliance through force; it operates as a legitimacy condition. Theater is minimal (0.05) — the prohibition's function (protecting life) is its practice. Accessibility collapse is low (0.1) because alternatives (retributive, deterrence frameworks) remain live and contested. Resistance is high (0.8) because the prohibition faces active opposition from retentionist states and justificatory frameworks. The trajectory shows a constraint that began as a radical claim (high extraction from state power, high theater of justification, high suppression of abolitionists) and became a settled mountain in the international order.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute different effective types per seat: for condemned persons and advocates, the constraint is mountain (negligible χ); for state killing power and retributive justification, it may compute as snare or tangled_rope (high χ for identity-locked payers). The divergence is the measurement — a constraint that is mountain for the rights-bearer can be extractive for the power it displaces.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons are full beneficiaries (d ≈ 0.0): the constraint exists for them, they pay nothing. Human rights advocates and abolitionist organizations are beneficiaries/agenda-setters (d low): they gain legitimacy and resources from the prohibition's expansion. State killing power is a payer (d high): it loses its ultimate sanction. Retributive justification is a payer (d high, identity_locked): its core premise is declared illegitimate; exit requires identity transformation. Deterrence justification is a payer (d moderate-high, constrained): it loses its strongest tool but can pivot to alternative strategies. Victims' families (retributive) are excluded: their preference for execution is structurally irrelevant to the reading's categorical form.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sovereign arbitrariness) remains live — states still kill arbitrarily where they retain the power. The prohibition has not atrophied; its mandate expands. Mandatrophy is resolved in the negative: the constraint's function is not obsolete, it is incomplete. The high resistance metric reflects the unfinished expansion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_constructed_prohibition,
    'Is the categorical prohibition a genuine natural-law mountain (emerging from the structure of human dignity) or a constructed constraint that benefits identifiable agents (abolitionist organizations, human rights legal profession)?',
    'Genealogical analysis of the prohibition''s emergence: if the prohibition''s expansion tracks the institutionalization of the abolitionist movement and the professionalization of human rights law rather than a discovery of pre-existing natural law, the constructed reading gains ground. Cross-cultural comparison: if the prohibition appears only where the movement has institutional penetration, it is constructed.',
    'If constructed, the false_summit_mountain signature may fire (mountain claim + declared beneficiaries = FSM candidate). The constraint would reclassify as tangled_rope (coordination function: universal floor; asymmetric extraction: state power pays; active enforcement: movement maintenance). If natural law, the mountain claim holds and beneficiaries are incidental to the structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_constructed_prohibition, conceptual, 'Natural-law vs. constructed-prohibition ambiguity — the core FSM question for this reading.').

omega_variable(
    extraction_referent_ambiguity,
    'Does the low extractiveness (0.15) measure extraction from the condemned person (near zero) or extraction from the state''s killing power (high)? The ε referent for kernel-reading stories is the standing arrangement under contest — the abolitionist reads the existing death-penalty system as highly extractive of the condemned person''s life. But this reading''s own ε measures its own operation. Which referent governs?',
    'Apply OQ-26 ruling: ε is reading-indexed over a fixed referent. The referent is the standing arrangement (state killing power). The abolition reading assesses that arrangement as highly extractive of the condemned person. But the constraint being authored IS the prohibition, not the death penalty. The prohibition''s operation extracts from state power. This omega documents the referent tension.',
    'If ε refers to the prohibition''s operation: low extraction (mountain-compatible). If ε refers to the death penalty''s operation (the referent of contest): high extraction (snare-compatible). The classification depends on which constraint the ε describes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_referent_ambiguity, conceptual, 'ε referent ambiguity in kernel-reading stories: the standing arrangement vs. the reading''s own constraint.').

omega_variable(
    victim_identity_legitimacy,
    'Are ''state_killing_power'', ''retributive_justification'', and ''deterrence_justification'' legitimate victims (actors who bear costs) or are they propositions/doctrines that should be in vindicated_propositions? The schema requires victims to be domain-specific group names identifying real-world actors.',
    'Test: does the entity have agency, interests, and the capacity to resist? State_killing_power (execution chambers, personnel, protocols) has institutional agency. Retributive_justification and deterrence_justification are frameworks — their adherents (prosecutors, victims'' families, criminologists) are the actors. The current victim list mixes actors and frameworks.',
    'If victims must be actors, retributive_justification and deterrence_justification should move to vindicated_propositions (as propositions the constraint''s operation defeats) and their adherents should be named as separate victim stakeholders. This would change the beneficiary/victim balance and the directionality derivation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_identity_legitimacy, conceptual, 'Whether justificatory frameworks are legitimate victim actors or defeated propositions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__abolition_reading, 1764, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1764, state_killing_legitimacy__abolition_reading, theater_ratio, 1764, 0.8).
narrative_ontology:measurement(stat_tr_t1800, state_killing_legitimacy__abolition_reading, theater_ratio, 1800, 0.6).
narrative_ontology:measurement(stat_tr_t1850, state_killing_legitimacy__abolition_reading, theater_ratio, 1850, 0.4).
narrative_ontology:measurement(stat_tr_t1900, state_killing_legitimacy__abolition_reading, theater_ratio, 1900, 0.25).
narrative_ontology:measurement(stat_tr_t1950, state_killing_legitimacy__abolition_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(stat_tr_t1976, state_killing_legitimacy__abolition_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(stat_tr_t2000, state_killing_legitimacy__abolition_reading, theater_ratio, 2000, 0.07).
narrative_ontology:measurement(stat_tr_t2024, state_killing_legitimacy__abolition_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(stat_be_t1764, state_killing_legitimacy__abolition_reading, base_extractiveness, 1764, 0.95).
narrative_ontology:measurement(stat_be_t1800, state_killing_legitimacy__abolition_reading, base_extractiveness, 1800, 0.85).
narrative_ontology:measurement(stat_be_t1850, state_killing_legitimacy__abolition_reading, base_extractiveness, 1850, 0.7).
narrative_ontology:measurement(stat_be_t1900, state_killing_legitimacy__abolition_reading, base_extractiveness, 1900, 0.55).
narrative_ontology:measurement(stat_be_t1950, state_killing_legitimacy__abolition_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(stat_be_t1976, state_killing_legitimacy__abolition_reading, base_extractiveness, 1976, 0.25).
narrative_ontology:measurement(stat_be_t2000, state_killing_legitimacy__abolition_reading, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement(stat_be_t2024, state_killing_legitimacy__abolition_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1764, state_killing_legitimacy__abolition_reading, suppression_requirement, 1764, 0.9).
narrative_ontology:measurement(stat_su_t1800, state_killing_legitimacy__abolition_reading, suppression_requirement, 1800, 0.75).
narrative_ontology:measurement(stat_su_t1850, state_killing_legitimacy__abolition_reading, suppression_requirement, 1850, 0.55).
narrative_ontology:measurement(stat_su_t1900, state_killing_legitimacy__abolition_reading, suppression_requirement, 1900, 0.35).
narrative_ontology:measurement(stat_su_t1950, state_killing_legitimacy__abolition_reading, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement(stat_su_t1976, state_killing_legitimacy__abolition_reading, suppression_requirement, 1976, 0.12).
narrative_ontology:measurement(stat_su_t2000, state_killing_legitimacy__abolition_reading, suppression_requirement, 2000, 0.08).
narrative_ontology:measurement(stat_su_t2024, state_killing_legitimacy__abolition_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__abolition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(state_killing_legitimacy__abolition_reading, 0.08).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__deterrence_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__retributive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the state_killing_legitimacy kernel. The three readings (abolition, deterrence, retributive) form a constraint family linked by network.affects_constraints. The abolition reading claims mountain status with low ε; the sibling readings claim coordination/justification functions with higher ε (they extract from the condemned person). The ε values differ structurally — this is not one constraint measured differently but three constraints generated from one kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_legitimacy__abolition_reading, institutional, 0.9).
constraint_indexing:directionality_override(state_killing_legitimacy__abolition_reading, organized, 0.85).
constraint_indexing:directionality_override(state_killing_legitimacy__abolition_reading, powerless, 0.0).
constraint_indexing:directionality_override(state_killing_legitimacy__abolition_reading, moderate, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
