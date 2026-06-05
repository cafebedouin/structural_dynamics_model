% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment__citizenship_clause
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment__citizenship_clause, []).

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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fourteenth_amendment__citizenship_clause
 *   human_readable: Fourteenth Amendment Citizenship Clause — Birthright Citizenship as Irreversible Legal Status
 *   domain: constitutional_law/citizenship
 *
 * SUMMARY:
 *   The Fourteenth Amendment's Citizenship Clause (Section 1: 'All persons
 *   born or naturalized in the United States, and subject to the jurisdiction
 *   thereof, are citizens of the United States and of the State wherein they
 *   reside') represents the post-Civil War constitutional overruling of Dred
 *   Scott v. Sandford, which had held that enslaved and free Black persons
 *   could not be citizens. The Clause makes citizenship automatic and
 *   birthright-based (jus soli), placing membership beyond political
 *   revocation or ancestor-based exclusion. This constraint story
 *   instantiates ONE READING of the Fourteenth Amendment kernel — the
 *   citizenship reading. Sibling readings include due_process_clause
 *   (procedural and substantive liberty), equal_protection_clause
 *   (antidiscrimination mandate), and privileges_or_immunities_clause
 *   (substantive national rights). The citizenship reading focuses on the
 *   automatic conferral mechanism: birth or naturalization triggers
 *   membership irreversibly. This is structurally distinct from the equal
 *   protection reading (which addresses equal treatment within the law) and
 *   the due process reading (which addresses procedures for deprivation). The
 *   Citizenship Clause generates a mountain-type classification because the
 *   rule appears irreversible and immutable from within the constitutional
 *   framework: once a person is born on US soil, no amount of subsequent
 *   political pressure, nativist sentiment, or legislative effort can
 *   retroactively strip that status without an Article V amendment. The
 *   constraint is remedial (overruling Dred Scott) and prospective
 *   (establishing an automatic rule going forward).
 *
 * KEY AGENTS:
 *   - Native-born persons (especially those of ancestry that would have been excluded pre-1868): Primary beneficiaries — receive automatic, irrevocable citizenship by birth
 *   - Naturalized persons: Secondary beneficiaries — receive irrevocable citizenship through statutory process; gain protection against retroactive exclusion
 *   - Nativist political coalitions/ancestry-gatekeeping interests: Primary victims — suffer suppression of their preferred membership mechanism (ancestry-based or discretionary citizenship)
 *   - The nation-state: Institutional beneficiary — gains coordination benefit of automatic, clear membership rules
 *   - Congress: Institutional actor with constrained power — can define 'born in the United States' narrowly but cannot retroactively strip citizenship
 *   - Analytical observer: Sees the constraint as a fixed constitutional point requiring Article V amendment to alter
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment__citizenship_clause, 0.08).
domain_priors:suppression_score(fourteenth_amendment__citizenship_clause, 0.04).
domain_priors:theater_ratio(fourteenth_amendment__citizenship_clause, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment__citizenship_clause, extractiveness, 0.08).
narrative_ontology:constraint_metric(fourteenth_amendment__citizenship_clause, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(fourteenth_amendment__citizenship_clause, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment__citizenship_clause, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(fourteenth_amendment__citizenship_clause, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment__citizenship_clause, mountain).
narrative_ontology:human_readable(fourteenth_amendment__citizenship_clause, "Fourteenth Amendment Citizenship Clause — Birthright Citizenship as Irreversible Legal Status").
narrative_ontology:topic_domain(fourteenth_amendment__citizenship_clause, "constitutional_law/citizenship").

domain_priors:emerges_naturally(fourteenth_amendment__citizenship_clause).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment__citizenship_clause, '7adb3603-aeb2-4a48-b150-89d2a7fef637').
narrative_ontology:cs_kernel_codification('7adb3603-aeb2-4a48-b150-89d2a7fef637', formalized).
narrative_ontology:cs_authority_grounding('7adb3603-aeb2-4a48-b150-89d2a7fef637', lineage).
narrative_ontology:cs_interpretation_layer_present('7adb3603-aeb2-4a48-b150-89d2a7fef637').
narrative_ontology:cs_reading_relation('7adb3603-aeb2-4a48-b150-89d2a7fef637', fourteenth_amendment__due_process_clause, influences).
narrative_ontology:cs_reading_relation('7adb3603-aeb2-4a48-b150-89d2a7fef637', fourteenth_amendment__equal_protection_clause, influences).
narrative_ontology:cs_reading_relation('7adb3603-aeb2-4a48-b150-89d2a7fef637', fourteenth_amendment__privileges_or_immunities_clause, influences).
narrative_ontology:cs_axiom('7adb3603-aeb2-4a48-b150-89d2a7fef637', foundational, jus_soli_birthright_automatic).
narrative_ontology:cs_axiom_status(jus_soli_birthright_automatic, holdable).
narrative_ontology:cs_axiom_grounding('7adb3603-aeb2-4a48-b150-89d2a7fef637', jus_soli_birthright_automatic, conventional).
narrative_ontology:cs_axiom('7adb3603-aeb2-4a48-b150-89d2a7fef637', foundational, citizenship_irrevocable_absent_amendment).
narrative_ontology:cs_axiom_status(citizenship_irrevocable_absent_amendment, holdable).
narrative_ontology:cs_axiom_grounding('7adb3603-aeb2-4a48-b150-89d2a7fef637', citizenship_irrevocable_absent_amendment, deontological).
narrative_ontology:cs_reference_frame('7adb3603-aeb2-4a48-b150-89d2a7fef637', post_dred_scott_overruling_birthright_regime).
narrative_ontology:cs_drift_state('7adb3603-aeb2-4a48-b150-89d2a7fef637', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7adb3603-aeb2-4a48-b150-89d2a7fef637', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(fourteenth_amendment__citizenship_clause, fourteenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment__citizenship_clause, native_born_persons).
narrative_ontology:constraint_beneficiary(fourteenth_amendment__citizenship_clause, naturalized_persons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NATIVE-BORN PERSON — From the perspective of a person born on US soil whose ancestry would have rendered them ineligible for citizenship under pre-1868 law or contemporary nativist frameworks, the Citizenship Clause appears as an immutable legal fact. Birth confers citizenship regardless of ancestry, parental status, or political will to revoke. This is not contingent on ongoing enforcement, petition, or demonstration of loyalty — it is automatic and irrevocable. The alternative (statelessness, denizen status, revocable membership) is foreclosed by the constitutional text itself.
constraint_indexing:constraint_classification(fourteenth_amendment__citizenship_clause, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER — From the standpoint of constitutional law as a formal system, the Citizenship Clause establishes a brightline rule: jus soli citizenship (born on soil) is the default; Congress can narrow the definition of 'born in the United States' but cannot retroactively strip citizenship. The rule's force derives from constitutional text, not from political consensus or enforcement capacity. Amendment would require Article V formality, not legislative whim. Within the formal system, the Clause appears as a fixed point.
constraint_indexing:constraint_classification(fourteenth_amendment__citizenship_clause, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: NATION-STATE INSTITUTIONAL PERSPECTIVE — The nation-state benefits from clear, automatic membership rules. The Citizenship Clause solves a coordination problem: rather than adjudicating each person's eligibility through discretionary processes (as Dred Scott effectively required), the state declares birthright citizenship as the default. This reduces administrative overhead, prevents arbitrary exclusion, and aligns state membership with actual territorial presence. The state experiences the clause as beneficial coordination that simplifies governance.
constraint_indexing:constraint_classification(fourteenth_amendment__citizenship_clause, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment__citizenship_clause_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fourteenth_amendment__citizenship_clause, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fourteenth_amendment__citizenship_clause, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fourteenth_amendment__citizenship_clause, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fourteenth_amendment__citizenship_clause, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fourteenth_amendment__citizenship_clause, ExtMetricName, E),
    domain_priors:suppression_score(fourteenth_amendment__citizenship_clause, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fourteenth_amendment__citizenship_clause),
    narrative_ontology:constraint_metric(fourteenth_amendment__citizenship_clause, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fourteenth_amendment__citizenship_clause, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fourteenth_amendment__citizenship_clause_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The Citizenship Clause does not extract resources, wealth, or labor from any group. It confers a status (citizenship) and prevents its revocation. This is the opposite of extraction — it is a membership guarantee. The beneficiary (native-born persons) gains irreversible legal status; the victim (nativist gatekeeping interests) loses the ability to exclude based on ancestry or discretion. Extractiveness measures the constraint's capacity to transfer value from one group to another; citizenship conferral is a status allocation, not a transfer. The non-zero value (0.08 rather than 0.0) reflects that the legal certainty the Clause provides creates administrative efficiencies that benefit the state, which could theoretically be characterized as an infinitesimal transfer from those who would prefer discretionary membership. This is at the absolute floor of the mountain category. Suppression (0.04): Minimal. The Clause suppresses certain alternatives: one cannot legally exclude persons born on soil from citizenship based on ancestry, race, or parental status. This suppression is the core function of the constraint — it prevents nativist membership gatekeeping. However, suppression is not high because the Clause works through constitutional prohibition, not through active coercion of agents. The nation-state's administrative apparatus enforces the rule, but enforcement requires only passive application of the birthright principle, not active suppression of resistant agents. Theater ratio (0.15): Minimal. The Citizenship Clause is a formal rule with clear application. When a person is born on US soil, citizenship follows without ceremony, petition, or performative loyalty demonstration (distinct from naturalization, which involves oath-taking). The small non-zero theater value (0.15 rather than 0.0) reflects that naturalization does require performative elements (oath of allegiance, civics test in some eras) and that citizenship claims can be contested in boundary cases (jus soli definition, 'subject to the jurisdiction thereof' interpretation). But the core rule is minimally theatrical — the default is presumptively citizenship, not a status that must be continually claimed or performed.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives arrive at mountain or rope classifications, with no Snare or Tangled Rope readings. This is consistent with the constraint's remedial and constitutional character — there is no coherent perspective from which the automatic birthright conferral appears as pure extraction. A 'nativist interest' perspective (if included) might classify as tangled_rope (the Clause coordinates national membership while suppressing ancestry-based gatekeeping), but such a perspective would be that of an agent opposing the constraint, not an agent positioned within its structure. The powerless/trapped perspective (native-born person) and analytical perspective both see mountain because the rule's immutability is its defining feature from both angles. The institutional perspective (nation-state) sees rope because the constraint coordinates membership without significant extraction cost. The perspectival convergence itself is diagnostic: constraints that appear as the same type from radically different observer positions are often genuine structural laws rather than contingent institutional arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the agent's structural relationship to the constraint. Native-born persons benefit from the Citizenship Clause (low d, beneficiary status) but have trapped exit options (cannot choose not to be born on US soil, cannot contract out of citizenship). The engine's derivation chain produces d ≈ 0.08 (beneficiary + trapped + powerless power atom). Naturalized persons also benefit (low d) but have constrained exit (can renounce citizenship, but doing so is costly and politically fraught). The nation-state benefits from the coordination (d ≈ 0.05, beneficiary + arbitrage + institutional) and experiences chi close to zero — the constraint is pure coordination with minimal extraction. The analytical observer (d ≈ 0.72 canonical) sees the constraint from the perspective of understanding its structural properties, which derives a moderate d reflecting the perspective's analytical position relative to the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by being a genuine mountain: the classification is stable across perspectives because the constraint's irreversibility is a fixed constitutional fact, not a contingent institutional arrangement. There is no meaningful dispute about whether the Citizenship Clause generates automatic citizenship for the native-born — the dispute is about what 'born in the United States' and 'subject to the jurisdiction thereof' mean in edge cases (children of diplomatic staff, territories, undocumented immigrants). But these boundary disputes do not affect the core rule's classification as mountain. The Clause is immutable absent Article V amendment; it cannot be revoked by legislative, executive, or state action; it operates automatically without ongoing enforcement or performance. These are the hallmarks of a mountain constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_reading,
    'Is the Citizenship Clause a natural law of political community (persons born on soil belong to that polity) or a constructed legal doctrine that benefits identifiable agents (those who would have been excluded under prior regimes)?',
    'Comparative constitutionalism: identify whether jus soli citizenship appears naturally in multiple constitutional traditions or only in post-slavery/post-exclusion contexts. Historical analysis of which actors argued for vs. against birthright citizenship before and after Dred Scott.',
    'If natural law: the mountain classification is genuine; alternative readings are incoherent. If constructed: the beneficiary declaration (native-born persons, naturalized persons) indicates potential false summit — the constraint may be a snare on ancestry-gatekeeping interests. The engine''s FSM detector will flag this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, conceptual, 'Whether birthright citizenship is a natural law or constructed doctrine benefiting identified agents').

omega_variable(
    definitional_boundary_contestation,
    'Does the Citizenship Clause''s irreversibility extend to all legal definitional boundaries of ''born in the United States'' (jus soli scope, temporal scope, parental immigration status scope), or only to the core conferral rule?',
    'Jurisprudential analysis: examine cases where Congress has redefined ''born in the United States'' (e.g., Wong Kim Ark, nationality of children born to non-resident aliens, territorial incorporation doctrine). Identify which redefinitions treat the core rule as immutable vs. which permit legislative narrowing of the citizenship category post-hoc.',
    'If the core rule is truly immutable but boundaries are revisable: the constraint is mountain for the core (birth confers citizenship) but rope for definitional scope (Congress can narrow who counts as ''born in the US''). If boundaries are also immutable: genuine mountain. If the core has been revised through doctrine: not a mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definitional_boundary_contestation, empirical, 'Scope of irrevocability: does immutability extend to definitional boundaries or only to the core conferral rule').

omega_variable(
    remedial_vs_naturalized_asymmetry,
    'Do naturalized citizens and native-born citizens occupy the same constitutional citizenship status, or does the Clause create a structural distinction between remedy (overruling Dred Scott''s exclusion) and ongoing enrollment (naturalization)?',
    'Constitutional doctrine review: compare the Court''s treatment of deprivation, loss of citizenship, and rights between native-born and naturalized categories. Examine whether the 14th Amendment''s scope applies equally or whether remedial intent (overruling Dred Scott) narrows the clause to certain populations.',
    'If asymmetry exists: the constraint may not be a unified mountain but two related constraints with different extraction profiles. Native-born status becomes immutable (mountain); naturalized status might be more conditional (rope or tangled rope). If equivalence is strict: single mountain constraint for all citizenship-bearers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_vs_naturalized_asymmetry, empirical, 'Whether native-born and naturalized citizenship occupy identical constitutional status').

omega_variable(
    loss_of_citizenship_doctrine_exception,
    'Can the United States government revoke citizenship through denaturalization, expatriation, or loss-of-citizenship statutes, and if so, does this constitute a breach of the Citizenship Clause or permissible legislative specification?',
    'Statutory and case law analysis: examine denaturalization grounds (Weissmann, felony conviction, fraud in naturalization application), expatriation doctrine (voluntary relinquishment, acquisition of foreign nationality), and whether these are treated as consistent with or contrary to the Clause. Identify the constitutional basis for any permitted loss mechanisms.',
    'If loss mechanisms exist but require affirmative individual action (voluntary relinquishment, fraud-based revocation): mountain for automatic conferral remains intact, but constraint is not absolute. If involuntary loss is permitted: significant exception to immutability, downgrading classification from mountain to rope or tangled rope. If loss is forbidden entirely: mountain is genuinely absolute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(loss_of_citizenship_doctrine_exception, empirical, 'Whether citizenship can be revoked and whether revocation mechanisms violate or are consistent with the Clause').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment__citizenship_clause, 1868, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(citizenship_tr_t1868, fourteenth_amendment__citizenship_clause, theater_ratio, 1868, 0.05).
narrative_ontology:measurement(citizenship_tr_t1924, fourteenth_amendment__citizenship_clause, theater_ratio, 1924, 0.1).
narrative_ontology:measurement(citizenship_tr_t1964, fourteenth_amendment__citizenship_clause, theater_ratio, 1964, 0.12).

% Extraction over time
narrative_ontology:measurement(citizenship_be_t1868, fourteenth_amendment__citizenship_clause, base_extractiveness, 1868, 0.08).
narrative_ontology:measurement(citizenship_be_t1924, fourteenth_amendment__citizenship_clause, base_extractiveness, 1924, 0.08).
narrative_ontology:measurement(citizenship_be_t1964, fourteenth_amendment__citizenship_clause, base_extractiveness, 1964, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment__citizenship_clause, identity_coordination).
narrative_ontology:affects_constraint(fourteenth_amendment__citizenship_clause, fourteenth_amendment__due_process_clause).
narrative_ontology:affects_constraint(fourteenth_amendment__citizenship_clause, fourteenth_amendment__equal_protection_clause).
narrative_ontology:affects_constraint(fourteenth_amendment__citizenship_clause, fourteenth_amendment__privileges_or_immunities_clause).
narrative_ontology:affects_constraint(fourteenth_amendment__citizenship_clause, dred_scott_overruling).
narrative_ontology:affects_constraint(fourteenth_amendment__citizenship_clause, denaturalization_doctrine).

% DUAL FORMULATION NOTE:
% The Citizenship Clause is one constraint story extracted from the Fourteenth Amendment kernel. The due_process_clause, equal_protection_clause, and privileges_or_immunities_clause readings are separate constraint stories with different ε values and victim sets, all grounded in the same constitutional text. This is not an observable-dependent classification — the readings are structurally distinct doctrinal claims that happen to coexist in the same amendment. The citizenship reading has minimal extractiveness (0.08, mountain); the due_process and equal_protection readings have higher extractiveness reflecting the ongoing litigation and doctrinal contestation they generate. The privileges_or_immunities reading, hollowed by Slaughter-House, has degraded into a piton or rope depending on perspective.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
