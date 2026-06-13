% ============================================================================
% CONSTRAINT STORY: udhr_article_3__procedural_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__procedural_hybrid_reading, []).

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
 *   constraint_id: udhr_article_3__procedural_hybrid_reading
 *   human_readable: Article 3 Procedural Due Process and Torture Prohibition (Hybrid Reading)
 *   domain: constitutional/human_rights
 *
 * SUMMARY:
 *   Article 3 of the UDHR and its codification in binding treaties (ICCPR,
 *   ECHR, CAT) guarantees a specific bundle of procedural protections: habeas
 *   corpus availability, prohibition on torture and degrading treatment,
 *   access to judicial review of detention legality. This constraint story
 *   represents the PROCEDURAL HYBRID READING: the claim that Article 3
 *   provides due process machinery without resolving the underlying contest
 *   between negative liberty (freedom from state violence as the core liberty
 *   interest) and positive entitlement (state obligation to provide material
 *   conditions for life and security). The reading acknowledges that courts
 *   and states IMPLEMENT Article 3 through procedural requirements—judges
 *   review detention, torture allegations are investigated, habeas petitions
 *   are filed—while leaving open what liberty or welfare goods the state is
 *   ultimately obligated to respect or provide. This is distinct from and
 *   coexists with: (1) the negative_liberty_reading, which interprets Article
 *   3 as primarily protecting freedom FROM state coercion, making detention
 *   and torture the violations to prevent; and (2) the
 *   positive_entitlement_reading, which argues Article 3 obligates the state
 *   to provide the material conditions (food, shelter, security) necessary
 *   for life and liberty to be meaningful. The procedural hybrid reading is
 *   the institutional solution adopted in practice: courts focus on whether
 *   detention procedures were followed, whether torture occurred, whether
 *   judicial review was available—WITHOUT DECIDING whether the state's
 *   ultimate duty is to minimize coercion (negative) or to maximize welfare
 *   provision (positive).
 *
 * KEY AGENTS:
 *   - detained_persons: powerless, trapped, immediate horizon — benefit from procedural access to courts and torture prohibition but cannot exit detention once initiated
 *   - state_security_apparatus: institutional, constrained exit, national scope — bear operational cost of mandatory review windows and torture prohibition but cannot abandon treaty obligations
 *   - national_judiciaries: institutional, constrained exit, national scope — set and administer procedural machinery, derive legitimacy from rule-of-law authority, cannot remove procedural requirements without legislative change
 *   - international_monitoring_bodies: organized, mobile, global scope — observe compliance and issue recommendations but have limited enforcement power
 *   - affected_families_advocates: organized, constrained exit, national scope — benefit from legal mechanisms to challenge detention but exit means abandoning detained relatives
 *   - population_in_security_emergencies: excluded, moderate power, trapped during crises — would prioritize speed over procedure but their voice is structurally muted in judicial proceedings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, 0.42).
domain_priors:suppression_score(udhr_article_3__procedural_hybrid_reading, 0.38).
domain_priors:theater_ratio(udhr_article_3__procedural_hybrid_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__procedural_hybrid_reading, rope).
narrative_ontology:human_readable(udhr_article_3__procedural_hybrid_reading, "Article 3 Procedural Due Process and Torture Prohibition (Hybrid Reading)").
narrative_ontology:topic_domain(udhr_article_3__procedural_hybrid_reading, "constitutional/human_rights").

domain_priors:requires_active_enforcement(udhr_article_3__procedural_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__procedural_hybrid_reading, '624177df-734f-4e9a-8cbf-f6960bc1418d').
narrative_ontology:cs_kernel_codification('624177df-734f-4e9a-8cbf-f6960bc1418d', fixed_text).
narrative_ontology:cs_authority_grounding('624177df-734f-4e9a-8cbf-f6960bc1418d', lineage).
narrative_ontology:cs_interpretation_layer_present('624177df-734f-4e9a-8cbf-f6960bc1418d').
narrative_ontology:cs_reading_relation('624177df-734f-4e9a-8cbf-f6960bc1418d', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('624177df-734f-4e9a-8cbf-f6960bc1418d', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_axiom('624177df-734f-4e9a-8cbf-f6960bc1418d', foundational, procedural_legitimacy_neutral_on_substance).
narrative_ontology:cs_axiom_status(procedural_legitimacy_neutral_on_substance, holdable).
narrative_ontology:cs_axiom_grounding('624177df-734f-4e9a-8cbf-f6960bc1418d', procedural_legitimacy_neutral_on_substance, instrumental).
narrative_ontology:cs_axiom('624177df-734f-4e9a-8cbf-f6960bc1418d', secondary, derogation_authority_over_procedure).
narrative_ontology:cs_axiom_status(derogation_authority_over_procedure, holdable).
narrative_ontology:cs_axiom_grounding('624177df-734f-4e9a-8cbf-f6960bc1418d', derogation_authority_over_procedure, conventional).
narrative_ontology:cs_reference_frame('624177df-734f-4e9a-8cbf-f6960bc1418d', rule_of_law_through_procedural_constraint).
narrative_ontology:cs_drift_state('624177df-734f-4e9a-8cbf-f6960bc1418d', contemporary_security_era_post_2001, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('624177df-734f-4e9a-8cbf-f6960bc1418d', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(udhr_article_3__procedural_hybrid_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, detained_persons).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, judicial_systems).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, civil_society_monitors).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__procedural_hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(udhr_article_3__procedural_hybrid_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__procedural_hybrid_reading_tests).
:- end_tests(udhr_article_3__procedural_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the procedural constraint genuinely limits security apparatus discretion (habeas review blocks some detention, torture cases produce accountability) but does NOT resolve substantive questions about how much detention or what liberty/welfare goods the state owes. The constraint operates at the procedural layer, which means: (1) security apparatus cannot hold persons indefinitely without review, cannot systematically torture with impunity, but CAN often obtain judicial authorization for detention because courts typically defer to security judgments once procedures are followed. (2) Detained persons benefit from access mechanisms but do not necessarily win release—the procedure may routinize rather than reverse state detention decisions. The measurement series shows extractiveness rising modestly (0.28 to 0.42) as enforcement machinery matures and courts gain capacity, but the rise is shallow because the ceiling of the constraint is set by its procedural scope—it can regulate HOW detention happens but not WHETHER detention should happen (that is the unresolved substantive question). Theater_ratio rises slowly (0.15 to 0.28) because procedural machinery requires real institutional resources (courts, judges, legal representation) and produces real-world effects (some detainees are released, torture cases are prosecuted), but the ratio still measures the growing gap between the procedural formalism (courts say detention is lawful if procedures were followed) and the material outcome (person remains detained, substantive liberty question unresolved). Suppression remains moderate and stable (0.32-0.38) because the constraint does suppress outright arbitrary detention but does not suppress the state's ability to detain for security reasons once it clears procedural hurdles. All measurements reflect ONE TIME GRID (every metric authored at t=0, 10, 20, 30) so temporal alignment is enforced.
 *
 * PERSPECTIVAL GAP:
 *   The seated divergence is stark: From the detained person's seat, the constraint is a life-or-death gate—access to habeas petition and torture prohibition can block indefinite detention or prevent systematic abuse. From the security apparatus seat, the constraint is a procedural overhead—security judgments are regularly upheld after judicial review, so the apparatus retains operational discretion. From the judicial seat, the constraint is a legitimacy machinery—courts can claim authority from rule-of-law principle while authorizing the security apparatus's detention decisions, satisfying both accountability norms and security interests. From the international monitoring seat, the constraint is spotty—some states comply seriously, others perform compliance theatrically. From the excluded emergency-population seat, the constraint is an obstruction—procedural requirements slow response to imminent threats. The engine should compute radically different types from different seats: detainees likely perceive a mountain (the constraint is the irreducible bedrock of law protecting them) or rope (real benefit from procedure); security apparatus perceive a rope (genuine coordination benefit—detention authority + rule-of-law legitimacy) or snare (the constraint as coercive imposition of judicial review overhead); courts perceive rope (they set it, benefit from it); monitors perceive rope (coordination function works, compliance is variable). No single type fits all seats. The claim (rope) matches the coordinator seats (courts, possibly international bodies) but not the target seat (security apparatus, under duress to comply) or the beneficiary seat (detainees, for whom it is existential rather than coordinating).
 *
 * DIRECTIONALITY LOGIC:
 *   Detained persons are structural beneficiaries (d toward 0.0): they gain access to courts, torture prohibition, judicial review—these constrain state violence. However, their benefit is bounded by the procedural scope: they do not gain substantive protection of liberty or welfare, only procedural machinery to contest detention. State security apparatus are payers (d toward 1.0): they bear the cost of procedural compliance, judicial review delays, torture prohibition enforcement. However, they are not targets of pure extraction because they retain substantial discretion to detain for security reasons once procedures are observed. National judiciaries sit near symmetric (d near 0.5): they benefit institutionally from the legitimacy and authority that comes from administering rule-of-law procedure, but they also bear the cost of resource commitment and political pressure from security apparatus and security-conscious publics. The procedural framing allows courts to appear as neutral arbiters while implicitly privileging the security apparatus through high authorization rates and deference to security judgments. Monitoring bodies and advocates have low d (near beneficiary) because they gain institutional platform and legitimacy, but their leverage is constrained—they can publicize violations but not override judicial or security decisions. Population_in_emergencies are excluded from directionality calculation (they do not participate in the constraint structure) but their exclusion is the point—the procedural machinery does not represent security-threat perceptions, only judicial review of state action.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and contested: arbitrary detention without trial and systematic torture ARE ongoing problems in non-compliant states and in compliant states during security emergencies. The constraint was built to solve this by creating procedural gates (habeas, review, torture prohibition). The mandatrophy risk is moderate: the procedural machinery can become theater if courts routinely authorize detention after finding procedures were followed, reducing the substantive constraint on state detention authority. This is visible in the measurement series: extractiveness and theater_ratio both rise over the interval, suggesting the procedural machinery is maturing (more complete, more widely used) but also becoming more routinized (fewer substantive surprises, more predictable outcomes for security apparatus). The constraint prevents the WORST outcome (indefinite detention without any review, systematic torture) but does not prevent the MIDDLE outcome (detention with procedural review that typically authorizes it, documented torture allegations that are investigated but rarely prosecuted). A mandatrophy verdict would require evidence that the procedural machinery has become pure formalism—that courts authorize virtually all detention once procedures are observed, that torture investigations never produce accountability, that the machinery's sole function is legitimating state detention rather than constraining it. The current metrics (moderate extractiveness, moderate theater) suggest the constraint is functioning but imperfectly—it provides real gates but gates that are frequently opened by the judiciary in deference to security. This is not yet mandatrophy, but it is the trajectory: as enforcement machinery matures and security apparatus learns to work within procedural requirements, the substantive constraint may degrade into procedural formalism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantive_vs_procedural_boundary,
    'Does Article 3''s procedural machinery (habeas, torture prohibition, review availability) succeed in constraining substantive detention scope, or does it become a formality ratifying security apparatus decisions while providing the appearance of constraint?',
    'Empirical analysis of habeas grant rates, torture complaint investigation outcomes, and average detention duration under the constraint versus counterfactual regimes without procedural gates. Comparison of de jure procedural requirements with de facto judicial behavior during security crises.',
    'If procedures routinely vindicate state detention decisions, the constraint approximates theater (high theater_ratio, low actual constraint on power). If procedures regularly block or shorten detention, the constraint performs genuine coordination. Determines whether the reading is actually a rope (coordination with procedural cost) or a tangled_rope (coordination cover for delegated extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantive_vs_procedural_boundary, empirical, 'Whether procedural machinery constrains substantive detention authority or routinizes state detention.').

omega_variable(
    torture_definition_scope,
    'What counts as torture under Article 3, and who decides? Does the torture prohibition extend to ''cruel, inhuman, degrading treatment'' or only to intentional severe pain infliction?',
    'Legal interpretation by international courts (European Court of Human Rights torture jurisprudence, UN CAT case law). Empirical documentation of interrogation techniques classified as torture versus degrading treatment across jurisdictions.',
    'Narrow reading of torture (only severe intentional pain) leaves aggressive interrogation, isolation, sleep deprivation as unregulated. Broad reading (includes degrading treatment) expands the constraint. The scope of the torture prohibition directly affects suppression intensity and extraction amplitude—broader definition = higher constraint enforcement burden on security apparatus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(torture_definition_scope, conceptual, 'The scope of torture prohibition as interpreter-dependent.').

omega_variable(
    emergency_suspension_availability,
    'Can the procedural requirements of Article 3 be suspended or derogated during declared emergencies, and under what conditions?',
    'Analysis of state derogation practice under ICCPR Article 4 and ECHR Article 15; examination of which procedural elements remain non-derogable (absolute prohibition on torture, right to life) versus derogable (habeas availability, bail presumption).',
    'If habeas and procedural review are suspendable during emergencies, the constraint''s enforcement evaporates precisely when detention most threatens arbitrary use. If torture prohibition alone remains non-derogable, the constraint offers minimal protection. Determines whether the constraint is structurally robust or fragile under pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergency_suspension_availability, empirical, 'Whether procedural guarantees survive state-declared emergencies.').

omega_variable(
    reading_coexistence_mechanism,
    'How do the negative liberty reading (freedom from state violence), positive entitlement reading (right to state-provided security goods), and procedural hybrid reading coexist in practice without resolution of the substantive liberty/welfare contest?',
    'Institutional analysis of how courts adjudicate Article 3 cases: do they reach substantive liberty or welfare claims, or do they systematize decisions via procedural requirements alone? Study whether the procedural hybrid reading is genuinely neutral on substance or silently privileges one substantive reading.',
    'If courts remain procedurally neutral without resolving substance, the three readings genuinely coexist. If courts implicitly privilege one substantive reading (e.g., by requiring states to justify detention via security harm, which presumes negative-liberty-centric legitimacy), the procedural reading is not neutral but architecturally privileges one substantive view. Affects whether the reading is genuinely a hybrid or a procedural wrapper for hidden substantive choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_mechanism, conceptual, 'Whether procedural framework is truly neutral on substantive liberty/welfare contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__procedural_hybrid_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__procedural_hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(udhr_tr_t10, udhr_article_3__procedural_hybrid_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(udhr_tr_t20, udhr_article_3__procedural_hybrid_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(udhr_tr_t30, udhr_article_3__procedural_hybrid_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(udhr_be_t10, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(udhr_be_t20, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(udhr_be_t30, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(udhr_su_t10, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(udhr_su_t20, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(udhr_su_t30, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__procedural_hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_article_3__procedural_hybrid_reading, 0.12).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, national_security_exception_doctrine).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, state_emergency_powers_framework).

% DUAL FORMULATION NOTE:
% Article 3 UDHR decomposes into three structurally distinct constraints corresponding to three readings of the contested kernel: negative_liberty_reading (freedom from state coercion), positive_entitlement_reading (right to state-provided welfare), procedural_hybrid_reading (due process machinery neutral on substance). The three readings are linked by network.affects_constraints. The procedural_hybrid_reading (this file) coexists with both substantive readings in practice—courts implement Article 3 via procedural machinery while different parties assert incompatible substantive interpretations underneath. Epsilon values differ sharply: negative reading has low extraction (state merely constrained from violence = public good with minimal distributional conflict); positive reading has high extraction (state obligated to redistribute resources = contested transfer); procedural reading has moderate extraction (machinery has real cost but does not resolve what is being protected). The three stories are not observations of one constraint from different angles—they are three genuinely different constraints sharing a single textual kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__procedural_hybrid_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
