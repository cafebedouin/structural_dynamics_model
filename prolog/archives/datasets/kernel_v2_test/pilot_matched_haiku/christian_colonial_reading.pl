% ============================================================================
% CONSTRAINT STORY: christian_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_christian_colonial_reading, []).

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
 *   constraint_id: christian_colonial_reading
 *   human_readable: Christian Colonial Marriage Authority: Ecclesiastical Tradition Enforced by Secular Courts
 *   domain: comparative_law/legal_pluralism/constitutional_theory
 *
 * SUMMARY:
 *   In post-colonial India, marriage law exhibits a unique legal pluralism:
 *   different religious communities are governed by different personal law
 *   codes (Christian, Hindu, Muslim, Parsi), each with its own rules for
 *   marriage, divorce, and succession. This constraint story focuses on ONE
 *   reading of the contested marriage authority kernel: the Christian
 *   colonial reading, which derives marriage authority from ecclesiastical
 *   tradition as codified in the Indian Divorce Act of 1869 (inherited from
 *   British colonial law) and enforced by secular courts. The Christian
 *   colonial reading grounds marriage authority in the doctrine of the
 *   Christian church (indissolubility of marriage except for adultery) and
 *   treats the secular state's role as enforcement of that doctrine. This
 *   reading coexists with four sibling readings: the Hindu codified reading
 *   (grounded in Hindu scripture and the Hindu Marriage Act of 1955), the
 *   Muslim Shariat reading (grounded in Islamic law and the Muslim Personal
 *   Law of 1937), the Parsi community reading (grounded in Parsi custom and
 *   the Parsi Marriage and Divorce Act of 1936), and the secular contractual
 *   reading (grounded in contract law and civil marriage statutes). The
 *   Christian colonial reading is the oldest and most archaic: it prohibited
 *   divorce entirely until the 2001 amendment introduced limited no-fault
 *   grounds. The constraint exhibits all six DR types from different
 *   perspectives, making it a diagnostic exemplar for how legal pluralism
 *   creates perspectival gaps. The same structural phenomenon — the
 *   codification of ecclesiastical doctrine into secular law — appears as
 *   pure extraction (snare) to women seeking divorce, as mixed
 *   coordination-extraction (tangled rope) to religious minorities navigating
 *   plural legal systems, as pure coordination (rope) to ecclesiastical
 *   authorities whose doctrine is enforced by the state, as a temporary
 *   problem being solved by legal reform (scaffold) to reform coalitions, as
 *   a degraded ritual (piton) to courts that enforce doctrine they question,
 *   or as an immutable feature of legal continuity (mountain) to those who
 *   naturalize colonial-era statutes. The 2001 amendment introducing no-fault
 *   divorce grounds represents a partial sunset of the Christian colonial
 *   reading, though the reading persists in modified form.
 *
 * KEY AGENTS:
 *   - Women seeking divorce: Primary victim (powerless/trapped) — ecclesiastical doctrine prohibits divorce except for adultery; secular courts enforce this prohibition; no alternative legal pathway until 2001 amendment
 *   - Ecclesiastical authority (Church leadership): Primary beneficiary (institutional/arbitrage) — benefits from state enforcement of church doctrine; experiences constraint as pure coordination
 *   - Secular courts (judicial system): Secondary beneficiary and victim (institutional/constrained) — benefits from inherited statute (reduces need for legislative action) but constrained by obligation to enforce archaic doctrine that conflicts with constitutional values
 *   - Religious minorities seeking personal law autonomy: Secondary victim (moderate/constrained) — constrained by Christian colonial framework's dominance but also benefit from parallel personal law system
 *   - Legal reform coalition: Organized agents (organized/mobile) — women's rights groups, constitutional scholars, reform-minded legislators; see constraint as temporary with declared sunset via 2001 amendment
 *   - Colonial legal continuity doctrine: Institutional actor (institutional/arbitrage) — maintains performative enforcement of inherited statute; sees own doctrine as archaic but persists through institutional inertia
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choice to preserve colonial-era doctrine as inherent feature of legal continuity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(christian_colonial_reading, 0.52).
domain_priors:suppression_score(christian_colonial_reading, 0.48).
domain_priors:theater_ratio(christian_colonial_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(christian_colonial_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(christian_colonial_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(christian_colonial_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(christian_colonial_reading, tangled_rope).
narrative_ontology:human_readable(christian_colonial_reading, "Christian Colonial Marriage Authority: Ecclesiastical Tradition Enforced by Secular Courts").
narrative_ontology:topic_domain(christian_colonial_reading, "comparative_law/legal_pluralism/constitutional_theory").

domain_priors:requires_active_enforcement(christian_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(christian_colonial_reading, '059dcff1-4965-4617-aaf8-dd85c8c15845').
narrative_ontology:cs_kernel_codification('059dcff1-4965-4617-aaf8-dd85c8c15845', fixed_text).
narrative_ontology:cs_authority_grounding('059dcff1-4965-4617-aaf8-dd85c8c15845', extraction).
narrative_ontology:cs_interpretation_layer_present('059dcff1-4965-4617-aaf8-dd85c8c15845').
narrative_ontology:cs_reading_relation('059dcff1-4965-4617-aaf8-dd85c8c15845', christian_colonial_reading__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('059dcff1-4965-4617-aaf8-dd85c8c15845', christian_colonial_reading__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('059dcff1-4965-4617-aaf8-dd85c8c15845', christian_colonial_reading__parsi_community_reading, coexists_with).
narrative_ontology:cs_reading_relation('059dcff1-4965-4617-aaf8-dd85c8c15845', christian_colonial_reading__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('059dcff1-4965-4617-aaf8-dd85c8c15845', foundational, marriage_indissolubility_except_adultery).
narrative_ontology:cs_axiom_status(marriage_indissolubility_except_adultery, overridden).
narrative_ontology:cs_axiom_grounding('059dcff1-4965-4617-aaf8-dd85c8c15845', marriage_indissolubility_except_adultery, deontological).
narrative_ontology:cs_axiom('059dcff1-4965-4617-aaf8-dd85c8c15845', foundational, ecclesiastical_authority_over_personal_law).
narrative_ontology:cs_axiom_status(ecclesiastical_authority_over_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('059dcff1-4965-4617-aaf8-dd85c8c15845', ecclesiastical_authority_over_personal_law, conventional).
narrative_ontology:cs_reference_frame('059dcff1-4965-4617-aaf8-dd85c8c15845', ecclesiastical_indissolubility_doctrine).
narrative_ontology:cs_drift_state('059dcff1-4965-4617-aaf8-dd85c8c15845', contemporary_2025, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('059dcff1-4965-4617-aaf8-dd85c8c15845', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(christian_colonial_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(christian_colonial_reading, ecclesiastical_authority_holders).
narrative_ontology:constraint_beneficiary(christian_colonial_reading, male_household_heads).
narrative_ontology:constraint_beneficiary(christian_colonial_reading, colonial_legal_continuity_doctrine).
narrative_ontology:constraint_victim(christian_colonial_reading, women_seeking_divorce).
narrative_ontology:constraint_victim(christian_colonial_reading, religious_minorities).
narrative_ontology:constraint_victim(christian_colonial_reading, secular_legal_reform_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMAN SEEKING DIVORCE (SNARE) — Trapped by ecclesiastical doctrine (no divorce grounds except adultery until 2001 amendment) and secular court enforcement of that doctrine. Exit from marriage is structurally unavailable; suppression is enforced through both religious authority and state machinery. Maximum extraction: the constraint forces continuation of marriage against her will, with no alternative legal pathway. Biographical time horizon reflects the lived experience of a marriage spanning decades with no exit.
constraint_indexing:constraint_classification(christian_colonial_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RELIGIOUS MINORITY SEEKING AUTONOMY (TANGLED ROPE) — Constrained by the Christian colonial framework's dominance but also benefits from the parallel personal law system (Hindu Code, Muslim Personal Law, Parsi law) that emerged as a coordination mechanism to manage religious pluralism. The constraint coordinates religious community identity while extracting through subordination to Christian-derived precedent. Generational time horizon reflects the institutional persistence of the personal law system across generations.
constraint_indexing:constraint_classification(christian_colonial_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ECCLESIASTICAL AUTHORITY (ROPE) — Benefits from the constraint's codification of church doctrine into secular law. The church experiences the constraint as pure coordination: the state enforces what the church teaches, eliminating the need for the church to maintain its own enforcement machinery. Immediate time horizon reflects the operational benefit of state enforcement in the present moment. Arbitrage exit reflects the church's ability to influence legal reform or maintain parallel authority structures.
constraint_indexing:constraint_classification(christian_colonial_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SECULAR COURTS (TANGLED ROPE) — Constrained by the inherited colonial statute that mandates enforcement of ecclesiastical doctrine. The courts coordinate the legal system's operation (coordination function) while extracting through the burden of enforcing archaic doctrine that conflicts with contemporary constitutional values (gender equality, freedom of religion). Constrained exit reflects the courts' inability to simply ignore the statute without legislative amendment. Biographical time horizon reflects the courts' lived experience of this constraint across decades of case law.
constraint_indexing:constraint_classification(christian_colonial_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGAL REFORM COALITION (SCAFFOLD) — Organized actors (women's rights groups, constitutional scholars, reform-minded legislators) see the constraint as a temporary coordination failure with a declared sunset: the 2001 amendment introducing no-fault divorce grounds represents the beginning of the sunset. The coalition has agency and a clear exit path through legislative reform. Mobile exit reflects the coalition's ability to organize, mobilize public opinion, and drive legislative change. Generational time horizon reflects the multi-decade struggle for reform.
constraint_indexing:constraint_classification(christian_colonial_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: COLONIAL LEGAL CONTINUITY DOCTRINE (PITON) — The doctrine that inherited colonial statutes remain valid absent explicit repeal has become largely performative. Courts invoke the doctrine to justify enforcement while simultaneously acknowledging its archaic character. The constraint persists through institutional inertia and the high transaction costs of legislative amendment, not because the doctrine is functionally necessary. Theater ratio is moderate (0.35) because the constraint retains genuine enforcement function, but the performative element is substantial: courts enforce doctrine they openly question.
constraint_indexing:constraint_classification(christian_colonial_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some legal continuity is inherent to institutional stability: legal systems cannot function if every statute is perpetually revisable. This perspective sees the constraint as an immutable property of how legal systems maintain coherence. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that 'legal continuity' naturalizes what is actually a contingent institutional choice to preserve colonial-era doctrine rather than undertake comprehensive reform.
constraint_indexing:constraint_classification(christian_colonial_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(christian_colonial_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(christian_colonial_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(christian_colonial_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(christian_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(christian_colonial_reading, TR),
    TR >= 0.70.

:- end_tests(christian_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate. The constraint extracts from women seeking divorce (high extraction) and religious minorities (moderate extraction) but benefits ecclesiastical authorities (negative extraction / subsidy). The aggregate extractiveness reflects the asymmetry: women bear substantial costs (trapped in unwanted marriages), while the church captures benefits (doctrine enforced by state). The measurement trajectory shows declining extractiveness over 75 years (0.68 → 0.32), driven primarily by the 2001 amendment and subsequent judicial reinterpretation. Suppression (0.48): Moderate. Significant barriers to exit include ecclesiastical doctrine (no divorce grounds except adultery until 2001), secular court enforcement of that doctrine, social stigma against divorce, and economic dependency. However, suppression is not total: some women can and do obtain divorces through adultery grounds or by establishing cruelty (a judicially-created exception). The measurement trajectory shows declining suppression (0.72 → 0.18) as the 2001 amendment and subsequent reforms reduced barriers. Theater ratio (0.35): Moderate-low. The constraint retains genuine enforcement function (courts actually enforce the doctrine), but the performative element is substantial: courts enforce doctrine they openly question as archaic and unconstitutional. The rising trajectory (0.15 → 0.42) reflects increasing performative character as the doctrine becomes more obviously at odds with constitutional values and social norms, yet persists through institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from a single set of base properties. Women seeking divorce see pure extraction (Snare) — the constraint forces continuation of marriage against their will with no exit. Ecclesiastical authorities see pure coordination (Rope) — the state enforces what the church teaches. Religious minorities see mixed coordination-extraction (Tangled Rope) — the constraint coordinates religious community identity while subordinating minority readings to the Christian colonial reading. Secular courts see mixed coordination-extraction (Tangled Rope) — the constraint coordinates the legal system while extracting through the burden of enforcing archaic doctrine. Legal reform coalitions see a temporary problem with a sunset (Scaffold) — the 2001 amendment and subsequent reforms are building alternative pathways. The colonial legal continuity doctrine sees its own degraded ritual (Piton) — the doctrine persists through inertia, not function. The civilizational analytical observer risks seeing an immutable feature of legal continuity (Mountain) — legal systems require some continuity — but the structural data reveals this as a false summit: the choice to preserve colonial-era doctrine is contingent, not necessary.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position relative to the constraint. Women seeking divorce are trapped (no exit) and victims (bear costs) → high d → high effective extraction. Ecclesiastical authorities are institutional (powerful) and beneficiaries (collect benefits) → low d → negative effective extraction (subsidy). Religious minorities are moderate power and victims (subordinated to Christian colonial reading) → moderate-high d → moderate effective extraction. Secular courts are institutional (powerful) but constrained (cannot ignore statute) and both beneficiaries (statute reduces legislative burden) and victims (forced to enforce archaic doctrine) → moderate d → moderate effective extraction. Legal reform coalitions are organized (powerful) and mobile (can exit through legislative reform) → low d → low effective extraction. The colonial legal continuity doctrine is institutional (powerful) and beneficiary (statute persists) but increasingly performative → moderate d with rising theater component. The analytical observer at civilizational scope risks naturalizing the constraint as immutable, but the structural data shows it is contingent on the choice to preserve colonial-era statutes.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits classic mandatrophy: the founding mandate (enforce ecclesiastical doctrine on Christian marriages) has outlived its function in a secular, pluralistic constitutional democracy. The constraint persists not because the mandate is still valid but because the transaction costs of legislative amendment are high and the beneficiaries (ecclesiastical authorities, institutional inertia) have incentives to maintain the status quo. The 2001 amendment introducing no-fault divorce grounds represents a partial resolution of mandatrophy: the amendment acknowledges that the original mandate (indissolubility except for adultery) is no longer valid, but the constraint persists in modified form. The measurement trajectory shows declining extractiveness and suppression as the mandate becomes increasingly obsolete, but the constraint does not disappear because the statute remains on the books and courts continue to enforce it. The piton perspective captures this dynamic: the constraint is maintained through institutional inertia and performative enforcement, not because it serves a current function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ecclesiastical_doctrine_vs_secular_law_boundary,
    'Is the constraint fundamentally about enforcing ecclesiastical doctrine, or about the secular state''s choice to preserve colonial-era statutes?',
    'Comparative analysis: examine whether courts enforce the doctrine because it is ecclesiastical (religious authority) or because it is codified in statute (legal positivism). Test via hypothetical: if the statute were amended to remove ecclesiastical grounds but retain secular grounds, would the constraint persist? If yes, the constraint is about statutory preservation, not ecclesiastical authority.',
    'If ecclesiastical: the constraint is a religious establishment issue (Establishment Clause violation in some constitutional frameworks). If statutory: the constraint is a legislative inertia issue (mandatrophy). Different reform pathways follow from each diagnosis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ecclesiastical_doctrine_vs_secular_law_boundary, conceptual, 'Whether constraint enforces ecclesiastical doctrine or preserves colonial statute').

omega_variable(
    gender_asymmetry_source,
    'Is the gender asymmetry (women bear higher exit costs) intrinsic to the ecclesiastical doctrine, or a contingent feature of how the doctrine was codified and enforced?',
    'Historical analysis of ecclesiastical doctrine pre-codification vs. post-codification; comparison with how other religious traditions (Hindu, Muslim, Parsi) codified their personal law and whether gender asymmetries differ; examination of whether secular courts have interpreted the doctrine to reduce gender asymmetry.',
    'If intrinsic: reform requires doctrinal change within the ecclesiastical tradition. If contingent: reform can proceed through judicial reinterpretation or statutory amendment without challenging the doctrine itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_asymmetry_source, empirical, 'Source of gender asymmetry in divorce grounds').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the marriage authority kernel. What makes this reading (Christian colonial) distinct from the sibling readings (Hindu codified, Muslim Shariat, Parsi community, secular contractual)?',
    'Structural comparison: each reading grounds marriage authority in a different source (ecclesiastical tradition, Hindu scripture, Islamic law, Parsi custom, secular contract). The readings coexist in the same legal system but occupy different communities. The contest is not whether one reading is ''correct'' but which reading governs which population and whether the state enforces one reading''s doctrine on populations that hold other readings.',
    'If readings are genuinely coexistent (each governs its own community): the constraint is a coordination mechanism for religious pluralism. If one reading dominates (Christian colonial reading enforced on all): the constraint is an establishment of religion. The 2001 amendment moved toward convergence (all communities now have no-fault divorce), reducing the reading contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading contest: Christian colonial vs. sibling readings').

omega_variable(
    id_2001_amendment_sunset_completeness,
    'Does the 2001 amendment introducing no-fault divorce grounds constitute a genuine sunset of the Christian colonial reading, or merely a partial erosion?',
    'Textual analysis: examine whether the amendment applies uniformly to all Christian denominations or only to some; whether it applies to all marriage types (civil, ecclesiastical, customary) or only some; whether courts have interpreted the amendment to override ecclesiastical doctrine or merely to supplement it. Empirical analysis: track divorce rates and grounds cited before and after 2001 to measure actual behavioral change.',
    'If genuine sunset: the Christian colonial reading is in terminal decline, and the constraint should be reclassified as Scaffold with a near-term sunset. If partial: the reading persists in modified form, and the constraint remains Tangled Rope with extended timeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(id_2001_amendment_sunset_completeness, empirical, 'Whether 2001 amendment constitutes genuine sunset of Christian colonial reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(christian_colonial_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccr_theater_1950, christian_colonial_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ccr_theater_1975, christian_colonial_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(ccr_theater_2000, christian_colonial_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(ccr_theater_2001_post_amendment, christian_colonial_reading, theater_ratio, 51, 0.38).
narrative_ontology:measurement(ccr_theater_2025, christian_colonial_reading, theater_ratio, 75, 0.42).

% Extraction over time
narrative_ontology:measurement(ccr_extractiveness_1950, christian_colonial_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(ccr_extractiveness_1975, christian_colonial_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(ccr_extractiveness_2000, christian_colonial_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(ccr_extractiveness_2001_post_amendment, christian_colonial_reading, base_extractiveness, 51, 0.38).
narrative_ontology:measurement(ccr_extractiveness_2025, christian_colonial_reading, base_extractiveness, 75, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(ccr_suppression_1950, christian_colonial_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(ccr_suppression_1975, christian_colonial_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(ccr_suppression_2000, christian_colonial_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement(ccr_suppression_2001_post_amendment, christian_colonial_reading, suppression_requirement, 51, 0.28).
narrative_ontology:measurement(ccr_suppression_2025, christian_colonial_reading, suppression_requirement, 75, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(christian_colonial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(christian_colonial_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(christian_colonial_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(christian_colonial_reading, parsi_community_reading).
narrative_ontology:affects_constraint(christian_colonial_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% The marriage authority kernel decomposes into five structurally distinct constraint stories, one for each reading. Each reading has its own ε value, beneficiary/victim structure, and classification. The Christian colonial reading is the oldest and most archaic; the secular contractual reading is the newest and most egalitarian. The readings coexist in the same legal system but have different temporal trajectories and different reform pressures. All five stories are linked via network.affects_constraints to show the kernel family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(christian_colonial_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
