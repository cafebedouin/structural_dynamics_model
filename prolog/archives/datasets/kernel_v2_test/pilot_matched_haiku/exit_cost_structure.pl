% ============================================================================
% CONSTRAINT STORY: exit_cost_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exit_cost_structure, []).

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
 *   constraint_id: exit_cost_structure
 *   human_readable: Exit Cost Structure in Indian Personal Law Regimes
 *   domain: comparative_law/legal_pluralism/constitutional_theory
 *
 * SUMMARY:
 *   India's personal law regime maintains separate marriage, divorce, and
 *   inheritance rules for Hindu, Muslim, Christian, Parsi, and Secular
 *   communities, grounded in Articles 25-28 (religious freedom) of the
 *   Constitution while the Directive Principles call for a Uniform Civil Code
 *   (Article 44). This creates a tangled structure where exit from one regime
 *   to another carries high costs: women seeking divorce under unequal rules
 *   must either convert (religious renunciation + social ostracism), opt into
 *   the Special Marriage Act (requires spousal consent + triggers community
 *   sanctions), or forum-shop (requires legal knowledge and resources). The
 *   constraint exhibits all six DR types from different perspectives. Legal
 *   intermediaries (lawyers, qazis, priests) benefit from the regime's
 *   complexity and experience it as coordination. Community gatekeepers
 *   (ulema, caste councils, church hierarchies) extract compliance through
 *   endogamy enforcement and threat of excommunication. Women seeking exit
 *   experience maximum extraction with minimal alternatives. Secular courts
 *   maintain a degraded function — they claim authority over all regimes but
 *   lack capacity to enforce Shariat uniformly or override community
 *   gatekeepers' social enforcement. The analytical observer risks
 *   naturalizing regime fragmentation as inherent to pluralism, when it is
 *   actually a contingent institutional choice to maintain separate systems
 *   rather than harmonize them.
 *
 * KEY AGENTS:
 *   - Women Seeking Exit: Primary victim (powerless/trapped) — face high costs to switch regimes; conversion requires religious renunciation and social ostracism; Special Marriage Act requires spousal consent; forum-shopping requires legal knowledge and resources
 *   - Inter-Faith Couples: Secondary victim (moderate/constrained) — constrained by legal complexity; choosing one partner's regime disadvantages the other; Special Marriage Act opt-in requires both to renounce religious identity
 *   - Legal Intermediaries: Primary beneficiary (institutional/arbitrage) — lawyers navigate forum-shopping, qazis adjudicate Shariat disputes, priests solemnize Christian marriages; benefit from regime complexity
 *   - Community Gatekeepers: Secondary beneficiary (institutional/constrained) — ulema, caste councils, church hierarchies coordinate community norms while extracting compliance through endogamy enforcement and threat of excommunication
 *   - Secular Courts: Institutional actor (institutional/arbitrage) — claim authority over all regimes but lack enforcement capacity; maintain degraded function through judicial theater
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choice as inherent to pluralism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exit_cost_structure, 0.62).
domain_priors:suppression_score(exit_cost_structure, 0.68).
domain_priors:theater_ratio(exit_cost_structure, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exit_cost_structure, extractiveness, 0.62).
narrative_ontology:constraint_metric(exit_cost_structure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(exit_cost_structure, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exit_cost_structure, tangled_rope).
narrative_ontology:human_readable(exit_cost_structure, "Exit Cost Structure in Indian Personal Law Regimes").
narrative_ontology:topic_domain(exit_cost_structure, "comparative_law/legal_pluralism/constitutional_theory").

domain_priors:requires_active_enforcement(exit_cost_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exit_cost_structure, '71bf3ea4-8ecb-4b66-81f3-ac3a9a425f47').
narrative_ontology:cs_kernel_codification('71bf3ea4-8ecb-4b66-81f3-ac3a9a425f47', distributed).
narrative_ontology:cs_authority_grounding('71bf3ea4-8ecb-4b66-81f3-ac3a9a425f47', extraction).
narrative_ontology:cs_interpretation_layer_present('71bf3ea4-8ecb-4b66-81f3-ac3a9a425f47').
narrative_ontology:cs_reading_relation('71bf3ea4-8ecb-4b66-81f3-ac3a9a425f47', exit_cost_structure__muslim_shariat_reading, forecloses).
narrative_ontology:cs_reading_relation('71bf3ea4-8ecb-4b66-81f3-ac3a9a425f47', exit_cost_structure__christian_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('71bf3ea4-8ecb-4b66-81f3-ac3a9a425f47', exit_cost_structure__parsi_community_reading, coexists_with).
narrative_ontology:cs_reading_relation('71bf3ea4-8ecb-4b66-81f3-ac3a9a425f47', exit_cost_structure__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('71bf3ea4-8ecb-4b66-81f3-ac3a9a425f47', foundational, parliamentary_sovereignty_over_religious_law).
narrative_ontology:cs_axiom_status(parliamentary_sovereignty_over_religious_law, holdable).
narrative_ontology:cs_axiom_grounding('71bf3ea4-8ecb-4b66-81f3-ac3a9a425f47', parliamentary_sovereignty_over_religious_law, deontological).
narrative_ontology:cs_axiom('71bf3ea4-8ecb-4b66-81f3-ac3a9a425f47', secondary, secular_court_authority_over_all_regimes).
narrative_ontology:cs_axiom_status(secular_court_authority_over_all_regimes, holdable).
narrative_ontology:cs_axiom_grounding('71bf3ea4-8ecb-4b66-81f3-ac3a9a425f47', secular_court_authority_over_all_regimes, instrumental).
narrative_ontology:cs_axiom('71bf3ea4-8ecb-4b66-81f3-ac3a9a425f47', secondary, gender_equality_via_statutory_reform).
narrative_ontology:cs_axiom_status(gender_equality_via_statutory_reform, holdable).
narrative_ontology:cs_axiom_grounding('71bf3ea4-8ecb-4b66-81f3-ac3a9a425f47', gender_equality_via_statutory_reform, deontological).
narrative_ontology:cs_reference_frame('71bf3ea4-8ecb-4b66-81f3-ac3a9a425f47', parliamentary_codification_with_secular_oversight).
narrative_ontology:cs_drift_state('71bf3ea4-8ecb-4b66-81f3-ac3a9a425f47', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('71bf3ea4-8ecb-4b66-81f3-ac3a9a425f47', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exit_cost_structure, legal_intermediaries).
narrative_ontology:constraint_beneficiary(exit_cost_structure, community_gatekeepers).
narrative_ontology:constraint_victim(exit_cost_structure, women_seeking_exit).
narrative_ontology:constraint_victim(exit_cost_structure, inter_faith_couples).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMAN SEEKING EXIT (SNARE) — Trapped by high exit costs: conversion requires religious renunciation and social ostracism; Special Marriage Act requires spousal consent and triggers community sanctions; forum-shopping requires legal knowledge and resources. No genuine alternatives. Maximum experienced extraction.
constraint_indexing:constraint_classification(exit_cost_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTER-FAITH COUPLE (TANGLED ROPE) — Constrained by legal complexity and social friction. The regime provides genuine coordination (marriage solemnization, inheritance clarity) but embeds asymmetric extraction: choosing one partner's regime disadvantages the other; Special Marriage Act opt-in requires both to renounce religious identity, creating mutual cost. Moderate extraction with some agency.
constraint_indexing:constraint_classification(exit_cost_structure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGAL INTERMEDIARIES (ROPE) — Institutional beneficiaries with arbitrage options. The regime's complexity creates demand for their services: lawyers navigate forum-shopping, qazis adjudicate Shariat disputes, priests solemnize Christian marriages. They experience the constraint as coordination (solving the problem of adjudicating marriage validity across communities) and benefit from the regime's persistence. Low effective extraction.
constraint_indexing:constraint_classification(exit_cost_structure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMMUNITY GATEKEEPERS (TANGLED ROPE) — Constrained by state authority but benefit from regime fragmentation. They coordinate community marriage norms (genuine function) while extracting social compliance through endogamy enforcement, caste/religious purity rules, and threat of excommunication. Active enforcement required; asymmetric extraction from those seeking to exit community norms.
constraint_indexing:constraint_classification(exit_cost_structure, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SECULAR COURTS (PITON) — Institutional actor maintaining a degraded function. Courts claim authority to adjudicate all marriage regimes (Hindu, Muslim, Christian, Parsi, Secular) but lack capacity to enforce Shariat uniformly, cannot override community gatekeepers' social enforcement, and perform theater of 'constitutional balance' between Articles 25-28 (religious freedom) and Article 44 (UCC directive). The judicial system sees its own role as atrophied — it enforces some regimes effectively (Hindu, Christian, Parsi) but Shariat enforcement remains community-mediated. Theater ratio reflects the gap between judicial authority claims and actual enforcement capacity.
constraint_indexing:constraint_classification(exit_cost_structure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of marriage authority differentiation is inherent to multi-religious societies: different communities have different substantive commitments about marriage (monogamy vs polygamy, divorce grounds, inheritance rules). This perspective sees the regime fragmentation as an immutable feature of religious pluralism itself. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that the 'inherent to pluralism' framing naturalizes what is actually a contingent institutional choice to maintain separate regimes rather than harmonize them.
constraint_indexing:constraint_classification(exit_cost_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exit_cost_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exit_cost_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exit_cost_structure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(exit_cost_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(exit_cost_structure, TR),
    TR >= 0.70.

:- end_tests(exit_cost_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The regime extracts from women seeking exit through high switching costs (conversion, spousal consent, legal complexity). The extraction is not maximal because some exit pathways exist (Special Marriage Act, forum-shopping) and some communities have lower exit costs (Parsi, Christian) than others (Muslim, Hindu). The rising trajectory (0.48 → 0.65 over 30 years) reflects increasing legal complexity and enforcement intensity as courts have clarified regime boundaries and community gatekeepers have strengthened endogamy enforcement. Suppression (0.68): High. Structural barriers include: legal requirement for spousal consent to opt into Special Marriage Act; lack of awareness of forum-shopping options; social sanctions (ostracism, loss of community status, denial of inheritance rights); and community gatekeepers' enforcement of endogamy rules. Suppression is rising (0.55 → 0.70) as community enforcement has intensified in response to perceived threats from secular law and inter-faith marriage. Theater ratio (0.45): Moderate. The constraint has genuine coordination function (adjudicating marriage validity across communities, enabling inheritance clarity) but also performative elements: courts claim authority over Shariat but lack enforcement capacity; the constitutional balance between Articles 25-28 and Article 44 is maintained through judicial theater rather than substantive resolution; community gatekeepers perform 'traditional authority' while actually enforcing modern social control. The rising trajectory (0.38 → 0.47) reflects increasing gap between judicial authority claims and actual enforcement capacity as regime complexity has grown.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across institutional and individual positions. Legal intermediaries see coordination (Rope) — they are solving the legitimate problem of adjudicating marriage validity across communities. Community gatekeepers see mixed coordination and extraction (Tangled Rope) — they coordinate community norms while extracting compliance. Women seeking exit see pure extraction (Snare) — the regime provides no genuine alternatives and all exit pathways carry high costs. Inter-faith couples see mixed coordination and extraction (Tangled Rope) — the regime enables marriage but embeds asymmetric costs. Secular courts see their own degraded function (Piton) — they maintain judicial authority through theater while lacking enforcement capacity. The analytical observer risks seeing an immutable natural law (Mountain) — regime fragmentation as inherent to pluralism — but the structural data reveals this as a false summit: the choice to maintain separate regimes rather than harmonize them is contingent, not necessary.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the extraction flow. Women seeking exit are trapped with no alternatives (d ≈ 1.0, maximum target). Legal intermediaries are institutional beneficiaries with arbitrage options (d ≈ 0.1, maximum beneficiary). Community gatekeepers are institutional beneficiaries constrained by state authority (d ≈ 0.3, moderate beneficiary). Inter-faith couples are moderate agents constrained by legal complexity (d ≈ 0.6, moderate target). Secular courts are institutional beneficiaries with arbitrage options but constrained by constitutional limits (d ≈ 0.2, beneficiary with constraints). The engine derives d from these structural positions and applies the sigmoid f(d) to produce experienced extractiveness chi. The perspectival gap reflects that beneficiaries experience low chi while targets experience high chi from the same constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy: the original mandate (adjudicate marriage validity across communities while respecting religious freedom) has outlived its function. The regime was designed to accommodate religious pluralism while maintaining constitutional secularism, but it has become a mechanism for extracting compliance from those seeking to exit unequal rules. The mandate persists through institutional inertia and constitutional theater (Articles 25-28 vs Article 44) rather than through genuine coordination function. The piton perspective (secular courts maintaining degraded function) and the false-summit perspective (naturalizing regime fragmentation as inherent to pluralism) both reflect mandatrophy. The constraint persists because: (1) community gatekeepers benefit from regime fragmentation and resist harmonization; (2) legal intermediaries benefit from complexity; (3) the constitutional framework is ambiguous enough to permit indefinite maintenance of separate regimes; (4) the political cost of imposing a Uniform Civil Code is high. The mandatrophy is not resolved — the constraint persists despite its original mandate being obsolete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_pluralism,
    'Is regime fragmentation an inherent feature of religious pluralism, or a contingent institutional choice to maintain separate legal systems?',
    'Comparative analysis: jurisdictions with unified civil marriage law (France, Turkey, Tunisia) vs those with maintained pluralism (India, Malaysia, Nigeria). If unified systems function without religious conflict, pluralism is contingent; if they generate backlash, pluralism may be structurally necessary.',
    'If contingent: the mountain perspective is a false summit naturalizing extraction. If necessary: the mountain perspective is correct and the constraint is immutable. Classification shifts from false-summit-tangled-rope to genuine-mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_pluralism, empirical, 'Whether regime fragmentation is inherent to pluralism or contingent institutional choice').

omega_variable(
    exit_cost_measurement_ambiguity,
    'What counts as ''exit cost''? Are social sanctions (ostracism, loss of community status) structural suppression or internalized identity cost?',
    'Post-exit trajectory analysis: do women who convert/opt-in to Special Marriage Act experience suppression that persists after legal exit, or does suppression dissolve once the legal mechanism is removed? If persistent, suppression is internalized (identity-locked); if dissolved, suppression is structural (trapped).',
    'If internalized: the woman''s exit_options should be identity_locked rather than trapped, changing the snare classification to rope at biographical horizon. If structural: trapped classification is correct. Suppression metric may be underestimated if internalized component is not measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_cost_measurement_ambiguity, empirical, 'Whether exit-cost suppression is structural or internalized').

omega_variable(
    shariat_enforcement_mechanism,
    'Is Shariat enforcement primarily state-mediated (qazi courts, judicial review) or community-mediated (social sanctions, informal adjudication)?',
    'Empirical audit of dispute resolution: what percentage of Muslim marriage disputes are adjudicated by state qazi courts vs community councils vs informal settlement? What enforcement mechanisms are actually used (legal sanction vs social pressure)?',
    'If state-mediated: suppression is structural and measurable via judicial capacity. If community-mediated: suppression is diffuse and harder to quantify; the piton classification of courts becomes more accurate (courts claim authority but lack enforcement capacity). Extraction mechanism shifts from legal to social.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(shariat_enforcement_mechanism, empirical, 'Whether Shariat enforcement is state-mediated or community-mediated').

omega_variable(
    special_marriage_act_uptake_barriers,
    'Why is Special Marriage Act uptake so low (< 2% of marriages nationally, < 5% even in urban areas)? Is it exit cost (legal/social barriers) or preference (communities genuinely prefer their own regimes)?',
    'Survey data on reasons for non-uptake: cost barriers, social pressure, lack of awareness, or genuine preference for community law. Comparison of uptake rates across regions with different enforcement intensity (high-enforcement communities vs low-enforcement communities).',
    'If exit cost: suppression metric is accurate and snare classification is correct. If preference: suppression is lower than measured and some agents are not victims (they are choosing their regime). If mixed: suppression is partially internalized (preference-shaped-by-cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(special_marriage_act_uptake_barriers, empirical, 'Whether low SMA uptake reflects exit costs or genuine preference').

omega_variable(
    kernel_reading_foreclosure,
    'Do the Muslim Shariat reading and the Secular Contractual reading logically foreclose each other within a single constitutional framework, or do they coexist as competing readings?',
    'Constitutional interpretation: can Articles 25-28 (religious freedom) and Article 44 (UCC directive) be held simultaneously, or does accepting one require rejecting the other? Does the Constitution itself foreclose one reading?',
    'If foreclosed: one reading must eventually be abandoned (constitutional resolution). If coexisting: the kernel remains contested indefinitely and the constraint persists. Classification implications: foreclosure suggests scaffold (temporary until resolution); coexistence suggests piton (indefinite maintenance of contradictory readings).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether kernel readings logically foreclose each other or coexist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exit_cost_structure, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exit_cost_tr_t0, exit_cost_structure, theater_ratio, 0, 0.38).
narrative_ontology:measurement(exit_cost_tr_t10, exit_cost_structure, theater_ratio, 10, 0.42).
narrative_ontology:measurement(exit_cost_tr_t20, exit_cost_structure, theater_ratio, 20, 0.45).
narrative_ontology:measurement(exit_cost_tr_t30, exit_cost_structure, theater_ratio, 30, 0.47).

% Extraction over time
narrative_ontology:measurement(exit_cost_be_t0, exit_cost_structure, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(exit_cost_be_t10, exit_cost_structure, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(exit_cost_be_t20, exit_cost_structure, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(exit_cost_be_t30, exit_cost_structure, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(exit_cost_su_t0, exit_cost_structure, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(exit_cost_su_t10, exit_cost_structure, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(exit_cost_su_t20, exit_cost_structure, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(exit_cost_su_t30, exit_cost_structure, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exit_cost_structure, identity_coordination).
narrative_ontology:affects_constraint(exit_cost_structure, uniform_civil_code_directive).
narrative_ontology:affects_constraint(exit_cost_structure, triple_talaq_reform).
narrative_ontology:affects_constraint(exit_cost_structure, inter_faith_marriage_recognition).

% DUAL FORMULATION NOTE:
% The exit cost structure is downstream of the constitutional choice to maintain separate personal law regimes. The upstream constraint (regime fragmentation itself) has different extractiveness reflecting the institutional choice; the exit cost structure has its own extractiveness reflecting the asymmetric costs imposed on those seeking to switch regimes. The two constraints are linked: regime fragmentation creates the conditions for high exit costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exit_cost_structure, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
