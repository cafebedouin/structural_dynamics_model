% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__universality_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__universality_paradox_reading, []).

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
 *   constraint_id: unconditional_income_support__universality_paradox_reading
 *   human_readable: Unconditional Income Support: Universality Paradox Reading
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   Unconditional income support (UBS) as a policy category is characterized
 *   by radical ambiguity across ideological boundaries. The left supports it
 *   as an autonomy-enabling floor that removes coercive labor market dynamics
 *   and eliminates welfare stigma. The right supports it as a replacement for
 *   welfare bureaucracy that respects individual choice and aligns with
 *   market incentives. Despite incompatible normative justifications,
 *   empirical research on taxing-back mechanisms shows that distributional
 *   outcomes converge across ideologically incompatible designs. This
 *   constraint models UBS from the universality-paradox reading: the policy's
 *   legitimacy depends on maintaining rhetorical ambiguity that masks the
 *   fact that implementation paths (generous with high tax-back vs austere
 *   with low tax-back) produce similar fiscal outcomes. Political
 *   entrepreneurs benefit from this ambiguity because it enables coalition
 *   formation without requiring agreement on what the policy actually does.
 *   Policy designers benefit because taxing-back mechanisms create rhetorical
 *   flexibility: the same final-outcome income distribution can be justified
 *   as either 'generous autonomy support' or 'austere incentive-compatible
 *   alternative' depending on the audience. The constraint's victims are
 *   targeted program recipients (whose means-tested benefits are cut in favor
 *   of universal payments at lower total levels) and ideological clarity
 *   itself (the public discourse cannot settle on what the policy actually
 *   accomplishes because the ambiguity is politically functional).
 *
 * KEY AGENTS:
 *   - Political Entrepreneurs: Primary beneficiary (institutional/arbitrage) — exploit ambiguity to build cross-ideological coalitions without requiring agreement on normative justification or distributional outcomes
 *   - Policy Designers with Taxing-Back Mechanisms: Primary beneficiary (institutional/arbitrage) — rhetorical flexibility allows them to frame identical distributional outcomes as either generous or austere depending on political context
 *   - Ideological Clarity: Primary victim (powerless/trapped) — cannot organize or exit; ambiguity prevents coherent public evaluation of what the policy does
 *   - Targeted Program Recipients: Primary victim (powerless/trapped) — lose specialized benefits (higher disability payments, housing assistance) and cannot claim special need without contradicting the universal logic
 *   - Labor Market Participants: Secondary actor (moderate/constrained) — experience mixed coordination (income floor) and extraction (tax-back creates effective marginal poverty traps)
 *   - Fiscal Authorities: Secondary actor (powerful/mobile) — bounded by distributional outcomes converging across designs; face genuine coordination problem stabilizing income distribution
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing political choice (maintain ambiguity) as structural necessity (ambiguity inherent to democracy)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, 0.38).
domain_priors:suppression_score(unconditional_income_support__universality_paradox_reading, 0.52).
domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Unconditional Income Support: Universality Paradox Reading").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, 'e8800eaf-081e-4869-8947-f42d078d7b1c').
narrative_ontology:cs_kernel_codification('e8800eaf-081e-4869-8947-f42d078d7b1c', distributed).
narrative_ontology:cs_authority_grounding('e8800eaf-081e-4869-8947-f42d078d7b1c', extraction).
narrative_ontology:cs_interpretation_layer_present('e8800eaf-081e-4869-8947-f42d078d7b1c').
narrative_ontology:cs_reading_relation('e8800eaf-081e-4869-8947-f42d078d7b1c', unconditional_income_support__freedom_floor_reading, influences).
narrative_ontology:cs_reading_relation('e8800eaf-081e-4869-8947-f42d078d7b1c', unconditional_income_support__dependency_trap_reading, influences).
narrative_ontology:cs_axiom('e8800eaf-081e-4869-8947-f42d078d7b1c', foundational, distributional_outcomes_converge_across_designs).
narrative_ontology:cs_axiom_status(distributional_outcomes_converge_across_designs, holdable).
narrative_ontology:cs_axiom_grounding('e8800eaf-081e-4869-8947-f42d078d7b1c', distributional_outcomes_converge_across_designs, empirically_contingent).
narrative_ontology:cs_axiom('e8800eaf-081e-4869-8947-f42d078d7b1c', foundational, policy_ambiguity_functionally_maintained_by_beneficiaries).
narrative_ontology:cs_axiom_status(policy_ambiguity_functionally_maintained_by_beneficiaries, holdable).
narrative_ontology:cs_axiom_grounding('e8800eaf-081e-4869-8947-f42d078d7b1c', policy_ambiguity_functionally_maintained_by_beneficiaries, instrumental).
narrative_ontology:cs_reference_frame('e8800eaf-081e-4869-8947-f42d078d7b1c', social_democratic_welfare_state_universality_norm).
narrative_ontology:cs_drift_state('e8800eaf-081e-4869-8947-f42d078d7b1c', contemporary_evidence_maturation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e8800eaf-081e-4869-8947-f42d078d7b1c', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers_with_taxing_back_mechanisms).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, ideological_clarity).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED PROGRAM RECIPIENTS (SNARE) — Trapped in the policy design without exit. Universal income framing justifies cuts to targeted programs (disability, housing, food assistance) that served them with higher benefit levels and no stigma. The universality cover story prevents organized opposition — they cannot claim special need without contradicting the universal logic. Maximum suppression of alternatives.
constraint_indexing:constraint_classification(unconditional_income_support__universality_paradox_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LABOR MARKET PARTICIPANTS (TANGLED ROPE) — Face mixed extraction and coordination. The policy does coordinate a genuine benefit (income floor, reduced labor market coercion) but embeds extraction through taxing-back mechanisms that claws back income, creating effective marginal tax rates that exceed targeted welfare. The coordination and extraction are structurally entangled — the same policy vehicle provides both.
constraint_indexing:constraint_classification(unconditional_income_support__universality_paradox_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: POLITICAL ENTREPRENEURS (ROPE) — See the constraint as pure coordination across ideological divides. Universal income appeal unites left (autonomy floor) and right (incentive-compatible alternative to welfare bureaucracy) without requiring agreement on normative justification. The ambiguity is the feature, not a bug — it enables coalition formation. The policy designer can arbitrage between implementation paths (generous universal, austere universal, heavily taxed-back) while maintaining the same label.
constraint_indexing:constraint_classification(unconditional_income_support__universality_paradox_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FISCAL AUTHORITIES (TANGLED ROPE) — Face genuine coordination problem (stabilizing income distribution across labor market transitions) and asymmetric extraction (designing tax-back mechanisms that concentrate marginal extraction on mid-income earners). The universal framing enables fiscal designers to hide distributional complexity. The constraint provides coordination benefit (unified income floor) and allows extraction mechanism (taxing-back creates effective poverty traps that were not salient in discourse).
constraint_indexing:constraint_classification(unconditional_income_support__universality_paradox_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: WELFARE STATE LEGITIMACY THEATER (PITON) — The universality framing is largely performative. Actual distributional outcomes (post-tax-back income) are nearly identical across ideologically incompatible implementation paths (generous design with high tax-back vs austere design with low tax-back). The performative layer — the rhetorical claim that 'universality respects autonomy' or 'universality eliminates bureaucratic paternalism' — persists because the alternative (admitting that distributional outcomes are contingent on taxing-back design) surfaces the policy ambiguity. Theater ratio high because the policy legitimacy depends on NOT making visible the implementation path dependence.
constraint_indexing:constraint_classification(unconditional_income_support__universality_paradox_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some policy ambiguity around income support is inherent to democracy: no single normative framework can satisfy all preferences simultaneously, so coalition-building requires rhetorical flexibility. This perspective sees the universality paradox as a structural feature of democratic policymaking itself — an immutable natural law of pluralist politics. However, the structural data (identified beneficiaries and victims) contradicts the mountain classification — the engine will compute this as a false summit, revealing that 'ambiguity is inherent to democracy' naturalizes what is actually a contingent political choice to maintain ambiguity rather than clarify implementation paths.
constraint_indexing:constraint_classification(unconditional_income_support__universality_paradox_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__universality_paradox_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unconditional_income_support__universality_paradox_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unconditional_income_support__universality_paradox_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, TR),
    TR >= 0.70.

:- end_tests(unconditional_income_support__universality_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint operates primarily through political ambiguity and taxing-back design, not through direct fiscal redistribution. The extractiveness is low relative to conventional welfare extraction because the final distributional outcomes are nearly identical across designs — the extraction is in the rhetorical space (political entrepreneurs capturing value from ambiguity-enabled coalition formation) and in the design space (policy designers gaining flexibility). The metric reflects that the constraint's primary mechanism is not fiscal but political: it extracts value from maintaining ambiguity rather than from redistributing material resources. The measurement trajectory (0.22 → 0.38) reflects accumulating policy sophistication in tax-back design — as implementations mature, the gap between nominal generosity and actual distributional effect widens, increasing the extractive potential of the ambiguity. Suppression (0.52): Moderate-high. Real barriers exist to clarity: (1) technical complexity of tax-back mechanisms obscures distributional outcomes from public discourse, (2) political coalition incentives reward ambiguity-maintenance over clarification, (3) ideological commitment makes some actors resistant to evidence that their preferred reading's empirical claims diverge from competitors' outcomes. But suppression is not total — technical policy documents reveal the convergence, and some institutional actors (fiscal authorities, evidence-based policy advocates) have incentives to surface it. Theater ratio (0.68): High. The universality framing is substantially performative. Public-facing rhetoric claims either autonomy-enabling or efficiency-enabling functions, but technical specifications show that distributional outcomes are determined by tax-back design, not by the nominal universality claim. The theater has increased (0.52 → 0.68) as policy implementations have matured — the gap between rhetorical claim and technical specification has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural mechanism (taxing-back mechanisms that converge distributional outcomes) appears as different types from different positions. The political entrepreneur sees coordination (Rope) — they are solving the genuine problem of building coalition across ideological divides. The fiscal authority sees tangled rope — they coordinate income distribution while extracting through tax-back design. Targeted program recipients see pure extraction (Snare) — the universal framing justifies cuts to their specialized benefits. The welfare legitimacy theater sees itself as degraded (Piton) — it maintains the universality rhetoric through inertia, not because it works. The labor market participant sees mixed coordination and extraction (Tangled Rope) — they benefit from the income floor but face effective poverty traps from tax-back mechanisms. The civilizational analytical observer risks seeing an immutable law (Mountain) — policy ambiguity is inherent to democracy — but structural data reveals this as a false summit: political entrepreneurs and policy designers actively maintain the ambiguity.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's d value (directionality) is determined by the agent's beneficiary/victim status and exit options. Political entrepreneurs benefit from the constraint and have arbitrage options (they can shop for ideological framings that fit their coalition needs) — they derive low d from beneficiary + arbitrage. Targeted program recipients are victims with no exit (universality logic prevents them from claiming special need) — they derive high d from victim + trapped. Labor market participants are both beneficiaries (income floor) and victims (tax-back extraction) with constrained exits — they derive moderate d. Fiscal authorities benefit from the constraint (it enables them to design distributional outcomes while maintaining rhetorical coherence) but face some constraining (they cannot abandon the universality framing without surfacing political costs) — they derive moderate-low d from beneficiary + mobile. The ideological clarity victim (powerless/trapped) has maximum d because it is not an agent with preferences but a property of the epistemic commons that cannot organize.
 *
 * MANDATROPHY ANALYSIS:
 *   Unconditional income support resolves mandatrophy by identifying THE CONSTRAINT as the ambiguity itself, not the policy. The policy mechanism (taxing-back designs) is instrumentally clean — distributional outcomes converge. But the policy's legitimacy depends on maintaining incompatible normative interpretations (autonomy vs efficiency vs incentive-compatibility). The mandatrophy (how to classify a policy that serves incompatible ends) resolves to: the constraint is not the policy outcome but the political requirement to maintain ambiguity. This is a tangled rope — the ambiguity provides genuine coordination function (enables coalition formation across ideological divides) while extracting value (political entrepreneurs benefit from the inability to surface shared understanding of what the policy does). The theater ratio distinguishes this from pure coordination (Rope) — if the universality framing were purely functional, theater would be low. High theater indicates that the framing persists partly through inertia and rhetorical convention, not because it optimally achieves the policy's stated goals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taxing_back_distributional_equivalence,
    'Do fiscal outcomes post-tax-back converge across ideologically incompatible UBS designs (generous universal with high tax-back vs austere universal with low tax-back)?',
    'Comparative fiscal analysis of actual UBS pilot programs and proposals: distribute final-outcome income distribution curves across design variants; measure Gini coefficient convergence',
    'If convergent: ε should be LOW (extractiveness is purely about political ambiguity, not fiscal redistribution); constraint is piton-dominant with tangled-rope veneer. If divergent: ε should be higher; actual redistributive intent drives classification. Convergence supports this reading; divergence undermines it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(taxing_back_distributional_equivalence, empirical, 'Whether tax-back mechanisms equalize distributional outcomes across UBS designs').

omega_variable(
    reading_contest_structure,
    'Do the three sibling readings (freedom_floor, dependency_trap, universality_paradox) genuinely foreclose each other or merely emphasize different structural features of the same policy mechanism?',
    'Formal logical analysis: map each reading''s core premise to a Prolog statement; test whether any reading''s core premise entails the negation of another reading''s core premise. If no reading''s premise entails negation of others'' premises, they coexist logically and differ only in emphasis.',
    'If readings foreclose: one reading''s adoption rules out others within a single framework (rare, rare — would indicate deep incompatibility). If readings coexist: they remain simultaneously defensible from different positions; ambiguity is not a bug in the kernel but a feature of the kernel structure itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_structure, conceptual, 'Logical relationship between sibling readings of the UBS kernel').

omega_variable(
    ideological_clarity_as_victim,
    'Does the universality framing genuinely suppress clear evaluation of distributional intent, or does it enable distributional transparency through rhetorical escape hatches (taxing-back language)?',
    'Discourse analysis: compare public-facing UBS framing across ideological contexts (left-aligned vs right-aligned proposals) and technical policy documents; measure gap between public rhetorical claim and technical specification; assess whether technical specifications made visible by policy designers themselves',
    'If clarity suppressed: ideological clarity is a genuine victim (constraint has real suppression component). If clarity is performatively suppressed but technically available: suppression is theater-dependent and could be reduced through institutional changes (mandatory technical disclosure). Affects the suppression metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_clarity_as_victim, empirical, 'Whether ideological clarity is genuinely suppressed or performatively obscured').

omega_variable(
    universality_paradox_reading_vs_freedom_floor,
    'READING CONTEST: Does the universality-paradox reading''s core premise (that implementation paths converge on similar fiscal outcomes) foreclose, coexist with, or influence the freedom-floor reading''s core premise (that unconditional income support enables autonomy)?',
    'Logical analysis: if distributional outcomes converge regardless of ideological framing, does this eliminate the autonomy-enabling function or merely change which agents experience autonomy gain? A policy that enables autonomy for some agents while extracting from others could still enable autonomy (coexist). A policy that produces identical distributional outcomes would eliminate the autonomy gain only if autonomy gain depends on the specific distributional path taken.',
    'If forecloses: the readings cannot both be true in any single framework. If coexists: both are structurally defensible (one emphasizes autonomy as the primary function; the other emphasizes fiscal equivalence; neither rules out the other). If influences: distributional convergence creates pressure on the freedom-floor reading''s empirical claims but does not logically rule them out.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_paradox_reading_vs_freedom_floor, conceptual, 'Relationship between universality-paradox and freedom-floor readings').

omega_variable(
    universality_paradox_reading_vs_dependency_trap,
    'READING CONTEST: Does the universality-paradox reading''s core premise coexist with, foreclose, or influence the dependency-trap reading''s core premise (that unconditional income support distorts labor incentives)?',
    'Empirical labor supply analysis across UBS designs: measure labor force participation, hours worked, and wage-seeking behavior. If dependency effects vary across designs, the dependency-trap reading is design-dependent (coexists with paradox reading). If effects are similar across designs, the paradox reading undermines the dependency-trap reading''s distributional claims but may not foreclose the normative claim that incentive-distortion is undesirable.',
    'If effects vary by design: both readings hold empirical weight from different implementation contexts. If effects are similar: the dependency-trap reading''s empirical claim is undermined, but its normative stance (incentive-distortion is bad) can coexist with the paradox reading (distributional outcomes converge regardless).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_paradox_reading_vs_dependency_trap, empirical, 'Relationship between universality-paradox and dependency-trap readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uis_univ_tr_t0, unconditional_income_support__universality_paradox_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(uis_univ_tr_t10, unconditional_income_support__universality_paradox_reading, theater_ratio, 10, 0.62).
narrative_ontology:measurement(uis_univ_tr_t20, unconditional_income_support__universality_paradox_reading, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(uis_univ_be_t0, unconditional_income_support__universality_paradox_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(uis_univ_be_t10, unconditional_income_support__universality_paradox_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(uis_univ_be_t20, unconditional_income_support__universality_paradox_reading, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, attachment_coordination).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__dependency_trap_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, welfare_bureaucracy_complexity).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, means_tested_benefit_targeting).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, labor_market_coercion_mechanisms).

% DUAL FORMULATION NOTE:
% The universality-paradox reading is one of three kernel readings of unconditional_income_support. Each reading (freedom_floor, dependency_trap, universality_paradox) has its own constraint_id, its own ε, its own perspectives, and instantiates a different normative framing of the same policy mechanism. The three readings are NOT perspectives on the same constraint — they are separate constraints that share a kernel (the ambiguous policy category). The network links are bidirectional: this constraint affects its sibling readings by surfacing empirical evidence (fiscal convergence) that both supports and challenges their respective claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unconditional_income_support__universality_paradox_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
