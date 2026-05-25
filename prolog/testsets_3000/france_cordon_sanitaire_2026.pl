% ============================================================================
% CONSTRAINT STORY: france_cordon_sanitaire_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_france_cordon_sanitaire_2026, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: france_cordon_sanitaire_2026
 *   human_readable: The "Front Républicain" (Republican Front) Cordon Sanitaire in France (2026)
 *   domain: political/social
 *
 * SUMMARY:
 *   The Republican Front (Front Républicain) cordon sanitaire is a post-1958
 *   French political mechanism by which center-right, center-left, and
 *   left-wing parties jointly refuse to form coalitions with far-right
 *   parties. Formalized during the 1990s-2000s as the RN (then FN) grew, the
 *   cordon functioned as a genuine coordination mechanism: it solved the
 *   collective action problem of preventing anti-establishment forces from
 *   entering government through electoral success. By 2026, the cordon has
 *   transitioned into a Piton state—a degraded constraint maintained by
 *   institutional inertia and performative assertion rather than structural
 *   necessity. The RN's rhetorical normalization (distancing from fascism,
 *   adopting constitutional-sounding language), changing electoral
 *   mathematics (second-round voter calculus no longer strictly requires
 *   cordon discipline), and the rise of younger cohorts who perceive the
 *   cordon as an oligarchic constraint rather than a democratic necessity
 *   have all eroded the mechanism's functional foundation. Yet the constraint
 *   persists, maintained through high theater: newspapers and political
 *   leaders continue to assert the cordon as a binding moral principle, even
 *   as its practical enforcement has become optional and its beneficiaries
 *   (the centrist establishment) have incentive to maintain the narrative
 *   regardless of its real effect. The constraint is simultaneously a
 *   victim-suppression mechanism (preventing far-right electoral choice) and
 *   a decaying institutional ritual.
 *
 * KEY AGENTS:
 *   - The RN Electorate: Primary victim (powerless/trapped) — voters supporting far-right candidates have no legal pathway to government even through electoral victory, due to coordinated elite refusal to partner
 *   - The Centrist Establishment: Primary beneficiary (institutional/arbitrage) — center-right and centrist parties (LREM, LR, MoDem) preserve their governing coalition access by excluding RN, preventing programmatic competition from below
 *   - The Institutional Left (PS, Greens, LFI): Secondary victim/beneficiary (moderate/constrained) — constrained by cordon discipline to coordinate against RN, losing independent electoral strategy, but also protected from far-right governance
 *   - Civil Society & Media: Organized beneficiary (organized/arbitrage) — journalists, educators, NGOs align with cordon narrative as alignment with stated democratic values; can exit at any time but choose not to
 *   - Youth and Post-2000 Cohorts: Emerging victim (powerless/constrained) — perceive cordon as elite constraint on democratic choice; lack shared historical trauma (wartime collaboration) that gave cordon legitimacy for older generations
 *   - Analytical Observer: Democratic theorist assessing whether cordon could be replaced by structural electoral reform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(france_cordon_sanitaire_2026, 0.28).
domain_priors:suppression_score(france_cordon_sanitaire_2026, 0.62).
domain_priors:theater_ratio(france_cordon_sanitaire_2026, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(france_cordon_sanitaire_2026, extractiveness, 0.28).
narrative_ontology:constraint_metric(france_cordon_sanitaire_2026, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(france_cordon_sanitaire_2026, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(france_cordon_sanitaire_2026, piton).
narrative_ontology:human_readable(france_cordon_sanitaire_2026, "The \"Front Républicain\" (Republican Front) Cordon Sanitaire in France (2026)").
narrative_ontology:topic_domain(france_cordon_sanitaire_2026, "political/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(france_cordon_sanitaire_2026, centrist_establishment).
narrative_ontology:constraint_beneficiary(france_cordon_sanitaire_2026, institutional_left_right_consensus).
narrative_ontology:constraint_victim(france_cordon_sanitaire_2026, electoral_competition).
narrative_ontology:constraint_victim(france_cordon_sanitaire_2026, programmatic_differentiation).
narrative_ontology:constraint_victim(france_cordon_sanitaire_2026, voter_choice_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE TRAPPED ELECTORATE (SNARE) — Voters committed to far-right parties or populist challengers face a structural barrier: even victory at the ballot results in cordon activation, requiring them to be containable through cross-party elite coordination. Exit is impossible within institutional channels. The suppression mechanism (media blackout, establishment coordination) is non-negotiable. These voters bear the full cost of the constraint without recourse.
constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE CENTRIST ESTABLISHMENT (PITON) — Primary institutional beneficiary. The cordon preserves the stable post-1958 center-right/center-left alternation and prevents genuine anti-establishment forces from governing. For establishments parties, the cordon functions as coordination (shared interest in excluding outsiders), but the mechanism has atrophied. Its primary function—preventing another wartime collaboration scenario—no longer credibly applies. The cordon persists through institutional inertia: it is performed in newspapers and campaign speeches, but the actual electoral mathematics no longer require it. Theater ratio is high (0.81) because the constraint is maintained more through narrative assertion than through structural necessity.
constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE NON-EXTREME OPPOSITION PARTIES (TANGLED ROPE) — Socialist, Green, and moderate right parties have constrained exit. The cordon coordinates their anti-RN strategy (genuine coordination benefit) but also constrains their programmatic differentiation from each other. They cannot credibly attack establishment partners without breaking the cordon. They have some agency (can negotiate terms within the coalition) but cannot leave without enabling the far-right to enter government. Mixed coordination-extraction with asymmetric power.
constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANTI-RN CIVIL SOCIETY & MEDIA GATEKEEPERS (ROPE) — Journalists, educators, NGOs, and cultural institutions see the cordon as pure coordination: solving the collective action problem of preventing far-right governance. They have significant arbitrage options (can withdraw, reframe coverage, or shift focus) but choose to maintain the cordon because it aligns with their stated values. Low coercion, genuine coordination function. Theater is still elevated (framing the cordon as moral necessity rather than pragmatic containment) but functional.
constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ANALYTICAL DEMOCRATIC REFORM PERSPECTIVE (SCAFFOLD) — An observer studying whether the cordon could be replaced by structural electoral reform (proportional representation, citizen assemblies, or deliberative mechanisms) sees it as a temporary coordination failure with potential sunset. If France adopts proportional representation or runoff rules that eliminate the two-round polarization, the cordon's functional purpose becomes obsolete. This perspective treats the 2026 cordon as a transitional constraint that could decompose over a 15-20 year horizon through institutional redesign.
constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 6: POLITICAL THEORY PERSPECTIVE (FALSE MOUNTAIN) — From a civilizational view, some cordon against extremism is inherent to stable democracy — no democratic system allows unlimited entry to anti-democratic movements. However, this perspective naturalizes what is a contingent institutional choice. The 2026 French cordon is not an invariant law but a specific mechanism (elite coordination + media gatekeeping) that depends on institutional maintenance. The engine's false summit detector reveals the naturalization: the cordon is not a mountain.
constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(france_cordon_sanitaire_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(france_cordon_sanitaire_2026, TR),
    TR >= 0.70.

:- end_tests(france_cordon_sanitaire_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate. The RN electorate is definitively prevented from achieving their electoral preference, a form of structural extraction. However, the prevention is not brutal—it operates through legal mechanisms (refusal to govern) rather than bans or suppression. The theater component means much of the suppression works through narrative/norm rather than explicit coercion. The trajectory shows low initial extraction (0.18 at t=0, when cordon was still functionally solving the two-round electoral problem) rising to moderate (0.28 by t=10, as the constraint becomes performative but still effective). Suppression (0.62): Moderate-high. The mechanism relies on strong norms (establishment parties will not partner with RN), media coordination (gatekeeping on RN legitimacy), and voter anticipation (second-round voters strategically vote against RN even in first round). The suppression is real but not absolute—it is not a legal ban, and alternative narratives are technically possible. Theater ratio (0.81): High and rising. The constraint has increasingly become performative. The actual electoral mathematics in 2026 are less dependent on explicit cordon invocation than they were in 2002 (post-Le Pen shock) or 2017 (post-Trump/Brexit wave). Establishment parties still invoke the cordon rhetorically, but much of what they are doing would happen anyway due to voter polarization and RN's remaining unpopularity. The theater ratio rise reflects Goodhart drift: the narrative assertion of the cordon (its performance) has increasingly decoupled from its functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival disagreement. The RN electorate experiences pure extraction (Snare)—no coordination benefit, maximum exclusion. The centrist establishment experiences near-zero extraction (Piton bordering on Rope)—they benefit from the constraint and experience it as necessary coordination. The opposition Left experiences mixed constraint and benefit (Tangled Rope)—forced coordination but protection from far-right governance. Civil society experiences pure coordination (Rope)—solving the collective action problem of preventing extremism, with voluntary participation. The democratic reformist sees a temporary problem (Scaffold)—solvable through proportional representation. The political theorist risks false naturalization (false Mountain)—treating contingent institutional choice as invariant law. No other constraint in the corpus shows this degree of perspectival divergence across the six types. The cordon is Piton for those who benefit, Snare for those excluded, Rope for those who embrace it, Tangled Rope for those forced into it, and Scaffold for those who see structural reform as viable. The mountain perspective is exposed as naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from structural position. RN voters have no exit options (trapped) and derive no benefit from the cordon—maximum d (~0.95). Centrist establishment have arbitrage options (could govern alone, could negotiate with RN, could build alternative coalitions) and benefit directly from cordon (exclusion of rivals)—minimal d (~0.10). Non-extreme opposition have constrained exits and mixed benefits—moderate d (~0.55). Civil society have full arbitrage (can withdraw support, reframe narrative) and voluntarily align—low d (~0.20). The derived d values feed into f(d) sigmoid, producing experienced extractiveness chi. For trapped voters, f(d) is maximal (~1.42); for institutional beneficiaries, f(d) is minimal (~-0.12); for moderate agents, f(d) is around 1.0. These are not explicit calculations in this commentary but structural facts that determine how each agent experiences the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL CASE: The cordon sanitaire exhibits the mandatrophy in its purest form. The constraint can be truthfully described as Mountain (inherent to democratic stability), Rope (coordination mechanism solving collective action), Piton (theatrical degradation), Snare (electoral suppression), and Tangled Rope (mixed coordination-extraction). The mandatrophy is resolved not by choosing one 'correct' type but by recognizing that the classification depends entirely on the observer's structural position and time horizon. For the RN electorate with immediate/biographical horizon and no exit, it is Snare. For the centrist establishment with immediate horizon and arbitrage options, it is Piton. For the analytical observer on a civilizational horizon, it appears as Mountain until the false summit detector exposes it as naturalization. The resolution is perspectival: all six readings are valid relative to their index. The constraint is NOT objectively one type—it is a presheaf of types over the observation space. This makes the cordon a diagnostic exemplar of how Deferential Realism dissolves false universality claims about constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rn_ideological_threshold,
    'Has the RN moved sufficiently toward constitutional acceptance that the classical cordon rationale (preventing anti-democratic takeover) no longer applies?',
    'Comparative analysis of RN policy platforms 2017-2026; expert assessment of constitutional commitment; historical comparison to other European far-right parties post-normalization (Austrian FPÖ, Italian Lega)',
    'If threshold crossed: cordon becomes pure extraction, classification shifts definitively to Snare. If not crossed: cordon retains mountain-like justification (though still classified as Piton due to atrophied function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rn_ideological_threshold, conceptual, 'Whether RN poses genuine anti-democratic threat or has normalized').

omega_variable(
    elite_coordination_sufficiency,
    'Does explicit cross-party coordination (verbal declarations of cordon) actually prevent far-right governance, or is it performed theater while electoral mathematics alone determine outcomes?',
    'Counterfactual simulation: if cordon were withdrawn in 2027 but all parties retained their 2026 platform positions, would RN win? Analysis of second-round voter behavior with/without explicit cordon statements from establishment leaders.',
    'If coordination is functionally necessary: piton classification confirmed (theater persists because it has causal effect). If cordon is performative theater masking pure electoral math: theater_ratio should increase to 0.90+, pushing toward pure piton degradation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(elite_coordination_sufficiency, empirical, 'Whether elite coordination functionally prevents far-right governance').

omega_variable(
    institutional_left_right_convergence,
    'On substantive policy dimensions (economic, social, environmental), how much genuine differentiation remains between PS, LREM, and moderate right? Does the cordon suppress program differences that would otherwise be salient?',
    'Manifesto coding analysis (Comparative Manifestos Project scale); voter perception surveys on issue differences; second-order variance in policy positions conditional on cordon presence/absence',
    'High convergence + cordon suppression = Snare for opposition parties (forced alliance despite preference divergence). Low convergence = Rope for opposition (natural coordination). Determines whether cordon is extraction mechanism or functional alliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_left_right_convergence, empirical, 'Degree of actual policy differentiation among centrist parties').

omega_variable(
    generational_legitimacy_collapse,
    'Among voters under 35, does the cordon still function as a normative principle (should prevent RN) or is it perceived as an illegitimate oligarchic constraint?',
    'Cohort-stratified polling on cordon legitimacy; comparison of 2022 vs 2027 youth voting patterns; analysis of far-right candidate performance in age groups where cordon narrative has weakest purchase',
    'If legitimacy collapses in younger cohorts: suppression mechanism (shared norm against RN governance) erodes, extraction becomes visible, classification could shift from Piton to Snare as theater fails to sustain voluntary coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generational_legitimacy_collapse, empirical, 'Whether younger voters accept cordon as legitimate constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(france_cordon_sanitaire_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cordon_tr_t0, france_cordon_sanitaire_2026, theater_ratio, 0, 0.55).
narrative_ontology:measurement(cordon_tr_t5, france_cordon_sanitaire_2026, theater_ratio, 5, 0.7).
narrative_ontology:measurement(cordon_tr_t10, france_cordon_sanitaire_2026, theater_ratio, 10, 0.81).

% Extraction over time
narrative_ontology:measurement(cordon_be_t0, france_cordon_sanitaire_2026, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cordon_be_t5, france_cordon_sanitaire_2026, base_extractiveness, 5, 0.24).
narrative_ontology:measurement(cordon_be_t10, france_cordon_sanitaire_2026, base_extractiveness, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(france_cordon_sanitaire_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(france_cordon_sanitaire_2026, french_electoral_bifurcation).
narrative_ontology:affects_constraint(france_cordon_sanitaire_2026, right_wing_populism_containment).
narrative_ontology:affects_constraint(france_cordon_sanitaire_2026, establishment_hegemony_2026).

% DUAL FORMULATION NOTE:
% The cordon sanitaire can be decomposed into two related but structurally distinct constraints: (1) The Electoral Barrier (the two-round runoff mathematics that creates incentive for second-round strategic voting), which is closer to Mountain; (2) The Institutional Compact (the explicit elite coordination to refuse RN partnership), which is Piton. This story focuses on the Institutional Compact version. The Electoral Barrier is a separate constraint with lower theater and arguably lower extractiveness, representing the genuine coordination function. As of 2026, the Compact is increasingly parasitic on the Barrier—maintaining theater about electoral barriers whose functional force has decayed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(france_cordon_sanitaire_2026, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
