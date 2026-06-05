% ============================================================================
% CONSTRAINT STORY: marriage_market_asymmetry_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_market_asymmetry_2026, []).

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
 *   constraint_id: marriage_market_asymmetry_2026
 *   human_readable: The Asymmetric Information Snare (Women Asking Out)
 *   domain: social/psychological/economic
 *
 * SUMMARY:
 *   The asymmetric information snare in romantic partner selection reflects a
 *   structural constraint where social norms enforce passive selection for
 *   women and active selection for men, creating a mechanism by which
 *   high-value males extract decision-making advantage from female
 *   initiators. The Gale-Shapley theorem models optimal matching when both
 *   sides have full information and can initiate. In reality, informational
 *   asymmetry is actively maintained through social cost: women who initiate
 *   face reputational penalties (perceived as 'desperate,' 'aggressive,' or
 *   'forward') that men do not face. This constraint is NOT a natural law of
 *   mate selection—it is a contingent institutional arrangement enforced
 *   through social feedback and internalized norms. The extraction operates
 *   at multiple levels: high-value males receive passive signals while
 *   avoiding rejection risk; lower-value males face compounded barriers
 *   (cannot initiate credibly, receive no passive signals); women optimize
 *   for signaling availability rather than compatibility; and overall
 *   matching efficiency is reduced because mutual interest remains hidden.
 *   The theater ratio (0.44, relatively low) indicates that unlike some
 *   constraints, the snare operates through direct social pressure rather
 *   than elaborate performative rituals—the enforcement mechanism is raw norm
 *   violation cost, not theater.
 *
 * KEY AGENTS:
 *   - Female Initiators: Primary victims (powerless/trapped) — bear reputational cost, rejection asymmetry, internalized norms preventing credible signaling
 *   - High-Value Male Selectors: Primary beneficiaries (institutional/arbitrage) — receive passive signals, avoid rejection risk, benefit from constrained female option set
 *   - Lower-Value Male Competitors: Secondary victims (moderate/constrained) — face amplified extraction: cannot initiate credibly AND receive no passive signals
 *   - Career-Oriented Women: Secondary victims (powerless/trapped) — face compound extraction: norms expect passivity while competing with women whose primary signal is availability
 *   - Feminist Coalitions: Organized challengers (organized/mobile) — have demonstrated exit options and promote norm change; see constraint as solvable coordination problem
 *   - Matching Efficiency Function: System-level victim (analytical/analytical) — overall match quality is reduced; mutual interest remains hidden
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_market_asymmetry_2026, 0.58).
domain_priors:suppression_score(marriage_market_asymmetry_2026, 0.68).
domain_priors:theater_ratio(marriage_market_asymmetry_2026, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_market_asymmetry_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_market_asymmetry_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(marriage_market_asymmetry_2026, theater_ratio, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_market_asymmetry_2026, snare).
narrative_ontology:human_readable(marriage_market_asymmetry_2026, "The Asymmetric Information Snare (Women Asking Out)").
narrative_ontology:topic_domain(marriage_market_asymmetry_2026, "social/psychological/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_market_asymmetry_2026, male_high_value_selectors).
narrative_ontology:constraint_victim(marriage_market_asymmetry_2026, female_initiators).
narrative_ontology:constraint_victim(marriage_market_asymmetry_2026, relationship_matching_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEMALE INITIATOR (SNARE) — Women who attempt to initiate dating face reputational cost, rejection asymmetry, and social feedback loops that penalize deviation from passive selection norms. Cannot credibly signal interest without bearing disproportionate social cost. Trapped by signaling constraints, asymmetric rejection burden, and internalized norms. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96.
constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HIGH-VALUE MALE SELECTORS (ROPE) — Men with high mate value benefit from the asymmetric initiation norm: they receive signals passively while avoiding rejection risk and social cost. The constraint solves a coordination problem (revealing mutual interest without mutual vulnerability). They experience this as enabling, not extractive. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: LOWER-VALUE MALE COMPETITORS (SNARE) — Men below the local status threshold face an amplified problem: they cannot credibly initiate (high rejection cost) and do not receive passive signals (low visibility in the selection pool). The female passivity norm is actually a secondary extraction mechanism targeting them. d≈0.85, f(d)≈1.20, σ=0.9 → χ≈0.63.
constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: MATCHING EFFICIENCY (TANGLED ROPE) — From a civilizational/system perspective, asymmetric initiation norms reduce overall matching efficiency: mutual interest that would be revealed through symmetric signaling remains hidden. The constraint has a real coordination function (mutual vulnerability reduction) but achieves it at the cost of misallocated matches. It requires active enforcement through social feedback. ε≈0.58, χ computed from mixed beneficiary (coordination) and victim (efficiency loss) status. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.45.
constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: CAREER-ORIENTED WOMEN (SNARE) — Women prioritizing career development over signaling availability face a compound extraction: norms expect them to remain passive while competing with women whose primary signaling is availability. Cannot exit without bearing reputational cost (appearing 'aggressive' or 'desperate'). Theater ratio (0.44) is relatively low here — the snare operates through direct social pressure, not performative ritual. d≈0.88, f(d)≈1.32, σ=1.2 → χ≈0.85.
constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: FEMINIST COALITIONS (ROPE) — Organized groups promoting symmetric initiation norms experience this as a coordination problem solvable by norm change. They have demonstrated exit options (creating alternative dating spaces, online platforms, subcultures where female initiation is normative) and organized agency. See this constraint as Rope because it solves the mutual-vulnerability problem without extraction — the alternative is messier but more efficient. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.22.
constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_market_asymmetry_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_market_asymmetry_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_market_asymmetry_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant decision-making advantage for high-value males and creates substantial costs for female initiators (psychological, reputational, opportunity). However, the extraction is not total—women retain ultimate veto power and can initiate, paying a cost. The measured value reflects that initiation norms have partially eroded in some populations (particularly online contexts) but remain strong in others. Suppression (0.68): High. Social cost of norm violation is substantial: women face reputational damage, social feedback, and psychological internalization of 'appropriate' feminine passivity. Suppression operates through social learning (observing costs for violators) and institutional reinforcement (media, parental guidance, peer pressure). But suppression is not total—exits exist (subcultures, online contexts, norm-shifted friend groups). Theater ratio (0.44): Moderate-low. Unlike some constraints, this one does not require elaborate performative ritual. The enforcement is direct social pressure and internalized norm—women learn not to initiate through feedback, not through complex theatrical structures. The theater_ratio reflects that norms are maintained through simple social cost, not performance complexity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a stark perspectival gap rooted in structural position. High-value males see a coordination mechanism (Rope) that solves the mutual-vulnerability problem elegantly—they receive signals without risking rejection. Female initiators see pure extraction (Snare)—they bear all the signaling cost and face rejection asymmetry. Lower-value males see a compounded snare where the female passivity norm actually increases their extraction burden. The organized feminist coalitions see a solvable coordination problem (Rope) because they have agency and exit options—they can create norm-reversed spaces and have demonstrated that alternative norms produce better outcomes. The matching efficiency function (analytical perspective) sees that overall system performance is degraded—the constraint solves the wrong problem (protecting high-value male selection) at the cost of matching quality. This perspectival gap is NOT an artifact of measurement choice or observer bias—it reflects genuine structural differences in how the constraint distributes costs and benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Female Initiators: Victims + trapped → d≈0.92, f(d)≈1.38. Near-maximal extraction. Cannot exit without bearing significant reputational and psychological cost. Career-oriented women face compound extraction: d≈0.88, f(d)≈1.32. High-Value Male Selectors: Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiaries. Receive signals while avoiding rejection risk. Lower-Value Males: Victims + constrained → d≈0.85, f(d)≈1.20. High extraction. Can initiate but face high rejection cost; do not receive passive signals. Feminist Coalitions: Organized + mobile → d≈0.35, f(d)≈0.32. Low effective extraction. Have agency and demonstrated exit options. Matching Efficiency: Analytical victim status → d≈0.50, f(d)≈0.65. Symmetric treatment reflects that the function has coordination aspect (mutual vulnerability reduction) and extraction aspect (efficiency loss) in balance.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is NOT a natural law of mate selection but a contingent institutional arrangement. The false summit is the claim that 'women prefer passive selection because it gives them higher-quality matches'—this naturalizes what is actually a norm-enforced outcome. The evidence for a snare classification is: (1) extractiveness (0.58) is well above coordination threshold (0.35); (2) suppression (0.68) is characteristic of snares; (3) beneficiaries (high-value males) and victims (female initiators, lower-value males) are clearly identified; (4) the constraint requires active enforcement through social cost—it does not emerge naturally; (5) alternative norms (symmetric initiation) have been implemented in subcultures and online contexts with improved matching efficiency. The Tangled Rope perspective for matching efficiency is correct—the constraint does have a coordination function (mutual vulnerability reduction) but achieves it asymmetrically through extraction. The overall classification is Snare because the extraction function dominates the coordination function in the structural data. Feminist coalitions demonstrating alternative norms with better outcomes is evidence that the extraction mechanism is not inevitable—it is a choice embedded in institutional enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rejection_symmetry_counterfactual,
    'If initiation norms were symmetric, would rejection rates and divorce rates change? Or is the passive-norm a stable equilibrium irrespective of norms?',
    'Natural experiments in subcultures/online platforms with norm reversal; longitudinal comparison of match quality and relationship stability between symmetric and asymmetric initiation populations',
    'If rejection/stability improves with symmetry: constraint is pure extraction (Snare confirmed). If they remain unchanged: constraint is neutral coordination (Rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rejection_symmetry_counterfactual, empirical, 'Whether symmetric initiation norms affect rejection rates and stability').

omega_variable(
    male_initiation_cost_internalization,
    'Do high-value males actually experience lower rejection cost, or have they internalized the norm such that they perceive equal cost but with lower frequency?',
    'Comparative rejection data controlling for match quality; psychological measures of rejection sensitivity across initiation-frequency groups; field experiments in norm-reversed environments',
    'If actual cost is lower: the asymmetry is real and extraction is structural. If cost is equal but frequency is lower: extraction is psychological (norm-induced), not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(male_initiation_cost_internalization, empirical, 'Whether male rejection cost is materially lower or norm-internalized').

omega_variable(
    norm_erosion_rate_technological,
    'Will online dating platforms and decoupled-from-reputation contexts (anonymity, geographic mobility) erode the asymmetric initiation norm faster than generational replacement?',
    'Comparative initiation rates online vs offline; generational cohort analysis; adoption rates of norm-reversal in new dating technologies',
    'If erosion is fast: constraint moves toward Scaffold (temporary, sunset) classification. If slow: Snare classification persists across generations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(norm_erosion_rate_technological, empirical, 'Rate of norm erosion in decoupled technological contexts').

omega_variable(
    female_selection_quality_under_passivity,
    'Do passive selection norms actually grant women higher-quality matches (by their own assessment) or do they select for high male visibility (status signaling) at the cost of compatibility?',
    'Relationship satisfaction surveys across initiation patterns; match quality metrics (compatibility scores, outcome stability); analysis of correlation between male initiation rate and partner satisfaction',
    'If quality is higher: constraint provides genuine coordination benefit (Rope/Tangled Rope). If quality is lower: passivity is a pure extraction mechanism targeting women''s matching efficiency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(female_selection_quality_under_passivity, empirical, 'Whether passive selection norms improve match quality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_market_asymmetry_2026, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mmas_tr_t0, marriage_market_asymmetry_2026, theater_ratio, 0, 0.32).
narrative_ontology:measurement(mmas_tr_t50, marriage_market_asymmetry_2026, theater_ratio, 50, 0.38).
narrative_ontology:measurement(mmas_tr_t100, marriage_market_asymmetry_2026, theater_ratio, 100, 0.44).

% Extraction over time
narrative_ontology:measurement(mmas_be_t0, marriage_market_asymmetry_2026, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(mmas_be_t50, marriage_market_asymmetry_2026, base_extractiveness, 50, 0.53).
narrative_ontology:measurement(mmas_be_t100, marriage_market_asymmetry_2026, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_market_asymmetry_2026, information_standard).
narrative_ontology:affects_constraint(marriage_market_asymmetry_2026, gender_based_social_feedback_cost).
narrative_ontology:affects_constraint(marriage_market_asymmetry_2026, male_rejection_frequency_asymmetry).

% DUAL FORMULATION NOTE:
% The asymmetric information snare decomposes into two structurally distinct claims: (1) Gender-based social feedback cost for initiation (ε≈0.52, Snare) — women bear reputational penalty men do not. (2) Male rejection frequency asymmetry (ε≈0.38, Tangled Rope) — high-value males face lower rejection frequency due to selectivity, creating coordination problem. These are linked but separate constraints; the snare emerges from their combination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_market_asymmetry_2026, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
