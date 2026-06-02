% ============================================================================
% CONSTRAINT STORY: marriage_market_asymmetry_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The marriage market constraint on female initiation operates through
 *   asymmetric information and reputational suppression. Women who propose to
 *   men face both rejection risk and social penalty (framed as 'unladylike,'
 *   'desperate,' 'aggressive'), while men who propose face status enhancement
 *   ('decisive,' 'romantic,' 'masculine'). This asymmetry is not biological
 *   but institutional — it derives from 20th-century Western dating norms
 *   that have become naturalized as evolutionary truth. The constraint
 *   extracts female agency by making female initiation costlier (in
 *   reputation, psychological safety, and relationship option value) than
 *   male initiation. It benefits male gatekeepers by concentrating
 *   information control and enabling screening without reciprocal commitment
 *   cost. The market is inefficient: many potential partnerships never form
 *   because the female party bears too much rejection cost to signal
 *   interest, while the male party waits for unmistakable female signals that
 *   don't arrive. The constraint exhibits all six classification types from
 *   different perspectives, making it a diagnostic case for how institutional
 *   arrangements masquerade as natural law.
 *
 * KEY AGENTS:
 *   - Female Proposer: Primary victim (powerless/trapped) — bears full reputational and psychological cost of initiation; has no exit option without social penalty
 *   - Male Gatekeeper: Primary beneficiary (moderate/constrained) — controls information and commitment signaling; benefits from female hesitation; constrained by reciprocal asymmetry when targeting women
 *   - Dating App Intermediary: Secondary beneficiary (institutional/arbitrage) — profits from constraint asymmetry; can arbitrage by designing algorithms that reduce initiation friction or redistribute signaling norms
 *   - Feminist Organizing Movement: Counter-beneficiary (organized/mobile) — actively building alternative norms and institutional rules (e.g., mutual swiping with explicit interest signals); has exit path and agency
 *   - Traditional Marriage Ritual System: Inertial holder (powerful/arbitrage) — maintains performative proposal ritual; acknowledges its degradation through meta-performance (elaborate surprise proposals)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks false naturalization of institutional norms as evolutionary constraints
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_market_asymmetry_2026, 0.58).
domain_priors:suppression_score(marriage_market_asymmetry_2026, 0.68).
domain_priors:theater_ratio(marriage_market_asymmetry_2026, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_market_asymmetry_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_market_asymmetry_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(marriage_market_asymmetry_2026, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_market_asymmetry_2026, snare).
narrative_ontology:human_readable(marriage_market_asymmetry_2026, "The Asymmetric Information Snare (Women Asking Out)").
narrative_ontology:topic_domain(marriage_market_asymmetry_2026, "social/psychological/economic").

domain_priors:requires_active_enforcement(marriage_market_asymmetry_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_market_asymmetry_2026, male_proposer_gatekeepers).
narrative_ontology:constraint_victim(marriage_market_asymmetry_2026, female_proposer_targets).
narrative_ontology:constraint_victim(marriage_market_asymmetry_2026, relationship_market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEMALE PROPOSER (SNARE) — Trapped by reputational risk, social stigma, and asymmetric rejection costs. Cannot exit without bearing substantial social penalty. Maximum experienced extraction through suppression of alternative strategies. The constraint extracts psychological cost (shame, humiliation risk) and opportunity cost (foregone relationship possibilities).
constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MALE GATEKEEPER (TANGLED ROPE) — Derives benefit from the informational advantage (he can observe female interest without reciprocal commitment cost). Also coordinates through the mechanism: male proposal clarity enables female selection. But experiences constraints on his own female targeting through reciprocal asymmetry. Constrained exit through cultural norm internalization.
constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DATING APP INTERMEDIARY (ROPE) — Benefits from constraint asymmetry (algorithm matches but cannot force reciprocal signaling norms). Experiences the constraint as pure coordination: standardizing 'who can initiate' reduces matching friction. High arbitrage options through algorithm design and norm-setting.
constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FEMINIST ORGANIZING MOVEMENT (SCAFFOLD) — Sees the constraint as a temporary institutional failure with clear sunset. Norms around female initiation are actively being reset through social organizing, media representation, and institutional (dating app) rule changes. Mobile agents with explicit exit path. Theater is moderate because the sunset is programmatic, not random.
constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL MARRIAGE RITUAL SYSTEM (PITON) — The 'man proposes' ritual is increasingly performative. Actual relationships often form through mutual initiation, dating apps with explicit mutual interest, or ambiguous sequential signaling. The ritual persists through institutional inertia and theatrical performance (proposal as public ceremony) despite low functional necessity. The system acknowledges its own degradation through meta-performance (elaborate surprise proposals, social media staging).
constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / EVOLUTIONARY PSYCHOLOGY VIEW (MOUNTAIN) — Tempting but false naturalizing frame: 'male proposal is rooted in biological investment asymmetry, parental uncertainty, and evolutionary mating psychology.' This perspective risks misclassifying a contingent institutional arrangement (20th-century Western dating norms) as an immutable constraint. The engine's false summit detector should identify this: the structural data shows suppression is cultural/informational, not biological.
constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_market_asymmetry_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_market_asymmetry_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(marriage_market_asymmetry_2026, TR),
    TR >= 0.70.

:- end_tests(marriage_market_asymmetry_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts female agency through the reputational and psychological cost of initiation. Over the 50-year interval, extractiveness has increased (0.42 → 0.58) as awareness of the norm has sharpened while compliance pressure has tightened — the constraint becomes more visible precisely as it becomes more contested. Suppression (0.68): High. Multiple suppression mechanisms: (a) reputational cost (social judgment as 'unladylike'), (b) psychological cost (rejection risk asymmetry), (c) institutional enforcement (dating norms, media representation, peer pressure), (d) economic reinforcement (female employment/independence lagging creates exit barriers). Theater ratio (0.55): Moderate. Dating rhetoric emphasizes 'natural' gendered initiation roles, but actual relationship formation increasingly involves mutual signaling. The theater has increased over the interval as norms have become more visible and contested — the ritual must work harder to maintain legitimacy as its artificiality becomes apparent.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme across the observation site. The female proposer experiences a Snare (maximum extraction, no exit). The male gatekeeper experiences a Tangled Rope (benefits from coordination + information control, but also experiences reciprocal constraints). The dating app intermediary experiences a Rope (pure coordination opportunity). The feminist movement experiences a Scaffold (temporary problem with explicit sunset). The traditional ritual system experiences a Piton (degraded functionality maintained through theater). The civilizational analytical view risks Mountain (false naturalization). The first-person experiences are maximally divergent because the constraint operates through distributed enforcement: each agent sees the constraint from their structural position and rationalizes it through their local incentives. The female proposer sees suppression. The male gatekeeper sees coordination. The app intermediary sees an arbitrage opportunity. The movement sees a temporary institutional failure. The ritual system sees its own performance. The analyst risks seeing a law of nature. No single perspective is 'the' constraint — the presheaf IS the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Female proposers have high d (0.85-0.95) because they are both victims of the constraint (bear suppression costs) and trapped with minimal exit options (social penalty makes exit costly). Male gatekeepers have moderate d (0.50-0.60) because they benefit from the constraint asymmetry but are also constrained by reciprocal expectations when they pursue female targets. Dating app intermediaries have low d (0.10-0.20) because they can arbitrage by changing algorithm rules — they experience the constraint as a coordination opportunity, not an extraction mechanism. The feminist organizing movement has low d (0.20-0.30) despite being constrained because their agency and explicit exit path (norm-setting, institutional reform) reduce their effective extraction experience. The traditional marriage ritual system has negative d (approaching 0.0) because it is a beneficiary of the constraint maintained through inertia. The analytical observer's d is high (0.70-0.75) because the analytical perspective bears the cost of false naturalization (misattributing institutional effects to biological law).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This is the core exemplar of mandatrophy in social constraints. The temptation is to classify as Mountain ('rooted in biology / investment asymmetry / parental uncertainty') or as pure Rope ('elegant matching mechanism that allocates who-proposes signal efficiently'). The snare classification resolves the mandatrophy by demonstrating that the constraint is NOT rooted in biological law and DOES asymmetrically extract female agency. The empirical test: if the constraint were biological (Mountain), removing it should be impossible or harmful. If it were a pure coordination mechanism (Rope), norm-shifting should have no cost. Instead, we observe: (a) communities successfully invert the norm without biological consequences, (b) the constraint creates net inefficiency (many potential partnerships don't form due to female initiation cost), (c) the beneficiary (male gatekeepers) actively maintain the constraint despite its inefficiency, (d) the victim (female proposers) experience measurable psychological and opportunity costs that persist even when logically they 'should' disappear (rational choice theory predicts norm-shift, but emotional suppression persists). This pattern is diagnostic of a Snare: an extraction mechanism masquerading as coordination or natural law. The theater ratio (0.55 and rising) reflects the increasing performativity required to maintain the norm as its artificiality becomes visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reputation_cost_measurement,
    'How much of the suppression magnitude derives from actual relationship rejection cost versus reputational/social penalty independent of relationship outcome?',
    'Longitudinal study comparing: (a) female initiation in anonymous contexts vs signed contexts, (b) outcomes of female-initiated relationships vs male-initiated (controlling for selection effects), (c) self-reported shame cost vs actual relationship formation rates',
    'If reputational cost > 50% of total suppression: the constraint is primarily a coordination signal problem (potentially Rope). If reputational cost < 30%: the constraint is primarily about rejection asymmetry (solidly Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reputation_cost_measurement, empirical, 'Magnitude of reputation-based suppression versus outcome-based suppression').

omega_variable(
    norm_shift_irreversibility,
    'Is the norm shift toward female initiation parity irreversible once cultural inversion occurs, or do communities regress to male-propose-only asymmetry?',
    'Comparative analysis across cultures/communities with different acceptance rates for female initiation; time series of acceptance norms in cohorts; qualitative study of male gatekeeper response to norm inversion',
    'If shift is irreversible: Scaffold classification confirmed, sunset is structural. If communities regress: Scaffold is aspirational; the Snare reconsolidates under pressure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(norm_shift_irreversibility, empirical, 'Whether cultural norm inversion toward female initiation is permanent').

omega_variable(
    information_asymmetry_necessity,
    'Does the male-gatekeeper role actually solve an information problem (distinguishing genuine interest from casual signal), or is it a mechanism for extracting commitment signals under asymmetric risk?',
    'Comparison of relationship stability, honesty of initial mutual interest, and match quality in contexts with symmetric vs asymmetric initiation norms; analysis of how information asymmetry affects negotiation of relationship terms (commitment, timeline, fidelity)',
    'If gatekeeper role genuinely solves information problem: the constraint is Tangled Rope (mixed coordination/extraction). If it primarily extracts commitment signals: the constraint is Snare with coordination rationalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_necessity, empirical, 'Whether male gatekeeper role serves genuine information function or pure extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_market_asymmetry_2026, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mma_tr_t0, marriage_market_asymmetry_2026, theater_ratio, 0, 0.38).
narrative_ontology:measurement(mma_tr_t25, marriage_market_asymmetry_2026, theater_ratio, 25, 0.48).
narrative_ontology:measurement(mma_tr_t50, marriage_market_asymmetry_2026, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(mma_be_t0, marriage_market_asymmetry_2026, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mma_be_t25, marriage_market_asymmetry_2026, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(mma_be_t50, marriage_market_asymmetry_2026, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_market_asymmetry_2026, information_standard).
narrative_ontology:affects_constraint(marriage_market_asymmetry_2026, courtship_signaling_asymmetry).
narrative_ontology:affects_constraint(marriage_market_asymmetry_2026, female_labor_market_bargaining_power).

% DUAL FORMULATION NOTE:
% The marriage market asymmetry decomposes into two structurally distinct constraints: (1) signaling asymmetry (who can initiate without reputational cost) — ε=0.58, Snare; (2) female economic dependency during courtship phase — ε=0.62, Tangled Rope. The signaling asymmetry is the more direct form of the constraint analyzed here. The economic dependency operates as a reinforcing mechanism, making female initiation more costly because female agency is economically constrained. Together they form an extractive system, but the signaling asymmetry is the immediate mechanism of suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_market_asymmetry_2026, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
