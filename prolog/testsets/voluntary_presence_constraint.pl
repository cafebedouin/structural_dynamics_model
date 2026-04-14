% ============================================================================
% CONSTRAINT STORY: voluntary_presence_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_voluntary_presence_constraint, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: voluntary_presence_constraint
 *   human_readable: Voluntary Presence Constraint in Intimate Relationships
 *   domain: philosophy_of_mind/social_psychology/relationship_ethics
 *
 * SUMMARY:
 *   The voluntary presence constraint operates at the intersection of
 *   philosophy of mind (what constitutes authentic choice), social psychology
 *   (how exit costs shape behavior), and relationship ethics (what conditions
 *   enable genuine care). The constraint's core mechanism: when exit from a
 *   relationship carries catastrophic cost (economic ruin, social isolation,
 *   identity dissolution, child custody loss), continued presence becomes
 *   structurally ambiguous — is the partner staying because they choose to,
 *   or because they cannot afford to leave? This ambiguity extracts authentic
 *   voluntary presence (the epistemic and ethical foundation of genuine care)
 *   while maintaining the theatrical performance of voluntary commitment. The
 *   constraint exhibits cyclical dynamics: tension accumulates → exit
 *   consideration → cost calculation → renewed commitment performance →
 *   tension accumulates. The theater_ratio (0.78) reflects that much
 *   relationship maintenance activity is performative rather than functional
 *   — anniversary celebrations, public displays of affection, and commitment
 *   narratives serve to signal voluntary presence to self and others despite
 *   underlying structural coercion. The constraint is downstream of
 *   epistemic_substitution (the inability to distinguish authentic from
 *   performed presence) and conformity_extraction (social pressure to
 *   maintain relationship stability regardless of quality).
 *
 * KEY AGENTS:
 *   - Trapped Partner: Primary victim (powerless/trapped) — economically dependent, socially isolated, or psychologically bound; cannot exit without catastrophic cost; bears full extraction
 *   - Identity-Fused Partner: Primary victim (moderate/identity_locked) — structurally mobile but cognitively trapped; self-concept constituted through relationship role; exit requires identity dissolution
 *   - Constrained Partner: Secondary victim (moderate/constrained) — faces high but surmountable exit costs; experiences mixed coordination and extraction
 *   - Mobile Partner: Beneficiary (powerful/mobile) — has independent resources and social network; experiences relationship commitments as genuine coordination
 *   - Exit Cost Beneficiaries: Institutional beneficiaries (institutional/arbitrage) — mortgage lenders, family courts, joint asset managers benefit from relationship persistence regardless of quality
 *   - Relationship Reform Coalition: Organized agents (organized/constrained) — building alternative structures to reduce exit costs and enable authentic voluntary presence
 *   - Romantic Love Ideology: Institutional actor (institutional/arbitrage) — maintains theatrical narrative that naturalizes structural entrapment as 'true commitment'
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent structural arrangements as inherent to human attachment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(voluntary_presence_constraint, 0.68).
domain_priors:suppression_score(voluntary_presence_constraint, 0.72).
domain_priors:theater_ratio(voluntary_presence_constraint, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(voluntary_presence_constraint, extractiveness, 0.68).
narrative_ontology:constraint_metric(voluntary_presence_constraint, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(voluntary_presence_constraint, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(voluntary_presence_constraint, snare).
narrative_ontology:human_readable(voluntary_presence_constraint, "Voluntary Presence Constraint in Intimate Relationships").
narrative_ontology:topic_domain(voluntary_presence_constraint, "philosophy_of_mind/social_psychology/relationship_ethics").

domain_priors:requires_active_enforcement(voluntary_presence_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(voluntary_presence_constraint, status_quo_relationship_stability).
narrative_ontology:constraint_beneficiary(voluntary_presence_constraint, exit_cost_beneficiaries).
narrative_ontology:constraint_victim(voluntary_presence_constraint, authentic_voluntary_presence).
narrative_ontology:constraint_victim(voluntary_presence_constraint, trapped_partners).
narrative_ontology:constraint_victim(voluntary_presence_constraint, relationship_epistemic_quality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED PARTNER (SNARE) — Economically dependent, socially isolated, or psychologically bound. Cannot exit without catastrophic cost. The constraint extracts authentic presence while maintaining the appearance of voluntary commitment. Maximum experienced extraction — no agency, no exit, full cost.
constraint_indexing:constraint_classification(voluntary_presence_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: IDENTITY-FUSED PARTNER (SNARE) — Structurally mobile but cognitively trapped. Self-concept is constituted through the relationship role. Exit would require becoming a different person. The constraint extracts through internalized framing rather than external barriers. High extraction despite moderate power because identity lock prevents exercising exit capacity.
constraint_indexing:constraint_classification(voluntary_presence_constraint, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 3: CONSTRAINED PARTNER (TANGLED ROPE) — Faces high but surmountable exit costs: shared property, children, social network disruption, career impact. Genuine coordination exists (shared life infrastructure) alongside extraction (presence maintained partly by cost rather than pure choice). Mixed experience — some agency, some benefit, significant extraction.
constraint_indexing:constraint_classification(voluntary_presence_constraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: MOBILE PARTNER (ROPE) — Has independent income, social network, housing options. Can exit at manageable cost. Experiences the relationship's structural commitments as coordination mechanisms that enable shared projects. Low extraction — presence is genuinely voluntary because exit is genuinely available.
constraint_indexing:constraint_classification(voluntary_presence_constraint, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: EXIT COST BENEFICIARY (ROPE) — Institutional actors who benefit from relationship stability regardless of quality: mortgage lenders, family court systems, joint asset managers, social institutions built on coupled-household assumptions. Experience the constraint as pure coordination — relationship persistence enables their function. Net beneficiary with arbitrage exit.
constraint_indexing:constraint_classification(voluntary_presence_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: RELATIONSHIP REFORM COALITION (SCAFFOLD) — Organized agents building alternative structures: no-fault divorce, domestic violence shelters, economic independence programs, therapeutic frameworks for healthy exit. See the constraint as temporary — exit costs are being systematically reduced through legal reform, economic policy, and cultural change. Sunset logic: as exit becomes genuinely available, authentic voluntary presence becomes structurally possible.
constraint_indexing:constraint_classification(voluntary_presence_constraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ROMANTIC LOVE IDEOLOGY (PITON) — Cultural narrative that 'true love conquers all' and 'commitment means staying through hardship' has degraded into theatrical cover for structural entrapment. The ideology persists through institutional inertia (wedding industry, relationship advice media, religious institutions) despite low functional value. High theater ratio — the performance of voluntary commitment masks involuntary persistence.
constraint_indexing:constraint_classification(voluntary_presence_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — Risks naturalizing the constraint as inherent to human attachment: 'relationships always involve compromise,' 'exit costs are natural consequences of interdependence,' 'authentic presence is philosophically incoherent.' This perspective sees the constraint as an immutable feature of intimate bonds. However, structural data contradicts this — the constraint's extractiveness varies with policy, economic structure, and cultural norms, revealing it as contingent rather than natural.
constraint_indexing:constraint_classification(voluntary_presence_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(voluntary_presence_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(voluntary_presence_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(voluntary_presence_constraint, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(voluntary_presence_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(voluntary_presence_constraint, TR),
    TR >= 0.70.

:- end_tests(voluntary_presence_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts authentic voluntary presence — the epistemic foundation of genuine care — by making exit prohibitively costly. Partners continue relationships despite stated dissatisfaction at rates far exceeding what would occur if exit were genuinely available. The extraction is not total (0.68 rather than 0.85+) because some coordination genuinely exists: shared life infrastructure, children, joint projects. But the extraction is severe enough to qualify as snare from trapped and identity-locked perspectives. Suppression (0.72): High. Multiple mechanisms suppress alternatives: economic dependency (shared assets, income disparity, housing costs), social isolation (relationship-contingent friendships, family pressure), psychological binding (sunk cost fallacy, identity fusion, fear of loneliness), legal barriers (custody arrangements, divorce costs), and cultural narratives that frame exit as moral failure. Suppression is not total because some exit pathways exist (domestic violence shelters, no-fault divorce, economic independence programs), but barriers are severe enough to make exit catastrophic for many. Theater ratio (0.78): High. Much relationship maintenance activity is performative: anniversary celebrations that mask dissatisfaction, public displays of affection that signal commitment to observers, commitment narratives that convince self and others of voluntary presence. The performance serves to maintain the fiction that presence is chosen rather than coerced. Theater has increased over the interval as the gap between relationship ideology (voluntary commitment based on authentic care) and relationship reality (persistence based on exit costs) has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — relationship persistence despite dissatisfaction — appears radically different depending on exit capacity. The trapped partner sees pure extraction (Snare) — they are held captive by economic dependency or social isolation. The identity-locked partner also sees extraction (Snare) but through internalized rather than external mechanisms — their identity is constituted through the relationship role. The constrained partner sees mixed coordination and extraction (Tangled Rope) — genuine shared life infrastructure exists alongside coercive exit costs. The mobile partner sees coordination (Rope) — relationship commitments enable shared projects because exit remains genuinely available. Exit cost beneficiaries see pure coordination (Rope) — relationship stability enables their institutional function. The reform coalition sees a temporary problem with a sunset (Scaffold) — exit costs are being systematically reduced through legal and economic reform. The romantic love ideology sees its own degraded ritual (Piton) — commitment narratives persist through inertia despite low functional value. The analytical observer risks seeing an immutable natural law (Mountain) — exit costs are inherent to interdependence — but structural data reveals this as naturalization of contingent arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality structure reveals how exit capacity determines experienced extraction. Trapped partners (d ≈ 0.95) experience maximum extraction — they are full targets with no exit and no agency. Identity-locked partners (d ≈ 0.89) experience high extraction despite moderate power because their cognitive binding prevents exercising structural exit capacity. Constrained partners (d ≈ 0.85) experience significant extraction but retain some agency through surmountable exit costs. Mobile partners (d ≈ 0.15) experience low extraction — they are partial beneficiaries because their exit capacity makes their presence genuinely voluntary. Exit cost beneficiaries (d ≈ 0.05) experience negative extraction — they benefit from relationship persistence regardless of quality. The directionality gradient maps directly onto the philosophical question: at what point do exit costs convert choice into coercion? The engine's structural answer: when d exceeds approximately 0.70, presence is more coerced than voluntary.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by demonstrating that 'voluntary presence' is not a binary property but a continuous function of exit cost. At low exit cost (mobile partner), presence is genuinely voluntary and the constraint functions as coordination (Rope). At moderate exit cost (constrained partner), presence is partly voluntary and partly coerced, producing mixed coordination and extraction (Tangled Rope). At high exit cost (trapped or identity-locked partner), presence is predominantly coerced and the constraint functions as extraction (Snare). The analytical observer's mountain classification (exit costs are inherent to interdependence) is a false summit — it naturalizes what is actually a policy-dependent variable. The reform coalition's scaffold classification reveals the sunset logic: as exit costs decline through legal reform (no-fault divorce), economic policy (income support, housing access), and cultural change (reduced stigma), the constraint's extractive component diminishes. The constraint is not 'really' a snare or 'really' a rope — it is a snare from the trapped perspective and a rope from the mobile perspective, and both classifications are structurally accurate descriptions of different agents' experiences of the same institutional arrangement. The mandatrophy dissolves when we recognize that the question 'is this constraint extractive or coordinative?' presupposes a single objective answer, when the framework's insight is that extractiveness is indexical — it depends on who you ask.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exit_cost_threshold,
    'What level of exit cost is compatible with genuine voluntary presence? At what threshold does cost convert choice into coercion?',
    'Longitudinal studies correlating exit cost magnitude with relationship satisfaction trajectories; comparison of relationship quality in high-exit-cost vs low-exit-cost structural contexts (e.g., economic independence, no-fault divorce availability)',
    'If threshold is low (e.g., any significant financial disruption): most long-term relationships involve coerced presence. If threshold is high (e.g., only physical danger or total economic ruin): the constraint affects fewer relationships than claimed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_cost_threshold, conceptual, 'Exit cost threshold distinguishing voluntary from coerced presence').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external barriers) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., partner reports continued inability to imagine exit even after gaining economic independence), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. If structural, reducing external barriers (economic policy, legal reform) directly reduces suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    authentic_presence_measurability,
    'Can authentic voluntary presence be distinguished from performed voluntary presence through observable behavior, or is it inherently private/phenomenological?',
    'Behavioral markers: frequency of relationship continuation despite stated dissatisfaction, correlation between exit cost reduction and relationship quality improvement, longitudinal tracking of partners who gain exit capacity',
    'If measurable: the constraint''s victim (authentic voluntary presence) can be empirically tracked. If inherently private: the constraint operates in a domain where extraction is undetectable from outside, making intervention difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authentic_presence_measurability, empirical, 'Measurability of authentic vs performed voluntary presence').

omega_variable(
    interdependence_vs_entrapment,
    'Does healthy interdependence necessarily involve exit costs, or can genuine mutual dependence coexist with low-cost exit?',
    'Case studies of relationships with high interdependence but low exit costs (e.g., economically independent partners with shared projects); comparison of relationship quality in high-interdependence/low-exit-cost vs high-interdependence/high-exit-cost contexts',
    'If interdependence requires exit costs: the constraint is partly inherent to intimate bonds (mountain component). If interdependence can coexist with low exit costs: the constraint is fully contingent on structural arrangements (snare component).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interdependence_vs_entrapment, empirical, 'Whether interdependence necessarily involves exit costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(voluntary_presence_constraint, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(volpres_tr_t0, voluntary_presence_constraint, theater_ratio, 0, 0.55).
narrative_ontology:measurement(volpres_tr_t3, voluntary_presence_constraint, theater_ratio, 3, 0.68).
narrative_ontology:measurement(volpres_tr_t6, voluntary_presence_constraint, theater_ratio, 6, 0.74).
narrative_ontology:measurement(volpres_tr_t10, voluntary_presence_constraint, theater_ratio, 10, 0.78).
narrative_ontology:measurement(volpres_tr_t2, voluntary_presence_constraint, theater_ratio, 2, 0.62).
narrative_ontology:measurement(volpres_tr_t5, voluntary_presence_constraint, theater_ratio, 5, 0.71).
narrative_ontology:measurement(volpres_tr_t8, voluntary_presence_constraint, theater_ratio, 8, 0.76).

% Extraction over time
narrative_ontology:measurement(volpres_be_t0, voluntary_presence_constraint, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(volpres_be_t3, voluntary_presence_constraint, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(volpres_be_t6, voluntary_presence_constraint, base_extractiveness, 6, 0.64).
narrative_ontology:measurement(volpres_be_t10, voluntary_presence_constraint, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(volpres_be_t2, voluntary_presence_constraint, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(volpres_be_t5, voluntary_presence_constraint, base_extractiveness, 5, 0.61).
narrative_ontology:measurement(volpres_be_t8, voluntary_presence_constraint, base_extractiveness, 8, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(voluntary_presence_constraint, attachment_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of epistemic_substitution (the inability to distinguish authentic from performed presence) and conformity_extraction (social pressure to maintain relationship stability). The voluntary presence constraint has its own extractiveness (0.68) reflecting the career and identity costs of exit; the upstream constraints have their own extractiveness values reflecting their distinct mechanisms. The constraint family models how epistemic limits (substitution) and social pressure (conformity) combine to create structural entrapment in intimate relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
