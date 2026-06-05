% ============================================================================
% CONSTRAINT STORY: sotu_1991_bush_soviet_conditional_engagement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1991_bush_soviet_conditional_engagement, []).

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
 *   constraint_id: sotu_1991_bush_soviet_conditional_engagement
 *   human_readable: Conditional Diplomatic Engagement with Soviet Union (SOTU 1991)
 *   domain: foreign_policy/geopolitics/conditional_engagement
 *
 * SUMMARY:
 *   The conditional diplomatic engagement mechanism announced in President
 *   Bush's 1991 State of the Union address established a quid pro quo
 *   framework linking U.S.-Soviet cooperation to observable Soviet behavioral
 *   compliance: withdrawal from Baltic States, reopening of dialogue with
 *   Soviet republics, and demonstrable movement toward democratization. This
 *   constraint operates at the intersection of geopolitical realpolitik and
 *   liberal internationalism — it is simultaneously a coordination mechanism
 *   (solving the credibility problem of how to shift from containment to
 *   engagement) and an extraction mechanism (imposing costs on Soviet
 *   leadership while benefiting the U.S. and Baltic liberation movements).
 *   The constraint's classification varies dramatically across perspectives:
 *   the U.S. experiences it as pure coordination (Rope); Soviet military
 *   experiences it as pure extraction (Snare); Soviet leadership experiences
 *   it as a hybrid with internal political costs (Tangled Rope); Baltic
 *   movements experience it as beneficial but identity-locking (Tangled Rope
 *   with identity_locked exit); the international democratization regime
 *   experiences it as temporary pressure with an implicit sunset (Scaffold);
 *   the Cold War institutional apparatus experiences it as performative
 *   rhetoric maintaining obsolete structures (Piton); and the civilizational
 *   observer risks naturalizing it as an immutable law of great-power
 *   politics (Mountain — a false summit).
 *
 * KEY AGENTS:
 *   - U.S. Government: Primary beneficiary (institutional/arbitrage) — gains diplomatic leverage, normalization options, strategic cooperation without major concessions
 *   - Soviet Military/Industrial Complex: Primary victim (powerless/trapped) — forced to relinquish regional leverage and sphere-of-influence control with no exit option
 *   - Soviet Leadership (Gorbachev faction): Secondary actor (powerful/constrained) — positioned between reformers benefiting from engagement and hardliners bearing extraction cost; identity split between Soviet preservation and Western integration
 *   - Baltic Independence Movements: Secondary beneficiary with identity-lock (moderate/identity_locked) — gain external leverage and international recognition but become dependent on U.S. enforcement; cannot accept partial solutions due to national independence commitment
 *   - International Democratization Regime: Organized beneficiary (organized/constrained) — experience low extraction through scaffold logic; mechanism is meant to be temporary pressure toward regime transformation
 *   - Cold War Institutional Apparatus: Institutional actor maintaining inertial structures (institutional/arbitrage) — sees engagement conditionality as rhetorical cover for continued containment enforcement
 *   - Analytical Observer: Civilizational position at risk of false summit (analytical/analytical) — may naturalize contingent diplomatic strategy as inevitable law of great-power politics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1991_bush_soviet_conditional_engagement, 0.52).
domain_priors:suppression_score(sotu_1991_bush_soviet_conditional_engagement, 0.58).
domain_priors:theater_ratio(sotu_1991_bush_soviet_conditional_engagement, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1991_bush_soviet_conditional_engagement, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1991_bush_soviet_conditional_engagement, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sotu_1991_bush_soviet_conditional_engagement, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1991_bush_soviet_conditional_engagement, tangled_rope).
narrative_ontology:human_readable(sotu_1991_bush_soviet_conditional_engagement, "Conditional Diplomatic Engagement with Soviet Union (SOTU 1991)").
narrative_ontology:topic_domain(sotu_1991_bush_soviet_conditional_engagement, "foreign_policy/geopolitics/conditional_engagement").

domain_priors:requires_active_enforcement(sotu_1991_bush_soviet_conditional_engagement).
narrative_ontology:has_sunset_clause(sotu_1991_bush_soviet_conditional_engagement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1991_bush_soviet_conditional_engagement, united_states_government).
narrative_ontology:constraint_beneficiary(sotu_1991_bush_soviet_conditional_engagement, baltic_independence_movements).
narrative_ontology:constraint_victim(sotu_1991_bush_soviet_conditional_engagement, soviet_leadership).
narrative_ontology:constraint_victim(sotu_1991_bush_soviet_conditional_engagement, soviet_military_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOVIET MILITARY COMMAND (SNARE) — Faces concrete extraction through conditional engagement terms. Withdrawal from Baltics represents territorial/strategic loss with no alternative exit path. The mechanism is pure extraction: military must choose between sphere-of-influence loss or diplomatic isolation. Theater ratio is low here (actual military redeployment required, not performative); suppression is structural (geopolitical reality, not coercive enforcement from within). This agent experiences maximum extraction with no compensation mechanism.
constraint_indexing:constraint_classification(sotu_1991_bush_soviet_conditional_engagement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SOVIET LEADERSHIP (TANGLED ROPE) — Gorbachev's position contains both coordination benefit and extraction. The conditional engagement mechanism enables genuine coordination: explicit behavioral targets (withdrawal, democratization dialogue) align incentives for both U.S. and reform faction within Soviet Union. But the mechanism also extracts: compliance costs are borne by Soviet military and hardliners who lose regional leverage. Soviet leadership's exit options are constrained — rejecting the terms means isolation; accepting means internal political cost. This is hybrid: real coordination value (clear behavioral benchmarks) + asymmetric extraction (reform benefits domestically, hardliners bear cost).
constraint_indexing:constraint_classification(sotu_1991_bush_soviet_conditional_engagement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: U.S. GOVERNMENT (ROPE) — Experiences the constraint as pure coordination mechanism. The quid pro quo structure solves the credibility problem: U.S. commits to positive engagement (trade normalization, diplomatic access, security cooperation) in exchange for observable Soviet behavioral change. U.S. has arbitrage options: can defect to containment, can renegotiate terms, can shift engagement focus to other powers. Low suppression and low theater from this perspective — behavioral targets are measurable and concrete. The constraint enables the coordination both parties claim to want.
constraint_indexing:constraint_classification(sotu_1991_bush_soviet_conditional_engagement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BALTIC INDEPENDENCE MOVEMENTS (TANGLED ROPE) — Structurally mobile (could organize militarily, could accept autonomy within Soviet framework) but identity-locked by national independence commitment. The conditional engagement mechanism provides external leverage — U.S. conditional support ties Soviet compliance to Baltic liberation. Tangled rope structure: genuine coordination (liberation movements and U.S. interests partially align) but with extraction embedded — the movements become dependent on U.S. enforcement of the conditions, losing autonomous agency. Identity lock prevents them from accepting partial solutions (autonomy without independence) that might be available at lower cost. Exit from the constraint would require abandoning national independence identity.
constraint_indexing:constraint_classification(sotu_1991_bush_soviet_conditional_engagement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL DEMOCRATIZATION REGIME (SCAFFOLD) — The conditional engagement mechanism embeds democratization conditionality that is structurally temporary. The sunset logic is implicit: as Soviet Union transitions to democracy or dissolves, the conditions themselves become moot or are replaced by governance-based relations. Organized actors (human rights organizations, Western democracies, Council of Europe) experience low effective extraction because the mechanism has an implicit exit path — it is meant to be temporary pressure toward regime change, not permanent extraction. Theater ratio is moderate (democratization commitment is subject to interpretation) but sunset clause is real (the mechanism only makes sense if Soviet system transforms).
constraint_indexing:constraint_classification(sotu_1991_bush_soviet_conditional_engagement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: COLD WAR INSTITUTIONAL APPARATUS (PITON) — From the civilizational perspective, the conditional engagement mechanism is a performative gesture layered over the persistent Cold War structure. The institutional apparatus for containment (NATO, military-industrial complex, strategic doctrine) persists largely unchanged by conditional engagement rhetoric. Theater ratio is high (the conditions are performatively enforced through diplomatic statements and summits) even though the underlying geopolitical reality shifts slowly. This perspective sees the SOTU commitment as theatrical maintenance of legitimacy for an institution (Cold War enforcement) that is about to become obsolete. Piton classification reflects the institutional inertia of containment logic persisting despite stated shift toward engagement.
constraint_indexing:constraint_classification(sotu_1991_bush_soviet_conditional_engagement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / REALPOLITIK VIEW (MOUNTAIN) — From a civilizational/universal perspective, the conditional engagement mechanism might be viewed as reflecting an immutable law of international relations: great powers never truly abandon sphere-of-influence logic; conditional engagement is merely the rhetorical form through which hegemonic interests rationalize material constraints. This perspective sees the extraction mechanism as 'natural' to great power politics — inevitable, not contingent. However, the structural data reveals this as a potential false summit: the conditioning mechanism is genuinely novel (explicit behavioral targets linked to reciprocal incentives) rather than merely rhetorical performance. The 'natural law' framing naturalizes what is actually a contingent diplomatic strategy.
constraint_indexing:constraint_classification(sotu_1991_bush_soviet_conditional_engagement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1991_bush_soviet_conditional_engagement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1991_bush_soviet_conditional_engagement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1991_bush_soviet_conditional_engagement, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1991_bush_soviet_conditional_engagement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1991_bush_soviet_conditional_engagement, TR),
    TR >= 0.70.

:- end_tests(sotu_1991_bush_soviet_conditional_engagement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The mechanism extracts from Soviet military through sphere-of-influence loss and from Soviet hardliners through democratization pressure. However, the extraction is not total — Soviet reformers benefit from engagement opening, and the costs are distributed asymmetrically (military/hardliners bear them; reformers and eventually integration-seeking elites benefit). The initial value (0.35) reflects that conditional engagement is new rhetoric in 1991 without yet producing behavioral extraction; it rises to 0.52 by measurement point 4 as the mechanism demonstrates enforcement through diplomatic isolation and economic conditionality. The plateau at 0.52 rather than continuing rise reflects the constraint reaching its equilibrium force — it cannot extract more without triggering defection or internal collapse. Suppression (0.58): Moderate-high. Soviet leadership faces real barriers to non-compliance: international isolation, economic sanctions, loss of strategic partnership. But suppression is not total — Soviet Union retains nuclear deterrent, alternative alliance options with China, and internal reformist constituency supporting compliance. The suppression mechanism combines external pressure (U.S./NATO isolation) with internal pressure (reformers using conditionality to justify internal reforms). Theater ratio (0.48): Moderate. The behavioral targets (Baltic withdrawal, democratization) are measurable and concrete; this is not pure theater. However, enforcement is partially performative — diplomatic summits, public statements, and media coverage of compliance create theater around the underlying behavioral verification. The theater does not dominate the mechanism, but it is present enough to enable interpretation games about what constitutes compliance. The rising theater ratio in measurements (0.38 → 0.50) reflects increasing ritualization of summits and compliance declarations as the mechanism settles into routine diplomatic practice.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival range is extraordinarily wide — from Rope (U.S.) to Snare (Soviet military) to Tangled Rope (Soviet leadership and Baltics) to Scaffold (democratization regime) to Piton (Cold War apparatus) to Mountain (realpolitik naturalization). The gap between U.S. (Rope) and Soviet Military (Snare) perspectives is maximal: the same mechanism appears as coordination enabling mutual benefit from one position and as pure extraction imposing non-negotiable costs from the other. This gap is not a measurement error or perspectival difference in interpretation — it reflects actual structural reality. The mechanism genuinely does coordinate U.S.-Soviet leadership interests while simultaneously imposing extraction on Soviet military. The Tangled Rope classification (Soviet leadership, Baltic movements) is the structural reality — the constraint contains both genuine coordination benefits and asymmetric extraction costs. The Piton and Mountain perspectives risk naturalizing what is a contingent diplomatic innovation as either institutional inertia or immutable great-power politics. The engine's false summit detector should flag the Mountain perspective — the 'natural law' framing obscures the contingent design of the conditionality mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is determined by each agent's structural relationship to the extraction flow. Soviet military (powerless/trapped) experiences maximum d ≈ 0.95 — they bear the extraction, have no exit, are subordinate to leadership decisions; f(d) ≈ 1.42 — high effective extraction experienced. U.S. government (institutional/arbitrage) experiences low d ≈ 0.15 — they are beneficiary with multiple alternatives (can shift engagement, can maintain containment, can arbitrage with other powers); f(d) ≈ -0.01 — negative or near-zero effective extraction. Baltic movements (moderate/identity_locked) experience d ≈ 0.60 — they benefit structurally but are identity-locked into dependence on U.S. enforcement; f(d) ≈ 0.75 — moderate effective extraction despite beneficiary status, because the identity lock prevents them from exercising their constrained exit options. Soviet leadership (powerful/constrained) experiences d ≈ 0.52 — they are neither pure beneficiary nor pure target, but caught between reformist benefits and hardliner costs; f(d) ≈ 0.65 — moderate experienced extraction, producing the tangled rope classification. The constraint's scope (continental/regional for affected parties, global for institutional actors) scales extractiveness through σ(S): regional scope suppresses chi slightly (σ ≈ 0.9), while global scope amplifies it (σ ≈ 1.2) for institutional observers. Soviet military at regional scope experiences χ ≈ 0.52 × 1.42 × 0.9 ≈ 0.66; U.S. at global scope experiences χ ≈ 0.52 × (-0.01) × 1.2 ≈ -0.006.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the mandatrophy at the civilizational timescale: the conditional engagement mechanism is simultaneously a genuine coordination solution and an extraction mechanism. The resolution requires recognizing that BOTH are true and WHICH one dominates depends on what question you ask. 'Does it solve the credibility problem of shifting from containment to engagement?' YES — pure coordination (Rope from U.S. perspective). 'Does it impose costs on Soviet military without compensation?' YES — pure extraction (Snare from Soviet military perspective). 'Does it benefit some Soviet actors while harming others?' YES — hybrid with internal political splitting (Tangled Rope from Soviet leadership perspective). The mandatrophy dissolves only when you recognize that the constraint's function is to convert geopolitical extraction (sphere-of-influence loss) into coordinated behavioral change. The extraction is real, but it is serving a coordination function — making the power redistribution explicit and conditioned rather than hidden and coercive. This is exactly what Tangled Rope is: genuine coordination embedded with asymmetric costs. The analytical observer risks missing this by either (a) naturalizing the power redistribution as inevitable law (Mountain — false summit) or (b) dismissing the coordination as rhetorical theater (Piton). The correct reading is Tangled Rope + Scaffold: coordination mechanism (explicit behavioral targets) + temporary extraction (conditions designed to shift toward democratization, with implicit sunset when regime transforms).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditionality_enforceability,
    'Can U.S. actually enforce behavioral compliance, or does the conditionality mechanism rely on Soviet internal reform faction to internalize the targets?',
    'Post-1991 empirical tracking: correlation between U.S. conditional statements and observable Soviet/Russian behavioral change. Distinguish between Soviet compliance driven by external conditionality vs. internal reform momentum.',
    'If enforced externally: mechanism is snare for Soviet military (pure extraction). If internalized by reform faction: mechanism is tangled rope (coordination + asymmetric cost distribution). If unenforced: mechanism degrades to piton (theatrical rhetoric without structural force).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditionality_enforceability, empirical, 'Whether conditionality is externally enforced or relies on internal Soviet reform internalization').

omega_variable(
    baltic_agency_independence,
    'Does conditional engagement mechanism enhance or eliminate Baltic agency? Does it create dependency on U.S. enforcement at cost of autonomous liberation capacity?',
    'Comparative historical analysis: Baltic movements with external conditional support vs. movements without such support. Track degree of autonomous decision-making and military/political capacity development.',
    'If enhances agency: conditionality is coordination (lower extraction). If creates dependency: conditionality is extraction mechanism (identity_locked agents unable to exit reliance on U.S.). If neutral: mechanism is primarily theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baltic_agency_independence, empirical, 'Whether external conditionality enhances or undermines Baltic independence agency').

omega_variable(
    soviet_union_terminal_status,
    'Is conditional engagement mechanism targeting a reform of the Soviet system, or is it structurally calibrated for dissolution? Does the sunset clause anticipate regime transformation or state collapse?',
    'Analysis of stated U.S. objectives in 1991 SOTU: did Bush administration expect Soviet Union to persist as reformed democracy or did they anticipate/facilitate dissolution? Cross-reference with subsequent NATO expansion and Russia sanctions doctrine.',
    'If reform-targeting: mechanism is tangled rope (genuine coordination + asymmetric cost). If dissolution-anticipating: mechanism is snare (designed extraction disguised as conditioning). If ambiguous: mechanism is scaffold with hidden end-goal (sunset clause becomes excuse for successive sanctions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soviet_union_terminal_status, conceptual, 'Whether conditional engagement targets Soviet reform or anticipates state dissolution').

omega_variable(
    democratization_criterion_specificity,
    'What constitutes ''demonstrable movement toward democratization''? Are the criteria specified enough to be verifiable, or are they deliberately vague to permit continued conditionality extraction?',
    'Textual analysis of SOTU language vs. subsequent diplomatic communiques. Track whether U.S. government accepts or rejects Soviet/Russian democratization claims as compliant. Identify whether criteria shift with political convenience.',
    'If specific and verifiable: mechanism enables genuine coordination (both parties know the target). If vague: mechanism enables indefinite extraction (moving target disguised as principle).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratization_criterion_specificity, empirical, 'Whether democratization criteria are specific and verifiable or deliberately vague').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1991_bush_soviet_conditional_engagement, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu91_tr_t0, sotu_1991_bush_soviet_conditional_engagement, theater_ratio, 0, 0.38).
narrative_ontology:measurement(sotu91_tr_t2, sotu_1991_bush_soviet_conditional_engagement, theater_ratio, 2, 0.45).
narrative_ontology:measurement(sotu91_tr_t4, sotu_1991_bush_soviet_conditional_engagement, theater_ratio, 4, 0.48).
narrative_ontology:measurement(sotu91_tr_t6, sotu_1991_bush_soviet_conditional_engagement, theater_ratio, 6, 0.5).

% Extraction over time
narrative_ontology:measurement(sotu91_be_t0, sotu_1991_bush_soviet_conditional_engagement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu91_be_t2, sotu_1991_bush_soviet_conditional_engagement, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(sotu91_be_t4, sotu_1991_bush_soviet_conditional_engagement, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(sotu91_be_t6, sotu_1991_bush_soviet_conditional_engagement, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1991_bush_soviet_conditional_engagement, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1991_bush_soviet_conditional_engagement, soviet_union_dissolution_structural_factors).
narrative_ontology:affects_constraint(sotu_1991_bush_soviet_conditional_engagement, nato_expansion_post_1991).
narrative_ontology:affects_constraint(sotu_1991_bush_soviet_conditional_engagement, baltic_state_independence_achievement).

% DUAL FORMULATION NOTE:
% Conditional engagement is downstream of Cold War structural competition but represents a distinct constraint on the specific mechanism of U.S.-Soviet transition. The upstream constraints (Cold War ideological antagonism, nuclear deterrence) establish the background; conditional engagement is the specific diplomatic innovation attempting to manage the transition. Network decomposition: structural factors enabling Soviet dissolution are separate from the conditional engagement mechanism's extractiveness — the engagement mechanism does not cause dissolution but rather shapes how it unfolds. Baltic independence achievement is downstream: conditional engagement provides external leverage but is not the causal driver of independence (internal Soviet dissolution and Baltic mobilization are primary). NATO expansion is downstream consequence: if conditional engagement extracts sphere-of-influence abandonment without corresponding security guarantees, NATO expansion becomes the default outcome.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1991_bush_soviet_conditional_engagement, powerful, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
