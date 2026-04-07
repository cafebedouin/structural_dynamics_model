% ============================================================================
% CONSTRAINT STORY: sotu_1977_ford_strategic_arms_limitation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1977_ford_strategic_arms_limitation, []).

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
 *   constraint_id: sotu_1977_ford_strategic_arms_limitation
 *   human_readable: Strategic Arms Limitation Framework (Vladivostok Accord Equal Ceiling)
 *   domain: military/geopolitics/nuclear_deterrence
 *
 * SUMMARY:
 *   The Vladivostok Accord of 1974 and the 1977 Ford SOTU announcement
 *   established an equal ceiling on strategic nuclear weapons (ICBM
 *   launchers, SLBM launchers, and heavy bombers) for the United States and
 *   Soviet Union. This constraint structurally solves a Schelling
 *   coordination problem: both superpowers prefer mutual vulnerability
 *   calibrated to calculable deterrence over unrestrained nuclear escalation,
 *   but neither can unilaterally limit without risking strategic
 *   disadvantage. The equal ceiling makes the coordination focal and enables
 *   mutual commitment through verification mechanisms (national technical
 *   means, on-site inspection). The constraint simultaneously benefits both
 *   institutional actors (stabilized deterrence) and the global civilian
 *   population (reduced nuclear war risk), while extracting costs from
 *   military-industrial sectors seeking unrestricted weapons development and
 *   from third-party nuclear states excluded from the bilateral framework.
 *   The theater ratio (0.42) reflects substantial performative content: the
 *   constraint's stabilizing function relies on deterrence theory assumptions
 *   that may not hold under crisis stress, and the institutional energy
 *   devoted to verification and compliance rituals may exceed the marginal
 *   stabilization gain from the equal ceiling per se (mutual assured
 *   destruction would provide deterrence even with unconstrained arsenals, as
 *   long as second-strike capability is preserved). The extractiveness trend
 *   rises from 0.15 to 0.28 over the interval as qualitative arms race
 *   (accuracy, yield-to-weight improvements) replaces quantitative
 *   competition, and as verification regime costs accumulate without binding
 *   the full scope of strategic competition.
 *
 * KEY AGENTS:
 *   - U.S. Strategic Command & Soviet General Staff (institutional/arbitrage) — Joint beneficiaries of predictable deterrence and reduced escalation risk; coordinate through equal ceiling
 *   - U.S. Military-Industrial Complex (powerful/constrained) — Primary extractor: constrained by treaty caps on weapons development and deployment; bears opportunity cost of forgone programs
 *   - Soviet Military-Industrial Complex (powerful/constrained) — Parallel extractor: constrained by command structure and treaty compliance; bears opportunity cost of unrestricted development
 *   - Global Civilian Population (powerless/mobile) — Primary beneficiary of nuclear war risk reduction; benefits from constraint's stabilizing coordination mechanism
 *   - Arms Control Verification Infrastructure (organized/constrained) — Hybrid agent: benefits from constraint's existence (career, funding) but bears costs of intrusive verification regimes
 *   - Third-Party Nuclear States: France, China, UK (powerful/constrained) — Secondary beneficiary of superpower stability; secondary victim of bilateral framework excluding them from ceiling negotiations
 *   - Analytical Observer (analytical/analytical) — Risks naturalizing contingent institutional stability as inevitable deterrence law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1977_ford_strategic_arms_limitation, 0.28).
domain_priors:suppression_score(sotu_1977_ford_strategic_arms_limitation, 0.35).
domain_priors:theater_ratio(sotu_1977_ford_strategic_arms_limitation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1977_ford_strategic_arms_limitation, extractiveness, 0.28).
narrative_ontology:constraint_metric(sotu_1977_ford_strategic_arms_limitation, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sotu_1977_ford_strategic_arms_limitation, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1977_ford_strategic_arms_limitation, rope).
narrative_ontology:human_readable(sotu_1977_ford_strategic_arms_limitation, "Strategic Arms Limitation Framework (Vladivostok Accord Equal Ceiling)").
narrative_ontology:topic_domain(sotu_1977_ford_strategic_arms_limitation, "military/geopolitics/nuclear_deterrence").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1977_ford_strategic_arms_limitation, us_soviet_mutual_deterrence_stability).
narrative_ontology:constraint_beneficiary(sotu_1977_ford_strategic_arms_limitation, global_nuclear_war_risk_reduction).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: U.S.-SOVIET MUTUAL DETERRENCE SYSTEM (ROPE) — Both superpowers benefit from the equal ceiling framework. The constraint solves the core coordination problem: how to make mutual nuclear vulnerability calculable rather than escalatory. Each side gains predictability of the other's arsenal size, enabling stable second-strike planning. The equal-ceiling principle is the coordination mechanism — it establishes symmetry and removes the incentive for unilateral breakout. Both institutional actors experience the constraint as enabling their deterrence strategy, not constraining it. Arbitrage exit (each could in principle develop other deterrence postures) but unwilling to exercise it because the parity framework delivers superior stability outcomes for both.
constraint_indexing:constraint_classification(sotu_1977_ford_strategic_arms_limitation, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: U.S. MILITARY-INDUSTRIAL WEAPONS DEVELOPMENT SECTOR (SNARE) — Constrained by treaty caps on weapons numbers and deployment modes. The constraint directly restricts revenue opportunities from unrestricted weapons development. High suppression (verification regimes, compliance monitoring, political oversight) limits the sector's ability to pursue alternative strategies (e.g., unilateral buildup, qualitative escalation toward first-strike capability). The sector bears extraction — opportunity cost of forgone weapons programs — but lacks exit (cannot simply withdraw from U.S. military procurement system). Powerful in absolute terms but trapped relative to this specific constraint. Classification as snare reflects that the sector experiences high suppression and extraction with limited exit options, despite its institutional power.
constraint_indexing:constraint_classification(sotu_1977_ford_strategic_arms_limitation, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SOVIET MILITARY-INDUSTRIAL WEAPONS DEVELOPMENT SECTOR (SNARE) — Structurally parallel to the U.S. sector. Constrained by treaty caps and Soviet command structure verification regimes. High suppression from state control and compliance monitoring. Bears extraction through forgone weapons programs and development opportunities. Constrained exit (cannot exit Soviet military procurement system without institutional reorganization). Experiences the constraint as pure extraction from the perspective of unrestricted weapons development aspirations.
constraint_indexing:constraint_classification(sotu_1977_ford_strategic_arms_limitation, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GLOBAL CIVILIAN POPULATION / NUCLEAR WAR RISK REDUCTION (ROPE) — The generational beneficiary of strategic arms limitation. The equal ceiling creates predictable deterrence (calculable mutual vulnerability) rather than open-ended nuclear escalation. Risk of catastrophic global thermonuclear war declines with reduced deployed arsenals and verification-enabled confidence. The global population has mobile exit options (migration, civil defense) but would never exercise them because the constraint's coordination function (stable deterrence) is a genuine public good. Powerless in absolute terms but clear beneficiary from the constraint's stabilizing mechanism. Experiences the constraint as pure coordination, not extraction.
constraint_indexing:constraint_classification(sotu_1977_ford_strategic_arms_limitation, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ARMS CONTROL VERIFICATION AND COMPLIANCE INFRASTRUCTURE (TANGLED ROPE) — National Security Agency analysts, treaty verification specialists, on-site inspection personnel, and verification technology developers occupy a hybrid position. They benefit from the constraint's existence (career, funding, institutional importance — this creates entire fields of expertise). But they also bear costs: the intrusive verification regimes required to enforce the equal ceiling constrain broader intelligence-gathering ambitions and operational secrecy. The constraint creates a genuine coordination function (enabling mutual verification) while extracting compliance costs from both sides. Organized and technically sophisticated, but constrained by treaty obligations and political oversight. Experience simultaneous benefit and burden from the same constraint.
constraint_indexing:constraint_classification(sotu_1977_ford_strategic_arms_limitation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THIRD-PARTY NUCLEAR-ARMED STATES / EUROPEAN AND CHINESE PERSPECTIVES (TANGLED ROPE) — The bilateral U.S.-Soviet ceiling creates coordination benefits (reduced risk of superpower nuclear exchange that could engulf third parties) and extraction costs (constrained ability to develop independent deterrent arsenals during the parity-enforcement period). These states benefit from U.S.-Soviet stability but are also excluded from the equal-ceiling framework, creating a two-tier nuclear system. Powerful and organized (possess independent nuclear arsenals) but constrained by the superpower duopoly. Experience both coordination (stability gains) and extraction (strategic autonomy reduced).
constraint_indexing:constraint_classification(sotu_1977_ford_strategic_arms_limitation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: THE STRATEGIC STABILITY THEATER / CIVILIZATIONAL ANALYTICAL VIEW (PITON) — From a civilizational perspective, the equal-ceiling framework is substantially performative. The constraint's claimed stabilizing function (calculable mutual vulnerability preventing escalation) rests on assumptions about rational deterrence theory that may not hold under stress. The theater ratio reflects that the verification rituals, compliance declarations, and strategic doctrine justifications consume substantial institutional energy relative to what actually prevents nuclear war (mutual assured destruction + second-strike capability, which would exist with or without the equal ceiling). The constraint persists through institutional inertia and political necessity (demonstrating arms control commitment to domestic and allied constituencies) rather than through proven stabilization mechanism. Not yet completely degraded (the framework does reduce some deployment flexibility) but showing signs of theater dominance over function. The 1977 Ford SOTU context shows theater: the political benefits of announcing parity are paramount, while the actual verification mechanisms are incomplete and contested.
constraint_indexing:constraint_classification(sotu_1977_ford_strategic_arms_limitation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: THE ANALYTICAL OBSERVER / PURE COORDINATION FRAME (ROPE) — From the analytical/civilizational position, the equal-ceiling constraint is a pure coordination mechanism solving a Schelling problem: both superpowers prefer mutual limitation to unrestrained escalation, and the equal ceiling provides the focal point that enables coordination. This perspective sees low extractiveness (0.28) and moderate suppression (0.35) as the cost of enabling mutual commitment. The constraint works because it makes defection detectable (verification), makes symmetry transparent (equal ceiling), and makes non-cooperation costly (return to escalation). This is the frame where the constraint is genuinely rope: coordination solving a cooperation problem with minimal coercive overhead. But this frame risks naturalizing what may be contingent institutional stability as though it were inevitable — a variant of false summit risk.
constraint_indexing:constraint_classification(sotu_1977_ford_strategic_arms_limitation, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1977_ford_strategic_arms_limitation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1977_ford_strategic_arms_limitation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1977_ford_strategic_arms_limitation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1977_ford_strategic_arms_limitation, TR),
    TR >= 0.70.

:- end_tests(sotu_1977_ford_strategic_arms_limitation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The constraint benefits both superpowers through reduced escalation risk, so mutual extraction is minimal. The extractiveness flows from the military-industrial sectors bearing opportunity costs of constrained development, not from asymmetric power dynamics between the superpowers. The trend from 0.15 to 0.28 reflects qualitative arms race replacing quantitative ceilings — extractiveness rises as the constraint becomes partially bypassable through capability improvements. Suppression (0.35): Moderate. Verification regimes (national technical means, on-site inspection) impose intrusive monitoring. Compliance mechanisms constrain both sides. But suppression is not severe (0.35 not 0.60+) because both sides choose the constraint, and exit costs (return to escalation, loss of deterrence stability) are high enough that suppression remains voluntary rather than coercive. The constraint induces compliance through mutual preference for the outcome, not through force. Theater ratio (0.42): Moderate-high. The institutional machinery (verification declarations, compliance reviews, strategic doctrine justifications) consumes substantial energy. But the constraint has real binding force: the equal ceiling does prevent some classes of deployment (prevents unilateral breakout toward numerical superiority). Theater is rising over the interval as qualitative competition increases — the quantitative ceiling becomes progressively theatrical as the real arms race happens in unmeasured dimensions (accuracy, yield-to-weight ratio, survivability). Claimed type (rope): Justified at the bilateral institutional level (both superpowers experience coordination benefit). But perspectives from the military-industrial sectors (snare) and third parties (tangled rope) show the constraint is not purely coordinative. From the aggregate view, the predominance of rope perspectives supports claimed type, but the analysis notes strong alternative perspectives.
 *
 * PERSPECTIVAL GAP:
 *   The rope classification at the bilateral institutional level (PERSPECTIVE 1) fundamentally disagrees with the snare classification from the military-industrial sector perspective (PERSPECTIVES 2-3). This gap reflects genuine structural disagreement: the superpowers benefit from the constraint and experience it as enabling their deterrence strategy; the military-industrial sectors bear extraction (opportunity costs of constrained development) and experience suppression (treaty limits, verification regimes, compliance oversight). The powerless global population perspective (PERSPECTIVE 4) also classifies as rope — they are unambiguous beneficiaries of nuclear war risk reduction with no extraction cost. The civilization-level piton perspective (PERSPECTIVE 7) introduces a meta-gap: it questions whether the constraint's stabilizing function is real or performative. If the piton perspective is correct (deterrence stability is pre-existing property of MAD, equal ceiling adds theater not stability), then the entire justification for the constraint collapses and all beneficiary-side rope classifications become false summits. The perspectival gap at the third-party level (tangled rope, PERSPECTIVE 6) reflects the bilateral framework's structural exclusion: these states benefit from superpower stability (genuine coordination component) but lose strategic autonomy (extraction component) through the two-tier nuclear system.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are determined by each agent's structural position: beneficiary status, exit options, and power level. The bilateral superpowers (institutional/arbitrage) derive low d (~0.05-0.15): they benefit from the constraint and have arbitrage exit (could develop alternative deterrence postures) but choose not to exercise it. The military-industrial sectors (powerful/constrained) derive high d (~0.85-0.95): they are pure targets of the constraint, constrained by its limits, with no exit options and no benefit. The global population (powerless/mobile) derives low d (~0.10-0.20): they benefit from the constraint and have mobile exit (though exercising it would be irrational). The verification infrastructure (organized/constrained) derives moderate d (~0.50-0.60): simultaneous benefit and burden. The third-party states (powerful/constrained) derive moderate-high d (~0.65-0.75): net victims of the bilateral framework despite deriving some stability benefit. These directionalities drive the perspectival classifications: beneficiaries with mobile exit see rope; targets with constrained exit see snare; mixed benefit-burden positions see tangled rope. The engine's sigmoid f(d) function converts these d values to experienced extractiveness (chi) scaled by scope modifier σ(S). Global scope (σ=1.2) amplifies extractiveness for third parties and global population; national scope (σ=1.0) normalizes for superpowers and domestic sectors.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely rope at the bilateral institutional level (solving a real coordination problem that both superpowers prefer over alternatives) while simultaneously being snare for the military-industrial sectors (pure extraction with constrained exit). The constraint is not mislabeled — it is legitimately multipolar at different scales. At the superpower scale, it is coordination (rope). At the military-industrial scale, it is extraction (snare). At the global civilian scale, it is public good provision (rope). At the third-party state scale, it is mixed (tangled rope). The mandatrophy resolves by recognizing that no single type is 'the' truth — the presheaf over the observation site is the constraint's true structure. The claim of 'rope' as the base type reflects the dominant institutional perspective (bilateral superpowers) and the beneficiary perspective (global population), which together justify the constraint's existence and political viability. The snare and tangled rope perspectives are subordinate but structurally real — they explain the constraint's political vulnerability and the ongoing resistance from military-industrial sectors. The piton perspective introduces a time-dependent risk: if qualitative escalation continues to bypass quantitative ceilings, theater ratio rises, and the constraint's functional mechanism (calculable mutual vulnerability) degrades without institutional abandonment (degradation to piton). This is the key long-term risk: the constraint persists through inertia even after it ceases to function, because the institutional investments (verification infrastructure, strategic doctrine justifications, compliance bureaucracies) create constituencies with interests in maintaining it regardless of actual stabilization effect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_stability_mechanism_ambiguity,
    'Does the equal ceiling actually stabilize deterrence, or is deterrence stability a pre-existing property of mutual assured destruction that persists regardless of arsenal size as long as second-strike capability exists?',
    'Comparative analysis of nuclear near-miss incidents and escalation risk before/after Vladivostok framework; game-theoretic modeling of escalation probability under constrained vs. unconstrained arsenals with realistic command-and-control failure rates',
    'If stabilizing: constraint is genuine rope (coordination solving escalation risk). If pre-existing: constraint is piton (theater maintaining institutional process that no longer has functional necessity). Theater ratio would rise from 0.42 toward 0.80+.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_stability_mechanism_ambiguity, conceptual, 'Whether equal ceiling actually stabilizes deterrence or duplicates MAD stability').

omega_variable(
    verification_gaming_vulnerability,
    'Can either superpower sustain hidden weapons development programs that defeat the verification regime while maintaining plausible compliance?',
    'Technical analysis of verification capabilities and known gaps (mobile launchers, submarine verification limits, qualitative vs. quantitative cheating pathways); declassified NRO assessments; Soviet compliance assessment reviews',
    'If verification defeats gaming: suppression is real structural (0.35 is accurate). If significant gaming vulnerability: suppression is theater (true suppression lower than 0.35, hidden weapons development reduces constraint''s binding force). Extractiveness remains ~0.28 but mechanism shifts from coordination to illusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_gaming_vulnerability, empirical, 'Whether verification regime prevents hidden weapons development').

omega_variable(
    bilateral_vs_multipolar_escalation,
    'Does stabilizing bilateral U.S.-Soviet parity create stability or fragility in a multipolar nuclear environment with China, France, UK, and emerging nuclear powers?',
    'Game-theoretic analysis of escalation pathways in multipolar nuclear scenarios; crisis simulation with multiple nuclear-armed actors; historical analysis of third-party nuclear states during superpower crises',
    'If stabilizing in multipolar context: constraint is rope from all perspectives (genuine coordination across multiple levels). If destabilizing: constraint is piton for third parties (creates false sense of stability that breaks down in actual multipolar crisis). Third-party perspective reclassifies from tangled_rope to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bilateral_vs_multipolar_escalation, conceptual, 'Whether bilateral parity stabilizes or destabilizes multipolar nuclear environment').

omega_variable(
    qualitative_vs_quantitative_escalation,
    'If quantitative ceilings are fixed by the treaty, will unconstrained qualitative escalation (accuracy, yield-to-weight ratio, survivability improvements) undermine the constraint''s stabilizing function?',
    'Technical analysis of how qualitative improvements affect counterforce capability and first-strike vulnerability; comparison of stability implications of equal numbers with different technical capabilities; historical assessment of Arms Race trajectories during SALT era',
    'If qualitative escalation erodes stability: the equal ceiling is insufficient constraint (theater rises from 0.42 toward 0.65+, extractiveness rises as qualitative arms race consumes resources without binding constraint). If qualitative improvements are negotiated within framework: constraint retains binding force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(qualitative_vs_quantitative_escalation, empirical, 'Whether qualitative escalation undermines quantitative ceiling stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1977_ford_strategic_arms_limitation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sal_tr_t0, sotu_1977_ford_strategic_arms_limitation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sal_tr_t3, sotu_1977_ford_strategic_arms_limitation, theater_ratio, 3, 0.38).
narrative_ontology:measurement(sal_tr_t7, sotu_1977_ford_strategic_arms_limitation, theater_ratio, 7, 0.42).

% Extraction over time
narrative_ontology:measurement(sal_be_t0, sotu_1977_ford_strategic_arms_limitation, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sal_be_t3, sotu_1977_ford_strategic_arms_limitation, base_extractiveness, 3, 0.24).
narrative_ontology:measurement(sal_be_t7, sotu_1977_ford_strategic_arms_limitation, base_extractiveness, 7, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1977_ford_strategic_arms_limitation, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1977_ford_strategic_arms_limitation, soviet_military_doctrine_contingency).
narrative_ontology:affects_constraint(sotu_1977_ford_strategic_arms_limitation, us_nuclear_command_control_procedures).
narrative_ontology:affects_constraint(sotu_1977_ford_strategic_arms_limitation, third_party_nuclear_proliferation_incentives).
narrative_ontology:affects_constraint(sotu_1977_ford_strategic_arms_limitation, military_industrial_arms_race_dynamics).

% DUAL FORMULATION NOTE:
% The strategic arms limitation constraint can be analyzed as (1) a bilateral coordination mechanism solving the superpower escalation dilemma (rope frame), or (2) an extraction mechanism constraining military-industrial weapons development (snare frame). These are not competing models but legitimate observations from different structural positions. The constraint's sustainability depends on maintaining political viability of the rope frame (both superpowers benefit) while managing pressure from the snare frame (military-industrial sectors bear costs). The network links capture how this constraint structurally influences command-and-control procedures, doctrine evolution, and third-party proliferation incentives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1977_ford_strategic_arms_limitation, institutional, 0.12).
constraint_indexing:directionality_override(sotu_1977_ford_strategic_arms_limitation, powerful, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
