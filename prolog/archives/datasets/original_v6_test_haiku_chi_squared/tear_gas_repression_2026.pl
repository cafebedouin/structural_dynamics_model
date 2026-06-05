% ============================================================================
% CONSTRAINT STORY: tear_gas_repression_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tear_gas_repression_2026, []).

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
 *   constraint_id: tear_gas_repression_2026
 *   human_readable: The Tear Gas Riot-Incentive Loop
 *   domain: political/technological/social
 *
 * SUMMARY:
 *   The tear gas riot-incentive loop is a structural constraint where a
 *   purported crowd-management tool functionally incites the very disorder it
 *   claims to manage. A peaceful or moderately disruptive protest —
 *   exercising right to assembly and political voice — is dispersed via
 *   chemical agent (CS or CR gas). The chemical causes immediate respiratory
 *   distress, panic, and defensive behavior indistinguishable from rioting.
 *   State security apparatus then characterizes the post-deployment chaos as
 *   evidence that force was necessary, justifying both the tear gas decision
 *   and requests for expanded security budgets and authority. The constraint
 *   operates on multiple timescales: immediate (the dispersal itself),
 *   biographical (repeated protester cohorts suffer cumulative health
 *   damage), and institutional (security apparatus maintenance and budget
 *   justification). The loop is extractive because peaceful political
 *   participation — a right and essential coordination mechanism in democracy
 *   — is suppressed via incitement. The state apparatus benefits through
 *   budget expansion and authority consolidation. Protesters and their
 *   supporting organizations bear costs through health damage, trauma, and
 *   constrained political voice. The constraint has degraded from an
 *   emergency tool (used in rare high-risk scenarios) to a routine control
 *   mechanism, with theater increasing as legal pretense thins.
 *
 * KEY AGENTS:
 *   - Peaceful Protester: Primary victim (powerless/trapped) — exercises assembly right; chemically dispersed; no meaningful exit except abandoning political voice
 *   - Repeated Protester Cohort: Secondary victim (moderate/constrained) — activists with cumulative exposure; organizational commitment makes exit costly
 *   - State Security Apparatus: Primary beneficiary (institutional/arbitrage) — uses tear gas as efficient authority tool; benefits from escalation justification and budget expansion
 *   - Riot-Manufacturing Subset: Ambiguous actor (powerful/mobile) — tactical units or agitators who escalate post-deployment chaos; benefit from expansion of security operations; also constrained by escalation logic
 *   - Civil Rights Coalition: Secondary beneficiary/victim (organized/mobile) — documents harms (beneficiary function) but also dramatizes victimhood for mobilization (extraction mechanism)
 *   - Tear Gas Supply Chain: Institutional actor (institutional/arbitrage) — maintains markets through regulatory ambiguity (piton); ban in warfare but undefined in domestic law
 *   - Anti-Tear-Gas Movement: Organized reformer (organized/mobile) — building sunset pathway through international bans and jurisdictional policy shifts
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent state choice (tear gas as physics) rather than recognizing it as political extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tear_gas_repression_2026, 0.62).
domain_priors:suppression_score(tear_gas_repression_2026, 0.75).
domain_priors:theater_ratio(tear_gas_repression_2026, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tear_gas_repression_2026, extractiveness, 0.62).
narrative_ontology:constraint_metric(tear_gas_repression_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(tear_gas_repression_2026, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tear_gas_repression_2026, snare).
narrative_ontology:human_readable(tear_gas_repression_2026, "The Tear Gas Riot-Incentive Loop").
narrative_ontology:topic_domain(tear_gas_repression_2026, "political/technological/social").

domain_priors:requires_active_enforcement(tear_gas_repression_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tear_gas_repression_2026, state_security_apparatus).
narrative_ontology:constraint_beneficiary(tear_gas_repression_2026, riot_escalation_justifiers).
narrative_ontology:constraint_victim(tear_gas_repression_2026, peaceful_protesters).
narrative_ontology:constraint_victim(tear_gas_repression_2026, civilian_respiratory_health).
narrative_ontology:constraint_victim(tear_gas_repression_2026, right_to_assembly).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TEAR-GASSED PROTESTER (SNARE) — Peaceful assembly dispersed by chemical agent; escalated violence is punishment for continued presence; no exit except retreat or riot participation. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.64. Trapped: cannot leave the space (home, workplace, transit route) without abandoning political voice.
constraint_indexing:constraint_classification(tear_gas_repression_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: REPEATED PROTESTER COHORT (SNARE) — Career activists; multiple exposures to tear gas cause cumulative health effects; constrained exit because political participation is identity and livelihood; cannot opt out without losing community and organization. d≈0.78, f(d)≈1.12, σ=0.9 → χ≈0.56. Constrained: organizational commitments and reputational capital make exit costly.
constraint_indexing:constraint_classification(tear_gas_repression_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CIVIL RIGHTS COALITION (TANGLED ROPE) — Organized agents (legal advocacy, medical documentation, protest coordination) benefit from tear gas incidents through increased mobilization and donor funding; also bear genuine costs (member health, strategic constraints). d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.40. Mobile: coalition has resources and legal tools to exit dispersal zones; coordination function is real (documentation, safety networks) but extraction mechanism also real (dramatized victimhood).
constraint_indexing:constraint_classification(tear_gas_repression_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE SECURITY APPARATUS (ROPE) — Sees tear gas deployment as coordination tool: dispersing crowd is solving a collective action problem (managing two incompatible masses: protesters demanding change, counter-protesters demanding status quo). From this perspective, chemical agent is efficient equilibrium mechanism. d≈0.10, f(d)≈0.05, σ=1.0 → χ≈0.03. Arbitrage: security apparatus has full exit (change tactics, withdraw, negotiate) but chooses tear gas because it extracts political benefit (justifies larger budget, escalates fears, polarizes).
constraint_indexing:constraint_classification(tear_gas_repression_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: TEAR GAS SUPPLY CHAIN (PITON) — Legal ban on tear gas in warfare (Chemical Weapons Convention) but undefined status in domestic law enforcement creates vestigial classification: tear gas persists because legal status is ambiguous, not because it functions well. theater_ratio=0.68 (regulatory theater; agencies claim chemical is safe, studies contradict). d≈0.08, f(d)≈-0.05, σ=1.2 → χ≈-0.02. Degraded: manufacturers maintain markets through regulatory opacity, not through demonstrated effectiveness.
constraint_indexing:constraint_classification(tear_gas_repression_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANTI-TEAR-GAS MOVEMENT (SCAFFOLD) — International ban campaigns, local prohibitions (France banned tear gas in riots 2023), alternative crowd-management research are building a sunset for chemical dispersal. d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.26. Mobile exit: movement has agency and path forward (policy change, technology replacement, norm shift). Sunset clause implicit: tear gas prohibition is expanding jurisdiction by jurisdiction; estimated 15-20 year horizon for global CWC category-specific ban.
constraint_indexing:constraint_classification(tear_gas_repression_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: RIOT-MANUFACTURING APPARATUS (TANGLED ROPE) — Subset of security apparatus (tactical units, some state-sponsored agitators) who benefit from escalation; also coordinate legitimate crowd management. d≈0.52, f(d)≈0.68, σ=1.1 → χ≈0.45. Mobile: tactical units can deploy other methods but choose tear gas because it incites the riot that justifies their existence and budget. Beneficiary (expanded operations, justified force) and victim (constrained by escalation logic they themselves create).
constraint_indexing:constraint_classification(tear_gas_repression_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 8: PHYSICOCHEMICAL OBSERVER (MOUNTAIN) — From a universal chemistry perspective, tear gas dispersal follows laws of fluid dynamics and mucosal irritation; the effect (dispersal) is mechanically inevitable given exposure. However, this masks the political extraction: the chemical effect is instrumentalized. d≈0.70, f(d)≈1.02, σ=1.0 → χ≈0.62. FALSE SUMMIT: base extractiveness (0.62) contradicts mountain gate (ε ≤ 0.25). The mechanistic inevitability naturalizes a contingent institutional choice.
constraint_indexing:constraint_classification(tear_gas_repression_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tear_gas_repression_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tear_gas_repression_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tear_gas_repression_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tear_gas_repression_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tear_gas_repression_2026, TR),
    TR >= 0.70.

:- end_tests(tear_gas_repression_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. Tear gas deployment extracts political right to assembly and replaces it with state-controlled authorization of protest. The extraction is efficient because the chemical mechanism creates plausible deniability (state claims dispersal was necessary; cloud itself incites the behavior that justifies this claim). The metric captures both the direct suppression (chemical agent prevents assembly) and the indirect mechanism (incitement justifies authority expansion). The value is not at maximum (0.75+) because some protests do occur despite tear gas threat, indicating incomplete suppression. Suppression (0.75): High. Multiple barriers prevent exit: legal barriers (right to assembly cannot be exercised without risk of tear gas), physical barriers (dispersal zones), and health barriers (respiratory effects trap people in the area). Suppression has not reached maximum because some organized groups have developed protective equipment and tactics that reduce risk. Theater ratio (0.68): Moderate-high. The dispersion mechanism itself is not theatrical — tear gas does disperse crowds. But the justification narrative is theatrical: state claims the chemical prevented riots, when the agent itself incites the riot-like response. Theater has increased over the interval as security agencies have deployed tear gas more routinely (first as emergency tool, now as standard control mechanism) while maintaining the emergency narrative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint displays maximum perspectival divergence. The peaceful protester sees pure snare: chemical dispersal of lawful assembly with no legitimate exit. The security apparatus sees rope: efficient coordination of incompatible crowd masses (pro-change, pro-status-quo). The state security sub-group (riot-manufacturing apparatus) sees tangled rope: genuine coordination function but coupled to extraction (escalation that justifies their expansion). The organized civil rights coalition sees mixed rope/snare: they benefit from documentation and mobilization (rope) but members suffer harms (snare). The supply chain sees piton: legal classification ambiguity allows continued market despite declining functional justification. The anti-tear-gas movement sees scaffold: building sunset pathway through policy change. The physicochemical observer risks seeing mountain: chemical dispersal effect is mechanically lawful, naturalizing the political choice to deploy it. The perspectival gap is diagnostic: the larger the gap, the more the constraint functions as hidden extraction masked as neutral tool.
 *
 * DIRECTIONALITY LOGIC:
 *   Peaceful protester: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. No exit except abandoning assembly right. Repeated protester cohort: Victim + constrained → d≈0.78, f(d)≈1.12. High extraction. Organizational commitment reduces exit options; cumulative health effects increase trap depth over time. State security apparatus: Beneficiary + arbitrage → d≈0.10, f(d)≈0.05. Minimal extraction from their perspective (they choose the tool). Arbitrage exit: could use alternative methods but chooses tear gas. Civil rights coalition: Mixed → d≈0.50, f(d)≈0.65. Both beneficiary (mobilization, donor funding) and victim (member health). Mobile exit allows strategic choice of participation. Riot-manufacturing apparatus: Beneficiary + mobile (but operationally constrained by escalation logic) → d≈0.52, f(d)≈0.68. High effective extraction because they create and maintain the escalation narrative. Tear gas supply chain: Institutional + arbitrage → d≈0.08, f(d)≈-0.05. Low extraction from supply perspective (beneficiary through regulatory loophole). Anti-tear-gas movement: Organized + mobile → d≈0.45, f(d)≈0.48. Moderate extraction but declining over time as movement builds exit (policy bans). Analytical observer: d≈0.70, f(d)≈1.02. Risk of over-extraction if observer naturalizes the physicochemical mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE VS. COORDINATION AMBIGUITY RESOLVED: The central question is whether tear gas functions as crowd coordination (rope) or peaceful-assembly suppression (snare). The mandatrophy resolves through the perspectival stack. From the security apparatus view, it coordinates incompatible groups (rope). From the protester view, it suppresses assembly (snare). The base metrics (ε=0.62, suppression=0.75, theater=0.68) indicate that the label 'coordination' is theater masking extraction: if tear gas genuinely coordinated incompatible goals, suppression would be lower and theater lower. The high theater ratio (0.68) indicates regulatory and narrative framing is doing work to justify the tool. The omegas (escalation causation, protest intent baseline, alternative effectiveness) are designed to test whether tear gas is inciting vs. merely dispersing. If omegas resolve to 'tear gas incites riot', snare classification is confirmed and the rope perspective is revealed as false natural law. If omegas resolve to 'tear gas disperses pre-existing riot', rope perspective gains credibility and snare may be overstated. The mandatrophy is not 'which perspective is correct' but 'what does the empirical evidence show about tear gas functionality?' High confidence resolution would require omegas 1 and 5 (escalation causation, protest intent baseline) to both indicate >70% of post-gas violence is induced rather than pre-existing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    escalation_causation,
    'Does tear gas deployment causally incite riot behavior or merely disperse pre-existing disorder?',
    'Comparative analysis: crowd dynamics before/after tear gas deployment across multiple events; studies of protest escalation timeline relative to chemical agent use; interviews with participants on motivation shift',
    'If causal incitement: snare mechanism is proven (tear gas is extraction tool). If merely dispersal: tear gas is coordination tool (rope). Threshold: >70% of post-gas violence is directionally distinct from pre-gas crowd energy = causal incitement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escalation_causation, empirical, 'Whether tear gas deployment causally incites riot escalation or merely disperses existing disorder').

omega_variable(
    health_outcome_suppression,
    'Are health costs of tear gas exposure systematically suppressed in official reporting?',
    'Comparison of medical records (emergency department visits, respiratory diagnoses) in tear-gassed vs non-gassed protest zones; longitudinal health tracking of repeated exposure cohorts; independent medical monitoring vs state health authorities',
    'If suppressed: victimization is real but hidden (snare confirmed). If transparently reported: suppression metric should be lower (0.55→0.40), potentially downgrading snare to tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(health_outcome_suppression, empirical, 'Whether health costs of tear gas are systematically suppressed in official reporting').

omega_variable(
    alternative_dispersal_effectiveness,
    'Do non-chemical crowd-management methods (psychological dispersal, traffic management, negotiation) achieve the same functional outcome with lower incitement?',
    'Controlled field comparison: protest management scenarios assigned randomly to tear gas vs alternative methods; measure crowd dispersal time, injury rates, riot escalation rates, cost to state apparatus',
    'If alternatives are effective: tear gas choice is extractive (snare confirmed). If alternatives fail: tear gas is necessary coordination (rope perspective valid). Threshold: alternative achieves >80% dispersal efficiency and <30% riot escalation = tear gas is contingent choice, not functional necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_dispersal_effectiveness, empirical, 'Whether alternative crowd-management methods achieve comparable outcomes with lower incitement').

omega_variable(
    state_budget_correlation,
    'Does tear gas deployment correlate with requests for expanded state security budgets in subsequent fiscal cycles?',
    'Time-series analysis: tear gas incident frequency vs security budget request timing; interviews with budget officials on causation narratives; comparison across jurisdictions with/without tear gas bans',
    'If strong correlation: extraction mechanism is budget justification (snare mechanism deepened). If no correlation: snare classification may be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_budget_correlation, empirical, 'Whether tear gas deployment correlates with expanded state security budget requests').

omega_variable(
    protest_intent_baseline,
    'What fraction of tear-gassed protests were non-violent before chemical agent deployment?',
    'Video documentation analysis (first-person footage, news archives, protest organizer records); protest permit data and stated objectives; independent observer classifications of crowd status pre-deployment',
    'If >80% non-violent before tear gas: snare mechanism proven (peaceful assembly transformed to riot). If <40% non-violent: tear gas may be reactive to existing disorder (rope or scaffold perspective more valid).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(protest_intent_baseline, empirical, 'Baseline fraction of protests that were non-violent before tear gas deployment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tear_gas_repression_2026, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tgrip_tr_t0, tear_gas_repression_2026, theater_ratio, 0, 0.52).
narrative_ontology:measurement(tgrip_tr_t6, tear_gas_repression_2026, theater_ratio, 6, 0.62).
narrative_ontology:measurement(tgrip_tr_t12, tear_gas_repression_2026, theater_ratio, 12, 0.68).

% Extraction over time
narrative_ontology:measurement(tgrip_be_t0, tear_gas_repression_2026, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(tgrip_be_t6, tear_gas_repression_2026, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(tgrip_be_t12, tear_gas_repression_2026, base_extractiveness, 12, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tear_gas_repression_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(tear_gas_repression_2026, protest_surveillance_infrastructure).
narrative_ontology:affects_constraint(tear_gas_repression_2026, state_security_budget_justification).
narrative_ontology:affects_constraint(tear_gas_repression_2026, respiratory_health_disparity).

% DUAL FORMULATION NOTE:
% Tear gas represents a constraint family with multiple ε-distinct claims: (1) Physicochemical dispersal mechanism (ε≈0.08, Mountain) — chemical properties cause respiratory effects. (2) Crowd management functionality (ε≈0.35, Rope or Tangled Rope) — whether tear gas actually disperses crowds more efficiently than alternatives. (3) Riot incitement mechanism (ε≈0.62, Snare) — whether tear gas deployment causally incites the disorder it purports to manage. This story focuses on (3). Story (1) would be mountain-type; story (2) would be rope-type; story (3) is snare-type. They are linked because claims about (1) are used to justify (2), and outcome of (2) justifies deployment despite (3).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tear_gas_repression_2026, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
