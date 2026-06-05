% ============================================================================
% CONSTRAINT STORY: israel_norwegian_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israel_norwegian_law, []).

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
 *   constraint_id: israel_norwegian_law
 *   human_readable: The Norwegian Law (Amendment to Article 42c)
 *   domain: political/institutional_governance
 *
 * SUMMARY:
 *   Amendment to Article 42c of the Israeli Basic Law permits ministers to
 *   resign from the Knesset while retaining their cabinet positions, with the
 *   next member of their party's election list automatically replacing them
 *   in the legislature. The mechanism emerged as a pragmatic coordination
 *   solution to a structural tension in Israeli parliamentary democracy: the
 *   dual mandate problem. Ministers must serve as both executives and
 *   legislators, creating a workload conflict. The law allows resolution
 *   without triggering new elections or coalition collapse. However, the
 *   mechanism also enables extraction: it gives the executive and coalition
 *   significant flexibility to reshape the Knesset composition without
 *   electoral accountability. Backbench MKs who ascend through this mechanism
 *   occupy precarious positions — they are temporary until a minister
 *   returns, giving them reduced leverage and career prospects. The
 *   constraint exhibits genuine coordination (predictable succession rules,
 *   no constitutional crisis) alongside asymmetric extraction (government
 *   flexibility gained at the cost of legislative independence and electoral
 *   fidelity).
 *
 * KEY AGENTS:
 *   - Executive Ministers: Primary beneficiary (institutional/arbitrage) — shed legislative burden; maintain majority through succession mechanism
 *   - Coalition Governing Parties: Primary beneficiary (organized/arbitrage) — retain Knesset seats without electoral exposure; control succession through party lists
 *   - Backbench MKs (List Successors): Primary victim (powerless/trapped) — temporary status; reduced career prospects; vulnerable to party discipline
 *   - Legislative Opposition: Secondary victim (moderate/constrained) — government can restructure Knesset without triggering elections; reduced leverage for accountability
 *   - Voter Mandate Integrity: Structural victim (powerless/trapped) — original votes elect specific individuals; mechanism allows party to substitute without new mandate
 *   - Democratic Reform Coalition: Organized agents (organized/mobile) — civil society and constitutional reformers pushing for separation of powers; see sunset potential in comprehensive institutional redesign
 *   - Israeli Constitutional System: Institutional actor (institutional/arbitrage) — maintains dual-mandate design; Article 42c patches symptom rather than resolving root tension
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israel_norwegian_law, 0.38).
domain_priors:suppression_score(israel_norwegian_law, 0.42).
domain_priors:theater_ratio(israel_norwegian_law, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israel_norwegian_law, extractiveness, 0.38).
narrative_ontology:constraint_metric(israel_norwegian_law, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(israel_norwegian_law, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israel_norwegian_law, tangled_rope).
narrative_ontology:human_readable(israel_norwegian_law, "The Norwegian Law (Amendment to Article 42c)").
narrative_ontology:topic_domain(israel_norwegian_law, "political/institutional_governance").

domain_priors:requires_active_enforcement(israel_norwegian_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israel_norwegian_law, executive_ministers).
narrative_ontology:constraint_beneficiary(israel_norwegian_law, coalition_governing_parties).
narrative_ontology:constraint_victim(israel_norwegian_law, legislative_representation_fidelity).
narrative_ontology:constraint_victim(israel_norwegian_law, backbench_mks).
narrative_ontology:constraint_victim(israel_norwegian_law, voter_mandate_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BYPASSED BACKBENCH MK (SNARE) — A legislator further down the party list who entered the Knesset through the resignation mechanism, only to be trapped in temporary status. Cannot remove themselves without weakening the coalition; cannot demand permanent seat without party sanction. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.52. Experiences the mechanism as pure extraction of their legislative tenure and legitimacy.
constraint_indexing:constraint_classification(israel_norwegian_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LEGISLATIVE OPPOSITION (TANGLED ROPE) — Benefits from the mechanism's transparency and predictability (coordination function: clear succession rules, no institutional mystery). Simultaneously victimized by the extraction: the government can swap MKs without triggering new elections, reducing the opposition's leverage to force electoral accountability. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.40. Constrained exit: withdrawal from parliament damages their seat count and coalition stability.
constraint_indexing:constraint_classification(israel_norwegian_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COALITION GOVERNMENT (ROPE) — Primary beneficiary. The mechanism is coordination pure and simple: it allows executive ministers to shed legislative duties without new elections or cabinet reshuffles. The government benefits from flexibility (ministers can exit the Knesset without constitutional crisis) while maintaining its Knesset majority. Arbitrage exit: the government can choose to invoke or ignore the mechanism depending on coalition management needs. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.04. Net beneficiary; negative effective extraction indicates pure coordination function.
constraint_indexing:constraint_classification(israel_norwegian_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DEMOCRATIC REFORM COALITION (SCAFFOLD) — Organized civil society and electoral reform advocates see the mechanism as a temporary solution to a deeper problem: should ministers hold dual legislative/executive roles? The sunset logic: if primary legislation abolishes dual mandates entirely (moving to a pure parliamentary or presidential system), the need for Article 42c evaporates. d≈0.45, f(d)≈0.58, σ=1.0 → χ≈0.22. Mobile exit: reform advocates can exit through legislative change; they are not trapped. Low theater: the mechanism itself is transparent and rule-bound, unlike other government workarounds.
constraint_indexing:constraint_classification(israel_norwegian_law, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ISRAELI CONSTITUTIONAL SYSTEM (PITON) — Article 42c represents a degraded patch on a fundamentally unstable institutional design: the dual mandate of ministers as both legislators and executives. The mechanism persists not because it solves the underlying problem but because the underlying problem (dual mandates created by proportional representation and coalition governance) has proven intractable. theater_ratio=0.58 indicates performative elements: the 'resignation' is theatrical — ministers shed legislative status while maintaining executive power, creating a pseudo-exit. The ritual of succession persists through institutional inertia; the foundational issue (should ministers be legislators?) remains unresolved.
constraint_indexing:constraint_classification(israel_norwegian_law, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CONSTITUTIONAL TENSION VIEW (MOUNTAIN) — From a comparative constitutional perspective, the tension between dual mandates and executive effectiveness is inherent to any proportional parliamentary system without clear separation of powers. All such systems face this constraint. However, the structural data (ε=0.38, suppression=0.42, theater=0.58) contradicts the mountain classification. The engine will flag this as a false summit: the 'inherent' framing naturalizes what is actually a contingent institutional choice about how to design parliamentary representation.
constraint_indexing:constraint_classification(israel_norwegian_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_norwegian_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israel_norwegian_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_norwegian_law, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(israel_norwegian_law, TR),
    TR >= 0.70.

:- end_tests(israel_norwegian_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. The mechanism is legitimate in addressing a real coordination problem (minister workload). However, extraction exists in: (a) government flexibility to reshape Knesset without elections, (b) party control over succession (not individual choice), (c) reduced accountability for changing representation. The value reflects that the coordination function is genuine, but asymmetric benefits flow to the executive. Suppression (0.42): Moderate. Barriers to alternative solutions include: constitutional rigidity (changing the Basic Law requires 61 Knesset votes), coalition fragility (any institutional reform risks government collapse), and entrenched dual-mandate practice. However, suppression is not total — democratic reform advocates maintain pressure for comprehensive redesign. Theater ratio (0.58): Moderate-high. The mechanism has performative elements: the 'resignation' is surgical (minister exits legislature but not cabinet), creating appearance of solving the problem without addressing the root dual-mandate conflict. The theatrical element increases over time as the mechanism becomes routine — it loses its original sense of exceptional coordination and becomes ritualized gap-filling.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across institutional levels. The coalition government sees pure coordination (Rope) — the mechanism is a tool for flexible governance. Backbench MKs see pure extraction (Snare) — the mechanism converts them into temporary placeholders with no independent power. The opposition sees mixed coordination and extraction (Tangled Rope) — they benefit from the transparency of succession rules but suffer from reduced electoral leverage. Democratic reformers see a temporary patch with a sunset (Scaffold) — the mechanism will become unnecessary if Israeli institutions separate executive and legislative roles. The constitutional system sees its own degradation (Piton) — the dual-mandate tension persists, solved only by surgical interventions rather than structural change. The civilizational analytical observer risks seeing an inherent natural law (Mountain) — all proportional systems have this problem — but the structural data reveals this as a false summit: the tension is a contingent institutional choice, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Executive ministers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Coalition parties: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.09. Net beneficiary. Backbench MKs: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no exit option. Opposition: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction; constrained exit because withdrawing from parliament damages their position. Voter mandate integrity: Victim + trapped → d≈0.95, f(d)≈1.42. Abstract collective; cannot organize or exit. Reform coalition: Organized + mobile → d≈0.45, f(d)≈0.58. Mobile exit through legislative reform; see mechanism as temporary. Constitutional system: Institutional + arbitrage → d≈0.08, f(d)≈-0.11. Piton classification comes from theater gate and inertia, not high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES MANDATROPHY by recognizing that the mechanism serves two genuinely distinct functions that could justify different classifications: (1) COORDINATION FUNCTION: Article 42c solves a real problem (dual-mandate workload conflict) and provides predictable succession rules that prevent constitutional crisis. This is legitimate rope-like coordination. (2) EXTRACTION FUNCTION: The same mechanism gives the executive and coalition flexibility to reshape Knesset composition without new elections, and converts list successors into temporary, powerless MKs. This is snare-like extraction. The tangled_rope classification captures that BOTH functions are structurally present and significant. The mechanism is not merely coordination being misperceived as extraction (a mandatrophy error), nor is it extraction disguised as coordination. It genuinely performs both roles. The perspectival gap (coalition sees rope, backbench MK sees snare, reformers see scaffold) reflects the true structure: different agents experience different functions because they occupy different structural positions relative to the mechanism. The classification resolves by accepting that all perspectives are accurate to their context.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dual_mandate_necessity,
    'Is the dual mandate of ministers as legislators inherent to proportional representation, or a solvable institutional design problem?',
    'Comparative analysis of parliamentary systems: do other proportional systems require dual mandates? Are there institutional designs that separate executive and legislative roles while maintaining coalition stability?',
    'If inherent: the mechanism is a necessary feature (mountain-like). If solvable: the mechanism is a patch on a design flaw (piton-like). The classification hinges on whether the underlying problem is structural or contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_mandate_necessity, conceptual, 'Whether dual mandates are an inherent feature of proportional representation').

omega_variable(
    electoral_mandate_fidelity,
    'Does replacing an elected MK with a list successor preserve the voter''s original mandate, or does it constitute an unelected transfer of representation?',
    'Legal analysis of democratic theory: does a party''s list authority supersede individual electoral mandates? Empirical tracking of how often successors diverge legislatively from their predecessors.',
    'If mandate is preserved: the mechanism is legitimate coordination (Rope perspective strengthened). If mandate is broken: the mechanism is extraction of voter intent (Snare perspective strengthened). This determines whether the victim classification for ''voter_mandate_integrity'' is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(electoral_mandate_fidelity, conceptual, 'Whether list succession preserves electoral mandate fidelity').

omega_variable(
    coalition_collapse_risk,
    'How often do departures/replacements under Article 42c trigger coalition instability or early elections?',
    'Historical analysis of Israeli coalitions: frequency of Article 42c invocations, correlation with coalition tension or collapse events, comparison to counterfactual (what would have happened without the mechanism).',
    'If frequent triggers: the mechanism is a pressure relief valve (prevents larger extraction; scaffold logic). If rare: the mechanism is cosmetic (piton logic). This affects whether the coordination function is genuine or performative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coalition_collapse_risk, empirical, 'Historical frequency of coalition instability tied to Article 42c succession').

omega_variable(
    backbench_mk_disadvantage_magnitude,
    'What is the actual career damage or legislative disadvantage to a backbench MK who enters via Article 42c succession?',
    'Longitudinal career tracking: do list successors achieve re-election to the same or higher positions? Do they suffer committee assignments, legislative influence, or party retaliation?',
    'If damage is severe: victimhood classification (Snare perspective) is confirmed. If minimal: the mechanism is neutral coordination (Rope perspective). The suppression value (0.42) and backbench victim classification depend on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(backbench_mk_disadvantage_magnitude, empirical, 'Career disadvantage to MKs who enter via list succession').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israel_norwegian_law, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(norwegian_tr_t0, israel_norwegian_law, theater_ratio, 0, 0.38).
narrative_ontology:measurement(norwegian_tr_t15, israel_norwegian_law, theater_ratio, 15, 0.5).
narrative_ontology:measurement(norwegian_tr_t30, israel_norwegian_law, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(norwegian_be_t0, israel_norwegian_law, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(norwegian_be_t15, israel_norwegian_law, base_extractiveness, 15, 0.34).
narrative_ontology:measurement(norwegian_be_t30, israel_norwegian_law, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israel_norwegian_law, enforcement_mechanism).
narrative_ontology:affects_constraint(israel_norwegian_law, israeli_coalition_stability).
narrative_ontology:affects_constraint(israel_norwegian_law, knesset_representation_legitimacy).

% DUAL FORMULATION NOTE:
% Article 42c addresses the dual-mandate problem, which is itself a distinct constraint upstream. The dual-mandate tension (executive vs legislative workload) generates pressure to which Article 42c responds. Decomposition: constraint_dual_mandate_tension (ε≈0.25, structural property of proportional systems) is upstream; israel_norwegian_law (ε=0.38, political mechanism to manage the tension) is downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(israel_norwegian_law, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
