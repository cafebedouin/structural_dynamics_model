% ============================================================================
% CONSTRAINT STORY: false_summit_institutional_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_false_summit_institutional_legitimation, []).

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
 *   constraint_id: false_summit_institutional_legitimation
 *   human_readable: False Summit Institutional Legitimation
 *   domain: institutional_epistemology/organizational_legitimacy
 *
 * SUMMARY:
 *   False summit institutional legitimation occurs when an institution
 *   naturalizes contingent rules as immutable law, converting what are
 *   actually chosen institutional designs into perceived natural necessities.
 *   This naturalizing frame serves a dual function: it coordinates behavior
 *   by establishing stable, predictable rules (genuine coordination
 *   function), and it extracts compliance by making challenge impossible
 *   ('you cannot change the laws of physics'). The false summit differs from
 *   ordinary Snares because it combines real coordination benefit with
 *   extraction mechanism — the institution genuinely does coordinate, but
 *   uses the false naturalizing frame to suppress reform and block
 *   consideration of alternatives. The distinction is crucial: a Snare
 *   extracts through coercive suppression of alternatives; a false summit
 *   extracts through epistemic suppression — making alternatives unthinkable.
 *   Subordinate agents internalize the naturalizing frame (identity lock),
 *   while leadership benefits from the stability-without-renegotiation that
 *   naturalization provides. The constraint's theater ratio (0.81) reflects
 *   that institutional legitimacy itself is highly performative — the
 *   institution must constantly demonstrate that its rules are necessary
 *   through compliance theater, audits, policy documentation, and expert
 *   justification. As the theater increases, agents become less able to
 *   distinguish genuine function from mere performance, deepening the false
 *   summit's hold.
 *
 * KEY AGENTS:
 *   - Institutional Leadership: Primary beneficiary (institutional/arbitrage) — captures stability and coordination benefits without needing to continuously renegotiate foundational rules; can migrate between institutions if false summit collapses
 *   - Subordinate Agents: Primary victim (powerless/trapped) — must accept false naturalizing frame to remain; face exclusion or retaliation for challenge; experience identity lock through internalization of 'natural law' framing
 *   - Peer Reformers: Secondary actor (moderate/constrained) — can see the contingency and coordinate reform, but face high exit costs (career damage, network loss, reputation); constrained by organizational structure and professional consequences
 *   - External Accountability Coalition: Organized observers (organized/constrained) — auditors, regulators, meta-institutions that can provide visibility pressure; have leverage through transparency systems and comparative benchmarking
 *   - Bureaucratic Procedure System: Institutional actor (institutional/arbitrage) — the formal apparatus that maintains the false summit through ritualistic compliance; persists through inertia (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the false summit itself as an unavoidable feature of institutions; vulnerable to the same epistemic mechanism the constraint uses
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(false_summit_institutional_legitimation, 0.52).
domain_priors:suppression_score(false_summit_institutional_legitimation, 0.58).
domain_priors:theater_ratio(false_summit_institutional_legitimation, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(false_summit_institutional_legitimation, extractiveness, 0.52).
narrative_ontology:constraint_metric(false_summit_institutional_legitimation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(false_summit_institutional_legitimation, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(false_summit_institutional_legitimation, tangled_rope).
narrative_ontology:human_readable(false_summit_institutional_legitimation, "False Summit Institutional Legitimation").
narrative_ontology:topic_domain(false_summit_institutional_legitimation, "institutional_epistemology/organizational_legitimacy").

domain_priors:requires_active_enforcement(false_summit_institutional_legitimation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(false_summit_institutional_legitimation, institutional_leadership).
narrative_ontology:constraint_beneficiary(false_summit_institutional_legitimation, status_quo_beneficiaries).
narrative_ontology:constraint_victim(false_summit_institutional_legitimation, subordinate_agents).
narrative_ontology:constraint_victim(false_summit_institutional_legitimation, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATE AGENT (SNARE) — Faces maximal extraction without exit. The subordinate must accept the false summit framing (naturalize contingent rules as immutable law) to remain within the institution. Challenge the naturalizing claim and face exclusion or retaliation. Zero degrees of freedom for internal critique. The extraction mechanism is behavioral suppression through selective visibility control.
constraint_indexing:constraint_classification(false_summit_institutional_legitimation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PEER REFORMER (TANGLED ROPE) — Moderate power with high exit costs. The reformer sees both the genuine coordination function (shared institutional standards enable coordination) and the false summit mechanism (naturalizing contingency to block reform). Can exit but faces career damage, loss of network access, and reputational cost. Experiences both extraction and coordination benefit — the institution does coordinate but extracts compliance through false necessity framing.
constraint_indexing:constraint_classification(false_summit_institutional_legitimation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL LEADERSHIP (ROPE) — Net beneficiaries with high exit capacity (can move between institutions, migrate leadership roles, or reframe the institution entirely). Experiences the false summit as pure coordination: the naturalizing claim stabilizes the institution, enables delegation, and prevents constant renegotiation of foundational rules. Leadership has no motivation to challenge the false summit because the false framing serves coordination goals. Experiences low extraction because they control the framing — effective d near 0.0.
constraint_indexing:constraint_classification(false_summit_institutional_legitimation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EXTERNAL ACCOUNTABILITY COALITION (SCAFFOLD) — Organized external actors (auditors, regulators, boards, meta-institutions) can see the false summit mechanism and provide pressure for visibility and reform. Their scrutiny has a sunset clause: as transparency systems mature (open data, performance audits, comparative benchmarking), the ability to naturalize contingent rules diminishes. The constraint relaxes as external accountability mechanisms take hold. Theater ratio declines as the false summit cannot survive disclosure.
constraint_indexing:constraint_classification(false_summit_institutional_legitimation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: BUREAUCRATIC PROCEDURE SYSTEM (PITON) — The formal apparatus of institutional procedures (policy documents, governance structures, compliance frameworks) persists through inertia long after the original functional justification has atrophied. Procedures are maintained performatively — the institution performs compliance with its own rules as evidence of legitimacy. The procedure system has become decoupled from coordination function, sustained only by the false summit framing that 'this is how things must work.' High theater ratio (0.81) reflects that most procedure execution is ritualistic performance rather than functional necessity.
constraint_indexing:constraint_classification(false_summit_institutional_legitimation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From the analytical civilizational view, the false summit emerges as an attempted mountain classification: the institution naturalizes contingent rules as immutable law ('this is how institutions must work,' 'coordination requires hierarchy,' 'efficiency demands centralization'). The false summit is the mechanism by which subordinate agents are taught to see contingency as necessity. However, the structural data immediately reveals this mountain as false: extractiveness (0.52) is far above the mountain threshold (≤0.25), suppression (0.58) exceeds the mountain ceiling (≤0.05), and beneficiary/victim declarations show asymmetric extraction. The engine's false summit detector flags this as naturalization of a contingent institutional arrangement, revealing the analytical observer's own vulnerability to the false summit framing.
constraint_indexing:constraint_classification(false_summit_institutional_legitimation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(false_summit_institutional_legitimation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(false_summit_institutional_legitimation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(false_summit_institutional_legitimation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(false_summit_institutional_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(false_summit_institutional_legitimation, TR),
    TR >= 0.70.

:- end_tests(false_summit_institutional_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The false summit extracts compliance through epistemic suppression — subordinate agents bear the cost of cognitive restriction (cannot challenge, cannot imagine alternatives, must perform legitimacy rituals). But the extraction is not total because genuine coordination benefits exist; the institution does produce shared standards and predictable behavior. The measured value reflects extraction layered onto coordination. Suppression (0.58): Moderate-high. The binding mechanism is epistemic rather than material: subordinate agents are suppressed through control of what appears natural/necessary, control of narrative frames, and social consequences for questioning. Material suppression (legal barriers, economic dependency) exists but is secondary to the epistemic mechanism. Theater ratio (0.81): Very high. The constraint's primary mechanism is performative: institutions must constantly perform legitimacy (compliance audits, policy documentation, expert justification) to maintain the false summit framing. As the theater increases over the interval, the institution becomes more dependent on performance-of-necessity to sustain belief in necessity. This creates a stability trap: the more performative the system becomes, the more agents suspect the performance is covering contingency, but the performance prevents agents from testing the suspicion.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the maximum perspectival distance in the corpus. Leadership sees a legitimate Rope (coordination without extraction), while subordinates see a Snare (pure extraction). The gap reveals the false summit mechanism: leadership genuinely experiences coordination because they control the frame that makes alternatives unthinkable. Subordinates experience extraction because they are forced to accept the leader-controlled frame while bearing the costs of constrained options. The Tangled Rope perspective (moderate agents with constrained exit) is the critical diagnostic: these agents see BOTH the genuine coordination function AND the false summit mechanism. They understand that the institution solves real coordination problems, but uses unnecessary authority and false naturalizing claims to do it. This dual vision is what enables reform thinking. The Scaffold perspective (external accountability) reveals the sunset structure: as transparency systems mature and comparability increases, the false summit cannot survive disclosure — alternative institutions with equivalent coordination but less false naturalizing become visible, undermining the false necessity claim. The Piton perspective shows institutional degradation: procedures persist through inertia long after the original function atrophied, maintained only by performance and the false summit frame. The mountain perspective is the most dangerous: it naturalizes the false summit itself as inevitable ('all institutions have some false legitimating myths,' 'coordination always requires some theater'). The analytical observer risks being captured by the very mechanism the constraint exemplifies.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply by agent power and exit options. Leadership (d ≈ 0.15): beneficiaries with arbitrage exit — they experience negative effective extraction because they control the false summit framing; the constraint subsidizes their stability. Subordinate agents (d ≈ 0.95): victims with trapped exit — they experience maximum extraction because they cannot refuse the false naturalizing frame and cannot exit; the constraint extracts their cognitive compliance. Peer reformers (d ≈ 0.60): moderate power with constrained exit — they can see the contingency but face high exit costs; their experienced extraction is moderate because they have some agency but limited escape. External coalition (d ≈ 0.50): organized power with constrained exit through accountability mechanisms — they experience moderate extraction because they can provide pressure but cannot directly control the institution. The perspectival gap is enormous: leadership sees coordination (Rope, low χ), while subordinates see extraction (Snare, high χ). This gap is itself a diagnostic signal: when the beneficiary and victim report drastically different classifications of the same constraint, the hidden mechanism is likely epistemic (false summit) rather than material (direct coercion).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY MECHANISM: The constraint's core mandatrophy is the distinction between genuine coordination function and false naturalizing mechanism. The false summit is a Tangled Rope that masquerades as Mountain in leadership and subordinate narratives. Leadership claims it's a Mountain ('these rules are necessary for any institution') to justify its own position. Subordinates may also adopt the Mountain framing because it's less cognitively taxing to accept necessity than to carry the dissonance of 'the institution coordinates but uses false claims to block my reform ideas.' Resolving the mandatrophy requires separating genuine coordination (the constraint is legitimately Rope/Tangled Rope) from false naturalizing (the constraint uses a false summit to suppress alternatives). The resolution is structural: identify which coordination functions genuinely require the current institutional design (cannot be done differently), and which functions persist in the current design but could be achieved through alternative structures. Everything in the second category is false summit framing. The analytical observer must resist the meta-level false summit: the claim that 'all institutions naturalize contingency' is itself a false summit that prevents institutional reform by claiming it's inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_boundary_ambiguity,
    'At what point does a socially constructed institutional rule become experienced as natural law by subordinate agents?',
    'Longitudinal ethnographic study of agents'' frames before and after organizational induction; comparison of rule explanations given by new vs long-tenured members; measurement of surprise/resistance when rule origin or contingency is revealed',
    'If naturalness boundary is crossed rapidly (<1 year): subordinate agents'' cognitive capture is highly efficient, enabling maximum extraction with minimal enforcement. If boundary crosses slowly (>5 years): the false summit framing is fragile and subject to cognitive drift — more agents maintain awareness of contingency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_boundary_ambiguity, empirical, 'Timeline for subordinate agents to experience institutional rules as natural law').

omega_variable(
    coordination_function_genuine_scope,
    'How much of the institutional procedure complexity is genuine coordination function versus performative legitimation theater?',
    'Functional decomposition: identify which procedures fail if removed (genuine function) vs which can be eliminated without loss of coordination (theater); measure coordination efficiency with vs without the procedure layer; track which procedures persist despite documented obsolescence',
    'If genuine function is high (>60%): the constraint is legitimately Rope/Tangled Rope with moderate theater. If genuine function is low (<30%): the constraint is primarily Piton with high theater — the false summit is the primary mechanism enabling extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_genuine_scope, empirical, 'Proportion of institutional procedures serving genuine coordination vs performative legitimation').

omega_variable(
    false_summit_visibility_paradox,
    'Can a false summit be ''revealed'' through analysis without immediately losing its binding force? Does analytical visibility of the false summit mechanism destroy the mechanism?',
    'Intervention study: introduce analytical visibility of false summit naturalizing mechanism to treatment institutional cohorts; measure changes in compliance, belief in naturalness, and rates of challenge to the false summit in treatment vs control groups; track institutional response to internal criticism pre- and post-visibility',
    'If visibility destroys mechanism: subordinate agents who see the analysis become less compliant and more reform-oriented, but institutional response typically involves suppression/exclusion of awareness (strengthening the extraction mechanism). If visibility is absorbed: the institution acknowledges contingency while maintaining the natural law framing (gaslighting), paradoxically strengthening the constraint by demonstrating inexpugnability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_visibility_paradox, conceptual, 'Whether revealing false summit mechanism destroys or strengthens the constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(false_summit_institutional_legitimation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(falsummit_tr_t0, false_summit_institutional_legitimation, theater_ratio, 0, 0.68).
narrative_ontology:measurement(falsummit_tr_t10, false_summit_institutional_legitimation, theater_ratio, 10, 0.75).
narrative_ontology:measurement(falsummit_tr_t20, false_summit_institutional_legitimation, theater_ratio, 20, 0.81).

% Extraction over time
narrative_ontology:measurement(falsummit_be_t0, false_summit_institutional_legitimation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(falsummit_be_t10, false_summit_institutional_legitimation, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(falsummit_be_t20, false_summit_institutional_legitimation, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(false_summit_institutional_legitimation, enforcement_mechanism).
narrative_ontology:affects_constraint(false_summit_institutional_legitimation, regulatory_capture_cognitive_lock).
narrative_ontology:affects_constraint(false_summit_institutional_legitimation, organizational_culture_inertia).
narrative_ontology:affects_constraint(false_summit_institutional_legitimation, institutional_identity_fusion).

% DUAL FORMULATION NOTE:
% False summit institutional legitimation is the epistemic variant of regulatory capture. Where regulatory capture operates through material benefit flows and career dependency, false summit operates through cognitive capture via naturalizing framing. They often co-occur: institutions use material extraction (regulatory capture) supported by epistemic extraction (false summit). The false summit is downstream because the naturalizing framing depends on prior institutional power to enforce the frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(false_summit_institutional_legitimation, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
