% ============================================================================
% CONSTRAINT STORY: complicity_equilibrium
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_complicity_equilibrium, []).

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
 *   constraint_id: complicity_equilibrium
 *   human_readable: Complicity Equilibrium: Mutual Extraction and Institutional Stability
 *   domain: institutional_sociology/political_economy
 *
 * SUMMARY:
 *   Complicity equilibrium occurs when institutional actors (leadership,
 *   middle management, professional cadres) form a mutual extraction pact:
 *   leadership distributes benefits to subordinates in exchange for enabling
 *   dysfunctional or harmful institutional behavior while maintaining a
 *   facade of legitimacy. Subordinates agree to silence, cooperation, or
 *   active participation in the dysfunction; in return they receive career
 *   advancement, resource access, and protection from accountability. The
 *   equilibrium is self-enforcing through a combination of economic incentive
 *   (subordinates are enriched by the arrangement), identity fusion
 *   (subordinates' professional identity becomes defined by participation),
 *   and selective enforcement (outsiders and reformers are excluded or
 *   punished). The constraint exhibits tangled rope characteristics because
 *   it solves a real coordination problem (how to maintain organizational
 *   stability without genuine accountability) while simultaneously producing
 *   asymmetric extraction (excluded agents bear costs, complicit agents
 *   receive benefits). The rising theater ratio (0.40 to 0.67 across the
 *   interval) reflects increasing resort to performative accountability
 *   mechanisms (token reforms, manufactured transparency, fake oversight
 *   bodies) as the underlying corruption accumulates and external pressure
 *   rises. This is diagnostic: as the real extraction grows, the theater must
 *   grow to obscure it.
 *
 * KEY AGENTS:
 *   - Institutional Leadership: Primary architect and beneficiary (institutional/arbitrage) — captures maximum value from the equilibrium by controlling resource distribution and accountability mechanisms
 *   - Complicit Subordinates: Secondary beneficiary and enforcer (moderate/constrained) — receive career/resource rewards in exchange for enabling dysfunction; identity-fused with organization; constrained by sunk costs
 *   - Excluded Outsiders: Primary victim (powerless/trapped) — structurally barred from the mutual benefit arrangement; bear costs through denial of opportunity, resources, and recognition
 *   - Institutional Integrity: Diffuse victim (powerless/trapped) — the organization's actual capacity to function legitimately; degraded by the equilibrium but has no organized advocate
 *   - External Oversight Bodies: Captured participant (organized/constrained) — regulatory agencies, media, civil society that are meant to police the arrangement but are incorporated into it through performative compliance and theater
 *   - Institutional Integrity Advocates: Reformers (powerful/mobile) — internal or external actors with sufficient power to disrupt the equilibrium but facing institutional retaliation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(complicity_equilibrium, 0.58).
domain_priors:suppression_score(complicity_equilibrium, 0.68).
domain_priors:theater_ratio(complicity_equilibrium, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(complicity_equilibrium, extractiveness, 0.58).
narrative_ontology:constraint_metric(complicity_equilibrium, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(complicity_equilibrium, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(complicity_equilibrium, tangled_rope).
narrative_ontology:human_readable(complicity_equilibrium, "Complicity Equilibrium: Mutual Extraction and Institutional Stability").
narrative_ontology:topic_domain(complicity_equilibrium, "institutional_sociology/political_economy").

domain_priors:requires_active_enforcement(complicity_equilibrium).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(complicity_equilibrium, institutional_leadership).
narrative_ontology:constraint_beneficiary(complicity_equilibrium, complicit_subordinates).
narrative_ontology:constraint_victim(complicity_equilibrium, excluded_outsiders).
narrative_ontology:constraint_victim(complicity_equilibrium, institutional_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED OUTSIDER (SNARE) — Structurally barred from the complicity equilibrium. Cannot enter the mutual benefit loop and bears extraction through denial of resources, opportunity, or recognition. No exit option within the institutional framework; exit requires abandoning institutional affiliation entirely.
constraint_indexing:constraint_classification(complicity_equilibrium, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPLICIT SUBORDINATE (TANGLED ROPE) — Participates in the mutual extraction equilibrium. Receives benefits (career advancement, resource access, protection from accountability) in exchange for enabling institutional dysfunction. Constrained exit due to career sunk costs and identity fusion with the organization. Genuine coordination function (maintaining organizational stability) coexists with asymmetric extraction (complicit agents benefit disproportionately).
constraint_indexing:constraint_classification(complicity_equilibrium, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL LEADERSHIP (ROPE) — Primary architect and beneficiary of the complicity equilibrium. Experiences the constraint as pure coordination: distributing benefits to subordinates buys their silence and cooperation, enabling the leadership to maintain power and pursue objectives unobstructed by accountability. Maximum arbitrage capacity — can exit by dissolving the bargain, but chooses not to because the arrangement is profitable.
constraint_indexing:constraint_classification(complicity_equilibrium, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EXTERNAL OVERSIGHT BODY (PITON) — Regulatory agencies, press, civil society watchdogs. The complicity equilibrium actively resists external oversight through performative accountability (token reforms, manufactured transparency). The oversight mechanism itself becomes degraded and inertial — performs the function of verification without actually verifying. Theater ratio high because compliance theater substitutes for genuine correction.
constraint_indexing:constraint_classification(complicity_equilibrium, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL INTEGRITY ADVOCATE (TANGLED ROPE) — External or internal reformer with sufficient power to disrupt the equilibrium but facing significant costs. Benefits from visibility and moral legitimacy; bears extraction through institutional retaliation, reputation attacks, and resource denial. Mobile exit options but high cost of exercise. Genuine coordination function (maintaining norms that would enable legitimate institutional functioning) coexists with extraction (reformers are punished for disrupting the profitable equilibrium).
constraint_indexing:constraint_classification(complicity_equilibrium, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the constraint as a structural solution to a coordination problem (how to maintain organizational stability when leadership cannot be held accountable) that necessarily produces asymmetric extraction (outsiders excluded, complicit agents enriched). The observer sees genuine coordination (the arrangement IS stable) and genuine asymmetry (it extracts from the powerless). Classification is tangled_rope because both dimensions are real.
constraint_indexing:constraint_classification(complicity_equilibrium, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(complicity_equilibrium_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(complicity_equilibrium, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(complicity_equilibrium, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(complicity_equilibrium, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(complicity_equilibrium, TR),
    TR >= 0.70.

:- end_tests(complicity_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over the interval. The equilibrium starts as a marginal arrangement (low-level coordination between leadership and select subordinates) but accumulates as the complicity deepens and more actors are drawn in or excluded. Initial extractiveness (0.35) represents the early-stage benefit-sharing phase where subordinates still receive significant value relative to their participation cost. By mid-interval (0.45-0.55), extractiveness rises as the leadership increasingly consolidates its position and extracts more value while distributing fewer rewards. Final state (0.58) represents mature complicity where the extraction is evident but still balanced by enough subordinate benefits to maintain the equilibrium. Suppression (0.68): High and structural. The equilibrium requires active suppression of alternatives (outsiders cannot enter the arrangement), suppression of dissent (reformers are punished), and suppression of truth (performative accountability prevents genuine investigation). Theater ratio (0.65): High and rising, indicating that as the real extraction increases, the institutional theater must increase proportionally to maintain legitimacy. Rising theater from 0.40 to 0.67 reflects increasing resort to pseudo-accountability (investigation committees that exonerate leadership, policy reforms that change nothing, diversity initiatives that exclude material change). The theater is a cost of the equilibrium that grows as the underlying corruption deepens.
 *
 * PERSPECTIVAL GAP:
 *   The excluded outsider sees pure extraction (Snare) — they are barred from participation and bear costs with no reciprocal benefit. The complicit subordinate sees coordination (Tangled Rope) — they genuinely benefit from the arrangement and their participation genuinely does solve the leadership's coordination problem (maintaining stability without accountability). The leadership sees pure coordination (Rope) — the arrangement is an elegant solution to the problem of maintaining power without accountability; from their perspective there is no extraction, only mutual benefit exchange. The external oversight body sees a problem-solving system (Piton) — the compliance theater creates an appearance of accountability and oversight, but the mechanism is largely performative and degraded. The integrity advocate sees both coordination and extraction (Tangled Rope) — they see that legitimate institutional functioning requires removing the complicity equilibrium, and they see the equilibrium as extracting from those who would benefit from actual accountability. The analytical observer sees tangled rope — both the coordination function (institutional stability) and the asymmetry (excluded agents bear the cost) are real structural features.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership has arbitrage exit options — they can dissolve the complicity arrangement if incentives change, or defect and exit to other organizations. Their directionality is low (beneficiaries), producing negative or near-zero effective extraction from their perspective. Complicit subordinates are constrained by career sunk costs and identity fusion — exit would require abandoning their professional identity and accepting significant financial loss. Their directionality is moderate (complicit beneficiaries), producing moderate effective extraction from their perspective weighted toward the benefits they receive. Excluded outsiders are trapped — they cannot enter the arrangement and cannot escape its effects without leaving the institutional domain entirely. Their directionality is high (victims), producing high effective extraction from their perspective. External oversight bodies are organized actors with significant power but are themselves constrained by institutional incorporation — they can theoretically exit or disrupt the equilibrium but face institutional and reputational costs. Their directionality is moderate-high (incorporated victims), producing moderate effective extraction from their perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by observing that complicity equilibrium is structurally tangled: it performs a genuine coordination function (maintaining organizational stability when legitimate accountability mechanisms fail or are unavailable) AND it necessarily produces asymmetric extraction (some agents are excluded and bear costs while complicit agents are enriched). The constraint cannot be classified as pure coordination (rope) because the asymmetry is not incidental but structural — the arrangement requires excluding outsiders and extracting from them to maintain the complicity bonus for insiders. The constraint cannot be classified as pure extraction (snare) because the coordination function is genuine — the arrangement does solve a real problem (how to maintain organizational functioning without accountability), and many complicit agents genuinely prefer the outcome to the alternative (organizational chaos or legitimate reform). The tangled rope classification shows that both dimensions are real and necessary. Any attempt to remove the extraction without solving the underlying coordination problem (what happens to organizational stability when accountability mechanisms are restored?) will fail. Any attempt to maintain the coordination without addressing the asymmetry will perpetuate injustice. This is the mandatrophy resolved: complicity equilibrium IS a mixed extraction-coordination system, and the mixing is not accidental but structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equilibrium_stability_mechanism,
    'What maintains the complicity equilibrium once formed? Is it fear of collapse, active enforcement of silence, or internalized acceptance of complicity as normal?',
    'Post-exit interviews with defectors; analysis of defection triggers and costs; comparison of stated rationales vs behavioral patterns',
    'If fear-based: equilibrium is brittle and vulnerable to coordinated exit. If enforcement-based: requires active surveillance/punishment systems. If internalized: equilibrium is self-sustaining and highly stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equilibrium_stability_mechanism, empirical, 'Mechanism maintaining complicity equilibrium stability').

omega_variable(
    complicity_vs_honest_coordination,
    'Can the same structural arrangement be sustained through legitimate coordination rather than mutual complicity? What is the empirical boundary between ''necessary organization of power'' and ''criminal conspiracy''?',
    'Comparative institutional analysis; identification of alternative equilibria in similar-scale organizations; counterfactual analysis of what coordination mechanisms could sustain without asymmetric extraction',
    'If boundary is clear: some complicity arrangements could be replaced with legitimate governance. If boundary is blurred: complicity is structurally necessary for organizations above some scale.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(complicity_vs_honest_coordination, conceptual, 'Boundary between legitimate coordination and extractive complicity').

omega_variable(
    outsider_threshold_effect,
    'Is there a critical mass threshold at which excluded outsiders can coordinate to disrupt the equilibrium? Do ''outsiders'' remain fundamentally unorganizable or can they form counter-coalitions?',
    'Social network analysis of excluded groups; historical cases of successful outsider coalition formation; identification of bottlenecks to outsider coordination',
    'If outsiders remain unorganizable: equilibrium is stable indefinitely. If coordinating threshold is achievable: equilibrium is vulnerable to external shock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(outsider_threshold_effect, empirical, 'Critical mass threshold for outsider coalition formation').

omega_variable(
    identity_lock_vs_rational_choice,
    'To what extent are complicit subordinates bound by identity fusion (their professional/organizational identity IS participation in the equilibrium) vs rational cost-benefit calculation (they stay because exit is expensive)?',
    'Psychological assessment of complicit agents; measurement of identity-fusion intensity; comparison with agents facing identical economic incentives but lower identity fusion',
    'If primarily identity-locked: agents perceive exit as identity death, making equilibrium very stable. If primarily rational: agents are exit-ready if costs change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_rational_choice, empirical, 'Identity lock vs rational incentive basis for complicity').

omega_variable(
    institutional_autophagy_lag,
    'How long can an institution maintain complicity equilibrium before internal degradation (loss of competent non-complicit agents, accumulation of institutional debt, skill atrophy) destabilizes the arrangement?',
    'Longitudinal institutional performance metrics; analysis of competency distribution as function of complicity tenure; identification of failure cascades in long-running complicity systems',
    'If lag is long (>20 years): equilibrium can persist across generational timescales. If lag is short (<5 years): equilibrium is self-limiting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_autophagy_lag, empirical, 'Timeline for institutional autophagy destabilization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(complicity_equilibrium, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(compl_tr_t0, complicity_equilibrium, theater_ratio, 0, 0.4).
narrative_ontology:measurement(compl_tr_t3, complicity_equilibrium, theater_ratio, 3, 0.55).
narrative_ontology:measurement(compl_tr_t6, complicity_equilibrium, theater_ratio, 6, 0.65).
narrative_ontology:measurement(compl_tr_t9, complicity_equilibrium, theater_ratio, 9, 0.67).

% Extraction over time
narrative_ontology:measurement(compl_be_t0, complicity_equilibrium, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(compl_be_t3, complicity_equilibrium, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(compl_be_t6, complicity_equilibrium, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(compl_be_t9, complicity_equilibrium, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(complicity_equilibrium, enforcement_mechanism).
narrative_ontology:affects_constraint(complicity_equilibrium, institutional_corruption_persistence).
narrative_ontology:affects_constraint(complicity_equilibrium, accountability_theater).
narrative_ontology:affects_constraint(complicity_equilibrium, career_incentive_misalignment).

% DUAL FORMULATION NOTE:
% Complicity equilibrium is the macroscale pattern that emerges from individual-level corruption (misuse of institutional power for personal benefit), accountability theater (performative compliance to suppress genuine oversight), and career misalignment (incentives that reward complicity over integrity). This story models the system-level constraint; the upstream stories model the individual and organizational-level mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(complicity_equilibrium, institutional, 0.08).
constraint_indexing:directionality_override(complicity_equilibrium, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
