% ============================================================================
% CONSTRAINT STORY: finite_pool_of_worry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_finite_pool_of_worry, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: finite_pool_of_worry
 *   human_readable: The Finite Pool of Worry Hypothesis
 *   domain: psychological/social
 *
 * SUMMARY:
 *   The finite pool of worry hypothesis describes a structural constraint
 *   where individuals and societies can only sustain attention on a limited
 *   number of simultaneous crises or concerns. This constraint operates at
 *   multiple levels: individual cognition (emotional capacity to process
 *   multiple threats), collective attention (media bandwidth and audience
 *   focus), and institutional coordination (crisis-response infrastructure
 *   can only manage a bounded set of simultaneous events). The constraint
 *   creates a winner-take-all dynamic where dominant issues consume available
 *   worry resources, leaving secondary concerns structurally invisible
 *   regardless of their actual severity or impact. This manifests as pure
 *   extraction from the perspective of ignored crises and overwhelmed
 *   individuals, as coordination opportunity from the perspective of media
 *   gatekeepers, as mixed extraction-coordination from activist coalitions,
 *   and as a degraded theater from institutional crisis-management systems.
 *   The constraint's extractiveness has increased over the measurement
 *   interval (0.18 → 0.38) as the complexity and frequency of simultaneous
 *   crises has outpaced institutional and psychological capacity to
 *   distribute attention fairly. Theater ratio has similarly increased,
 *   indicating that crisis-response mechanisms increasingly consist of
 *   performative activity (advisories, preparedness statements, coordinated
 *   messaging) that substitutes for actual capacity to handle multiple
 *   simultaneous emergencies.
 *
 * KEY AGENTS:
 *   - Ignored Crisis: Primary victim (powerless/trapped) — structural crises below the attention threshold remain invisible and unaddressed
 *   - Overwhelmed Individual: Primary victim (powerless/constrained) — forced to ratify attention allocation under resource scarcity; emotional and cognitive capacity exhausted
 *   - Activist Coalition: Secondary actor (moderate/constrained) — experience coordination benefits from shared focus but extraction from forced prioritization
 *   - Media Network: Primary beneficiary (institutional/arbitrage) — captures agenda-setting power and audience resources through gatekeeping function
 *   - Alternative Information Architecture: Organized actor (organized/constrained) — building distributed curation mechanisms as sunset pathway for finite pool constraint
 *   - Institutional Crisis Management: Theatrical performer (institutional/arbitrage) — maintains crisis-response apparatus that is increasingly performative rather than functional
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing culturally contingent attention allocation as biological law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(finite_pool_of_worry, 0.38).
domain_priors:suppression_score(finite_pool_of_worry, 0.42).
domain_priors:theater_ratio(finite_pool_of_worry, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(finite_pool_of_worry, extractiveness, 0.38).
narrative_ontology:constraint_metric(finite_pool_of_worry, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(finite_pool_of_worry, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(finite_pool_of_worry, tangled_rope).
narrative_ontology:human_readable(finite_pool_of_worry, "The Finite Pool of Worry Hypothesis").
narrative_ontology:topic_domain(finite_pool_of_worry, "psychological/social").

domain_priors:requires_active_enforcement(finite_pool_of_worry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(finite_pool_of_worry, dominant_issue_framers).
narrative_ontology:constraint_beneficiary(finite_pool_of_worry, media_gatekeepers).
narrative_ontology:constraint_victim(finite_pool_of_worry, marginalized_secondary_concerns).
narrative_ontology:constraint_victim(finite_pool_of_worry, public_attentional_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IGNORED CRISIS (SNARE) — Structural crises that fall outside the finite pool of public worry have no mechanism for engagement. They remain structurally invisible regardless of severity. Powerless agents bearing costs from secondary issues (climate tipping points acknowledged but deprioritized, endemic disease burdens, slow institutional collapse) cannot escape or reframe attention. Maximum extraction: the constraint prevents even awareness of victimization.
constraint_indexing:constraint_classification(finite_pool_of_worry, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OVERWHELMED INDIVIDUAL (SNARE) — When forced to choose among multiple simultaneous crises, individuals experience the finite pool as coercive rationing of emotional resources. The constraint suppresses agency: once primary worry allocations are locked in (by trauma, illness, financial stress, family emergency), secondary concerns receive zero attention regardless of their importance. Suppression is high — coping mechanisms are exhausted and unavailable for alternative concerns.
constraint_indexing:constraint_classification(finite_pool_of_worry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ACTIVIST COALITION (TANGLED ROPE) — Moderate-power agents (local activists, NGOs, community organizers) experience the finite pool as mixed coordination and extraction. The constraint enables coalition-building around shared concerns (coordination benefit) but forces brutal prioritization where only dominant issues receive sustained attention (extraction cost). Limited exit options — resources are insufficient to address all recognized problems simultaneously.
constraint_indexing:constraint_classification(finite_pool_of_worry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: MEDIA NETWORK (ROPE) — News organizations experience the finite pool as a coordination mechanism: editorial gatekeeping solves the collective action problem of where to direct public attention. Media institutions benefit from the constraint through the ability to set agenda and capture audience resources. The relationship appears as pure coordination from this perspective — they are solving a genuine information scarcity problem. Exit is high (arbitrage): media can shift between stories based on market/political incentives.
constraint_indexing:constraint_classification(finite_pool_of_worry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE INFORMATION ARCHITECTURE (SCAFFOLD) — Decentralized information networks (social media subgroups, specialized forums, citizen journalism platforms) are building parallel attention allocation mechanisms that bypass the finite pool assumption. These organized actors see the bottleneck as temporary and solvable through distributed curation. High agency but constrained exit — their platforms still compete within the same attention economy.
constraint_indexing:constraint_classification(finite_pool_of_worry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ATTENTION-ECONOMY THEATER (PITON) — The formal mechanisms of crisis management (emergency response protocols, disaster coordination, crisis communication) are substantially performative. The theater persists: agencies conduct drills, issue advisories, and maintain crisis infrastructure. But the underlying function (actually distributing attention fairly across simultaneous crises) has atrophied. Theater ratio high because the appearance of crisis readiness often substitutes for actual capacity to handle multiple simultaneous events.
constraint_indexing:constraint_classification(finite_pool_of_worry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: COGNITIVE UNIVERSALIST (MOUNTAIN) — From a universalist/civilizational frame, cognitive attention is a finite biological resource. No society, no technology, no institutional design can overcome the basic constraint that human minds have limited capacity for simultaneous concern. This perspective risks naturalizing what may be a culturally contingent allocation pattern as a biological law. The engine's false summit detector will evaluate whether extractiveness values support this universalization.
constraint_indexing:constraint_classification(finite_pool_of_worry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(finite_pool_of_worry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(finite_pool_of_worry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(finite_pool_of_worry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(finite_pool_of_worry, TR),
    TR >= 0.70.

:- end_tests(finite_pool_of_worry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts value primarily by enabling agenda-setting power for dominant issue framers, but this extraction is not maximal (0.46+) because the underlying coordination problem is real — societies do face genuine attention scarcity. The measurement trajectory (0.18 → 0.38) reflects increasing institutional amplification of a naturally-occurring constraint: media consolidation, algorithmic curation, political polarization, and attention-capture industries have layered additional extraction mechanisms onto the basic cognitive limit. Suppression (0.42): Moderate-high. Individuals facing multiple simultaneous crises have constrained but not zero options — emergency prioritization, sequential processing, delegation to institutions. The suppression is structural rather than coercive: it emerges from resource limits rather than explicit prohibition. Theater ratio (0.58): Moderate-high. Crisis-response systems maintain substantial performative activity (emergency declarations, coordinated communication, preparedness drills) that creates the appearance of capacity to handle multiple simultaneous events. As actual capacity has not kept pace with crisis frequency, theater has substituted for function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The powerless agent sees pure extraction (Snare) — the constraint prevents even awareness of their victimization. The activist sees mixed coordination and extraction (Tangled Rope) — the constraint enables coalition-building but forces brutal prioritization. The media institution sees pure coordination (Rope) — they are solving a genuine information scarcity problem and benefit from that solution. The organized alternative network sees a temporary problem with an exit path (Scaffold) — distributed information systems can bypass the finite pool through parallel curation. The crisis-management apparatus sees its own degraded function (Piton) — the theater persists through institutional inertia, not effectiveness. The analytical observer risks seeing a biological law (Mountain) — finite attention is a universal constraint. These divergences are not measurement artifacts; they reflect genuine differences in structural position and exit capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (dominant issue framers, media gatekeepers) occupy institutional power positions with arbitrage exit options — they can shift between stories based on audience demand and political opportunity. Their directionality is low (d ≈ 0.20): they are positioned as net beneficiaries, experiencing the constraint as enabling rather than extractive. Victims (ignored crises, overwhelmed individuals) are powerless agents trapped within the constraint — they cannot exit or reframe attention allocation. Their directionality is high (d ≈ 0.85-0.95): the constraint extracts from them through attentional deprivation. The activist coalition occupies a middle position with constrained exit: they benefit from coordination around shared concerns but are victimized by forced prioritization. Their directionality is moderate (d ≈ 0.55). The alternative information architecture (organized actors building decentralized curation) has agency but still operates within the same attention economy — constrained exit limits their ability to transcend the underlying pool structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The finite pool constraint resolves mandatrophy by revealing that the classification shifts with institutional architecture. If the pool is purely biological (fixed cognitive capacity), the constraint approaches Mountain from all perspectives — it is an immutable limit. If the pool is amplified by media consolidation, algorithmic curation, and institutional gatekeeping, it is Tangled Rope from the analytical observer's perspective: it provides genuine coordination (attention allocation is a real problem) but enables extraction (gatekeepers capture agenda-setting power and audience resources). The empirical tests are: (1) Does crisis severity override the pool constraint, suggesting it has functional coordination properties? (2) Can decentralized information networks distribute attention across more simultaneous concerns, suggesting the constraint is institutional rather than biological? (3) What happens to the pool size when media consolidation decreases or institutional coordination capacity increases? If the pool expands, it is not Mountain. The measurement trajectory (theater increasing faster than extractiveness) suggests institutional amplification: the constraint is becoming more performative and less functionally coordinated over time, pointing toward Snare classification as the underlying institutional structures capture larger rents from attention allocation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pool_size_determinant,
    'What determines the size of the ''worry pool'' — is it a fixed biological constant, or does it scale with attentional infrastructure, institutional coordination capacity, and cultural practice?',
    'Cross-cultural comparison of simultaneous crisis engagement; historical analysis of attention-handling capacity during periods of institutional variation; neuroscience studies on attention load under different information architectures',
    'If biologically fixed: mountain classification holds across all perspectives. If culturally variable: classification shifts to tangled_rope/scaffold — the constraint is institutional, not natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pool_size_determinant, empirical, 'Whether the finite pool is biological constant or institutional variable').

omega_variable(
    crisis_severity_independence,
    'Does the finite pool operate independently of actual crisis severity, or does severity override the pool constraint (larger crises capture disproportionate attention)?',
    'Temporal analysis of public concern allocation relative to measured crisis impact metrics; comparison of attention patterns for equivalent-severity events with different media salience',
    'If independent: extraction mechanism is pure (snare). If severity-responsive: the constraint has partially functional coordination properties (tangled_rope more accurate).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(crisis_severity_independence, empirical, 'Whether pool operates independently of crisis severity').

omega_variable(
    alternative_information_effectiveness,
    'Can decentralized information networks (social media, forums, specialized platforms) actually distribute attention across more simultaneous concerns than centralized media, or do they simply fragment the pool into smaller pools with identical structure?',
    'Measurement of diversity in simultaneously-engaged concerns on centralized vs decentralized platforms; tracking of attention allocation before/after network shift; network analysis of cross-concern topic bridges',
    'If effective: scaffold perspective is structural, sunset is real. If fragmenting: the constraint persists in new form, and scaffold is aspirational rather than viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_information_effectiveness, empirical, 'Whether alternative networks overcome the finite pool constraint').

omega_variable(
    institutional_amplification,
    'To what extent is the finite pool constraint naturally occurring vs deliberately amplified by media gatekeeping, political polarization mechanics, and attention-capture industries?',
    'Historical comparison of attention allocation patterns across media epochs (pre-digital, early internet, algorithmic era); analysis of gating decisions and their impact on concern visibility; measurement of extractive markup imposed by institutional actors',
    'If amplified: extractiveness significantly overstates biological constraint; snare classification shifts toward tangled_rope. If natural: extractiveness reflects genuine cognitive limits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_amplification, conceptual, 'Degree to which the pool is naturally occurring vs institutionally amplified').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(finite_pool_of_worry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fpow_tr_t0, finite_pool_of_worry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fpow_tr_t5, finite_pool_of_worry, theater_ratio, 5, 0.48).
narrative_ontology:measurement(fpow_tr_t10, finite_pool_of_worry, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(fpow_be_t0, finite_pool_of_worry, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(fpow_be_t5, finite_pool_of_worry, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(fpow_be_t10, finite_pool_of_worry, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(finite_pool_of_worry, information_standard).
narrative_ontology:affects_constraint(finite_pool_of_worry, attention_economy_extraction).
narrative_ontology:affects_constraint(finite_pool_of_worry, media_gatekeeping_power).
narrative_ontology:affects_constraint(finite_pool_of_worry, crisis_response_saturation).

% DUAL FORMULATION NOTE:
% The finite pool of worry can be decomposed into: (1) a biological/cognitive constraint (attention capacity limits), ε ≈ 0.08, Mountain; and (2) an institutional amplification layer (media consolidation, algorithmic curation, political polarization), ε ≈ 0.38, Tangled Rope. The present story models the combined system. Upstream cognitive limit is invariant; downstream amplification mechanisms are contingent and potentially reversible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
