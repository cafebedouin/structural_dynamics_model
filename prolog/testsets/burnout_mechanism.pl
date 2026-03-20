% ============================================================================
% CONSTRAINT STORY: burnout_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_burnout_mechanism, []).

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
 *   constraint_id: burnout_mechanism
 *   human_readable: Multi-Year Book Project Burnout Mechanism
 *   domain: writing_psychology/creative_labor_economics/professional_identity
 *
 * SUMMARY:
 *   The multi-year book project commitment creates a burnout mechanism
 *   through the interaction of sunk cost psychology, scope creep, identity
 *   fusion, and loss of creative variety. Writers enter contracts with 12-24
 *   month timelines that routinely extend to 3-5 years through editorial
 *   revisions, market repositioning, and perfectionist mission creep.
 *   Non-refundable advances create a ratchet: each month invested raises the
 *   psychological cost of abandoning the project. The writer's professional
 *   identity becomes fused with the book — exit would mean not just financial
 *   loss but identity dissolution. Meanwhile, creative variety collapses: the
 *   writer spends years in a single conceptual space, unable to pursue new
 *   ideas without abandoning sunk investment. Post-publication data shows 0-4
 *   year silence periods, permanent output reduction (writers who previously
 *   published every 2-3 years now publishing every 5-7 years or stopping
 *   entirely), and complete cessation. The constraint exhibits all six DR
 *   types: the committed writer experiences a snare (identity-locked, maximum
 *   extraction), the publisher experiences coordination (rope), mid-career
 *   writers with options experience hybrid coordination-extraction (tangled
 *   rope), organized writers are building alternative models with sunset
 *   logic (scaffold), and the analytical observer risks naturalizing creative
 *   labor's inherent difficulty (false mountain) or recognizes the genuine
 *   hybrid structure (analytical tangled rope).
 *
 * KEY AGENTS:
 *   - Committed Writer: Primary victim (powerless/identity_locked) — professional identity fused with project; exit psychologically impossible; bears full cost of creative exhaustion and permanent productivity damage
 *   - Writer Long-Term Productivity: Abstract victim (powerless/trapped) — the writer's future creative capacity, which has no advocate and cannot exit; damaged by present extraction
 *   - Publishers/Agents: Primary beneficiary (institutional/arbitrage) — capture dedicated labor through contract lock-in; portfolio diversification protects against individual burnout; can exit to other writers
 *   - Mid-Career Writer with Options: Secondary victim (moderate/constrained) — has some exit capacity but faces high costs; experiences both coordination and extraction
 *   - Writers' Guild: Organized agents (organized/mobile) — building alternative contract structures (milestone advances, scope limits, right-to-pause clauses) with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent contract structures as inherent properties of creative labor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(burnout_mechanism, 0.58).
domain_priors:suppression_score(burnout_mechanism, 0.68).
domain_priors:theater_ratio(burnout_mechanism, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(burnout_mechanism, extractiveness, 0.58).
narrative_ontology:constraint_metric(burnout_mechanism, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(burnout_mechanism, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(burnout_mechanism, snare).
narrative_ontology:human_readable(burnout_mechanism, "Multi-Year Book Project Burnout Mechanism").
narrative_ontology:topic_domain(burnout_mechanism, "writing_psychology/creative_labor_economics/professional_identity").

domain_priors:requires_active_enforcement(burnout_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(burnout_mechanism, publishers_agents).
narrative_ontology:constraint_victim(burnout_mechanism, writer_long_term_productivity).
narrative_ontology:constraint_victim(burnout_mechanism, writer_creative_health).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE COMMITTED WRITER (SNARE) — Identity-locked by professional self-concept and sunk cost accumulation. Exit would require abandoning not just the project but the writer identity itself. The multi-year commitment creates a ratchet: each month invested raises the psychological cost of walking away. Scope creep and editorial demands compound the trap. Experiences maximum extraction through creative exhaustion, loss of variety, and permanent productivity damage.
constraint_indexing:constraint_classification(burnout_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PUBLISHER/AGENT (ROPE) — Benefits from writer lock-in through contract structure. Experiences the multi-year commitment as coordination: securing dedicated labor for a marketable product. Can exit to other writers if one burns out. Portfolio diversification protects against individual writer failure. The constraint coordinates their business model — long development cycles justify advance structures and option clauses.
constraint_indexing:constraint_classification(burnout_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE MID-CAREER WRITER WITH OPTIONS (TANGLED ROPE) — Has some exit capacity through established reputation and alternative income streams, but still faces significant extraction. Can renegotiate scope or walk away at high cost (reputation damage, advance repayment, relationship loss). Experiences both coordination (the structure enables ambitious projects) and extraction (scope creep, creative exhaustion, opportunity cost). The constraint is genuinely hybrid: the multi-year commitment solves real coordination problems while embedding asymmetric costs.
constraint_indexing:constraint_classification(burnout_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE WRITERS' GUILD (SCAFFOLD) — Organized writers building alternative contract structures with sunset logic: milestone-based advances, scope limitation clauses, creative health provisions, right-to-pause clauses. Serial rights and modular project structures reduce lock-in. As these norms diffuse, the burnout mechanism's extraction force weakens. Estimated sunset: 15-25 years for alternative contract norms to become standard in trade publishing.
constraint_indexing:constraint_classification(burnout_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, creative work inherently requires sustained focus, and ambitious projects inherently take years. The burnout risk is an immutable property of deep creative labor. However, this perspective naturalizes what is actually a contingent institutional arrangement: the specific contract structures, advance models, and scope expectations that create the trap. The engine's false summit detector will identify this as naturalization of extractive institutional design.
constraint_indexing:constraint_classification(burnout_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / STRUCTURAL VIEW (TANGLED ROPE) — Recognizes the constraint as genuinely hybrid. Multi-year commitments solve real coordination problems: publishers need predictable product pipelines, writers need advance income to sustain work. But the current implementation embeds asymmetric extraction through non-refundable advances creating sunk cost traps, option clauses extending lock-in, scope creep without compensation adjustment, and lack of creative health provisions. The coordination function is real; the extraction is also real.
constraint_indexing:constraint_classification(burnout_mechanism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(burnout_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(burnout_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(burnout_mechanism, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(burnout_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(burnout_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The multi-year commitment extracts through creative exhaustion, opportunity cost (inability to pursue other projects), permanent productivity damage (0-4 year silence periods, reduced lifetime output), and identity dissolution risk. The extraction is not maximal because some writers do recover and because the coordination function (enabling ambitious projects) is real. Suppression (0.68): High. Barriers to exit include non-refundable advances (financial trap), professional identity fusion (psychological trap), reputation risk (walking away signals unreliability), relationship loss (burning bridges with publisher/agent), and sunk cost accumulation (each month invested raises exit cost). But suppression is not total — some writers do exit, renegotiate, or pivot. Theater ratio (0.45): Moderate. Some performative elements exist (writers performing productivity for publishers, publishers performing editorial rigor) but much of the constraint's function is real: the multi-year timeline genuinely enables complex projects, and the burnout is genuine harm, not theater. Theater has increased over the interval as market pressures have added performative productivity signaling.
 *
 * PERSPECTIVAL GAP:
 *   The publisher sees coordination (Rope) — the multi-year commitment solves the legitimate problem of securing dedicated labor for complex projects. The committed writer sees a snare (Snare) — identity-locked in a trap that is damaging their long-term creative capacity. The mid-career writer sees hybrid coordination-extraction (Tangled Rope) — the structure both enables and harms. The organized guild sees a temporary problem with a sunset (Scaffold) — alternative contract models are emerging. The civilizational analytical observer risks seeing an immutable law (Mountain) — deep creative work inherently requires sustained focus and carries burnout risk — but this naturalizes what is actually a contingent contract design. The biographical analytical observer sees the genuine hybrid (Tangled Rope) — real coordination function, real extraction, both structural.
 *
 * DIRECTIONALITY LOGIC:
 *   The committed writer is identity-locked: their professional self-concept is constituted through the writer identity, and abandoning the project would require abandoning that identity. This is cognitive entrapment layered on top of structural constraints (advance repayment, reputation damage). The identity lock makes exit literally unthinkable from within the frame, even when structural exit is possible at high cost. The publisher/agent is a beneficiary with arbitrage exit: they benefit from writer lock-in and can exit to other writers if one burns out. The mid-career writer with options is a victim with constrained exit: they face high costs (reputation, relationships, finances) but exit is possible. The organized writers' guild has mobile exit: collective bargaining creates alternative pathways. The analytical observer at civilizational time risks seeing the constraint as a mountain (creative labor is inherently difficult) but the structural data reveals this as naturalization of contingent institutional arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the multi-year commitment genuinely coordinates (enables ambitious projects that require sustained focus) AND genuinely extracts (creates identity traps, sunk cost ratchets, and permanent productivity damage). The coordination is not a cover story — publishers do need predictable pipelines, writers do need advance income, complex books do require years of work. But the current implementation embeds asymmetric extraction: non-refundable advances create financial traps, option clauses extend lock-in beyond the initial project, scope creep occurs without compensation adjustment, and no creative health provisions exist. The tangled rope classification captures this: it is neither pure coordination (rope) nor pure extraction (snare) but genuinely both. The perspectival gap is diagnostic: beneficiaries see coordination, victims see extraction, and both are structurally correct from their positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recovery_trajectory_variance,
    'What proportion of post-book silence periods represent genuine creative recovery vs permanent productivity damage vs strategic repositioning?',
    'Longitudinal tracking of writer output patterns; interviews distinguishing voluntary pause from inability to work; correlation between silence duration and subsequent productivity',
    'If most silence is recovery: lower extractiveness, constraint is harsh but reversible. If most is permanent damage: higher extractiveness, constraint causes lasting harm. If most is strategic: writers have more agency than victim framing suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recovery_trajectory_variance, empirical, 'Whether post-book silence represents recovery, damage, or strategy').

omega_variable(
    scope_creep_mechanism,
    'Is scope creep primarily publisher-driven (editorial demands, market repositioning) or writer-driven (perfectionism, mission creep)?',
    'Analysis of contract revisions and editorial correspondence; writer testimony on source of scope changes; comparison of initial proposals to final manuscripts',
    'If publisher-driven: extraction is institutional and addressable through contract reform. If writer-driven: extraction is partly self-imposed, suggesting identity-lock mechanism. If mixed: tangled rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_creep_mechanism, empirical, 'Primary driver of scope creep in multi-year projects').

omega_variable(
    alternative_model_viability,
    'Do modular/serialized publishing models (Substack, Patreon, chapter-by-chapter release) actually reduce burnout or merely shift the extraction mechanism?',
    'Comparison of burnout rates and recovery patterns between traditional book contracts and alternative models; income stability analysis; creative satisfaction surveys',
    'If genuinely lower burnout: scaffold perspective confirmed, alternative models provide real exit. If burnout persists: the multi-year commitment is not the core mechanism, identity-lock or creative labor structure itself may be extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_model_viability, empirical, 'Whether alternative publishing models reduce burnout').

omega_variable(
    identity_fusion_threshold,
    'At what point does professional writer identity become constitutive rather than instrumental, making exit from a failing project psychologically impossible?',
    'Psychological assessment of identity fusion in writers at different career stages; correlation between identity fusion measures and project abandonment rates; analysis of writers who successfully exited vs those who persisted to burnout',
    'If fusion occurs early (pre-contract): identity-lock is primary mechanism, exit options are illusory from the start. If fusion occurs mid-project: sunk cost and scope creep create the lock. If fusion is rare: constrained exit is more accurate than identity-locked.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_threshold, conceptual, 'When writer identity becomes constitutive vs instrumental').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(burnout_mechanism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(burnout_tr_t0, burnout_mechanism, theater_ratio, 0, 0.3).
narrative_ontology:measurement(burnout_tr_t3, burnout_mechanism, theater_ratio, 3, 0.38).
narrative_ontology:measurement(burnout_tr_t6, burnout_mechanism, theater_ratio, 6, 0.45).
narrative_ontology:measurement(burnout_tr_t9, burnout_mechanism, theater_ratio, 9, 0.48).

% Extraction over time
narrative_ontology:measurement(burnout_be_t0, burnout_mechanism, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(burnout_be_t3, burnout_mechanism, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(burnout_be_t6, burnout_mechanism, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(burnout_be_t9, burnout_mechanism, base_extractiveness, 9, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(burnout_mechanism, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of opportunity_cost_asymmetry (the structural imbalance between publisher portfolio diversification and writer project concentration). The upstream constraint establishes the asymmetric risk distribution; this constraint describes the burnout mechanism that asymmetry enables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
