% ============================================================================
% CONSTRAINT STORY: agentive_optimism_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_agentive_optimism_2026, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: agentive_optimism_2026
 *   human_readable: The Agentive Optimism Gap
 *   domain: political/social
 *
 * SUMMARY:
 *   The agentive optimism gap emerges from a structural divergence in how
 *   different population segments experience agency and possibility. Elite
 *   policy-makers — characterized by rare combinations of institutional
 *   access, social capital, educational credentials, and insulation from
 *   economic precarity — develop a sense of personal agency and
 *   future-orientation that enables them to formulate coherent long-term
 *   policies and maintain institutional legitimacy. A significant segment of
 *   the public, by contrast, experiences economic precarity, institutional
 *   distance, accumulated policy failures, and learned helplessness that
 *   produces 'overpowering pessimism.' The constraint operates when elites
 *   maintain optimistic policy frames and communication despite widespread
 *   public pessimism, creating a divergence in epistemic contexts: the same
 *   policy (e.g., 'the economy is improving,' 'technology will solve climate
 *   change,' 'institutions are functioning') is experienced by elites as
 *   genuine opportunity and by pessimists as gaslighting. This gap functions
 *   simultaneously as a coordination mechanism (shared elite optimism enables
 *   coherent governance) and as an extraction mechanism (public pessimism is
 *   suppressed, delegitimized, and converted into compliance through
 *   hopelessness). The constraint has intensified over the past 15+ years as
 *   material conditions have diverged (asset appreciation for property-owning
 *   classes, wage stagnation and precarity for service and manufacturing
 *   workers) while elite optimism rhetoric has remained constant, increasing
 *   the gap. Theater ratio has risen as civic participation mechanisms
 *   (voting, town halls, public comment) have become increasingly
 *   performative — valued as legitimacy rituals rather than as mechanisms
 *   that determine outcomes.
 *
 * KEY AGENTS:
 *   - Elite Policy-Making Class: Primary beneficiary (institutional/arbitrage) — derives agency, legitimacy, and coherence from optimistic framing; insulated from consequences of pessimistic outcomes
 *   - Pessimistic Public Segment: Primary victim (powerless/trapped) — experiences constraint as suppression of legitimate grievance; learned helplessness reduces exit capacity
 *   - Struggling Working Class: Secondary victim/moderate agent (moderate/constrained) — partially mobile but constrained by dependency on geographic/institutional networks; perceives mixed coordination benefit (solidarity) and extraction
 *   - Labor and Community Organizations: Organized intermediary (organized/arbitrage) — mobilize public pessimism as organizing tool; maintain mobile positioning between elite and base
 *   - Regenerative Futures Coalition: Emerging organized alternative (organized/mobile) — build parallel institutions with explicit sunset logic; offer exit pathway outside traditional politics
 *   - Democratic Participation Rituals: Institutional performance (institutional/arbitrage) — perpetuate legitimacy through voting, town halls, public comment despite low actual influence; piton classification from high theater ratio
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees tension between genuine coordination (elite coherence) and genuine extraction (public delegitimization)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(agentive_optimism_2026, 0.58).
domain_priors:suppression_score(agentive_optimism_2026, 0.68).
domain_priors:theater_ratio(agentive_optimism_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(agentive_optimism_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(agentive_optimism_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(agentive_optimism_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(agentive_optimism_2026, tangled_rope).
narrative_ontology:human_readable(agentive_optimism_2026, "The Agentive Optimism Gap").
narrative_ontology:topic_domain(agentive_optimism_2026, "political/social").

domain_priors:requires_active_enforcement(agentive_optimism_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(agentive_optimism_2026, elite_policy_makers).
narrative_ontology:constraint_beneficiary(agentive_optimism_2026, institutional_gatekeepers).
narrative_ontology:constraint_victim(agentive_optimism_2026, pessimistic_public_segment).
narrative_ontology:constraint_victim(agentive_optimism_2026, political_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PESSIMISTIC PUBLIC SEGMENT (SNARE) — Structurally trapped by learned helplessness, economic precarity, and institutional distance from decision-making. The perception of powerlessness creates behavioral inertia even when formal exit options exist. Policy communication from elite optimists is experienced as gaslighting: 'things are better' contradicts lived experience, creating distrust. No genuine exit from the constraint except collective organizing (high barrier). Maximum experienced extraction: the public segment bears the cost of policy failures while excluded from the optimism that justifies those policies.
constraint_indexing:constraint_classification(agentive_optimism_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STRUGGLING WORKING CLASS (TANGLED ROPE) — Partially mobile through geographic relocation or occupational retraining, but constrained by social networks, family obligations, and regional economic dependency. The constraint provides coordination benefit (shared identity, mutual aid networks) alongside extraction (wage suppression, labor market segmentation). This group can partially exit through individual mobility but faces collective action problems. Some agency but also significant structural extraction.
constraint_indexing:constraint_classification(agentive_optimism_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LABOR AND COMMUNITY ORGANIZATIONS (ROPE) — Can exit by shifting narrative frame or coalition, possess organizational resources and media access. The constraint functions for them as coordination mechanism: shared grievance narratives mobilize membership and secure funding. They experience the optimism gap as a tool for their own positioning. Genuine coordination without maximal extraction — they have agency in the relationship.
constraint_indexing:constraint_classification(agentive_optimism_2026, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ELITE POLICY-MAKING CLASS (ROPE) — Derives substantial benefit from the agentive optimism gap. Personal sense of agency enables policy coherence and morale. Optimistic framing justifies institutional continuity and their own decision-making authority. Can arbitrage between private pessimism and public optimism. The constraint functions as a coordination mechanism for the elite: shared optimism maintains institutional legitimacy and enables long-term planning. They experience the gap as necessary theater that enables effective governance.
constraint_indexing:constraint_classification(agentive_optimism_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGENERATIVE FUTURES COALITION (SCAFFOLD) — Emerging networks of younger organizers, technologists, and policy entrepreneurs who reject both elite optimism and learned helplessness. Build parallel institutions (cooperative economics, local resilience networks, climate adaptation platforms) with explicitly provisional sunset logic: 'these alternatives exist until mainstream policy catches up.' They see the gap as a coordination failure with a 10-20 year sunset as pessimism and optimism converge in shared material reality. High agency, low suppression, declining extraction as alternative institutions mature.
constraint_indexing:constraint_classification(agentive_optimism_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: DEMOCRATIC RITUAL AND CIVIC PARTICIPATION THEATER (PITON) — Voting, town halls, public comment periods, and 'have your voice heard' campaigns are largely performative when structural policy decisions occur behind closed doors. The participation rituals persist through institutional inertia and democratic ideology, not because they determine outcomes. Theater ratio high (0.64) — the rituals consume time and energy but produce minimal policy influence for participants. Classified as piton because the functional mechanism (genuine input into decision-making) has atrophied while the performative shell remains, maintained by both elites (legitimacy) and public (hope).
constraint_indexing:constraint_classification(agentive_optimism_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational scale, the agentive optimism gap serves both a coordination function (shared narratives enable coherent policy) and an extraction function (the gap suppresses lower-class political organizing by discounting their pessimism as irrational). The constraint is not a natural law but a contingent institutional arrangement that embeds power asymmetry into epistemic frameworks. The analytical perspective sees genuine tension between coordination benefit (elite coherence) and extraction cost (public despair and delegitimization).
constraint_indexing:constraint_classification(agentive_optimism_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agentive_optimism_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(agentive_optimism_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agentive_optimism_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(agentive_optimism_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(agentive_optimism_2026, TR),
    TR >= 0.70.

:- end_tests(agentive_optimism_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The elite policy class extracts significant value during the interval — they maintain decision-making authority, institutional legitimacy, and long-term planning capacity by sustaining optimistic framing even as public pessimism increases. However, the extraction is not maximal (0.66+) because the mechanism depends on public compliance that could collapse if the pessimism-to-optimism gap widens further. The value reflects real extraction (suppression of public voice, concentration of decision-making) but also real instability (the constraint requires constant maintenance as public skepticism increases). The measurement trajectory (0.35 → 0.62 over 24 time units) reflects accumulating extraction as the gap widens and public frustration intensifies despite elite messaging remaining constant. Suppression (0.68): High. Multiple suppression mechanisms operate: institutional distance (policy decisions occur behind closed doors), epistemic delegitimization (public pessimism is labeled irrational/media-driven rather than reflecting real conditions), economic precarity (populations dependent on current institutions have reduced capacity to exit or organize), and theater (participation mechanisms that feel like agency but produce minimal influence). Theater ratio (0.64): Moderate-high. Civic participation rituals (voting, town halls, online petitions, public comment periods) consume significant public time and energy but produce minimal policy influence in most contexts. The rituals are maintained because they provide legitimacy to both elites (we are listening) and participants (we have voice), but the functional mechanism (actual public input determining policy) has significantly atrophied. Measurement trajectory shows theater rising as participation mechanisms proliferate (digital platforms expand apparent access) while actual influence remains flat or declining.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence from the same structural facts. The pessimistic public sees a snare that traps them through hopelessness. The elite sees a necessary coordination mechanism. The organized intermediaries see a tool they can mobilize. The emerging alternatives see a scaffold to be gradually replaced. The democratic rituals see themselves as adequate forums while functioning as pitons. The analytical view sees both genuine coordination (elite coherence enables governance) and genuine extraction (public despair is suppressed and converted to compliance). No perspective agrees on the constraint's character because the extraction mechanism depends on epistemic disagreement — if all perspectives converged on a single assessment, the constraint would destabilize.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to the extraction flow. Elite policy makers hold beneficiary status with arbitrage options (can shift framing, maintain institutional authority) — derived d ≈ 0.10-0.15 (institutional + arbitrage = low d → negative χ for this group, they experience negative extraction, i.e., benefit). Pessimistic public holds victim status with trapped options — derived d ≈ 0.90-0.95 (powerless + trapped = high d → maximum f(d) → maximum χ). The moderately organized labor groups hold mixed positions: victim status (they organize around public pessimism) but mobile options (can reposition) — derived d ≈ 0.50-0.55 (moderate + constrained = middle d → middle f(d)). The regenerative coalition holds organized + mobile options — derived d ≈ 0.35-0.40 (mobile exit reduces experienced extraction despite victim status in traditional frame). The theater itself (democratic rituals) operates with institutional power and arbitrage (can redefine what counts as successful participation) — derived d ≈ 0.15 (institutional + arbitrage, low extraction for the ritual itself). The analytical observer has no structural benefit or cost — d ≈ 0.72 (analytical + analytical context).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolved through perspectival divergence analysis: this constraint demonstrates that the mandatrophy (is this coordination or extraction?) is genuinely underdetermined by the base metrics alone. The constraint IS both coordination (elite optimism coordinates decision-making, enables coherence, solves internal institutional coordination problems) AND extraction (public pessimism is suppressed, delegitimized, and converted into compliance). The resolution is not 'pick one type' but 'acknowledge the hybrid structure is real and structural, not a measurement error.' The tangled rope classification captures this: genuine coordination function (beneficiaries: elite policy makers benefit from shared optimistic framing that enables long-term planning and institutional coherence) bundled with genuine asymmetric extraction (victims: public pessimism is treated as irrationality rather than legitimate response to material conditions, suppressing alternative policy frames). The constraint requires active enforcement (elites must continuously maintain optimism framing despite contradicting evidence; public must be continuously delegitimized as irrational). Both the coordination and extraction are load-bearing — removing either one changes the structural character. The mandatrophy is resolved by showing that tangled rope structure is load-bearing: elite coherence genuinely depends on suppressing public voice, not despite it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimism_contagion_threshold,
    'At what public pessimism percentage does elite optimism cease to function as coordinating narrative and become actively destabilizing?',
    'Time series analysis of public confidence indices, policy approval ratings, and protest frequency; identification of inflection point where elite optimism messaging produces backlash rather than compliance',
    'If threshold is low (30-40% public pessimism): gap destabilizes within 5-10 years, forcing policy recalibration. If threshold is high (60%+): elite optimism can persist despite massive public opposition, suggesting extraction is structural rather than rhetorical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimism_contagion_threshold, empirical, 'The public pessimism threshold where elite optimism becomes destabilizing').

omega_variable(
    is_optimism_elite_privilege_or_epistemic_accuracy,
    'Does elite optimism reflect genuine epistemic access to positive trends (technological progress, institutional capacity) or does it reflect class-position privilege that insulates elites from precarity?',
    'Comparative analysis of elite and public predictions against realized outcomes over 10-year horizon; examination of whether elite optimism outperforms pessimism in forecast accuracy or merely feels better; behavioral testing of whether optimism predicts investment/risk-taking or merely morale',
    'If optimism is epistemically justified: public pessimism is irrational despair, and the gap represents coordination problem solvable through better communication. If optimism is privilege: the gap is extraction (elite decision-making insulated from consequences of their choices), and convergence requires redistribution of risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(is_optimism_elite_privilege_or_epistemic_accuracy, conceptual, 'Whether elite optimism reflects epistemic accuracy or class-position privilege').

omega_variable(
    public_pessimism_exogeneity,
    'Is public pessimism endogenous to the gap itself (produced by elite optimism messaging that contradicts lived experience) or exogenous (driven by material conditions independent of elite framing)?',
    'Counterfactual analysis: do public pessimism levels vary when elite optimism framing changes (holding material conditions constant)? Do material improvements reduce pessimism even when elite messaging remains optimistic? Qualitative research on public attribution of pessimism sources.',
    'If endogenous: the gap is self-reinforcing — elite optimism creates distrust, increasing public pessimism, which elites interpret as irrationality requiring more optimism messaging. Breaking the cycle requires institutional change. If exogenous: pessimism reflects real conditions, and closing the gap requires either elite pessimism-realism or material improvement for public.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_pessimism_exogeneity, empirical, 'Whether public pessimism is produced by the gap itself or driven by material conditions').

omega_variable(
    coalition_formation_latency,
    'How long does it take for trapped/pessimistic populations to overcome learned helplessness and organize collective action against the constraint?',
    'Historical analysis of social movement emergence timelines; correlation between pessimism intensity and time-to-mobilization; identification of tipping points where passive despair shifts to active resistance.',
    'If latency is long (15+ years): elite optimism can persist through burnout of opposition. If latency is short (3-5 years): constraint becomes unstable quickly, forcing rapid institutional adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_formation_latency, empirical, 'The latency period for organizing trapped populations into collective action').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(agentive_optimism_2026, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(agopt_tr_t0, agentive_optimism_2026, theater_ratio, 0, 0.48).
narrative_ontology:measurement(agopt_tr_t8, agentive_optimism_2026, theater_ratio, 8, 0.58).
narrative_ontology:measurement(agopt_tr_t16, agentive_optimism_2026, theater_ratio, 16, 0.64).
narrative_ontology:measurement(agopt_tr_t24, agentive_optimism_2026, theater_ratio, 24, 0.71).

% Extraction over time
narrative_ontology:measurement(agopt_be_t0, agentive_optimism_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(agopt_be_t8, agentive_optimism_2026, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(agopt_be_t16, agentive_optimism_2026, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(agopt_be_t24, agentive_optimism_2026, base_extractiveness, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(agentive_optimism_2026, information_standard).
narrative_ontology:boltzmann_floor_override(agentive_optimism_2026, 0.42).
narrative_ontology:affects_constraint(agentive_optimism_2026, institutional_legitimacy_deficit).
narrative_ontology:affects_constraint(agentive_optimism_2026, learned_helplessness_trap).
narrative_ontology:affects_constraint(agentive_optimism_2026, elite_epistemic_closure).

% DUAL FORMULATION NOTE:
% The agentive optimism gap decomposes into three structurally related constraints: (1) institutional_legitimacy_deficit (ε ≈ 0.65, pure snare from public perspective) — the loss of trust in institutions despite elite maintenance of institutional confidence; (2) learned_helplessness_trap (ε ≈ 0.72, pure snare) — psychological entrenchment of pessimism across populations; (3) elite_epistemic_closure (ε ≈ 0.45, rope-to-tangled-rope spectrum) — institutional insulation from disconfirming evidence. These three are distinct constraints with different intervention points but are linked by the agentive optimism gap as their common pressure valve: the gap functions to prevent the three from coalescing into a unified crisis. If the gap closes (elite pessimism increases or public optimism increases), the three downstream constraints will create rapid institutional instability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(agentive_optimism_2026, institutional, 0.12).
constraint_indexing:directionality_override(agentive_optimism_2026, powerless, 0.93).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
