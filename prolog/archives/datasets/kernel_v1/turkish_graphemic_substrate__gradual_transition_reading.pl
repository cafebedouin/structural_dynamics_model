% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__gradual_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__gradual_transition_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: turkish_graphemic_substrate__gradual_transition_reading
 *   human_readable: Dual-Script Literacy Transition: Turkish Graphemic Substrate (Gradual Transition Reading)
 *   domain: political_linguistics/state_formation/cultural_engineering
 *
 * SUMMARY:
 *   The Turkish script transition initiated by Atatürk's 1928 reform
 *   confronts the state with a permanent structural tension: rapid national
 *   modernization requires graphemic alignment with international commerce
 *   and European infrastructure (Latin script), yet such alignment risks
 *   severing continuity with Ottoman-Islamic civilization, inherited cultural
 *   knowledge, and elder-cohort literacy. The gradual_transition_reading
 *   proposes a managed compromise: maintain dual-script literacy education
 *   during a 5-15 year window while the state completes its infrastructure
 *   pivot. This reduces generational rupture and preserves access to Ottoman
 *   cultural materials. However, the reading instantiates a tangled_rope
 *   constraint: the state benefits from modernization acceleration and the
 *   legitimacy gain from claiming to preserve cultural continuity, while
 *   intermediate cohorts bear significant educational burden and elder
 *   cohorts experience degrading social literacy utility. The constraint's
 *   extractiveness accumulates during the transition window (t=0 to t=10) as
 *   the intermediate cohorts internalize the cost, peaks at t=10, then
 *   decreases post-sunset (t=15) once Ottoman script support is formally
 *   withdrawn and only younger cohorts remain. The theater ratio similarly
 *   peaks mid-transition (t=10, 0.58) as dual-script education becomes
 *   performative compliance, then drops post-sunset when the theater is no
 *   longer needed.
 *
 * KEY AGENTS:
 *   - State Modernization Apparatus (institutional/arbitrage): Primary beneficiary — designs and controls the transition timeline; captures legitimacy gain from claiming cultural preservation while achieving rapid graphemic modernization
 *   - Younger Cohorts (organized/arbitrage): Secondary beneficiary — native to Latin script; gain access to Ottoman culture without primary burden; arbitrage exit available post-sunset
 *   - Intermediate Cohorts (moderate/constrained): Primary mixed agent — mandated dual-script literacy by state education policy; bear cognitive and pedagogical costs for coordination benefit of knowledge transfer
 *   - Elder Cohorts (powerless/trapped): Primary victim — accumulated literacy in Ottoman script degrades as state infrastructure privileges Latin; no exit within biographical timeline
 *   - Ottoman-Islamic Cultural Institutions (institutional/arbitrage): Institutional victim — continue operating during transition but under declining state support; experience the period as degraded inertia (piton perspective)
 *   - Ottoman-Islamic Intelligentsia (powerful/mobile): Transnational victim — scholars and networks benefit from temporary dual-script window but bear the underlying directionality that treats Ottoman script as deprecating
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__gradual_transition_reading, 0.52).
domain_priors:suppression_score(turkish_graphemic_substrate__gradual_transition_reading, 0.48).
domain_priors:theater_ratio(turkish_graphemic_substrate__gradual_transition_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__gradual_transition_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__gradual_transition_reading, "Dual-Script Literacy Transition: Turkish Graphemic Substrate (Gradual Transition Reading)").
narrative_ontology:topic_domain(turkish_graphemic_substrate__gradual_transition_reading, "political_linguistics/state_formation/cultural_engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__gradual_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__gradual_transition_reading, '60e4dc24-b842-4e02-9bb8-a9739116e464').
narrative_ontology:cs_kernel_codification('60e4dc24-b842-4e02-9bb8-a9739116e464', formalized).
narrative_ontology:cs_authority_grounding('60e4dc24-b842-4e02-9bb8-a9739116e464', extraction).
narrative_ontology:cs_interpretation_layer_present('60e4dc24-b842-4e02-9bb8-a9739116e464').
narrative_ontology:cs_reading_relation('60e4dc24-b842-4e02-9bb8-a9739116e464', turkish_graphemic_substrate__ottoman_continuity_reading, influences).
narrative_ontology:cs_reading_relation('60e4dc24-b842-4e02-9bb8-a9739116e464', turkish_graphemic_substrate__secular_nationalist_reading, influences).
narrative_ontology:cs_axiom('60e4dc24-b842-4e02-9bb8-a9739116e464', foundational, intergenerational_knowledge_preservation_feasible).
narrative_ontology:cs_axiom_status(intergenerational_knowledge_preservation_feasible, holdable).
narrative_ontology:cs_axiom_grounding('60e4dc24-b842-4e02-9bb8-a9739116e464', intergenerational_knowledge_preservation_feasible, empirically_contingent).
narrative_ontology:cs_axiom('60e4dc24-b842-4e02-9bb8-a9739116e464', foundational, managed_state_transition_timeline_binding).
narrative_ontology:cs_axiom_status(managed_state_transition_timeline_binding, holdable).
narrative_ontology:cs_axiom_grounding('60e4dc24-b842-4e02-9bb8-a9739116e464', managed_state_transition_timeline_binding, conventional).
narrative_ontology:cs_reference_frame('60e4dc24-b842-4e02-9bb8-a9739116e464', intergenerational_knowledge_preservation_through_dual_literacy).
narrative_ontology:cs_drift_state('60e4dc24-b842-4e02-9bb8-a9739116e464', post_ataturk_secular_state_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('60e4dc24-b842-4e02-9bb8-a9739116e464', '2025-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, state_modernization_apparatus).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, younger_cohorts).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, ottoman_islamic_cultural_continuity).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, elder_cohorts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ELDER COHORTS (SNARE) — Trapped in degrading literacy as the state privileges Latin script infrastructure, textbooks, and signage. No exit from biographical timeline. Bears full cost of the transition: accumulated knowledge in Ottoman script becomes unmaintained cultural artifact. Maximum experienced extraction with no agency.
constraint_indexing:constraint_classification(turkish_graphemic_substrate__gradual_transition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TRANSITION-PERIOD COHORTS (TANGLED ROPE) — Constrained to develop dual-script literacy by school curriculum mandate. This enables intergenerational knowledge transfer (genuine coordination benefit) but imposes substantial cognitive and educational cost. They experience both extraction (forced pedagogical overhead) and coordination benefit (preserving access to Ottoman cultural record). Constrained exit — literacy in both scripts is required, not voluntary.
constraint_indexing:constraint_classification(turkish_graphemic_substrate__gradual_transition_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: YOUNGER COHORTS (ROPE) — Primary beneficiary. Native Latin-script literacy aligns with state modernization, international commerce, and technological integration. Dual-script capability during transition window provides cultural access without primary burden. Arbitrage exit: can ignore Ottoman script entirely once transition period closes. Low experienced extraction; net coordination benefit from preserved access pathway.
constraint_indexing:constraint_classification(turkish_graphemic_substrate__gradual_transition_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE MODERNIZATION APPARATUS (SCAFFOLD) — Primary institutional beneficiary. The dual-script transition is explicitly temporary and sunsetting: the 5-15 year window is the managed interval during which state education, administration, and infrastructure complete the pivot to Latin script. After sunset, Ottoman script support is withdrawn (or residualized to heritage/museum contexts). Beneficiary experiences low effective extraction because timeline is declared and managed. Sunset clause is structural: the apparatus itself defines the transition window as temporary policy, not permanent arrangement.
constraint_indexing:constraint_classification(turkish_graphemic_substrate__gradual_transition_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: OTTOMAN-ISLAMIC INSTITUTIONS (PITON) — These institutions experience the dual-script period as degraded inertia: they continue transmitting Ottoman knowledge and conducting religious scholarship in Arabic script, but with declining institutional support, shrinking cohort of readers, and marginalization within state education. The constraint maintains them through the transition (they are not immediately prohibited), but only as performative cultural residue, not as functional transmission. Theater ratio ≥ 0.70: the institutions continue ritual practice (Quran recitation, Ottoman manuscript scholarship) but without structural renewal or state endorsement.
constraint_indexing:constraint_classification(turkish_graphemic_substrate__gradual_transition_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: OTTOMAN-ISLAMIC INTELLIGENTSIA (TANGLED ROPE) — Organized intellectuals and scholars with transnational connections (Al-Azhar scholars, Ottoman manuscript specialists, diaspora communities) experience the constraint as mixed. They benefit from the dual-script window (access to Ottoman records is preserved temporarily) but are victim to the underlying directionality (the state's modernization framework treats Arabic script as deprecating, not as equally legitimate). Mobile exit — can continue Ottoman-script scholarship in diaspora or transnational networks, but not in the state's primary institutional context.
constraint_indexing:constraint_classification(turkish_graphemic_substrate__gradual_transition_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, any state undergoes script modernization to align with dominant international infrastructure; this transition is presented as natural evolutionary law — all states that modernize undergo script convergence; the transition period is just the necessary friction. However, this naturalization occludes the structurally contingent choice of which script is the 'modern' one and who bears the cost. Engine false-summit detector will identify this as illegitimate naturalization.
constraint_indexing:constraint_classification(turkish_graphemic_substrate__gradual_transition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__gradual_transition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(turkish_graphemic_substrate__gradual_transition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(turkish_graphemic_substrate__gradual_transition_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(turkish_graphemic_substrate__gradual_transition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(turkish_graphemic_substrate__gradual_transition_reading, TR),
    TR >= 0.70.

:- end_tests(turkish_graphemic_substrate__gradual_transition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The constraint extracts from elder cohorts and cultural institutions through the mechanism of state-directed graphemic displacement. However, the extraction is not maximal (snare level ≥0.66) because: (1) intermediate cohorts receive genuine coordination benefit (intergenerational knowledge transfer), not pure extraction; (2) younger cohorts experience low extraction and net benefit. The extractiveness trajectory (0.38→0.48→0.52→0.45) reflects mid-transition peak intensity as educational mandates bite hardest, then decline post-sunset. Suppression (0.48): Moderate. Significant suppression mechanisms include: state infrastructure prioritization of Latin script in signage, administration, education; declining institutional support for Ottoman literacy training; social pressure to acquire 'modern' Latin literacy; marginalization of Ottoman script in public discourse. However, suppression is not total because dual-script mandate maintains official access pathways (schools, libraries, heritage institutions). Theater ratio (0.58): Moderate-high. The dual-script education system is substantially performative: it claims to preserve intergenerational knowledge transfer but often devolves to script recognition without functional literacy; Ottoman texts are archived rather than actively read; the preservation framing legitimates the underlying graphemic displacement. Post-sunset theater drops (0.40) because the institutional performance is no longer needed — Ottoman script is now openly marginalized as heritage rather than claimed as legitimate contemporary literacy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits dramatic perspectival divergence across the observation site. State apparatus sees scaffold (temporary, managed, with sunset clause). Younger cohorts see rope (coordination benefit without substantial burden). Intermediate cohorts see tangled_rope (mixed extraction and coordination). Elder cohorts see snare (pure extraction with no escape). Ottoman institutions see piton (degraded ceremonial continuation without structural renewal). Transnational intelligentsia see tangled_rope (benefit from preservation window offset by underlying directionality favoring Latin script). The analytical observer risks seeing mountain (graphemic transitions are natural evolutionary consequence of modernization), but structural data reveals this as false summit: the 'inevitability' of Latin script modernization is a contingent choice that benefits specific agents, not a law of nature. The perspectival gap reveals that classification depends entirely on structural position and time horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position: beneficiary vs victim, power level, and exit options. State modernization apparatus (institutional/arbitrage): d≈0.08 — primary beneficiary with optimal exit (can declare transition complete); derives low-to-negative f(d), low experienced extraction. Younger cohorts (organized/arbitrage): d≈0.20 — secondary beneficiary with optimal exit (post-sunset, no longer required to maintain Ottoman literacy); low experienced extraction, net benefit. Intermediate cohorts (moderate/constrained): d≈0.55 — mixed victim/beneficiary: they gain knowledge access but pay substantial educational cost; constrained exit (must complete dual-script education to progress); high-moderate experienced extraction. Elder cohorts (powerless/trapped): d≈0.92 — primary victim with no exit (biographical timeline closure); maximum experienced extraction. Ottoman institutions (institutional/arbitrage): d≈0.75 — institutional victim with arbitage exit (can continue outside state framework, as diaspora or transnational networks) but structurally disadvantaged within state authority; high-moderate experienced extraction. The engine computes χ = ε × f(d) × σ(S), where ε=0.52 (peak mid-transition), f(d) varies per perspective (sigmoid map of d), and σ(S)=1.0 (national scope). Peak χ occurs for powerless cohorts: χ≈0.52×1.42×1.0≈0.74 (snare threshold).
 *
 * MANDATROPHY ANALYSIS:
 *   READING-SPECIFIC MANDATROPHY: The gradual_transition_reading instantiates mandatrophy at the kernel level. The core claim (both scripts can coexist legitimately during transition) is under continuous pressure from: (1) nationalist directionality (secular_nationalist_reading) that treats Ottoman script as symbolically incompatible with modern Turkish identity; (2) continuity logic (ottoman_continuity_reading) that treats the transition period as arbitrary rupture from authentic Ottoman substrate. The reading can resolve mandatrophy only if: institutional configurations exist that permit both sibling readings to remain live simultaneously (empirical question: omega variable #4). If the state authority enforces a single reading (either nationalist or continuity), the gradual_transition collapses into a temporal phase of the winning reading — not a stable equilibrium. The reading's stability depends on the sunset clause being non-negotiable AND on the state maintaining institutional neutrality between the scripts during the transition window. Historical evidence suggests both conditions are fragile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intergenerational_knowledge_transfer_effectiveness,
    'Does dual-script literacy during the transition window actually preserve meaningful Ottoman-Islamic cultural transmission, or does the state''s prioritization of Latin script effectively foreclose it regardless of theoretical access?',
    'Longitudinal tracking of Ottoman-script literacy retention; measurement of cultural knowledge transmission depth in dual-literate cohorts vs. post-transition cohorts; assessment of whether dual-script education produces functional Ottoman-text reading or merely performative script recognition',
    'If effective: transition genuinely coordinates preservation with modernization; tangled_rope classification stands. If ineffective: dual-script mandate is theatrical compliance; constraint reclassifies toward snare (pure extraction from elder cohorts without actual coordination benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_knowledge_transfer_effectiveness, empirical, 'Effectiveness of dual-script education for knowledge transfer').

omega_variable(
    reading_foreclosure_by_nationalist_frame,
    'Does the secular_nationalist_reading''s core claim (Turkish identity is modern/distinct from Ottoman past) logically foreclose the gradual_transition_reading''s core claim (both scripts can legitimately coexist), or can these be held simultaneously by different institutional actors?',
    'Structural analysis of whether the nationalist frame explicitly rejects the legitimacy of Ottoman script, or merely prioritizes Latin script without denying Ottoman cultural value. Case history: Turkey 1928-1960, post-Atatürk revisionism, recent Ottoman cultural revival movements.',
    'If foreclosed: the readings are not coexisting but competing for state authority; the gradual_transition is unstable and will resolve toward one or the other. If coexisting: the transition period''s institutional legitimacy is higher; the constraint can persist beyond the nominal sunset if political will permits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_by_nationalist_frame, conceptual, 'Whether nationalist identity frame logically excludes gradual transition').

omega_variable(
    cost_internalization_by_intermediate_cohorts,
    'Are the educational and cognitive costs of dual-script literacy borne fairly by the intermediate cohorts, or are they disproportionately concentrated on already-marginalized subgroups (rural populations, religious minorities, lower-income families)?',
    'Comparative literacy outcomes across geographic and socioeconomic strata; measurement of dual-script achievement rates by cohort; identification of which populations abandon Ottoman-script learning despite state mandate',
    'If fairly distributed: tangled_rope classification holds (mixed extraction/coordination). If concentrated: extract from specific victim populations intensifies; constraint reclassifies toward snare from the affected cohorts'' perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_internalization_by_intermediate_cohorts, empirical, 'Distribution of dual-script literacy costs across cohorts').

omega_variable(
    alternative_reading_coexistence_institutional_basis,
    'What institutional configuration would permit ottoman_continuity_reading and secular_nationalist_reading to coexist within the state framework during the transition period, rather than one dominating state policy and foreclosing the other?',
    'Historical case analysis of federal or pluralistic states managing competing identity frameworks; assessment of whether Turkey''s unitary state structure permits genuine institutional coexistence, or whether state apparatus necessarily enforces one reading over others',
    'If coexistence feasible: gradual_transition is structurally sound and can achieve the claimed coordination benefit. If coexistence impossible at state level: the transition period masks an underlying antagonism; the constraint will resolve at sunset toward whichever reading has institutional control.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_reading_coexistence_institutional_basis, conceptual, 'Institutional basis for reading coexistence').

omega_variable(
    sunset_clause_enforceability,
    'Is the declared 5-15 year sunset clause binding on future state authorities, or can it be unilaterally extended/collapsed by political actors with interests in the transition period?',
    'Constitutional analysis of whether transition mandate is codified as time-bounded policy or declarative statement; assessment of Turkish precedents for policy sunset compliance; measurement of actual institutional behavior vs. declared sunset timeline across 1928-present',
    'If binding: sunset is real structural property; scaffold classification holds. If unenforceable: sunset is performative; constraint drifts toward snare (indefinite extraction from elder cohorts under guise of temporary transition) or piton (degraded Ottoman institutions maintained through inertia, not genuine transition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_enforceability, empirical, 'Enforceability of transition sunset clause').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__gradual_transition_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tgs_grad_theater_t0, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(tgs_grad_theater_t5, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(tgs_grad_theater_t10, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(tgs_grad_theater_t15, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 15, 0.4).

% Extraction over time
narrative_ontology:measurement(tgs_grad_extractiveness_t0, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tgs_grad_extractiveness_t5, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(tgs_grad_extractiveness_t10, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(tgs_grad_extractiveness_t15, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 15, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(tgs_grad_suppression_t0, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(tgs_grad_suppression_t5, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(tgs_grad_suppression_t10, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(tgs_grad_suppression_t15, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 15, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__gradual_transition_reading, identity_coordination).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, ottoman_continuity_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, secular_nationalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel turkish_graphemic_substrate. The three readings (gradual_transition, ottoman_continuity, secular_nationalist) are structurally distinct constraints with different ε values and beneficiary/victim structures. They are not three perspectives on a single constraint but three competing constraints that will be linked by the engine's network contamination analysis. Decomposition into three separate files permits precise ε values per reading: gradual_transition (ε=0.52, tangled_rope), ottoman_continuity (ε≈0.35-0.40, rope or tangled_rope depending on institutional implementation), secular_nationalist (ε≈0.60-0.70, snare or tangled_rope depending on violence intensity). Each reading is authored as its own complete constraint story with its own perspectives, omegas, and measurements. The network edges establish that these readings contaminate each other's institutional plausibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(turkish_graphemic_substrate__gradual_transition_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
