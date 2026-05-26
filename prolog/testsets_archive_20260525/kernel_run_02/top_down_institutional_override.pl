% ============================================================================
% CONSTRAINT STORY: top_down_institutional_override
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_top_down_institutional_override, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: top_down_institutional_override
 *   human_readable: Meiji Top-Down Institutional Override: Calendar and Dress Reform
 *   domain: political_history/institutional_change/modernization
 *
 * SUMMARY:
 *   The Meiji government's 1872-1873 calendar reform (lunisolar to Gregorian)
 *   and 1870-1886 dress reform (traditional kimono to Western clothing) were
 *   presented as necessary modernization but operated as top-down
 *   institutional overrides with unequal distribution of coordination
 *   benefits and compliance costs. These reforms test the commitment-system
 *   framework's capacity to model forced change where the prior system
 *   persists in practice despite legal abolition, creating stratified
 *   temporality (official Gregorian + concealed lunisolar; state Western
 *   dress + persistent rural kimono) rather than clean institutional
 *   displacement. The constraint exhibits extraction (concentrated costs on
 *   tradition-practitioners, concentrated benefits in state apparatus and
 *   merchant classes) coupled with genuine coordination function (calendar
 *   alignment with Western commerce, dress synchronization for diplomatic
 *   legitimacy). The measurement trajectory shows extractiveness declining
 *   from 0.72 to 0.35 over 20 years while theater_ratio rises from 0.28 to
 *   0.75, indicating the constraint evolved from active enforcement (snare)
 *   through hybrid enforcement-and-coordination (tangled rope) into
 *   theatrical maintenance with de facto parallel systems (piton). The
 *   constraint reveals how top-down overrides can succeed nominally (the
 *   official calendar is Gregorian, the official dress is Western) while
 *   failing substantively (lunisolar calculations persist in shrine records,
 *   agricultural practice, and folk calendars; traditional dress continues in
 *   rural areas, religious contexts, and private practice).
 *
 * KEY AGENTS:
 *   - Meiji State Apparatus: Primary beneficiary (institutional/arbitrage) — captures coordination gains from calendar synchronization and dress standardization; exercises enforcement power with minimal cost to state apparatus
 *   - Western Trading Bloc / Diplomatic Corps: Secondary beneficiary (institutional/arbitrage) — benefits from calendar and dress alignment; experiences low extraction cost as the constraint was designed for their coordination
 *   - Rural Calendar Practitioners: Primary victim (powerless/trapped) — agricultural timing, festival scheduling, and market participation were structured around lunisolar calendar; face legal prohibition, enforcement, and economic dislocation with no exit
 *   - Traditional Dress Artisans: Primary victim (powerless/trapped) — silk specialists, kimono-makers, and garment merchants lose market demand as state procurement and status signaling shift to Western suppliers; face cultural stigma and economic unviability
 *   - Organized Rural Communities: Secondary actor (organized/constrained) — generate de facto parallel systems (rescheduled festivals, hidden lunisolar almanacs) and negotiate informal exemptions; bear suppression costs but retain some agency
 *   - Western-Aligned Bureaucrats: Beneficiary (institutional/arbitrage) — advance careers through reform implementation; experience constraint as coordination mechanism solving their own problems
 *   - Analytical Observer: Sees the constraint as false summit when naturalized as modernization necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(top_down_institutional_override, 0.58).
domain_priors:suppression_score(top_down_institutional_override, 0.72).
domain_priors:theater_ratio(top_down_institutional_override, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(top_down_institutional_override, extractiveness, 0.58).
narrative_ontology:constraint_metric(top_down_institutional_override, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(top_down_institutional_override, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(top_down_institutional_override, tangled_rope).
narrative_ontology:human_readable(top_down_institutional_override, "Meiji Top-Down Institutional Override: Calendar and Dress Reform").
narrative_ontology:topic_domain(top_down_institutional_override, "political_history/institutional_change/modernization").

domain_priors:requires_active_enforcement(top_down_institutional_override).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(top_down_institutional_override, distributed).
narrative_ontology:cs_authority_grounding(top_down_institutional_override, extraction).
narrative_ontology:cs_reading_relation(top_down_institutional_override, meiji_calendar_as_inevitable_modernity, forecloses).
narrative_ontology:cs_reading_relation(top_down_institutional_override, meiji_calendar_as_endogenous_merchant_demand, coexists_with).
narrative_ontology:cs_axiom(top_down_institutional_override, foundational, gregorian_calendar_imposed_by_state_decree).
narrative_ontology:cs_axiom_status(gregorian_calendar_imposed_by_state_decree, holdable).
narrative_ontology:cs_axiom_grounding(top_down_institutional_override, gregorian_calendar_imposed_by_state_decree, empirically_contingent).
narrative_ontology:cs_axiom(top_down_institutional_override, foundational, traditional_practitioners_bore_transition_costs).
narrative_ontology:cs_axiom_status(traditional_practitioners_bore_transition_costs, holdable).
narrative_ontology:cs_axiom_grounding(top_down_institutional_override, traditional_practitioners_bore_transition_costs, empirically_contingent).
narrative_ontology:cs_axiom(top_down_institutional_override, secondary, reform_represented_genuine_coordination_need).
narrative_ontology:cs_axiom_status(reform_represented_genuine_coordination_need, holdable).
narrative_ontology:cs_axiom_grounding(top_down_institutional_override, reform_represented_genuine_coordination_need, empirically_contingent).
narrative_ontology:cs_reference_frame(top_down_institutional_override, lunar_temporal_coordination_baseline).
narrative_ontology:cs_drift_state(top_down_institutional_override, gregorian_official_implementation, gap(codification_collapse, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(top_down_institutional_override, meiji_state_apparatus).
narrative_ontology:constraint_beneficiary(top_down_institutional_override, western_aligned_bureaucrats).
narrative_ontology:constraint_victim(top_down_institutional_override, lunar_calendar_practitioners).
narrative_ontology:constraint_victim(top_down_institutional_override, traditional_dress_artisans).
narrative_ontology:constraint_victim(top_down_institutional_override, rural_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL CALENDAR PRACTITIONER (SNARE) — Agricultural rhythms, religious observances, and market cycles were structured around the lunisolar calendar. The 1873 decree criminalized traditional timing and severed coordination of planting, festivals, and transactions. No exit: the new system was legally mandatory, economically enforced through state offices, and geographically inescapable. High suppression — the constraint produced temporal disorientation and economic dislocation with minimal coordination benefit to the practitioner.
constraint_indexing:constraint_classification(top_down_institutional_override, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TRADITIONAL DRESS ARTISAN (SNARE) — Kimono-makers, silk specialists, and garment merchants lost primary market demand after 1870s dress reform mandates and elite status signaling shifted to Western clothing. The constraint was legal (sumptuary regulations codified Western dress for officials and public spaces), economic (state procurement shifted to Western suppliers), and social (status hierarchy inverted). Trapped with no exit — the artisan's craft became economically unviable in the state sector and faced cultural stigma.
constraint_indexing:constraint_classification(top_down_institutional_override, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: RURAL COMMUNITY — ORGANIZED RESPONSE (TANGLED ROPE) — Rural communities did not passively absorb the calendar reform. Local festivals (matsuri) were rescheduled but maintained; agricultural societies negotiated de facto dual-calendar operation; religious practitioners preserved lunisolar observances despite prohibition. The constraint exhibits genuine coordination function (aligning state administration with global commercial networks) alongside asymmetric extraction (erasure of traditional timing systems). High suppression (legal prohibition, confiscation of calendars, fines for traditional observance) but organized resistance created informal parallel structures. Organized power → constrained exit → moderate experienced extraction.
constraint_indexing:constraint_classification(top_down_institutional_override, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MEIJI STATE APPARATUS (ROPE) — The state apparatus experiences both constraints as pure coordination mechanisms: calendar synchronization with the Western trading world enables international commerce, reduces transaction costs, and aligns Japan's temporal infrastructure with global markets. Dress reform synchronized Japan's official appearance with Western diplomatic and commercial standards, reducing friction in international negotiations. The state has arbitrage — it can exit (revert to lunisolar system) without bearing extraction costs. Net beneficiary with low experienced extraction because the state designed the constraint for its own coordination benefit.
constraint_indexing:constraint_classification(top_down_institutional_override, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: WESTERN TRADING BLOC / DIPLOMATIC CORPS (ROPE) — The constraint solved coordination problems for Western merchants and diplomats: calendar synchronization eliminated temporal friction in contracts and shipping; dress reform made Japanese officials legible and socially compatible in international settings. Low extraction cost for Western actors — they benefit from the coordination without bearing suppression costs. The Western perspective sees the constraint as efficient coordination, not oppression.
constraint_indexing:constraint_classification(top_down_institutional_override, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PRESERVED PARALLEL TRADITION SYSTEMS (PITON) — The constraint did not achieve complete displacement. Shinto shrine calendars maintained lunisolar calculations; agricultural almanacs circulated informally; some communities observed traditional New Year on the old calendar for decades. Theater_ratio = 0.68 reflects this degradation: the official reform was performative — the state declared Gregorian calendar mandatory, but enforcement was porous, allowing parallel tradition systems to persist through theater (nominal compliance with hidden practice). By 1900, the constraint had become a piton — maintained through institutional inertia rather than active extraction, as the state tacitly accepted de facto coexistence.
constraint_indexing:constraint_classification(top_down_institutional_override, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — MODERNIZATION FRAME (TANGLED ROPE) — From a civilizational perspective, the Meiji reforms were not pure top-down imposition but a hybrid: genuine coordination problem (Japanese temporal and dress systems were genuinely incompatible with Western commercial and diplomatic systems) coupled with asymmetric extraction (the burden of transition fell entirely on tradition-practicing communities, with benefits concentrated in the state apparatus and merchant classes). The constraint exhibits both coordination function and extraction. Theater_ratio = 0.68 reflects that the reform's success in changing elite practice (government offices, military, schools) was genuine, but rural communities' de facto continued reliance on lunisolar timing shows the constraint's reach limits.
constraint_indexing:constraint_classification(top_down_institutional_override, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER — FALSE SUMMIT VIEW (MOUNTAIN) — A competing frame naturalizes the reform as inevitable: modernization requires calendar and dress synchronization with the Western standard; the Gregorian calendar is 'objectively' superior to the lunisolar system (solar year matches agricultural cycles more precisely, enables global commerce); Western dress is 'objectively' more efficient for industrial work. From this naturalizing perspective, the constraint appears as immutable law — the terminal attractor of modernization itself. However, the structural data reveals beneficiaries (state, merchants, Western traders) and victims (artisans, rural practitioners), indicating this is a false summit: the framing as 'inevitable modernization' naturalizes a contingent institutional choice that extracted value from specific groups.
constraint_indexing:constraint_classification(top_down_institutional_override, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(top_down_institutional_override_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(top_down_institutional_override, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(top_down_institutional_override, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(top_down_institutional_override, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(top_down_institutional_override, TR),
    TR >= 0.70.

:- end_tests(top_down_institutional_override_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, declining over interval. Initial value (0.72) reflects strong asymmetry in 1872-1873 when enforcement is active and penalties are steep. The decline to 0.35 by 1890s reflects the constraint's evolution toward piton status — enforcement becomes porous, parallel systems become tacitly acceptable, and the state no longer insists on complete displacement. At the endpoint (1890s), extractiveness is moderate because enforcement has degraded but the official system remains in place, creating 'dual practice' rather than clean restoration. Suppression (0.72): High and relatively stable. The constraint operates through legal prohibition (calendar decrees of 1872, dress codes of 1872-1886), economic enforcement (state procurement shifts, market disappearance of traditional goods), and social stigma (education curricula promoting Western standards). Suppression does not drop to near-zero by 1890 because legal prohibition remains formally in force, even as enforcement becomes selective. Theater ratio (0.68): Rising from 0.28 to 0.75 over the interval. Initial low theater reflects that the constraint was genuinely new and enforced — state offices actually switched to Gregorian calendar, officials actually wore Western dress, and this represented a real break. Rising theater reflects degradation: by 1890s, the Gregorian calendar is official but shrine calendars circulate lunisolar dates; Western dress is mandated for state functions but traditional dress persists in rural areas, creating a performative compliance (the official practice is Western, but actual practice is stratified). Theater rise indicates classic piton trajectory — the constraint persists through institutional momentum, not active function.
 *
 * PERSPECTIVAL GAP:
 *   The Meiji reformers and international merchants see the constraint as pure coordination (Rope: aligning Japan's temporal and sartorial infrastructure with global standards solves real problems). Rural practitioners trapped in the old system see extraction and suppression (Snare: their timing systems are criminalized, their livelihoods destroyed, their exits eliminated). Organized rural communities see a hybrid where some coordination benefit exists (state administration requires aligned timing, so cooperation has value) alongside extraction (the cooperation is mandatory, the costs are borne unequally, the parallel systems they construct exist in legal limbo). By the 1890s, the constraint degrades into theater (Piton) — the official system persists but enforcement becomes selective, creating a 'modern surface, traditional depth' aesthetic that neither fulfills the snare's extraction nor achieves the rope's coordination. The analytical observer can frame the reform as inevitable modernization (false summit: Mountain view that naturalizes the extraction as necessary law) or as a contingent institutional choice that extracted value from identifiable groups (tangled rope view that highlights the beneficiaries and cost-bearers).
 *
 * DIRECTIONALITY LOGIC:
 *   Meiji State Apparatus: Beneficiary status with arbitrage exit → derived d ≈ 0.05 → negative f(d) → low or negative effective extraction (the state experiences the constraint as coordination benefit, not extraction cost). Western Trading Bloc: Beneficiary status with arbitrage exit → derived d ≈ 0.10 → low f(d) → low experienced extraction. Rural practitioners and artisans: Victim status with trapped exit → derived d ≈ 0.92 → high f(d) → maximum experienced extraction (0.92 × 1.70 of the sigmoid gives f(d) ≈ 1.35). Organized rural communities: Victim status with constrained exit → derived d ≈ 0.68 → moderate f(d) ≈ 0.95 → moderate experienced extraction. The perspectival gap arises because the state and Western traders derive low or negative d (they benefit from the alignment), while tradition-practitioners derive high d (they bear the costs). Organized rural response partly reduces d by developing parallel systems that provide partial exit, shifting from snare (d ≈ 0.92) to tangled rope (d ≈ 0.68).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that the answer to 'is this coordination or extraction?' is both, but whose perspective you ask determines which classification dominates. The beneficiary (state apparatus) experienced pure coordination: solving the real problem of temporal and sartorial incompatibility with global systems. The victim (tradition-practitioners) experienced pure extraction: bearing costs with no benefit, suppressed by law, offered no option to exit. The analytical observer's task is to recognize that the apparent 'efficiency' of the modernization narrative is itself a product of whose perspective is amplified. The constraint does not fit neatly into any single type because it genuinely is a hybrid (tangled_rope is correct as the claimed type) with perspectival variations (snare for those trapped, rope for beneficiaries, piton as it degrades). The mandatrophy is resolved by accepting that institutional overrides are inherently asymmetric — they solve real coordination problems for beneficiaries while imposing costs on victims. The question is not 'which type is true?' but 'who bears the costs of coordination and who captures the benefits?' The measurement trajectory shows this graphically: as the constraint's 'extractive pressure' declines (extractiveness falls from 0.72 to 0.35), its 'theatrical content' rises (theater from 0.28 to 0.75), indicating the constraint is succeeding as spectacle (official Gregorian, official Western dress) while failing as hegemony (lunisolar and traditional dress persist).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_versus_exogenous_reform,
    'Did the Meiji state impose these reforms exogenously (external force crushing internal resistance) or did it respond to endogenous pressure (merchant demand, cultural aspiration, coordination necessity)?',
    'Documentary evidence: samurai merchant petitions for calendar alignment with Western trading partners; intellectual movement within Japan toward Western standards (Fukuzawa, bunmei kaika); comparative analysis with other non-Western modernization cases; reconstruction of state decision-making hierarchy and competing internal factions.',
    'If exogenous: constraint is pure extraction-with-coordination theater. If endogenous: constraint is a genuine hybrid where some segments of Japanese society benefited from alignment and actively supported reform. Classification shifts from stronger snare bias to genuine tangled_rope across perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_versus_exogenous_reform, empirical, 'Whether reforms were externally imposed or responded to internal demand').

omega_variable(
    suppression_structural_versus_internalized,
    'Is the measured suppression (0.72) structural (legal prohibition, enforcement, economic barriers) or internalized (the practitioner class came to accept Gregorian calendar as superior or inevitable)?',
    'Linguistic/archival evidence: petitions from calendar practitioners demanding restoration; educational curricula and literacy materials promoting Gregorian system; religious texts and folk memory recording resistance or acceptance narratives. Longitudinal interview data if available; comparison of rural vs urban acceptance trajectories.',
    'If structural: suppression persists until enforcement mechanisms relax. If internalized: suppression was converted to internalized legitimacy — the constraint shifted from snare to rope as practitioners adopted the new system as their own. Post-Meiji data on voluntary calendar use, dress choice, and transmission to next generation reveals mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_structural_versus_internalized, empirical, 'Whether suppression operated through external barriers or internalized acceptance').

omega_variable(
    parallel_tradition_persistence_mechanism,
    'Did the parallel tradition systems (lunisolar calculations, traditional dress continuing in rural areas) persist through active resistance, bureaucratic tolerance, or epistemic legitimacy gap (the reformers did not fully believe their own narrative)?',
    'State records on enforcement variations across regions; comparison of penalty rates for calendar/dress violations in urban vs rural prefectures; inspection reports from state officials documenting de facto coexistence; private correspondence of reformers revealing ambivalence about complete displacement.',
    'If active resistance: victims retained agency and power. If bureaucratic tolerance: state tacitly accepted parallel systems, indicating weaker extraction capacity than formal prohibition suggests. If legitimacy gap: reformers were not convinced of Gregorian superiority themselves. Each mechanism implies different classification trajectory — piton classification (degradation via theater) requires one of these explanations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parallel_tradition_persistence_mechanism, empirical, 'Why parallel tradition systems persisted despite legal prohibition').

omega_variable(
    stratified_temporality_boundary,
    'At what point did the constraint transition from active enforcement (snare/tangled_rope) to maintenance through theater (piton)? When did parallel systems become ''acceptable traditional practice'' rather than prohibited violation?',
    'Timeline of enforcement action: frequency of calendar violation prosecutions, confiscations, and fines by decade; archival records of state policy shifts acknowledging traditional observance; newspaper and intellectual texts documenting changing discourse from ''eradication'' to ''modernization coexisting with tradition.'' Sociological markers of acceptance (educational institutions, government calendars, shrine practices).',
    'If transition occurred early (by 1890): constraint downgraded to piton faster than measurement interval suggests. If transition was delayed (post-1900): constraint maintained high extractiveness longer. Theater_ratio (0.68) captures this intermediate state; precise timing reveals how durable the snare classification was versus how quickly it degraded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratified_temporality_boundary, empirical, 'Timeline of transition from enforcement to theatrical coexistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(top_down_institutional_override, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ttio_tr_t0, top_down_institutional_override, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ttio_tr_t3, top_down_institutional_override, theater_ratio, 3, 0.42).
narrative_ontology:measurement(ttio_tr_t6, top_down_institutional_override, theater_ratio, 6, 0.58).
narrative_ontology:measurement(ttio_tr_t10, top_down_institutional_override, theater_ratio, 10, 0.68).
narrative_ontology:measurement(ttio_tr_t15, top_down_institutional_override, theater_ratio, 15, 0.72).
narrative_ontology:measurement(ttio_tr_t20, top_down_institutional_override, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(ttio_be_t0, top_down_institutional_override, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(ttio_be_t3, top_down_institutional_override, base_extractiveness, 3, 0.68).
narrative_ontology:measurement(ttio_be_t6, top_down_institutional_override, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(ttio_be_t10, top_down_institutional_override, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(ttio_be_t15, top_down_institutional_override, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(ttio_be_t20, top_down_institutional_override, base_extractiveness, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(top_down_institutional_override, global_infrastructure).
narrative_ontology:affects_constraint(top_down_institutional_override, meiji_state_capacity_expansion).
narrative_ontology:affects_constraint(top_down_institutional_override, western_diplomatic_legitimacy).

% DUAL FORMULATION NOTE:
% The calendar reform and dress reform are treated as a single constraint (institutional override mechanism) because they operate on the same time scale, share enforcement mechanisms, and produce similar victim and beneficiary profiles. If decomposing: calendar_synchronization_coordination (ε ≈ 0.30, Rope for merchants; Snare for agricultural communities; Piton by 1890s) and dress_standardization_mandate (ε ≈ 0.52, Rope for state; Snare for artisans; similar piton trajectory). Both feed upstream into meiji_state_capacity_expansion (administrative centralization) and downstream into western_diplomatic_legitimacy (international standing). Current unified story treats them as expressions of a single override mechanism: the state's imposition of Western institutional forms as markers of modernity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(top_down_institutional_override, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
