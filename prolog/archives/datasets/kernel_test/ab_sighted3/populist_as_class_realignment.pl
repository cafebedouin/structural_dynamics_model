% ============================================================================
% CONSTRAINT STORY: populist_as_class_realignment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_populist_as_class_realignment, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: populist_as_class_realignment
 *   human_readable: Populist Realignment as Education-Based Class Restructuring
 *   domain: political_economy/comparative_politics/democratic_theory
 *
 * SUMMARY:
 *   The populist realignment represents a fundamental restructuring of class
 *   politics in post-industrial democracies, driven by education-based
 *   stratification that cross-cuts traditional economic cleavages.
 *   Working-class voters without college degrees have shifted from
 *   left-of-center parties (which historically mobilized them around economic
 *   redistribution) to right-wing populist parties (which mobilize them
 *   around cultural protection and anti-elite framing). This constraint
 *   exhibits multiple DR types from different perspectives, making it a
 *   diagnostic case for how political-economic structures can appear as
 *   natural laws (mountain), coordination mechanisms (rope), temporary
 *   problems with sunset logic (scaffold), or extraction mechanisms
 *   (snare/tangled_rope), depending on the observer's structural position.
 *   The constraint's rising theater_ratio (0.15 → 0.35) reflects increasing
 *   divergence between populist parties' cultural protection rhetoric and
 *   their actual policy delivery, particularly on economic redistribution.
 *   The rising suppression_requirement (0.30 → 0.48) tracks the
 *   intensification of barriers to cross-class solidarity: media gatekeeping
 *   that frames economic redistribution as politically impossible, party
 *   establishment resistance to left-populist challengers, and the dominance
 *   of cultural identity framing that makes economic solidarity literally
 *   unthinkable from within the identity frame.
 *
 * KEY AGENTS:
 *   - Deindustrialized Worker: Primary victim (powerless/identity_locked) — working-class identity reconstituted through cultural grievance rather than economic solidarity; cannot exit the realignment because the identity frame makes economic solidarity invisible
 *   - Union Member: Secondary victim (moderate/constrained) — retains organizational memory of economic solidarity but faces high costs to exit populist coalition; experiences both coordination (anti-elite mobilization) and extraction (cultural framing displaces redistribution)
 *   - Right-Wing Populist Party: Primary beneficiary (institutional/arbitrage) — captures working-class votes without committing to redistributive policy; experiences realignment as pure coordination
 *   - Social Democratic Party: Victim and complicit actor (institutional/constrained) — education-based stratification of own coalition alienates working-class base; exit is costly because returning to economic redistribution requires alienating professional-class infrastructure
 *   - Cross-Class Solidarity Movement: Organized agents (organized/mobile) — Sanders, Corbyn, Mélenchon, Podemos attempting to rebuild economic solidarity across education divides; see realignment as temporary with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing education stratification as inevitable post-industrial feature, obscuring contingent political choices by center-left parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(populist_as_class_realignment, 0.38).
domain_priors:suppression_score(populist_as_class_realignment, 0.48).
domain_priors:theater_ratio(populist_as_class_realignment, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(populist_as_class_realignment, extractiveness, 0.38).
narrative_ontology:constraint_metric(populist_as_class_realignment, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(populist_as_class_realignment, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(populist_as_class_realignment, tangled_rope).
narrative_ontology:human_readable(populist_as_class_realignment, "Populist Realignment as Education-Based Class Restructuring").
narrative_ontology:topic_domain(populist_as_class_realignment, "political_economy/comparative_politics/democratic_theory").

domain_priors:requires_active_enforcement(populist_as_class_realignment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(populist_as_class_realignment, right_wing_populist_parties).
narrative_ontology:constraint_beneficiary(populist_as_class_realignment, cultural_protection_coalitions).
narrative_ontology:constraint_beneficiary(populist_as_class_realignment, anti_elite_mobilization_networks).
narrative_ontology:constraint_victim(populist_as_class_realignment, social_democratic_welfare_coalitions).
narrative_ontology:constraint_victim(populist_as_class_realignment, redistributive_policy_frameworks).
narrative_ontology:constraint_victim(populist_as_class_realignment, working_class_economic_representation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEINDUSTRIALIZED WORKER (SNARE) — Identity-locked into cultural protection framing after economic abandonment by traditional left parties. Cannot exit the realignment because their working-class identity has been reconstituted through cultural grievance rather than economic solidarity. The shift from economic to cultural framing is experienced as natural rather than constructed, making the extraction mechanism invisible from within the identity frame.
constraint_indexing:constraint_classification(populist_as_class_realignment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: UNION MEMBER (TANGLED ROPE) — Constrained by declining union density and weakened collective bargaining power, but retains organizational memory of economic solidarity. Experiences genuine coordination function (populist parties do mobilize against elite extraction) alongside asymmetric extraction (cultural framing displaces economic redistribution). Can see both the coordination and the extraction but faces high costs to exit — leaving the populist coalition means abandoning the only political force that acknowledges their grievances, even if it misdirects them.
constraint_indexing:constraint_classification(populist_as_class_realignment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RIGHT-WING POPULIST PARTY (ROPE) — Primary beneficiary. Experiences the realignment as pure coordination: mobilizing working-class voters around cultural protection and anti-elite framing solves the collective action problem of building a winning electoral coalition. Extraction runs toward this agent — they capture working-class votes without committing to redistributive policy. Arbitrage exit: can shift between cultural and economic framing as electoral incentives dictate.
constraint_indexing:constraint_classification(populist_as_class_realignment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SOCIAL DEMOCRATIC PARTY (TANGLED ROPE) — Victim of the realignment but also complicit in creating it. Constrained by the education-based stratification of their own coalition: highly educated urban professionals dominate party leadership and policy priorities, alienating working-class base. Experiences both coordination (the party still mobilizes voters and contests elections) and extraction (working-class defection undermines redistributive coalitions). Exit is costly: returning to economic redistribution requires alienating the professional-class donors and activists who now control party infrastructure.
constraint_indexing:constraint_classification(populist_as_class_realignment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CROSS-CLASS SOLIDARITY MOVEMENT (SCAFFOLD) — Organized coalitions (Bernie Sanders 2016/2020, Jeremy Corbyn 2017, Jean-Luc Mélenchon, Podemos) attempting to rebuild economic solidarity across education divides. See the realignment as temporary: education-based stratification is a contingent feature of post-industrial economies, not a permanent cleavage. Sunset logic: if these movements successfully reframe class politics around economic redistribution rather than cultural identity, the populist realignment loses its structural foundation. Low extraction because the coalition has agency and sees an exit path, but suppression is real — media gatekeeping, party establishment resistance, and cultural framing dominance create barriers.
constraint_indexing:constraint_classification(populist_as_class_realignment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL DETERMINISM VIEW (MOUNTAIN) — From a civilizational/global perspective, education-based stratification appears as an inevitable consequence of post-industrial economic transformation: knowledge economies structurally privilege credentialed workers, and political coalitions realign accordingly. This perspective sees the populist shift as an immutable feature of late capitalism. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that the 'inevitable post-industrial realignment' framing naturalizes what is actually a contingent political choice by center-left parties to abandon economic redistribution in favor of cultural liberalism.
constraint_indexing:constraint_classification(populist_as_class_realignment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(populist_as_class_realignment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(populist_as_class_realignment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(populist_as_class_realignment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(populist_as_class_realignment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Right-wing populist parties capture working-class votes and mobilize anti-elite sentiment, but redirect it away from economic redistribution toward cultural protection and immigration restriction. The extraction is real but not maximal — some genuine coordination function exists (populist parties do challenge elite consensus on trade, immigration, and cultural issues). The value reflects that working-class voters gain symbolic recognition and cultural validation, even as they lose material redistribution. Suppression (0.48): Moderate-high. Significant barriers to cross-class economic solidarity include: media framing that treats economic redistribution as politically impossible, party establishment resistance to left-populist challengers (Sanders/Corbyn treatment), dominance of cultural identity framing in political discourse, and union decline reducing organizational capacity for economic mobilization. But suppression is not total — cross-class solidarity movements have emerged and contested elections, proving exit is possible at high cost. Theater ratio (0.35): Moderate. Populist parties deliver some cultural protection (immigration restriction, symbolic cultural policy) but the gap between rhetoric and material policy delivery is substantial. Campaign promises of economic nationalism and anti-elite redistribution rarely translate to governing policy — tax cuts for the wealthy, deregulation, and welfare retrenchment are common. The theater has increased over the interval as the divergence between cultural rhetoric and economic policy has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — working-class realignment from left to right-wing populist parties — appears as different constraint types depending on structural position. The deindustrialized worker sees a snare (identity-locked into cultural framing, extraction invisible from within). The union member sees tangled_rope (genuine coordination function alongside asymmetric extraction, can see both but exit is costly). The right-wing populist party sees rope (pure coordination — mobilizing voters solves collective action problem). The social democratic party sees tangled_rope (victim of realignment but also complicit in creating it through education-based stratification of own coalition). The cross-class solidarity movement sees scaffold (temporary problem with sunset logic — education stratification is contingent, not permanent). The analytical observer risks seeing mountain (education stratification as inevitable post-industrial feature) but the structural data reveals this as a false summit — the 'inevitable' framing naturalizes contingent political choices by center-left parties to abandon economic redistribution.
 *
 * DIRECTIONALITY LOGIC:
 *   The deindustrialized worker perspective uses identity_locked exit, which derives d from victim status + identity fusion. The worker is structurally mobile (could vote for other parties, could organize economically) but functionally trapped because their working-class identity has been reconstituted through cultural grievance. Exit would require not just changing vote but abandoning the identity frame that makes sense of their economic abandonment. The union member perspective uses constrained exit — higher barriers than mobile but lower than trapped. Union members retain organizational memory of economic solidarity and can see the extraction mechanism, but face high costs to exit (leaving the populist coalition means abandoning the only political force acknowledging their grievances). The right-wing populist party perspective uses arbitrage exit — can shift between cultural and economic framing as electoral incentives dictate, experiencing minimal extraction. The social democratic party perspective uses constrained exit — returning to economic redistribution requires alienating the professional-class infrastructure that now controls the party. The cross-class solidarity movement uses mobile exit — organized coalitions with agency and exit paths, though suppression is real. The analytical observer uses analytical exit — observes from outside but risks naturalizing contingent political choices as structural inevitability.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC CASE: This constraint resolves the mandatrophy by showing that all classification types are legitimate perspectival readings of the same structural data, with the critical addition that one perspective (the analytical mountain) is itself a naturalization that serves identifiable beneficiaries. The mandatrophy is not 'which type is correct?' but 'which perspective reveals the extraction mechanism?' The deindustrialized worker's snare is their lived reality — identity-locked and unable to see the extraction from within. The union member's tangled_rope is their structural position — can see both coordination and extraction but exit is costly. The populist party's rope is their genuine experience — they are the beneficiary. The social democratic party's tangled_rope reflects their complicity — victims of a realignment they helped create. The scaffold is the organized coalition's structural possibility — sunset is real if economic solidarity can be rebuilt. The mountain is the false summit — the analytical framing that naturalizes education stratification as inevitable serves the professional class that dominates center-left parties and prefers cultural liberalism to economic redistribution. The presheaf over the observation site IS the answer, with the additional recognition that some perspectives are themselves part of the extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    education_cleavage_permanence,
    'Is education-based political stratification a permanent feature of post-industrial democracies, or a contingent outcome of specific policy choices by center-left parties?',
    'Cross-national comparison of countries where social democratic parties maintained economic redistribution focus (Nordic model) vs those that shifted to cultural liberalism (Third Way). Longitudinal analysis of working-class vote share correlation with party economic vs cultural policy emphasis.',
    'If permanent: mountain classification from analytical perspective is correct — realignment is structural. If contingent: false summit — the ''inevitable'' framing naturalizes a political choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(education_cleavage_permanence, empirical, 'Whether education stratification is structural or contingent').

omega_variable(
    cultural_framing_sufficiency,
    'Do right-wing populist parties actually deliver cultural protection to working-class voters, or is the cultural framing purely theatrical cover for elite economic extraction?',
    'Policy outcome analysis: immigration restriction effectiveness, cultural policy implementation, symbolic vs material benefits delivered to working-class base. Comparison of campaign rhetoric to governing record.',
    'If cultural protection is real: coordination function is genuine, tangled_rope classification holds. If purely theatrical: coordination function collapses, classification shifts toward snare from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_framing_sufficiency, empirical, 'Whether cultural protection framing delivers real benefits').

omega_variable(
    identity_lock_reversibility,
    'Can working-class voters who have shifted to cultural identity framing be re-mobilized around economic solidarity, or is the identity transformation irreversible within a biographical timeframe?',
    'Experimental evidence from cross-class solidarity campaigns (Sanders, Corbyn, Mélenchon). Voter panel data tracking identity frame shifts in response to economic vs cultural messaging. Psychological research on identity fusion and deconversion.',
    'If reversible: scaffold perspective is structurally grounded — sunset is achievable. If irreversible: identity_locked exit option is permanent, snare classification becomes dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether cultural identity lock can be broken biographically').

omega_variable(
    false_summit_beneficiary_identification,
    'Is the analytical mountain perspective (education stratification as inevitable) itself a naturalization that benefits specific actors — credentialed professional classes who dominate center-left parties and prefer cultural liberalism to economic redistribution?',
    'Sociological analysis of who produces and disseminates the ''inevitable post-industrial realignment'' narrative. Correlation between academic/media/political elite class position and acceptance of structural determinism framing. Material interest analysis: does the mountain framing serve the professional class by pre-adjudicating economic redistribution as politically impossible?',
    'If the mountain framing benefits identifiable actors: false summit confirmed — the ''natural law'' is a constructed constraint. If the framing is genuinely observer-independent: mountain classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_beneficiary_identification, conceptual, 'Whether the mountain framing itself serves beneficiary interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(populist_as_class_realignment, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pop_realign_theater_1980, populist_as_class_realignment, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pop_realign_theater_1990, populist_as_class_realignment, theater_ratio, 10, 0.22).
narrative_ontology:measurement(pop_realign_theater_2000, populist_as_class_realignment, theater_ratio, 20, 0.3).
narrative_ontology:measurement(pop_realign_theater_2010, populist_as_class_realignment, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(pop_realign_extract_1980, populist_as_class_realignment, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(pop_realign_extract_1990, populist_as_class_realignment, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(pop_realign_extract_2000, populist_as_class_realignment, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(pop_realign_extract_2010, populist_as_class_realignment, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(pop_realign_suppress_1980, populist_as_class_realignment, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(pop_realign_suppress_1990, populist_as_class_realignment, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(pop_realign_suppress_2000, populist_as_class_realignment, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(pop_realign_suppress_2010, populist_as_class_realignment, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(populist_as_class_realignment, identity_coordination).

% DUAL FORMULATION NOTE:
% The populist realignment is downstream of post_industrial_spatial_extraction (the geographic concentration of economic opportunity in credentialed urban centers, leaving deindustrialized regions behind). The upstream constraint creates the material conditions (spatial inequality, economic abandonment) that make cultural identity framing politically viable. The realignment has its own extractiveness reflecting the political mechanism (cultural framing displacing economic redistribution), distinct from the upstream constraint's extractiveness reflecting the economic mechanism (spatial concentration of opportunity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(populist_as_class_realignment, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
