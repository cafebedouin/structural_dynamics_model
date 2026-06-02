% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__democratic_gradualism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__democratic_gradualism_reading, []).

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
 *   constraint_id: manifesto_revolutionary_method__democratic_gradualism_reading
 *   human_readable: Democratic Gradualism as Revolutionary Method (Marxist Reading)
 *   domain: political_philosophy/historical_materialism/revolutionary_theory
 *
 * SUMMARY:
 *   The democratic-gradualist reading of Marxist revolutionary method claims
 *   that socialism is achievable through winning electoral majorities within
 *   existing liberal-democratic frameworks and implementing gradual
 *   institutional reform that expropriates capital without rupture with the
 *   state apparatus. This reading instantiates ONE interpretation of the
 *   contested kernel 'manifesto_revolutionary_method' — the claim that
 *   working-class power can be exercised to achieve socialism. The reading is
 *   held as a live strategic position by social-democratic parties,
 *   Eurocommunist traditions, and democratic-socialist movements; it competes
 *   within the same intellectual and organizational traditions (Marxism,
 *   historical materialism) with vanguard-rupture readings (Leninism, Maoism)
 *   and council-communist/autonomist readings (spontaneous-mass-movement
 *   traditions). The constraint exhibits a complex structure: it provides
 *   genuine coordination functions (pooling working-class electoral power,
 *   creating legal frameworks for collective organizing) while simultaneously
 *   extracting via institutional channeling (suppressing revolutionary
 *   militants as 'adventurist,' limiting tactical repertoire to those
 *   permitted by democratic procedure, structuring working-class
 *   participation through reformist party apparatus). The
 *   democratic-gradualist method requires suppression of alternative
 *   organizing forms as the cost of its own institutional stability.
 *   Extractiveness remains moderate (0.40) because the coordination benefits
 *   to working-class participants are real, even if structurally constrained.
 *   Theater_ratio rising over the interval (0.35 → 0.58) reflects increasing
 *   performative content as the constraint's empirical warrant (whether
 *   working-class power actually accumulates) has been continuously
 *   challenged in practice (1976 Swedish defeat, 1981-1986 Mitterrand
 *   austerity turn, post-Eurocommmunism collapse, neoliberal institutional
 *   capture).
 *
 * KEY AGENTS:
 *   - Social Democratic Leadership: Institutional beneficiary (institutional/arbitrage) — gains governmental positions, legislative negotiating power, party apparatus resources from electoral-majoritarian structure
 *   - Established Trade Unions: Institutional beneficiary (institutional/arbitrage) — achieves legal recognition, protected negotiating status, wage/benefit gains through corporatist channels dependent on gradual institutional reform
 *   - Revolutionary Militants: Primary victim (powerless/trapped) — systematically suppressed as 'adventurist,' 'ultra-left,' or 'undemocratic' for advocating extra-institutional organizing; trapped within the democratic framework that forecloses their strategic method
 *   - Working-Class Base: Constrained agent (moderate/constrained) — experiences genuine coordination benefits (franchise, collective power) alongside asymmetric extraction (channeling toward reformism, leadership co-optation risk, structural limits on transformative scope)
 *   - Democratic Socialist Reformist Current: Organized agents (organized/constrained) — articulates scaffold vision of gradual transition toward socialism via legal-institutional reform; sees structure as having sunset (wealth redistribution, worker control experiments eventually superseding capitalist relations)
 *   - Cold War Liberal Democracy Consensus: Institutional sedimentation (institutional/arbitrage) — maintains democratic-gradualist method as 'only legitimate path' through post-WWII consensus; functions as piton (inertial maintenance despite contested empirical warrant)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.5).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic Gradualism as Revolutionary Method (Marxist Reading)").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political_philosophy/historical_materialism/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, 'c8106191-cc71-40c3-8736-04ac96eb2bfd').
narrative_ontology:cs_kernel_codification('c8106191-cc71-40c3-8736-04ac96eb2bfd', formalized).
narrative_ontology:cs_authority_grounding('c8106191-cc71-40c3-8736-04ac96eb2bfd', extraction).
narrative_ontology:cs_interpretation_layer_present('c8106191-cc71-40c3-8736-04ac96eb2bfd').
narrative_ontology:cs_reading_relation('c8106191-cc71-40c3-8736-04ac96eb2bfd', manifesto_revolutionary_method__vanguard_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8106191-cc71-40c3-8736-04ac96eb2bfd', manifesto_revolutionary_method__council_communist_reading, coexists_with).
narrative_ontology:cs_axiom('c8106191-cc71-40c3-8736-04ac96eb2bfd', foundational, working_class_electoral_power_accumulation).
narrative_ontology:cs_axiom_status(working_class_electoral_power_accumulation, holdable).
narrative_ontology:cs_axiom_grounding('c8106191-cc71-40c3-8736-04ac96eb2bfd', working_class_electoral_power_accumulation, empirically_contingent).
narrative_ontology:cs_axiom('c8106191-cc71-40c3-8736-04ac96eb2bfd', foundational, state_apparatus_convertibility_to_socialist_use).
narrative_ontology:cs_axiom_status(state_apparatus_convertibility_to_socialist_use, holdable).
narrative_ontology:cs_axiom_grounding('c8106191-cc71-40c3-8736-04ac96eb2bfd', state_apparatus_convertibility_to_socialist_use, empirically_contingent).
narrative_ontology:cs_reference_frame('c8106191-cc71-40c3-8736-04ac96eb2bfd', working_class_electoral_accumulation_framework).
narrative_ontology:cs_drift_state('c8106191-cc71-40c3-8736-04ac96eb2bfd', contemporary_neoliberal_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c8106191-cc71-40c3-8736-04ac96eb2bfd', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, established_trade_unions).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, institutionalized_labor_movements).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, extra_institutional_organizing).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, spontaneous_mass_movements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REVOLUTIONARY MILITANT (SNARE) — Faces systematic suppression as 'adventurist' or 'ultra-left' within the democratic gradualist frame. Cannot exit the constraint without abandoning revolutionary commitment entirely. The democratic-electoral structure channels mass energy into reformist parties, then suppresses militant forms of organizing as violations of democratic procedure. Maximum experienced extraction — the militant is trapped within the very democratic framework that forecloses their method.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__democratic_gradualism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKING-CLASS BASE (TANGLED ROPE) — Genuine coordination function: democratic majoritarianism pools working-class electoral power and provides legal/institutional frameworks for union organizing. Asymmetric extraction: leadership cadres benefit from institutional positions; rank-and-file bear risk of co-optation into reformism. The constraint provides real power (franchise, collective bargaining) while canalizing that power away from rupture. Constrained exit — workers can withdraw participation but face material dependency on wages/benefits won through institutional channels.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__democratic_gradualism_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SOCIAL DEMOCRATIC LEADERSHIP (ROPE) — Experiences democratic gradualism as coordination mechanism for mobilizing working-class voters and preventing competitive fragmentation of the left. Net beneficiary: institutional positions, government access, negotiating power with capital. The constraint coordinates mass mobilization without requiring rupture; leadership has arbitrage options (coalition-building, parliamentary maneuver, sector-specific negotiation). Effective extraction runs toward institutional leadership, not away.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__democratic_gradualism_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ESTABLISHED TRADE UNIONS (ROPE) — Coordination function: binding workers into representative structures that negotiate with employers and parties for wages, benefits, working conditions. Beneficiary: secure institutional existence, protected negotiating status, legal recognition. The constraint coordinates sectoral interests without requiring confrontation with state apparatus. Union leadership has arbitrage options and can modulate between aggressive and conciliatory strategies. Low effective extraction — the constraint serves the unions' organizational interests directly.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__democratic_gradualism_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DEMOCRATIC SOCIALIST REFORMIST CURRENT (SCAFFOLD) — Organized actors (democratic socialists, Eurocommunism, Nordic social democracy) see democratic gradualism as a transitional structure with built-in sunset: as working-class power accumulates through electoral and institutional channels, the constraint can be gradually extended toward socialist transformation through legal-institutional reform. Beneficiaries with constrained but non-zero exit: can articulate an exit path (wealth redistribution, worker control experiments, public ownership) within existing democratic structures. Theater is moderate — some performative justification required, but the reformist vision is substantive.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__democratic_gradualism_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: COLD WAR LIBERAL DEMOCRACY CONSENSUS (PITON) — The democratic-gradualist method itself has become institutionalized as 'the only legitimate path to socialism' within post-WWII social democracy, particularly in the Atlantic bloc. This institutional sediment persists despite the theoretical premise being contested (particularly post-1968, post-Eurocommmunism collapse). High theater_ratio: the formula is maintained through consensus even as its empirical warrant (whether working-class power actually accumulates through existing democratic structures without rupture) has been continuously challenged. The piton persists through inertia and institutional lock-in, not because contemporary practice validates the original claim.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__democratic_gradualism_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL REALIST (TANGLED ROPE) — From a civilizational/global scope, democratic-gradualist socialism exhibits both genuine coordination and asymmetric extraction. Coordination: it pools working-class electoral power and creates legal frameworks for collective organizing without requiring immediate rupture with state apparatus (reducing risk of state violence during transition). Extraction: it institutionalizes a two-step process (electoral victory, then legislative reform) that requires capital to voluntarily accept expropriation within democratic-procedural boundaries — a structural asymmetry favoring those already possessing institutional power and capacity to influence media/legal interpretation. The constraint is neither pure coordination (rope) nor pure extraction (snare), but a hybrid where the coordination function and the extraction mechanism are structurally interdependent.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__democratic_gradualism_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(manifesto_revolutionary_method__democratic_gradualism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(manifesto_revolutionary_method__democratic_gradualism_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, TR),
    TR >= 0.70.

:- end_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.40): Moderate, justified by the constraint's hybrid character. The democratic-majoritarian structure provides genuine coordination (pools electoral power, creates legal frameworks for collective bargaining). Working-class participants gain real benefits (franchise, union rights, wage advances where parties win office). However, the structure also channels and constrains working-class power in ways that prevent or delay transformation of property relations. The extractiveness reflects this asymmetry: the constraint delivers immediate reformist gains while structurally limiting revolutionary transformation. Suppression (0.50): Moderate-high. The democratic-gradualist frame suppresses alternative organizing through delegitimation ('adventurism,' 'undemocratic') and institutional foreclosure (prioritizing legal tactics, fragmenting movements that exceed democratic procedure). However, suppression is not total — militant organizing persists, and legal/protected space for unions exists (though shrinking). Theater_ratio (0.58): Moderate-high and rising. Early in the constraint's history (Kautsky, early 20th century), the gradualist thesis had substantial empirical warrant — European social democracy was accumulating legislative power. By mid-20th century (measurement at t=20), empirical warrant was contested (postwar defeats, Eurocommmunism collapse). By contemporary moment (t=40), theater has increased significantly — the democratic-gradualist method persists in institutional practice and party rhetoric despite repeated failures to produce working-class socialist transformation (1976 Sweden, 1981-86 Mitterrand, neoliberal capture). The rising theater reflects institutionalization of the method as consensual 'only legitimate path' even as its empirical validity erodes.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. The social-democratic leadership sees rope (genuine coordination for electoral mobilization). The revolutionary militant sees snare (suppression of their strategic method within the democratic framework). The working-class base sees tangled_rope (real gains coupled with structural constraints). The reformist current sees scaffold (a transitional structure with built-in sunset). The Cold War consensus sees piton (inertial maintenance). The analytical observer (structural realist) sees tangled_rope (genuine coordination + asymmetric extraction structurally interdependent). The largest gap: snare (militant perspective) vs. rope (leadership perspective) — same structural phenomenon, radically different experienced extraction based on positioning.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from power level + exit options + beneficiary/victim status. Social-democratic leaders: institutional power + arbitrage exit + beneficiary status → low d → low/negative f(d) → low experienced extraction. Revolutionary militants: powerless + trapped + victim status → high d → high f(d) → high experienced extraction (snare). Working-class base: moderate power + constrained exit + victim status (regarding channeling into reformism) + beneficiary status (regarding actual wage/union gains) → mixed d → medium f(d) → tangled_rope. The perspectival gap is not measurement error but structural reality: different positioning produces genuinely different experienced constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT resolve mandatrophy in the classical sense (all perspectives converging on one type). Instead, it instantiates THE CORE MANDATROPHY: whether the democratic-gradualist path is actually a viable working-class strategy (rope/scaffold classification appropriate) or a constraint that channelizes and ultimately neutralizes working-class power (snare classification appropriate). The perspectives reveal this as a strategic contest, not a measurement ambiguity. The revolutionary militant and social-democratic leader genuinely experience different constraints from the same structural phenomenon. The analytical observer sees both coordination AND extraction operating together. Resolving the mandatrophy requires empirical investigation of the five omegas — particularly omega_empirical_gradualism_hypothesis (does working-class power actually accumulate through electoral channels?) and omega_counter_revolutionary_institutional_response (are democratic transitions systematically blocked by institutional veto?). Until those empirical questions are resolved, the classification remains genuinely indeterminate — a tangled_rope, not a snare, not a rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_gradualism_hypothesis,
    'Does working-class power actually accumulate through repeated electoral victories, or does institutional participation structurally limit transformative capacity?',
    'Historical comparative analysis: post-WWII European social democracy trajectories (Sweden 1976-1982 defeats, Mitterrand 1981-1986 turn to austerity), Allende Chile 1970-1973 military coup, Vietnam''s reunification mechanics. Measurement of gap between electoral mandate and implementable policy scope. Analysis of capital-strike capacity, monetary policy constraints (Delors rules, ECB), and institutional veto points.',
    'If empirically validated: gradualism moves toward rope classification (genuine accumulation mechanism). If empirically falsified: snare classification strengthens — the constraint channels working-class power into institutional forms that ensure its own defeat. Determines whether scaffold''s sunset is real or aspirational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_gradualism_hypothesis, empirical, 'Whether electoral socialism accumulates power or structurally degrades working-class capacity').

omega_variable(
    institutional_transformation_scope,
    'Can a capitalist state apparatus be converted to socialist use through democratic-legal means, or does state form enforce capital-reproduction requirements that procedural democracy cannot overcome?',
    'Structural Marxist analysis of state apparatuses (Poulantzas, Jessop): examination of whether legal-institutional reform can dissolve the state''s relative autonomy and structural dependence on capital accumulation. Historical cases: USSR achieved via rupture, Cuba via rupture, Yugoslavia via rupture, Allende via gradualism (failed). Theoretical investigation of whether the social base of existing state institutions can be fundamentally altered without simultaneous organizational rupture.',
    'If state form is fundamentally transformable within democratic-procedural constraints: gradualism is strategically viable, scholarship moves toward rope/scaffold. If state requires rupture for socialist transformation: gradualism is necessarily co-optive, snare classification strengthens. This omega addresses whether the reading''s core strategic premise is viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_transformation_scope, conceptual, 'Whether state apparatus can be transformed to socialist use without rupture').

omega_variable(
    counter_revolutionary_institutional_response,
    'When working-class parties approach power through democratic channels, do state institutions (military, judiciary, capital) automatically mobilize counter-revolutionary action, or is democratic transition genuinely possible?',
    'Historical pattern analysis: Chile 1973, Greece 1967, Portugal 1974-1975 transition, Poland 1981, Ecuador 2000, Bangladesh 2013, Thailand coups. Distinction between cases where democratic election led to stabilized transformation (Portugal, Grenada, Nicaragua) vs. immediate military/institutional coup response (Chile, Allende''s loss within 3 years). Identification of conditions under which democratic transition is permitted vs. foreclosed by institutional actors.',
    'If counter-revolutionary institutional response is contingent (depends on balance of forces, international context): gradualism remains strategic option. If automatic: democratic-gradualist path is fundamentally constrained by institutional veto, snare classification strengthens. Addresses whether democratic electoralism is actually available as a strategy or foreclosed by structural forces.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counter_revolutionary_institutional_response, empirical, 'Whether democratic socialist elections trigger counter-revolutionary institutional response').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Is the democratic-gradualist reading a genuine strategy for working-class power accumulation, or is it a legitimation narrative constructed by parties that benefit from channeling working-class energy into reformism?',
    'Analysis of the reading''s historical emergence: Who authored the democratic-gradualist thesis (Kautsky, Austro-Marxism, Eurocommunism)? In what political contexts? Did it emerge from working-class practice, or was it imposed as intellectual framework by party intellectuals? Examination of whether the reading''s institutional beneficiaries (social democratic parties, unions) benefit from its promulgation regardless of its empirical validity. Investigation of whether the constraint naturalizes party interest as working-class interest.',
    'If the reading is a genuine strategic discovery from working-class practice: it merits serious consideration despite empirical weaknesses. If it is a legitimation narrative constructed by institutional beneficiaries: the democratic-gradualist constraint is more accurately classified as an extraction mechanism (higher snare probability). This is the committer omega — it addresses whether the reading itself is truth-seeking or interest-serving.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Whether democratic-gradualism is a working-class strategy or a party-interest legitimation').

omega_variable(
    revolutionary_alternative_viability,
    'Are vanguard-party and council-communist alternatives empirically more viable at producing working-class socialism than democratic gradualism?',
    'Comparative historical analysis: revolutionary insurrectionary path (USSR, China, Cuba, Vietnam, Grenada) vs. democratic-electoral path (Allende, Nicaragua partial, Ecuador partial) vs. council/spontaneous path (Paris Commune, Russia 1917 soviets before Bolshevization, Yugoslavia self-management). Measurement of outcomes: durability, working-class participation in post-revolutionary governance, ability to withstand counter-revolutionary pressure, degree of bureaucratization, relationship between vanguard structure and working-class power.',
    'If vanguard/council approaches are more viable: the democratic-gradualist reading appears as a constraint that suppresses more-effective alternatives, snare classification. If democratic-gradualist approach is most viable: readings of alternative strategies appear as constraints that risk catastrophic state violence and working-class defeat. This omega directly addresses the strategic competition between the three sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revolutionary_alternative_viability, empirical, 'Comparative viability of revolutionary alternatives to democratic gradualism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dem_grad_tr_t0, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dem_grad_tr_t20, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(dem_grad_tr_t40, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(dem_grad_be_t0, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(dem_grad_be_t20, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(dem_grad_be_t40, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 40, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(dem_grad_su_t0, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(dem_grad_su_t20, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(dem_grad_su_t40, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 40, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, resource_allocation).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__council_communist_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, state_apparatus_transformation_feasibility).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate_cannalization).

% DUAL FORMULATION NOTE:
% This constraint is part of a constraint family decomposing the kernel 'manifesto_revolutionary_method' into three strategically distinct readings. Each reading has its own ε, perspectives, and empirical warrant. Democratic gradualism (this file) ε=0.40, tangled_rope. Vanguard rupture (sibling file) ε=0.65, snare-or-tangled_rope (depending on outcome analysis). Council communist (sibling file) ε=0.55, rope-or-tangled_rope. The three are linked via affects_constraints: democratic gradualism affects and is affected by both siblings because they represent competing strategic claims about the same working-class power question. The network decomposition prevents false averaging across incommensurable strategic claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__democratic_gradualism_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
