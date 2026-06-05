% ============================================================================
% CONSTRAINT STORY: roman_bath_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_bath_system, []).

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
 *   constraint_id: roman_bath_system
 *   human_readable: The Roman System of Public Baths
 *   domain: technological/social/infrastructure
 *
 * SUMMARY:
 *   The Roman public bath system (thermae) represents one of history's most
 *   extensive state-sponsored infrastructure networks, serving urban
 *   populations across the Mediterranean, Britain, and beyond from roughly
 *   the 2nd century BCE through the 5th century CE. The system provided
 *   bathing, heating, recreation, and social gathering at minimal cost to
 *   users, subsidized by imperial taxation and private benefaction. From the
 *   primary perspective, this appears as a pure coordination mechanism: it
 *   solved the collective hygiene problem at scale, enabled mixed-class civic
 *   participation, and distributed maintenance labor through a stable
 *   institutional framework. However, the system's foundation in enslaved
 *   labor, its construction through extractive taxation, and its increasing
 *   theatrical function (social display and elite legitimation) over time
 *   reveal it as more complex than a simple rope. The constraint exhibits all
 *   characteristics of coordination (low base extractiveness, low
 *   suppression, low theater) but is layered with secondary extraction
 *   mechanisms that are obscured by the public benefit narrative.
 *
 * KEY AGENTS:
 *   - Urban lower classes (powerless/mobile): Primary beneficiaries of free/cheap access to hygiene and social spaces; experience constraint as pure coordination; ability to visit multiple baths provides exit options
 *   - Imperial administration (institutional/arbitrage): Designer and enforcer of system; benefits through social stability and civic loyalty; controls subsidy levels and can withdraw at will
 *   - Bath attendants and water workers (moderate/mobile): Gain employment and income from system maintenance; modest extraction through wage suppression; can exit to other trades
 *   - Enslaved workforce (organized/constrained): Performs labor maintaining system; constrained exit options; experiences tangled_rope: forced labor plus minimal access benefit
 *   - Wealthy citizens (powerful/arbitrage): Use baths for social status and networking; can afford private alternatives; experience minimal extraction
 *   - Analytical observer (analytical/analytical): Sees system as primarily coordinative with secondary extraction components; risks naturalizing social stratification as inevitable feature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_bath_system, 0.35).
domain_priors:suppression_score(roman_bath_system, 0.25).
domain_priors:theater_ratio(roman_bath_system, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_bath_system, extractiveness, 0.35).
narrative_ontology:constraint_metric(roman_bath_system, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(roman_bath_system, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_bath_system, rope).
narrative_ontology:human_readable(roman_bath_system, "The Roman System of Public Baths").
narrative_ontology:topic_domain(roman_bath_system, "technological/social/infrastructure").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_bath_system, urban_lower_classes).
narrative_ontology:constraint_beneficiary(roman_bath_system, enslaved_workforce).
narrative_ontology:constraint_beneficiary(roman_bath_system, imperial_administration).
narrative_ontology:constraint_beneficiary(roman_bath_system, merchants_and_craftspeople).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: URBAN LABORER (ROPE) — Enjoys free or near-free access to bathing, hygiene, and social gathering. Exit options are mobile: can visit any of multiple public baths in most cities; can also use public aqueducts or rivers. Experiences the constraint as pure coordination: shared infrastructure solves the collective hygiene problem without coercion. d≈0.35, f(d)≈0.25, σ=0.8 → χ≈0.07. Low effective extraction.
constraint_indexing:constraint_classification(roman_bath_system, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: IMPERIAL ADMINISTRATION (ROPE) — Benefits from bath system as a coordination mechanism: solves urban hygiene collectively, reduces disease, enables labor mobility, and generates civic loyalty through provision of public amenities. Exit options are arbitrage: can withdraw or modify the system at will; can scale up or down based on political needs. Experiences extraction costs as acceptable overhead for social stability. d≈0.15, f(d)≈0.01, σ=1.1 → χ≈0.00. Net beneficiary; no effective extraction.
constraint_indexing:constraint_classification(roman_bath_system, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: BATH ATTENDANT & WATER SERVICE WORKER (ROPE) — Gains employment from the bath system as maintenance staff, attendants, and water-infrastructure workers. Mobile exit: can seek work in other trades or services, though specialized bath knowledge has value. Experiences the system as coordination with modest extraction for labor: wages are modest but stable employment is provided by the state or contractors. d≈0.45, f(d)≈0.55, σ=0.9 → χ≈0.17. Modest effective extraction; primarily coordination.
constraint_indexing:constraint_classification(roman_bath_system, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: ENSLAVED WORKFORCE (TANGLED ROPE) — Performs much of the physical labor maintaining the bath system (heating, water pumping, cleaning, repairs) and may have limited access to bathing facilities. Constrained exit: enslaved status reduces mobility options severely. Experiences both coordination benefit (hygiene when allowed access) and extraction (labor coercion). The system actively enforces slavery structures while providing minimal reciprocal benefit. d≈0.88, f(d)≈1.30, σ=0.9 → χ≈0.41. Significant asymmetric extraction masked by coordination rhetoric.
constraint_indexing:constraint_classification(roman_bath_system, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: WEALTHY CITIZEN (ROPE) — Uses public baths as social venues and civic participation, but often has private bathing facilities at home. Exit options are strong arbitrage: can choose between public and private, can afford alternative hygiene solutions. Experiences public baths as a coordination mechanism enabling mixed-class socialization and civic display. d≈0.20, f(d)≈0.05, σ=0.8 → χ≈0.01. Minimal extraction; strongly beneficiary.
constraint_indexing:constraint_classification(roman_bath_system, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE-DOMINANT VIEW) — The Roman bath system is a genuine coordination mechanism: it solves the collective hygiene problem at scale, enables cross-class social integration, distributes maintenance labor through a stable institutional framework, and generates civic legitimacy. The system exhibits low extractiveness (0.35), low suppression (0.25), and low theater (0.38), consistent with pure coordination. The primary tension is that the system is built partly on enslaved labor extraction, which the rope classification does not fully capture from enslaved perspectives. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.23. Moderate classification from balanced perspective; reveals need for perspectival decomposition.
constraint_indexing:constraint_classification(roman_bath_system, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_bath_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_bath_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_bath_system, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(roman_bath_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The system combines genuine public coordination (hygiene, labor distribution) with subtle extraction mechanisms. The primary extraction is the hidden labor cost: enslaved workers and low-wage service workers subsidize the system through suppressed compensation. Secondary extraction occurs through taxation: all citizens pay into a system whose benefits are distributed unequally (wealthy have private alternatives). The value is moderate rather than low because the benefit distribution is genuinely broad, and the coordination problem solved (urban hygiene at scale) is substantial. Suppression (0.25): Low-moderate. No strong coercion is applied to bath *users* — participation is voluntary and access is relatively open. Suppression operates primarily on *workers* (enslaved and wage labor) who maintain the system. For users, alternatives exist (rivers, private facilities, aqueducts), so the constraint is not highly suppressive. Theater ratio (0.38): Moderate-low. The bath system has genuine functional content (hygiene, heat distribution, labor organization) but includes significant theatrical elements: elite use of baths for status display, imperial propaganda about public provision, and performative aspects of bathing rituals. Over the imperial period, the theater ratio rises from 0.25 to 0.38 as the system becomes more about civic identity and less about raw hygiene (aqueduct improvements reduce hygiene necessity; elaborate baths become status markers).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is substantial and follows extraction axes. The urban laborer sees pure coordination (rope): free access, multiple alternatives, solves a real problem. The imperial administration also sees rope: coordination mechanism for social stability. The analytical observer sees rope-dominant: low extractiveness metrics support this. However, the enslaved workforce sees tangled_rope: forced labor extraction masked by coordination narrative. The wealthy citizen sees rope: they enjoy the social benefit with arbitrage option. The bath worker occupies intermediate position: modest extraction through wage suppression, but genuine employment benefit. The key tension is that 'public benefit' narratives obscure the extraction experienced by laborers (especially enslaved). The system is genuinely coordinative (it solves hygiene), but the coordination is built on and maintained through extraction from workers. This is a classic tangled_rope pattern: genuine coordination function + asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Urban lower classes: Primary beneficiary + mobile → d≈0.35, f(d)≈0.25. Low directionality; net beneficiary. Imperial administration: Beneficiary + arbitrage → d≈0.15, f(d)≈0.01. Minimal directionality; strong beneficiary position. Bath workers: Victim of wage suppression + mobile → d≈0.45, f(d)≈0.55. Moderate directionality; extraction through wage suppression, but benefit from stable employment and access. Enslaved workforce: Victim of forced labor + constrained → d≈0.88, f(d)≈1.30. High directionality; forced labor extraction is the primary structure. Wealthy citizens: Beneficiary + arbitrage → d≈0.20, f(d)≈0.05. Very low directionality; strong beneficiary with exit options. Analytical observer: Balanced → d≈0.50, f(d)≈0.65. Moderate directionality; sees both coordination and extraction; perspective captures the system's hybridity.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in the Roman bath system is the tension between 'public infrastructure = pure coordination' and 'public infrastructure = extractive apparatus.' The rope classification is correct for users (lower classes, wealthy): the system genuinely solves the collective hygiene problem and distributes benefits broadly. The tangled_rope classification is correct for workers (enslaved and wage laborers): the system requires extraction of labor to function, and that extraction is masked by the coordination narrative. The system is not a false rope (doesn't hide pure extraction) — the coordination is real. But it is not a pure rope (doesn't lack asymmetric extraction). The tangled_rope classification resolves the mandatrophy by acknowledging that the system simultaneously performs coordination (hygiene) and extraction (labor). The theater_ratio (0.38) is low enough that the system is not primarily theatrical, but high enough to indicate that elite legitimation and status display are significant secondary functions. The system's extractiveness rising from 0.20 to 0.35 over 100 years indicates that as hygiene improvements reduce the functional necessity of baths, the system becomes more about social coordination and status, increasing the ratio of extraction (labor cost) to genuine hygiene benefit. This is consistent with piton dynamics: original function (hygiene) is partly superseded by alternative infrastructure (aqueducts), leaving the system sustained by institutional inertia and elite benefit, with theater rising.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    slave_labor_degree,
    'What proportion of bath system maintenance labor came from enslaved versus free/wage workers, and did this ratio change over the imperial period?',
    'Epigraphic evidence (inscriptions of bath workers), papyri records, historical census data; comparison across regions and time periods',
    'High slave proportion (>50%): tangled_rope classification dominates; system is extraction masked by public coordination narrative. Low slave proportion (<20%): rope classification is robust; extraction component is secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slave_labor_degree, empirical, 'Proportion of enslaved labor in bath system maintenance').

omega_variable(
    cost_distribution_subsidy,
    'How were bath operating costs distributed across tax revenue, user fees, and private benefactor funding? Did the distribution reflect true public benefit or hidden extraction?',
    'Analysis of municipal tax records, benefactor inscriptions, Roman financial texts (Frontinus, etc.); cost accounting of fuel (wood), water pumping, maintenance; comparison of subsidy levels across cities and time periods',
    'If heavily tax-subsidized with low user fees: rope classification confirmed (public coordination). If high user fees on lower classes: tangled_rope (coordination with extraction). If private benefactors absorbed costs: rope with patron-client extraction (secondary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_distribution_subsidy, empirical, 'Cost distribution and subsidy sources for Roman baths').

omega_variable(
    disease_prevention_efficacy,
    'How much of Roman urban health improvements in the imperial period were causally attributable to public baths versus improved sanitation, aqueducts, food supply, or other factors?',
    'Paleontological and skeletal evidence of disease prevalence; comparative analysis of cities with/without extensive bath systems; epidemiological modeling; historical records of plague, cholera, mortality rates',
    'High causal attribution: baths function as critical coordination mechanism; rope classification is robust. Low attribution: baths are partly theater (social/elite function); theater_ratio rises, piton perspective becomes viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disease_prevention_efficacy, empirical, 'Causal impact of baths on urban public health').

omega_variable(
    access_equity_reality,
    'Were Roman public baths genuinely accessible to all social classes or were practical barriers (cost, time, gender, ethnic, or class discrimination) substantial?',
    'Historical sources on bath admission policies; economic analysis of opportunity cost of time for lower-income workers; gender access restrictions; spatial distribution of baths and population density; comparative access data across cities',
    'High genuine access: rope classification robust across all perspectives. High practical barriers: tangled_rope or scaffold (temporary access) from lower-class perspective; reveals snare-like dynamics for excluded groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(access_equity_reality, empirical, 'Actual accessibility of baths to lower classes and marginalized groups').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_bath_system, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bath_tr_t0, roman_bath_system, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bath_tr_t50, roman_bath_system, theater_ratio, 50, 0.35).
narrative_ontology:measurement(bath_tr_t100, roman_bath_system, theater_ratio, 100, 0.38).

% Extraction over time
narrative_ontology:measurement(bath_be_t0, roman_bath_system, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(bath_be_t50, roman_bath_system, base_extractiveness, 50, 0.3).
narrative_ontology:measurement(bath_be_t100, roman_bath_system, base_extractiveness, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_bath_system, resource_allocation).
narrative_ontology:affects_constraint(roman_bath_system, roman_aqueduct_system).
narrative_ontology:affects_constraint(roman_bath_system, imperial_taxation_structure).
narrative_ontology:affects_constraint(roman_bath_system, slavery_institution_rome).

% DUAL FORMULATION NOTE:
% The Roman bath system decomposition: The public access/hygiene coordination function (rope) is structurally distinct from the labor extraction mechanism (tangled_rope/snare from worker perspective). The unified constraint story captures both, but alternative decomposition would separate 'bath hygiene coordination' (ε≈0.15, rope) from 'bath labor extraction' (ε≈0.55, snare). The unified story's ε≈0.35 reflects the hybrid. Network edges: aqueduct system provides upstream water infrastructure (affects baths); taxation structure provides funding mechanism (affects baths); slavery institution provides labor supply (affects baths). Baths affect social stability, disease prevention, and labor mobility downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(roman_bath_system, organized, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
