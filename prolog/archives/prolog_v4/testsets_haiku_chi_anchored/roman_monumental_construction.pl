% ============================================================================
% CONSTRAINT STORY: roman_monumental_construction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_monumental_construction, []).

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
 *   constraint_id: roman_monumental_construction
 *   human_readable: The Roman State's Monopoly on Opus Caementicium Construction
 *   domain: socio_technological/political_economy
 *
 * SUMMARY:
 *   Roman monumental construction via opus caementicium (lime-based concrete)
 *   represents a sophisticated socio-technical constraint that combines
 *   genuine coordination benefits with systematic political extraction. The
 *   imperial state maintained a de facto monopoly on large-scale concrete
 *   production and use for monumental architecture, controlling material
 *   supply, contractor licensing, and technical knowledge dissemination
 *   across the provinces. This constraint operated at multiple structural
 *   levels: technical (concrete superiority for durability), economic
 *   (pricing control and material logistics), political (control of
 *   infrastructure investment), and epistemological (monopoly on construction
 *   knowledge). The constraint extracted significant wealth from provincial
 *   elites and municipalities while simultaneously delivering coordination
 *   benefits through standardized infrastructure and rapid deployment. As
 *   imperial enforcement capacity declined after 250 CE, the constraint
 *   degraded from active extraction mechanism to theatrical persistence
 *   (piton), with regional builders increasingly deploying alternative
 *   technologies and informal knowledge transmission undermining the
 *   monopoly.
 *
 * KEY AGENTS:
 *   - Imperial Administration: Primary beneficiary (institutional/arbitrage) — controls material supply, contractor licensing, and public investment allocation; captures pricing rents and political control benefits
 *   - State-Approved Contractors: Secondary beneficiary (powerful/mobile) — monopoly on approved construction; benefits from restricted competition and guaranteed material supply; also subject to quota obligations and pricing controls
 *   - Provincial Municipalities: Primary victim (powerless/trapped) — forced to use approved contractors and materials; cannot exit; bear extraction costs through taxation and construction markup pricing
 *   - Regional Elites: Secondary victim (moderate/constrained) — wealthy but constrained by monopoly; extracted through taxation supporting state infrastructure and from being unable to fund competing regional projects
 *   - Regional Economies: Tertiary victim (powerless/trapped) — local building trades suppressed; economic development diverted to state projects; alternative construction methods actively discouraged
 *   - Opus Caementicium Technical Guild: Institutional actor (institutional/arbitrage) — maintains knowledge monopoly with state support; piton perspective reflects degrading function over the interval
 *   - Coalition of Provincial Builders: Organized resistance (organized/constrained) — informal knowledge sharing and alternative techniques represent emerging scaffold with sunset logic as empire declines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_monumental_construction, 0.58).
domain_priors:suppression_score(roman_monumental_construction, 0.68).
domain_priors:theater_ratio(roman_monumental_construction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_monumental_construction, extractiveness, 0.58).
narrative_ontology:constraint_metric(roman_monumental_construction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(roman_monumental_construction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_monumental_construction, snare).
narrative_ontology:human_readable(roman_monumental_construction, "The Roman State's Monopoly on Opus Caementicium Construction").
narrative_ontology:topic_domain(roman_monumental_construction, "socio_technological/political_economy").

domain_priors:requires_active_enforcement(roman_monumental_construction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_monumental_construction, imperial_administration).
narrative_ontology:constraint_beneficiary(roman_monumental_construction, state_contractors).
narrative_ontology:constraint_victim(roman_monumental_construction, provincial_elites).
narrative_ontology:constraint_victim(roman_monumental_construction, municipal_authorities).
narrative_ontology:constraint_victim(roman_monumental_construction, regional_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROVINCIAL MUNICIPALITY (SNARE) — Cannot exit the constraint. Regional authority must commission construction through imperial-approved contractors using state-controlled opus caementicium supply chains. No alternative technology for monumental durability exists at comparable scale. Cost extraction occurs via material pricing, contractor licensing, and mandatory use of imperial logistics. d≈0.92, f(d)≈1.38, σ=1.1 → χ≈0.88.
constraint_indexing:constraint_classification(roman_monumental_construction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: REGIONAL ELITE LANDOWNER (SNARE) — Constrained by state monopoly on monumental materials and approval process. Cannot build competing infrastructure. Extraction occurs through taxation funding state projects that benefit imperial control infrastructure (roads, administrative centers, garrison facilities) rather than local economic development. d≈0.78, f(d)≈1.12, σ=1.0 → χ≈0.65.
constraint_indexing:constraint_classification(roman_monumental_construction, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: IMPERIAL ADMINISTRATION (ROPE) — Monopoly holder benefits from state contractor system and material pricing control. Experiences the constraint as coordination: opus caementicium standardization enables rapid deployment of infrastructure across provinces. Arbitrage exit available — can reallocate resources, adjust pricing, or redirect projects. d≈0.08, f(d)≈-0.11, σ=1.1 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(roman_monumental_construction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STATE-APPROVED CONTRACTOR (TANGLED ROPE) — Benefits from monopoly on approved construction (coordination function: reliable material supply, standardized specifications, economies of scale). Also subject to state extraction (pricing controls, forced participation in public projects, quota obligations). Mobile exit available — could relocate, seek imperial favor, or vertical integrate into material production. d≈0.48, f(d)≈0.62, σ=1.0 → χ≈0.36.
constraint_indexing:constraint_classification(roman_monumental_construction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: OPUS CAEMENTICIUM TECHNICAL GUILD (PITON) — Maintains knowledge monopoly on cement and aggregate specifications, but this function is increasingly theatrical. By late Empire, many regional elites have learned formulations; the guild's enforcement decays. Theater ratio = 0.55 reflects that knowledge monopoly persists through institutional inertia and state support, not through irreplaceable technical skill. Guild sees its own role as degraded — maintained because the imperial system still requires it nominally, not because alternatives couldn't replace it.
constraint_indexing:constraint_classification(roman_monumental_construction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: COALITION OF PROVINCIAL BUILDERS (SCAFFOLD) — Organized resistance to monopoly (informal knowledge sharing, alternative stone-construction techniques, wooden-frame innovations) represents temporary constraint that will sunset as local expertise accumulates. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.41. Coalition has agency and sees paths to reduced dependence — but constraint persists during the interval due to state enforcement and material logistics advantages.
constraint_indexing:constraint_classification(roman_monumental_construction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / TECHNICAL NECESSITY VIEW (MOUNTAIN) — From universal perspective, Roman concrete's superior durability and load-bearing capacity creates an apparent natural law: monumental construction at scale requires opus caementicium. This perspective risks naturalizing what is actually a contingent monopoly. However, the structural data (ε=0.58, suppression=0.68, theater=0.55) contradicts mountain classification — the engine will compute false summit, revealing that 'technical superiority' naturalizes a political extraction mechanism.
constraint_indexing:constraint_classification(roman_monumental_construction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_monumental_construction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_monumental_construction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_monumental_construction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(roman_monumental_construction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(roman_monumental_construction, TR),
    TR >= 0.70.

:- end_tests(roman_monumental_construction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not extreme. The monopoly does extract significant value through material pricing, contractor licensing, and forced use of approved suppliers. However, the extraction is partially justified by genuine coordination benefits: standardized specifications enable infrastructure at unprecedented scale and durability. The v1.0 assessment (0.72) overestimated because it failed to credit the coordination function. Revised to 0.58 to reflect both extraction and legitimate coordination payoff. Suppression (0.68): High. Multiple barriers prevent alternative construction: (1) technical knowledge monopoly enforced through state control and guild restrictions; (2) material supply dominance through state quarries and distribution networks; (3) legal prohibitions on unauthorized monumental construction; (4) economic barriers — alternative stone or timber construction costs more labor and lacks durability guarantees; (5) social barriers — prestige attached to imperial-approved opus caementicium. Theater ratio (0.55): Moderate. The constraint combines genuine technical function (concrete superiority) with performative elements (imperial prestige, guild theater, unnecessary specifications). By 300 CE, the theater increases as enforcement declines but the ritual persists — exactly the piton signature.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival collapse (most perspectives see extraction) with one outlier (imperial administration sees coordination). The provincial municipality and regional elite both see pure snare — they bear costs, have no exit, and receive no direct benefit. The contractor sees hybrid (tangled rope) — benefits from monopoly but also constrained by it. The guild sees its function as degraded (piton). The organized resistance sees temporary constraint with sunset (scaffold). The analytical observer risks naturalizing the monopoly as technical necessity (false summit mountain). The perspectival gap reveals the underlying mandatrophy: the constraint bundles genuine coordination (standardized infrastructure) with systematic extraction (monopoly rent). The empirical question is whether the coordination benefits could be achieved without the extraction — if yes, it's a pure snare; if no, it's a tangled rope that honest agents disagree about.
 *
 * DIRECTIONALITY LOGIC:
 *   Imperial administration: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary with strong exit options (can adjust pricing, redirect projects, reallocate resources). Provincial municipality: Victim + trapped → d≈0.92, f(d)≈1.38. Maximal extraction — must use the system, no alternatives. Regional elite: Victim + constrained → d≈0.78, f(d)≈1.12. High extraction, some exit options (can commission less monumental structures, use alternatives for non-public projects). State contractor: Beneficiary + mobile → d≈0.48, f(d)≈0.62. Benefits from monopoly but can exit (seek imperial favor, vertically integrate, relocate). Guild: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11 (but piton gate applies independently). Coalition: Victim + constrained → d≈0.55, f(d)≈0.75. Organized resistance lowers d relative to unorganized victims.
 *
 * MANDATROPHY ANALYSIS:
 *   TENSION RESOLUTION: The constraint exhibits classic mandatrophy between snare and tangled rope. From the provincial victim perspective, it appears pure snare: extraction without coordination benefit (for them). From the imperial perspective, it appears coordination mechanism: standardized infrastructure enables state control and rapid deployment. The resolution lies in recognizing that both are structurally correct but observer-dependent. The imperial state genuinely benefits from coordination (roads, bridges, administrative centers). The provinces also genuinely benefit from durability and reduced maintenance costs. However, the extraction (pricing control, monopoly rent) exceeds the coordination benefits for victims — making it snare-classifying for them while rope-classifying for beneficiaries. The tangled rope classification emerges for the contractor (middle position). The omega variables on technical necessity and regional knowledge sufficiency determine whether the coordination benefits require the monopoly (snare with justification) or could be achieved with distributed knowledge (pure snare). Current evidence suggests the latter — the constraint persists partly because it works well and partly because extraction is profitable, making it a tangled rope that masquerades as technical necessity (false summit).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_superiority_necessity,
    'Is opus caementicium technically necessary for monumental durability, or does the monopoly naturalize a contingent material choice?',
    'Comparative analysis of stone-arch, wooden-frame, and terracotta construction longevity across provinces; evidence of successful non-concrete monumental projects; documentation of alternative material suppression',
    'If technically necessary: constraint has mountain properties despite high extraction (materials dictate politics). If contingent: extraction is pure political monopoly (pure snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_superiority_necessity, empirical, 'Whether opus caementicium is technically necessary or politically chosen').

omega_variable(
    regional_knowledge_sufficiency,
    'Did provincial builders possess sufficient knowledge to produce opus caementicium independently, or was the technical knowledge gap genuine?',
    'Paleographic and archaeological analysis of regional construction techniques; comparative material composition analysis of approved vs informal construction; documentation of imperial knowledge restrictions vs natural technical barriers',
    'If knowledge was accessible: suppression was purely political, snare classification confirmed. If knowledge was genuinely scarce: suppression reflects technical barrier, pushing toward tangled rope for some perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regional_knowledge_sufficiency, empirical, 'Whether regional knowledge barriers were technical or political').

omega_variable(
    economic_extraction_magnitude,
    'What fraction of provincial wealth extraction was attributable to opus caementicium monopoly pricing versus other imperial revenue mechanisms (taxation, conquest rent)?',
    'Economic historian analysis of construction cost inflation vs material cost baselines; comparison of labor-to-material price ratios in monopolized vs non-monopolized regions; impact accounting of public construction on provincial GDP',
    'If extraction is <20% of total imperial revenue: constraint is secondary snare. If >40%: major snare driving provincial impoverishment and rebellion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_extraction_magnitude, empirical, 'Magnitude of economic extraction through monopoly pricing').

omega_variable(
    late_empire_degradation_timeline,
    'At what point does the constraint transition from active snare to piton (theatrical monopoly)?',
    'Historical documentation of unauthorized construction, informal knowledge dissemination, imperial enforcement capacity decline; measurement of actual vs nominal monopoly compliance rates over time',
    'If transition occurs by 250 CE: scaffold perspective is realistic, piton perspective premature. If persists to 400 CE: piton is accurate, monopoly enforcement proves remarkably durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(late_empire_degradation_timeline, empirical, 'Timeline for constraint transition from active enforcement to theatrical persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_monumental_construction, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rmc_tr_t0, roman_monumental_construction, theater_ratio, 0, 0.25).
narrative_ontology:measurement(rmc_tr_t150, roman_monumental_construction, theater_ratio, 150, 0.42).
narrative_ontology:measurement(rmc_tr_t300, roman_monumental_construction, theater_ratio, 300, 0.55).

% Extraction over time
narrative_ontology:measurement(rmc_be_t0, roman_monumental_construction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rmc_be_t150, roman_monumental_construction, base_extractiveness, 150, 0.52).
narrative_ontology:measurement(rmc_be_t300, roman_monumental_construction, base_extractiveness, 300, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_monumental_construction, global_infrastructure).
narrative_ontology:affects_constraint(roman_monumental_construction, roman_provincial_taxation).
narrative_ontology:affects_constraint(roman_monumental_construction, roman_military_supply_chain).
narrative_ontology:affects_constraint(roman_monumental_construction, roman_regional_autonomy).

% DUAL FORMULATION NOTE:
% The opus caementicium monopoly is structurally distinct from but causally upstream of provincial taxation systems and military logistics. The material monopoly enables state extraction through infrastructure investment and pricing control. The military supply constraint (roads, fortifications, supply depots) depends on the monopoly's delivery infrastructure. Regional autonomy is constrained by forced participation in state construction projects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(roman_monumental_construction, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
