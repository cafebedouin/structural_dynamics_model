% ============================================================================
% CONSTRAINT STORY: afghan_labor_trafficking
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_afghan_labor_trafficking, []).

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
 *   constraint_id: afghan_labor_trafficking
 *   human_readable: Afghan Labor Trafficking Network
 *   domain: labor/human_rights/geopolitical
 *
 * SUMMARY:
 *   Afghan labor trafficking represents a structural extraction mechanism
 *   embedded in irregular migration corridors, operating across South and
 *   Central Asia to exploit Afghan workers fleeing conflict and poverty. The
 *   constraint exhibits classic snare characteristics: high suppression
 *   (confiscated documents, wage theft, deportation threats, family
 *   coercion), high extractiveness (labor value captured by trafficking
 *   networks and exploitative employers), and minimal coordination benefit to
 *   victims. The extractiveness has increased over the measurement interval
 *   (0.65 → 0.78) as trafficking networks have professionalized and expanded,
 *   while the theater ratio has remained low (0.22 → 0.35) because
 *   trafficking operates in fugitive space outside formal oversight. The
 *   constraint is downstream of Afghanistan's state collapse and regional
 *   geopolitical instability (refugee crises, labor market gaps in host
 *   countries) but is structurally autonomous — it persists and profits
 *   regardless of whether formal labor migration infrastructure exists. The
 *   perspectival gap reveals that the same constraint appears as a pure
 *   extraction mechanism (snare) from the trafficked worker's position, a
 *   mixed coordination-extraction mechanism (tangled rope) from the Afghan
 *   state's position (which benefits from remittances while failing to
 *   regulate trafficking), and an aspirational solvable problem (scaffold)
 *   from the international anti-trafficking coalition's position. The piton
 *   perspective reveals that international labor migration frameworks are
 *   substantially performative — conventions are signed and agencies exist,
 *   but enforcement capacity and political will are minimal.
 *
 * KEY AGENTS:
 *   - Trafficked Afghan Workers: Primary victims (powerless/trapped) — face debt bondage, document confiscation, wage theft, deportation threats, and physical abuse with no exit pathway
 *   - Families in Afghanistan: Secondary victims (moderate/identity_locked) — economically desperate but locked into belief that migration (even via trafficking) is necessary for survival
 *   - Afghan State: Ambiguous actor (organized/constrained) — genuine interest in remittances but insufficient enforcement capacity and institutional capture by corrupt officials who benefit from trafficking
 *   - Trafficking Networks: Primary beneficiaries (institutional/arbitrage) — profit from debt bondage, document sales, and wage theft across supply chains
 *   - Exploitative Employers: Beneficiaries (powerful/arbitrage) — access low-cost suppressed labor; practice wage theft and safety violations
 *   - Corrupt Officials (immigration, police, labor): Beneficiaries (institutional/arbitrage) — extract bribes from trafficking networks and trafficked workers
 *   - International Anti-Trafficking Coalition (IOM, UNODC, NGOs): Organized agents (organized/constrained) — building formal labor pathways (bilateral agreements, worker certification) as alternatives to trafficking
 *   - Host Countries' Labor Regulators: Institutional actors (institutional/constrained) — formal coordination interest (labor supply) conflicts with enforcement responsibility; many benefit from informal suppressed labor
 *   - Analytical Observer: Civilizational context (analytical/analytical) — sees trafficking as a contingent failure of institutional infrastructure and enforcement will, not as inevitable consequence of migration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(afghan_labor_trafficking, 0.78).
domain_priors:suppression_score(afghan_labor_trafficking, 0.88).
domain_priors:theater_ratio(afghan_labor_trafficking, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(afghan_labor_trafficking, extractiveness, 0.78).
narrative_ontology:constraint_metric(afghan_labor_trafficking, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(afghan_labor_trafficking, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(afghan_labor_trafficking, snare).
narrative_ontology:human_readable(afghan_labor_trafficking, "Afghan Labor Trafficking Network").
narrative_ontology:topic_domain(afghan_labor_trafficking, "labor/human_rights/geopolitical").

domain_priors:requires_active_enforcement(afghan_labor_trafficking).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(afghan_labor_trafficking, trafficking_networks).
narrative_ontology:constraint_beneficiary(afghan_labor_trafficking, corrupt_officials).
narrative_ontology:constraint_beneficiary(afghan_labor_trafficking, exploitative_employers).
narrative_ontology:constraint_victim(afghan_labor_trafficking, afghan_migrant_workers).
narrative_ontology:constraint_victim(afghan_labor_trafficking, vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAFFICKED WORKER (SNARE) — Afghan migrants trapped in debt bondage with documents confiscated, facing deportation, abuse, and wage theft. Exit is structurally impossible: legal status revoked, family threatened, migration pathways closed. Maximum suppression and extraction experienced directly. No coordination benefit perceived.
constraint_indexing:constraint_classification(afghan_labor_trafficking, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DESPERATE FAMILY IN AFGHANISTAN (SNARE) — Structurally mobile (could refuse migration), but identity-locked into accepting trafficker offers due to economic necessity narrative and internalized belief that migration is the only survival path. The family's identity as 'those who must migrate to survive' prevents perceiving refusal as viable, even when trafficker deception is evident.
constraint_indexing:constraint_classification(afghan_labor_trafficking, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: LABOR EXPORTING STATE (TANGLED ROPE) — Afghan state has genuine coordination interest in formal labor migration (remittances represent 4-5% of GDP). Traffickers exploit this legitimate coordination function while extracting labor value and destabilizing legitimate pathways. State faces enforcement costs and sovereignty constraints; trafficking benefits corrupt officials and undermines formal regulation.
constraint_indexing:constraint_classification(afghan_labor_trafficking, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: HOST COUNTRY LABOR MARKET (ROPE) — Employer networks and receiving-country institutions coordinate labor supply and demand. The coordination function is genuine (matching workers to jobs), but trafficking persists because of information asymmetries and enforcement gaps that benefit exploitative employers. Receives low-cost labor; suppression mechanisms (document confiscation, wage theft) are coordination costs hidden from formal oversight.
constraint_indexing:constraint_classification(afghan_labor_trafficking, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL ANTI-TRAFFICKING COALITION (SCAFFOLD) — IOM, UNODC, NGOs, and bilateral programs see trafficking as a solvable coordination failure with sunset logic: bilateral labor agreements, worker certification, remittance regulation, and enforcement capacity building are building formal pathways that can displace trafficking. Coalition has agency and identifies specific exit pathways (formal bilateral agreements, worker protection protocols). Theater ratio low because enforcement mechanisms are functional, not performative.
constraint_indexing:constraint_classification(afghan_labor_trafficking, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LABOR MIGRATION REGULATORY FRAMEWORK (PITON) — International conventions (Palermo Protocol, ILO Conventions) and national labor codes exist but enforcement is minimal; the legal framework persists through institutional formality (treaties signed, agencies established) while trafficking continues. The regulatory theater is high; the functional verification of compliance is near-zero. Framework is maintained by political commitment to appear concerned about trafficking, not by actual capacity to regulate.
constraint_indexing:constraint_classification(afghan_labor_trafficking, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational/universal perspective, the constraint is a pure extraction mechanism: systematic institutional failure to regulate labor markets creates profitable opportunities for trafficking networks. The extraction persists because it is structurally invisible to formal institutions (fugitive labor, undocumented movement, suppressed reporting) and profitable to multiple institutional actors (employers, corrupt officials, trafficking brokers). No natural law necessitates this — it is a contingent failure of coordination infrastructure and enforcement will.
constraint_indexing:constraint_classification(afghan_labor_trafficking, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(afghan_labor_trafficking_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(afghan_labor_trafficking, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(afghan_labor_trafficking, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(afghan_labor_trafficking, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(afghan_labor_trafficking, TR),
    TR >= 0.70.

:- end_tests(afghan_labor_trafficking_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. The constraint captures the majority of labor value through multiple mechanisms: trafficker debt bondage (20-40% of wage captured), employer wage theft (15-30%), corruption payments to officials (5-15%), and unsafe conditions with no compensation (effective 5-10% additional cost burden). Total extraction approaches 70-80% of the worker's presumed wage value. The measurement interval shows increase from 0.65 → 0.78 as trafficking networks have professionalized and employer enforcement has intensified — the mechanisms have become more extractive over time. Suppression (0.88): Extremely high. Multiple mechanisms prevent exit: legal (documents confiscated, no work visa, deportation threat if complaint made), economic (debt bondage, wage advance trap, remittance dependency for families), social (shame, isolation from community), and violent (beatings, threats to family members in Afghanistan). The combination creates near-total suppression — even workers who recognize exploitation have multiple layers of barriers preventing exit or help-seeking. Theater ratio (0.35): Low to moderate. Trafficking operates in fugitive space and generates minimal performative activity — no formal oversight mechanisms are in place and trafficking networks explicitly avoid institutional visibility. The ratio is non-zero because some host countries maintain labor inspection frameworks (theater), and international bodies conduct surveys and reports (theater) that document trafficking without substantially disrupting it. The ratio increases over time as international attention drives NGO presence and reporting, but this theater masks minimal actual enforcement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. The trafficked worker and desperate family experience snare — pure extraction with overwhelming suppression and no coordination benefit. The Afghan state experiences tangled rope — genuine remittance coordination benefit alongside institutional failure to regulate trafficking and corruption benefit from trafficking networks. The trafficking network and employer experience rope (from their position as beneficiaries) — the constraint coordinates labor supply and demand while extracting value. The international coalition experiences scaffold — sees trafficking as a solvable temporary problem with exit pathways (bilateral agreements, worker certification). The labor regulatory framework appears as piton — formal mechanisms exist and are maintained through institutional ritual, but enforcement is minimal and theater is high. The analytical observer sees snare — a structural extraction mechanism that persists because it is profitable to multiple institutional actors and invisible to formal oversight. The perspectival divergence reveals that 'Afghan labor trafficking' is not a single constraint from different angles — it is a constraint family where decomposition would distinguish: (1) debt bondage extraction (snare), (2) state remittance coordination with trafficking capture (tangled rope), (3) labor market informal coordination (rope from employer perspective), and (4) formal labor migration frameworks (piton/scaffold). The mandatrophy is resolved by recognizing that snare is the primary classification from the victim's perspective and that the analytical observer's snare confirms it — no natural law necessitates trafficking, and the constraint is contingent on institutional failure.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's structural position determines directionality and effective extraction experienced (chi). Trafficked workers have d ≈ 1.0 (full targets): trapped exit + victim status + powerless power → maximum chi. Desperate families have d ≈ 0.95 (near-full targets): identity_locked exit prevents perception of agency despite some structural mobility. Afghan state has d ≈ 0.45 (near-symmetric): receives remittance benefit (low d) but also institutional costs from trafficking (high d); organized power and constrained exit reduce experienced extraction relative to raw ε. Trafficking networks and employers have d ≈ 0.05 (beneficiaries): institutional power + arbitrage exit + beneficiary status → d near zero → negative or minimal chi (they experience the constraint as enabling, not extractive). International coalition has d ≈ 0.30 (partial victim): organized power + constrained exit + partial victim (trying to solve trafficking) → moderate d. Analytical observer has d ≈ 0.72 (analytical canonical) → chi ≈ 1.15ε under global scope. The directionality pipeline makes no overrides — the derivation from beneficiary/victim + exit + power captures the structural relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: This constraint classifies as snare from all primary perspectives except the beneficiaries (who see rope) and the aspirational coalition (who see scaffold). The mandatrophy is resolved by recognizing that snare is the structurally accurate classification from the victim's position, and that the analytical observer confirms it — trafficking is pure extraction with minimal coordination function (the 'coordination' is between traffickers, employers, and corrupt officials, not between any legitimate parties). There is no mislabeling risk: calling this tangled rope would require finding a genuine coordination benefit to the victim, which does not exist. Calling it rope would require suppression to be low or optional — suppression is near-total. The constraint is unambiguously a snare from the structural analysis perspective. The piton classification (from the labor regulatory framework) and scaffold classification (from the international coalition) are legitimate perspectival readings but do not override the primary classification — they reflect aspirational or institutional framings that mask the underlying snare structure. Mandatrophy is resolved: snare is correct, and the divergent perspectives reveal institutional denial rather than genuine analytical ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trafficking_network_vs_employer_extraction,
    'Is the primary extraction mechanism the trafficking network (debt bondage, document confiscation) or the exploitative employer (wage theft, unsafe conditions)?',
    'Worker interviews tracking extraction sequence: which mechanism captures larger share of labor value? Are network and employer operationally separable or fused?',
    'If trafficking network: constraint is pure snare (high suppression, high extraction). If employer: constraint becomes tangled rope (coordination + extraction fused). If fused: decompose into two stories per ε-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(trafficking_network_vs_employer_extraction, empirical, 'Attribution of extraction to trafficking networks vs employers').

omega_variable(
    family_economic_necessity_vs_deception,
    'Do families accept trafficking due to genuine economic necessity or due to trafficker deception about working conditions and wages?',
    'Pre-migration interviews tracking family decision-making; post-rescue interviews about what families believed they were accepting vs. reality; correlation between family pre-migration expectations and actual conditions',
    'If necessity: identity_locked classification is accurate; suppression is partly internalized. If deception: families are victims of information asymmetry (constrained exit); suppression is entirely structural. If both: suppression mechanism is hybrid (structural + internalized) — requires omega variable on suppression composition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_economic_necessity_vs_deception, empirical, 'Relative weight of economic necessity vs deception in family trafficking acceptance').

omega_variable(
    formal_bilateral_agreement_effectiveness,
    'Do bilateral labor agreements and worker certification programs actually reduce trafficking rates, or do they merely displace trafficking to informal channels?',
    'Time-series analysis of trafficking rates in corridors with vs. without bilateral agreements; geographic displacement analysis (trafficking flow redirection); worker complaint reporting pre/post-formalization',
    'If effective: scaffold perspective confirmed — sunset is structurally real. If displacement: scaffold is aspirational; extraction mechanism adapts rather than declines. If both: constraint bifurcates into two stories (formal pathway coordination vs. informal trafficking snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_bilateral_agreement_effectiveness, empirical, 'Whether bilateral agreements reduce trafficking or displace it').

omega_variable(
    remittance_incentive_alignment,
    'Do remittance flows to Afghanistan create sufficient institutional incentive for the state to regulate trafficking, or do benefits from unregulated migration (informal taxation, political leverage) outweigh formalization costs?',
    'Analysis of Afghan government enforcement allocation; correlation between remittance volume and state anti-trafficking investment; interviews with officials on private vs. public anti-trafficking commitment',
    'If incentive aligned: state perspective is genuine tangled rope (coordination interest is real). If misaligned: state is complicit beneficiary — state perspective should be snare or rope (state extracts from both formal and informal channels).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remittance_incentive_alignment, conceptual, 'Whether remittances incentivize Afghan state to regulate trafficking').

omega_variable(
    suppression_mechanism_composition,
    'Is suppression primarily structural (legal barriers, deportation threats, economic dependency) or internalized (shame, identity fusion with ''migrant'' role, belief in inevitability)?',
    'Post-rescue longitudinal tracking: persistence of suppression beliefs after structural barriers removed; worker self-reporting on perceived agency vs. actual constraints',
    'If structural: standard snare analysis applies. If internalized: constraint carries suppression into post-exit period; requires higher therapeutic intervention costs. If both: decompose suppression omega into structural and internalized components.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Composition of suppression mechanism (structural vs. internalized)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(afghan_labor_trafficking, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(afgh_tr_t0, afghan_labor_trafficking, theater_ratio, 0, 0.22).
narrative_ontology:measurement(afgh_tr_t5, afghan_labor_trafficking, theater_ratio, 5, 0.28).
narrative_ontology:measurement(afgh_tr_t10, afghan_labor_trafficking, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(afgh_be_t0, afghan_labor_trafficking, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(afgh_be_t5, afghan_labor_trafficking, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(afgh_be_t10, afghan_labor_trafficking, base_extractiveness, 10, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(afghan_labor_trafficking, resource_allocation).
narrative_ontology:affects_constraint(afghan_labor_trafficking, afghan_refugee_displacement).
narrative_ontology:affects_constraint(afghan_labor_trafficking, regional_labor_market_inequality).
narrative_ontology:affects_constraint(afghan_labor_trafficking, host_country_labor_standard_erosion).

% DUAL FORMULATION NOTE:
% Afghan labor trafficking is downstream of state collapse and refugee crises but structurally autonomous — it would persist under different upstream conditions. If decomposed into constraint family: (1) debt_bondage_snare (ε=0.78, pure extraction), (2) state_remittance_coordination (ε=0.55, tangled rope), (3) formal_labor_pathways (ε=0.25, scaffold/rope). Each has distinct metrics and perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
