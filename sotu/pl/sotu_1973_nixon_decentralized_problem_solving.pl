% ============================================================================
% CONSTRAINT STORY: sotu_1973_nixon_decentralized_problem_solving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1973_nixon_decentralized_problem_solving, []).

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
 *   constraint_id: sotu_1973_nixon_decentralized_problem_solving
 *   human_readable: Subsidiarity Principle: States and Individuals as Primary Problem-Solvers (1973 Nixon Doctrine)
 *   domain: social_policy/federalism/governance
 *
 * SUMMARY:
 *   The subsidiarity principle institutionalized in Nixon's 1973 domestic
 *   policy represents a fundamental structural shift in the
 *   federal-state-individual relationship, assigning primary responsibility
 *   for solving domestic problems to the actors 'closest' to those problems —
 *   states, localities, and individuals themselves. This constraint exhibits
 *   genuine coordination function (local adaptation, responsiveness to local
 *   conditions, reduction of federal-bureaucratic overhead) paired with
 *   extractive asymmetry (cost-shifting to least-resourced actors, unequal
 *   capacity to solve problems, degradation of national standards). The
 *   constraint benefits state and local governments that gain autonomy and
 *   wealthy communities that can afford private alternatives to federal
 *   provision, while imposing costs on populations that depend on nationally
 *   coordinated or uniform resource distribution. The extractiveness
 *   trajectory shows acceleration from 1973-1976 as the policy implications
 *   unfold (welfare reductions, education devolution, healthcare
 *   fragmentation), then slight stabilization as federal contingency
 *   mechanisms reassert themselves behind the subsidiarity rhetoric. The
 *   theater ratio rises as the cosmetic framing ('returning power to the
 *   people') diverges from the structural mechanism (cost-shifting to
 *   localities with least capacity). The constraint operates as tangled rope:
 *   genuine subsidiarity principles solve legitimate coordination problems,
 *   but the selective application to service delivery (which enables
 *   cost-shifting) rather than to all governance domains reveals the
 *   extractive mechanism.
 *
 * KEY AGENTS:
 *   - State Governments: Primary beneficiaries (institutional/arbitrage) — gain decision authority and budget flexibility; can pass costs to localities
 *   - Local Governments: Secondary beneficiaries (organized/constrained) — gain autonomy but face budget pressures from unfunded mandates
 *   - Wealthy Communities: Primary beneficiaries (powerful/arbitrage) — can afford private provision; benefit from reduced federal redistribution
 *   - Low-Income Populations and Rural Poor: Primary victims (powerless/trapped) — lose access to federal guarantees; cannot pay for private alternatives; trapped in lower-capacity jurisdictions
 *   - Marginalized Minorities: Primary victims (powerless/constrained) — subsidiarity allows regionally discriminatory application of civil rights; federal oversight weakened
 *   - Federal Agencies: Mixed (institutional/constrained) — lose directive authority but retain implementation leverage through grants
 *   - Progressive Reform Coalition: Organized opposition (organized/constrained) — sees temporary constraint with structural sunset via political mobilization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1973_nixon_decentralized_problem_solving, 0.52).
domain_priors:suppression_score(sotu_1973_nixon_decentralized_problem_solving, 0.58).
domain_priors:theater_ratio(sotu_1973_nixon_decentralized_problem_solving, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1973_nixon_decentralized_problem_solving, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1973_nixon_decentralized_problem_solving, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sotu_1973_nixon_decentralized_problem_solving, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1973_nixon_decentralized_problem_solving, tangled_rope).
narrative_ontology:human_readable(sotu_1973_nixon_decentralized_problem_solving, "Subsidiarity Principle: States and Individuals as Primary Problem-Solvers (1973 Nixon Doctrine)").
narrative_ontology:topic_domain(sotu_1973_nixon_decentralized_problem_solving, "social_policy/federalism/governance").

domain_priors:requires_active_enforcement(sotu_1973_nixon_decentralized_problem_solving).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1973_nixon_decentralized_problem_solving, state_governments).
narrative_ontology:constraint_beneficiary(sotu_1973_nixon_decentralized_problem_solving, local_governments).
narrative_ontology:constraint_beneficiary(sotu_1973_nixon_decentralized_problem_solving, wealthy_communities).
narrative_ontology:constraint_beneficiary(sotu_1973_nixon_decentralized_problem_solving, business_interests_reduced_regulation).
narrative_ontology:constraint_victim(sotu_1973_nixon_decentralized_problem_solving, low_income_populations).
narrative_ontology:constraint_victim(sotu_1973_nixon_decentralized_problem_solving, rural_poor).
narrative_ontology:constraint_victim(sotu_1973_nixon_decentralized_problem_solving, marginalized_minorities).
narrative_ontology:constraint_victim(sotu_1973_nixon_decentralized_problem_solving, uniform_service_dependents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL POOR AND MARGINALIZED (SNARE) — Communities without state/local resources to address problems are trapped in degradation. Cannot exit to wealthier jurisdictions; cannot afford private solutions; no federal fallback. Maximum extraction. Subsidiarity principle assigns 'responsibility' to actors (local governments, individuals) who lack capacity or capital. The burden of 'self-help' falls on those with least capacity.
constraint_indexing:constraint_classification(sotu_1973_nixon_decentralized_problem_solving, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-CLASS COMMUNITIES WITH PARTIAL CAPACITY (TANGLED ROPE) — Communities with moderate resources experience both coordination benefit (local responsiveness, tailored solutions) and extraction cost (responsibility without federal backup, variance in service quality). Constrained by resource limits and cannot easily relocate. Mixed structural position: genuine coordination function (local problem-solving efficiency) paired with asymmetric extraction (unequal bearing of service provision burden).
constraint_indexing:constraint_classification(sotu_1973_nixon_decentralized_problem_solving, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE AND LOCAL GOVERNMENTS (ROPE) — Primary beneficiaries. Gain autonomy, decision-making authority, and flexibility to tailor programs. Experience the constraint as pure coordination: subsidiarity solves the alignment problem between policy and local conditions. Can leverage existing administrative capacity. Net benefit: authority without proportionate federal responsibility transfer.
constraint_indexing:constraint_classification(sotu_1973_nixon_decentralized_problem_solving, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: WEALTHY COMMUNITIES AND PRIVATE SECTOR (ROPE) — Benefits from reduced federal regulatory burden and redistribution. Wealthy communities can afford private provision of services (education, healthcare, security) and benefit from tax burden reduction. Private sector gains arbitrage advantage: federal social provision reduces market size and competition, allowing private alternatives to command premium prices. Pure coordination from their perspective: autonomy enables market-responsive solutions.
constraint_indexing:constraint_classification(sotu_1973_nixon_decentralized_problem_solving, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PROGRESSIVE REFORM COALITION (SCAFFOLD) — Organized agents (advocacy groups, civil rights organizations, labor unions) see subsidiarity as a temporary degradation with a structural sunset. Federal civil rights enforcement, poverty programs, and service provision are building organizational capacity and political consciousness. Long-term trajectory: grassroots organizing, community development corporations, and electoral power of marginalized populations will reassert federal accountability. Sunset mechanism: voting bloc formation, civil rights litigation, and demonstrated failure of local-only solutions create demand for federal re-engagement. Theater moderate because the constraint's justification (local responsiveness) has some genuine truth, but the real mechanism is cost-shifting to least-resourced actors.
constraint_indexing:constraint_classification(sotu_1973_nixon_decentralized_problem_solving, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE FEDERALISM DOCTRINE APPARATUS (PITON) — The constitutional federalism framework that Nixon invokes has degraded from its original coordinating function (balancing state autonomy with national standards) into performative maintenance of the states-rights rhetorical tradition. The actual mechanism no longer coordinates — federal agencies still direct resource flows despite the subsidiarity doctrine. The theater of 'returning power to the states' persists through institutional inertia while federal categorical grants and mandates remain. The doctrine is maintained because dismantling it would require renegotiating the entire federal-state bargain, not because it functionally organizes governance.
constraint_indexing:constraint_classification(sotu_1973_nixon_decentralized_problem_solving, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal frame, some governance problems are inherently local (zoning, community policing, school curricula) and some are inherently national (interstate commerce, civil rights standards, epidemic control). Subsidiarity maps to this natural structure of problem scope. This perspective risks seeing subsidiarity as an inevitable principle of sound governance — immutable because it correctly reflects the nature of problems. However, the structural data reveals this as a false summit: the selective application of subsidiarity (to services that benefit from cost-shifting, not to services that require national standards) demonstrates that the principle is ideological, not ontological.
constraint_indexing:constraint_classification(sotu_1973_nixon_decentralized_problem_solving, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: FEDERAL AGENCIES (TANGLED ROPE) — Federal bureaucracies experience mixed structural effects. They lose directive authority and budget (extraction cost) but retain implementation control through grant conditions, audit rights, and regulatory override capacity (coordination benefit). They maintain gatekeeping power over state/local funding. The constraint coordinates federal-state relations while extracting power asymmetrically from federal agencies — they implement subsidiarity while preserving structural leverage over implementation.
constraint_indexing:constraint_classification(sotu_1973_nixon_decentralized_problem_solving, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1973_nixon_decentralized_problem_solving_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1973_nixon_decentralized_problem_solving, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1973_nixon_decentralized_problem_solving, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1973_nixon_decentralized_problem_solving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1973_nixon_decentralized_problem_solving, TR),
    TR >= 0.70.

:- end_tests(sotu_1973_nixon_decentralized_problem_solving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts significant material and political benefits to state/local governments and wealthy communities through cost-shifting and reduced federal oversight. The extraction is not maximal (0.70+) because some genuine subsidiarity benefits exist: local adaptation, reduced bureaucratic overhead, and some increase in responsiveness. The middle trajectory reflects the initial shock of policy shift (1973-1976 rapid extraction increase to 0.54) followed by partial federal reassertion (through amendments, court decisions, re-funding) that stabilizes extractiveness around 0.52. Suppression (0.58): Moderate-high. Barriers to exit and alternatives include: geographic immobility (individuals cannot easily relocate to wealthier jurisdictions), resource constraints (communities cannot afford to build alternative institutions), and legal constraints (individuals and lower-capacity states cannot mandate federal provision). Suppression is not maximal because federal fallback mechanisms remain and political mobilization is possible. Theater ratio (0.65): Moderate-high. The subsidiarity rhetoric ('returning power to the people,' 'decision-making closer to those affected') is substantially performance. The actual mechanism selectively devolved service provision to low-capacity actors while retaining federal control over higher-value functions. Federal categorical grants remain but with reduced funding. State/local 'autonomy' is largely autonomy to reduce services, not autonomy to innovate. Theater has declined slightly (0.68 peak in mid-period) as the gap between rhetoric and outcome became obvious, but remains elevated because both beneficiaries and federal agencies continue invoking subsidiarity as justification.
 *
 * PERSPECTIVAL GAP:
 *   The structural gap between beneficiary and victim perspectives is maximal. State governments and wealthy communities experience pure coordination function (rope) — subsidiarity solves the alignment problem and reduces federal overhead while preserving authority. Low-income populations experience pure extraction (snare) — responsibility is assigned to actors and jurisdictions without capacity, creating a poverty trap. The middle-class perspective shows the hybrid nature (tangled rope): genuine benefit from local responsiveness paired with extraction through unequal burden-sharing. The federal agencies perspective reveals the piton mechanism: the subsidiarity doctrine persists through ideological force while federal leverage remains operational. The progressive coalition perspective identifies the scaffold: the constraint is temporary because marginalized populations' political mobilization will eventually reassert federal accountability. The analytical observer's mountain reveals a false summit: subsidiarity is presented as an immutable principle of sound governance (problems are best solved by actors closest to them) but this naturalizes what is actually a contingent political choice to shift costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position: (1) State governments: beneficiary + arbitrage exit = low d (around 0.15) = negative effective extraction (they benefit). (2) Wealthy communities: beneficiary + arbitrage exit = low d = negative χ. (3) Low-income populations: victim + trapped exit = high d (around 0.95) = maximum f(d) = maximum experienced extraction. (4) Federal agencies: mixed (lose authority but retain leverage) = moderate d (around 0.55). (5) Progressive coalition: victim + organized exit = moderate d (around 0.40-0.50) = moderate f(d) = moderate extraction, but with agency and coalition capacity. The perspectival gap widens because directionality differentiates agents along the extraction flow: beneficiaries see coordination (rope), trapped victims see pure extraction (snare), organized opposition sees temporary constraint with sunset (scaffold), federal apparatus sees degraded doctrine (piton), and analytical observer risks naturalizing a contingent policy as inevitable federalism principle (false summit).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolved through perspectival decomposition. The apparent paradox: 'Is subsidiarity coordination or extraction?' is resolved by recognizing that it is genuinely BOTH. The constraint coordinates federal-state relations (genuine subsidiarity function reduces bureaucratic overhead and enables local adaptation). Simultaneously, it extracts asymmetrically by shifting costs to low-capacity actors and jurisdictions. The extraction mechanism depends on capacity variance: if all jurisdictions had equal resources, subsidiarity would be pure coordination. Because they do not, subsidiarity becomes an extraction device that concentrates burden on least-resourced actors. The mandatrophy is resolved by the tangled_rope classification: both functions are real and structural. The beneficiary sees rope (coordination), the victim sees snare (extraction), and the truthful structural analysis recognizes both. The false-summit mountain perspective (subsidiarity as natural principle) is revealed by the beneficiary/victim pattern: if subsidiarity were genuinely neutral, we would not observe systematic directional extraction flow. The fact that it consistently benefits state/local governments while harming low-income populations indicates the principle is deployed selectively to achieve ideological outcomes rather than to solve structural governance problems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidiarity_scope_selection,
    'What determines which problems are deemed ''inherently local'' vs ''inherently national'' under subsidiarity logic?',
    'Historical analysis of problem classification over time; comparison of rationalized scope assignments with actual implementation patterns. Trace whether scope classification correlates with fiscal burden (local = low-cost problems, national = high-cost problems).',
    'If classification is genuine principle-driven: subsidiarity correctly matches governance level to problem structure. If scope selection correlates with cost-shifting incentives: subsidiarity is ideological cover for decentralized extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidiarity_scope_selection, empirical, 'Whether subsidiarity scope classification reflects inherent problem structure or cost-shifting incentives').

omega_variable(
    state_capacity_variance,
    'Does the performance variance between wealthy and poor states after subsidiarity implementation reflect genuine local preference diversity or structural inequality in implementation capacity?',
    'Comparative analysis of outcomes in wealthy vs poor jurisdictions for identical problem types (education, healthcare, poverty); correlation analysis between state fiscal capacity and service provision quality; measurement of service variance within-state vs between-state.',
    'If variance reflects preference: subsidiarity achieves stated coordination goal. If variance reflects capacity inequality: subsidiarity becomes an extraction mechanism that accumulates disadvantage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_capacity_variance, empirical, 'Whether state outcome variance reflects preference diversity or capacity inequality').

omega_variable(
    federal_fallback_persistence,
    'Does the federal government retain de facto responsibility for crisis intervention despite subsidiarity doctrine, indicating that ''primary responsibility'' is rhetorical rather than structural?',
    'Historical tracking of federal emergency interventions and bailouts post-1973; analysis of whether subsidiarity doctrine is suspended during crises; examination of federal contingency mechanisms (emergency powers, categorical grants, deficit spending) that remain in place.',
    'If federal fallback persists: subsidiarity is theater masking federal reserve role. If federal fully withdraws from contingency: subsidiarity has genuine structural force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_fallback_persistence, empirical, 'Whether federal government maintains de facto crisis responsibility despite subsidiarity doctrine').

omega_variable(
    poverty_trap_mechanism,
    'Do low-income individuals and communities become trapped in poverty cycles by subsidiarity, creating self-reinforcing extraction where lack of resources prevents capacity-building, which prevents service provision, which deepens poverty?',
    'Intergenerational poverty tracking in high-subsidiarity vs high-federal-support jurisdictions; measurement of poverty exit rates and mobility by region; analysis of poverty transitions before/after 1973.',
    'If poverty trap confirmed: subsidiarity operates as snare for marginalized populations. If outcomes improve under subsidiarity: the constraint enables local adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(poverty_trap_mechanism, empirical, 'Whether subsidiarity creates self-reinforcing poverty traps for low-income communities').

omega_variable(
    collective_action_failure_scope,
    'For problems with positive externalities or coordination requirements (pollution control, disease prevention, infrastructure), does subsidiarity enable free-riding by low-contribution jurisdictions, degrading collective outcomes?',
    'Analysis of environmental compliance variance, disease spread patterns, and infrastructure fragmentation post-1973; measurement of collective action problems in subsidiarity-devolved domains; comparison with centrally coordinated alternatives.',
    'If collective action failures observed: subsidiarity breaks coordination for public goods. If local solutions prove adequate: subsidiarity enables efficient adaptation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_action_failure_scope, empirical, 'Whether subsidiarity enables free-riding and collective action failure in public goods domains').

omega_variable(
    ideological_versus_structural_principle,
    'Is subsidiarity a genuinely operational principle of governance, or is it a rhetorical frame deployed to justify political outcomes favored by beneficiary groups?',
    'Trace instances where subsidiarity principle is invoked vs ignored; identify systematic correlation between invocation and fiscal burden reduction for beneficiary groups; compare principle application to alternative governance theories (pragmatic federalism, scale efficiency, capability matching).',
    'If purely structural: subsidiarity correctly guides governance design. If primarily ideological: reclassify constraint from tangled_rope to snare with false-summit mountain overlay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_versus_structural_principle, conceptual, 'Whether subsidiarity is operational principle or ideological cover story').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1973_nixon_decentralized_problem_solving, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subsid_tr_t0, sotu_1973_nixon_decentralized_problem_solving, theater_ratio, 0, 0.5).
narrative_ontology:measurement(subsid_tr_t3, sotu_1973_nixon_decentralized_problem_solving, theater_ratio, 3, 0.6).
narrative_ontology:measurement(subsid_tr_t6, sotu_1973_nixon_decentralized_problem_solving, theater_ratio, 6, 0.68).
narrative_ontology:measurement(subsid_tr_t10, sotu_1973_nixon_decentralized_problem_solving, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(subsid_be_t0, sotu_1973_nixon_decentralized_problem_solving, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(subsid_be_t3, sotu_1973_nixon_decentralized_problem_solving, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(subsid_be_t6, sotu_1973_nixon_decentralized_problem_solving, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(subsid_be_t10, sotu_1973_nixon_decentralized_problem_solving, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1973_nixon_decentralized_problem_solving, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1973_nixon_decentralized_problem_solving, welfare_devolution_state_discretion).
narrative_ontology:affects_constraint(sotu_1973_nixon_decentralized_problem_solving, education_funding_equalization_failure).
narrative_ontology:affects_constraint(sotu_1973_nixon_decentralized_problem_solving, healthcare_fragmentation_access_variance).
narrative_ontology:affects_constraint(sotu_1973_nixon_decentralized_problem_solving, civil_rights_enforcement_capacity_degradation).

% DUAL FORMULATION NOTE:
% Subsidiarity principle is the macro-institutional framework; specific service domains (welfare, education, healthcare, civil rights enforcement) are decomposed into separate constraint stories with their own extractiveness values and beneficiary/victim structures. Each domain story measures extractiveness from the implementation perspective; this story measures extractiveness from the principle-level federal-state relationship. The principle story affects all downstream domain stories through the enforcement mechanism type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1973_nixon_decentralized_problem_solving, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
