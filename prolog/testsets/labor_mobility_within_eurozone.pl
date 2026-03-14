% ============================================================================
% CONSTRAINT STORY: labor_mobility_within_eurozone
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_mobility_within_eurozone, []).

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
 *   constraint_id: labor_mobility_within_eurozone
 *   human_readable: Labor Mobility Constraints Within the Eurozone
 *   domain: economic/labor/political
 *
 * SUMMARY:
 *   The free movement of labor within the Eurozone is formally enshrined in
 *   EU treaties as a fundamental right, yet substantial barriers limit its
 *   practical exercise. This constraint demonstrates how legal mobility and
 *   structural immobility can coexist: workers have the formal right to
 *   relocate across member states but face language requirements, credential
 *   non-recognition, housing discrimination, family obligations, and
 *   welfare/social network dependence that function as practical barriers.
 *   The constraint shows characteristics of both coordination (genuine
 *   economic gains from labor market flexibility) and extraction (high-skill
 *   workers capture disproportionate benefits while low-skill workers face
 *   wage suppression in origin countries, and peripheral states experience
 *   brain drain while bearing fiscal costs for remaining populations). The
 *   extractiveness has risen from 0.35 to 0.52 over the 20-year interval as
 *   skill-based wage gaps have widened and brain drain from peripheral states
 *   has accelerated. The theater ratio (0.58) reflects that formal freedom of
 *   movement is celebrated as an achieved principle while the practical
 *   enabling mechanisms (credential harmonization, language support,
 *   anti-discrimination enforcement) remain underfunded and performative.
 *
 * KEY AGENTS:
 *   - Low-skill workers in peripheral states: Primary victims (powerless/trapped) — legally mobile but practically immobilized by language, credential, and social barriers; bear wage suppression from arbitrage threat
 *   - High-skill professionals: Primary beneficiaries (powerful/arbitrage) — full practical mobility; benefit from career optimization and geographic wage premiums; no meaningful exit barriers
 *   - Peripheral member states (Poland, Greece, Portugal, Bulgaria): Secondary victims (institutional/constrained) — experience brain drain, tax base loss, public service pressure; bound by EU law; cannot reimpose controls without treaty violation
 *   - Core member states / capital owners: Beneficiaries (institutional/arbitrage) — access flexible labor supply; benefit from wage competition; can tighten enforcement selectively while maintaining EU compliance
 *   - Constrained destination workers: Mixed position (moderate/constrained) — benefit from access to diverse labor markets; bear costs of wage competition and housing inflation in destination countries
 *   - EU institutional reform coalition: Organized agents (organized/constrained) — EU Parliament, labor advocates, migration researchers pursuing credential harmonization, language training, fiscal transfer mechanisms
 *   - Formal freedom of movement system: Institutional actor (institutional/arbitrage) — maintains formal commitment while enabling mechanisms remain underfunded; sees own degradation but enforces principles selectively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_mobility_within_eurozone, 0.52).
domain_priors:suppression_score(labor_mobility_within_eurozone, 0.65).
domain_priors:theater_ratio(labor_mobility_within_eurozone, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_mobility_within_eurozone, extractiveness, 0.52).
narrative_ontology:constraint_metric(labor_mobility_within_eurozone, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(labor_mobility_within_eurozone, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_mobility_within_eurozone, tangled_rope).
narrative_ontology:human_readable(labor_mobility_within_eurozone, "Labor Mobility Constraints Within the Eurozone").
narrative_ontology:topic_domain(labor_mobility_within_eurozone, "economic/labor/political").

domain_priors:requires_active_enforcement(labor_mobility_within_eurozone).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_mobility_within_eurozone, capital_owners).
narrative_ontology:constraint_beneficiary(labor_mobility_within_eurozone, high_skill_workers).
narrative_ontology:constraint_beneficiary(labor_mobility_within_eurozone, wealthy_member_states).
narrative_ontology:constraint_victim(labor_mobility_within_eurozone, low_skill_workers).
narrative_ontology:constraint_victim(labor_mobility_within_eurozone, peripheral_state_populations).
narrative_ontology:constraint_victim(labor_mobility_within_eurozone, destination_country_fiscal_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMMOBILIZED LOW-SKILL WORKER (SNARE) — Faces structural barriers to mobility: language requirements, credential non-recognition, family obligations, social networks rooted in origin country. Legally mobile within the EU but practically trapped by linguistic, educational, and social costs. Bears extraction through wage suppression in home labor market (suppressed by arbitrage threat from high-mobility workers) while unable to access higher-wage destinations. No viable exit option.
constraint_indexing:constraint_classification(labor_mobility_within_eurozone, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GLOBALIZED HIGH-SKILL PROFESSIONAL (ROPE) — Full mobility within Eurozone; benefits from credential recognition, English-language access, professional networks spanning borders. Experiences the constraint as pure coordination: EU free movement enables career optimization without friction. Net beneficiary from labor mobility framework. Arbitrage exit — can relocate to access opportunities in any member state or beyond.
constraint_indexing:constraint_classification(labor_mobility_within_eurozone, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: CONSTRAINED DESTINATION WORKER (TANGLED ROPE) — Moderate mobility (could relocate but faces significant costs: family separation, credential translation, housing barriers, discrimination). Experiences genuine coordination benefit (access to diverse labor markets, skill-matching across borders) alongside extraction (wage competition in destination market, housing cost inflation driven by migrant influx, welfare state burden-sharing). Mixed experience — agency exists but constrained by relocation costs.
constraint_indexing:constraint_classification(labor_mobility_within_eurozone, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: PERIPHERAL MEMBER STATE (TANGLED ROPE) — Constrained by European legal commitments; cannot reimpose border controls or credential barriers without treaty violation. Experiences genuine coordination benefit: access to EU market, intra-European supply chains, capital flows. Simultaneously experiences extraction: brain drain of high-skill workers, fiscal pressure from remaining low-income population, loss of tax base. Active enforcement required via EU law; exit via treaty withdrawal carries massive costs (Brexit analogue).
constraint_indexing:constraint_classification(labor_mobility_within_eurozone, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CORE MEMBER STATE / CAPITAL OWNER (ROPE) — Experiences labor mobility as coordination mechanism: access to lower-wage workers improves labor market flexibility and wage competition, increasing capital returns. Benefits from inbound migration of productive workers without bearing integration costs proportional to benefit. Arbitrage option: can tighten immigration policy (labor code enforcement, credential barriers) if political pressure rises, while maintaining EU legal compliance through selective enforcement.
constraint_indexing:constraint_classification(labor_mobility_within_eurozone, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EU INSTITUTIONAL REFORM COALITION (SCAFFOLD) — Organized agents (EU Parliament, progressive labor advocates, academic migration researchers) see labor mobility constraints as solvable through institutional reform: credential portability agreements, language-training infrastructure, housing subsidy coordination, fiscal transfer mechanisms. Perceive a sunset: as EU integration deepens, linguistic and credential barriers decline; as fiscal equalization mechanisms emerge, burden-sharing improves. Suppression visible but declining — reform mechanisms exist and are actively deployed.
constraint_indexing:constraint_classification(labor_mobility_within_eurozone, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: FORMAL FREEDOM OF MOVEMENT RITUAL (PITON) — The Treaty of Rome's freedom of movement provisions are formally universal but functionally constrained by non-treaty barriers (language, credential recognition, housing access, discrimination). The constraint persists through institutional inertia: EU celebrates labor mobility as achieved principle while the actual mechanism remains theatrical (border controls dismantled but structural barriers persist). Theater ratio high because the formal legal right exceeds the practical exercise thereof. The EU system sees this degradation but maintains the principle through performative commitment without adequate enforcement of the enabling conditions.
constraint_indexing:constraint_classification(labor_mobility_within_eurozone, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some labor immobility is inherent to human migration: language, kinship, cultural anchoring, credential systems serve legitimate coordination functions. The gap between legal mobility and practical mobility might be naturalized as inherent to the complexity of multi-national labor markets. However, structural data reveals this is a false summit: the suppression mechanisms (credential non-recognition, language barriers, housing discrimination) are contingent policy choices, not laws of nature. Active enforcement could reduce them; the constraint is not immutable.
constraint_indexing:constraint_classification(labor_mobility_within_eurozone, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_mobility_within_eurozone_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_mobility_within_eurozone, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_mobility_within_eurozone, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_mobility_within_eurozone, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_mobility_within_eurozone, TR),
    TR >= 0.70.

:- end_tests(labor_mobility_within_eurozone_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over the interval. The constraint generates real economic value through labor market matching and capital returns from wage competition. However, the benefits are asymmetrically distributed: high-skill workers and capital owners gain disproportionately while low-skill workers in peripheral states lose. The rise from 0.35 to 0.52 reflects widening skill-based wage gaps and accelerating brain drain as EU expansion increased the periphery-core labor pool differential. Suppression (0.65): High. Multiple reinforcing barriers limit labor mobility despite formal right: language requirements (high cost to acquire), credential non-recognition (requires re-certification even for equivalent qualifications), housing discrimination (informal barriers not effectively enforced), family/social network dependence (psychic cost of relocation), welfare/public service anchoring (loss of accumulated benefits). No single barrier is absolute but the combination is substantial. Theater ratio (0.58): Moderate-high. The EU celebrates freedom of movement as a core principle and maintains formal abolition of border controls. Yet practical barriers persist and are largely unconfroned: credentials remain nationally regulated despite mutual recognition agreements, languages are taught inadequately, housing markets remain segmented by origin-country discrimination. The formal freedom exceeds its practical enablement. The theater is not total (some workers do move, movement rates are higher than pre-EU baseline) but significant.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates a full perspectival gradient. The high-skill professional sees Rope (pure coordination benefit, minimal extraction experience). The core state sees Rope (labor flexibility benefit, extraction captured by capital). The peripheral state sees Tangled Rope (genuine economic benefit from EU membership + supply chains, but extraction via brain drain and fiscal pressure). The constrained destination worker sees Tangled Rope (access to opportunity + wage competition + discrimination). The immobilized low-skill worker sees Snare (legal right that is practically blocked, wage suppression from arbitrage threat, no viable exit). The EU reform coalition sees Scaffold (temporary problem solvable through policy — credential harmonization, language training, anti-discrimination enforcement with sunset as integration deepens). The formal system sees Piton (the freedom of movement principle is maintained through performative commitment while the enabling conditions degrade). The analytical observer risks seeing Mountain (labor immobility inherent to human nature, language barriers immutable) — but the structural data reveals this as false naturalization: the barriers are policy-contingent and could be substantially reduced through coordinated EU investment.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position within the constraint. High-skill workers with arbitrage options experience low d (benefit flow runs toward them): d ≈ 0.10-0.20, f(d) ≈ -0.05, negative effective extraction. Low-skill trapped workers experience high d (bear extraction): d ≈ 0.92, f(d) ≈ 1.38, high effective extraction. Peripheral states constrained by treaty experience moderate d (both benefits and costs): d ≈ 0.65, f(d) ≈ 1.00, moderate effective extraction. Scope modifier σ(S) is 1.0 at national scope, 1.1 at continental scope — the Eurozone's continental scope slightly amplifies extracted values, reflecting that larger geographic integration makes individual state resistance less feasible. Core states with arbitrage options experience negative chi despite moderate ε because their directionality is beneficiary-aligned (d ≈ 0.15). Peripheral states experience high chi despite identical ε because their directionality is victim-aligned (d ≈ 0.65). This explains why the same formal rule generates opposite classifications from core vs peripheral perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES VIA PERSPECTIVAL PLURALITY: The mandatrophy is resolved by recognizing that all perspectives except the false mountain are legitimate readings of genuine structural features. There is no single 'true' classification because different agents experience genuinely different constraint structures. The high-skill worker genuinely experiences coordination benefit (Rope). The peripheral state genuinely experiences mixed coordination + extraction (Tangled Rope). The immobilized low-skill worker genuinely experiences extraction with no exit (Snare). The EU reform coalition genuinely perceives a temporary problem with institutional solutions (Scaffold). The piton reading is accurate — the formal principle exceeds enabling capacity. The mountain is false naturalization — the barriers are policy choices, not laws of nature. The constraint's true nature is the presheaf over all legitimate perspectives, not a single canonical type. The analytical frame must capture this perspectival plurality to avoid mislabeling the constraint. If forced to a single type for administrative purposes, Tangled Rope is the most accurate: the constraint genuinely coordinates (provides economic benefits via labor market matching) and genuinely extracts (distributes benefits asymmetrically, suppresses wages for immobile workers, extracts fiscal value from peripheral states).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_portability_mechanism,
    'Are credential non-recognition barriers structural necessities or contingent policy choices?',
    'Comparative analysis: jurisdictions with automatic credential recognition vs those with case-by-case assessment; correlation between recognition speed and labor market outcomes; analysis of whether non-recognition serves legitimate public safety purposes vs protects incumbent professional monopolies',
    'If structural: suppression value justified, extractiveness > 0.50 holds. If contingent: suppression could fall to 0.35-0.40 with policy harmonization, shifting classification toward Rope from multiple perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_portability_mechanism, empirical, 'Whether credential barriers serve legitimate safety/quality purposes or protect professional rents').

omega_variable(
    language_barrier_resolution_feasibility,
    'Can EU-funded language training infrastructure reduce the language barrier to sub-critical levels within one generation?',
    'Cost-benefit analysis of EU language training programs; comparison of mobility rates before/after intensive language intervention; data on employer demand for multilingual workers vs language skill supply',
    'If feasible: suppression could decline by 0.15-0.20 points within 20 years, scaffold sunset timeline is realistic. If infeasible: language barrier remains structural, reducing mobility potential across generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(language_barrier_resolution_feasibility, empirical, 'Feasibility of EU language training reducing barriers within one generation').

omega_variable(
    fiscal_transfer_mechanism_scope,
    'Can fiscal equalization mechanisms (EU fund transfers to peripheral states) adequately compensate for brain-drain fiscal loss?',
    'Accounting analysis: tax revenue loss from high-skill emigration vs EU transfer receipt; comparison with remittance flows and return-migration rates; assessment of whether peripheral states can maintain public services (education, healthcare) with reduced tax base',
    'If adequate: peripheral state extraction perceived as lower; classification shifts toward Rope. If inadequate: extraction remains high; peripheral state continues as victim; tangled_rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_transfer_mechanism_scope, empirical, 'Whether EU fiscal transfers adequately compensate for brain-drain effects').

omega_variable(
    discrimination_enforcement_effectiveness,
    'Do existing EU anti-discrimination enforcement mechanisms actually prevent informal barriers (housing discrimination, employer preference for natives)?',
    'Audit studies: testing for differential treatment in housing/hiring across EU destinations; analysis of complaint rates vs detected discrimination; assessment of penalty severity vs likelihood of detection',
    'If enforcement effective: suppression overstated, actual barriers lower than measured. If ineffective: informal discrimination constitutes substantial hidden suppression, classification as Snare from low-skill perspective strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discrimination_enforcement_effectiveness, empirical, 'Effectiveness of EU anti-discrimination enforcement in housing and employment').

omega_variable(
    return_migration_equilibrium,
    'Is brain drain permanent or cyclical? What proportion of emigrants return within 10 years?',
    'Longitudinal tracking of EU migrant cohorts; analysis of return rates by skill level, destination country, and time period; identification of factors triggering return migration',
    'If high return rate: brain drain framing is overstated; peripheral states experience temporary labor loss, not permanent extraction. If low return rate: extraction from peripheral states is ongoing; classification as victim remains valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(return_migration_equilibrium, empirical, 'Proportion and patterns of return migration to peripheral EU states').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_mobility_within_eurozone, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lmez_tr_t0, labor_mobility_within_eurozone, theater_ratio, 0, 0.42).
narrative_ontology:measurement(lmez_tr_t10, labor_mobility_within_eurozone, theater_ratio, 10, 0.5).
narrative_ontology:measurement(lmez_tr_t20, labor_mobility_within_eurozone, theater_ratio, 20, 0.58).
narrative_ontology:measurement(lmez_tr_t5, labor_mobility_within_eurozone, theater_ratio, 5, 0.45).

% Extraction over time
narrative_ontology:measurement(lmez_be_t0, labor_mobility_within_eurozone, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lmez_be_t10, labor_mobility_within_eurozone, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(lmez_be_t20, labor_mobility_within_eurozone, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(lmez_be_t5, labor_mobility_within_eurozone, base_extractiveness, 5, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_mobility_within_eurozone, resource_allocation).
narrative_ontology:affects_constraint(labor_mobility_within_eurozone, eurozone_wage_divergence).
narrative_ontology:affects_constraint(labor_mobility_within_eurozone, peripheral_state_fiscal_pressure).
narrative_ontology:affects_constraint(labor_mobility_within_eurozone, credential_recognition_harmonization).

% DUAL FORMULATION NOTE:
% Labor mobility within the Eurozone is a composite constraint family. The formal freedom of movement (ε=0.05, Rope/Mountain) is distinct from the practical barriers (ε=0.52, Tangled Rope). Credential non-recognition is a separate constraint (ε=0.45, Tangled Rope) that partially determines the labor mobility extractiveness. Wage divergence is both cause and effect of mobility constraints. These are linked via network edges because each affects the others' structural parameters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(labor_mobility_within_eurozone, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
