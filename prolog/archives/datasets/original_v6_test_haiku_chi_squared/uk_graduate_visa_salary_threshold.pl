% ============================================================================
% CONSTRAINT STORY: uk_graduate_visa_salary_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_graduate_visa_salary_threshold, []).

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
 *   constraint_id: uk_graduate_visa_salary_threshold
 *   human_readable: UK Graduate Visa Minimum Salary Threshold
 *   domain: economic/political
 *
 * SUMMARY:
 *   The UK Graduate Visa Minimum Salary Threshold represents a hybrid
 *   constraint combining labour market protection rhetoric with extraction
 *   mechanisms. The rule requires international graduates to secure
 *   employment at or above a specified salary threshold (£30,000 in 2024,
 *   indexed to inflation) to extend their work visa beyond the initial
 *   two-year period. The constraint exhibits high structural asymmetry: it
 *   protects high-wage employers and domestic labour supply while extracting
 *   value from international graduates through constrained employment
 *   negotiation and from lower-wage sectors through artificial labour
 *   scarcity. The theatre ratio (0.45) reflects moderate performative content
 *   — the constraint is partly genuine labour policy and partly extraction
 *   mechanism using labour protection as justification. The extractiveness
 *   has risen over the interval as the policy shifted from facilitating
 *   post-study work to restricting it, and as awareness of its exclusionary
 *   effects on shortage sectors has increased.
 *
 * KEY AGENTS:
 *   - International Graduates: Primary victims (powerless/trapped) — face binary choice between meeting salary threshold or losing visa status; constrained in wage negotiation by visa dependency
 *   - UK Domestic Workers: Secondary beneficiaries (institutional/arbitrage) — experience labour market protection through restricted international competition; stabilized wage floors in graduate roles
 *   - Lower-Wage Sector Employers: Secondary victims (moderate/constrained) — excluded from international graduate talent pool; forced to either raise wages beyond market levels or accept vacancy; examples: nursing, social care, hospitality, teaching
 *   - High-Wage Employers: Primary beneficiaries (institutional/arbitrage) — access to qualified talent with restricted negotiating power; no threat from lower-wage competition; compliance costs minimal
 *   - UK Higher Education Sector: Institutional actor (institutional/constrained) — originally benefited from Graduate Visa as recruitment tool; now faces declining international applications and revenue loss; constrained by immigration policy divergence from educational interests
 *   - Analytical Observer: (analytical/analytical) — sees structural extraction mechanism masked as labour protection policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_graduate_visa_salary_threshold, 0.58).
domain_priors:suppression_score(uk_graduate_visa_salary_threshold, 0.68).
domain_priors:theater_ratio(uk_graduate_visa_salary_threshold, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_graduate_visa_salary_threshold, extractiveness, 0.58).
narrative_ontology:constraint_metric(uk_graduate_visa_salary_threshold, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(uk_graduate_visa_salary_threshold, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_graduate_visa_salary_threshold, snare).
narrative_ontology:human_readable(uk_graduate_visa_salary_threshold, "UK Graduate Visa Minimum Salary Threshold").
narrative_ontology:topic_domain(uk_graduate_visa_salary_threshold, "economic/political").

domain_priors:requires_active_enforcement(uk_graduate_visa_salary_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_graduate_visa_salary_threshold, uk_domestic_labour_supply).
narrative_ontology:constraint_beneficiary(uk_graduate_visa_salary_threshold, high_wage_employers).
narrative_ontology:constraint_victim(uk_graduate_visa_salary_threshold, international_graduates).
narrative_ontology:constraint_victim(uk_graduate_visa_salary_threshold, lower_wage_sectors).
narrative_ontology:constraint_victim(uk_graduate_visa_salary_threshold, uk_higher_education_revenue).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTERNATIONAL GRADUATE (SNARE) — Faces binary choice: secure qualifying salary or lose right to remain. Trapped by visa rules, cannot negotiate employment terms. Significant extraction of labor value through salary suppression (employers know graduates need visas). d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.82.
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOWER-WAGE SECTOR EMPLOYER (SNARE) — Excluded from accessing international graduate talent (hospitality, social care, nursing, retail). Constrained by salary threshold; cannot compete on non-wage terms. Faces dual extraction: excluded from labour pool AND required to raise wages for domestic workers. d≈0.78, f(d)≈1.18, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH-WAGE EMPLOYER (ROPE) — Benefits from restricted labour supply and legitimized wage floors. Sees constraint as coordination mechanism: salary threshold signals stable workforce access and competitive labor standards. Can absorb threshold requirement easily. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: UK DOMESTIC LABOUR FORCE (ROPE) — Primary beneficiary of restricted international competition. Constraint appears as labour market protection — reduces downward wage pressure in graduate-level roles. Coordination function: establishes floor for graduate employment standards. d≈0.12, f(d)≈0.00, σ=1.0 → χ≈0.00. Neutral to positive.
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: HIGHER EDUCATION SECTOR (PITON) — Originally benefited from Graduate Visa as recruitment tool (international students attracted by work rights). Now sees revenue threat — fewer international applicants. Constraint appears as degraded institution: visa rules persist through immigration policy logic, but the original coordination function (attracting talent to UK) has atrophied. theater_ratio=0.45 is borderline; sector maintains performative compliance with visa rules while their strategic interest has shifted to advocating exemptions. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.34.
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: COMPETITIVE GLOBAL ECONOMY (TANGLED ROPE) — UK salary thresholds create structural asymmetry: restrict inbound talent but also reduce UK's attractiveness relative to Canada, Australia, Germany. Constraint exhibits dual function: protects domestic labour (coordination) AND extracts value through reduced international competitiveness. Mobile exit option (graduates relocate to alternative destinations) makes effective extraction visible. d≈0.45, f(d)≈0.45, σ=1.2 → χ≈0.31.
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — Structural analysis reveals pure extraction masked as labour market protection. Salary threshold exceeds median graduate earnings in sectors experiencing labour shortages (nursing, teaching, social care), creating artificial scarcity. Suppression is high: graduates have few alternatives (other countries have own visa restrictions), and UK rules are enforced through visa revocation. Engine derives χ≈0.82 from ε=0.58, suppression=0.68, beneficiary/victim structure. Classification: Snare, not labour protection. d≈0.90, f(d)≈1.38, σ=1.2 → χ≈0.98.
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_graduate_visa_salary_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_graduate_visa_salary_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_graduate_visa_salary_threshold, TR),
    TR >= 0.70.

:- end_tests(uk_graduate_visa_salary_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The salary threshold creates direct extraction through constrained wage negotiation (international graduates accept lower offers to meet threshold, then visa becomes dependent on that salary). Secondary extraction occurs through sectoral exclusion — employers in lower-wage sectors face artificial labour shortage and must either raise wages (cost extraction) or accept vacancies (productivity extraction). The value of 0.58 reflects that extraction is not absolute (high-wage employers can easily comply; some sectors can substitute with domestic workers) but is substantial and asymmetric. Suppression (0.68): High. International graduates have severely limited exit options: visa revocation, forced to lower-wage roles, or relocation to other countries. Alternatives are blocked by threshold (cannot accept lower-wage role and keep visa; cannot negotiate for flexible compliance). Exit options are trapped or exit UK entirely — no middle ground. Suppression is elevated by the fact that UK enforces the rule through revocation and that equivalent labour markets (Canada, Australia, Germany) have alternative visa pathways creating competitive pressure but not relief within UK system. Theater ratio (0.45): Moderate. The constraint is framed as labour market protection and wage floor enforcement, which provides genuine coordination logic (protects domestic workers from downward wage pressure). However, the performative element is substantial: the threshold height excludes major labour shortage sectors where protection is unnecessary (surplus of UK workers in nursing, social care). The policy thus appears as labour protection but functions partly as talent exclusion. Rising theatre over the interval reflects increasing awareness that shortage sectors are excluded despite claims of labour market balance.
 *
 * PERSPECTIVAL GAP:
 *   Massive perspectival divergence. High-wage employers (institutional/arbitrage) see a coordination mechanism establishing stable labour standards — they experience the constraint as Rope with negative effective extraction (they benefit from restricted competition). Domestic labour force sees labour protection (Rope). International graduates see a trap with no exit (Snare) — they are forced into wage suppression to meet the threshold. Lower-wage sectors see exclusion from the labour market (Snare). The higher education sector (institutional/constrained) sees institutional degradation (Piton) — the Graduate Visa was originally their recruitment tool, but immigration policy tightening has made it a liability, and the sector now cannot escape it (constrained exit). The global competitive view (powerful/mobile) reveals extraction through talent redistribution — UK loses competitive advantage by restricting inbound talent. The analytical observer sees pure extraction mechanism disguised as policy. The perspectival gap between high-wage employers (who see Rope) and international graduates (who see Snare) is the maximum possible structural disagreement: the same rule is coordination for one and extraction for the other.
 *
 * DIRECTIONALITY LOGIC:
 *   International graduates: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction directionality. Cannot exit without losing visa; cannot negotiate because visa depends on salary acceptance. Lower-wage sector employers: Victim + constrained → d≈0.78, f(d)≈1.18. High extraction directionality. Constrained by inability to access talent without meeting threshold; forced to raise wages or accept vacancies. UK domestic labour: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.02. Net beneficiary directionality. Restricted competition creates wage floor protection; can exit by accepting lower-wage roles (arbitrage option). High-wage employers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary directionality. Restricted labour supply increases their bargaining power; compliance costs minimal; can exit by raising wages marginally. Higher education sector: Mixed victim/beneficiary, constrained → d≈0.55, f(d)≈0.75. Originally beneficiary (recruitment tool), now victim (declining applications). Constrained by inability to change policy. Global competitive view: Mixed beneficiary/victim, mobile → d≈0.45, f(d)≈0.45. Mobile exit option (talent redistributes globally) makes extraction visible; constraint is both labour protection (coordination) and talent exclusion (extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL MANDATROPHY CASE: The constraint exhibits classic mandate drift — policy framed as labour market protection and wage floor establishment but functions as pure talent extraction at the powerless agent level (international graduates) and labour scarcity extraction at the sectoral level (shortage sectors). The mandatrophy is resolved by perspectival decomposition: the policy IS genuine labour protection from the domestic labour perspective (Rope classification is correct) AND genuine extraction from the international graduate perspective (Snare classification is correct). The false mandate is the claim that the constraint serves both perspectives equally. In reality, it strongly serves domestic labour interests at the expense of international graduates and shortage sectors. The theatre ratio (0.45) indicates the policy is not purely performative — there is real coordination logic protecting domestic wages — but the performative element (framing as 'protecting all workers' when it actually protects only high-wage sectors) is substantial. The mandatrophy resolves when the constraint is decomposed into its actual constituencies: constraint benefits domestic labour in high-wage sectors (genuine Rope) and extracts from international graduates and lower-wage sectors (genuine Snare). No single classification is correct because the constraint operates on two structurally different populations with opposite interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_wage_effect_causality,
    'Does the salary threshold meaningfully increase wages for UK domestic workers, or does it merely exclude international workers without raising domestic compensation?',
    'Longitudinal wage analysis for graduate roles pre/post threshold introduction, controlling for sector, region, and employer size. Comparison to equivalent labour markets (EU, Australia) with different visa policies.',
    'If threshold raises domestic wages: constraint has genuine labour protection function (Rope from domestic perspective). If threshold excludes without raising wages: constraint is pure extraction with minimal coordination benefit (Snare from all perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_wage_effect_causality, empirical, 'Whether salary threshold raises domestic wages or merely excludes international workers').

omega_variable(
    labour_shortage_sector_coverage,
    'What percentage of labour shortage sectors (nursing, social care, hospitality) fall below the salary threshold, creating artificial exclusion vs addressing genuine excess supply?',
    'Sector-by-sector analysis: recruitment gap data, vacancy rates, wage elasticity of supply for roles below threshold. Comparison of threshold height to sectoral median graduate wages.',
    'If threshold excludes majority of shortage sectors: constraint is extraction-focused (Snare dominates). If threshold aligns with genuine labour oversupply: constraint is coordination-focused (Rope dominates).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labour_shortage_sector_coverage, empirical, 'Coverage of labour shortage sectors by salary threshold').

omega_variable(
    international_talent_redistribution,
    'Where do excluded UK graduate visa applicants relocate? Do they represent permanent loss of talent to UK economy, or do alternative visa pathways (skilled worker visas) capture them at higher cost?',
    'Tracking international graduate destinations pre/post threshold change; analysis of alternative visa route applications; longitudinal tax/NI records for cohorts affected by threshold.',
    'If redistribution to alternative (more expensive) visas: constraint is revenue-extraction (forces employers to sponsor more costly skilled worker routes). If permanent loss: constraint is talent-destructive (reduces long-term economic productivity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_talent_redistribution, empirical, 'Where excluded international graduates relocate and whether redistribution occurs').

omega_variable(
    threshold_inflation_rate,
    'Will salary threshold be indexed to inflation or adjusted for wage growth, or will nominal threshold values create increasing exclusion over time?',
    'Policy documentation; historical precedent from previous visa schemes; government inflation adjustment protocols.',
    'If indexed to inflation: constraint is stable labour protection (Rope, lower χ). If nominal and static: constraint becomes increasingly extractive as real wages grow (evolution toward pure Snare, rising χ).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_inflation_rate, conceptual, 'Whether salary threshold is indexed to inflation or static').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_graduate_visa_salary_threshold, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ukvisa_tr_t0, uk_graduate_visa_salary_threshold, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ukvisa_tr_t2, uk_graduate_visa_salary_threshold, theater_ratio, 2, 0.42).
narrative_ontology:measurement(ukvisa_tr_t4, uk_graduate_visa_salary_threshold, theater_ratio, 4, 0.45).

% Extraction over time
narrative_ontology:measurement(ukvisa_be_t0, uk_graduate_visa_salary_threshold, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ukvisa_be_t2, uk_graduate_visa_salary_threshold, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(ukvisa_be_t4, uk_graduate_visa_salary_threshold, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_graduate_visa_salary_threshold, resource_allocation).
narrative_ontology:affects_constraint(uk_graduate_visa_salary_threshold, uk_skilled_worker_visa_route).
narrative_ontology:affects_constraint(uk_graduate_visa_salary_threshold, international_student_recruitment_policy).
narrative_ontology:affects_constraint(uk_graduate_visa_salary_threshold, healthcare_sector_labour_shortage).

% DUAL FORMULATION NOTE:
% The salary threshold constraint decomposes into two structurally distinct claims: (1) labour market protection for UK domestic workers (ε≈0.15, Rope), which is genuine coordination; (2) talent extraction from international graduates (ε≈0.68, Snare), which is pure extraction. The aggregate ε=0.58 reflects these as a hybrid mechanism. Upstream constraints (student recruitment policy, visa route design) have different ε values reflecting the original intention (facilitate post-study work); downstream constraints (healthcare labour shortage, wage floor effects) depend on this threshold's extraction effect. The constraint family reflects how immigration policy became a resource allocation mechanism by accident (policy tightening for border control purposes) rather than by design (labour market policy intention).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
