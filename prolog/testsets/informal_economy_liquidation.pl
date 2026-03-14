% ============================================================================
% CONSTRAINT STORY: informal_economy_liquidation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_informal_economy_liquidation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: informal_economy_liquidation
 *   human_readable: Informal Economy Liquidation through Formalization Policy
 *   domain: economic_policy/labor/development
 *
 * SUMMARY:
 *   Informal economy liquidation through formalization policy represents a
 *   systematic extraction mechanism disguised as development coordination.
 *   Workers in informal sectors — street vendors, domestic workers, day
 *   laborers, micro-entrepreneurs — comprise 50-90% of employment in many
 *   developing economies and represent the primary income source for the
 *   global poor. Formalization policy, promoted by multilateral development
 *   institutions and adopted by national governments, mandates registration,
 *   licensing, tax payment, and regulatory compliance for informal workers.
 *   The stated coordination objective is genuine: bringing workers into the
 *   tax system, extending labor protections, and formalizing the economy. The
 *   extraction mechanism is equally real: formalization costs (licensing
 *   fees, equipment, compliance infrastructure, continuous regulatory
 *   payments) are unaffordable for workers earning $2-5 per day.
 *   Non-compliance triggers confiscation of goods, fines, market exclusion,
 *   and detention. Enforcement is asymmetric — targeting workers rather than
 *   employers or tax evasion by formal sector — creating suppression that
 *   prevents exit or organizing. The constraint exhibits dramatic
 *   perspectival divergence: the vendor sees pure extraction (Snare), the tax
 *   authority sees coordination (Rope), the organized coalition sees mixed
 *   coordination-extraction (Tangled Rope), and the international development
 *   framework sees this as an immutable feature of economic modernization
 *   (Piton + false Mountain).
 *
 * KEY AGENTS:
 *   - Informal Sector Workers: Primary victims (powerless/trapped) — street vendors, domestic workers, day laborers, micro-entrepreneurs in informal employment with no labor protections
 *   - Tax Authority / Regulatory Agencies: Primary beneficiary (institutional/arbitrage) — captures expanded tax base, achieves regulatory coverage, solves coordination problem of bringing workers into formal system
 *   - Formal Sector Employers: Secondary beneficiary (powerful/mobile) — eliminates informal competition, secures labor supply from formalization-displaced workers
 *   - Informal Worker Coalition: Secondary victim/organized agent (organized/constrained) — unions, cooperatives, NGOs attempting to negotiate and organize; perceive both coordination benefits and extraction asymmetry
 *   - International Development Institutions: Institutional actor (institutional/arbitrage) — World Bank, IMF, bilateral donors promoting formalization as development best practice; capture through policy framework rather than direct enforcement
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy-contingent extraction as immutable feature of economic development
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(informal_economy_liquidation, 0.68).
domain_priors:suppression_score(informal_economy_liquidation, 0.75).
domain_priors:theater_ratio(informal_economy_liquidation, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(informal_economy_liquidation, extractiveness, 0.68).
narrative_ontology:constraint_metric(informal_economy_liquidation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(informal_economy_liquidation, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(informal_economy_liquidation, snare).
narrative_ontology:human_readable(informal_economy_liquidation, "Informal Economy Liquidation through Formalization Policy").
narrative_ontology:topic_domain(informal_economy_liquidation, "economic_policy/labor/development").

domain_priors:requires_active_enforcement(informal_economy_liquidation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(informal_economy_liquidation, formal_sector_employers).
narrative_ontology:constraint_beneficiary(informal_economy_liquidation, tax_collecting_authorities).
narrative_ontology:constraint_beneficiary(informal_economy_liquidation, regulatory_agencies).
narrative_ontology:constraint_victim(informal_economy_liquidation, informal_sector_workers).
narrative_ontology:constraint_victim(informal_economy_liquidation, street_vendors).
narrative_ontology:constraint_victim(informal_economy_liquidation, domestic_workers).
narrative_ontology:constraint_victim(informal_economy_liquidation, micro_entrepreneurs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFORMAL VENDOR (SNARE) — Trapped without viable exit. Formalization requires capital for licensing, equipment, compliance infrastructure, and continuous regulatory payments. Non-compliance triggers confiscation, fines, and exclusion from formal markets. The vendor cannot afford formalization costs, cannot maintain informal status without penalty risk, and cannot escape geographic jurisdiction. Maximum experienced extraction with suppression of alternatives.
constraint_indexing:constraint_classification(informal_economy_liquidation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOMESTIC WORKER (SNARE) — Trapped in informal employment with no labor protections, no ability to negotiate wages or conditions, no recourse for wage theft. Formalization requires employer cooperation (which dissolves the wage advantage employers seek) and documentation often unavailable to migrants or undocumented workers. Suppression operates through legal status barriers and lack of organizing capacity.
constraint_indexing:constraint_classification(informal_economy_liquidation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: TAX AUTHORITY (ROPE) — Experiences formalization policy as solving a genuine coordination problem: bringing informal workers into the tax/regulatory system increases revenue and extends social safety net coverage. The authority benefits from captured workers and expanded tax base. Sees the constraint as legitimate coordination mechanism, not extraction.
constraint_indexing:constraint_classification(informal_economy_liquidation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INFORMAL WORKER COALITION (TANGLED ROPE) — Organized groups (unions, cooperatives, NGOs) perceive both genuine coordination benefits (formalization creates labor protections, enforces minimum wages) and asymmetric extraction (formalization costs are passed entirely to workers; regulatory capture ensures enforcement targets workers, not employers). Coalition has some agency and can negotiate, but faces structural constraints in enforcement asymmetry.
constraint_indexing:constraint_classification(informal_economy_liquidation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL SECTOR EMPLOYER (TANGLED ROPE) — Benefits from formalization policy by eliminating informal competition and ensuring labor supply moves into regulated employment. Perceives formalization as coordinating labor market. But also subject to regulation, wage floors, and labor law enforcement. Experiences mixed coordination and extraction — genuine coordination gains tempered by enforcement asymmetry favoring workers when politically salient.
constraint_indexing:constraint_classification(informal_economy_liquidation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: DEVELOPMENT POLICY FRAMEWORK (PITON) — International development institutions (World Bank, IMF, bilateral donors) have promoted formalization policy for decades as best practice. The policy persists through institutional inertia and donor requirement despite limited evidence of success and documented harm to vulnerable workers. Theater ratio reflects that formalization rhetoric ('bringing workers into the system,' 'expanding tax base') persists even as implementation concentrates harm on powerless agents.
constraint_indexing:constraint_classification(informal_economy_liquidation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / ECONOMIC NECESSITY VIEW (MOUNTAIN) — From the analytical/civilizational perspective, some formalization is necessary for modern economies: tax collection, labor protections, and infrastructure provision require identifying workers and employers. This perspective sees the extraction as an immutable feature of economic organization — the cost of civilization. However, the structural data reveals this as naturalization: the extraction concentrates on the most vulnerable; formalization could occur with subsidy or gradual transition rather than immediate enforcement.
constraint_indexing:constraint_classification(informal_economy_liquidation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(informal_economy_liquidation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(informal_economy_liquidation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(informal_economy_liquidation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(informal_economy_liquidation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(informal_economy_liquidation, TR),
    TR >= 0.70.

:- end_tests(informal_economy_liquidation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The constraint exhibits accelerating extraction over the interval. Initial value (0.35) reflects modest formalization pressure and widespread informal evasion capacity. As implementation intensifies (through technological surveillance, digital payment tracking, agency coordination), extraction increases to 0.55 at midpoint and 0.68 at final measurement. The trajectory shows policy deepening rather than stabilization. Suppression (0.75): Very high. Multiple mechanisms suppress alternatives: (1) Legal status — informal workers cannot legally operate; (2) Capital barriers — formalization costs are prohibitive; (3) Enforcement asymmetry — authorities target workers, not employers or formal sector tax evasion; (4) Organizing barriers — informal workers lack stable workplaces, union infrastructure, and legal protection for collective action; (5) Geographic barriers — informal workers are dispersed, making coordination difficult; (6) Migration status — many informal workers lack legal residency, creating additional vulnerability. Theater ratio (0.55): Moderate-high. The rhetoric of formalization emphasizes development goals ('expanding tax base,' 'extending labor protections,' 'bringing workers into the system') while implementation concentrates on extraction. Theater has been increasing as policy rhetoric intensifies (development success narratives) while actual worker harm has become more apparent, creating a gap between stated and actual function.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between vendor and authority is maximal. The vendor (powerless/trapped) perceives extraction with zero exit options; the authority (institutional/arbitrage) perceives coordination with pure benefit. This gap is the engine detecting structure: the constraint's function is entirely reversed depending on whether you are the beneficiary or the victim. The authority sees 'bringing informal workers into the system' as coordination success; the vendor sees 'being forced into unaffordable compliance' as extraction. Both perceptions are accurate — they perceive different aspects of the same mechanism. The gap reveals that the constraint is not truly coordination (which would benefit both parties) but rather redistribution of power and resources. The authority's Rope classification is correct from their structural position; the vendor's Snare classification is correct from theirs. The contradiction is not a measurement error — it is a signal that the constraint's function is fundamentally asymmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is computed from beneficiary/victim status and exit options. Informal workers are victims (classified in base_properties.victims) with trapped exit options — they experience high d (≈0.95) and high f(d) ≈1.42, producing severe χ. Tax authorities are beneficiaries with arbitrage options — they experience low d (≈0.05) and negative f(d) ≈-0.12, producing negative χ (the policy subsidizes them). This computation reveals the asymmetry: the policy extracts from those who cannot exit and benefits those with maximum flexibility. The scope modifier σ(S)=1.0 for national scope does not change the directionality computation but reflects that the constraint operates across the full national economy (not local, not global). The suppression metric (0.75) is unscaled — it applies equally to all perspectives, reflecting that the structural barriers (legal status, capital, enforcement risk) are binding for all informal workers regardless of power level.
 *
 * MANDATROPHY ANALYSIS:
 *   Informal economy liquidation resolves mandatrophy through clarity about what coordination problem formalization actually solves and for whom. The stated problem is economic modernization — formalizing the economy, expanding the tax base, extending labor protections. This is genuine coordination. But formalization achieves these goals through extraction from informal workers (who bear all costs) rather than through subsidized transition or employer-side enforcement (which would be true coordination). The extractive design reveals that the constraint serves not the stated coordination goal but rather the distributional goal of transferring workers from informal (low-tax, low-cost) to formal (high-tax, higher-cost) employment while capturing the captured workers for formal sector labor supply. This is mandatrophy resolution: the constraint is Snare (not Tangled Rope) because the coordination benefit is incidental — the primary function is extraction. A genuine Tangled Rope would involve burden-sharing (subsidized formalization, gradual enforcement, employer accountability), not worker-targeted enforcement. The piton perspective on development policy reveals how institutional inertia sustains a policy with documented harm — formalization has been promoted for 40+ years with limited success and clear negative outcomes for vulnerable workers, yet donor requirements perpetuate it. The false Mountain perspective naturalizes policy as inevitable economic law, preventing recognition that alternative designs are possible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formalization_feasibility_threshold,
    'Below what income level does formalization cost exceed the worker''s ability to absorb without reducing consumption below subsistence?',
    'Empirical household budget analysis correlating formalization compliance costs to worker income distribution; tracking of exit from informal employment following formalization campaigns',
    'If threshold is high (common finding): formalization is economically impossible for majority of informal workers, converting the policy from coordination to pure extraction. If threshold is low: formalization may be feasible with targeted support, suggesting tangled_rope classification is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formalization_feasibility_threshold, empirical, 'Feasibility threshold for informal worker formalization').

omega_variable(
    enforcement_asymmetry_mechanism,
    'Do regulatory agencies enforce formalization requirements against employers and informal workers equally, or do enforcement costs and political barriers result in targeting workers preferentially?',
    'Audit of enforcement actions: ratio of employer penalties to worker penalties; tracking of confiscation vs wage recovery cases; political economy analysis of which violations trigger response',
    'If enforcement is symmetric: coordination mechanism is genuine. If asymmetric toward worker targeting: snare classification is accurate and suppression floor should increase.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_asymmetry_mechanism, empirical, 'Enforcement asymmetry in formalization policy implementation').

omega_variable(
    alternative_formalization_pathways,
    'Are there feasible subsidized, gradual, or delegated formalization pathways that preserve informal worker agency and income levels while achieving fiscal/regulatory objectives?',
    'Comparative policy analysis across countries; pilot programs testing subsidized compliance, employer-side enforcement, or cooperative formalization; measurement of worker welfare outcomes under alternative designs',
    'If alternatives exist: policy choice to use coercive rather than supportive formalization reveals intentional extraction. If no alternatives work: snare classification is structural rather than contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_formalization_pathways, empirical, 'Existence of feasible alternative formalization pathways').

omega_variable(
    formal_sector_competition_spillover,
    'Does formalization policy primarily benefit formal sector employers through elimination of informal competition, or does labor protectionvalue offset this competitive capture?',
    'Economic analysis of formal sector profit margins, labor cost changes, and competitive displacement following formalization campaigns; measurement of worker wage and condition changes in formal sector post-formalization',
    'If formal sector dominates benefit: formalization is primarily an extraction mechanism enabling formal sector rent capture. If workers benefit through protected wages: tangled_rope classification is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_sector_competition_spillover, empirical, 'Formal sector capture vs. worker protection spillover from formalization').

omega_variable(
    identity_lock_versus_constraint,
    'To what extent do informal workers accept formalization constraints as legitimate (identity-locked, internalizing policy''s framing as natural/necessary) versus perceiving them as external barriers to evade?',
    'Qualitative research (interviews, focus groups) on worker perceptions of formalization legitimacy; measurement of compliance behavior (evasion rates, bribing officials, exit from markets) vs. internalized acceptance; tracking of narrative framing shifts over time',
    'If primarily identity_locked: suppression is higher (internalized), worker organizing is weaker, and exit becomes even more difficult psychologically. If primarily perceived as external constraint: suppression is lower, organizing potential is higher, and reclassify toward lower suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_versus_constraint, empirical, 'Identity lock vs. external constraint perception in formalization acceptance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(informal_economy_liquidation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(info_tr_t0, informal_economy_liquidation, theater_ratio, 0, 0.4).
narrative_ontology:measurement(info_tr_t5, informal_economy_liquidation, theater_ratio, 5, 0.5).
narrative_ontology:measurement(info_tr_t10, informal_economy_liquidation, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(info_be_t0, informal_economy_liquidation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(info_be_t5, informal_economy_liquidation, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(info_be_t10, informal_economy_liquidation, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(informal_economy_liquidation, resource_allocation).
narrative_ontology:boltzmann_floor_override(informal_economy_liquidation, 0.18).
narrative_ontology:affects_constraint(informal_economy_liquidation, labor_market_segmentation).
narrative_ontology:affects_constraint(informal_economy_liquidation, tax_evasion_incentive_structure).
narrative_ontology:affects_constraint(informal_economy_liquidation, subsistence_income_constraint).

% DUAL FORMULATION NOTE:
% Informal economy liquidation is downstream of labor market segmentation (which creates informal employment as rational response to formal sector barriers) but represents a distinct structural constraint with its own extractiveness trajectory. The constraint family includes: (1) labor_market_segmentation (ε=0.15, Rope) — the initial coordination/segmentation mechanism; (2) informal_economy_liquidation (ε=0.68, Snare) — the policy response that turns workers into extraction targets; (3) post_formalization_precarity (ε=0.55, Tangled Rope) — the outcome for workers who formalize and face new extraction mechanisms through formal sector labor conditions. Each story has distinct base properties, perspectives, and policy implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(informal_economy_liquidation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
