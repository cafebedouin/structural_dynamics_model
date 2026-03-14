% ============================================================================
% CONSTRAINT STORY: monopsony_labor_market
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monopsony_labor_market, []).

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
 *   constraint_id: monopsony_labor_market
 *   human_readable: Monopsony Labor Market Extraction
 *   domain: economic/labor_markets
 *
 * SUMMARY:
 *   A monopsony labor market exists when a single dominant employer (or small
 *   group of employers) possesses significant wage-setting power due to
 *   geographic isolation, high worker relocation costs, or firm-specific
 *   human capital. Workers face limited outside options and cannot credibly
 *   threaten to exit, enabling the employer to suppress wages below what
 *   competitive markets would provide. The constraint exhibits as Snare from
 *   the powerless worker perspective (trapped by geography and capital
 *   lock-in), Tangled Rope from skilled workers with moderate bargaining
 *   power, Rope from the employer (who experiences labor coordination), and
 *   Scaffold from organized labor seeking collective action with a
 *   generational sunset. The extractiveness (0.58) reflects that monopsony
 *   extraction is substantial but not total — the employer must still pay
 *   above subsistence to retain workers and maintain production. Theater
 *   ratio (0.35) is low because monopsony operates through transparent wage
 *   suppression mechanisms (job offers below market rates), not through
 *   performative compliance rituals.
 *
 * KEY AGENTS:
 *   - Low-skill, geographically isolated workers: Primary victims (powerless/trapped) — bear full extraction through below-market wages and credible unemployment threat
 *   - Workers with identity fusion: Secondary victims (powerless/identity_locked) — structurally mobile but identity-bound to job; carry suppression internally after barriers dissolve
 *   - Skilled workers with bargaining power: Tertiary victims (moderate/constrained) — experience monopsony extraction but have exit options; perceive mixed coordination and extraction
 *   - Dominant regional employer: Primary beneficiary (institutional/arbitrage) — captures wage suppression through market power; experiences arrangement as labor coordination
 *   - Labor unions or worker coalitions: Organized challengers (organized/constrained) — can break monopsony power through collective action and legislative change
 *   - Regional government/regulator: Institutional stakeholder (institutional/constrained) — balances monopsony's employment provision against social costs; tolerates extraction as lesser evil
 *   - Analytical observer: Civilizational view (analytical/analytical) — identifies pure extraction rather than coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopsony_labor_market, 0.58).
domain_priors:suppression_score(monopsony_labor_market, 0.65).
domain_priors:theater_ratio(monopsony_labor_market, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopsony_labor_market, extractiveness, 0.58).
narrative_ontology:constraint_metric(monopsony_labor_market, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(monopsony_labor_market, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopsony_labor_market, snare).
narrative_ontology:human_readable(monopsony_labor_market, "Monopsony Labor Market Extraction").
narrative_ontology:topic_domain(monopsony_labor_market, "economic/labor_markets").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopsony_labor_market, dominant_employer).
narrative_ontology:constraint_victim(monopsony_labor_market, low_skill_workers).
narrative_ontology:constraint_victim(monopsony_labor_market, geographically_isolated_workers).
narrative_ontology:constraint_victim(monopsony_labor_market, workers_with_firm_specific_capital).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED WORKER (SNARE) — Faces real structural barriers to exit: relocation costs, firm-specific skills with no transferable value, geographic isolation (single dominant employer in the region), unemployment benefits insufficient to sustain family, no alternative job market. The monopsony extracts wage suppression through the credible threat of termination leading to destitution. Maximum experienced extraction — the worker perceives the constraint as unchangeable within biographical time.
constraint_indexing:constraint_classification(monopsony_labor_market, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: WORKER WITH IDENTITY FUSION (SNARE) — Structurally similar to Perspective 1 but with internalized constraint: the worker's identity is constituted through the job (meatpacking plant worker, mining operation, agricultural worker). Exit would require abandoning not just the employment relationship but the self-concept built over decades. Even if relocation were possible (mobile exit options), the worker cannot exercise it because their identity frame makes exit literally unthinkable. The constraint is internalized — the worker carries the extraction mechanism with them even if structural barriers dissolved. High suppression persists after barrier removal through cognitive entrapment.
constraint_indexing:constraint_classification(monopsony_labor_market, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: SKILLED WORKER WITH BARGAINING POWER (TANGLED ROPE) — Faces monopsony wage suppression but has some exit options: specialized skills, ability to relocate at moderate cost, potential to negotiate with multiple employers regionally. Experiences both coordination (the employer provides stable employment, training, advancement pathways) and extraction (wages below marginal product, limited upward mobility). At generational time scale, can migrate out or organize within firm. Mixed experience — asymmetric but not total.
constraint_indexing:constraint_classification(monopsony_labor_market, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: DOMINANT EMPLOYER (ROPE) — Experiences the monopsony as a coordination mechanism: the firm solves the labor aggregation problem by establishing wage floors, hiring procedures, and worker retention through modest above-subsistence payments. From the employer's perspective, the constraint coordinates labor supply and firm production. The employer has arbitrage options (can shift production, source labor elsewhere, relocate). Net beneficiary — the extraction flows toward the firm, but the firm perceives the arrangement as legitimate coordination of labor markets.
constraint_indexing:constraint_classification(monopsony_labor_market, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR UNION OR ORGANIZING COALITION (SCAFFOLD) — Organized workers can reduce the monopsony's extraction power through collective bargaining, changing the employer's exit options from arbitrage to constrained (relocation becomes costly if workers are organized regionally). The coalition sees the monopsony as a temporary coordination failure with a sunset: collective action, legislative change (minimum wage, sectoral bargaining, antitrust enforcement), or demographic shifts can break the geographic isolation that enables monopsony power. Extraction is moderate and declining over generational time.
constraint_indexing:constraint_classification(monopsony_labor_market, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: REGIONAL GOVERNMENT OR REGULATOR (TANGLED ROPE) — The state or regional authority faces a mixed constraint: the monopsony coordinates labor supply and production (benefiting local tax base, employment statistics) while extracting from workers (creating social costs, political instability, health outcomes). Government has constrained exit options (cannot easily replace dominant employer without economic shock) and sees both coordination (labor is supplied) and extraction (wages below fair value). May enforce or tolerate monopsony as lesser evil relative to mass unemployment.
constraint_indexing:constraint_classification(monopsony_labor_market, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational/global view, the monopsony constraint exhibits pure extraction with minimal coordination benefits: wages below marginal product, suppressed through geographic isolation and threat of unemployment. The constraint's primary function is wealth transfer from workers to capital, not solving a genuine coordination problem. The 'coordination' framing (Perspectives 4 and 6) is a post-hoc justification for asymmetric extraction.
constraint_indexing:constraint_classification(monopsony_labor_market, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopsony_labor_market_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monopsony_labor_market, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monopsony_labor_market, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(monopsony_labor_market, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monopsony_labor_market_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The monopsony suppresses wages through credible threat of unemployment and worker relocation infeasibility. Empirical estimates suggest wage gaps of 10-30% below competitive market levels depending on worker skill and regional isolation. The extraction is not total (not 0.80+) because the employer must maintain wage floors above subsistence and some workers retain bargaining power through skills or alternative opportunities. Over the measurement interval (0-20 years), extractiveness has risen from 0.42 to 0.58, reflecting increasing employer consolidation and weakening worker outside options (declining regional wage competition, reduced interstate migration). Suppression (0.65): High. Structural barriers include relocation costs, firm-specific skill atrophy if workers leave, unemployment insurance gaps, and geographic isolation. Additionally, suppression includes internalized components — workers come to accept below-market wages as normal, reducing their willingness to pursue exit even when it becomes feasible. Theater ratio (0.35): Low. Monopsony operates through direct wage offers, not through performative compliance rituals. The employer does not need to disguise extraction through theater — geographic isolation provides credible exit threat without additional ritual. Theater has increased slightly over time as employers have adopted HR framing of 'competitive compensation' and 'benefits packages' to justify suppression.
 *
 * PERSPECTIVAL GAP:
 *   The Rope perspective (employer) and Snare perspective (trapped worker) represent opposite poles of directionality and experienced extraction. The employer benefits from the geographic isolation that traps workers, experiencing wage suppression as legitimate profit margin rather than extraction. Workers experience the same wage offer as extractive suppression with no alternative. The Tangled Rope (skilled worker) represents a middle position where both coordination and extraction are structurally present. The Scaffold (organized labor) perspective reframes the constraint as temporary, revealing the structural conditions that generate monopsony (geographic isolation, regulatory barriers to employer mobility, sectoral consolidation) as changeable through collective action and policy. The identity-locked worker perspective reveals that suppression persists through internalized mechanisms even if structural barriers dissolved — the constraint's true depth is cognitive, not just material. The analytical observer perspective challenges all others' framing by identifying the 'coordination' function as post-hoc justification for pure extraction: the employer does not need workers to coordinate production at below-market wages; the employer uses geographic isolation to extract rents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status and exit options per the constraint pipeline. Powerless trapped workers show d ≈ 0.95 → high f(d) ≈ 1.42 → high experienced extractiveness chi. Identity-locked workers show d ≈ 0.89 → f(d) ≈ 1.28 → high chi (identity-bound workers may be structurally more mobile than trapped workers, but identity fusion prevents exercise of mobility, so effective exit is even more constrained). Skilled workers with constrained exit show d ≈ 0.55 → f(d) ≈ 0.75 → moderate chi. Employers with arbitrage options show d ≈ 0.15 → f(d) ≈ -0.01 → negative/near-zero chi (beneficiaries experience the constraint as coordination, not extraction). Organized labor with constrained options shows d ≈ 0.40 → f(d) ≈ 0.40 → low-moderate chi (coalition has agency and exit path). Regional government with institutional constrained shows d ≈ 0.60 → f(d) ≈ 0.90 → moderate-high chi (government is partially trapped by employer dependence, partially benefits from employment provision). Analytical observer shows d ≈ 0.72 → f(d) ≈ 1.15 → high chi (sees pure extraction rather than coordination).
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE VS TANGLED ROPE CLASSIFICATION: The constraint classifies as Snare from the trapped worker perspective (ε > 0.46, suppression high, χ high) but as Tangled Rope from the skilled worker and government perspectives (suppression high, but coordination function present alongside extraction). The mandatrophy is resolved by recognizing that these are legitimate perspectival differences: for isolated low-skill workers, the constraint is pure extraction (employer provides no coordination benefit beyond subsistence wages); for skilled workers and government, the constraint has genuine coordination content (stable employment, production coordination) alongside extraction. The Snare classification at Perspective 1 is correct — not a false positive for pure extraction. The Rope classification at Perspective 4 (employer) is also correct from the employer's structural position. The constraint genuinely exhibits multiple types from different observational positions. The mandatrophy does not arise here because the claimed_type (snare) matches the primary victim perspective, which is the canonical position for classification when a constraint has identifiable targets.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geographic_isolation_source,
    'Is monopsony power primarily caused by geographic isolation (exogenous), employer deliberate strategy (endogenous), or lock-in from worker investment in firm-specific capital?',
    'Historical analysis of employer movement patterns; worker migration data before/after employer entry; competitor entry barriers; job search cost measurement',
    'If primarily geographic: monopsony is Rope or Tangled Rope depending on relocation feasibility. If primarily strategic: monopsony is pure Snare. If primarily lock-in: classification depends on whether workers foresee the trap at entry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_isolation_source, empirical, 'Whether monopsony power is exogenous geographic isolation or endogenous employer strategy').

omega_variable(
    suppression_mechanism_internalization,
    'What proportion of measured suppression is structural (material barriers to exit) versus internalized (cognitive/identity-based entrapment that persists after barriers dissolve)?',
    'Post-exit suppression trajectory: track workers who leave monopsony regions; measure wage recovery, mobility confidence, and identity recalibration over 2-5 year periods. If suppression persists after barrier removal, classify as internalized.',
    'High internalization → constraint''s true suppression is higher than measured. Workers export the extraction mechanism with them after exit. Organizing strategy must address cognitive capture alongside material barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized suppression in monopsony labor markets').

omega_variable(
    marginal_product_verification,
    'Can worker marginal product be empirically established, or does firm-specific capital make the MPL calculation indeterminate?',
    'Wage gap analysis using comparable workers in non-monopsony markets; task-level productivity measurement; counterfactual wage estimates. If MPL cannot be pinned down, the claim of ''below marginal product'' becomes harder to verify.',
    'If MPL is verifiable: extractiveness ≥ 0.50 (clear below-MPL extraction). If indeterminate: extractiveness may be lower; employer may be capturing rents from firm-specific capital rather than pure wage suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(marginal_product_verification, empirical, 'Empirical verification of worker marginal product in monopsony markets').

omega_variable(
    collective_action_feasibility,
    'Can workers organize to break monopsony power, or are organizing costs prohibitive relative to individual exit?',
    'Historical cases of successful unionization in monopsony regions; organizing cost measurement; comparison of unionization rates in single-employer vs multi-employer regions',
    'If organizing is feasible: Scaffold perspective is realistic — collective action can establish sunset. If prohibitive: Snare classification is more accurate — workers must exit individually or remain trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_feasibility, empirical, 'Feasibility of collective action in breaking monopsony power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopsony_labor_market, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monop_tr_t0, monopsony_labor_market, theater_ratio, 0, 0.28).
narrative_ontology:measurement(monop_tr_t10, monopsony_labor_market, theater_ratio, 10, 0.32).
narrative_ontology:measurement(monop_tr_t20, monopsony_labor_market, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(monop_be_t0, monopsony_labor_market, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(monop_be_t10, monopsony_labor_market, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(monop_be_t20, monopsony_labor_market, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopsony_labor_market, resource_allocation).
narrative_ontology:affects_constraint(monopsony_labor_market, labor_market_segmentation).
narrative_ontology:affects_constraint(monopsony_labor_market, firm_specific_capital_lock_in).
narrative_ontology:affects_constraint(monopsony_labor_market, geographic_wage_dispersion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monopsony_labor_market, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
