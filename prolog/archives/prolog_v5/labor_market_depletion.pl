% ============================================================================
% CONSTRAINT STORY: labor_market_depletion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_market_depletion, []).

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
 *   constraint_id: labor_market_depletion
 *   human_readable: Labor Market Depletion: Selective Extraction and Coordination Failure
 *   domain: economic_labor_policy
 *
 * SUMMARY:
 *   Labor market depletion is a structural dynamic in which employers and
 *   capital holders extract value from workers through systematic wage
 *   suppression, precarity management, and the strategic construction of
 *   artificial skill/labor shortages. The constraint exhibits tangled rope
 *   properties: it contains genuine coordination functions (wage standards,
 *   workplace safety, skill development) alongside asymmetric extraction
 *   concentrated on the most vulnerable workers (immigrants, precarious
 *   employees, the chronically underemployed). The extractiveness has
 *   increased from 0.35 to 0.58 over the interval as union decline,
 *   outsourcing, and immigration restrictionism have intensified
 *   supplier-side precarity. Theater has risen from 0.32 to 0.48 as 'skills
 *   gap' and 'labor shortage' narratives have become more central to employer
 *   messaging despite contradicting employment data. The constraint affects
 *   multiple institutional contexts: regulatory labor standards, immigration
 *   policy, education credentialing systems, and capital mobility frameworks.
 *   The key asymmetry is that capital can arbitrage globally while workers
 *   face nationalist restrictions, producing a labor supply asymmetry that is
 *   then narrated as a natural market outcome rather than a policy-enforced
 *   hierarchy.
 *
 * KEY AGENTS:
 *   - Capital Holders / Employers: Primary beneficiaries (institutional/arbitrage) — benefit from wage suppression, worker desperation, mobility advantages; can exit via offshoring/automation
 *   - Precarious Workers: Primary victims (powerless/trapped) — face material dependence, immigration restrictions, debt obligations; no exit options
 *   - Organized Labor Movement: Secondary actor (moderate/constrained) — coordinate worker safety and standards but constrained by anti-organizing law, capital mobility
 *   - Peripheral Labor Force (Immigrants/Gig Workers): Secondary victims (moderate/mobile but constrained by visa/xenophobia) — structurally mobile globally but constrained by legal/discriminatory barriers
 *   - Regulatory Reform Coalition: Organized agent (organized/constrained) — see sunset via policy reform (sectoral bargaining, union law, wage floors)
 *   - Labor Market Rituals: Institutional performance (institutional/arbitrage) — skills narratives, education mythology, shortage claims maintained through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing capitalist labor extraction as economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_market_depletion, 0.58).
domain_priors:suppression_score(labor_market_depletion, 0.65).
domain_priors:theater_ratio(labor_market_depletion, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_market_depletion, extractiveness, 0.58).
narrative_ontology:constraint_metric(labor_market_depletion, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(labor_market_depletion, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_market_depletion, tangled_rope).
narrative_ontology:human_readable(labor_market_depletion, "Labor Market Depletion: Selective Extraction and Coordination Failure").
narrative_ontology:topic_domain(labor_market_depletion, "economic_labor_policy").

domain_priors:requires_active_enforcement(labor_market_depletion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_market_depletion, capital_holders).
narrative_ontology:constraint_beneficiary(labor_market_depletion, wage_suppression_beneficiaries).
narrative_ontology:constraint_victim(labor_market_depletion, peripheral_workers).
narrative_ontology:constraint_victim(labor_market_depletion, labor_force_precarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (SNARE) — Trapped by material dependence on wage income, immigration status constraints, debt obligations, and geographic immobility. Faces suppression via underemployment, wage theft, schedule volatility, and lack of alternative income sources. No exit options; maximum extraction experienced.
constraint_indexing:constraint_classification(labor_market_depletion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED LABOR MOVEMENT (TANGLED ROPE) — Constrained by declining unionization, anti-organizing legal frameworks, and capital mobility. Yet labor movements genuinely coordinate worker safety, wage standards, and skill development across sectors. Mixed extraction and coordination: unions extract membership dues and enforce discipline; they also provide collective bargaining, grievance procedures, and knowledge commons.
constraint_indexing:constraint_classification(labor_market_depletion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMPLOYER COALITION (ROPE) — Benefits from labor market depletion through wage suppression and increased worker desperation. Exit via capital mobility (offshoring, automation, outsourcing). Experiences the constraint as pure coordination: maintaining depletion requires consistent messaging about 'skill gaps,' 'labor shortage,' and the necessity of immigration restriction. The coordination function is real — employers must align on wage ceilings and labor supply narratives.
constraint_indexing:constraint_classification(labor_market_depletion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PERIPHERAL LABOR FORCE (TANGLED ROPE) — Includes immigrants, gig workers, and the chronically underemployed. Mobile globally but constrained by visa restrictions, credential non-recognition, and xenophobic wage hierarchies. Benefits from access to higher-wage labor markets but bears costs of discrimination, precarity, and exploitation. Coordination exists (informal networks, diaspora knowledge-sharing) but is asymmetrically captured by employers.
constraint_indexing:constraint_classification(labor_market_depletion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY REFORM COALITION (SCAFFOLD) — Organized agents (labor standards agencies, progressive policymakers, worker advocacy organizations) see labor market depletion as a temporary coordination failure solvable through policy: sectoral bargaining, wage floors, union organizing rights, portable benefits, and progressive taxation. The sunset is explicit: as organizing infrastructure rebuilds and labor law reforms mature, the extraction mechanism loses force. Reform momentum is real but faces capital resistance.
constraint_indexing:constraint_classification(labor_market_depletion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LABOR MARKET RITUAL (PITON) — From civilizational perspective, labor market 'balancing' appears as ritualistic performance: skill-gap narratives, education-as-solution mythology, and labor shortage claims are performatively maintained despite contradicting employment data. The ritual persists through institutional inertia—schools, employers, and policymakers have career investments in the narrative. Theater dominates function; the actual matching mechanism has atrophied.
constraint_indexing:constraint_classification(labor_market_depletion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the analytical civilizational scope, labor depletion appears as an immutable feature of capitalist accumulation: surplus labor reserve armies are inherent to capital's need for wage suppression. This perspective risks naturalizing what are contingent institutional arrangements (union busting, immigration restrictions, credential gatekeeping) as laws of economics. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(labor_market_depletion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_market_depletion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_market_depletion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_market_depletion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_market_depletion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_market_depletion, TR),
    TR >= 0.70.

:- end_tests(labor_market_depletion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. Capital holders systematically suppress wages below productivity through precarity management, outsourcing threats, and labor supply narrative control. The extraction is not total (workers do extract some value through negotiation and informal economy) but is substantial and accelerating. Suppression (0.65): High. Workers face material barriers (wage dependence, debt, geographic constraints), legal barriers (immigration restrictions, anti-organizing law), and psychological barriers (internalized deservingness narratives). The suppression mechanisms are layered: structural (no exit), legal (unions prohibited in many sectors), and narrative (skills deficiency framing). Theater (0.48): Moderate-high. Skills gap and labor shortage narratives are partially performative—they serve to justify wage suppression and restriction despite employment data contradicting them. However, some coordination function is real: labor standards, safety regulation, and skill development genuinely occur through labor market institutions. The theater has increased as narrative management has intensified.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (capital/employers) experiences the constraint as coordination: aligning on wage ceilings, skill narratives, and anti-union strategy solves their coordination problem. The victim (precarious worker) experiences it as pure extraction: wages decline, precarity increases, exit options vanish. The organized labor movement experiences Tangled Rope: genuine coordination of safety/standards alongside asymmetric power loss. The peripheral labor force experiences Tangled Rope differently: international mobility option exists but is gatekept by visa/discrimination, creating mixed extraction. The scaffold reformer sees a fixable institutional failure with sunset logic; the piton ritual maintainer sees performative theater; the analytical observer risks seeing immutable capitalist nature. The perpectival gap reveals that 'labor shortage' is not an objective fact but an indexical claim: it is real from the capital perspective (shortage of desperate workers) and false from the worker perspective (abundance of desperate workers seeking any wage). The constraint's extractiveness appears different when measured by wage stagnation (beneficiary view: extraction is 'natural market'), labor force participation (worker view: extraction is 'forced precarity'), or surplus value appropriation (analytical view: extraction is 'systematic'), because each measurement observable captures a different structural layer.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: Capital holders benefit from depletion (low d ≈ 0.10-0.20) → low/negative f(d) → effective extraction chi is reduced. They see wage suppression as market coordination, not extraction. Victim directionality: Precarious workers are targets of depletion (high d ≈ 0.85-0.95) → high f(d) ≈ 1.20-1.40 → effective extraction chi is amplified. They experience the constraint as maximum coercion. Organized labor: Mixed beneficiary-victim status (d ≈ 0.45-0.55) → moderate f(d) ≈ 0.60-0.75 → Tangled Rope chi. Peripheral workers: Victims with partial exit (d ≈ 0.75-0.85) → f(d) ≈ 1.10-1.25 → high chi modulated by some arbitrage option. Reformer coalition: Constrained agents with policy exit (d ≈ 0.50-0.60) → moderate f(d) ≈ 0.70-0.85 → Scaffold chi, declining over time. The directionality spread across perspectives is large: from -0.12 (capital arbitrage) to 1.40 (precarious trap), revealing structural opposition.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: Labor market depletion resolves the mandatrophy by showing that 'coordination' and 'extraction' are not binary categories but scalar perspectives. The employer coalition genuinely coordinates (Rope view: alignment on wage ceilings, skill narratives, union avoidance), but this coordination occurs at the expense of workers (Snare view: wage suppression, precarity, no exit). The same structural mechanism (employer coalition formation) is simultaneously coordination (for capital) and extraction (for labor). The mandatrophy asks: is this Rope (pure coordination) or Snare (pure extraction)? The answer is Tangled Rope: genuine coordination of employer interests alongside asymmetric extraction of worker value. The false summit is the analytical civilizational view that naturalizes this as 'the market' — it is actually a policy-enforced hierarchy in which capital mobility is subsidized (trade agreements, offshoring incentives) while labor mobility is restricted (immigration law, visa systems). The constraint would not exist in its current form under different policy frameworks (sectoral bargaining, portable benefits, union power). Removing contingent enforcement (anti-organizing law, visa restrictions, credential gatekeeping) would shift classification toward Rope (pure coordination of labor standards). This shows that labor market depletion is not a Mountain but a Tangled Rope with a specific policy realization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_gap_authenticity,
    'Do documented ''skills gaps'' reflect actual employer requirements or performative certification demands disconnected from job function?',
    'Task analysis studies comparing posted job requirements to actual task performance; correlation of credential inflation with wage suppression; international comparison of skills-wage relationships across differently regulated labor markets',
    'If gaps are authentic: labor market depletion is partially a coordination failure (higher Rope/Scaffold classification). If performative: depletion is primarily extraction masquerading as shortage (higher Snare/Tangled Rope classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_gap_authenticity, empirical, 'Whether documented skills gaps reflect actual requirements or performative credentialing').

omega_variable(
    labor_shortage_construction,
    'Are reported ''labor shortages'' descriptive observations or strategic narratives constructed to justify wage suppression and immigration restriction?',
    'Analysis of job posting turnover rates, actual vacancy duration, wage offer responses; temporal correlation between shortage claims and wage stagnation; comparison of shortage narratives across sectors with different union presence',
    'If genuine shortages: constraint is primarily coordination (Rope/Scaffold from beneficiary perspectives). If constructed: constraint is primarily extraction (Snare/Tangled Rope dominates, false summit in mountain view).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_shortage_construction, empirical, 'Whether labor shortages are authentic or constructed narratives').

omega_variable(
    peripheral_worker_exit_cost,
    'What proportion of peripheral worker suppression is structural (visa barriers, credential non-recognition, discriminatory hiring) versus identity-locked (internalized xenophobia, belief in undeservingness)?',
    'Comparative analysis of exit behavior when barriers are removed; longitudinal tracking of worker identity frames before and after access to alternative labor markets; ethnographic documentation of self-concept shifts during labor movement organizing',
    'If primarily structural: classification remains Snare/Tangled Rope with trapped/constrained exit. If significantly identity-locked: powerless peripheral workers may have higher agency than suppression metrics suggest; classification shifts toward identity_locked exit option.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peripheral_worker_exit_cost, empirical, 'Whether suppression of peripheral workers is structural or identity-locked').

omega_variable(
    coordination_function_authenticity,
    'Does the employer coalition genuinely coordinate labor standards and safety, or does ''coordination'' reduce to wage suppression and union avoidance?',
    'Analysis of employer coalition outputs: actual wage floor setting vs. regulatory minimum; safety standard innovation vs. regulatory compliance theater; skill development investment vs. credential gatekeeping; comparative advantage from coordination vs. cost savings from suppression',
    'If genuine coordination exists: Rope classification for beneficiaries is accurate. If coordination is a cover story: Rope should downgrade to Snare, and the beneficiary perspective should classify as pure extraction rather than mixed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_authenticity, empirical, 'Whether employer coalition has genuine coordination function or is pure extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_market_depletion, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lmd_tr_t0, labor_market_depletion, theater_ratio, 0, 0.32).
narrative_ontology:measurement(lmd_tr_t10, labor_market_depletion, theater_ratio, 10, 0.4).
narrative_ontology:measurement(lmd_tr_t20, labor_market_depletion, theater_ratio, 20, 0.48).
narrative_ontology:measurement(lmd_tr_t5, labor_market_depletion, theater_ratio, 5, 0.36).
narrative_ontology:measurement(lmd_tr_t15, labor_market_depletion, theater_ratio, 15, 0.44).

% Extraction over time
narrative_ontology:measurement(lmd_be_t0, labor_market_depletion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lmd_be_t10, labor_market_depletion, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(lmd_be_t20, labor_market_depletion, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(lmd_be_t5, labor_market_depletion, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(lmd_be_t15, labor_market_depletion, base_extractiveness, 15, 0.53).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_market_depletion, resource_allocation).
narrative_ontology:affects_constraint(labor_market_depletion, wage_stagnation).
narrative_ontology:affects_constraint(labor_market_depletion, credential_gatekeeping).
narrative_ontology:affects_constraint(labor_market_depletion, immigration_restriction).
narrative_ontology:affects_constraint(labor_market_depletion, union_suppression).

% DUAL FORMULATION NOTE:
% Labor market depletion is a cluster constraint with four major structural components. Each component has its own extractiveness value and can be decomposed into separate stories: wage_stagnation (how employer coordination suppresses wage growth), credential_gatekeeping (how education systems enforce skill hierarchies), immigration_restriction (how nationalist policy gatekeeps labor supply), union_suppression (how anti-organizing law disables worker collective power). The present story models the aggregate extractiveness (0.58) across all four mechanisms. Each downstream story has lower extractiveness (0.30-0.45) representing its specific mechanism. The network linkages show how the four mechanisms reinforce each other: credential gatekeeping justifies wage suppression, immigration restriction justifies skill narratives, union suppression enables employer coordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(labor_market_depletion, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
