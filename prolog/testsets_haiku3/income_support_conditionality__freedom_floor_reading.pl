% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__freedom_floor_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_conditionality__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Labor Decommodification (Freedom Floor Reading)
 *   domain: political_economy/labor_economics/social_policy
 *
 * SUMMARY:
 *   Under this reading, unconditional income support operates as a
 *   coordination mechanism that decommodifies labor—it removes the coercive
 *   pressure that comes from treating workers' capacity to labor as a pure
 *   commodity to be priced in the market. Workers receive a guaranteed floor
 *   independent of employment, which converts desperation-driven labor supply
 *   into genuine choice. The constraint's function is to establish exit
 *   options that make refusal of coercive work materially possible. Employers
 *   lose the ability to extract surplus from worker desperation and must
 *   compete on actual wages and conditions. This reading emphasizes positive
 *   freedom (the concrete ability to refuse) rather than abstract rights. The
 *   claim is ROPE (coordination on a shared floor) and the metrics are
 *   authored to reflect low extractiveness (the flow is from general taxation
 *   to workers, not from workers to a capturing agent), low theater (the
 *   mechanism is direct income transfer, not performative compliance), and
 *   low suppression (the constraint is enabling, not coercive—it removes
 *   coercion from labor markets). The measurement series shows slight decline
 *   in extractiveness over time as the floor's effect on labor-market
 *   desperation compounds, and stable low theater and suppression, because
 *   the constraint does not need active enforcement once the income
 *   infrastructure is built.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__freedom_floor_reading, 0.18).
domain_priors:suppression_score(income_support_conditionality__freedom_floor_reading, 0.12).
domain_priors:theater_ratio(income_support_conditionality__freedom_floor_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Support as Labor Decommodification (Freedom Floor Reading)").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "political_economy/labor_economics/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, '52aa36dd-86af-4cc9-9ffd-7fee7f33a7e8').
narrative_ontology:cs_kernel_codification('52aa36dd-86af-4cc9-9ffd-7fee7f33a7e8', distributed).
narrative_ontology:cs_authority_grounding('52aa36dd-86af-4cc9-9ffd-7fee7f33a7e8', extraction).
narrative_ontology:cs_interpretation_layer_present('52aa36dd-86af-4cc9-9ffd-7fee7f33a7e8').
narrative_ontology:cs_reading_relation('52aa36dd-86af-4cc9-9ffd-7fee7f33a7e8', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('52aa36dd-86af-4cc9-9ffd-7fee7f33a7e8', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('52aa36dd-86af-4cc9-9ffd-7fee7f33a7e8', foundational, decommodification_of_labor_as_justice).
narrative_ontology:cs_axiom_status(decommodification_of_labor_as_justice, holdable).
narrative_ontology:cs_axiom_grounding('52aa36dd-86af-4cc9-9ffd-7fee7f33a7e8', decommodification_of_labor_as_justice, deontological).
narrative_ontology:cs_axiom('52aa36dd-86af-4cc9-9ffd-7fee7f33a7e8', foundational, positive_freedom_through_material_exit).
narrative_ontology:cs_axiom_status(positive_freedom_through_material_exit, holdable).
narrative_ontology:cs_axiom_grounding('52aa36dd-86af-4cc9-9ffd-7fee7f33a7e8', positive_freedom_through_material_exit, deontological).
narrative_ontology:cs_reference_frame('52aa36dd-86af-4cc9-9ffd-7fee7f33a7e8', commodified_labor_market_baseline).
narrative_ontology:cs_drift_state('52aa36dd-86af-4cc9-9ffd-7fee7f33a7e8', post_implementation_decade, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('52aa36dd-86af-4cc9-9ffd-7fee7f33a7e8', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, workers_in_precarious_sectors).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, caregivers_and_artists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, organized_labor).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, employers_of_low_wage_labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income floor independent of employment. Can refuse coercive work arrangements, renegotiate wages upward, exit abusive workplaces without immediate destitution. Exit from precarious or exploitative work becomes structurally possible for the first time, not merely theoretically available. The income floor shifts their bargaining position from desperation to negotiated entry.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_workers, beneficiary,
    powerless, biographical, mobile, national).

% Gig workers, seasonal workers, contract workers gain stability and reduced urgency in accepting any available work. Can wait for better terms, reject unsafe conditions, invest in skill development or transition without income collapse. The income floor decouples survival from accepting the first available job.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, workers_in_precarious_sectors, beneficiary,
    moderate, biographical, mobile, national).

% Can pursue care work, artistic practice, community organizing, or other socially valuable but low-wage or unpaid work without market pressure to commodify it or abandon it for survival income. The constraint recognizes their contribution outside commodity markets.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, caregivers_and_artists, beneficiary,
    moderate, generational, mobile, national).

% Lose the coercive firing power that came from workers' desperation. Cannot exploit wage suppression sustained by worker destitution. Must offer competitive wages, better conditions, and genuine benefits to attract workers who now have a credible outside option. Their cost structure adjusts upward where it was previously subsidized by worker poverty.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, employers_of_low_wage_labor, payer,
    institutional, biographical, constrained, national).

% Sees the income floor as a complement to union power: workers backed by unconditional income are harder to break during strikes, can refuse substandard contracts, and have stronger collective bargaining leverage. Observes from a position of partial beneficiary (strengthens worker power) and partial competitor (universal floor may reduce unionization's premium).
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, organized_labor, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__freedom_floor_reading, organized_labor, observer).

% Largely insulated from the constraint (labor markets for specialized skills clear above the floor). Observes the wage pressure on lower-wage sectors and may face pressure to maintain internal wage spreads, but does not face coercive-labor dynamics. Can arbitrage labor costs across jurisdictions without the floor.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, high_wage_employers, observer,
    institutional, biographical, arbitrage, global).

% Sets the policy, enforces the payment infrastructure, and bears the fiscal cost of the income floor. Manages the constraint's persistence through taxation and budgeting. Can modify conditionality, benefit level, or coverage, but under this reading does not use the income support as a coercive mechanism — it is a guaranteed floor, not a means test or behavior gate.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, state_fiscal_authority, agenda_setter,
    institutional, generational, analytical, national).

% Countries and institutions committed to maintaining subsistence through market wages and coercive labor discipline are excluded from this reading's logic and would oppose it. They do not participate in setting the constraint but are affected by its regional or global spillover if it alters labor market expectations.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, subsistence_threshold_societies, excluded,
    powerful, civilizational, trapped, global).

% Analyze the empirical effects: whether the floor actually decommodifies labor or triggers dependency (the contested reading boundary). Provide evidence about work incentives, wage dynamics, and exit patterns that other readings interpret differently.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, policy_advocates_and_economists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:fixing_cost_class(income_support_conditionality__freedom_floor_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a collective commitment to provide unconditional subsistence outside the labor market, removing the desperation that forces workers to accept coercive employment. Coordinates a shared recognition that survival is a social responsibility, not a commodity purchased through labor-market subordination. Enables workers to participate in labor markets from choice rather than necessity.
% TRANSFER_FUNCTION: Moves resources from general taxation (or wealth tax, land value capture, natural resource rents) to all adult residents (or working-age adults, or all citizens) as an unconditional monthly or periodic income floor. The transfer is not earned through labor, not means-tested on employment, and not conditioned on behavior. It functions to decommodify—to remove subsistence from the labor market entirely.
% ABSENT_VOICES: Market-discipline advocates who argue workers require the pressure of destitution to maintain work effort and investment in skills. Employers who benefit from desperation-driven wage suppression and would lose coercive firing power. Countries and institutions organized around subsistence-through-wages who view the floor as culturally impossible or fiscally insane. Workers whose bargaining power comes from scarcity (credential holders) who may see the floor as reducing their premium and undermining individual achievement narratives.
% DISAPPEARANCE_RATIONALE: If unconditional income support disappeared, labor-market desperation would return, workers would be forced to accept any available work, employers would regain coercive firing power, wage suppression would intensify, care work and artistic practice would decline further, and the distribution of power in labor markets would shift sharply toward capital. The removal would be experienced by precarious workers as a catastrophic loss of freedom—the ability to refuse would vanish overnight.
% FOUNDING_PROBLEM: Under commodity logic, labor power is treated as a pure market input priced according to supply and demand, with no floor except the biological minimum for survival. Workers must sell their labor to survive and cannot refuse any available employment without facing destitution. This creates systematic coercion: employers can demand low wages, dangerous conditions, humiliating treatment, unpredictable scheduling, and uncompensated overtime because the alternative is homelessness and hunger. The founding problem is the commodification of labor power itself—the institutional project that treats human capacity to work as an ordinary commodity rather than as a right-bearing aspect of human dignity.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists document persistent wage suppression in low-wage sectors correlated with worker precarity (Autor, Card, Acemoglu & Robinson). Sociologists document coercive employment practices: wage theft, schedule manipulation, termination threats tied to work discipline (Ehrenreich, Purcell, Kessler-Harris). Historians document that labor commodification was not a natural market outcome but a deliberate institutional project: enclosure stripped peasants of land, slavery and indenture forcibly commodified human beings, colonialism imposed commodity-logic on subsistence economies (Thompson, Federici, Graeber, Scott). Worker testimony and labor union organizing histories attest to coercive conditions sustained by destitution risk—that workers remain in abusive situations because they have nowhere else to go. These sources are independent observers (not the benefiting parties of this reading), documenting the founding condition from which the income floor is offered as remedy.
narrative_ontology:disappearance_verdict(income_support_conditionality__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_conditionality__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__freedom_floor_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__freedom_floor_reading_tests).
:- end_tests(income_support_conditionality__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as LOW (0.18 at interval end) because the constraint's logic is that resources flow FROM the state TO workers as a baseline, not that workers are harvested for surplus. The beneficiary set (low-wage workers, precarious workers, caregivers) collects the income floor without running it; they benefit without extraction. Suppression is authored as LOW (0.12) because the constraint removes coercive pressure from labor markets—it does not coerce compliance; it enables refusal. Theater is authored as LOW (0.08) because the payment is direct and functional; there is minimal performative overhead. Accessibility_collapse (0.25) is moderate because the alternative to the income floor (desperation-driven labor) remains available to those who reject it, but its material force is dramatically reduced once the floor exists. Resistance (0.72) is high because employers and market-discipline advocates actively resist the constraint—they argue it creates dependency, undermines work ethics, and is fiscally unsustainable. The measurement series declines slightly over the first 15 years (as the income floor's stabilizing effects compound and labor market desperation weakens) then stabilizes, reflecting a mature constraint whose function is steady-state coordination, not dynamic extraction. The shared time grid ensures every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The employer and low-wage worker seats should compute radically differently. From the low-wage worker seat, the income floor is emancipatory coordination—it transforms them from victims of labor-market desperation into agents with genuine choice. From the employer seat, the same constraint is extractive—it forces them to pay higher wages and accept worker refusal. The engine computes this divergence from power × exit_options × beneficiary/victim declarations. The policy debate (dependency vs. freedom) IS this perspectival gap, computed from structural asymmetry. The present reading takes the freedom side; sibling readings take the dependency or wage-subsidy sides. None of these framings is authored into the metrics—they are structural consequences of the declared relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   From the low-wage worker seat: directionality is near 0.0 (full beneficiary—they receive the floor without bearing extractive costs, only the diffuse cost of general taxation, which is universal not worker-specific). From the employer seat: directionality is near 1.0 (full target—they lose coercive firing power, must pay competitive wages, their cost structure rises). From the state/fiscal authority seat: directionality is near 0.5 (symmetric—they must fund the floor through taxation or reallocation, but they also benefit from reduced social disorder and strengthened labor dignity). From high-wage employer seats: directionality is near 0.0 (insulated beneficiary—the floor does not reach them; they observe wage pressure in low-wage markets but remain outside the constraint's direct scope). The engine computes these divergences from the structural data (beneficiary/victim declarations + power + exit options); the commentary explains WHY they diverge: different seats have different material relationships to the income floor.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy would occur if the income floor persisted but lost its function—if the founding problem (coercive labor through desperation) had been solved by some other mechanism (e.g., full employment, strong unions, high prevailing wages) but the income transfer continued from institutional inertia. Under this reading, that is unlikely to occur because the function IS the income support itself—once removed, coercive labor pressure returns. Mandatrophy would require the founding problem to be actually dead while the solution persists, which would only happen if labor had been permanently decommodified through some other route (e.g., universal worker ownership, reduction of work hours across the board, or shift away from wage-labor entirely). The reading does not predict mandatrophy; it predicts stability so long as the founding problem (labor commodification pressure) remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_boundary_dependency_vs_freedom,
    'Does unconditional income support create positive freedom to refuse coercive work (this reading), or does it reduce work incentives and create long-term dependency (dependency_trap_reading)?',
    'Longitudinal study of work participation, wage dynamics, and job-switching patterns before and after implementation. Track whether workers use the floor to exit bad jobs and find better work (freedom hypothesis) or whether labor participation declines and workers stop searching (dependency hypothesis). Control for regional employment conditions and skill-level changes.',
    'If workers use the floor to exit low-wage work and find higher-wage or less coercive employment, the freedom reading is supported and the constraint computes as rope (coordination). If labor participation drops substantially and workers report reduced job-search intensity, the dependency reading gains support and the constraint would recompute as snare (extraction from work discipline). If both occur in different subgroups (freedom for some, dependency for others), the reading requires granulation by skill level, sector, and family status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_dependency_vs_freedom, empirical, 'Whether the income floor enables exit from coercive work or reduces work participation altogether.').

omega_variable(
    reading_boundary_wage_subsidy,
    'Does unconditional income support allow employers to suppress wages while maintaining worker subsistence (wage_subsidy_reading), or does it enable workers to demand higher wages by making refusal credible (this reading)?',
    'Wage analysis post-implementation: compare wage growth in low-wage sectors before and after the floor. If wages stagnate despite the floor (employers capture the subsidy), the wage_subsidy_reading gains support. If wages rise as workers become harder to employ at stagnant rates, the freedom reading is supported. The test requires controlling for skill-biased technological change, immigration effects, and sector-specific demand shifts.',
    'If employers suppress wages despite the floor, the constraint''s extractiveness rises (employers extract surplus from both worker desperation AND the public income support) and it recomputes toward snare-hybrid territory. If workers use the floor to demand higher wages, the freedom reading holds and the rope classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_wage_subsidy, empirical, 'Whether the income floor is captured by employers as a wage-suppression subsidy or used by workers to negotiate higher wages.').

omega_variable(
    labor_market_coercion_locus,
    'Where is the coercive pressure in labor markets located: in employers'' ability to fire workers into destitution (this reading''s diagnosis), in workers'' internalized shame and discipline (Foucauldian reading), in market institutions that create scarcity (institutional economics reading), or in the historical commodification project itself (Marxist/postcolonial reading)?',
    'Qualitative research: in-depth interviews with workers in precarious sectors pre- and post-floor, asking about decision-making in job refusal, wage negotiation, and exit. Track which mechanisms workers identify as coercive. Observational work in low-wage industries to identify enforcement points (firing threats, schedule manipulation, credential barriers). Historical analysis of how labor commodification was institutionalized.',
    'If coercion is primarily in firing-threat credibility, the income floor directly addresses it and the rope reading holds. If coercion is primarily internalized (workers believe they deserve poverty for refusing work), the floor removes material coercion but may leave internalized suppression intact—the constraint partially succeeds and partially fails, requiring a more nuanced reading. If coercion is in market scarcity, the floor addresses it only at the margin (it gives workers time to search but does not create jobs). If coercion is in the commodification structure itself, the floor is a partial remedy that requires deeper institutional change (decommodification of other social goods: housing, education, care) to fully work.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_market_coercion_locus, conceptual, 'The structural location of coercive pressure in labor markets determines how fully the income floor addresses the founding problem.').

omega_variable(
    suppression_locus_structural_vs_internalized,
    'Is suppression in labor markets primarily structural (external barriers: firing threat, destitution risk, credit denial) or primarily internalized (workers have internalized the logic of their own commodification, believe they deserve low wages, carry shame about refusal)?',
    'Post-exit trajectory: after workers exit bad jobs with the income floor, do they continue to report suppressive beliefs (I deserve this, I cannot refuse, work is a duty) or do suppressive beliefs decline with material security? If suppression persists after exit, it was internalized; if it declines, it was structural. Comparative study: interview workers in countries without income floors, with conditional support, and with unconditional support to track suppressive belief patterns.',
    'If suppression is purely structural, the income floor removes it entirely and the constraint delivers the freedom reading. If suppression is internalized, the floor removes its material basis but workers must actively re-frame their self-understanding to use the freedom—the floor is necessary but not sufficient. The measured suppression (0.12) may underestimate the internalized component. A finding of high internalized suppression would mean the income floor requires complementary work: cultural change, education, therapeutic support to help workers reclaim agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_locus_structural_vs_internalized, empirical, 'Whether labor-market suppression is structural (removable by material support) or internalized (requiring psychological/cultural work to address).').

omega_variable(
    kernel_contest_sibling_foreclosure,
    'Do the three readings of the income_support_conditionality kernel logically foreclose each other, or do they coexist as different interpretations of the same policy held by different parties?',
    'Foundational-premise analysis: extract the core normative claim from each reading. Freedom_floor_reading: workers have positive freedom right to refuse coercive work; commodity logic is unjust. Dependency_trap_reading: work discipline is necessary; unconditional support undermines it. Wage_subsidy_reading: employers capture income support as wage-subsidy. Test whether any two premises directly contradict (foreclosure) or whether they are distinct empirical claims about the same policy that could all be partially true (coexistence).',
    'If the readings foreclose each other, one reading is correct and the others are false. If they coexist, the policy produces all three effects simultaneously in different sectors or for different subgroups—some workers gain freedom, some develop dependency, employers capture some surplus—and the overall classification depends on which effect dominates. Coexistence would mean the constraint is more complex than any single reading captures and requires structural decomposition (three separate constraint stories, one per effect, linked by network relationships).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_sibling_foreclosure, conceptual, 'Whether the income_support_conditionality kernel''s three readings logically foreclose or coexist.').

omega_variable(
    fiscal_sustainability_founding_problem,
    'Is the founding problem (coercive labor through commodification pressure) primarily a moral/justice problem or a sustainability problem? If the income floor is fiscally unsustainable, does that change whether it solves the founding problem?',
    'Fiscal analysis: compute the cost of the income floor as a percentage of GDP or total tax revenue under various targeting rules and benefit levels. Compare to historical social spending and military spending. Test whether sustainability concerns are empirically grounded or ideological (used to justify returning to coercive labor). Historical analysis: trace which societies sustained what levels of support and when sustainability arguments were deployed to justify retrenchment.',
    'If the floor is fiscally sustainable (evidence from historical systems, from rich countries that allocate resources to it, from financing mechanisms like wealth tax or land value tax), the founding problem is solvable and the rope reading holds. If the floor is truly unsustainable at scale, the founding problem (coercive labor pressure) persists even if morally recognized—the freedom floor reading captures the moral solution, but pragmatic constraint is that the solution cannot be implemented. This would be an omega that affects policy viability, not the constraint''s structural classification (which stays rope), but flags the gap between moral mandate and material feasibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_founding_problem, empirical, 'Whether unconditional income support is fiscally sustainable, which determines whether the freedom reading can be institutionalized at scale.').

omega_variable(
    scope_degradation_universal_vs_targeted,
    'Does the scope of the income floor affect whether it functions as labor decommodification (universal application removes coercive pressure everywhere) or as selective subsidy (targeted floor leaves non-recipients in coercive labor, creating a two-tier system)?',
    'Comparative analysis: universal income floor vs. targeted income support. Study labor-market coercion in the excluded group (those not receiving the floor). If coercive pressure remains high for non-recipients while recipients gain freedom, the constraint has created a bifurcated market, not decommodification—it is coordination for insiders, coercion for outsiders. Test whether universality is structural (required for the freedom reading to hold) or contingent (the reading can apply to any level of scope).',
    'If universality is required, the freedom floor reading depends on scope being at least national and ideally global. A means-tested or conditional income support would not instantiate this reading (it would instantiate a different reading focused on discipline or subsidy). If universality is not required, the reading can apply to any policy that provides genuine exit options, even for subgroups. The measurement grid should reflect scope differences—a localized floor might compute as rope in that local context but snare in the broader market.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_degradation_universal_vs_targeted, conceptual, 'Whether universality of scope is structural to the freedom floor reading or contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__freedom_floor_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__freedom_floor_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__freedom_floor_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__freedom_floor_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(inco_tr_t30, income_support_conditionality__freedom_floor_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(inco_tr_t40, income_support_conditionality__freedom_floor_reading, theater_ratio, 40, 0.08).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__freedom_floor_reading, base_extractiveness, 5, 0.2).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__freedom_floor_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__freedom_floor_reading, base_extractiveness, 15, 0.17).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__freedom_floor_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(inco_be_t30, income_support_conditionality__freedom_floor_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement(inco_be_t40, income_support_conditionality__freedom_floor_reading, base_extractiveness, 40, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__freedom_floor_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__freedom_floor_reading, suppression_requirement, 5, 0.13).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__freedom_floor_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__freedom_floor_reading, suppression_requirement, 15, 0.12).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__freedom_floor_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement(inco_su_t30, income_support_conditionality__freedom_floor_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement(inco_su_t40, income_support_conditionality__freedom_floor_reading, suppression_requirement, 40, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__freedom_floor_reading, 0.12).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_conditionality__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_conditionality__wage_subsidy_reading).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, labor_commodification_kernel).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, employer_wage_coercion_mechanism).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, work_discipline_institutional_complex).

% DUAL FORMULATION NOTE:
% This story is one reading of the income_support_conditionality kernel. The freedom_floor_reading instantiates the claim that unconditional income support decommodifies labor and creates positive freedom to refuse coercive work. Sibling readings (dependency_trap_reading, wage_subsidy_reading) interpret the same policy but locate different extractive or disciplinary effects. All three readings share the same referent (the policy of unconditional income support) but differ in their empirical claims about its effects and normative assessment of those effects. Each reading is a structurally independent constraint story with its own epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family because they contest the same policy domain and upstream claims feed into the interpretation of this one. Changes to the foundational claim about labor commodification (whether it is a justice problem requiring remedy or a natural feature of markets) would affect which reading applies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
