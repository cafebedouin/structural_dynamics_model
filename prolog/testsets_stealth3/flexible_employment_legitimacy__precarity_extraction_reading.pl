% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__precarity_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__precarity_extraction_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: flexible_employment_legitimacy__precarity_extraction_reading
 *   human_readable: Platform Flexible Employment as Structural Precarity (Precarity-Extraction Reading)
 *   domain: economic/labor/social_policy
 *
 * SUMMARY:
 *   Flexible employment — platform-mediated, contractor-classified,
 *   algorithmically managed work — is read here as structural precarity that
 *   enables platforms to capture surplus value: nominal wage gains are offset
 *   by externalized risk, algorithmic management disciplines effort without
 *   carrying employer obligations, and social-security gaps shift protection
 *   costs onto workers and public systems. This file instantiates ONE reading
 *   of the contested kernel flexible_employment_legitimacy; the
 *   market-efficiency and developmental-state readings are separate
 *   constraints with their own epsilon, beneficiary/victim structures, and
 *   types, linked through network.affects_constraints. Per the
 *   epsilon-referent rule, extractiveness is authored for the standing
 *   arrangement under contest — actual platform labor relations as they
 *   operate — assessed by this reading's own lights, never for the formalized
 *   arrangement this reading's tradition would prefer. KEY AGENTS (by
 *   structural relationship): - platform_operators: agenda-setting
 *   beneficiary (institutional/arbitrage) — designs the work relationship,
 *   sets rates, deploys algorithmic management, collects the spread -
 *   platform_investors: beneficiary (institutional/arbitrage) — collects
 *   returns on the margin the arrangement preserves - gig_workers: primary
 *   target (powerless/constrained) — bear income volatility, asset costs, and
 *   protection gaps - on_demand_service_consumers: beneficiary
 *   (organized/constrained) — receive services priced below full-cost
 *   recovery - traditional_sector_employers: secondary target
 *   (institutional/constrained) — bear payroll and benefit obligations their
 *   exempt competitors avoid - social_insurance_systems: secondary target
 *   (institutional/trapped) — absorb contribution leakage and downstream
 *   old-age poverty - deactivated_workers: excluded voice (powerless/trapped)
 *   — terminated by automated decision, outside every negotiating forum -
 *   labor_regulators: analytical observer (institutional/analytical) —
 *   adjudicate classification disputes and can alter enforcement
 *
 * KEY AGENTS:
 *   - platform_operators: agenda-setting beneficiary (institutional/arbitrage) — designs the work relationship, sets rates, deploys algorithmic management, collects the spread
 *   - platform_investors: beneficiary (institutional/arbitrage) — collects returns on the margin the arrangement preserves
 *   - gig_workers: primary target (powerless/constrained) — bear income volatility, asset costs, and protection gaps
 *   - on_demand_service_consumers: beneficiary (organized/constrained) — receive services priced below full-cost recovery
 *   - traditional_sector_employers: secondary target (institutional/constrained) — bear compliance obligations their exempt competitors avoid
 *   - social_insurance_systems: secondary target (institutional/trapped) — absorb contribution leakage and downstream old-age poverty
 *   - deactivated_workers: excluded voice (powerless/trapped) — terminated by automated decision, outside every negotiating forum
 *   - labor_regulators: analytical observer (institutional/analytical) — adjudicate classification disputes and can alter enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, 0.78).
domain_priors:suppression_score(flexible_employment_legitimacy__precarity_extraction_reading, 0.71).
domain_priors:theater_ratio(flexible_employment_legitimacy__precarity_extraction_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__precarity_extraction_reading, tangled_rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__precarity_extraction_reading, "Platform Flexible Employment as Structural Precarity (Precarity-Extraction Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__precarity_extraction_reading, "economic/labor/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__precarity_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__precarity_extraction_reading, '1c74f38d-f1d1-4bb6-ab0b-ac49528f8c65').
narrative_ontology:cs_kernel_codification('1c74f38d-f1d1-4bb6-ab0b-ac49528f8c65', distributed).
narrative_ontology:cs_authority_grounding('1c74f38d-f1d1-4bb6-ab0b-ac49528f8c65', distributed).
narrative_ontology:cs_reading_relation('1c74f38d-f1d1-4bb6-ab0b-ac49528f8c65', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c74f38d-f1d1-4bb6-ab0b-ac49528f8c65', flexible_employment_legitimacy__developmental_state_reading, influences).
narrative_ontology:cs_axiom('1c74f38d-f1d1-4bb6-ab0b-ac49528f8c65', foundational, risk_externalization_constitutes_surplus_extraction).
narrative_ontology:cs_axiom_status(risk_externalization_constitutes_surplus_extraction, holdable).
narrative_ontology:cs_axiom_grounding('1c74f38d-f1d1-4bb6-ab0b-ac49528f8c65', risk_externalization_constitutes_surplus_extraction, empirically_contingent).
narrative_ontology:cs_axiom('1c74f38d-f1d1-4bb6-ab0b-ac49528f8c65', foundational, algorithmic_management_functions_as_discipline).
narrative_ontology:cs_axiom_status(algorithmic_management_functions_as_discipline, holdable).
narrative_ontology:cs_axiom_grounding('1c74f38d-f1d1-4bb6-ab0b-ac49528f8c65', algorithmic_management_functions_as_discipline, empirically_contingent).
narrative_ontology:cs_reference_frame('1c74f38d-f1d1-4bb6-ab0b-ac49528f8c65', protected_employment_standard).
narrative_ontology:cs_drift_state('1c74f38d-f1d1-4bb6-ab0b-ac49528f8c65', platform_economy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1c74f38d-f1d1-4bb6-ab0b-ac49528f8c65', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_investors).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, on_demand_service_consumers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, gig_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, traditional_sector_employers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, social_insurance_systems).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, marxian_surplus_value_theory).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, labor_process_control_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and continuously revise the terms under which millions perform paid tasks through their apps: they set per-task prices and the share retained, classify contributors as independent businesses rather than employees, route work through acceptance-rate and rating systems, and can terminate a contributor's access automatically. They fund legal teams and ballot campaigns defending the contractor classification, and can shift operations, corporate structure, or jurisdiction if regulations bind in one territory.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold equity in the operators and collect dividends and valuation growth that depend on the margin between what customers pay and what contributors receive net of their own costs. Capital moves freely across sectors and borders; nothing ties an investment position to any particular labor arrangement.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% Accept tasks through an app using their own vehicle, home, or tools; they wait unpaid between assignments, absorb fuel, maintenance, insurance, and equipment costs, and receive no sick pay, pension contributions, or injury coverage through the platform. Income depends on algorithm-assigned volume and rates that can change without negotiation. Leaving means losing an income channel they may depend on; staying means accepting revised terms as issued.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, gig_workers, payer,
    powerless, biographical, constrained, national).

% Order rides, meals, and tasks at prices and speeds that assume the person fulfilling them carries their own risks and protections. Individual households can switch providers easily, but the convenience itself depends on the arrangement continuing, and few consumers participate in the regulatory contests that determine its terms.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, on_demand_service_consumers, beneficiary,
    organized, biographical, constrained, global).

% Run taxis, restaurants, delivery fleets, and care agencies under employment contracts carrying payroll taxes, minimum-wage floors, and benefit duties, while competing against services whose contributors bear those costs privately. They cannot adopt the exempt classification without dismantling their own employment relationships, and their campaigns against the exemption have largely failed.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, traditional_sector_employers, payer,
    institutional, generational, constrained, national).

% Administer pension, health, and unemployment schemes financed substantially by employer and employee contributions attached to classified employment. As work migrates to contractor status their contribution base erodes while future claims from unprotected workers arrive regardless; they cannot decline the obligations or reclassify themselves out of the function.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, social_insurance_systems, payer,
    institutional, generational, trapped, national).

% Were cut off from the apps by automated fraud, rating, or compliance decisions, often without explanation or functioning appeal. They lose the income channel entirely, retain the vehicle debt and skills invested in it, and have no seat in the consultations where classification rules are argued.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, deactivated_workers, excluded,
    powerless, immediate, trapped, local).

% Investigate whether contributor classifications match the reality of directed, scheduled, evaluated work; they run hearings, commission studies, issue directives, and in some jurisdictions have reclassified drivers as employees. Their decisions can rewrite the terms of the whole arrangement, and they sit outside the revenue flows it generates.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__precarity_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches geographically dispersed, individually owned labor capacity (vehicles, scooters, homes, skills) to spiky, unpredictable urban demand in real time, without fixed schedules, worksites, or long-term commitments; gives workers low-barrier entry to paid work and customers immediate fulfillment.
% TRANSFER_FUNCTION: Moves money from customers to workers per transaction with a platform-set percentage retained; moves risk (income volatility, illness, accident, old-age provision) from platform balance sheets to workers' households; concentrates pricing and scheduling leverage in platform algorithms away from individual workers; over time shifts unfunded protection costs toward public systems.
% ABSENT_VOICES: Workers deactivated by automated systems have no standing in classification debates; prospective workers in cities where services have not launched cannot object to terms set before arrival; informal-sector workers in lower-income countries whose conditions anchor the global rate floor are outside every regulatory forum where the arrangement's terms are negotiated.
% DISAPPEARANCE_RATIONALE: If the arrangement were replaced overnight by classified employment with standard protections, service prices would rise, response times would lengthen, some marginal services would disappear, platform margins would compress sharply, millions of workers' income composition would shift to wage-plus-benefits, and public contribution bases would begin refilling.
% FOUNDING_PROBLEM: After the 2008-09 employment collapse, idle private assets and underemployed time needed matching to urban demand that traditional employment structures served poorly; platforms built low-friction matching to put cars, rooms, and spare hours to work.
% FOUNDING_PROBLEM_CORROBORATION: Labor-statistics agencies and ILO analyses corroborate that the matching problem was and remains real. Judicial findings on misclassification (UK Supreme Court 2021, California courts) and peer-reviewed time-use studies corroborate, from outside the benefiting parties, that the arrangement now exceeds the founding problem; no source outside the benefiting parties attests that the current risk allocation is required by the matching function itself.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__precarity_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__precarity_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__precarity_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the transfer operates on three stacked channels: the platform-set spread between customer payment and worker payout, uncompensated time (waiting, deadhead travel) and asset costs charged against gross receipts, and the absence of employer-borne protection contributions. Suppression (0.71) reflects that the arrangement's persistence depends on actively maintained machinery — classification litigation, arbitration clauses, deactivation regimes, and ballot-measure defense — not on participant preference alone. Theater ratio (0.32) is moderate: the 'flexibility' and 'be your own boss' narrative grows more performative as control instruments tighten, but the underlying matching function is real and heavily used. Accessibility collapse (0.45): alternatives remain partly available (traditional jobs, multi-apping, exit), but degrade for workers who have financed vehicles against expected volume or accumulated platform-specific rating histories. Resistance (0.6) is substantial and organized: strike waves, litigation, ballot-counter-campaigns, and the EU platform-work directive. The measurement series run on one shared time grid (interval units approximate years, t0 = 2009 platform-era onset, t16 = 2025) so every tracked metric is authored at every examined time point; trajectories are monotonic rather than cyclical — enforcement capacity ratcheted upward through the classification wars rather than oscillating. Coalition note: gig_workers are authored powerless despite their numbers because fragmentation, individualized rating, geographic dispersion, and elastic replacement supply have so far defeated coalition formation; the EU directive and jurisdiction-level wins show the coalition path exists and would move the computed classification if consolidated.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the operator seat the arrangement is a product it designed and continuously optimizes — flexibility is a feature it sells to both sides of the market. From the investor seat it is a return stream whose margin depends on the classification holding. From the worker seat the same structure is an income channel whose terms change unilaterally and whose risks arrive privately. From the insurer seat it is contribution leakage with deferred liabilities. From the consumer seat it is convenience priced fairly. The engine derives these divergent per-seat classifications from the power/exit/role data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for platform_operators (collects the spread directly), platform_investors (collects distributions on preserved margin), and on_demand_service_consumers (receives price levels subsidized by externalized risk — a genuine but indirect beneficiary, hence not the gain_flow seat). Victim declarations drive high directionality for gig_workers (bear volatility, asset costs, and protection gaps with constrained exit — nearest the full-target end), traditional_sector_employers (bear the compliance asymmetry but retain conventional revenue structures, so moderately below full-target), and social_insurance_systems (bear diffuse fiscal leakage, trapped by statutory function). Deactivated_workers sit at the extreme target end with zero voice; labor_regulators are analytical and roughly symmetric. No directionality overrides are used: the derivation from role declarations plus exit options captures every seat's relationship, and the override surface is keyed by power atom, which would smear corrections across the four distinct institutional seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents mislabeling in both directions. A pure-snare reading would erase the genuine coordination benefit — low-barrier income access and real-time matching that workers demonstrably use and value. A pure-rope reading would erase the asymmetric transfer riding on that matching: the same structure that solves the coordination problem also moves risk downward and surplus upward, and it holds only because enforcement actively defends the classification. On the genealogy axis, the founding problem (post-crash matching of idle capacity to demand) is still live — matching continues to function — but the arrangement's center of gravity has drifted toward defending its risk allocation rather than improving the match, which the slowly rising theater_ratio tracks. The mandate has not fully outlived the function, so no mandatrophy resolution is declared; the drift series is the early-warning surface if the matching function further atrophies behind the classification defense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the precarity_extraction_reading of kernel flexible_employment_legitimacy: would the market_efficiency_reading or developmental_state_reading classify the same observable arrangement differently, and where exactly is the disagreement located?',
    'Side-by-side engine runs of the three sibling stories over identical structural data; the disagreement locates in whether risk externalization is a priced voluntary trade (efficiency reading), a transitional inefficiency declining under formalization (developmental reading), or constitutive extraction (this reading).',
    'Sibling readings would author materially different epsilon values and victim sets for the same arrangement; the cross-reading divergence is the kernel''s measurement object, not a defect in this file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega recording that this constraint is one reading of the flexible-employment-legitimacy kernel and locating the inter-reading disagreement.').

omega_variable(
    effective_hourly_wage_dispute,
    'What is the effective hourly return to gig work once unpaid waiting, deadhead travel, vehicle depreciation, fuel, insurance, and absent benefits are charged against gross receipts?',
    'Linked app-telemetry and time-use panel studies computing all-in hourly returns against local living-wage benchmarks, disaggregated by city and service line.',
    'Below-minimum all-in returns confirm this reading''s central quantitative claim and justify the high epsilon; comfortable all-in returns would force re-authoring epsilon downward and weaken the gig_worker victim declaration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_hourly_wage_dispute, empirical, 'Whether net-of-costs effective wages substantiate the surplus-transfer claim.').

omega_variable(
    constitutive_vs_incidental_extraction,
    'Is the surplus transfer a constitutive feature of platform unit economics (margins collapse under employee classification with benefit obligations) or an incidental markup correctable without destroying the matching function?',
    'Natural experiments from jurisdictions mandating reclassification or pay floors (UK driver status, Seattle and New York pay standards): track platform margins, service prices, coverage, and workforce retention post-reform.',
    'Survival of the matching function under reclassification supports the developmental sibling''s transitional framing and would push this story toward scaffold-like dynamics; margin collapse confirms the structural claim and hardens the snare-side gradient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_incidental_extraction, empirical, 'Whether extraction is load-bearing for the platform business model or removable overhead.').

omega_variable(
    algorithmic_discipline_measurement,
    'Does algorithmic management function as labor discipline (deactivation threat, acceptance-rate pressure, gamified incentives measurably intensifying work) or as neutral dispatch optimization?',
    'Quasi-experimental studies comparing effort intensity and earnings under varying incentive architectures; audit studies of deactivation decisions and appeal outcomes.',
    'Discipline findings support counting supervisory control as an employer function transferred to workers without compensation, raising measured extractiveness; neutral-dispatch findings would attribute less of the suppression scalar to management and more to market structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_discipline_measurement, empirical, 'Whether algorithmic control is discipline or logistics — the second structural element separating this reading from its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__precarity_extraction_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(flex_tr_t3, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 3, 0.21).
narrative_ontology:measurement(flex_tr_t6, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement(flex_tr_t9, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 9, 0.27).
narrative_ontology:measurement(flex_tr_t12, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 12, 0.29).
narrative_ontology:measurement(flex_tr_t16, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 16, 0.32).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(flex_be_t3, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 3, 0.6).
narrative_ontology:measurement(flex_be_t6, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(flex_be_t9, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 9, 0.7).
narrative_ontology:measurement(flex_be_t12, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 12, 0.74).
narrative_ontology:measurement(flex_be_t16, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 16, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(flex_su_t3, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 3, 0.5).
narrative_ontology:measurement(flex_su_t6, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 6, 0.57).
narrative_ontology:measurement(flex_su_t9, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 9, 0.63).
narrative_ontology:measurement(flex_su_t12, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 12, 0.67).
narrative_ontology:measurement(flex_su_t16, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 16, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__precarity_extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy__developmental_state_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'flexible employment' decomposes, per the epsilon-invariance principle, into three structurally distinct claims that cannot share one story: (1) market_efficiency_reading — a legitimate clearing mechanism, negligible extraction, no victim set; (2) developmental_state_reading — a transitional form whose justification is the passage to formalization, sunset-shaped; (3) this file, precarity_extraction_reading — structural precarity constitutive of surplus capture, high epsilon, named victims. Upstream/downstream structure: the efficiency reading is cited by platforms as legitimation for the standing arrangement; this reading's diagnosis feeds the developmental reading's prescription. All three stories link one another through network.affects_constraints; each carries a single stable epsilon over the same standing arrangement, assessed by its own lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
