% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__precarity_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Flexible Employment as Structural Precarity (Platform Extraction Reading)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   Across platform economies and increasingly in conventional labor markets,
 *   work is organized through flexible, non-standard arrangements:
 *   app-dispatched gig work, zero-hours contracts, agency and freelance
 *   chains. This story instantiates ONE reading of the contested kernel
 *   'flexible employment legitimacy': the precarity_extraction_reading, which
 *   holds that flexible employment is structural precarity enabling platforms
 *   and firms to take surplus value while externalizing business risk onto
 *   workers. Per the epsilon-invariance principle, epsilon here refers to the
 *   standing flexible-employment arrangement as this reading assesses it —
 *   high, because wage gains are offset by risk externalization, algorithmic
 *   management functions as labor discipline, and social-security gaps shift
 *   costs onto workers. The sibling readings (market_efficiency_reading,
 *   developmental_state_reading) are separate constraints with their own
 *   epsilon, beneficiary structures, and classifications; they are linked
 *   through network.affects_constraints, not folded into this story. Claim
 *   and metrics are authored independently: the claimed type is what this
 *   reading holds structurally true; the metrics describe the arrangement's
 *   operation as this reading observes it. KEY AGENTS (by structural
 *   relationship): - platform_operators: Primary beneficiary and
 *   agenda-setter (institutional/arbitrage) — sets rates, runs algorithmic
 *   enforcement, collects commissions - gig_platform_workers: Primary target
 *   (powerless/trapped) — bears the externalized risk stack -
 *   zero_hours_contract_workers: Secondary target (powerless/trapped) —
 *   formal employment shell, no guaranteed hours -
 *   on_demand_service_consumers: Diffuse beneficiary (organized/mobile) —
 *   below-cost pricing - businesses_using_contingent_labor: Secondary
 *   beneficiary (powerful/arbitrage) — converts fixed payroll to variable
 *   cost - public_social_insurance_systems: Residual cost bearer
 *   (institutional/trapped) — absorbs uncovered risk -
 *   platform_worker_union_organizers: Excluded voice (organized/constrained)
 *   — outside bargaining law - labor_regulators: Analytical observer
 *   (institutional/analytical) — sees the full structure through testimony
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, 0.72).
domain_priors:suppression_score(flexible_employment_legitimacy__precarity_extraction_reading, 0.62).
domain_priors:theater_ratio(flexible_employment_legitimacy__precarity_extraction_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__precarity_extraction_reading, tangled_rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__precarity_extraction_reading, "Flexible Employment as Structural Precarity (Platform Extraction Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__precarity_extraction_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__precarity_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__precarity_extraction_reading, '036e9d71-b21f-4562-a354-bd15d5bbec6c').
narrative_ontology:cs_kernel_codification('036e9d71-b21f-4562-a354-bd15d5bbec6c', distributed).
narrative_ontology:cs_authority_grounding('036e9d71-b21f-4562-a354-bd15d5bbec6c', distributed).
narrative_ontology:cs_reading_relation('036e9d71-b21f-4562-a354-bd15d5bbec6c', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('036e9d71-b21f-4562-a354-bd15d5bbec6c', flexible_employment_legitimacy__developmental_state_reading, influences).
narrative_ontology:cs_axiom('036e9d71-b21f-4562-a354-bd15d5bbec6c', foundational, platform_labor_constitutes_subordinated_employment).
narrative_ontology:cs_axiom_status(platform_labor_constitutes_subordinated_employment, holdable).
narrative_ontology:cs_axiom_grounding('036e9d71-b21f-4562-a354-bd15d5bbec6c', platform_labor_constitutes_subordinated_employment, empirically_contingent).
narrative_ontology:cs_axiom('036e9d71-b21f-4562-a354-bd15d5bbec6c', foundational, economic_dependence_grounds_social_protection_entitlement).
narrative_ontology:cs_axiom_status(economic_dependence_grounds_social_protection_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('036e9d71-b21f-4562-a354-bd15d5bbec6c', economic_dependence_grounds_social_protection_entitlement, deontological).
narrative_ontology:cs_reference_frame('036e9d71-b21f-4562-a354-bd15d5bbec6c', protected_employment_baseline).
narrative_ontology:cs_drift_state('036e9d71-b21f-4562-a354-bd15d5bbec6c', contemporary_platform_labor_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('036e9d71-b21f-4562-a354-bd15d5bbec6c', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, businesses_using_contingent_labor).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, on_demand_service_consumers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, gig_platform_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, zero_hours_contract_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, public_social_insurance_systems).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, independent_contractor_classification_doctrine).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, labor_market_flexibility_premise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and operate the dispatch, pricing, and rating systems through which contingent work is allocated. Classify the people performing tasks as independent contractors rather than employees, set piece rates unilaterally, and enforce performance standards through acceptance-rate thresholds and account deactivation. Collect a commission on each transaction and carry none of the payroll taxes, sick pay, injury compensation, or pension obligations that attach to employment status. Operate across many jurisdictions and can relocate entities, restructure contracts, or adjust algorithms faster than any single regulator can respond.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Perform delivery, driving, care, microtask, or freelance work dispatched through apps. Bear the costs of vehicles, fuel, insurance, tools, and unpaid waiting time, and receive no sick pay, paid leave, minimum-wage guarantee, or pension accrual. Income depends on remaining available during demand peaks; declining work or falling below acceptance thresholds risks reduced dispatch priority or account deactivation. Most entered because formal hiring channels were closed, slow, or insufficient, and vehicle or training debt incurred for the work ties them to continuing; leaving a platform means losing an income stream they rely on.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, gig_platform_workers, payer,
    powerless, biographical, trapped, global).

% Hold formal employment contracts that guarantee no minimum hours. Must stay reachable for shift offers whose volume the employer controls week to week; refusing shifts reduces future offers. Income volatility makes budgeting, credit access, and housing tenancy difficult. Alternative local employers run the same contract form, so moving employers does not change the arrangement.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, zero_hours_contract_workers, payer,
    powerless, biographical, trapped, national).

% Purchase rides, deliveries, and services at prices and speeds made possible by the staffing model. Switch between apps freely based on price and wait time. The connection between the prices they pay and the absence of employer costs on the supply side is not visible in the transaction.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, on_demand_service_consumers, beneficiary,
    organized, immediate, mobile, global).

% Staff warehouses, care rosters, logistics fleets, and project teams through agency contracts, freelance invoices, and platform dispatch instead of permanent hires. Convert fixed payroll into variable cost, avoid contributions and severance obligations, and scale headcount with demand. Can reclassify roles, rotate agencies, or shift geographies when classification rules tighten.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, businesses_using_contingent_labor, beneficiary,
    powerful, biographical, arbitrage, global).

% Finance unemployment support, housing assistance, public healthcare, and eventual pension top-ups for workers whose periods of low or absent earnings generate no contributions. The contribution base erodes as more work is performed outside employment status, while outlays on in-work poverty and old-age poverty rise. Cannot decline the residual risk; it arrives through the welfare system regardless.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, public_social_insurance_systems, payer,
    institutional, generational, trapped, national).

% Attempt to organize courier and driver collectives, strike over rate cuts, and litigate for employee status. In most jurisdictions collective-bargaining law does not reach workers classified as contractors, so their organizations operate in a legal grey zone; wins such as rider-status rulings and the EU platform work directive came after years of campaigning. Members are individually identifiable through the platforms' own data systems.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_worker_union_organizers, excluded,
    organized, biographical, constrained, continental).

% Investigate misclassification, run enforcement pilots, and draft presumption rules. Hear testimony from platforms, workers, and client firms; commission economic analysis; face intense lobbying and jurisdictional competition, since tightening rules in one territory risks displacing work to laxer ones.
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
% COORDINATION_FUNCTION: Matches dispersed, individually small labor supply to fluctuating, geographically scattered demand in real time; lowers search and transaction costs on both sides; provides an income channel that bypasses slow, credential-gated formal hiring; lets firms scale headcount with demand.
% TRANSFER_FUNCTION: Moves surplus value and business risk from workers to platforms, intermediary firms, and consumers: commissions on each transaction flow upward; uncompensated waiting time, equipment, fuel, insurance, injury risk, and pension non-accrual shift downward onto workers; consumers receive below-cost pricing financed by that shift; residual uncovered risk lands on public social-insurance budgets.
% ABSENT_VOICES: Workers have no seat where dispatch algorithms, acceptance thresholds, and deactivation rules are designed; union organizers are locked out of collective-bargaining frameworks that cover only employees; future retirees who will draw on top-ups for today's uncovered years are represented by no one in the transaction.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, on-demand prices would jump as employer costs internalized, several platform business models would collapse or restructure into employment relationships, millions of workers would lose an income channel they currently depend on while gaining protection coverage, and logistics, food delivery, and care rostering would reorganize around formal employment or higher-priced formal alternatives. The underlying demand does not disappear; its organization does.
% FOUNDING_PROBLEM: After 2008, large pools of underemployed labor and idle personal assets met smartphone-enabled dispatch technology: the founding problem was matching that idle capacity to latent demand for instant, small-batch services without the overhead of formal employment.
% FOUNDING_PROBLEM_CORROBORATION: Independent labor-economics research documents real matching-efficiency and income-access gains even where it criticizes the risk structure; worker surveys record flexibility valued by subsets of workers; court findings in misclassification cases concede the service innovation while ruling on status. Corroboration of the founding problem's liveness comes from outside the benefiting parties — though the same sources dispute whether the arrangement's current shape is necessary to solve it.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__precarity_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__precarity_extraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__precarity_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__precarity_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.72) because the arrangement decouples reward from contribution: piece rates are set unilaterally, waiting time and expenses are uncompensated, and the wedge between what a task yields and what the worker receives funds commissions and below-cost consumer pricing. Suppression (0.62) is structural rather than theatrical: acceptance thresholds, dispatch-priority penalties, and deactivation power discipline labor without formal managerial hierarchy, and income dependence limits walk-away. Theater ratio (0.31) reflects the 'be your own boss' and 'flexibility choice' framing layered over what operates as managed labor, plus gamified earnings dashboards that present unilateral terms as personal performance. Accessibility collapse is moderate (0.45): formal employment, other platforms, and self-employment remain nominally available, but for the core target seats income necessity and asset commitments close the practical exit. Resistance (0.55) is real and growing — strikes, misclassification litigation, the EU platform work directive — yet has so far reshaped the arrangement at the margins rather than dissolved it. The three measurement series share one time grid (t=0 approximates 2009, t=16 approximates 2025) so every metric is authored at every examined point. Suppression_requirement is tracked deliberately: enforcement capacity visibly built up over the interval (algorithmic management matured, deactivation systems hardened, classification defenses professionalized), which is an enforcement-infrastructure dynamic, not merely an extraction shift. Claimed type and metrics were authored independently; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting seat (platform_operators) experiences the arrangement as coordination it built and optimizes: matching, scale, option value. The target seats (gig_platform_workers, zero_hours_contract_workers) experience the same structure as disciplined dependency: rates they cannot negotiate, availability they cannot refuse without penalty, protections they cannot accrue. Consumers sit near-symmetric — real service gains, diffuse hidden costs. Regulators see both faces simultaneously, which is precisely why classification disputes concentrate in courts. The engine computes these divergent per-seat classifications from power, exit, and directional position; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators combine agenda-setting with direct collection (commissions, avoided contributions), placing them at the beneficiary pole. Businesses using contingent labor collect avoided fixed costs without running the dispatch machinery — strongly beneficiary. On-demand consumers benefit through below-cost pricing with mobile exit — mildly beneficiary, near-symmetric once their own payment is weighed. Gig and zero-hours workers bear the externalized risk stack with trapped exit — full-target pole. Public social-insurance systems absorb residual costs they cannot decline — moderately targeted. Union organizers are workers-in-resistance: structurally targeted, currently excluded from the bargaining table. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce these positions through the structural derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification is what keeps this reading honest. A pure-snare rendering would erase the genuine matching function the arrangement performs — real-time allocation of dispersed labor to fluctuating demand is a real coordination achievement, and worker-side surveys attest that some workers value the flexibility. A pure-rope rendering would erase the asymmetry this reading exists to name. Holding both: the founding problem (idle-capacity matching amid post-2008 labor slack) is still live, so no mandatrophy is declared; the arrangement persists because the problem persists AND because extraction rides on it. The classification would drift toward piton only if automated dispatch displaced human labor entirely, leaving the contractual shell theatrically maintained; it would drift toward snare if exit options collapsed further (universal deactivation blacklists, de facto exclusivity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This constraint is one reading of the flexible_employment_legitimacy kernel (reading: precarity_extraction_reading) — is the corpus treating the sibling readings as separate constraints rather than folding them into this one?',
    'Corpus check: sibling files market_efficiency_reading and developmental_state_reading exist with their own epsilon, victims, and claimed types; cross-reading comparison runs through network edges, never merged metrics.',
    'If readings were merged, epsilon would average across incompatible assessments and classification would be meaningless; kept separate, each reading''s divergence is itself the measurable signal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Committer-frame membership: one reading of a contested kernel, siblings held separately.').

omega_variable(
    sibling_delta_market_efficiency,
    'What would the market_efficiency_reading change structurally if instantiated instead of this reading?',
    'Author the sibling story: under voluntary-exchange premises, epsilon falls toward coordination-cost levels, the victim set empties (workers become transacting parties), and the computed type moves toward rope.',
    'The disagreement is located in one element: whether algorithmic control (unilateral rates, acceptance thresholds, deactivation) constitutes subordination sufficient to defeat the voluntariness premise. Resolving that element converges or cleanly separates the two readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_delta_market_efficiency, conceptual, 'Sibling structural delta: the efficiency reading removes victims and lowers epsilon.').

omega_variable(
    sibling_delta_developmental_state,
    'What would the developmental_state_reading change structurally if instantiated instead of this reading?',
    'Author the sibling story: under transitional-form premises, extraction is discounted by expected duration (sunset-like logic), victims are temporary, and the type leans scaffold pending formalization progress.',
    'The disagreement is located in whether precarity is structural or transitional: longitudinal data on whether contingent shares stabilize or formalize decides which reading the evidence supports.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_delta_developmental_state, conceptual, 'Sibling structural delta: transitional framing discounts extraction by expected duration.').

omega_variable(
    net_earnings_after_risk_adjustment,
    'Do platform workers'' net earnings — after vehicle, fuel, insurance, unpaid waiting time, and injury-risk adjustment — exceed comparable formal-sector wages?',
    'Administrative tax data matched to platform earnings histories; expense-audited panel studies comparing gross app income to hourly net.',
    'If net earnings fall below comparable formal wages, measured extraction is understated and the reading strengthens toward snare; if they exceed, the coordination share of the tangled rope is larger than this reading assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_earnings_after_risk_adjustment, empirical, 'Whether wage gains survive risk-and-expense adjustment.').

omega_variable(
    algorithmic_control_subordination_threshold,
    'How much algorithmic control — unilateral pricing, route assignment, acceptance-rate penalties, deactivation — before the working relationship is subordinated in fact?',
    'Comparative court analyses applying control tests to platform data; natural experiments from jurisdictions with presumption rules (EU platform work directive, Spanish rider law).',
    'Above the threshold, misclassification is constitutive and employment-protection entitlement attaches; below it, part of the measured extraction is recharacterized as the price of genuine autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_control_subordination_threshold, empirical, 'Degree of algorithmic control sufficient for factual subordination.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression holding workers in place structural (income dependence, deactivation threat, asset debt) or internalized (''being your own boss'' identity, sunk-cost commitment to the platform narrative)?',
    'Post-exit trajectory studies: workers who leave platforms for formal jobs — does deference to algorithmic terms persist? Survey instruments separating economic constraint from self-concept.',
    'If substantially internalized, effective suppression exceeds the structural measure and survives formal reclassification; remedies must then address identity framing, not just status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural versus internalized components of worker attachment.').

omega_variable(
    reclassification_elasticity,
    'If gig workers were reclassified as employees, how much work volume would be lost versus how much protection gained?',
    'Quasi-experiments: the California AB5/Prop 22 episode, the Spanish rider law, the UK Uber ruling — employment and earnings trajectories around status changes.',
    'High displacement elasticity supports the efficiency reading''s warning and tempers this reading''s remedy; low elasticity removes the main objection to full reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reclassification_elasticity, empirical, 'Employment-versus-protection tradeoff under reclassification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__precarity_extraction_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(precarity_extraction_tr_t0, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(precarity_extraction_tr_t0, observed).
narrative_ontology:measurement(precarity_extraction_tr_t3, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 3, 0.2).
narrative_ontology:measurement_basis(precarity_extraction_tr_t3, observed).
narrative_ontology:measurement(precarity_extraction_tr_t6, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 6, 0.23).
narrative_ontology:measurement_basis(precarity_extraction_tr_t6, observed).
narrative_ontology:measurement(precarity_extraction_tr_t8, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement_basis(precarity_extraction_tr_t8, observed).
narrative_ontology:measurement(precarity_extraction_tr_t11, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 11, 0.28).
narrative_ontology:measurement_basis(precarity_extraction_tr_t11, observed).
narrative_ontology:measurement(precarity_extraction_tr_t13, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 13, 0.29).
narrative_ontology:measurement_basis(precarity_extraction_tr_t13, observed).
narrative_ontology:measurement(precarity_extraction_tr_t16, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement_basis(precarity_extraction_tr_t16, observed).

% Extraction over time
narrative_ontology:measurement(precarity_extraction_be_t0, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(precarity_extraction_be_t0, observed).
narrative_ontology:measurement(precarity_extraction_be_t3, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement_basis(precarity_extraction_be_t3, observed).
narrative_ontology:measurement(precarity_extraction_be_t6, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement_basis(precarity_extraction_be_t6, observed).
narrative_ontology:measurement(precarity_extraction_be_t8, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement_basis(precarity_extraction_be_t8, observed).
narrative_ontology:measurement(precarity_extraction_be_t11, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 11, 0.65).
narrative_ontology:measurement_basis(precarity_extraction_be_t11, observed).
narrative_ontology:measurement(precarity_extraction_be_t13, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 13, 0.69).
narrative_ontology:measurement_basis(precarity_extraction_be_t13, observed).
narrative_ontology:measurement(precarity_extraction_be_t16, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 16, 0.72).
narrative_ontology:measurement_basis(precarity_extraction_be_t16, observed).

% Suppression requirement over time
narrative_ontology:measurement(precarity_extraction_su_t0, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(precarity_extraction_su_t0, observed).
narrative_ontology:measurement(precarity_extraction_su_t3, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 3, 0.42).
narrative_ontology:measurement_basis(precarity_extraction_su_t3, observed).
narrative_ontology:measurement(precarity_extraction_su_t6, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement_basis(precarity_extraction_su_t6, observed).
narrative_ontology:measurement(precarity_extraction_su_t8, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement_basis(precarity_extraction_su_t8, observed).
narrative_ontology:measurement(precarity_extraction_su_t11, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 11, 0.57).
narrative_ontology:measurement_basis(precarity_extraction_su_t11, observed).
narrative_ontology:measurement(precarity_extraction_su_t13, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 13, 0.6).
narrative_ontology:measurement_basis(precarity_extraction_su_t13, observed).
narrative_ontology:measurement(precarity_extraction_su_t16, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement_basis(precarity_extraction_su_t16, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__precarity_extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, developmental_state_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the 'flexible employment' label per the epsilon-invariance principle: the colloquial concept conflates at least three structurally distinct claims — voluntary exchange (market_efficiency_reading, low epsilon, no victims), transitional stage (developmental_state_reading, temporally bounded extraction), and structural precarity (this reading, high epsilon, persistent victims). Each member carries its own epsilon, victim set, and claimed type; the links here record the family edges. Direction of rhetorical influence runs from the efficiency claim DOWNSTREAM into this one: platforms cite market-clearing legitimacy as cover, which this reading contests — the family edge records that citation pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
