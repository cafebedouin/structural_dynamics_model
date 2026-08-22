% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__freedom_floor_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: unconditional_income_support__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Autonomy-Enabling Floor
 *   domain: political_economy/social_policy/welfare_state
 *
 * SUMMARY:
 *   This is the freedom-floor reading of unconditional income support: a
 *   social policy that provides every resident with an unconditional,
 *   universal, modest cash transfer sufficient to meet basic needs without
 *   employment. The reading frames the transfer as removing coercion from
 *   labor markets, eliminating welfare stigma, and enabling participation in
 *   valuable work that markets systematically undervalue. The structural
 *   claim is that this is a coordination mechanism solving collective-action
 *   problems in labor-market formation and social recognition, not an
 *   extraction mechanism. The reading attributes no victimization—it claims
 *   Pareto improvement via autonomy expansion. This is one of three readings
 *   of a contested kernel; the other readings (dependency-trap,
 *   universality-paradox) instantiate different constraints with different ε
 *   values, beneficiary/victim structures, and types.
 *
 * KEY AGENTS:
 *   - Precarious workers: in gig, seasonal, low-wage employment; face choice between exploitative terms and destitution; gain negotiating power and mobile exit
 *   - Unpaid caregivers: parents, elderly-care providers, community organizers; confined to survival-threatening unpaid work; gain economic viability and social recognition
 *   - Creative workers: artists, writers, researchers; face forced wage-work for survival; gain time horizon for long-gestation projects
 *   - Abuse survivors: people exiting intimate-partner, child, or institutional abuse; face economic coercion to stay; gain exit mechanism
 *   - Fiscal authority: administers and funds the system; sets the rate and eligibility (unconditional, universal in this reading)
 *   - Labor-market participants as a class: gain from tightened labor markets and improved bargaining conditions
 *   - Society-wide (vindicated proposition, not an agent): the axioms the reading vindicates—labor market freedom, stigma elimination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.28).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.15).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Unconditional Income Support as Autonomy-Enabling Floor").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "political_economy/social_policy/welfare_state").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, '9eb60891-ab73-44a0-ba0c-805357aec85d').
narrative_ontology:cs_kernel_codification('9eb60891-ab73-44a0-ba0c-805357aec85d', distributed).
narrative_ontology:cs_authority_grounding('9eb60891-ab73-44a0-ba0c-805357aec85d', distributed).
narrative_ontology:cs_reading_relation('9eb60891-ab73-44a0-ba0c-805357aec85d', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('9eb60891-ab73-44a0-ba0c-805357aec85d', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('9eb60891-ab73-44a0-ba0c-805357aec85d', foundational, labor_market_freedom_enables_autonomy).
narrative_ontology:cs_axiom_status(labor_market_freedom_enables_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('9eb60891-ab73-44a0-ba0c-805357aec85d', labor_market_freedom_enables_autonomy, deontological).
narrative_ontology:cs_axiom('9eb60891-ab73-44a0-ba0c-805357aec85d', foundational, unpaid_work_carries_intrinsic_value).
narrative_ontology:cs_axiom_status(unpaid_work_carries_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('9eb60891-ab73-44a0-ba0c-805357aec85d', unpaid_work_carries_intrinsic_value, deontological).
narrative_ontology:cs_reference_frame('9eb60891-ab73-44a0-ba0c-805357aec85d', coercion_free_labor_market).
narrative_ontology:cs_drift_state('9eb60891-ab73-44a0-ba0c-805357aec85d', contemporary_precarity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9eb60891-ab73-44a0-ba0c-805357aec85d', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, unpaid_caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, creative_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, abuse_survivors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, labor_market_participants).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, labor_market_freedom_doctrine).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, stigma_elimination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers in gig, seasonal, or low-wage employment gain negotiating power: they can refuse exploitative terms without facing immediate destitution. The income floor converts their labor supply from inelastic (must accept any offer to survive) to elastic (can wait for acceptable work). They exit coercive employment relationships.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, precarious_workers, beneficiary,
    moderate, biographical, mobile, national).

% Parents, elderly-care providers, and community organizers performing vital unpaid social reproductive labor gain recognition and economic viability. The income floor permits staying in caregiving roles that markets systematically undervalue. Without it, they face forced entry into wage work regardless of other responsibilities.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, unpaid_caregivers, beneficiary,
    powerless, generational, identity_locked, local).

% Artists, writers, researchers, and cultural producers face a choice: rapid income from non-creative work or slow accumulation in their primary field. The floor permits the latter without subsistence crisis. They can pursue long-gestation creative projects with visibility and integrity.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, creative_workers, beneficiary,
    moderate, biographical, constrained, regional).

% People escaping intimate-partner violence, child abuse, or institutional abuse gain economic exit: they can leave abusive relationships without immediate homelessness or forced return. The floor removes the economic coercion that holds many victims in place.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, abuse_survivors, beneficiary,
    powerless, biographical, trapped, local).

% Employed workers and labor organizations benefit from a tightened labor market: when survival no longer depends on accepting any wage, employers must improve conditions and wages to attract workers. Bargaining power shifts from employer-monopsony toward competitive equilibrium.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, labor_market_participants, beneficiary,
    organized, biographical, mobile, national).

% The government or public institution that administers the income floor sets the rate and eligibility rules, and bears the redistributive cost. This reading assumes the system is administratively simple (universal, unconditional) and funded through progressive taxation, not through deductions from other assistance programs.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, fiscal_authority, agenda_setter,
    institutional, generational, analytical, national).

% Employers with strong wage-bargaining position (tech, professional services, capital-intensive industries) have lower interest in the floor and are not named as payers. Their labor supplies are elastic for other reasons (education, mobility, professional identity). Some oppose the floor on tax grounds; they are excluded from the constraint's structural seats because this reading does not name them as bearing the extraction cost.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, high_wage_employers, excluded,
    powerful, biographical, analytical, national).

% Employers in minimum-wage sectors (retail, hospitality, food service) would experience tighter labor markets and upward wage pressure. This reading does not claim they are victimized by the floor—the constraint's structural logic is that labor markets become more competitive, not that extraction shifts to them. They would object; their objections are not modeled as structural victims in the freedom-floor reading.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, minimum_wage_employers, excluded,
    moderate, biographical, analytical, regional).

% The vindicated proposition: labor market freedom and stigma elimination are abstract goods the constraint's operation vindicates. Society-wide is not an agent—it collects no rents—but is listed here because the reading's legitimacy claim centers on these principles.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, society_wide, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(unconditional_income_support__freedom_floor_reading, society_wide).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of labor-market coercion: workers cannot unilaterally exit survival-wage employment, creating downward wage pressure and forced labor-market participation. A floor coordinate by decoupling survival from employment, enabling individual exit and restoring market competition. Solves the visibility problem of unpaid social reproduction: caregiving, community organizing, and cultural production are economically invisible without valuation; the floor recognizes them.
% TRANSFER_FUNCTION: Moves income from general taxation (progressive rate structure, presumed) to all residents at or below a threshold, unconditionally. The transfer is not from a specific extracting party but from the public revenue system. This reading claims no payer class—it frames the transfer as Pareto improving: those with strong labor-market positions gain from tightened labor markets; those with weak positions gain from autonomy; fiscal costs are distributed via progressive taxation, which this reading presumes acceptable.
% ABSENT_VOICES: Conservative economists and employers in minimum-wage sectors would object to the floor as economically inefficient and inflationary. Libertarian critics across the spectrum would object to the taxation required to fund it. Means-tested welfare advocates would object to universal provision. This reading does not seat them as structural victims—it excludes them because their objections are framed as policy disputes, not structural asymmetry. They would be present if the fiscal structure shifted (if the floor crowded out targeted aid, moving the reading toward the dependency-trap frame).
% DISAPPEARANCE_RATIONALE: If the floor vanished overnight, workers in precarious employment would lose exit options; caregivers would face forced wage-work entry or economic precarity; creative workers would lose the viability horizon for long-gestation projects. Abuse survivors would lose a critical exit mechanism. Labor-market bargaining power would shift back toward employer monopsony. The structure of who can participate in voluntary work would reorganize around survival urgency rather than choice.
% FOUNDING_PROBLEM: Labor-market desperation forces workers to accept exploitative wages and conditions. Caregiving, cultural work, and survival-crisis recovery are economically invisible and unsustainable when they cannot be done without destitution. Market mechanisms alone cannot price survival-essential work correctly. People in abusive situations cannot exit without economic ruin.
% FOUNDING_PROBLEM_CORROBORATION: Precarious workers, abuse-survivor advocates, and labor economists attest the founding problem is live: gig workers report accepting unsafe conditions for survival; domestic-violence organizations report economic dependence as a primary barrier to exit; cultural workers report forced wage work as a constraint on creative output. Independent research from Alaska Permanent Fund, Kenya GiveDirectly trials, and labor-market studies outside beneficiary parties attests the problem exists and the floor addresses it.
narrative_ontology:disappearance_verdict(unconditional_income_support__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unconditional_income_support__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__freedom_floor_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__freedom_floor_reading_tests).
:- end_tests(unconditional_income_support__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.28 at interval end, rising from 0.18) because the constraint solves coordination problems for identified beneficiaries without claiming victims. The rise reflects accumulating evidence that some labor-supply effects do occur (workers reducing precarious employment to pursue other activities), which shifts the transfer's classification from pure coordination toward modest extraction of fiscal capacity. Suppression is very low (0.15) because no one is held in place against their will—the floor removes suppression from labor markets rather than imposing it. Theater ratio is low and stable (0.12) because the administrative simplicity (universal, unconditional) keeps performative overhead minimal. Accessibility of alternatives remains moderate (0.25) because the floor does not make alternative labor arrangements materially unavailable; it makes them viable choices rather than survival requirements. Resistance is moderate-high (0.45) because the constraint meets real objections from those framing it differently (dependency-trap reading, efficiency concerns) even though this reading does not model them as structural victims. The measurement series shows slight drift in extractiveness as labor-supply responses accumulate, stabilizing as the system reaches its operating steady state.
 *
 * PERSPECTIVAL GAP:
 *   The fiscal-authority seat and the beneficiary seats should experience this constraint differently. The authority bears redistributive costs and faces potential labor-supply contraction (reducing the tax base); beneficiaries gain autonomy and bargaining power. From the authority's seat, the constraint is administratively simple but fiscally demanding and economically uncertain. From the beneficiary seats, the constraint is liberation. The precarious-worker seat sees immediate negotiating-power gain; the caregiver seat sees recognition and viability; the abuse-survivor seat sees exit mechanism. These divergences are structural, not observational. The excluded high-wage-employer seat and the minimum-wage-employer seat see the floor as economically disruptive but are not named as victims because this reading does not claim their interests are harmed in a way that requires their structural participation—their objections are policy disputes, not asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are correctly named: precarious workers, unpaid caregivers, creative workers, and abuse survivors all have their autonomy expanded by the floor. They move from constrained choice sets (survival-coerced labor, forced wage work, inability to exit abuse) to expanded ones (negotiation power, viability in unpaid work, exit mechanism). Directionality for beneficiaries is low (d toward 0.0, subsidy from the constraint's operation). No victims are named because this reading claims no extraction from identifiable parties—the transfer is from public revenue (progressively funded), not from a specific payer class. The fiscal authority bears the cost of provision, not extraction by the constraint; they choose to implement it. The excluded parties (high-wage employers, minimum-wage employers, conservative economists) have grounds to object but are not named as victims because their objections are about policy efficacy, not about structural asymmetry in the constraint itself. Directionality for the fiscal authority would be moderate-to-high (d toward 0.5-1.0) if they were named—they bear administrative and fiscal costs—but this reading does not name them as victims, treating administration as a chosen function, not an imposed burden.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and attested: labor-market coercion, invisibility of unpaid work, and economic barriers to abuse exit are documented in research and practitioner testimony outside the constraint's beneficiary parties. The disappearance verdict (world_rearranges) confirms that the constraint's operation matters to real coordination outcomes. The theater ratio remains very low, indicating the constraint's function has not atrophied into performance—it continues to do the autonomy-enabling and recognition work it was designed for. If theater ratio were to rise above 0.5, the constraint would be approaching piton status; currently it does not. No mandatrophy is detected: the founding problem persists, the constraint remains functional, and beneficiaries are not phantoms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_response_magnitude,
    'How large is the labor-supply response to unconditional income support? Do workers substantially reduce work effort (dependency-trap mechanism) or marginal-reallocation only (freedom-floor mechanism)?',
    'Randomized controlled trials with longer follow-up (3+ years); administrative data from Alaska Permanent Fund, Kenya GiveDirectly, Finland, and Stockton pilots on labor-market participation, job-switching, and sector reallocation.',
    'If labor supply contracts significantly (>10% reduction in aggregate work hours), ε rises and the constraint moves toward snare-like extraction of fiscal capacity. If reallocation is marginal and selective (workers exiting precarious for better employment, caregivers sustaining unpaid work), ε remains moderate and the rope framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_response_magnitude, empirical, 'The magnitude of labor-supply response to the floor.').

omega_variable(
    fiscal_sustainability_and_crowding,
    'Can unconditional income support be funded progressively at scale without crowding out or replacing targeted assistance to vulnerable populations? Is the constraint additive or substitutive?',
    'Fiscal-impact modeling and implementation experience from countries implementing large-scale UBI or near-UBI programs; legislative deliberation about integration with existing welfare state.',
    'If the floor crowds out targeted assistance (disability, housing, medical support), ε rises and the constraint approaches dependency-trap mechanics. If it is genuinely additive, the rope framing and moderate ε remain stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_and_crowding, empirical, 'Whether the floor is fiscally sustainable and additive or cannibalistic.').

omega_variable(
    universalism_vs_targeting_tradeoff,
    'Is universal unconditional provision necessary to achieve stigma elimination and autonomy, or can targeted assistance achieve the same outcomes with lower fiscal cost?',
    'Comparative evidence from universal (Alaska, Finland, Kenya) and targeted (Chile, Brazil, Mexico) programs on stigma reduction, autonomy measures, and labor-market participation.',
    'If targeted programs achieve equivalent outcomes, the universality of this reading is not structurally required—it becomes a political-implementation choice, not a necessity. The constraint might be reframed as selective targeted assistance (different constraint, different reading). If universality is necessary for stigma elimination (empirically true if stigma attaches to means-testing itself), then the freedom-floor reading''s universalism is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universalism_vs_targeting_tradeoff, conceptual, 'Whether universality is structurally required for the freedom-floor mechanism or merely a political choice.').

omega_variable(
    counterfactual_labor_market_equilibrium,
    'If the floor removes coercion from labor markets, what equilibrium wage and employment structure emerges? Does the floor''s effect depend on other institutional features (minimum wage, labor-market regulation, social investment)?',
    'Theoretical labor-economics modeling and empirical comparison across contexts with different labor-market institutions combined with varying levels of unconditional support.',
    'If the floor''s effects are context-dependent (working as coordination mechanism only in strong-institution settings, producing inflation or unemployment elsewhere), the constraint''s classification diverges by institutional context. This would require decomposition into multiple constraint stories, not one universal reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_labor_market_equilibrium, conceptual, 'The counterfactual equilibrium and its dependence on institutional context.').

omega_variable(
    psychological_identity_lock_in_caregiving,
    'For unpaid caregivers, does the income floor genuinely expand identity-locked exit, or does it entrench identity-locked participation in caregiving by making unpaid work financially viable?',
    'Post-implementation follow-up on caregiver exit rates, sector-switching, and subjective measures of autonomy and constraint; comparison with counterfactual of wage-forced exit.',
    'If the floor increases caregiver exit (identity-lock is broken by autonomy), the freedom-floor reading holds. If it increases caregiver persistence (identity-lock is now financially viable but still non-optional), the constraint approaches tangled_rope or snare mechanics for the caregiver seat—the floor would be vindicating their identity-locked role rather than liberating them from it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psychological_identity_lock_in_caregiving, empirical, 'Whether the floor expands or entrench identity-locked caregiving participation.').

omega_variable(
    kernel_reading_foreclosure_test,
    'Can the freedom-floor reading and the dependency-trap reading both be held within a single coherent welfare-state framework, or does endorsement of one logically foreclose the other?',
    'Theoretical examination of whether a coherent political economy can hold both: (a) that the floor removes labor-market coercion and enables autonomy, AND (b) that the floor distorts incentives and crowds out targeted aid. The resolution depends on how the same data (e.g., labor-supply reduction) is interpreted.',
    'If the readings can coexist (different parties holding different interpretations of the same mechanism), they relate via coexists_with. If one reading''s core premise logically contradicts the other''s (e.g., if coercion removal and incentive distortion are opposites), the relation is forecloses. Currently modeled as coexists_with (different parties in real disagreement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_test, conceptual, 'Whether the freedom-floor and dependency-trap readings logically coexist or foreclose each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(unco_tr_t0, observed).
narrative_ontology:measurement(unco_tr_t4, unconditional_income_support__freedom_floor_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement_basis(unco_tr_t4, observed).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__freedom_floor_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement_basis(unco_tr_t8, observed).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__freedom_floor_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement_basis(unco_tr_t12, observed).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__freedom_floor_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement_basis(unco_tr_t16, projected).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__freedom_floor_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(unco_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(unco_be_t0, observed).
narrative_ontology:measurement(unco_be_t4, unconditional_income_support__freedom_floor_reading, base_extractiveness, 4, 0.22).
narrative_ontology:measurement_basis(unco_be_t4, observed).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__freedom_floor_reading, base_extractiveness, 8, 0.25).
narrative_ontology:measurement_basis(unco_be_t8, observed).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__freedom_floor_reading, base_extractiveness, 12, 0.27).
narrative_ontology:measurement_basis(unco_be_t12, observed).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__freedom_floor_reading, base_extractiveness, 16, 0.28).
narrative_ontology:measurement_basis(unco_be_t16, projected).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__freedom_floor_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(unco_be_t20, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(unconditional_income_support__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__freedom_floor_reading, 0.18).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__dependency_trap_reading).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__universality_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'unconditional_income_support.' The constraint family decomposes because the kernel (universal unconditional cash transfer) admits multiple readings with substantially different ε values, beneficiary/victim structures, and types. The freedom-floor reading (this constraint) claims moderate extractiveness via resource-allocation coordination, no named victims, and rope classification. The dependency-trap reading claims high extractiveness via incentive distortion and crowding, with fiscal authority as effective payer, and snare classification. The universality-paradox reading claims moderate-high extractiveness via political ambiguity and cross-ideological appropriation, with implementation-path incompatibility as the extraction mechanism, and tangled_rope classification. All three readings share the same kernel (the policy commitment to unconditional universal transfer) but instantiate different constraints because they attribute different functions, different costs, and different beneficiary/victim structures to that commitment. No reading is correct; each is one party's interpretation. The ε-invariance principle requires separate constraint stories because ε (what function the commitment serves, assessed from each reading's own lights) differs substantially across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
