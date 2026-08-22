% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__legalization_reading, []).

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
 *   constraint_id: substance_control_kernel__legalization_reading
 *   human_readable: Substance Control via Legalization with Externality Regulation
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The legalization reading frames substance use as an individual liberty
 *   issue, legitimating state intervention ONLY to prevent third-party harm
 *   (DUI, secondhand exposure) and capture externality costs via taxation.
 *   Users move from victim set (criminal penalties in prohibition) to
 *   beneficiary set (autonomy without incarceration); third parties move into
 *   the victim set (externality bearers); the state becomes an agenda-setter
 *   and revenue collector; and a regulated legal industry emerges as
 *   beneficiary. This reading competes with prohibition_reading (which
 *   grounds intervention in moral transgression) and harm_reduction_reading
 *   (which grounds intervention in health management independent of use
 *   cessation). The three readings share the same kernel—state involvement in
 *   substance control—but instantiate fundamentally different constraints
 *   with different victim sets, beneficiary structures, and enforcement
 *   logics. This JSON describes ONLY the legalization reading as a clean
 *   ε-invariant constraint.
 *
 * KEY AGENTS:
 *   - substance_users: shift from criminal victim to autonomous agent with externality regulation
 *   - regulated_legal_industry: new beneficiary emerging under legalization regime
 *   - state_revenue_apparatus: agenda-setter and dual beneficiary (taxation + crime reduction)
 *   - third_party_externality_bearers: new victim set (DUI, secondhand exposure, healthcare burden)
 *   - treatment_capacity: payer bearing unmet need from legalization-driven use prevalence
 *   - black_market_suppliers: excluded by legalization enforcement, persist in gaps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.58).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.42).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Substance Control via Legalization with Externality Regulation").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, '9ee906b4-a040-43ce-b1c9-60e693883cf6').
narrative_ontology:cs_kernel_codification('9ee906b4-a040-43ce-b1c9-60e693883cf6', distributed).
narrative_ontology:cs_authority_grounding('9ee906b4-a040-43ce-b1c9-60e693883cf6', extraction).
narrative_ontology:cs_reading_relation('9ee906b4-a040-43ce-b1c9-60e693883cf6', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('9ee906b4-a040-43ce-b1c9-60e693883cf6', substance_control_kernel__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('9ee906b4-a040-43ce-b1c9-60e693883cf6', foundational, substance_use_individual_liberty_right).
narrative_ontology:cs_axiom_status(substance_use_individual_liberty_right, holdable).
narrative_ontology:cs_axiom_grounding('9ee906b4-a040-43ce-b1c9-60e693883cf6', substance_use_individual_liberty_right, deontological).
narrative_ontology:cs_axiom('9ee906b4-a040-43ce-b1c9-60e693883cf6', foundational, state_intervention_externality_justified).
narrative_ontology:cs_axiom_status(state_intervention_externality_justified, holdable).
narrative_ontology:cs_axiom_grounding('9ee906b4-a040-43ce-b1c9-60e693883cf6', state_intervention_externality_justified, instrumental).
narrative_ontology:cs_reference_frame('9ee906b4-a040-43ce-b1c9-60e693883cf6', autonomous_substance_user_with_regulated_externality_capture).
narrative_ontology:cs_drift_state('9ee906b4-a040-43ce-b1c9-60e693883cf6', contemporary_tax_revenue_shortfall, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9ee906b4-a040-43ce-b1c9-60e693883cf6', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, regulated_legal_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_revenue).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, users_exercising_autonomy).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, third_party_externality_bearers).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, addiction_treatment_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, substance_users).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, criminal_justice_system).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, criminal_justice_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under legalization, users retain the choice to use or abstain without criminal sanction. They benefit from legal access, quality control, and the absence of arrest and incarceration. Some users argue they also bear costs: exposure to regulation, taxation, and residual social stigma. The shift from victim (in prohibition) to beneficiary (in legalization) is the core reading delta.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, substance_users, beneficiary,
    organized, biographical, mobile, national).

% Legal manufacturers, retailers, testing labs, and ancillary services (packaging, advertising, distribution) emerge as organized industries under legalization. They collect profit from legal sales, are subject to regulatory compliance costs, and lobby for favorable tax and licensing terms. Their existence depends on the legalization regime persisting.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, regulated_legal_industry, beneficiary,
    powerful, generational, arbitrage, national).

% The state sets regulations, collects taxes on sales, and administers licensing and quality control. It frames the arrangement as capturing externality costs (DUI enforcement, health treatment) and funding prevention via taxation. It is both the enforcer of the rule and a direct financial beneficiary. This dual role (agenda-setter and payer of some externalities) creates incentive tensions.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, state_revenue_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Non-users bear the costs of substance-related externalities: DUI injury and death, secondhand smoke/vapor exposure, workplace/school disruption, emergency room overflow, and property crime by users seeking funds. Under legalization, the state's taxation is meant to internalize these costs, but actual funding rarely matches the full externality cost. These actors have no voice in setting the rule and no exit from exposure.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, third_party_externality_bearers, payer,
    powerless, biographical, trapped, national).

% Treatment providers, public health systems, and harm-reduction agencies are expected to manage addiction under legalization with state-collected tax revenue. The constraint imposes a tension: legalization increases access and use prevalence (expanding need), but tax revenue often lags need growth or is diverted to other priorities. Treatment capacity is perpetually under-resourced relative to the problem.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, addiction_treatment_capacity, payer,
    moderate, generational, constrained, national).

% Illegal suppliers are structurally excluded by legalization: they lose their market share to legal producers and are subject to law enforcement. Where legalization leaves gaps (high taxes, limited access, regulatory delays), black markets persist in serving price-sensitive or geographically remote users. They would argue for recognition as a competing distribution system but are kept out by the enforcement of the legalization regime itself.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, black_market_suppliers, excluded,
    organized, biographical, trapped, national).

% Legalization removes incarceration for simple possession, reducing caseload and prison populations. However, it retains enforcement against impaired driving, underage sales, and trafficking. Law enforcement budgets are redirected from possession enforcement to DUI and supply-chain oversight. The system shifts from criminalizing users to regulating the market—a reorganization rather than an exit.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, criminal_justice_system, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, criminal_justice_system, payer).

% International treaties (UN drug conventions) set baseline prohibition norms. Legalization reading jurisdictions navigate around these norms via domestic statutory override or treaty withdrawal. International observers and treaty signatories can challenge the legitimacy of legalization but lack enforcement power over sovereign legislation.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, international_drug_control_regime, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__legalization_reading, regulated_legal_industry).
narrative_ontology:fixing_cost_class(substance_control_kernel__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes substance distribution through regulated legal channels, replacing fragmented black-market supply: standardized quality, predictable pricing, age-gating, and integrated externality taxation. Solves the quality-assurance and price-discovery problems of illegal markets while creating a revenue stream to fund externality management.
% TRANSFER_FUNCTION: Moves money from substance users (via purchase price and taxation) to the legal industry and the state; moves money from the state (via tax allocation) to treatment and enforcement. It also redistributes harm: away from users (no criminal penalty) and toward regulated management of externalities borne by third parties.
% ABSENT_VOICES: Black market suppliers would argue for market recognition as a competing distribution system. Some users argue legalization doesn't go far enough (e.g., demand state monopoly on distribution to prevent industry profiteering). Public health officials and harm-reduction advocates argue the reading subordinates health to autonomy, under-funding treatment and prevention.
% DISAPPEARANCE_RATIONALE: If the legalization regime disappeared—returning to prohibition—supply would re-criminalize overnight, users would return to unregulated markets, treatment funding would evaporate, the entire regulated industry would collapse, and incarceration rates would spike. The entire enforcement, taxation, and regulatory apparatus depends on legalization persisting.
% FOUNDING_PROBLEM: Prohibition created black markets, incarcerated millions, failed to eliminate use, and generated organized crime revenue. Legalization was proposed to reduce incarceration, improve product safety, fund treatment via taxation, and preserve user autonomy while capturing externality costs via regulation.
% FOUNDING_PROBLEM_CORROBORATION: Public health researchers in legalization pilot jurisdictions (Colorado, Oregon, Canada) document reduced incarceration and improved product safety. Externality cost accounting (from DOT and health agencies) shows taxation does not fully fund externalities created. Jurisdictions maintaining prohibition and international drug control treaty bodies attest the founding problem is overstated or that legalization creates worse harms (increased use prevalence, gateway effects).
narrative_ontology:disappearance_verdict(substance_control_kernel__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_kernel__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__legalization_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because the legalization regime distributes gains across users (autonomy benefit), the legal industry (profit), and the state (taxation) while imposing costs on third parties (externalities). The trajectory shows rising extractiveness early (0-10 years) as the legal industry consolidates and discovers it can lobby for favorable tax rates and regulatory exemptions, then plateaus as the system reaches equilibrium. Suppression is lower (0.42 at end) than in prohibition because the constraint does not criminally penalize users; instead it enforces the legal boundary (age gating, DUI enforcement, black market suppression). Theater_ratio rises early (0-10 years) as regulatory compliance becomes theatrical (testing requirements that don't track true potency, advertising restrictions that are largely performative), then stabilizes at 0.31. The measurements are authored on one shared time grid: every metric has a value at every time point (0, 5, 10, 15, 20, 25 years). The basis field distinguishes observed data (0-15 years, from legalization pilot jurisdictions) from projected data (20-25 years, forward estimate).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (third_party_externality_bearers, treatment_capacity) and the beneficiary seats (regulated_legal_industry, state_revenue, users) compute radically different types from the same structural facts. From the beneficiary seats, the constraint is genuine coordination (replaced black markets with regulated supply, funded treatment, preserved autonomy). From the externality-bearer seats, the constraint is an extraction arrangement where third parties bear the harm cost while the legal industry and state capture the revenue. The engine computes per-seat classifications: beneficiary seats will likely compute rope or tangled_rope; externality-bearer seats will likely compute tangled_rope or snare. The perspectival gap arises because directionality differs by 0.5-0.7 between beneficiary and payer seats, and power levels differ (state/legal_industry institutional; third_parties powerless). The authored claim (rope) is the beneficiary reading; the metrics (high extractiveness, asymmetric suppression of black markets and user autonomy, rising theater) support a more extractive reading from the payer perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Users have low directionality (d ~0.2) under legalization: they benefit from autonomy and legal access, bearing mainly the indirect cost of taxation. The regulated_legal_industry has d near 0.1 (clear beneficiary, no suppression cost). The state has d ~0.3-0.4 (collects revenue, bears some externality cost via treatment funding, enforces the boundary). Third_party_externality_bearers have high d (0.75-0.9): they bear externality costs without choice or compensation. Treatment_capacity has d ~0.65: it bears unmet need (constrained resources) but is partially subsidized by state taxation. Black_market_suppliers have d = 1.0 (pure targets of enforcement exclusion). The variation in d across seats drives the per-seat type divergence: beneficiary seats compute low χ (effective extraction diminished); payer seats compute high χ (effective extraction amplified by suppression and powerlessness).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prohibition's failures: incarceration, black markets, product danger) is structurally live under legalization—the constraint exists because that problem persists in jurisdictions that maintain prohibition. However, legalization's own founding problem is contested: whether legalization adequately internalizes externality costs via taxation, and whether it actually reduces overall harm or merely shifts it from users to third parties. The measurement trajectory shows extraction rising (0.38 to 0.58) over the first 20 years, suggesting that the legal industry and state increasingly consolidate revenue and regulatory power. If this trajectory continues and externality taxation remains flat, the constraint risks mandatrophy: the founding rationale (externality capture) diverges from actual operation (revenue concentration), and a new regime might be demanded (higher taxation, state monopoly on supply, or return to prohibition). The theater_ratio trajectory (rising to 0.31) supports this concern: regulatory theater increasingly substitutes for genuine externality internalization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_taxation_shortfall,
    'Does state tax revenue from legalized substance sales actually internalize third-party externality costs, or is taxation set below the true externality cost?',
    'Empirical accounting: compare external cost estimates (DUI, healthcare, productivity loss) against actual tax revenue allocated to treatment and prevention.',
    'If taxation falls short, the constraint is extractive from third parties—they bear unpaid externality costs while the state and legal industry profit. The constraint would shift from rope (genuine coordination) toward tangled_rope (asymmetric extraction hidden by a coordination narrative).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(externality_taxation_shortfall, empirical, 'Whether legalization''s tax mechanism genuinely captures externality costs or underfunds remediation.').

omega_variable(
    autonomy_vs_harm_reading_contest,
    'Is the legalization reading''s prioritization of user autonomy over harm reduction a structural necessity or a framing choice?',
    'Conceptual: the harm_reduction_reading prioritizes health outcomes independent of use cessation; the legalization_reading prioritizes autonomy with state regulation of externalities. These are distinct kernel readings, not complementary lenses on the same constraint.',
    'If autonomy is the structurally grounding axiom, the reading holds as stated. If harm reduction is adopted as the primary goal, the constraint''s classification shifts: the state becomes a payer (for unmet health needs) rather than a beneficiary (from taxation), and third parties shift from external cost-bearers to participants in a coordinated health strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_vs_harm_reading_contest, conceptual, 'The foundational reading contest: autonomy-centered vs. health-centered framing of substance control.').

omega_variable(
    black_market_persistence,
    'Under legalization, does the black market collapse entirely or persist as a parallel system serving gaps in the legal market?',
    'Empirical observation from legalization pilot jurisdictions: track black market prices, supply volumes, and user distribution between legal and illegal sources over time.',
    'If the black market collapses, legalization genuinely replaces prohibition''s coordination problem. If it persists, legalization creates a two-tier market (legal regulated, illegal unregulated) with users stratified by price sensitivity and geographic access. Persistent black markets mean the constraint is imperfectly enforced and some users remain in a semi-prohibited state.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(black_market_persistence, empirical, 'Whether legalization is a complete regime shift or a layered arrangement.').

omega_variable(
    reading_sibling_axis_kernel_contest,
    'Which reading of the substance_control_kernel grounds the legitimacy of state intervention?',
    'This is the core committer-frame ambiguity: prohibition_reading asserts moral transgression as the grounding; harm_reduction_reading asserts health management; legalization_reading asserts externality capture and autonomy. These cannot coexist in a single state''s decision-making framework—they are structurally incompatible foundational premises.',
    'The reading adopted determines the entire victim set, beneficiary structure, and enforcement machinery. A state cannot simultaneously criminalize use (prohibition), fund treatment regardless of use continuation (harm reduction), and regulate externalities while permitting use (legalization). The constraint this story instantiates is the legalization version; the other readings are OTHER constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_sibling_axis_kernel_contest, conceptual, 'The kernel-level reading contest: which normative grounding legitimizes state intervention?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(subs_tr_t5, substance_control_kernel__legalization_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__legalization_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(subs_tr_t15, substance_control_kernel__legalization_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__legalization_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(subs_tr_t25, substance_control_kernel__legalization_reading, theater_ratio, 25, 0.31).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(subs_be_t5, substance_control_kernel__legalization_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__legalization_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(subs_be_t15, substance_control_kernel__legalization_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__legalization_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(subs_be_t25, substance_control_kernel__legalization_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(subs_su_t5, substance_control_kernel__legalization_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__legalization_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(subs_su_t15, substance_control_kernel__legalization_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__legalization_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(subs_su_t25, substance_control_kernel__legalization_reading, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__legalization_reading, 0.18).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% The substance_control_kernel constraint family comprises three structurally distinct readings, each a separate constraint with its own ε, victim/beneficiary set, and enforcement logic. Legalization_reading shifts the victim set FROM users (in prohibition) TO third-party externality bearers, and the beneficiary set to users + legal industry + state. The ε difference arises from the distinct victim/beneficiary structures, not measurement ambiguity: prohibition targets users (high extraction from them), legalization externalizes costs to non-users (extraction from third parties). Each reading is a complete constraint story; they are linked by network.affects_constraints to document the kernel contest and enable cross-reading analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__legalization_reading, powerless, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
