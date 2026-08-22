% ============================================================================
% CONSTRAINT STORY: income_support_commitment__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__freedom_floor_reading, []).

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
 *   constraint_id: income_support_commitment__freedom_floor_reading
 *   human_readable: Unconditional Income Floor — Freedom-Floor Reading
 *   domain: political economy/social policy
 *
 * SUMMARY:
 *   A national polity operates a universal, unconditional income floor: every
 *   resident above the age of majority receives a periodic cash payment with
 *   no means test, no work-search requirement, and no behavioral conditions,
 *   funded from the general tax base. This story instantiates the
 *   freedom-floor reading of that arrangement — the reading on which the
 *   floor's function is autonomy, dignity, and exit capacity: caregivers hold
 *   an independent income while caring, precarious workers can refuse the
 *   worst offers, abuse survivors hold an income no controlling partner can
 *   withdraw, and artists and early-stage entrepreneurs can survive the years
 *   their work takes to mature. On this reading the funding side is the
 *   arrangement's visible coordination cost, set and contested through
 *   ordinary democratic machinery, and no seat is targeted by it:
 *   universality means the payment and the tax net reach the same population.
 *   The claimed type and the metric values are authored independently — each
 *   states what this reading takes to be structurally and descriptively true.
 *   Sibling readings of the same kernel are separate constraints with their
 *   own epsilon and victim structure (see commentary.kernel_context and
 *   network.dual_formulation_note); they are not adjudicated here.
 *
 * KEY AGENTS:
 *   - - caregivers: primary beneficiary (powerless/constrained) — holds an independent income while caring, with no conditionality or eligibility verification attached
 *   - - precarious_workers: primary beneficiary (moderate/constrained) — refusal capacity against the worst offers and the ability to wait out gaps between contracts
 *   - - abuse_survivors: primary beneficiary (powerless/constrained) — an income no controlling partner can withdraw; the material exit path from economically abusive households
 *   - - artists_entrepreneurs: beneficiary (moderate/constrained) — risk capacity for long-maturation work that no lender or client will finance
 *   - - general_taxpayers: funding side, payer with secondary beneficiary position (organized/constrained) — funds the floor and receives it back; net position varies by income
 *   - - employers: constrained party (institutional/mobile) — wage-setting power compressed by worker exit capacity; gains on the demand side
 *   - - legislature_funding_authority: agenda setter (institutional/mobile) — sets the payment level and tax base by ordinary legislation under electoral constraint
 *   - - excluded_residents: excluded seat (powerless/trapped) — live and work in the economy, often paying taxes, but outside universality by legal status
 *   - - welfare_state_analysts: analytical observer (analytical/analytical) — tracks effects on poverty, labor supply, bargaining outcomes, and public finances
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__freedom_floor_reading, 0.15).
domain_priors:suppression_score(income_support_commitment__freedom_floor_reading, 0.1).
domain_priors:theater_ratio(income_support_commitment__freedom_floor_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Unconditional Income Floor — Freedom-Floor Reading").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political economy/social policy").

domain_priors:requires_active_enforcement(income_support_commitment__freedom_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, 'bc605a97-23f6-4362-809b-bb3f7ba0af94').
narrative_ontology:cs_kernel_codification('bc605a97-23f6-4362-809b-bb3f7ba0af94', formalized).
narrative_ontology:cs_authority_grounding('bc605a97-23f6-4362-809b-bb3f7ba0af94', distributed).
narrative_ontology:cs_reading_relation('bc605a97-23f6-4362-809b-bb3f7ba0af94', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc605a97-23f6-4362-809b-bb3f7ba0af94', income_support_commitment__targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom('bc605a97-23f6-4362-809b-bb3f7ba0af94', foundational, universality_constitutive_of_floor).
narrative_ontology:cs_axiom_status(universality_constitutive_of_floor, holdable).
narrative_ontology:cs_axiom_grounding('bc605a97-23f6-4362-809b-bb3f7ba0af94', universality_constitutive_of_floor, deontological).
narrative_ontology:cs_axiom('bc605a97-23f6-4362-809b-bb3f7ba0af94', foundational, exit_capacity_requires_unconditionality).
narrative_ontology:cs_axiom_status(exit_capacity_requires_unconditionality, holdable).
narrative_ontology:cs_axiom_grounding('bc605a97-23f6-4362-809b-bb3f7ba0af94', exit_capacity_requires_unconditionality, empirically_contingent).
narrative_ontology:cs_reference_frame('bc605a97-23f6-4362-809b-bb3f7ba0af94', universal_unconditional_autonomy_floor).
narrative_ontology:cs_drift_state('bc605a97-23f6-4362-809b-bb3f7ba0af94', contemporary_pilot_evidence_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('bc605a97-23f6-4362-809b-bb3f7ba0af94', '2026-06-12T09:30:00Z').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, abuse_survivors).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, artists_entrepreneurs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, general_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, general_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, employers).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, monopsony_discipline_thesis).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, universalist_welfare_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provide unpaid care to children, elderly relatives, or disabled family members, which keeps them out of paid work or confined to marginal part-time work. The unconditional payment arrives without work-search requirements or eligibility interviews, so caring does not require proving destitution or accepting surveillance. Without the payment their income depends entirely on a partner's earnings or on leaving care duties unmet; with it they hold an independent income stream. Leaving the arrangement itself would mean emigrating or renouncing the payment, which few do.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, caregivers, beneficiary,
    powerless, biographical, constrained, national).

% Work in gig, seasonal, or short-contract jobs with irregular hours and thin margins. The floor arrives regardless of employment status, which lets them refuse the worst-paid or most abusive shifts and wait out gaps between contracts instead of accepting any available work. They cannot opt out of receiving the payment, and once earnings cross the tax threshold they contribute back through payroll and income taxes. Moving abroad would forfeit the payment.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, precarious_workers, beneficiary,
    moderate, biographical, constrained, national).

% Live in or leave households where a partner or family member controls money, housing, or documents; economic dependence is the mechanism that keeps them in dangerous households. The unconditional payment gives them an income no one else can withdraw, which is the material precondition for walking out. Shelters and relocation still cost more than the floor alone covers, so the exit path is real but narrow. As universal recipients they cannot be removed from the rolls by a partner's income changing.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, abuse_survivors, beneficiary,
    powerless, biographical, constrained, national).

% Produce art, research, or early-stage businesses whose returns are distant, uncertain, or non-monetary. The floor underwrites the years of unpaid or loss-making work that banks and clients will not finance, converting destitution risk into a survivable bet. They pay back into the tax base when their ventures succeed. They cannot opt out of the payment or the tax net; the alternative is abandoning the practice for wage work.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, artists_entrepreneurs, beneficiary,
    moderate, biographical, constrained, national).

% Fund the floor through income, payroll, and consumption taxes, and simultaneously receive it as universal recipients; net position ranges from large net contributors at the top of the income distribution to net recipients at the bottom. They contest the funding level and tax base through elections, budget hearings, and ballot measures, which is the arrangement's standing adjustment mechanism. Exit means renouncing tax residency, which is realistic mainly for mobile capital and high earners.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, general_taxpayers, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__freedom_floor_reading, general_taxpayers, beneficiary).

% Face a labor market in which workers can refuse sub-floor offers and quit without destitution, which compresses the wage-setting power that concentrated local labor markets otherwise confer. They contribute through business and payroll taxes and, on the demand side, gain from stabilized customer purchasing power and a healthier workforce; thin-margin, low-wage business models feel the discipline most. Relocating or automating is available to some and not to small local employers.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, employers, payer,
    institutional, generational, mobile, national).

% Sets the payment level, the tax base that funds it, and the statutory universality guarantee through ordinary legislation subject to elections. It administers the payment machinery through the tax authority and can raise, lower, means-test, or abolish the floor by majority vote; the binding constraint on it is electoral, not procedural. It also absorbs the fiscal risk of downturns, when outlays rise as tax receipts fall.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, legislature_funding_authority, agenda_setter,
    institutional, generational, mobile, national).

% Undocumented migrants, temporary visa holders, and recent arrivals who live and work in the economy — often paying consumption and payroll taxes — but fall outside the universality guarantee by legal status. They perform the same work as covered residents without the floor, which makes them the cheapest labor available and the population employers can still press hardest. They hold no seat in the funding debate; their remedy would be status regularization, which lies outside this arrangement's control.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, excluded_residents, excluded,
    powerless, biographical, trapped, national).

% Economists, social policy researchers, and statistical agencies who track the floor's effects on poverty rates, labor supply, bargaining outcomes, and public finances across pilots and national implementations. They publish the evidence the funding debate consumes and hold no stake in how the payment is distributed.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, welfare_state_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_commitment__freedom_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the income floor that private markets cannot: insurance against job loss and exploitation is unbuyable individually because the people who need it most are the ones no insurer will cover at an affordable rate, means-tested relief recreates poverty through benefit cliffs and stigma, and the exit capacity that disciplines employers and controlling households is a good no one can purchase alone. Universal, unconditional payment solves the funding problem once, through the tax base, and delivers the floor to everyone without conditionality's verification machinery.
% TRANSFER_FUNCTION: Moves purchasing power from the general tax base — weighted toward higher earners and capital — to every resident as an equal unconditional payment; as a second-order effect it moves bargaining power from employers to workers and from controlling household members to dependent ones, because the floor underwrites refusal.
% ABSENT_VOICES: Excluded residents — undocumented workers and temporary migrants — would object that universality as enacted draws the floor's boundary at legal status while their consumption and labor are taxed; they are outside the funding debate entirely. If financing leans on debt, future generations bear costs no current seat represents. Both absences are structural: the arrangement's own universality principle, applied consistently, would admit them.
% DISAPPEARANCE_RATIONALE: If the floor vanished overnight, caregivers would be forced into labor markets or dependency on partners, abuse survivors' exit paths close at the door, precarious workers lose refusal capacity and wage floors erode as employers regain monopsony power, artists and early-stage ventures lose their risk capital, and the demand floor underwriting small business revenue disappears — the labor market, household bargaining, and the care economy all reorganize around the return of destitution as the default disciplinarian.
% FOUNDING_PROBLEM: Industrial labor markets and family households expose people to destitution and domination through no fault of their own — unemployment, disability, caregiving obligations, abusive partners, monopsony employers — while means-tested relief, the standard remedy, recreates the problem it treats through benefit cliffs, stigma, surveillance, and take-up failure.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary seats: labor economists across the political spectrum document persistent employer monopsony power; public-health and domestic-violence research independently links economic independence to abuse-exit rates; fiscal institutions and cross-country statistical agencies attest the take-up failures and cliff effects of means-tested programs; retrenchment studies show the constituency entrenchment of universal programs. None of these attestations originates with the arrangement's beneficiary set.
narrative_ontology:disappearance_verdict(income_support_commitment__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__freedom_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_commitment__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__freedom_floor_reading, 0.15, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__freedom_floor_reading_tests).
:- end_tests(income_support_commitment__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the transfer IS the function: money moves from the tax base to every resident by design, the funding level is set in open budget politics, and universality denies any seat a targeted extraction surface. Suppression is low (0.10) and structural only: there is no conditionality apparatus — no work-search verification, no sanctions regime, no means-test surveillance — and the one coercive element, tax collection, is the general fiscal system rather than machinery specific to this arrangement; suppression is authored as the raw structural property and is not scaled by scope or directionality (the engine scales extractiveness only). Theater is low (0.10): the function is direct cash delivery, and performative activity is limited to annual reporting around funding debates. Accessibility collapse is moderate-low (0.35): alternatives persist alongside the floor — means-tested supplements, private insurance, charity, and employment all remain available — because the floor complements rather than replaces them. Resistance is moderate (0.45): funding level and tax base are perennially contested by net contributors and fiscal conservatives, but that contest runs through the arrangement's own adjustment mechanism rather than against a coercive apparatus. The claimed type (rope) is authored from this reading's structural belief — a genuine collective-action problem (income floors are individually unbuyable, adverse selection defeats private provision, and means-testing recreates poverty through cliffs and stigma) solved with minimal coercive overhead and no suppressed alternatives — independently of the metric values. The resource_allocation coordination type's default floor applies; no floor override is declared because tax-collection overhead is ordinary for a transfer system of this shape. The measurement series run on one shared grid (0, 4, 8, 12, 16, 20, 24) with both tracked metrics authored at every point; no suppression_requirement series is authored because the enforcement picture is static — tax collection via the general fiscal system, no conditionality machinery to ratchet — so the scalar in base_properties carries it.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently from the same structure. From the net-contributor taxpayer seat the floor is a standing tax burden whose return arrives only as the diffuse reciprocity of living in a covered polity; from the caregiver, survivor, and precarious-worker seats the same payment is the material precondition of any exit at all — the difference is not disagreement about facts but opposite positions in the transfer. The employer seat is internally split: low-wage, thin-margin firms experience the floor as labor-market discipline that compresses wage-setting power, while demand-dependent firms experience the same floor as stabilized customer purchasing power. The legislature experiences the arrangement as an electoral entanglement — it cannot adjust funding without confronting the universal constituency the design creates. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: caregivers, abuse survivors, precarious workers, and artists/entrepreneurs receive the payment without conditionality, placing their derived directionality near the beneficiary end. General taxpayers are declared payers with a secondary beneficiary position: they fund the floor and receive it back as universal recipients, with net position varying by income — the derivation from the payer role plus constrained exit alone would push the organized power atom toward the target end, so a directionality override sets organized agents to 0.55, the class-level net position of a progressively funded universal transfer. Employers bear the arrangement's disciplining cost (compressed wage-setting power) alongside tax contributions and demand-side gains; they are not declared victims because this reading locates no victim set — their seat is carried by their stakeholder situation rather than a per-atom override, since a single override on the institutional atom would misapply to the legislature, which administers the arrangement rather than bearing its costs. Scope is national: the floor's verification burden is low because universality removes eligibility verification entirely, and the modest scope amplification of effective extraction operates on an already-small base.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — destitution and domination exposure that markets and means-tested relief both fail to solve — remains live: monopsony power, economic abuse, unpaid care, and take-up failures are documented by sources outside the arrangement's beneficiary set, so no mandatrophy is declared and founding_problem_status is live. The classification guards in both directions. Against the extraction overclaim: the tax side is the arrangement's visible coordination cost, contested and adjustable through ordinary politics, not a covert transfer to a capturing seat — gain_flow is diffuse by design, and removal is prohibitive for constituency reasons (universal programs entrench their beneficiaries) rather than because an administrator profits from maintaining a dead function; the cost asymmetry that marks decayed arrangements — the administrator could change it but bears too little of it to bother — does not hold here because the function is live and the constituency is the point of the design. Against the pure-coordination overclaim: the low epsilon is contingent on unconditionality and universality holding; conditionality creep or status carve-outs would create targeted burdens and a new victim set, at which point this story decomposes into a different constraint rather than reclassifying (see the conditionality_creep_boundary and universality_boundary_status_carveout omegas). The classification thus prevents mislabeling the funding fight as suppression and prevents mislabeling the arrangement as unconditional where its universality is in fact bounded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is the freedom_floor_reading of the income_support_commitment kernel; what would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Author the sibling readings as separate constraint stories and compare classifications across the kernel''s reading set; the disagreement resolves, if at all, by comparative evidence on conditionality''s effects, not by adjudication inside this story.',
    'If the dependency_trap_reading''s mechanism dominates empirically, this arrangement gains a victim set (skill-atrophied long-term recipients), epsilon rises sharply, and the classification moves toward enforced extraction; if the targeting_efficiency_reading''s allocation principle prevails, universality is replaced by means-tested allocation, a surveilled claimant class appears as the new victim set, and suppression rises. This story''s low-epsilon classification holds only under its own reading''s premises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Reading-membership of the income_support_commitment kernel: the disagreement across readings is located in whether unconditionality and universality are constitutive of income support''s function or its defect.').

omega_variable(
    funding_incidence_distribution,
    'Does the tax package that funds the floor actually bear progressively in economic incidence, or do consumption-tax components, payroll caps, and rent inflation pass net cost to lower deciles?',
    'Statutory incidence analysis of the funding package combined with price, rent, and labor-market pass-through studies in covered regions.',
    'Progressive incidence supports the no-victim declaration and the low-epsilon classification; regressive incidence would mean the floor partially extracts from the population it pays, weakening the rope reading and pushing toward a hybrid coordination-extraction classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_incidence_distribution, empirical, 'Whether the funding side''s economic incidence matches its progressive statutory design.').

omega_variable(
    monopsony_exit_elasticity,
    'How much employer wage-setting power does the floor actually remove — does recipient refusal capacity translate into measurable wage, scheduling, and working-condition improvements, or do employer-side norms absorb it?',
    'Wage-panel and vacancy studies around floor introduction and payment-level variation; comparison of labor-market outcomes in high-coverage versus low-coverage local markets.',
    'Strong discipline effects confirm the reading''s employer-seat mechanism and widen the perspectival gap; null effects shift the reading''s weight onto the caregiver, survivor, and autonomy seats and leave the employer seat''s position closer to symmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monopsony_exit_elasticity, empirical, 'Magnitude of the exit-capacity channel through which the floor constrains employer wage-setting.').

omega_variable(
    conditionality_creep_boundary,
    'This arrangement''s identity depends on unconditionality holding; at what point do added requirements — work-search conditions, sanctions, residency tests — make the arrangement a different constraint rather than a degraded version of this one?',
    'Statutory tracking of conditionality additions, combined with the epsilon-invariance test: if measuring the arrangement with and without the added conditions yields different epsilon values, they are different constraints and the story must decompose.',
    'Conditionality additions would create a surveilled, sanctionable claimant class (a new victim set), raise suppression, and move the arrangement out of this story''s classification entirely; this story''s classification is valid only for the unconditional core.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditionality_creep_boundary, conceptual, 'The constraint-identity boundary at which conditionality creep produces a different constraint rather than drift within this one.').

omega_variable(
    universality_boundary_status_carveout,
    'The reading''s constitutive claim is that universality eliminates stigma and victim sets; the enacted floor excludes residents by legal status — is status-based exclusion a boundary of the arrangement''s membership (which the reading tolerates) or a defect that creates a victim class inside the economy?',
    'Comparative analysis of floors that do and do not carry status carve-outs: wage and working-condition outcomes for the excluded class, and the political trajectory of the carve-out over time.',
    'If the carve-out is constitutive (the floor defines civic membership), this story''s no-victim declaration holds for its population; if it is a defect, the excluded_residents seat is a victim set, epsilon rises for the arrangement as a whole, and the reading''s universality axiom is only partially instantiated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_boundary_status_carveout, conceptual, 'Whether legal-status exclusion from an otherwise universal floor preserves or violates the reading''s constitutive universality claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__freedom_floor_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__freedom_floor_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(inco_tr_t0, observed).
narrative_ontology:measurement(inco_tr_t4, income_support_commitment__freedom_floor_reading, theater_ratio, 4, 0.07).
narrative_ontology:measurement_basis(inco_tr_t4, observed).
narrative_ontology:measurement(inco_tr_t8, income_support_commitment__freedom_floor_reading, theater_ratio, 8, 0.08).
narrative_ontology:measurement_basis(inco_tr_t8, observed).
narrative_ontology:measurement(inco_tr_t12, income_support_commitment__freedom_floor_reading, theater_ratio, 12, 0.09).
narrative_ontology:measurement_basis(inco_tr_t12, observed).
narrative_ontology:measurement(inco_tr_t16, income_support_commitment__freedom_floor_reading, theater_ratio, 16, 0.09).
narrative_ontology:measurement_basis(inco_tr_t16, observed).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__freedom_floor_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(inco_tr_t20, observed).
narrative_ontology:measurement(inco_tr_t24, income_support_commitment__freedom_floor_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement_basis(inco_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__freedom_floor_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(inco_be_t0, observed).
narrative_ontology:measurement(inco_be_t4, income_support_commitment__freedom_floor_reading, base_extractiveness, 4, 0.11).
narrative_ontology:measurement_basis(inco_be_t4, observed).
narrative_ontology:measurement(inco_be_t8, income_support_commitment__freedom_floor_reading, base_extractiveness, 8, 0.12).
narrative_ontology:measurement_basis(inco_be_t8, observed).
narrative_ontology:measurement(inco_be_t12, income_support_commitment__freedom_floor_reading, base_extractiveness, 12, 0.12).
narrative_ontology:measurement_basis(inco_be_t12, observed).
narrative_ontology:measurement(inco_be_t16, income_support_commitment__freedom_floor_reading, base_extractiveness, 16, 0.13).
narrative_ontology:measurement_basis(inco_be_t16, observed).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__freedom_floor_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement_basis(inco_be_t20, observed).
narrative_ontology:measurement(inco_be_t24, income_support_commitment__freedom_floor_reading, base_extractiveness, 24, 0.15).
narrative_ontology:measurement_basis(inco_be_t24, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(income_support_commitment__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'income support' decomposes into three structurally distinct constraints, one per reading of the income_support_commitment kernel. This story authors the freedom-floor reading: universality and unconditionality as constitutive, no victim set, low epsilon. The dependency_trap_reading authors the same payment as a dependence-producing arrangement (new victim set: skill-atrophied long-term recipients; high epsilon). The targeting_efficiency_reading authors need-concentrated allocation (universality replaced by means tests; a surveilled claimant class bears the verification and cliff costs). The readings share the kernel but diverge on one structural element — whether unconditionality is the function or the defect — so each gets its own epsilon, stakeholders, and classification, linked here as a constraint family. This reading is upstream of the siblings in one sense only: its enactment generates the exit-capacity and labor-supply evidence by which the sibling readings' empirical claims are tested.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_commitment__freedom_floor_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
