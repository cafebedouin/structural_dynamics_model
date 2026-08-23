% ============================================================================
% CONSTRAINT STORY: substance_control_authority__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__legalization_reading, []).

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
 *   constraint_id: substance_control_authority__legalization_reading
 *   human_readable: State Authority to Regulate Drug Markets as Legal Commerce (Legalization Reading)
 *   domain: public health policy/criminal justice/political economy
 *
 * SUMMARY:
 *   The legalization reading converts state drug authority from criminal
 *   sanction into market regulation: licensed production, tested product,
 *   age-gated retail, taxed transactions. Its promise is a triple liberation
 *   — users exit the criminal-victim set, third parties are protected by
 *   product standards instead of police, and illicit markets wither as legal
 *   commerce outcompetes them. The operating record shows the first two
 *   promises substantially kept and the third substantially broken: excise
 *   stacks of 25-40 percent plus scarce, expensive licenses keep unlicensed
 *   supply price-competitive, so the regime retains a standing enforcement
 *   target (unlicensed suppliers) and a standing victim population
 *   (neighborhoods where open-air markets persist) inside its own borders.
 *   KEY AGENTS (by structural relationship): - state_regulatory_agency:
 *   agenda setter (institutional/arbitrage) — administers the license wall,
 *   sets fee structures, allocates enforcement between channels -
 *   public_treasury: primary fiscal beneficiary (institutional/constrained) —
 *   receives the excise stack, budgeted against continuation -
 *   licensed_drug_producers and licensed_dispensary_owners: protected
 *   incumbents (organized/constrained) — collect scarcity rents behind
 *   limited entry - adult_users: net beneficiaries with dual-channel mobility
 *   (moderate/mobile) — safety gained, price premium avoidable via the
 *   surviving illicit channel - dependent_heavy_users: heaviest price-burden
 *   bearers (powerless/trapped) — inelastic demand absorbs the full tax load
 *   - unlicensed_suppliers: enforcement residue (organized/trapped) —
 *   criminalized by the same statutes the reform was supposed to retire -
 *   communities_with_persistent_illicit_markets: diffuse cost bearers
 *   (powerless/constrained) — carry the disorder the reform promised to end -
 *   public_health_agencies: observer seat (institutional/analytical) —
 *   measure both the safety wins and the shortfall -
 *   international_narcotics_control_bodies: excluded voice
 *   (institutional/constrained, global scope) — object from outside the
 *   process. CONSTRAINT FAMILY NOTE (epsilon-invariance decomposition): the
 *   colloquial label 'drug policy' conflates three structurally distinct
 *   arrangements instantiated by the three readings of this kernel. The
 *   prohibition_reading authors epsilon for the criminalization arrangement
 *   as it stands (near-total extraction from users, enforced by police). The
 *   harm_reduction_reading authors epsilon for the tolerated-use/intervention
 *   arrangement. THIS file authors epsilon for the regulated-commerce
 *   arrangement as it operates: moderate-high, driven by tax incidence and
 *   licensure rents rather than by criminal sanction. Each story has one
 *   stable epsilon, its own victim sets, and its own type; the files are
 *   linked through network.affects_constraints and must never be averaged.
 *
 * KEY AGENTS:
 *   - state_regulatory_agency: agenda setter (institutional/arbitrage) — writes licensing rules, sets fees, directs channel enforcement
 *   - public_treasury: primary fiscal beneficiary (institutional/constrained) — collects the excise stack, budgets against its continuation
 *   - licensed_drug_producers: protected incumbent (organized/constrained) — scarcity-rent collector behind limited entry
 *   - licensed_dispensary_owners: protected incumbent with cost exposure (organized/constrained) — retail-margin collector, fee payer, renewal-dependent
 *   - adult_users: net beneficiary with dual-channel mobility (moderate/mobile) — buys safety, dodges price via the surviving illicit channel
 *   - dependent_heavy_users: primary price-burden bearer (powerless/trapped) — inelastic demand absorbs tax and margin load
 *   - unlicensed_suppliers: enforcement residue (organized/trapped) — prosecuted under retained statutes for serving the price-sensitive majority of volume
 *   - communities_with_persistent_illicit_markets: diffuse cost bearer (powerless/constrained) — hosts the disorder the reform promised to eliminate
 *   - public_health_agencies: analytical observer (institutional/analytical) — documents both safety gains and illicit-market persistence
 *   - international_narcotics_control_bodies: excluded voice (institutional/constrained) — treaty-based objection with no seat in the domestic process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__legalization_reading, 0.52).
domain_priors:suppression_score(substance_control_authority__legalization_reading, 0.5).
domain_priors:theater_ratio(substance_control_authority__legalization_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "State Authority to Regulate Drug Markets as Legal Commerce (Legalization Reading)").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public health policy/criminal justice/political economy").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, '02646497-0cb1-4962-8185-9a6cc32ca754').
narrative_ontology:cs_kernel_codification('02646497-0cb1-4962-8185-9a6cc32ca754', formalized).
narrative_ontology:cs_authority_grounding('02646497-0cb1-4962-8185-9a6cc32ca754', expertise).
narrative_ontology:cs_interpretation_layer_present('02646497-0cb1-4962-8185-9a6cc32ca754').
narrative_ontology:cs_reading_relation('02646497-0cb1-4962-8185-9a6cc32ca754', substance_control_authority__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('02646497-0cb1-4962-8185-9a6cc32ca754', substance_control_authority__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('02646497-0cb1-4962-8185-9a6cc32ca754', foundational, regulated_commerce_is_optimal_control).
narrative_ontology:cs_axiom_status(regulated_commerce_is_optimal_control, holdable).
narrative_ontology:cs_axiom_grounding('02646497-0cb1-4962-8185-9a6cc32ca754', regulated_commerce_is_optimal_control, instrumental).
narrative_ontology:cs_axiom('02646497-0cb1-4962-8185-9a6cc32ca754', foundational, adult_consumption_autonomy).
narrative_ontology:cs_axiom_status(adult_consumption_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('02646497-0cb1-4962-8185-9a6cc32ca754', adult_consumption_autonomy, deontological).
narrative_ontology:cs_reference_frame('02646497-0cb1-4962-8185-9a6cc32ca754', regulated_commerce_framework).
narrative_ontology:cs_drift_state('02646497-0cb1-4962-8185-9a6cc32ca754', post_legalization_implementation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('02646497-0cb1-4962-8185-9a6cc32ca754', '').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, licensed_drug_producers).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, licensed_dispensary_owners).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, public_treasury).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, dependent_heavy_users).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, unlicensed_suppliers).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, communities_with_persistent_illicit_markets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, adult_users).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, licensed_dispensary_owners).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, adult_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and administers the licensing rules, sets application fees and renewal conditions, runs product-testing mandates, and directs enforcement priority between the licensed channel and the unlicensed one. Can restructure categories, adjust tax pass-through recommendations, and redefine which activities fall inside the legal market. Its budget and staffing grow with the scope of the regime it administers.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_regulatory_agency, agenda_setter,
    institutional, generational, arbitrage, national).

% Receives excise and sales taxes levied on every legal transaction, plus licensing and application fees. The tax line has become a budgeted revenue stream that ministries plan around; reducing rates to compete with unlicensed supply would mean surrendering booked revenue. Part of the take is statutorily earmarked for treatment and prevention programs, creating downstream dependents on continued high prices.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_treasury, beneficiary,
    institutional, generational, constrained, national).

% Hold cultivation and manufacturing licenses obtained through capital-intensive applications. Limited license issuance restricts the number of competitors, so incumbents sell into a structurally short-supplied market at margins well above unregulated production cost. Their facilities, compliance staff, and track-record requirements are sunk assets that lose most value if the licensing framework is reopened to general entry. They lobby for stricter enforcement against unlicensed growers.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, licensed_drug_producers, beneficiary,
    organized, generational, constrained, national).

% Operate storefronts under retail licenses with zoning caps that limit how many can exist in each municipality. Collect the retail margin on legal sales and benefit from customers who prefer guaranteed product testing, but pay licensing fees, compliance overhead, and the same excise stack their customers ultimately bear. License renewals tie their livelihood to continuous regulatory good standing.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, licensed_dispensary_owners, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__legalization_reading, licensed_dispensary_owners, payer).

% Purchase tested, labeled products without criminal exposure, which is what the reform promised them. They pay the regulated retail price, which carries the full tax stack and licensed-channel margins, but retain a live alternative: the unlicensed market still sells comparable product cheaper. Their purchasing splits between channels based on price and convenience, and that split is visible in every jurisdiction's own sales data.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, adult_users, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__legalization_reading, adult_users, payer).

% Consume at levels where dependence constrains their ability to reduce quantity purchased. Unlike casual buyers, they cannot substitute down or shift much volume to cheaper unlicensed sources without disruption, so they absorb the tax and margin load nearly inelastically. For opioid-adjacent regimes they additionally face access controls, purchase caps, and pharmacy-level monitoring that treat their consumption pattern as an administrative risk category.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, dependent_heavy_users, payer,
    powerless, immediate, trapped, national).

% Continue operating the supply networks that existed before legalization because the licensed channel's prices and entry barriers left most of the customer base reachable only through them. They face raids, asset seizure, and prosecution under the same statutes that were supposedly retired, now applied to the residue outside the license wall. Entry into the licensed channel requires capital, documentation, and tax compliance capacity that many of them, especially small legacy growers, do not have.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, unlicensed_suppliers, payer,
    organized, biographical, trapped, regional).

% Live in neighborhoods where the promised elimination of street markets did not occur because licensed outlets cluster in commercially attractive zones while enforcement pressure concentrates where unlicensed trade remains. They carry the ongoing costs of visible open-air dealing, policing operations, and the displacement dynamics that follow each enforcement sweep, without holding licenses, revenue shares, or a formal seat in the regime's design.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, communities_with_persistent_illicit_markets, payer,
    powerless, generational, constrained, local).

% Track poisoning presentations, product-contamination incidents, use prevalence, and youth initiation across the transition from criminalized to regulated supply. Publish evaluations that both support the reform's safety gains and document its shortfalls, particularly the unlicensed market's persistence. Their findings feed legislative reviews but they hold no vote over tax rates or license counts.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_health_agencies, observer,
    institutional, generational, analytical, national).

% Administer the treaty framework that classified these substances as strictly controlled and regard commercial legalization as treaty violation. Domestic reform proceeds without them: they file objections, issue critical annual reports, and press for reversal, but no domestic licensing board seats them, and the treaty amendment process that would accommodate their position is effectively frozen. Their objection is documented but structurally outside the conversation.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, international_narcotics_control_bodies, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__legalization_reading, public_treasury).
narrative_ontology:fixing_cost_class(substance_control_authority__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the adulterated-supply and anonymous-access problems of prohibited markets by concentrating quality assurance, dose labeling, age verification, and transaction traceability in a licensed channel, replacing street intermediation with inspectable commerce.
% TRANSFER_FUNCTION: Moves consumer spending into public coffers via excise and sales taxes and into license holders' margins via restricted entry; moves enforcement attention away from possessors and onto unlicensed suppliers; moves a statutorily earmarked slice of revenue to treatment and prevention programs.
% ABSENT_VOICES: International narcotics-control bodies object from outside the domestic process with no seat in it. Unlicensed suppliers — the people actually targeted by the regime's continuing enforcement — have no formal voice; their customers who quietly defect to cheaper unlicensed product express the objection only as revealed preference. Residents of zones where open-air markets persist bear the regime's residual disorder without holding licenses, revenue, or standing. Prospective small producers priced out by application costs are absent from the license-allocation table that decides their exclusion.
% DISAPPEARANCE_RATIONALE: If the licensing-and-tax apparatus vanished overnight, the licensed industry would dissolve, booked tax revenue and earmarked treatment funding would disappear from budgets, demand would snap back to the pre-existing illicit networks within weeks, and police priorities would revert to prosecuting possession — the entire post-reform commercial, fiscal, and enforcement landscape depends on the arrangement's continuance.
% FOUNDING_PROBLEM: Prohibition's recorded failures: poisoned and mislabeled supply killing users, violent illicit distribution networks, and mass criminalization of possessors — to be solved while keeping state control over who accesses what, at what strength, at what age.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies and peer-reviewed epidemiological studies outside the benefiting parties corroborate the safety half: contamination deaths in the licensed channel fell sharply. Government audit offices and independent criminologists, likewise outside the beneficiary set, attest that the second half — eliminating illicit markets — is unmet, with unlicensed share remaining at a quarter to nearly half of volume in mature jurisdictions. Treasury's claim that the founding problem is solved is contradicted by its own revenue dependence on prices that sustain the illicit channel; no party outside the beneficiary set attests full resolution.
narrative_ontology:disappearance_verdict(substance_control_authority__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_authority__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__legalization_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.52 at interval end) because the regime's price architecture — excise plus licensing scarcity — is decoupled from the marginal cost of quality assurance, and because the tax take is budgeted revenue that the fiscal seat now defends. Suppression is 0.50 and structural: enforcement machinery did not demobilize after reform, it re-targeted from possessors to unlicensed suppliers, and age-gate, potency-cap, and advertising restrictions remain actively policed. Theater is 0.32: product testing and chain-of-custody tracking perform real functions, but a growing share of compliance activity (multi-year license renewals, packaging micro-rules, duplicative reporting) functions as barrier maintenance rather than safety output. Accessibility_collapse is LOW (0.35) — the diagnostic signature of this arrangement is precisely that the alternative did NOT collapse: the unlicensed market persists as a functioning escape hatch, which is why resistance sits at a real 0.45 (supplier evasion, price-defection purchasing, small-producer exclusion suits) rather than collapsing toward zero. The temporal series show a monotone ratchet, not a cycle: taxes step up, license fees compound, compliance paperwork accretes, and enforcement against the residue intensifies as the promised market elimination fails to arrive. All three tracked metrics are authored on one shared grid ({0,2,4,6,8,10,12}) so no row borrows an end-state value for an earlier time. Suppression here is a raw structural property of the arrangement; it enters the engine unscaled — only extractiveness is scaled by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergent arrangements from identical statutes. From the treasury and regulatory-agency seats this is a functioning public-health administration doing what administrations do: licensing, taxing, inspecting. From the licensed-incumbent seats it is earned protection — capital converted into a regulatory moat. From the dependent-user seat it is a regressive consumption tax levied on compulsion. From the unlicensed supplier's seat it is the old war continued under new letterhead, with the same statutes and the same raids aimed at whoever stayed outside the license wall. Same text, four arrangements; the engine computes each seat's type from the structural data, and the divergence between those computations is the finding, not a defect to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: licensed producers, dispensary owners, and the treasury sit near the subsidized end (low effective extraction; the treasury's d is damped further by its agenda-setting power). Dependent heavy users and unlicensed suppliers sit near the full-target end — both are declared victims, and both carry exit profiles (trapped) that amplify effective extraction: the dependent user cannot elasticize away from the tax, and the supplier has no licit path into the regime that displaced him. Communities with persistent markets bear diffuse costs with no offsetting receipts. Adult users are the interesting middle case: declared net beneficiaries (safety, legality) but bearing the excise load — their mobile dual-channel exit damps their effective extraction well below the dependent users', and is the reason the regime's price burden falls regressively on its least mobile payers. Scope is national for most seats, which scales verification difficulty modestly upward in the engine's computation; the excluded international body sits at global scope outside the arrangement entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — prohibition's poisoned supply, violent markets, and mass criminalization — is half-solved and half-perpetuated: contamination deaths fell in the licensed channel while the regime's own price design sustains the illicit channel it was built to replace. Founding_problem_status is therefore 'contested', and paired with disappearance_verdict 'world_rearranges' this avoids the dead-mandate-plus-dependence mismatch that flags a zombie. Classifying this as tangled_rope does double duty against the two available mislabels: the advocate's label (pure coordination rope) hides the standing victims — dependent users taxed inelastically, suppliers re-criminalized, neighborhoods still hosting the markets — while the abolitionist critic's label (pure snare) erases the real coordination delivered: tested supply, honest labeling, age gates, and the fiscal recycling of revenue into treatment. The hybrid classification keeps both truths legible and makes the decisive question empirical rather than rhetorical: does the illicit residue shrink when rates drop and entry opens? That question is carried by the illicit_market_residue omega rather than settled by the claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading — legalization_reading — of the substance_control_authority kernel. What structurally changes if a sibling reading is instantiated instead?',
    'Compare the compiled victim sets and enforcement surfaces of the sibling files: the prohibition_reading returns users to the criminal-victim set with enforcement as the primary mechanism; the harm_reduction_reading keeps supply largely illicit while accepting use and intervening clinically. The disagreement is located in the legitimate MODE of state authority (sanction vs. commerce regulation vs. clinical management) and therefore in where the victim-set boundary falls.',
    'Classification is reading-indexed: the same kernel yields different epsilon and different types per reading. Cross-reading comparisons must join on the kernel, never average across readings — averaging would fabricate a constraint none of the parties hold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: this story instantiates one reading of a contested kernel; sibling readings are separate constraint files.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the contested kernel ''state authority over substance markets'' (framing A, yielding a commerce-regulation constraint) or ''state authority over bodily conduct'' (framing B, yielding a paternalism-over-persons constraint)? The obvious framing is the market; the less obvious is the person.',
    'Test which framing the operative statutes and adjudicated cases actually track: if enforcement and litigation turn on product standards, licensing, and tax incidence, framing A governs; if they turn on possession, consumption location, and personal-conduct offenses retained inside the regime, framing B does.',
    'Under framing B the constraint''s coordination function thins (it coordinates markets only incidentally) and the measured extraction lands more heavily on persons than transactions, shifting the computed pattern toward the extractive end; under framing A the current tangled-coordination reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'CS-framing under-determination: two coherent framings of the same authority produce different classifications.').

omega_variable(
    illicit_market_residue,
    'Does the regime''s own design (excise stack plus limited licensing) permanently sustain a dual market, making ''illegal markets eliminated'' — the reading''s core success criterion — unreachable without rate and entry reform?',
    'Natural experiments from jurisdictions that cut rates or expanded licenses: if unlicensed share collapses toward zero as price gaps close, the residue is a design parameter; if it persists at any price point, an entrenched parallel economy exists that regulation alone cannot absorb.',
    'If the residue is structural, a large fraction of the measured suppression is permanently aimed at people the reform promised to liberate, and the arrangement hardens toward the extractive end of the spectrum with the excluded as its standing victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(illicit_market_residue, empirical, 'Whether the dual market is transitional or a designed-in permanent feature.').

omega_variable(
    use_volume_elasticity,
    'How much does legal availability raise total and youth use volume, and does that increase offset the safety gains from tested supply?',
    'Longitudinal cohort studies comparing prevalence and initiation curves before and after market opening, with adjacent non-legalizing jurisdictions as controls.',
    'If volume effects dominate, the third-party-protection claim weakens and the regime''s coordination function narrows to quality control alone; if volume is roughly flat, the safety gains stand as net coordination benefit supporting the coordination half of the hybrid classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(use_volume_elasticity, empirical, 'Whether normalization-driven volume growth negates the regime''s health gains.').

omega_variable(
    license_concentration_capture,
    'Are limited licenses converging into a captured oligopoly that writes the rules that protect it, converting the coordination layer into a private rent machine?',
    'Track license-holder concentration ratios, incumbent lobbying expenditures relative to new entrants'', and the correlation between campaign contributions and license-cap legislation over successive renewal cycles.',
    'Confirmed capture would mean the extraction component is privately received rather than fiscally recycled, strengthening the case that the arrangement persists for its beneficiaries'' sake rather than its coordination function — and that the agenda-setting seat has been absorbed into the beneficiary coalition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(license_concentration_capture, empirical, 'Whether licensing scarcity is drifting from public stewardship into private cartel maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__legalization_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__legalization_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(subs_tr_t2, substance_control_authority__legalization_reading, theater_ratio, 2, 0.21).
narrative_ontology:measurement(subs_tr_t4, substance_control_authority__legalization_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(subs_tr_t6, substance_control_authority__legalization_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(subs_tr_t8, substance_control_authority__legalization_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(subs_tr_t10, substance_control_authority__legalization_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(subs_tr_t12, substance_control_authority__legalization_reading, theater_ratio, 12, 0.32).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__legalization_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(subs_be_t2, substance_control_authority__legalization_reading, base_extractiveness, 2, 0.4).
narrative_ontology:measurement(subs_be_t4, substance_control_authority__legalization_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(subs_be_t6, substance_control_authority__legalization_reading, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(subs_be_t8, substance_control_authority__legalization_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(subs_be_t10, substance_control_authority__legalization_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(subs_be_t12, substance_control_authority__legalization_reading, base_extractiveness, 12, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__legalization_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(subs_su_t2, substance_control_authority__legalization_reading, suppression_requirement, 2, 0.37).
narrative_ontology:measurement(subs_su_t4, substance_control_authority__legalization_reading, suppression_requirement, 4, 0.41).
narrative_ontology:measurement(subs_su_t6, substance_control_authority__legalization_reading, suppression_requirement, 6, 0.44).
narrative_ontology:measurement(subs_su_t8, substance_control_authority__legalization_reading, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(subs_su_t10, substance_control_authority__legalization_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement(subs_su_t12, substance_control_authority__legalization_reading, suppression_requirement, 12, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the natural-language concept 'drug policy' / 'state drug authority' covers three structurally distinct arrangements that cannot share one story because their epsilon values, victim sets, and enforcement surfaces differ irreconcilably. This file (legalization_reading) authors the regulated-commerce arrangement; substance_control_authority__prohibition_reading authors the criminalization arrangement (epsilon maximal for users, police as mechanism); substance_control_authority__harm_reduction_reading authors the tolerated-use/intervention arrangement (supply stays illicit, clinical apparatus as mechanism). The prohibition story is upstream historically (its failure is the founding problem this reading cites as warrant); the harm-reduction story is a lateral sibling competing for the same policy space. All three files link each other through network.affects_constraints; cross-reading analysis joins on kernel_id and never averages epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
