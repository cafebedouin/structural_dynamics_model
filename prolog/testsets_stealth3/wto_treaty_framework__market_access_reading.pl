% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_treaty_framework__market_access_reading, []).

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
 *   constraint_id: wto_treaty_framework__market_access_reading
 *   human_readable: WTO Treaty Framework — Market-Access Reading (Symmetric Universal Liberalization Obligation)
 *   domain: international_trade_law/development_economics/political_economy
 *
 * SUMMARY:
 *   This story instantiates the WTO covered agreements as the market-access
 *   reading holds them: liberalization as a symmetric universal obligation,
 *   non-discrimination and market access as the treaty's primary purpose, and
 *   special-and-differential treatment as temporary transitional exception.
 *   Under this reading the standing arrangement binds tariff ceilings, caps
 *   subsidy allowances, and prohibits local-content and performance
 *   requirements for all members alike, while incumbent economies operate
 *   large support programs within negotiated ceilings and their firms hold
 *   established scale advantages. The claim/metric independence is deliberate
 *   and load-bearing here: the reading CLAIMS symmetric fair exchange — that
 *   is its constitutive premise — while the authored METRICS describe
 *   substantially extractive operation, because the incidence of the binding
 *   obligations falls on exactly the instruments late industrializers need
 *   and spares the instruments incumbents already use. That divergence
 *   between the reading's declared symmetry and the arrangement's measured
 *   incidence is the datum this story exists to record. KEY AGENTS (by
 *   structural relationship): - major_trading_powers: Agenda-setter
 *   (institutional/arbitrage) — draft texts, convene mini-ministerials, drive
 *   consensus - multinational_corporations: Primary beneficiary
 *   (powerful/arbitrage) — market access widens their operating territory -
 *   developed_country_agri_exporters: Secondary beneficiary
 *   (organized/mobile) — access for surplus, support retained -
 *   developing_country_governments: Net target with partial shield
 *   (moderate/constrained) — policy space ceded, courtroom gained -
 *   developing_country_infant_industries: Primary target (powerless/trapped)
 *   — sheltering instruments bound or banned -
 *   least_developed_country_producers: Deepest target (powerless/trapped) —
 *   paper preferences, limited uptake - import_market_consumers: Incidental
 *   beneficiary (powerless/mobile) — lower prices, no organized seat -
 *   global_south_civil_society: Excluded voice (organized/constrained) —
 *   objects from outside the negotiating rooms - trade_law_analysts:
 *   Analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, 0.72).
domain_priors:suppression_score(wto_treaty_framework__market_access_reading, 0.62).
domain_priors:theater_ratio(wto_treaty_framework__market_access_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__market_access_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__market_access_reading, "WTO Treaty Framework — Market-Access Reading (Symmetric Universal Liberalization Obligation)").
narrative_ontology:topic_domain(wto_treaty_framework__market_access_reading, "international_trade_law/development_economics/political_economy").

domain_priors:requires_active_enforcement(wto_treaty_framework__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__market_access_reading, '408c0f22-352d-4d30-b6e7-0f2339fb344f').
narrative_ontology:cs_kernel_codification('408c0f22-352d-4d30-b6e7-0f2339fb344f', formalized).
narrative_ontology:cs_authority_grounding('408c0f22-352d-4d30-b6e7-0f2339fb344f', lineage).
narrative_ontology:cs_interpretation_layer_present('408c0f22-352d-4d30-b6e7-0f2339fb344f').
narrative_ontology:cs_reading_relation('408c0f22-352d-4d30-b6e7-0f2339fb344f', wto_treaty_framework__developmental_reading, forecloses).
narrative_ontology:cs_axiom('408c0f22-352d-4d30-b6e7-0f2339fb344f', foundational, universal_symmetric_liberalization_obligation).
narrative_ontology:cs_axiom_status(universal_symmetric_liberalization_obligation, holdable).
narrative_ontology:cs_axiom_grounding('408c0f22-352d-4d30-b6e7-0f2339fb344f', universal_symmetric_liberalization_obligation, deontological).
narrative_ontology:cs_axiom('408c0f22-352d-4d30-b6e7-0f2339fb344f', secondary, differential_treatment_transitional_only).
narrative_ontology:cs_axiom_status(differential_treatment_transitional_only, holdable).
narrative_ontology:cs_axiom_grounding('408c0f22-352d-4d30-b6e7-0f2339fb344f', differential_treatment_transitional_only, instrumental).
narrative_ontology:cs_reference_frame('408c0f22-352d-4d30-b6e7-0f2339fb344f', symmetric_reciprocal_obligation_baseline).
narrative_ontology:cs_drift_state('408c0f22-352d-4d30-b6e7-0f2339fb344f', post_appellate_body_paralysis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('408c0f22-352d-4d30-b6e7-0f2339fb344f', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__market_access_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, developed_country_agri_exporters).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, import_market_consumers).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, developing_country_infant_industries).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, least_developed_country_producers).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, developing_country_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, developing_country_governments).
narrative_ontology:constraint_vindicates(wto_treaty_framework__market_access_reading, comparative_advantage_specialization_doctrine).
narrative_ontology:constraint_vindicates(wto_treaty_framework__market_access_reading, reciprocal_binding_credibility_equilibrium).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft the negotiating texts, convene mini-ministerial and restricted sessions, and drive consensus across more than 160 members. Their markets are the destinations others need, which sets the pace and scope of every liberalization package. Their firms and farms already hold global scale, so further opening costs them little and gains much. Bilateral and regional deals and unilateral trade measures stand ready as outside options that keep their bargaining position strong.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, major_trading_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% Run integrated production and distribution networks across many jurisdictions. Each new market-access commitment widens the territory in which they can produce, sell, and move goods without border friction. They fund advocacy and route grievances through home-government complaint channels when barriers appear. Shifting production between countries is routine business planning, which insulates them from any single jurisdiction's rules.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, multinational_corporations, beneficiary,
    powerful, biographical, arbitrage, global).

% Large commercial farming and food-processing sectors moving grain, meat, and processed products into foreign markets. Access rounds open destinations for their surpluses while domestic support programs remain available within negotiated ceilings. They mobilize politically at home whenever access concessions stall.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, developed_country_agri_exporters, beneficiary,
    organized, biographical, mobile, global).

% Households in importing countries buying clothing, electronics, and food more cheaply than closed-market alternatives would allow. Their stake is real but diffuse; they do not organize around tariff schedules specifically, and their interest reaches negotiations only indirectly through retailers and importers.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, import_market_consumers, beneficiary,
    powerless, biographical, mobile, national).

% Governments of still-industrializing members. They acceded for predicted export growth and investment inflows; in exchange their tariff ceilings, subsidy allowances, and performance-based industrial policies became bound or prohibited. Dispute settlement hands them a courtroom where procedural equality sometimes beats market weight — small members have brought and won cases. Their reported net ledger is nonetheless negative: the instruments every earlier industrializer used are no longer lawful for them, and walking out would forfeit both the courtroom and preferential access.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, developing_country_governments, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__market_access_reading, developing_country_governments, beneficiary).

% Manufacturing sectors in early-industrialization phases — assembly, basic metals, machinery — that need sheltered time to reach viable scale. Tariff bindings cap the duties that protect them; subsidy disciplines and local-content bans remove tools their competitors' predecessors used freely. Their fate is decided in negotiating rooms they attend as junior parties, and their assets cannot relocate to escape the rules.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, developing_country_infant_industries, payer,
    powerless, generational, trapped, national).

% Subsistence and smallholder producers meeting imported competition and commodity-price swings with minimal buffers. Duty-free schemes exist on paper, but rules of origin and product coverage limit uptake. They maintain no dedicated negotiators for their product lines and possess no litigation capacity whatsoever.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, least_developed_country_producers, payer,
    powerless, immediate, trapped, regional).

% Development organizations, farmer federations, and labor movements from the Global South that mobilized around the Seattle and Cancun ministerials. They hold positions on food security, equity, and policy space but occupy no seat in negotiating sessions; their influence travels through street protest and allied delegations rather than drafting rights.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, global_south_civil_society, excluded,
    organized, biographical, constrained, continental).

% Academic and institute specialists in trade law and development economics. They document notification and compliance patterns, model welfare and distributional effects, and publish the asymmetry analyses that disputing coalitions cite against each other. They collect nothing from the arrangement and decide nothing in it.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, trade_law_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_treaty_framework__market_access_reading, multinational_corporations).
narrative_ontology:fixing_cost_class(wto_treaty_framework__market_access_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts a sprawl of discriminatory bilateral trade deals into a single non-discriminatory standard: most-favored-nation treatment eliminates the transaction costs of managing hundreds of preferential margins; scheduled bindings make opening commitments credible and hard to reverse opportunistically; legalized dispute settlement substitutes adjudicated procedure for retaliatory power politics.
% TRANSFER_FUNCTION: Moves binding commitments on trade-policy instruments — tariff ceilings, subsidy allowances, local-content prohibitions — from every member into the common pool. The realized gains flow asymmetrically toward whoever holds existing scale and comparative advantage: exporting multinationals and developed agribusiness collect the widened market access, financed by the foreclosed industrial-policy options of late-industrializing members.
% ABSENT_VOICES: Subsistence farmers and informal-sector workers in adjusting regions, future industrial entrants too unorganized to lobby, import-displaced workers in the North, and ecological-equity critics hold no seat at the table. They surface episodically as street protest at ministerials or through sympathetic delegations, then leave the room again when the sessions resume behind closed doors.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would return trade governance to bilateral power bargaining: small states would lose the courtroom that lets them win against large markets, preferential-margin management costs would return, and tariff spirals would become a live risk rather than a historical memory. Supply chains would reorganize around rival regional blocs within years, and the smallest traders would face terms dictated by whichever great power their geography assigned them.
% FOUNDING_PROBLEM: The interwar spiral of protection, discriminatory imperial-preference blocs, and competitive devaluation that deepened the Depression and is widely credited with feeding the road to war. The postwar designers built reciprocal, non-discriminatory liberalization under common rules to make that relapse impossible.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians of the interwar collapse corroborate that the founding problem was real and severe — the nearest thing to a disinterested witness, since the 1930s record predates every current beneficiary. Whether the problem remains live is disputed along the reading lines: the major powers and mainstream trade economists attest the discipline remains necessary against protectionist relapse, while developing-country coalitions and heterodox development economists attest the arrangement now primarily locks in incumbent advantage and that the problem's residual forms are managed by other institutions. Beyond the historical record, no attester fully outside the benefiting parties exists, and that absence is itself signal.
narrative_ontology:disappearance_verdict(wto_treaty_framework__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__market_access_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__market_access_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_treaty_framework__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_treaty_framework__market_access_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_treaty_framework__market_access_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_treaty_framework__market_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.72: the obligations bind precisely the catch-up toolkit — tariff ceilings, subsidy caps, prohibition of performance requirements — while incumbent support instruments persist within negotiated ceilings; extraction accumulates as phase-out deadlines bite and jurisprudence narrows flexibilities. Suppression 0.62 is a raw structural property, deliberately UNSCALED: it reflects retaliation exposure for small economies, preference dependence, and the adjacency of financing conditionality, and it is not amplified by power or scope in the way extractiveness is. Theater ratio 0.36: the binding, scheduling, and dispute-settlement core remains functional, but a growing share of activity — S&D review cycles, ministerial declarations, micro-deliverables celebrated as round completions — is declaratory rather than operative, hence the steady rise from 0.16. Accessibility_collapse 0.50: autarky is demonstrably costly and regional agreements substitute only partially, but workable outside arrangements exist. Resistance 0.58: the Doha deadlock, developing-country coalition blockades, and the appellate-appointment blockade are sustained, organized refusal. The three series run on ONE shared time grid (t = 0,5,10,15,20,25,30) with every tracked metric authored at every point. The suppression series rises to a peak at t=20 and then declines: the appellate-body paralysis beginning late in the interval eroded multilateral enforcement capacity faster than unilateral substitutes replaced it — an enforcement-decay tail, not stabilization, which is why the endpoint scalar sits below the peak.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply and the engine computes that divergence from the structural data rather than from the claim. From the agenda-setter seat the arrangement is an equilibrium its architects built and still referee; from the multinational seat it is friction removal; from the developing-country government seat the same legal order operates as a cage with a courtroom attached — binding constraints on industrial instruments, offset by dispute-settlement access that lets small members win cases they could never win in power politics; from the least-developed producer seat it is prices and rules arriving from outside with no handle to grasp. Same-nominal-level actors differ too: developing-country governments and the major trading powers are both sovereign treaty parties, but agenda control, market gravity, and arbitrage-capable firm networks give the latter exits and leverage the former lack — the G-20 episode showed coalition formation briefly converting diffuse Southern positions into blocking power, and its subsequent deflation showed how costly maintaining that power is.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low-directionality seats: multinationals (arbitrage exit, near-zero d), developed agri-exporters (mobile, low d), import-market consumers (incidental gains, diffuse losses, low d). Victim declarations map to high-directionality seats: infant industries (trapped inside the protected niches the bindings cap, high d), least-developed producers (no litigation capacity, no negotiators, highest d), developing-country governments (net target side). One override is authored: developing_country_governments carry a dual role (payer primary, beneficiary secondary through the courtroom), and the automatic derivation from dual-role-plus-constrained-exit would center them near symmetric; the story's ledger — bound tariffs, banned performance requirements, capped subsidies weighed against occasional litigation wins — nets clearly target-side, so the override fixes d at 0.58. Consumers are assigned power 'powerless' rather than 'moderate': in trade politics diffuse consumer interests are rationally ignored, which leaves the moderate power atom occupied solely by the governments the override addresses.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the interwar spiral of protection, discriminatory preference blocs, and retaliatory collapse — is not dead: protectionist relapse remains the live fear every safeguard invocation cites, so no mandatrophy resolution is declared. The tangled_rope claim guards both misclassification errors. Against snare: the coordination core is real — MFN eliminated a genuine transaction-cost morass, and legalized dispute settlement delivers wins to small members (small-state complainants have beaten far larger respondents), which a pure extraction machine would not permit. Against rope: the incidence is asymmetric by construction of this reading — identical rules applied to radically asymmetric capacities convert formal symmetry into material extraction, which is why beneficiaries and victims are both declared and enforcement is required to hold the structure. The piton-risk signal worth watching is the theater_ratio trend (0.16 to 0.36): if dispute-settlement decay continues while S&D stays decorative, the arrangement drifts toward theatrical maintenance of a coordination story its enforcement no longer performs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_wto,
    'This constraint is the market_access_reading instantiation of the wto_treaty_framework kernel. What changes structurally if the sibling developmental_reading is instantiated instead, and where exactly is the disagreement located?',
    'Compare against the sibling story when generated: the victim and beneficiary sets invert (infant industries become accommodated principals rather than targets; policy space becomes a treaty-core commitment rather than a forfeited exception), epsilon for Southern seats falls sharply, and the S&D provisions migrate from temporary exception to permanent structural accommodation. The disagreement is located in two elements: the status of differential treatment (temporary transition vs permanent accommodation) and whether policy-space/technology-transfer commitments are treaty-grade obligations or best-endeavor recitals.',
    'Classification is reading-relative: this story''s tangled_rope verdict with high epsilon holds only under the market_access instantiation. Under the sibling reading the same treaty text computes different directionalities, a different victim set, and plausibly a rope-side verdict. Cross-reading comparisons must join on the kernel, not on the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_wto, conceptual, 'Committer structure routed to omega: one kernel, two structurally distinct readings, disagreement localized in S&D status and policy-space grade.').

omega_variable(
    sd_exception_vs_accommodation_status,
    'Are the special-and-differential-treatment provisions materially operative transitions, or declaratory placeholders whose ''temporary exception'' framing is the load-bearing premise of this reading?',
    'Audit waiver utilization rates, LDC duty-free quota-free implementation coverage, and the outcome record of the S&D review mechanism from 1995 forward; count provisions actually invoked against provisions merely recited in preamble and decision language.',
    'If S&D is materially operative, extraction on the least-developed seats falls and per-seat classifications drift toward rope; if decorative, the temporary-transition premise operates as cover for permanently asymmetric incidence and the snare gradient strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sd_exception_vs_accommodation_status, empirical, 'Whether the transitional-exception framing matches the operational record of differential treatment.').

omega_variable(
    developed_subsidy_symmetry_test,
    'Does the persistence of large developed-country agricultural support within negotiated ceilings violate, in effect, the symmetry premise this reading rests on?',
    'Compare notified Aggregate Measurement of Support and de minimis usage by developed members against developing members'' demonstrated fiscal capacity to deploy equivalent instruments, using annual notification datasets and farm-support estimates.',
    'A confirmed double standard converts the reading''s symmetry axiom into asymmetric-in-effect extraction, strengthening the tangled_rope-to-snare gradient; a clean comparison preserves the coordination-first account and lowers measured epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(developed_subsidy_symmetry_test, empirical, 'Whether ceiling-legal incumbent support constitutes effective violation of the symmetry the reading claims.').

omega_variable(
    exit_option_realism_small_members,
    'How real is exit — withdrawal or non-participation — for small trading states facing adverse rules or rulings?',
    'Study historical withdrawal and non-ratification episodes alongside counterfactual market-access-loss modeling for small open economies dependent on external demand.',
    'If exit is effectively closed, trapped-target directionality holds and suppression reads as structural; if credible exit exists, measured suppression overstates the constraint and rope-side weighting rises for the small-member seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_realism_small_members, empirical, 'Realism of the exit option underlying small-member directionality derivations.').

omega_variable(
    policy_space_measurement_frame,
    'Is ''compressed policy space'' a demonstrable, targeted reduction relative to what earlier industrializers used, or an indistinct sliver of ordinary treaty sovereignty cost?',
    'Code the industrial-policy toolkit (tariff bindings coverage, subsidy disciplines, performance and local-content requirements) available to mid-century East Asian and Latin American developers versus the WTO-consistent equivalent set today, controlling for capital-account and technology regime changes.',
    'Demonstrable, catch-up-specific compression corroborates the infant-industry victim declaration and sustains high epsilon; evidence that equivalent space persists narrows the victim set and pulls epsilon down.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_space_measurement_frame, empirical, 'Measurability and specificity of the policy-space compression attributed to the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__market_access_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t0, wto_treaty_framework__market_access_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement_basis(wto__tr_t0, observed).
narrative_ontology:measurement(wto__tr_t5, wto_treaty_framework__market_access_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement_basis(wto__tr_t5, observed).
narrative_ontology:measurement(wto__tr_t10, wto_treaty_framework__market_access_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement_basis(wto__tr_t10, observed).
narrative_ontology:measurement(wto__tr_t15, wto_treaty_framework__market_access_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(wto__tr_t15, observed).
narrative_ontology:measurement(wto__tr_t20, wto_treaty_framework__market_access_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(wto__tr_t20, observed).
narrative_ontology:measurement(wto__tr_t25, wto_treaty_framework__market_access_reading, theater_ratio, 25, 0.34).
narrative_ontology:measurement_basis(wto__tr_t25, observed).
narrative_ontology:measurement(wto__tr_t30, wto_treaty_framework__market_access_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement_basis(wto__tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(wto__be_t0, wto_treaty_framework__market_access_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(wto__be_t0, observed).
narrative_ontology:measurement(wto__be_t5, wto_treaty_framework__market_access_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(wto__be_t5, observed).
narrative_ontology:measurement(wto__be_t10, wto_treaty_framework__market_access_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement_basis(wto__be_t10, observed).
narrative_ontology:measurement(wto__be_t15, wto_treaty_framework__market_access_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(wto__be_t15, observed).
narrative_ontology:measurement(wto__be_t20, wto_treaty_framework__market_access_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement_basis(wto__be_t20, observed).
narrative_ontology:measurement(wto__be_t25, wto_treaty_framework__market_access_reading, base_extractiveness, 25, 0.71).
narrative_ontology:measurement_basis(wto__be_t25, observed).
narrative_ontology:measurement(wto__be_t30, wto_treaty_framework__market_access_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(wto__be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t0, wto_treaty_framework__market_access_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(wto__su_t0, observed).
narrative_ontology:measurement(wto__su_t5, wto_treaty_framework__market_access_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(wto__su_t5, observed).
narrative_ontology:measurement(wto__su_t10, wto_treaty_framework__market_access_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(wto__su_t10, observed).
narrative_ontology:measurement(wto__su_t15, wto_treaty_framework__market_access_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(wto__su_t15, observed).
narrative_ontology:measurement(wto__su_t20, wto_treaty_framework__market_access_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(wto__su_t20, observed).
narrative_ontology:measurement(wto__su_t25, wto_treaty_framework__market_access_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement_basis(wto__su_t25, observed).
narrative_ontology:measurement(wto__su_t30, wto_treaty_framework__market_access_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(wto__su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__market_access_reading, resource_allocation).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, wto_treaty_framework__developmental_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the natural-language label 'the WTO treaty framework' covers two structurally distinct claims that cannot share one story. This file authors the market_access_reading — symmetric obligation with transitional exceptions, high epsilon concentrated on late-developer policy instruments, incumbents and their firms as beneficiaries. The sibling file authors the developmental_reading — policy space as equal-status commitment, permanent structural accommodation, inverted victim and beneficiary sets. The epsilon values differ because each reading assesses the same standing arrangement by its own lights and identifies different parties as paying; forcing one story to carry both would make epsilon observer-dependent, which the chi formula forbids. The market_access reading is the historically operational one (secretariat practice and dispute-settlement jurisprudence were built under it) and therefore exerts downstream influence on the conditions under which developmental demands are voiced; each story links the other through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_treaty_framework__market_access_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
