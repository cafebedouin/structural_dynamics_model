% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__market_access_reading, []).

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
 *   constraint_id: gdpr_article_3_scope__market_access_reading
 *   human_readable: GDPR Article 3(2) as Conditional Market Access Requirement (Market-Access Reading)
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   Under the market-access reading, Article 3(2) of the GDPR operates as a
 *   condition of entry to the Union market rather than an assertion of
 *   regulatory jurisdiction over foreign territory: any controller outside
 *   the Union that offers goods or services to people in the Union, or
 *   monitors their behavior, may serve that market only on the Union's data
 *   protection terms. Compliance is therefore priced like other market-entry
 *   costs, and the provision's global reach arises because firms rationally
 *   standardize on one privacy regime worldwide rather than maintain regional
 *   variants — standard diffusion doing the work that jurisdictional
 *   enforcement cannot. The claim/metric gap is deliberate: the reading
 *   CLAIMS tangled_rope (a genuine single-standard coordination function
 *   carrying an asymmetrically distributed access charge) while the metrics
 *   are authored from the arrangement's observed operation; the engine
 *   measures the divergence. This story instantiates one reading of the
 *   gdpr_article_3_scope kernel; the sibling readings are separate constraint
 *   files linked in network.affects_constraints.
 *
 * KEY AGENTS:
 *   - eu_supervisory_authorities: Agenda setter (institutional/arbitrage) — writes and enforces the access condition, collects fines, gains institutional reach
 *   - eu_data_subjects: Primary intended beneficiary (moderate/mobile) — receives enforceable data rights; carries indirect costs as secondary payer
 *   - eu_established_controllers: Secondary beneficiary and gross payer (organized/constrained) — harmonization gains offset by heavy direct compliance spending
 *   - us_multinational_platforms: Primary target among large firms (powerful/constrained) — pays the access charge and converts it into worldwide standardization
 *   - small_foreign_online_retailers: Primary target among small firms (powerless/trapped) — bears the regressive share of the charge with no viable exit
 *   - third_country_regulators: Excluded party (institutional/trapped) — policy space narrowed by diffusion they never voted on
 *   - trade_policy_analysts: Analytical observer (analytical/analytical) — documents cost and diffusion patterns for external audiences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__market_access_reading, 0.62).
domain_priors:suppression_score(gdpr_article_3_scope__market_access_reading, 0.51).
domain_priors:theater_ratio(gdpr_article_3_scope__market_access_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__market_access_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__market_access_reading, "GDPR Article 3(2) as Conditional Market Access Requirement (Market-Access Reading)").
narrative_ontology:topic_domain(gdpr_article_3_scope__market_access_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__market_access_reading, '273be841-cda6-452c-9377-ca7b262af2ca').
narrative_ontology:cs_kernel_codification('273be841-cda6-452c-9377-ca7b262af2ca', fixed_text).
narrative_ontology:cs_authority_grounding('273be841-cda6-452c-9377-ca7b262af2ca', lineage).
narrative_ontology:cs_interpretation_layer_present('273be841-cda6-452c-9377-ca7b262af2ca').
narrative_ontology:cs_reading_relation('273be841-cda6-452c-9377-ca7b262af2ca', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('273be841-cda6-452c-9377-ca7b262af2ca', gdpr_article_3_scope__territorial_sovereignty_reading, influences).
narrative_ontology:cs_axiom('273be841-cda6-452c-9377-ca7b262af2ca', foundational, compliance_is_rational_market_entry_pricing).
narrative_ontology:cs_axiom_status(compliance_is_rational_market_entry_pricing, holdable).
narrative_ontology:cs_axiom_grounding('273be841-cda6-452c-9377-ca7b262af2ca', compliance_is_rational_market_entry_pricing, empirically_contingent).
narrative_ontology:cs_axiom('273be841-cda6-452c-9377-ca7b262af2ca', secondary, standard_diffusion_substitutes_for_jurisdictional_coercion).
narrative_ontology:cs_axiom_status(standard_diffusion_substitutes_for_jurisdictional_coercion, holdable).
narrative_ontology:cs_axiom_grounding('273be841-cda6-452c-9377-ca7b262af2ca', standard_diffusion_substitutes_for_jurisdictional_coercion, empirically_contingent).
narrative_ontology:cs_reference_frame('273be841-cda6-452c-9377-ca7b262af2ca', conditional_market_access_standard).
narrative_ontology:cs_drift_state('273be841-cda6-452c-9377-ca7b262af2ca', post_enforcement_maturity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('273be841-cda6-452c-9377-ca7b262af2ca', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_established_controllers).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_supervisory_authorities).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, us_multinational_platforms).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, small_foreign_online_retailers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, eu_data_subjects).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, eu_established_controllers).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__market_access_reading, brussels_effect_hypothesis).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__market_access_reading, regulatory_standard_diffusion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National data protection authorities and the European Data Protection Board issue guidelines, coordinate cross-border enforcement, and impose administrative fines of up to 4% of worldwide turnover on controllers wherever established that serve or monitor people in the Union. Fine proceeds flow into member-state budgets, enforcement teams expand, and the authorities' remit grows as foreign firms bring their disputes to their door. Their discretion lies in enforcement posture: priority-setting, guideline issuance, and adequacy negotiations all shift how demanding the compliance bar is.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_supervisory_authorities, agenda_setter,
    institutional, generational, arbitrage, continental).

% Residents of the Union whose personal data is processed by the services they use, including services operated from abroad. They hold enforceable rights over their data — access, erasure, portability, objection — exercisable through a single complaint channel regardless of where the service is based. They also absorb indirect costs: consent interruptions across the web, occasional withdrawal of services from the Union market altogether, and compliance spending passed through into prices.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_data_subjects, beneficiary,
    moderate, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__market_access_reading, eu_data_subjects, payer).

% Companies established inside the Union that already operated under the 1995 Data Protection Directive. Harmonization replaces twenty-seven divergent national regimes with one rulebook and one lead-supervisor contact, and raises the compliance floor beneath foreign competitors entering their home market. They nonetheless carry heavy direct compliance spending of their own — data protection officers, records of processing, impact assessments — and participated extensively in shaping the final text through industry consultation during the legislative trilogue.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_established_controllers, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__market_access_reading, eu_established_controllers, payer).

% Large non-EU technology companies whose advertising, cloud, and social products serve hundreds of millions of Union residents. They maintain dedicated compliance organizations, rebuild global data flows around Union requirements, and have absorbed headline fines ranging from hundreds of millions to over a billion euros. Union revenue is material, so withdrawal is not a live option; instead they standardize practices worldwide — spreading Union requirements to markets that never voted for them — and litigate individual orders they regard as unfounded.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, us_multinational_platforms, payer,
    powerful, biographical, constrained, global).

% Small merchants and content sites outside the Union that sell to or reach Union residents. Fixed compliance tasks — appointing representatives, maintaining processing records, answering data subject requests — weigh far more heavily on a ten-person operation than on a platform with thousands of lawyers, and operators report the compliance load exceeding the profit earned on Union sales. Their realistic choices are absorbing disproportionate costs or blocking Union visitors, which for export-dependent shops amounts to losing the market.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, small_foreign_online_retailers, payer,
    powerless, immediate, trapped, global).

% Data protection and telecommunications regulators in countries whose industries adopt Union standards because their firms operate globally. They did not take part in setting the rules their domestic firms now follow, and their room to legislate lighter or different national rules narrows whenever their industry has already standardized on the Union text. Several respond by drafting mirror legislation, which further entrenches the exported standard.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, third_country_regulators, excluded,
    institutional, generational, trapped, global).

% Trade lawyers, economists, and international-relations scholars tracking how the arrangement reshapes cross-border data commerce. They publish assessments of compliance-cost surveys, standard-diffusion patterns, and adequacy negotiations; legislatures and firms cite their work when deciding whether to treat the arrangement as market regulation, rights protection, or trade barrier.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, trade_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__market_access_reading, eu_supervisory_authorities).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__market_access_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces twenty-seven divergent national data protection regimes with a single rulebook and a single lead-supervisor interface for any firm serving the Union market, giving cross-border digital commerce one predictable set of handling rules and giving individuals one enforceable rights regime regardless of where a service is based.
% TRANSFER_FUNCTION: Moves compliance expenditure, administrative-fine exposure, and de facto rule-setting authority from firms seeking access to the Union market — disproportionately foreign entrants and smaller operators — toward the Union's supervisory apparatus, member-state budgets, and the compliance-services sector, while shifting decision power over worldwide data practices to Brussels.
% ABSENT_VOICES: Third-country regulators whose policy space narrows when their industries standardize on rules they never helped write; non-Union data subjects whose protection depends on their own governments importing a standard set elsewhere; small foreign merchants with no seat in trilogue consultations or supervisory proceedings. All sit outside the arrangement's formation and adjudication entirely.
% DISAPPEARANCE_RATIONALE: If the Article 3(2) access condition vanished overnight, firms would revert to jurisdiction-segmented compliance, the worldwide standardization built on Union terms would unwind over years as regional variants re-emerged, supervisory authorities would lose their practical reach over foreign controllers, and the compliance-services market built around the unified rulebook would contract sharply.
% FOUNDING_PROBLEM: Cross-border data flows had outgrown national supervision: twenty-seven divergent implementations of the 1995 Directive left firms facing inconsistent rules and individuals with rights that stopped at borders, while enforcement against foreign processors was practically unavailable.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: the Council of Europe's Convention 108 (1981) and the OECD Privacy Guidelines (1980) independently document the cross-border enforcement gap decades before the GDPR; United States state legislatures citing the same fragmentation enacted their own statutes; multinational firms' public filings describe multi-jurisdiction compliance complexity as a standing cost. No corroborating source attests that the problem is solved.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__market_access_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__market_access_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gdpr_article_3_scope__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__market_access_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__market_access_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__market_access_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gdpr_article_3_scope__market_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the access charge is real and decoupled from marginal protection cost at the top of the market — a single firm absorbed a EUR 1.2 billion fine — but it purchases genuine goods: one rulebook instead of twenty-seven, legal certainty for market entry, and enforceable rights that data subjects actually exercise. Suppression 0.51: enforcement machinery matured steadily (coordinated enforcement actions, cross-border procedures, adequacy leverage), yet exits remain structurally available — geo-blocking, market withdrawal, adequacy litigation — so coercion bounds behavior without eliminating alternatives. Theater_ratio 0.33: consent-interface theater peaked around t=3 (interstitial banners, pre-ticked boxes) and was partially corrected by guidance against deceptive design, leaving residual performative compliance in policies nobody reads; the series is a rise-and-partial-correction hump, not an oscillation. Accessibility_collapse 0.55: once a firm understands the terms of entry, non-compliance collapses as a live option for Union-facing business models, but the geo-block exit keeps alternatives partly open for marginal operators. Resistance 0.50: sustained lobbying, diplomatic pushback from trading partners, scholarly criticism, and episodic withdrawal by smaller sites met the rollout, while large firms largely complied and converted compliance into strategy. The time grid maps t=0..15 onto 2016..2031 in three-year steps; all three series share the one six-point grid, and points after t=9 are projections marked as such.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical nominal roles. Two payer seats face the same provision: the large-platform seat experiences the charge as a manageable strategic cost it can amortize globally and even deploy against smaller rivals, while the small-retailer seat experiences the same charge as potentially fatal overhead — fixed compliance costs scale with firm size, so nominally equal rates are unequal burdens. The small-firm seat is scattered across jurisdictions and sectors with no coalition infrastructure, so its numerical weight never converts into negotiating power the way organized industry associations' does. The agenda-setter seat experiences the arrangement as legitimate condition-setting it administers and benefits from administering; the excluded third-country regulator seat experiences a narrowing of policy space it never consented to. The engine derives these divergent classifications from power, exit, and directionality data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the seats the arrangement subsidizes: supervisory authorities sit nearest the beneficiary pole (they write the terms, collect the fines, and gain institutional reach), data subjects near it (enforceable rights against diffuse indirect costs). The dual-positioned EU controller seat is the one place the automatic derivation would mislead: declared beneficiary, it also pays heavy direct compliance costs, so an override lifts its directionality to 0.30 — a net mild beneficiary whose gross payment position still matters for the extraction arithmetic. Victim declarations drive the payer seats toward the target pole: large platforms near-full target (substantial payments, constrained exit, partially recouped through the competitive moat compliance builds), small foreign retailers at the full-target end (disproportionate share, no viable exit). Third-country regulators are neither payers of the access charge nor beneficiaries of it; their loss runs through standard diffusion and is carried as an open question in the sovereignty_cost_attribution omega rather than forced into the victim set.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legal fragmentation across twenty-seven regimes and an enforcement gap for data crossing borders faster than supervision could follow — remains live: cross-border data flows, AI training corpora, and behavioral advertising reproduce it at larger scale. Because the mandate is alive, the classification guards against two opposite mislabels: reading the arrangement as pure coordination would erase the regressive distribution of the access charge documented in the payer seats; reading it as pure extraction would erase the voluntary-compliance character and the genuine single-standard good the arrangement delivers. Tangled rope keeps both facts on the table, and the temporal series shows the extraction component accumulating gently on top of a stable coordination core rather than displacing it. No sunset applies because no transition is underway; the arrangement presents itself as steady-state market regulation, not scaffolding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the market_access_reading of the gdpr_article_3_scope kernel; how would classification shift under the sibling readings?',
    'Generate the sibling stories (effects_jurisdiction_reading, territorial_sovereignty_reading) and compare computed types and epsilon values; divergence between the three localizes the contest to specific structural elements.',
    'Under the effects reading the same provision computes as jurisdictional assertion with higher enforcement tension and a state-like extraction profile; under the territorial reading it computes as authority overreach with a legitimacy deficit. This story''s epsilon refers only to the market-access arrangement and must not be averaged across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three readings of a contested statutory-scope kernel.').

omega_variable(
    brussels_effect_vs_deterrence,
    'Is worldwide alignment on Union data protection terms driven by market-access rationality (one standard is cheaper than many) or by deterrence (fear of fines and orders)?',
    'Natural experiment: measure compliance persistence where enforcement probability is negligible (long-tail foreign small firms, lightly supervised sectors), alongside firm disclosures on single-standard consolidation decisions.',
    'If deterrence-dominated, the arrangement sits closer to enforced extraction than the tangled_rope claim allows; if market-rationality-dominated, the extraction measured is closer to an access price for a genuine coordination good, supporting the claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brussels_effect_vs_deterrence, empirical, 'Whether the diffusion mechanism is voluntary standardization or coercive deterrence.').

omega_variable(
    sme_burden_regression,
    'How is the compliance burden distributed as a share of revenue across firm sizes and establishment locations?',
    'Compliance-cost surveys stratified by firm size and jurisdiction, controlling for sector and data-processing intensity.',
    'A strongly regressive distribution confirms the asymmetric-extraction half of the tangled_rope structure and sharpens the small-retailer seat''s computed extraction; a flat distribution would support reading the charge as ordinary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sme_burden_regression, empirical, 'Regressivity of the access charge across firm sizes.').

omega_variable(
    geo_block_exit_prevalence,
    'How many non-EU services actually exercise the geo-block exit rather than comply?',
    'Longitudinal measurement of changes in Union-accessible service availability following enforcement milestones, distinguishing deliberate withdrawal from unrelated market exits.',
    'Widespread exit lowers the measured suppression (the exit is real and cheap for some); rare exit indicates the exit is nominal for most and effective suppression is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geo_block_exit_prevalence, empirical, 'Prevalence of the market-withdrawal exit path.').

omega_variable(
    sovereignty_cost_attribution,
    'Do the policy-space losses of third-country regulators count as extraction by this arrangement, or as externalities of other governments'' voluntary adoption decisions?',
    'Conceptual analysis distinguishing imposed standards from adopted ones, tracing individual adoption decisions to market incentives versus regulatory expectation, combined with comparative review of mirror-legislation episodes.',
    'If counted as extraction, the victim set expands beyond the payer seats and epsilon rises; if treated as externality, the current victim set stands and the excluded seat stays commentary-grade.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_cost_attribution, conceptual, 'Attribution of third-country sovereignty costs to the arrangement versus to adopters.').

omega_variable(
    framing_under_determination,
    'Is the kernel best framed as the statutory text of Article 3 (fixed_text, as declared here) or as the enforcement-practice complex itself (an implicit kernel defined by what supervisory authorities actually do)?',
    'Compare classification under both framings: signals favoring the textual frame are the primacy of CJEU interpretation and the stability of the statutory language; signals favoring the practice frame are guideline-driven drift and discretionary enforcement posture.',
    'An implicit-kernel framing would move authority_grounding toward practice, change the drift assessment (practice would define rather than depart from the kernel), and potentially alter the computed commitment-system pattern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_under_determination, conceptual, 'Framing choice between textual and practice-defined kernels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__market_access_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_ma_read_tr_t0, gdpr_article_3_scope__market_access_reading, theater_ratio, 0, 0.24).
narrative_ontology:measurement_basis(gdpr_ma_read_tr_t0, observed).
narrative_ontology:measurement(gdpr_ma_read_tr_t3, gdpr_article_3_scope__market_access_reading, theater_ratio, 3, 0.41).
narrative_ontology:measurement_basis(gdpr_ma_read_tr_t3, observed).
narrative_ontology:measurement(gdpr_ma_read_tr_t6, gdpr_article_3_scope__market_access_reading, theater_ratio, 6, 0.39).
narrative_ontology:measurement_basis(gdpr_ma_read_tr_t6, observed).
narrative_ontology:measurement(gdpr_ma_read_tr_t9, gdpr_article_3_scope__market_access_reading, theater_ratio, 9, 0.35).
narrative_ontology:measurement_basis(gdpr_ma_read_tr_t9, observed).
narrative_ontology:measurement(gdpr_ma_read_tr_t12, gdpr_article_3_scope__market_access_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement_basis(gdpr_ma_read_tr_t12, projected).
narrative_ontology:measurement(gdpr_ma_read_tr_t15, gdpr_article_3_scope__market_access_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement_basis(gdpr_ma_read_tr_t15, projected).

% Extraction over time
narrative_ontology:measurement(gdpr_ma_read_be_t0, gdpr_article_3_scope__market_access_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(gdpr_ma_read_be_t0, observed).
narrative_ontology:measurement(gdpr_ma_read_be_t3, gdpr_article_3_scope__market_access_reading, base_extractiveness, 3, 0.51).
narrative_ontology:measurement_basis(gdpr_ma_read_be_t3, observed).
narrative_ontology:measurement(gdpr_ma_read_be_t6, gdpr_article_3_scope__market_access_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement_basis(gdpr_ma_read_be_t6, observed).
narrative_ontology:measurement(gdpr_ma_read_be_t9, gdpr_article_3_scope__market_access_reading, base_extractiveness, 9, 0.59).
narrative_ontology:measurement_basis(gdpr_ma_read_be_t9, observed).
narrative_ontology:measurement(gdpr_ma_read_be_t12, gdpr_article_3_scope__market_access_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement_basis(gdpr_ma_read_be_t12, projected).
narrative_ontology:measurement(gdpr_ma_read_be_t15, gdpr_article_3_scope__market_access_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(gdpr_ma_read_be_t15, projected).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_ma_read_su_t0, gdpr_article_3_scope__market_access_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(gdpr_ma_read_su_t0, observed).
narrative_ontology:measurement(gdpr_ma_read_su_t3, gdpr_article_3_scope__market_access_reading, suppression_requirement, 3, 0.38).
narrative_ontology:measurement_basis(gdpr_ma_read_su_t3, observed).
narrative_ontology:measurement(gdpr_ma_read_su_t6, gdpr_article_3_scope__market_access_reading, suppression_requirement, 6, 0.44).
narrative_ontology:measurement_basis(gdpr_ma_read_su_t6, observed).
narrative_ontology:measurement(gdpr_ma_read_su_t9, gdpr_article_3_scope__market_access_reading, suppression_requirement, 9, 0.47).
narrative_ontology:measurement_basis(gdpr_ma_read_su_t9, observed).
narrative_ontology:measurement(gdpr_ma_read_su_t12, gdpr_article_3_scope__market_access_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement_basis(gdpr_ma_read_su_t12, projected).
narrative_ontology:measurement(gdpr_ma_read_su_t15, gdpr_article_3_scope__market_access_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement_basis(gdpr_ma_read_su_t15, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__market_access_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope__territorial_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'GDPR extraterritoriality' conflates three structurally distinct claims about one statutory provision: a jurisdictional-effects claim, a market-access/standard-diffusion claim, and a sovereignty-overreach claim. Each yields a different epsilon, victim set, and enforcement profile, so the kernel decomposes into three linked stories. This file carries the market-access reading, which sits upstream: firms' voluntary compliance supplies the observable facts the other two readings argue about, so this reading's diffusion dynamics exert structural pressure on both siblings without resolving their dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_article_3_scope__market_access_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
