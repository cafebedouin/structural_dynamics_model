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
 *   human_readable: GDPR as Conditional Market Access Requirement (Market-Access Reading)
 *   domain: technology governance/international law/privacy regulation
 *
 * SUMMARY:
 *   The General Data Protection Regulation conditions access to the EU market
 *   for any organization processing EU residents' personal data, wherever
 *   that organization sits. Firms worldwide weigh the cost of GDPR-grade
 *   practice against the value of roughly 450 million customers, and the
 *   overwhelming majority conclude that access is worth the price: they build
 *   compliance functions, appoint data-protection officers, and extend
 *   GDPR-standard handling to their global operations. On this reading the
 *   regulation's worldwide footprint is the shadow of that market calculation
 *   — a standard diffusing through commercial incentive, with EU supervisory
 *   authorities maintaining the standard's credibility through guidance and
 *   selective enforcement rather than projecting day-to-day authority over
 *   foreign conduct. Compliance functions as market strategy: the largest
 *   payers treat the cost as a price of admission and lobby over
 *   implementation, while marginal payers exercise the option the arrangement
 *   leaves open and simply decline the market. KEY AGENTS (by structural
 *   relationship): - eu_supervisory_authorities: Agenda setter
 *   (institutional/constrained) — administers, issues guidance, collects
 *   fines and reputational capital - eu_residents_data_subjects: Primary
 *   beneficiary (moderate/constrained) — hold the enforceable rights the
 *   arrangement delivers - multinational_platforms: Primary payer
 *   (powerful/constrained) — bears the largest compliance outlays and fines -
 *   small_non_eu_online_services: Marginal payer (moderate/mobile) — bears
 *   fixed costs disproportionate to EU revenue; exits by geoblocking -
 *   eu_domiciled_firms: Secondary beneficiary (organized/constrained) —
 *   harmonization raised rivals' costs in their home market -
 *   compliance_industry: Fee-collecting beneficiary (organized/mobile) —
 *   sells the compliance the arrangement requires - non_eu_regulators:
 *   Excluded voice (institutional/trapped) — terms were set without them;
 *   several answered with copycat statutes - brussels_effect_scholars:
 *   Analytical observer (analytical/analytical) — tracks diffusion and
 *   attributes effects
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__market_access_reading, 0.38).
domain_priors:suppression_score(gdpr_article_3_scope__market_access_reading, 0.3).
domain_priors:theater_ratio(gdpr_article_3_scope__market_access_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__market_access_reading, rope).
narrative_ontology:human_readable(gdpr_article_3_scope__market_access_reading, "GDPR as Conditional Market Access Requirement (Market-Access Reading)").
narrative_ontology:topic_domain(gdpr_article_3_scope__market_access_reading, "technology governance/international law/privacy regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__market_access_reading, 'f27b6a91-3879-4e06-a14c-92e0ac9a46c9').
narrative_ontology:cs_kernel_codification('f27b6a91-3879-4e06-a14c-92e0ac9a46c9', formalized).
narrative_ontology:cs_authority_grounding('f27b6a91-3879-4e06-a14c-92e0ac9a46c9', lineage).
narrative_ontology:cs_interpretation_layer_present('f27b6a91-3879-4e06-a14c-92e0ac9a46c9').
narrative_ontology:cs_reading_relation('f27b6a91-3879-4e06-a14c-92e0ac9a46c9', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('f27b6a91-3879-4e06-a14c-92e0ac9a46c9', gdpr_article_3_scope__territorial_sovereignty_reading, influences).
narrative_ontology:cs_axiom('f27b6a91-3879-4e06-a14c-92e0ac9a46c9', foundational, extraterritorial_compliance_is_voluntary_adoption).
narrative_ontology:cs_axiom_status(extraterritorial_compliance_is_voluntary_adoption, holdable).
narrative_ontology:cs_axiom_grounding('f27b6a91-3879-4e06-a14c-92e0ac9a46c9', extraterritorial_compliance_is_voluntary_adoption, empirically_contingent).
narrative_ontology:cs_axiom('f27b6a91-3879-4e06-a14c-92e0ac9a46c9', foundational, market_access_conditioning_is_legitimate_prerogative).
narrative_ontology:cs_axiom_status(market_access_conditioning_is_legitimate_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('f27b6a91-3879-4e06-a14c-92e0ac9a46c9', market_access_conditioning_is_legitimate_prerogative, conventional).
narrative_ontology:cs_reference_frame('f27b6a91-3879-4e06-a14c-92e0ac9a46c9', market_access_conditionality).
narrative_ontology:cs_drift_state('f27b6a91-3879-4e06-a14c-92e0ac9a46c9', contemporary_enforcement_maturation, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('f27b6a91-3879-4e06-a14c-92e0ac9a46c9', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_residents_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_domiciled_firms).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_supervisory_authorities).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, compliance_industry).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, small_non_eu_online_services).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, multinational_platforms).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__market_access_reading, brussels_effect_standard_diffusion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National data-protection authorities and the European Data Protection Board issue binding guidance, investigate complaints, and levy administrative fines up to four percent of worldwide turnover. They wrote the delegated guidance that fills the regulation's open texture and decide which violations to pursue. Their budgets and staffing grew substantially after 2018, and the regulation's international reputation raises their standing; fine receipts accrue to member-state budgets.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_supervisory_authorities, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__market_access_reading, eu_supervisory_authorities, beneficiary).

% Roughly 450 million people hold enforceable rights to access, correct, delete, and port their personal data, and to complain to a supervisor at no cost. Most never invoke the machinery individually; NGOs and representative actions carry the workload. Their practical leverage runs through complaint volume and electoral salience rather than direct bargaining with any firm.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_residents_data_subjects, beneficiary,
    moderate, biographical, constrained, continental).

% Global advertising, cloud, and social firms process EU residents' data at scale and maintain permanent compliance organizations spanning legal, engineering, and product review. They absorbed the largest absolute compliance outlays and the largest fines to date. EU revenue is material to their business models, so excluding EU users is periodically evaluated and rejected; they lobby over implementation details rather than the regulation's existence.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, multinational_platforms, payer,
    powerful, generational, constrained, global).

% Foreign websites and apps with modest EU traffic faced the same fixed compliance costs as the giants — counsel, data-protection-officer appointment, records of processing — against thin EU revenue. Many responded by blocking EU IP addresses entirely, forgoing the market rather than building a compliance function; others bought standardized tooling and carried the overhead.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, small_non_eu_online_services, payer,
    moderate, immediate, mobile, regional).

% European companies operated under national data-protection law before 2016 and had already built the relevant functions; harmonization replaced twenty-seven divergent regimes with one and raised the entry costs their foreign competitors face in the home market. They staffed the trade associations that shaped implementing guidance.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_domiciled_firms, beneficiary,
    organized, biographical, constrained, continental).

% Law firms, consultancies, privacy-software vendors, and consent-management platforms sell interpretation, audits, and tooling to every firm touching EU data. Demand spiked at the 2018 application deadline and settled into a permanent maintenance market; consent-banner vendors became a distinct product category.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, compliance_industry, beneficiary,
    organized, biographical, mobile, global).

% Trade ministries and data-protection agencies in the United States, Asia, and elsewhere negotiate adequacy or mutual-recognition arrangements from outside the room where the terms were written. Their exporters must satisfy provisions they had no vote on. Several responded by drafting their own statutes modeled on the EU text.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, non_eu_regulators, excluded,
    institutional, generational, trapped, global).

% Comparative-law and international-regulation researchers track how the EU text propagates through corporate policy and foreign legislation. They publish the adoption counts and causal analyses that the other seats cite when characterizing what the regulation does abroad.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, brussels_effect_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__market_access_reading, compliance_industry).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__market_access_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single uniform rulebook for processing personal data across the 27-member EU market, replacing fragmented national regimes: any firm can reach roughly 450 million users under one compliance architecture, and individuals hold identical rights everywhere in the union.
% TRANSFER_FUNCTION: Moves compliance expenditure from firms seeking EU market access to legal, audit, and privacy-engineering providers and to internal data-protection functions; occasionally moves administrative fines from violators to member-state budgets; moves data-control entitlements to EU residents.
% ABSENT_VOICES: Non-EU governments and trade ministries had no seat in the trilogue that set terms conditioning their exporters' market access. Small foreign online businesses — the class least able to amortize fixed compliance costs — were represented neither by large-firm lobbies nor by EU civil-society groups. Third-country data subjects whose local standards shift by diffusion had no voice either.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the EU's adequacy framework would lapse, national data-protection law would fragment back toward pre-2016 divergence, firms would unwind unified compliance architectures into per-jurisdiction patchworks, and the worldwide adoption of GDPR-grade rules would reverse as rival standards or no standard governed third-market practice.
% FOUNDING_PROBLEM: Pre-2016 fragmentation: twenty-seven divergent national data-protection regimes under the 1995 Directive produced uneven individual protection, legal uncertainty, and duplicated compliance for cross-border digital services, under rules written before cloud computing and social media.
% FOUNDING_PROBLEM_CORROBORATION: European Commission fitness-check evaluations and EDPB coherence reports — bodies whose remit is evaluation rather than rent collection — document continued enforcement divergence among authorities, and independent comparative-privacy scholarship outside the beneficiary set attests both the original fragmentation problem and its persistence in transformed form around cross-border transfers and AI training data. Platform trade associations dispute continuing necessity; the corroborating sources for the transformed-live reading are non-beneficiary.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__market_access_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__market_access_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gdpr_article_3_scope__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__market_access_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__market_access_reading_tests).
:- end_tests(gdpr_article_3_scope__market_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38): compliance outlays are real and recurring, but they are purchased against market access that most incumbents judge worth more than their cost, and the bulk of the spend converts into professional fees and internal capability rather than accruing to the rule-setter. Suppression is low-moderate (0.30): nothing prohibits serving the EU without compliance to a firm that declines the market — geoblocking is exercised routinely — so the residual suppressive force is the practical difficulty of exit at platform scale plus adequacy leverage over third countries; the mechanism is structural (market dependence), not internalized. Theater is moderate (0.32): consent interfaces and boilerplate notices generate well-documented performative compliance, while records-of-processing, security, and DPO functions remain substantive. Accessibility collapse is partial (0.52): once a firm commits to EU revenue, alternatives to compliance collapse almost entirely, but the exit of declining the market stays open and is visibly used. Resistance is moderate-low (0.35): sustained lobbying over implementation and periodic third-country objections, but no serious repeal movement — widespread paid compliance is the revealed verdict of most payers. The claim and the metrics are independent authored facts: the rope claim reflects this reading's structure (conditional access, voluntary uptake, no capturing seat), and the engine computes per-seat classifications from the structural data, where divergence at particular seats is the expected signal. The measurement series run on one shared time grid (2016–2026, six points) so every tracked metric is authored at every examined time point; the rising suppression_requirement series traces the deliberate maturation of enforcement capacity (EDPB consolidation, landmark fines, growing DPA staffing), not a shift in the arrangement's coercive character.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as successful standard-setting it built and maintains; the payer seats experience it as a cost of doing business whose size varies sharply with scale. The two payer seats share a nominal role but diverge structurally: platforms amortize fixed costs over enormous EU revenue and stay, while small foreign services face the same fixed costs against thin revenue and exit — same role, opposite revealed choices, driven entirely by exit optionality. Among beneficiaries the gap is equally real: residents receive rights most rarely exercise personally, while the compliance industry receives a fee stream contingent on the very complexity it helps elaborate. The engine computes these per-seat differences from power, exit, and directional position; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place four seats near the subsidized end: residents (rights delivered at no charge), EU-domiciled firms (a moat they did not pay extra for), the compliance industry (a demand stream), and the supervisory authorities (mandate, staffing, fine receipts, reputational capital). Victim declaration places small non-EU services near the target end, but their mobile exit dampens effective extraction — they can and do decline the market, which is precisely why their loss lands as forgone access rather than captured rent. Multinational platforms derive high directionality from their large transfers and constrained exit: EU revenue is material, so the access condition binds them at close to full weight despite their power. Scope amplification applies modestly: the arrangement's continental-to-global reach makes verification harder, which the engine scales into effective extraction for the targeted seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — twenty-seven divergent national regimes — was substantially solved inside the EU by 2018, yet the arrangement persists and has expanded. The mandatrophy question is whether persistence rides the solved problem or a transformed live one; the evidence (cross-border transfer litigation, AI training-data disputes, continued enforcement divergence) supports the latter, so no dead-mandate zombie flag is warranted. Classification discipline cuts both ways here: reading the arrangement as pure extraction would erase the voluntary-uptake evidence — firms pay without being chased, and the marginal ones leave; reading it as frictionless coordination would erase the unilateral terms-setting and the small-firm casualties. The rope claim with an authored victim seat keeps both signals live for the engine's per-seat computation, and the R5 interview records the founding problem as live-in-transformed-form rather than resolved or dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is Article 3(2)''s operative structure better captured as conditional market access (this reading) or as effects-based extension of protection through the targeting-and-monitoring tests?',
    'Comparative analysis of enforcement practice against organizations with no EU establishment: if sanctions attach only to conduct undertaken for EU market participation, the access-condition account holds; if they reach conduct with a mere monitoring nexus, the effects account gains ground.',
    'Resolution reallocates the phenomenon between this story and the effects-jurisdiction sibling story; extraction and suppression estimates move with whichever account is adopted, and this reading''s rope claim would weaken under the effects account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which sibling reading of the gdpr_article_3_scope kernel captures Article 3(2)''s operative structure.').

omega_variable(
    exit_realism_for_global_platforms,
    'Is declining EU market access a real option for dominant global platforms, or is the nominally voluntary compliance choice compelled for any firm whose business model requires EU users?',
    'Revealed-preference study of firms that geoblocked or exited versus absorbed compliance costs, controlling for EU revenue share and substitutability of other markets.',
    'If exit is unreal at platform scale, effective extraction for the platform seat rises toward the effects-reading profile and the rope claim weakens toward a hybrid at that seat; if exit is real, the conditional-access framing stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_realism_for_global_platforms, empirical, 'Whether platform compliance is strategic purchase or compelled concession.').

omega_variable(
    standard_diffusion_beneficiary_attribution,
    'Who actually captures the standard-diffusion payoff — identifiable EU institutional seats (negotiating leverage, reputational capital), or diffuse third-country populations whose local protections upgrade as firms extend GDPR-grade practice worldwide?',
    'Trace the diffusion channels separately — adequacy negotiations, contractual cascades through supply chains, copycat legislation — and attribute welfare gains per recipient class.',
    'Determines whether the beneficiary map should widen beyond the EU-centered seats authored here, or whether an EU-institutional capture seat should be added; changes directionality for the authority seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(standard_diffusion_beneficiary_attribution, conceptual, 'Attribution of the diffusion dividend across EU institutional and third-country recipients.').

omega_variable(
    fixed_cost_exclusion_magnitude,
    'How many small non-EU online services actually lost EU market access because fixed compliance costs exceeded reachable EU revenue?',
    'Survey geoblocking incidence among small non-EU services before and after 2018, and measure traffic restoration as compliance tooling commoditized.',
    'Quantifies the victim class: a negligible count thins the victim structure toward a pure coordination reading, while a large count strengthens the distributional objection and the target-seat classification for small firms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fixed_cost_exclusion_magnitude, empirical, 'Magnitude of the small-firm exclusion effect from fixed compliance costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__market_access_reading, 2016, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t2016, gdpr_article_3_scope__market_access_reading, theater_ratio, 2016, 0.1).
narrative_ontology:measurement_basis(gdpr_tr_t2016, observed).
narrative_ontology:measurement(gdpr_tr_t2018, gdpr_article_3_scope__market_access_reading, theater_ratio, 2018, 0.18).
narrative_ontology:measurement_basis(gdpr_tr_t2018, observed).
narrative_ontology:measurement(gdpr_tr_t2020, gdpr_article_3_scope__market_access_reading, theater_ratio, 2020, 0.24).
narrative_ontology:measurement_basis(gdpr_tr_t2020, observed).
narrative_ontology:measurement(gdpr_tr_t2022, gdpr_article_3_scope__market_access_reading, theater_ratio, 2022, 0.28).
narrative_ontology:measurement_basis(gdpr_tr_t2022, observed).
narrative_ontology:measurement(gdpr_tr_t2024, gdpr_article_3_scope__market_access_reading, theater_ratio, 2024, 0.31).
narrative_ontology:measurement_basis(gdpr_tr_t2024, observed).
narrative_ontology:measurement(gdpr_tr_t2026, gdpr_article_3_scope__market_access_reading, theater_ratio, 2026, 0.32).
narrative_ontology:measurement_basis(gdpr_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t2016, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2016, 0.22).
narrative_ontology:measurement_basis(gdpr_be_t2016, observed).
narrative_ontology:measurement(gdpr_be_t2018, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2018, 0.34).
narrative_ontology:measurement_basis(gdpr_be_t2018, observed).
narrative_ontology:measurement(gdpr_be_t2020, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2020, 0.36).
narrative_ontology:measurement_basis(gdpr_be_t2020, observed).
narrative_ontology:measurement(gdpr_be_t2022, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2022, 0.37).
narrative_ontology:measurement_basis(gdpr_be_t2022, observed).
narrative_ontology:measurement(gdpr_be_t2024, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2024, 0.38).
narrative_ontology:measurement_basis(gdpr_be_t2024, observed).
narrative_ontology:measurement(gdpr_be_t2026, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2026, 0.38).
narrative_ontology:measurement_basis(gdpr_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t2016, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2016, 0.08).
narrative_ontology:measurement_basis(gdpr_su_t2016, observed).
narrative_ontology:measurement(gdpr_su_t2018, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2018, 0.16).
narrative_ontology:measurement_basis(gdpr_su_t2018, observed).
narrative_ontology:measurement(gdpr_su_t2020, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2020, 0.22).
narrative_ontology:measurement_basis(gdpr_su_t2020, observed).
narrative_ontology:measurement(gdpr_su_t2022, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2022, 0.26).
narrative_ontology:measurement_basis(gdpr_su_t2022, observed).
narrative_ontology:measurement(gdpr_su_t2024, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2024, 0.29).
narrative_ontology:measurement_basis(gdpr_su_t2024, observed).
narrative_ontology:measurement(gdpr_su_t2026, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2026, 0.3).
narrative_ontology:measurement_basis(gdpr_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__market_access_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope__territorial_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'GDPR extraterritoriality' covers three structurally distinct claims about Article 3 and decomposes into a three-story constraint family per the epsilon-invariance principle: this story (market-access conditionality — compliance as purchased admission, lowest enforcement tension), the effects-jurisdiction story (protection following effects on EU residents via targeting/monitoring tests — higher extraction, contested enforcement reach), and the territorial-sovereignty story (application bounded by territorial authority — framed as overreach). Each story carries its own epsilon, beneficiary/victim structure, and enforcement profile. This reading sits upstream of the sovereignty story in argumentative structure: the access-condition framing is the standard rejoinder to sovereignty objections and materially changed that debate's terrain, while coexisting with the effects account in official discourse, where the Commission and supervisory authorities mix both languages.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
