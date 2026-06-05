% ============================================================================
% CONSTRAINT STORY: data_privacy_regulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_data_privacy_regulation, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: data_privacy_regulation
 *   human_readable: Data Privacy Regulation (e.g., GDPR)
 *   domain: political/economic/social/technological
 *
 * SUMMARY:
 *   Data privacy regulation (exemplified by GDPR) represents a fundamental
 *   structural tension between user protection and institutional data
 *   monetization. The constraint manifests as Tangled Rope because it
 *   simultaneously solves a coordination problem (establishing uniform
 *   privacy norms across fragmented markets) and imposes asymmetric
 *   extraction (compliance costs burden small actors disproportionately, and
 *   enforcement remains theatrical despite intentions). GDPR created a
 *   regulatory apparatus ostensibly protecting 'data subjects,' but the
 *   protection is stratified: technically literate users can exercise rights
 *   (access, erasure, portability); users without technical capacity are
 *   trapped by complexity. Large platforms bear compliance costs but benefit
 *   from reduced competition (compliance costs as barrier to entry); small
 *   competitors and startups face existential compliance burdens. Data
 *   brokers face maximum suppression — their business model of selling
 *   profiles without consent is directly targeted. Regulators (DPAs) are
 *   institutional beneficiaries, gaining authority and funding. The
 *   constraint's theater ratio reflects that much compliance is performative:
 *   cookie banners as gesture politics, privacy policies as liability
 *   shifting rather than genuine transparency, consent forms that users don't
 *   read. The measurement trajectory shows extractiveness declining (0.72 →
 *   0.52) as enforcement matures and platforms invest in compliance
 *   infrastructure, but theater ratio rising (0.38 → 0.58) as enforcement
 *   becomes selective and performative. Privacy-tech advocates see a
 *   potential sunset: cryptographic privacy technologies could eventually
 *   make centralized data collection uncompetitive, rendering centralized
 *   regulation redundant.
 *
 * KEY AGENTS:
 *   - Data Subjects: Primary beneficiaries (moderate/mobile) — gain privacy protection, transparency rights, control over data. Highly stratified: technically literate subjects realize benefits; others trapped by complexity.
 *   - Large Tech Platforms: Powerful institutional victims (powerful/constrained) — bear compliance costs (DPOs, audits, breach notification, consent infrastructure) but benefit from competitive consolidation (barrier to entry)
 *   - Data Brokers: Powerful victims (powerful/constrained) — targeted by regulation; core business model (profile sales without consent) under maximum suppression; face high extraction via compliance, liability, revenue restrictions
 *   - Small Tech Startups: Organized victims (organized/constrained) — face disproportionate compliance burden relative to market presence; benefit from platform barrier-to-entry effect but pay high absolute compliance costs
 *   - Privacy Regulators (DPAs): Institutional beneficiaries (institutional/arbitrage) — gain authority, funding, political legitimacy; can adjust enforcement stringency
 *   - Citizens Without Technical Literacy: Powerless trapped victims (powerless/trapped) — nominally protected but cannot exercise rights; complexity becomes extraction mechanism
 *   - Privacy-Tech Developers: Organized beneficiaries (organized/constrained) — building alternatives (differential privacy, federated learning) that could displace centralized collection; have agency but constrained by current platform dominance
 *   - Analytical Observer: Sees full structure, risks naturalizing as law of nature (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(data_privacy_regulation, 0.52).
domain_priors:suppression_score(data_privacy_regulation, 0.65).
domain_priors:theater_ratio(data_privacy_regulation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(data_privacy_regulation, extractiveness, 0.52).
narrative_ontology:constraint_metric(data_privacy_regulation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(data_privacy_regulation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(data_privacy_regulation, tangled_rope).
narrative_ontology:human_readable(data_privacy_regulation, "Data Privacy Regulation (e.g., GDPR)").
narrative_ontology:topic_domain(data_privacy_regulation, "political/economic/social/technological").

domain_priors:requires_active_enforcement(data_privacy_regulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(data_privacy_regulation, data_subjects).
narrative_ontology:constraint_beneficiary(data_privacy_regulation, privacy_advocates).
narrative_ontology:constraint_beneficiary(data_privacy_regulation, small_competitors).
narrative_ontology:constraint_victim(data_privacy_regulation, large_tech_platforms).
narrative_ontology:constraint_victim(data_privacy_regulation, data_brokers).
narrative_ontology:constraint_victim(data_privacy_regulation, analytics_services).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT (ROPE) — Individual user benefits from privacy protection and transparency rights. Can exercise rights (access, erasure, portability) with moderate effort. Exit options exist: migrate platforms, opt-out of tracking. The constraint solves the coordination problem of individual users having minimal leverage against data aggregation. d≈0.35, f(d)≈0.30, σ=0.9 → χ≈0.14.
constraint_indexing:constraint_classification(data_privacy_regulation, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: CITIZEN WITHOUT TECHNICAL LITERACY (SNARE) — Nominally protected by GDPR but cannot effectively exercise rights due to complexity of data flows, obfuscated consent forms, and burden of proof. Trapped: must use digital services to participate in modern society. No realistic exit option. d≈0.88, f(d)≈1.30, σ=1.2 → χ≈0.80. GDPR's complexity becomes an extraction mechanism for the least empowered.
constraint_indexing:constraint_classification(data_privacy_regulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: LARGE TECH PLATFORM (TANGLED ROPE) — Bears compliance costs (data protection officers, audits, breach notification, consent infrastructure). Also benefits from coordination: GDPR establishes uniform rules, eliminates patchwork regulations, creates barriers to entry for smaller competitors (compliance costs are fixed overhead, disproportionate burden on startups). Constrained exit: must comply to operate in EU/global markets. d≈0.60, f(d)≈0.80, σ=1.2 → χ≈0.50. Hybrid: real compliance extraction + real competitive consolidation benefit.
constraint_indexing:constraint_classification(data_privacy_regulation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: DATA BROKER NETWORK (SNARE) — GDPR directly extracts from data brokers by restricting data sale, requiring consent, imposing audit burdens, and enabling data subject requests. Data brokers' core business model (selling profiles without transparent consent) is under maximum suppression. Constrained exit: cannot abandon EU markets without abandoning major revenue. High extraction via compliance costs, liability exposure, restricted data monetization. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(data_privacy_regulation, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PRIVACY REGULATOR (ROPE) — Institutional beneficiary. GDPR creates enforcement authority, funding, political power, and legitimacy. Sees the constraint as coordinating data protection norms across a fragmented landscape. Arbitrage exit: can adjust enforcement stringency, can trade political favors for compliance flexibility. d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.05. Net beneficiary due to institutional mandate expansion.
constraint_indexing:constraint_classification(data_privacy_regulation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: EMERGING TECH STARTUP (TANGLED ROPE) — Benefits from GDPR as a barrier to entry (large compliance costs protect them from mega-platform competition in certain niches). Also bears asymmetric burden: must build privacy-by-design from day one (expensive), cannot use dark patterns for engagement, has limited resources for compliance infrastructure compared to incumbents. Constrained exit: cannot avoid compliance in major markets. d≈0.52, f(d)≈0.68, σ=1.0 → χ≈0.35. Mixed: real coordination benefit (level playing field) + real extraction (compliance overhead).
constraint_indexing:constraint_classification(data_privacy_regulation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: DATA PROTECTION AUTHORITY BUREAUCRACY (PITON) — Theater ratio 0.58 reflects that much GDPR compliance is performative: consent checkboxes that most users don't read, cookie banners as theatrical gestures, privacy policies as legal liability mitigation rather than genuine transparency. Authorities conduct theater compliance audits (forms, documentation) rather than testing actual data protection. The bureaucratic infrastructure persists through institutional inertia and political mandate. Enforcement is selective (high-profile fines) but low-coverage (millions of violations, tiny investigation rate).
constraint_indexing:constraint_classification(data_privacy_regulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 8: PRIVACY-PRESERVING TECHNOLOGY COALITION (SCAFFOLD) — Organized advocates (technologists, privacy lawyers, civil society) see GDPR as a temporary regulatory bridge toward a sunset: cryptographic privacy (differential privacy, federated learning, homomorphic encryption, zero-knowledge proofs) will eventually make centralized data collection unnecessary. GDPR enforces consent and transparency today; technological alternatives will make regulation redundant tomorrow. Constrained exit (must comply now) but organized agency (can influence next-generation standards). d≈0.45, f(d)≈0.45, σ=1.0 → χ≈0.20. Sunset rationale: Privacy-tech maturation estimated 10-15 years; as it scales, centralized data aggregation loses competitive advantage.
constraint_indexing:constraint_classification(data_privacy_regulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Some versions of privacy regulation claim to rest on a natural law: information asymmetry between data collectors and subjects is inevitable in scale; privacy protection requires regulatory force. However, the structural data (ε=0.52, suppression=0.65, theater=0.58) contradicts mountain classification. This is a false summit: the extractiveness, suppression, and theatrical elements are all contingent institutional choices, not irreducible limits. Privacy regulation is not a law of nature.
constraint_indexing:constraint_classification(data_privacy_regulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(data_privacy_regulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(data_privacy_regulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(data_privacy_regulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(data_privacy_regulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(data_privacy_regulation, TR),
    TR >= 0.70.

:- end_tests(data_privacy_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52, down from 0.72 at regulatory launch): GDPR imposes real compliance costs on data collectors (~$5-10M annual for large platforms, 1-3% of revenue). But extractiveness is moderate, not extreme, because: (a) some users genuinely benefit (transparency, control), (b) large platforms can absorb costs and offset with reduced competition, (c) enforcement is selective (high-profile targets get hit; systematic violations undetected). The declining trajectory (0.72→0.52) reflects platform habituation to compliance and DPA enforcement learning curve. Suppression (0.65): Significant barriers remain despite regulation — users still face obfuscated consent flows, dark patterns in cookie banners, practical difficulty exercising rights (GDPR's 'right to be forgotten' faces deletion cost asymmetries). But suppression is not total because regulatory framework exists and some users do mobilize rights (class actions, advocacy). Theater ratio (0.58, rising from 0.38): Rising trajectory reflects increasing performativity. Early GDPR implementation (2018-2020) involved genuine privacy infrastructure (data protection officers, security audits). Current enforcement (2024+) increasingly theater: selective high-profile fines while systematic violations persist unaddressed; cookie banner compliance without genuine consent engineering; privacy impact assessments as checkbox bureaucracy. Claimed type (Tangled Rope): Real coordination function (uniform rules, reduced patchwork) + real asymmetric extraction (compliance costs concentrate, competitive barrier, selective enforcement).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. Data subjects see protection (Rope). Citizens without technical literacy see a trap (Snare — complexity as extraction). Large platforms see mixed burden and benefit (Tangled Rope). Data brokers see pure extraction (Snare). Regulators see institutional legitimacy (Rope). Tech startups see consolidation subsidy (Tangled Rope). Privacy-tech advocates see a temporary bridge (Scaffold with sunset). The civilizational observer risks seeing an immutable principle (Mountain: 'privacy requires regulation') but this is a false summit — the extractive elements are contingent institutional design. The perspectival gap is maximal because the constraint operates at the intersection of power asymmetries (users vs. platforms), institutional interests (regulators), and technological change (privacy-tech alternatives). Different agents experience it as coordination, extraction, theater, opportunity, or trap depending on their structural position and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Data subjects (beneficiary + mobile): d≈0.35, f(d)≈0.30. Low extraction because they have meaningful exit options (platform switching, opt-out mechanisms) and genuine benefit realization. Citizens without literacy (victim + trapped): d≈0.88, f(d)≈1.30. Maximum extraction — cannot exit modern society, cannot exercise rights, complexity traps them. Large platforms (victim + constrained): d≈0.60, f(d)≈0.80. Moderate-high extraction — significant compliance burden but constrained exit (EU/global market access required) and partial benefit from competitive consolidation. Data brokers (victim + constrained): d≈0.92, f(d)≈1.38. Maximum extraction — core business model targeted, constrained exit, no offsetting benefits. Regulators (beneficiary + arbitrage): d≈0.08, f(d)≈-0.10. Net beneficiary — institutional mandate expansion, political power, adjustable enforcement. Startups (mixed + constrained): d≈0.52, f(d)≈0.68. Moderate extraction — high compliance costs but benefits from platform barrier-to-entry. Privacy-tech coalition (organized + constrained): d≈0.45, f(d)≈0.45. Low extraction — has agency, sees alternative pathway. Analytical observer (analytical): d≈0.72, f(d)≈1.15. Mountain classification is perspectival (naturalizes contingent design as law).
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL: The mandatrophy is resolved by decomposing the 'data privacy regulation' label into two structurally distinct constraints. (1) **Core Privacy Coordination** (ε≈0.15, Mountain): The intrinsic principle that users cannot directly monitor data use at scale requires regulatory coordination. This is a genuine Rope or Mountain element — it solves a real coordination failure. (2) **Compliance Extraction Mechanism** (ε≈0.52, current story): The *implementation* of privacy regulation creates selective enforcement, theatrical bureaucracy, compliance cost concentration, and barrier-to-entry effects. These are contingent institutional design choices, not laws of nature. The confusion 'is GDPR coordination or extraction?' arises from conflating these two constraints. The present story correctly isolates the implementation level (Tangled Rope with theater and selective enforcement). A separate story would address the core coordination principle (Mountain or pure Rope). Network link: this story affects constraints like 'platform_data_monetization' and 'regulatory_arbitrage_jurisdictional_shopping' because GDPR's implementation induces downstream institutional responses. The mandatrophy is resolved by clarity: GDPR's *goal* is coordination; GDPR's *implementation* exhibits extraction. Calling it 'Tangled Rope' with theater component captures both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_fiction_sufficiency,
    'Does informed consent (under GDPR Article 7 standards) actually represent voluntary authorization, or is it an extractive fiction that transfers liability from data collectors to users?',
    'Empirical analysis: consent withdrawal rates when cost-free; actual comprehension testing of privacy policies; comparison of user intent vs. disclosed data use; A/B testing of consent UI designs with and without dark patterns',
    'If consent is meaningful: GDPR is coordination (Rope) from most perspectives. If consent is fiction: GDPR is theater (Piton) and regulation does not prevent extraction (Snare persists). High-extractiveness hypothesis requires this to resolve toward ''fiction''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_fiction_sufficiency, empirical, 'Whether GDPR informed consent represents genuine authorization or extractive fiction').

omega_variable(
    compliance_cost_concentration,
    'Do compliance costs disproportionately burden small actors relative to large platforms, creating a consolidation subsidy?',
    'Cost-benefit analysis by firm size: small (<100 employees) vs. medium vs. large (10K+ employees); tracking of startup entry rates post-GDPR; market share concentration metrics in affected sectors; audit of whether compliance costs exceeded compliance benefit for small firms',
    'If costs are regressive: GDPR is Tangled Rope (real privacy benefit + real consolidation extraction). If costs are uniform: GDPR is purer Rope. If costs favor small actors: GDPR is Rope or Scaffold. The ''hidden beneficiary'' problem: does GDPR help small competitors or harm them?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_cost_concentration, empirical, 'Whether GDPR compliance costs create consolidation subsidy for large platforms').

omega_variable(
    enforcement_coverage_asymmetry,
    'Are GDPR enforcement actions concentrated on high-profile targets (mega-platforms) while systematic violations by smaller actors go undetected and unpunished?',
    'Audit of DPA case files: distribution of fines by firm size, sector, and violation type; ratio of detected violations to estimated actual violations; investigation capacity analysis (how many FTEs per 1M residents)',
    'If enforcement is highly asymmetric: regulation becomes theater (Piton) — it punishes visible bad actors but leaves systematic extraction unaddressed. If enforcement is balanced: regulation is genuine Tangled Rope. Theater ratio rises as asymmetry increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_coverage_asymmetry, empirical, 'Whether GDPR enforcement is concentrated on high-profile targets').

omega_variable(
    privacy_tech_displacement_timeline,
    'Can privacy-preserving technologies (differential privacy, federated learning, zero-knowledge proofs) mature fast enough to make centralized data collection uncompetitive within the scaffold''s sunset window (10-15 years)?',
    'Roadmap analysis of privacy-tech deployment: tracking adoption rates in production systems, cost curves for differential privacy, federated learning accuracy parity with centralized ML; market analysis of privacy-tech startups vs. data-collection platforms',
    'If tech matures fast: Scaffold is realistic, sunset clause is viable, constraint transitions from Tangled Rope toward obsolescence. If tech stalls: Scaffold is aspirational (false sunset), constraint remains Tangled Rope or Snare indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(privacy_tech_displacement_timeline, empirical, 'Whether privacy-tech can mature fast enough to displace centralized data collection').

omega_variable(
    dark_pattern_regulatory_cat_and_mouse,
    'Does GDPR enforcement against dark patterns (cookie banner manipulation, consent-nag redesigns) actually constrain manipulation, or does it drive innovation in regulatory arbitrage (finding loopholes, jurisdictional shopping)?',
    'Longitudinal UI/UX analysis of consent mechanisms: tracking how designs change post-enforcement; comparison of manipulation techniques across jurisdictions (GDPR vs. non-GDPR); analysis of whether enforcement triggers new workarounds faster than regulation can adapt',
    'If enforcement constrains dark patterns: suppression value (0.65) is realistic. If dark patterns persist or evolve: suppression is lower (0.45-0.50), theater ratio higher (0.65+), constraint becomes more Snare-like. Mandatrophy outcome: ''does regulation prevent extraction or just relocate it?''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dark_pattern_regulatory_cat_and_mouse, empirical, 'Whether GDPR enforcement constrains dark patterns or drives regulatory arbitrage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(data_privacy_regulation, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dpr_tr_t0, data_privacy_regulation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(dpr_tr_t3, data_privacy_regulation, theater_ratio, 3, 0.48).
narrative_ontology:measurement(dpr_tr_t6, data_privacy_regulation, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(dpr_be_t0, data_privacy_regulation, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(dpr_be_t3, data_privacy_regulation, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(dpr_be_t6, data_privacy_regulation, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(data_privacy_regulation, enforcement_mechanism).
narrative_ontology:affects_constraint(data_privacy_regulation, platform_data_monetization).
narrative_ontology:affects_constraint(data_privacy_regulation, regulatory_arbitrage_jurisdictional_shopping).
narrative_ontology:affects_constraint(data_privacy_regulation, consumer_surveillance_technology).

% DUAL FORMULATION NOTE:
% This story focuses on GDPR as an enforcement and compliance mechanism (ε=0.52). A complementary story would address the underlying coordination problem (core privacy principle, ε≈0.15, Mountain) that GDPR attempts to solve. The present constraint is downstream of that core principle and exhibits extraction via implementation; the upstream principle is the genuine natural law of information asymmetry at scale.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(data_privacy_regulation, moderate, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
