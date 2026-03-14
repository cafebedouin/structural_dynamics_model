% ============================================================================
% CONSTRAINT STORY: journalistic_accountability_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_journalistic_accountability_gap, []).

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
 *   constraint_id: journalistic_accountability_gap
 *   human_readable: Journalistic Accountability Gap: Asymmetric Verification and Correction Costs
 *   domain: media/epistemology/institutional_accountability
 *
 * SUMMARY:
 *   The journalistic accountability gap describes a structural asymmetry
 *   between the ease and speed of publishing claims and the difficulty and
 *   cost of retracting or correcting them. This constraint operates across
 *   multiple institutional and individual levels, creating a hybrid
 *   extraction mechanism embedded in what is nominally a coordination system
 *   (journalism's role in informing the public). The gap manifests as: (1)
 *   publication costs asymmetry — publishing a false claim is cheap and
 *   instant; correction is expensive, slow, and reaches 5-10% of original
 *   audience; (2) reputational cost distribution — costs fall primarily on
 *   individual subjects and the public epistemic commons rather than
 *   institutions; (3) legal liability shielding — most false claims fall
 *   below the threshold for economically significant legal action; (4)
 *   algorithmic amplification — social media platforms often preferentially
 *   amplify original claims over corrections. The constraint exhibits all six
 *   DR types from different structural positions, making it diagnostic for
 *   how institutional incentive structures embed extraction in nominally
 *   coordinate functions.
 *
 * KEY AGENTS:
 *   - News Organizations: Primary beneficiary (institutional/arbitrage) — capture audience, engagement, and advertising value from speed prioritization; bear minimal correction costs
 *   - Individual Journalists: Secondary beneficiary (moderate/constrained) — gain prestige and audience attention from breaking news; risk reputational cost for errors
 *   - Falsely Accused Individuals: Primary victim (powerless/trapped) — suffer permanent reputational damage; face high costs to sue; corrections reach tiny fraction of original audience
 *   - Public Epistemic Commons: Primary victim (powerless/trapped) — abstract collective good degraded by false claims; no self-correction mechanism scales to match publication speed
 *   - Fact-Checking Community: Institutional actor (moderate/constrained) — benefits from authority gap requiring verification; constrained by resource limits
 *   - Editorial Standards Bodies: Institutional actor (institutional/arbitrage) — maintain codes of ethics without enforcement; performative accountability
 *   - Open Source Verification Networks: Organized agents (organized/mobile) — building alternative pathways with lower theater and sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing publication-correction asymmetry as inherent cognitive law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(journalistic_accountability_gap, 0.52).
domain_priors:suppression_score(journalistic_accountability_gap, 0.58).
domain_priors:theater_ratio(journalistic_accountability_gap, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(journalistic_accountability_gap, extractiveness, 0.52).
narrative_ontology:constraint_metric(journalistic_accountability_gap, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(journalistic_accountability_gap, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(journalistic_accountability_gap, tangled_rope).
narrative_ontology:human_readable(journalistic_accountability_gap, "Journalistic Accountability Gap: Asymmetric Verification and Correction Costs").
narrative_ontology:topic_domain(journalistic_accountability_gap, "media/epistemology/institutional_accountability").

domain_priors:requires_active_enforcement(journalistic_accountability_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(journalistic_accountability_gap, news_organizations).
narrative_ontology:constraint_beneficiary(journalistic_accountability_gap, prominent_journalists).
narrative_ontology:constraint_victim(journalistic_accountability_gap, misinformation_subjects).
narrative_ontology:constraint_victim(journalistic_accountability_gap, public_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FALSELY ACCUSED INDIVIDUAL (SNARE) — Cannot exit the reputational damage; bears permanent extraction from a false story with asymmetric correction costs. No practical recourse for ordinary people against institutional media power. Maximum suppression: retractions are buried, corrections reach 5-10% of original audience, legal remedies are expensive and slow.
constraint_indexing:constraint_classification(journalistic_accountability_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PUBLIC EPISTEMIC COMMONS (SNARE) — Abstract collective good. False claims contaminate the information environment; corrections propagate poorly; cumulative effect degrades shared reality. No correction mechanism scales to match publication speed. Trapped by structural asymmetry: publication is instant, correction is expensive.
constraint_indexing:constraint_classification(journalistic_accountability_gap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: FACT-CHECKING COMMUNITY (TANGLED ROPE) — Constrained by resource limits (underfunded relative to media output) but also benefits from the accountability gap: fact-checkers gain authority and funding from being verifiers. Genuine coordination function (creating verification standards) embedded in asymmetric extraction (checking institutions without reciprocal accountability).
constraint_indexing:constraint_classification(journalistic_accountability_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: NEWS ORGANIZATION MANAGEMENT (ROPE) — Experiences the constraint as coordination: publishing breaking news creates audience, engagement, and advertising value. Corrections are optional, costly, and less profitable than original story. Net beneficiary with high exit optionality: can choose publication speed over accuracy, arbitrage between speed and truth.
constraint_indexing:constraint_classification(journalistic_accountability_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EDITORIAL STANDARDS AND CODES OF ETHICS (PITON) — Professional journalism codes (SPJ, AP) prescribe verification, corrections, and accountability. But enforcement is performative: peer review of journalism is weak, professional sanctions are rare, reputational costs are absorbed by individuals rather than institutions. Theater ratio rises as institutions invoke codes without implementing enforcement mechanisms.
constraint_indexing:constraint_classification(journalistic_accountability_gap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY AND LEGAL ACCOUNTABILITY MOVEMENTS (TANGLED ROPE) — Organized actors (defamation law reformers, media regulation advocates) see the gap as a hybrid coordination-extraction problem: media needs verification standards (coordination) but resists accountability mechanisms (extraction resistance). Constrained by free press doctrine limiting regulatory reach, but pushing for transparency requirements and correction obligations.
constraint_indexing:constraint_classification(journalistic_accountability_gap, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: OPEN SOURCE VERIFICATION PLATFORMS (SCAFFOLD) — Organized agents (NewsGuard, First Draft News, Bellingcat, local verification networks) are building alternative verification pathways. These platforms have mobile exit options (can redirect resources, change methods) and lower theater than traditional editorial oversight. Sunset logic: as distributed verification becomes routine, centralized institutional accountability mechanisms become less essential. Temporary support role as old systems transition to new.
constraint_indexing:constraint_classification(journalistic_accountability_gap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (NATURAL LAW VIEW) (MOUNTAIN) — From civilizational scale, publication-correction asymmetry is inherent to human cognition: false claims propagate faster than corrections (backfire effect, illusory truth effect, cognitive fluency). This perspective sees the gap as an immutable law of information physics. However, structural data suggests this is a false summit: the extractiveness arises from institutional choices (profit incentives, publication speed prioritization) rather than cognitive necessity. The 'natural law' framing naturalizes contingent extractive design.
constraint_indexing:constraint_classification(journalistic_accountability_gap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(journalistic_accountability_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(journalistic_accountability_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(journalistic_accountability_gap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(journalistic_accountability_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(journalistic_accountability_gap, TR),
    TR >= 0.70.

:- end_tests(journalistic_accountability_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts value through: (1) career/prestige benefits to journalists from publication speed over accuracy; (2) audience/advertising benefits to institutions from sensational false claims; (3) temporal asymmetry giving early claimants attention advantage over later correctors; (4) correction costs borne by subjects and fact-checkers rather than publishers. Not as severe as pure snare (0.66+) because institutional reputation does sustain some cost and some false claims are eventually corrected. The value increased from 0.28 at start of interval to 0.52 currently, reflecting institutional prioritization of speed over accuracy and platform algorithmic amplification of false claims over corrections. Suppression (0.58): Moderate-high. Multiple barriers prevent correction: (1) economic: correction is expensive (staff time, no audience draw); (2) psychological: backfire effect makes corrections counterproductive if aggressive; (3) legal: defamation suits require proof of intent, high cost barrier; (4) structural: no mechanism forces proportional correction distribution; (5) platform: algorithms deprioritize corrections. Theater ratio (0.64): Moderate-high. Editorial ethics codes, fact-checking partnerships, and correction sections create performative accountability: they appear in professional standards and newspaper homepages but rarely scale to match original claim distribution or reach. The theater increased from 0.42 to 0.64 as institutions adopted visible accountability mechanisms without implementing enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates striking perspectival divergence. News organization management experiences rope (coordination) — publishing and correcting stories is their coordination mechanism. Individual journalists experience tangled rope — genuine coordination function in informing public, embedded in extraction through speed incentives and reputational distribution. Fact-checkers experience tangled rope — genuine need for verification standards, embedded in extraction through resource asymmetry. The falsely accused and public epistemic commons experience snare — pure extraction with no coordination benefit. Editorial codes experience piton — professional standards persist through institutional inertia without enforcement. Open source verification networks experience scaffold — temporary role as traditional accountability fails, with sunset as distributed verification becomes routine. The natural law perspective (analytical/civilizational) risks wrongly classifying the publication-correction asymmetry as inherent to human cognition rather than as a contingent institutional choice. The perspectival gap reveals that 'journalism' is not a single constraint but multiple constraints layered: a coordination function (informing public) with an extraction mechanism (speed/accuracy tradeoff) embedded in it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across agents. News institutions experience low d (beneficiaries with arbitrage exit options) — they can choose publication speed, can delay or skip corrections, can arbitrage between speed and truth. Individual journalists experience moderate d (moderate power, constrained exits) — prestige and employment depend on publication records; can publish quickly but face reputational costs for egregious errors. Fact-checkers experience moderate-high d (moderate power, constrained by resources) — benefit from verification gap but constrained by inability to scale response. Falsely accused and epistemic commons experience high d (powerless/trapped) — cannot exit the contaminated information environment, bear full cost of false claim with no reciprocal benefit. The directionality pipeline assigns low f(d) to beneficiaries and high f(d) to victims, producing asymmetric effective extractiveness chi. News institutions perceive low extraction; targets perceive very high extraction. The gap is not perceptual disagreement but structural: chi genuinely differs by agent position.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint embeds extraction (snare/tangled rope from victim positions) within a nominally coordinate function (journalism's truth-telling role). The analytical observer risks wrongly classifying the entire system as mountain (inherent to cognition/information physics), naturalizing what is actually a institutional design choice. The resolution: journalism is a genuine coordination mechanism (rope from some perspectives) but with an embedded extraction mechanism (snare from victim perspectives). The extractive component arises from institutional incentive structures (publish speed, profit from engagement, minimal correction cost), not from cognitive necessity. The mandatrophy is fully resolved by the perspectival divergence: the system is coordinate AND extractive simultaneously, experienced differently by different agents. The false summit (mountain classification) is exposed by the fact that open source verification platforms (scaffold perspective) are successfully building alternative pathways with lower extractiveness — if the asymmetry were a natural law, alternative systems would reproduce it. Instead, alternatives show that extractiveness is contingent on institutional design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    correction_reach_asymmetry_mechanism,
    'Is the 5-10% correction reach an inherent cognitive property or a result of incentive misalignment in media distribution?',
    'Comparison of correction propagation rates in institutional vs decentralized media; analysis of active correction promotion vs passive correction placement; experimental trials of equal-effort distribution for corrections vs original claims',
    'If inherent: correction gap is partially unavoidable and extraction is partly structural unavoidability. If incentive-driven: gap is deliberate design choice and extraction is purely institutional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(correction_reach_asymmetry_mechanism, empirical, 'Whether low correction reach is cognitive or institutional').

omega_variable(
    journalistic_reputation_versus_institutional_reputation,
    'Does reputational cost fall primarily on individual journalists or on news institutions, and does this distribution affect correction incentives?',
    'Analysis of career trajectory impact (do journalists with error records face persistent consequences?); institutional branding analysis (do news organizations suffer persistent reputational damage from published falsehoods?); case studies of high-profile retractions and career outcomes',
    'If cost falls on individuals: institutional accountability is weak and extraction is protected at institutional level. If cost falls on institutions: extraction is partially self-correcting through market pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(journalistic_reputation_versus_institutional_reputation, empirical, 'Distribution of reputational consequences for journalistic errors').

omega_variable(
    legal_liability_threshold_effect,
    'At what scale of false claim does legal liability (defamation, libel) become economically significant relative to reputational correction cost?',
    'Comparative analysis of settlement costs vs reputational damage; threshold determination for different claim categories (false accusations vs statistical claims vs public figure allegations); litigation rate vs published false claims rate',
    'If threshold is high: most extraction is shielded from legal accountability and suppression is high. If threshold is low: legal liability creates deterrent and extraction is limited.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_liability_threshold_effect, empirical, 'Legal liability threshold relative to extraction magnitude').

omega_variable(
    platform_algorithmic_amplification_feedback,
    'Do social media algorithms preferentially amplify original false claims over corrections, creating structural incentive misalignment independent of journalist choice?',
    'Comparative analysis of amplification metrics (reach, engagement, algorithmic ranking) for identical false claim vs correction pairs; temporal analysis of viral trajectory; platform transparency reports on correction visibility',
    'If algorithms amplify false over true: extraction is partly externally driven by platform design and journalistic institutions are partly captured. If symmetric amplification: extraction is primarily journalistic choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_algorithmic_amplification_feedback, empirical, 'Algorithmic amplification asymmetry between claims and corrections').

omega_variable(
    corrective_disclosure_cost_externalization,
    'Who bears the economic cost of fact-checking infrastructure: news institutions (internalized cost) or fact-checkers and platforms (externalized cost)?',
    'Budget analysis of in-house fact-checking teams vs independent fact-checker organizations; funding source analysis; correlation between news organization size and correction rate',
    'If externalized: news institutions extract verification benefit without bearing verification cost, deepening asymmetry. If internalized: institutions bear accountability cost and extraction is limited.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corrective_disclosure_cost_externalization, empirical, 'Cost allocation for journalistic accountability infrastructure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(journalistic_accountability_gap, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jag_tr_t0, journalistic_accountability_gap, theater_ratio, 0, 0.42).
narrative_ontology:measurement(jag_tr_t8, journalistic_accountability_gap, theater_ratio, 8, 0.53).
narrative_ontology:measurement(jag_tr_t16, journalistic_accountability_gap, theater_ratio, 16, 0.64).
narrative_ontology:measurement(jag_tr_t4, journalistic_accountability_gap, theater_ratio, 4, 0.48).
narrative_ontology:measurement(jag_tr_t12, journalistic_accountability_gap, theater_ratio, 12, 0.59).

% Extraction over time
narrative_ontology:measurement(jag_be_t0, journalistic_accountability_gap, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(jag_be_t8, journalistic_accountability_gap, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(jag_be_t16, journalistic_accountability_gap, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(jag_be_t4, journalistic_accountability_gap, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(jag_be_t12, journalistic_accountability_gap, base_extractiveness, 12, 0.46).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(journalistic_accountability_gap, information_standard).
narrative_ontology:affects_constraint(journalistic_accountability_gap, misinformation_propagation_asymmetry).
narrative_ontology:affects_constraint(journalistic_accountability_gap, epistemic_commons_degradation).
narrative_ontology:affects_constraint(journalistic_accountability_gap, defamation_legal_liability_threshold).

% DUAL FORMULATION NOTE:
% The journalistic accountability gap is upstream of specific false claims (which have their own constraint stories) but represents a distinct structural constraint on correction mechanisms. Decomposition: (1) publication-correction_cost_asymmetry (ε≈0.48, this story) coordinates with (2) algorithmic_amplification_of_false_claims (ε≈0.55) and (3) legal_liability_shielding (ε≈0.38). Each has distinct extractiveness but all three create the observed 5-10% correction reach.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(journalistic_accountability_gap, institutional, 0.12).
constraint_indexing:directionality_override(journalistic_accountability_gap, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
