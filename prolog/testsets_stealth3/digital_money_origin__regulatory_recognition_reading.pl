% ============================================================================
% CONSTRAINT STORY: digital_money_origin__regulatory_recognition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__regulatory_recognition_reading, []).

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
 *   constraint_id: digital_money_origin__regulatory_recognition_reading
 *   human_readable: Regulatory Recognition Gate for Digital Money (Official-Incorporation Reading)
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This reading of the digital_money_origin kernel holds that digital money
 *   entered institutional existence when monetary authorities formally
 *   incorporated it - rewriting statistical aggregates to capture electronic
 *   balances and building licensing regimes (e-money directives,
 *   payment-services frameworks) around its issuers. The story is therefore
 *   about the standing arrangement that reading takes as constitutive: the
 *   recognition gate, the machinery that decides which instruments count as
 *   money, who may issue them, and on what terms. That gate carries a genuine
 *   coordination function (common measures, prudential coverage,
 *   interoperability) and, through the same structure, shelters incumbents
 *   while taxing or excluding outsiders - hence the tangled_rope claim. The
 *   epsilon referent is fixed to the recognition-gate arrangement itself,
 *   assessed by this reading's own lights; it is not the arrangement any
 *   sibling reading would endorse. Claim and metrics are authored
 *   independently: the type claim comes from the structure (coordination plus
 *   asymmetric incidence plus active enforcement), the metric values from the
 *   arrangement's observable operation. KEY AGENTS (by structural
 *   relationship):
 *
 * KEY AGENTS:
 *   - monetary_authorities: agenda-setting definer ([institutional]/[identity_locked]) - drafts the definitions, licenses issuers, publishes the aggregates; identity fused with the defining role
 *   - licensed_commercial_banks: primary beneficiary ([institutional]/[mobile]) - collects deposit migration and moat protection
 *   - incumbent_card_networks: secondary beneficiary ([institutional]/[mobile]) - entry costs screen would-be settlement rivals
 *   - unlicensed_fintech_innovators: primary payer ([moderate]/[constrained]) - bears licensing, capital, and compliance costs or exits
 *   - community_currency_issuers: peripheral payer ([powerless]/[trapped]) - instruments unrecognized, schemes exposed to inquiry
 *   - cryptocurrency_ecosystem_builders: excluded outsider ([organized]/[arbitrage]) - locked out of official status, built parallel rails
 *   - retail_digital_wallet_users: dual-positioned user ([moderate]/[constrained]) - receives protections, pays indirect costs
 *   - licensed_emoney_institutions: absorbed former entrant ([institutional]/[constrained]) - paid the toll, now defends the perimeter
 *   - international_standards_bodies: template-setter ([institutional]/[analytical]) - drafts the harmonized manuals others transpose
 *   - academic_monetary_economists: analytical observer ([analytical]/[analytical]) - interprets the boundary, cites and is cited
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, 0.64).
domain_priors:suppression_score(digital_money_origin__regulatory_recognition_reading, 0.74).
domain_priors:theater_ratio(digital_money_origin__regulatory_recognition_reading, 0.43).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, theater_ratio, 0.43).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__regulatory_recognition_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__regulatory_recognition_reading, "Regulatory Recognition Gate for Digital Money (Official-Incorporation Reading)").
narrative_ontology:topic_domain(digital_money_origin__regulatory_recognition_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__regulatory_recognition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__regulatory_recognition_reading, '29b553bb-84ed-4196-aab8-468007d5b58e').
narrative_ontology:cs_kernel_codification('29b553bb-84ed-4196-aab8-468007d5b58e', formalized).
narrative_ontology:cs_authority_grounding('29b553bb-84ed-4196-aab8-468007d5b58e', expertise).
narrative_ontology:cs_interpretation_layer_present('29b553bb-84ed-4196-aab8-468007d5b58e').
narrative_ontology:cs_reading_relation('29b553bb-84ed-4196-aab8-468007d5b58e', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('29b553bb-84ed-4196-aab8-468007d5b58e', digital_money_origin__first_held_reading, influences).
narrative_ontology:cs_axiom('29b553bb-84ed-4196-aab8-468007d5b58e', foundational, official_incorporation_constitutes_money_status).
narrative_ontology:cs_axiom_status(official_incorporation_constitutes_money_status, holdable).
narrative_ontology:cs_axiom_grounding('29b553bb-84ed-4196-aab8-468007d5b58e', official_incorporation_constitutes_money_status, conventional).
narrative_ontology:cs_axiom('29b553bb-84ed-4196-aab8-468007d5b58e', secondary, prudential_control_requires_perimeter_membership).
narrative_ontology:cs_axiom_status(prudential_control_requires_perimeter_membership, holdable).
narrative_ontology:cs_axiom_grounding('29b553bb-84ed-4196-aab8-468007d5b58e', prudential_control_requires_perimeter_membership, instrumental).
narrative_ontology:cs_reference_frame('29b553bb-84ed-4196-aab8-468007d5b58e', official_recognition_constitutes_monetary_existence).
narrative_ontology:cs_drift_state('29b553bb-84ed-4196-aab8-468007d5b58e', contemporary_stablecoin_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('29b553bb-84ed-4196-aab8-468007d5b58e', '').
narrative_ontology:cs_kernel_id(digital_money_origin__regulatory_recognition_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, licensed_commercial_banks).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, incumbent_card_networks).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, unlicensed_fintech_innovators).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, community_currency_issuers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, retail_digital_wallet_users).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, licensed_emoney_institutions).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, retail_digital_wallet_users).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, licensed_emoney_institutions).
narrative_ontology:constraint_vindicates(digital_money_origin__regulatory_recognition_reading, official_statistics_money_definition_doctrine).
narrative_ontology:constraint_vindicates(digital_money_origin__regulatory_recognition_reading, prudential_perimeter_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central banks and supervisory agencies draft the legal definitions and statistical manuals that decide which instruments count as money; they license or refuse issuers, set capital and safeguarding requirements, police the boundary, and publish the aggregates that anchor policy debate. Their institutional identity is fused with being the sole legitimate definer of the monetary category - abandoning that role would dissolve the office's core function. They collect data, supervision fees, and procedural deference, and carry the burden of defending the boundary whenever innovation presses on it.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, monetary_authorities, agenda_setter,
    institutional, generational, identity_locked, national).

% Hold charters inside the supervised perimeter and enjoy the resulting protections: deposit migration whenever a rival instrument is denied money status, privileged access to settlement accounts, and a decisive voice in consultations that set entry terms. Relocating charters or business lines across jurisdictions is routine for them.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, licensed_commercial_banks, beneficiary,
    institutional, biographical, mobile, global).

% Operate established payment rails whose regulatory standing predates the newest entrants. Each new licensing and capital requirement raises the fixed cost a would-be competing settlement layer must clear, while the networks' own compliance machinery is long since amortized. They can shift volume and domicile freely.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, incumbent_card_networks, beneficiary,
    institutional, biographical, mobile, global).

% Build wallet, remittance, and stored-value products and then discover that reaching scale lawfully means acquiring an e-money or payments license, posting capital and safeguarding funds, and absorbing continuous compliance overhead - or partnering with a chartered bank that takes a share of revenue. Those who cannot clear the bar pivot to software provision, sell to incumbents, or shut down.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unlicensed_fintech_innovators, payer,
    moderate, immediate, constrained, global).

% Run mutual-credit ledgers, time banks, and local exchange schemes whose units function as money for their members. The units appear in no official aggregate, members face friction declaring income denominated in them, and organizers have faced unlicensed-banking inquiries. The membership cannot relocate, so the scheme lives or dies inside one jurisdiction's discretion.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, community_currency_issuers, payer,
    powerless, generational, trapped, local).

% Developers, miners, and foundations maintaining permissionless settlement networks. Their instruments are invisible in official aggregates and their exchanges are periodic targets of enforcement and banking cutoffs, yet the networks keep running without anyone's permission. They were never invited into the consultation process that assigns money status; their response was to build rails that do not require it.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, cryptocurrency_ecosystem_builders, excluded,
    organized, generational, arbitrage, global).

% Use recognized wallets and accounts and receive the resulting protections: deposit recourse schemes, fraud liability rules, interoperable transfers. They pay indirectly through merchant fees passed on in prices and through slower product rollouts while compliance cycles complete. Switching providers is possible but bounded by network effects and employer or bank defaults.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, retail_digital_wallet_users, beneficiary,
    moderate, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__regulatory_recognition_reading, retail_digital_wallet_users, payer).

% Paid the licensing and capital toll years ago and now hold a defensible position inside the perimeter. Supervision costs continue, but the barrier that once threatened them now screens their successors. They lobby for rules proportional to their own size and defend the perimeter's necessity in consultations.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, licensed_emoney_institutions, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__regulatory_recognition_reading, licensed_emoney_institutions, payer).

% Committees at standard-setting organizations draft the harmonized manuals and recommendations that national authorities transpose: statistical definitions of money, prudential categories, and rules for novel value-transfer instruments. They set templates and observe outcomes; they do not bear enforcement costs themselves.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, international_standards_bodies, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__regulatory_recognition_reading, international_standards_bodies, observer).

% Study where the money boundary sits and how it moved, producing the histories and measurements that official bodies cite back. No operational stake in the boundary; their stake is interpretive.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, academic_monetary_economists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__regulatory_recognition_reading, licensed_commercial_banks).
narrative_ontology:fixing_cost_class(digital_money_origin__regulatory_recognition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Uniform statistical definitions of money and harmonized regulatory categories solve real problems: comparable measurement across jurisdictions, calibratable policy targets, prudential oversight of anything functioning as money, fraud and laundering controls, and interoperable payment infrastructure.
% TRANSFER_FUNCTION: Moves definitional authority and market access: the right to issue widely held digital money flows only through licensing; capital, safeguarding, and compliance costs flow from issuers to the supervisory apparatus; deposits migrate toward chartered banks whenever the perimeter tightens; transaction data flows upward to authorities.
% ABSENT_VOICES: Community-currency participants, open-source protocol developers, informal value-transfer operators serving migrant corridors, and unbanked users would object that money status should track social acceptance rather than official registration. They are outside the perimeter by design: consulted rarely, represented at most by filtered trade associations, or deliberately building parallel rails instead of petitioning for inclusion.
% DISAPPEARANCE_RATIONALE: If the recognition machinery vanished overnight, digital value transfer would continue but its legal treatment would fragment by jurisdiction; incumbents' definitional moat would erode within months as entrants issued freely; monetary statistics would lose their object of measurement; courts and tax authorities would improvise ad hoc treatments of every instrument. The arrangements of every named seat depend on the gate existing.
% FOUNDING_PROBLEM: As payments dematerialized from the late 1970s onward, the official map of money stopped matching the territory: electronic balances escaped the M-series definitions, and non-bank issuers of prepaid and stored-value instruments accumulated liabilities no supervisor watched. The apparatus was built to re-align the statistical map with actual monetary practice and to extend prudential oversight over non-bank issuers before they grew systemic.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested from outside the benefiting parties: fintech industry associations document the licensing burden as a live barrier (adverse-interest testimony), competition-authority entry-barrier studies and academic work on the e-money directives corroborate both the original map-territory gap and its recurrence with each new instrument wave (virtual currency, stablecoins), and central-bank consultation papers openly concede that new instruments outrun current definitions. No corroborator depends on the perimeter for its position except the supervisors themselves.
narrative_ontology:disappearance_verdict(digital_money_origin__regulatory_recognition_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__regulatory_recognition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__regulatory_recognition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_origin__regulatory_recognition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__regulatory_recognition_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__regulatory_recognition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__regulatory_recognition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.64 at interval end) reflects the decoupling between what the gate charges and what it costs to run: licensing pipelines, capital and safeguarding floors, and the deflection of entrants into revenue-sharing partnerships with chartered banks, alongside the deposit migration that follows every denial of money status to a rival instrument. Suppression (0.74) is authored as a raw structural property, unscaled by power or scope: operating outside the perimeter triggers banking-access denial, correspondent cutoffs, laundering exposure, and in the limit prosecution, and enforcement capacity visibly ratcheted across the interval (post-2001 surveillance finance rules, the 2000 e-money directive, PSD2-era licensing, 2019 travel-rule extension to virtual assets, stablecoin crackdowns) - hence the rising suppression_requirement series. Theater (0.43) is composed unevenly: the regulatory limb remains functional, while the statistical limb has grown increasingly ceremonial since major central banks abandoned aggregate targeting in the 1980s yet still maintain, announce, and revise the M-series - a maintained measurement shell whose operational load has decayed; consultation processes add performative weight. Accessibility_collapse (0.55) is partial: alternatives collapse completely for anyone who needs bank accounts and card rails at scale, but permissionless networks and offshore domiciles keep escape routes alive for the organized and the determined. Resistance (0.58) is real and partly effective: trade-association lobbying, litigation over master-account denial, state-level charter end-runs, and counter-institution building. The temporal series run on one shared grid (t=0..40, roughly 1980-2020, eight-unit steps); the trajectory is a stepped monotonic ratchet tied to identifiable events rather than a cycle, so no oscillation analysis is warranted. End-state values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute divergent types from identical structural data. From the agenda-setter seat the gate is the legitimate monetary order itself: the definer experiences boundary maintenance as prudence, and its fused identity makes exit unthinkable rather than merely costly. From the payer seats the same gate is a tollbooth whose fee is untethered from service cost. From the excluded seat it is an illegitimate closure - a claim that nothing exists until registered, contradicted daily by functioning parallel rails. The absorbed former-entrant seat (licensed e-money institutions) is the sharpest divergence case: identical nominal sector, opposite perimeter position versus unlicensed startups, everything hinging on the year of entry. Coalition dynamics among payers partially offset their weak individual positions - industry associations and litigation funds have extracted concessions - which is why suppression, however high, has not produced full capitulation. The engine computes these per-seat classifications from power, exit, and directionality data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (banks, card networks) sit near the beneficiary pole: the gate subsidizes them through moat and deposit migration, and their mobility lets them arbitrage the friendliest perimeter. Licensed e-money institutions are declared beneficiary-primary but carry a payer secondary role: they still fund supervision and absorb compliance cycles, so their true directional position is slightly above a pure collector - noted here rather than overridden, since the derivation from their dual declaration should capture it. Payers (unlicensed innovators, community currencies) derive high directional values from the victim declarations plus poor exit; the powerless-local-trapped combination pins community currencies nearest the full-target end despite their small footprint. Retail users sit near symmetric: protections received roughly balance fees and friction carried. The one explicit correction: cryptocurrency ecosystem builders are the story's sole organized-power agent and hold no victim-array slot, so the structural derivation would hand them the canonical fallback for organized actors - far too low. Their actual relationship is near-full target: excluded from the category the gate administers, periodically struck by its enforcement arm, surviving only through arbitrage-grade parallel rails. Hence the override for the organized power atom to 0.78. Monetary authorities, as agenda-setters with fused identity, derive near the beneficiary end and correctly so: the arrangement's principal yield - definitional sovereignty - accrues to them.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline cuts both ways here. Reading the arrangement as pure predation would erase the coordination half: uniform money measures and prudential coverage solved real problems, and jurisdictions without them developed the pathologies (shadow liabilities, unmeasured money stock) that prompted the founding acts. Reading it as pure coordination would erase the incidence data: the same manuals that enable measurement also draw the moat line, and the fee schedule tracks market power more closely than marginal cost. The founding problem - keeping the official map aligned with monetary practice as money dematerializes - is still live: each new instrument wave (electronic money, virtual currency, stablecoins) re-fires it, so no mandatrophy declaration is made. But the theater series flags an asymmetric decay inside the arrangement: the statistical limb is drifting toward ceremonial maintenance while the regulatory limb stays load-bearing. If that divergence widens, the honest resolution is decomposition into two stories - a functioning licensing gate and a maintained-but-unused measurement shell - linked by network edges, rather than a single averaged classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_origin_question_underdetermination,
    'Does the emergence question have a unique structural answer, or do the three readings of digital_money_origin measure three different constraints wearing one label?',
    'Cross-classification of the three sibling stories: persistent divergence in epsilon, beneficiary structure, and computed type across readings indicates a decomposed constraint family; convergence would indicate one constraint mislabeled thrice.',
    'Persistent divergence validates the family decomposition and the network links; convergence would collapse the family into a single disambiguated story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_origin_question_underdetermination, conceptual, 'Whether the kernel decomposes or the readings dispute one thing.').

omega_variable(
    incumbent_shaping_of_recognition_criteria,
    'Did incumbent banks materially shape the recognition criteria - threshold sizes, capital floors, aggregate definitions - through dominance of the consultation process?',
    'Consultation-response archives, lobbying disclosure records, and comparative study of jurisdictions where industry access to the drafting table differed.',
    'Confirmed shaping raises the agenda-setter seat''s effective directionality toward serving the beneficiaries and supports a harsher reading of the same structure; refuted shaping strengthens the coordination half.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_shaping_of_recognition_criteria, empirical, 'Capture hypothesis applied to the drafting of recognition criteria.').

omega_variable(
    cs_authority_framing_expertise_vs_extraction,
    'Is the adjudicating authority over the money-boundary kernel grounded in demonstrated competence (the expertise framing declared here) or in benefit from preventing kernel revision (an extraction framing)?',
    'Count instances where authorities revised aggregate definitions or entry criteria against incumbent interests when evidence warranted, versus evidence-resistant definitional maintenance; the ratio discriminates the framings.',
    'An extraction framing raises attributed gains at the agenda-setter seat and pushes per-seat classifications toward harder types; the expertise framing sustains the coordination reading of the same facts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_authority_framing_expertise_vs_extraction, conceptual, 'Commitment-system framing under-determination for this constraint.').

omega_variable(
    statistical_limb_operational_decay,
    'Do monetary aggregates remain load-bearing inputs to policy, or has the statistical half become ceremonial while only the regulatory half functions?',
    'Estimate policy reaction functions on money-growth versus interest-rate variables across the interval; audit internal supervisory use of aggregate forecasts versus their publication profile.',
    'If ceremonial, the theater_ratio is concentrated in the statistical limb and the arrangement is a candidate for decomposition into a functioning licensing gate plus an inertially maintained measurement shell.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statistical_limb_operational_decay, empirical, 'Whether the statistical half of the arrangement still does work.').

omega_variable(
    unrecognized_instrument_materiality_threshold,
    'At what scale of unrecognized circulation does the constitutive premise - nothing functions as money until officially incorporated - fail as a description of the world?',
    'Measure unrecognized stablecoin and settlement volumes actually used for payments and store of value; identify thresholds at which courts and tax authorities treat such instruments as money irrespective of official aggregates.',
    'Crossing the threshold converts drift from descriptive anomaly into premise failure, forcing the recognition reading to choose between revision and intensified enforcement against the unrecognized instruments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unrecognized_instrument_materiality_threshold, empirical, 'Materiality threshold at which the reading''s constitutive premise breaks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__regulatory_recognition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__regulatory_recognition_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(digi_tr_t0, observed).
narrative_ontology:measurement(digi_tr_t8, digital_money_origin__regulatory_recognition_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(digi_tr_t8, observed).
narrative_ontology:measurement(digi_tr_t16, digital_money_origin__regulatory_recognition_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement_basis(digi_tr_t16, observed).
narrative_ontology:measurement(digi_tr_t24, digital_money_origin__regulatory_recognition_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement_basis(digi_tr_t24, observed).
narrative_ontology:measurement(digi_tr_t32, digital_money_origin__regulatory_recognition_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(digi_tr_t32, observed).
narrative_ontology:measurement(digi_tr_t40, digital_money_origin__regulatory_recognition_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement_basis(digi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(digi_be_t0, observed).
narrative_ontology:measurement(digi_be_t8, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement_basis(digi_be_t8, observed).
narrative_ontology:measurement(digi_be_t16, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement_basis(digi_be_t16, observed).
narrative_ontology:measurement(digi_be_t24, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement_basis(digi_be_t24, observed).
narrative_ontology:measurement(digi_be_t32, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement_basis(digi_be_t32, observed).
narrative_ontology:measurement(digi_be_t40, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement_basis(digi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(digi_su_t0, observed).
narrative_ontology:measurement(digi_su_t8, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement_basis(digi_su_t8, observed).
narrative_ontology:measurement(digi_su_t16, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement_basis(digi_su_t16, observed).
narrative_ontology:measurement(digi_su_t24, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement_basis(digi_su_t24, observed).
narrative_ontology:measurement(digi_su_t32, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement_basis(digi_su_t32, observed).
narrative_ontology:measurement(digi_su_t40, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement_basis(digi_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__regulatory_recognition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__first_held_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'when did digital money emerge?' conflates three structurally distinct claims: conceptual feasibility (became_thinkable_reading), practical holding (first_held_reading), and official incorporation (this file). Per the epsilon-invariance principle they are authored as three stories with distinct epsilon values, distinct beneficiary/victim sets (thinkability-era claims implicate almost no one; recognition-era claims implicate incumbents and excluded innovators), and distinct failure modes, linked here via affects_constraints. Upstream/downstream structure: the feasibility reading is cited as a precondition by both later readings; recognition events reshape the holding conditions the middle reading treats as primary. This reading carries the latest origin date, a legal/regulatory-dominated constraint set, incumbent financial institutions as beneficiaries, and unregulated innovators as victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_origin__regulatory_recognition_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
