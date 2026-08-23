% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__consumer_holdings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__consumer_holdings_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__consumer_holdings_reading
 *   human_readable: Consumer-Holdings Boundary of Digital Money Emergence
 *   domain: economic/regulatory-history/technology-governance
 *
 * SUMMARY:
 *   In the consumer-holdings telling, digital money began when people could
 *   hold and spend electronic value themselves — the 1990s purse pilots,
 *   consummated legally by the 2000 Electronic Money Directive's issuer
 *   category. The arrangement this story is ABOUT is the resulting boundary:
 *   a legal-statistical line asserting that consumer-held electronic value
 *   constitutes digital money while bank-internal electronic transfer does
 *   not, instantiated in the directive category, the deposits-versus-e-money
 *   seam in the monetary aggregates, and the licensing regime wrapped around
 *   issuance. The boundary solves a real problem — ungoverned stored value,
 *   issuer failure, statistical blindness — and simultaneously distributes
 *   position: its authors collect category authority and fee income, licensed
 *   issuers receive a protected charter with scale-favoring fixed costs,
 *   sub-scale issuers meet a compliance wall, the Chaum-tradition designers
 *   of anonymous bearer cash find their design space closed, banks carry
 *   redefinition burdens alongside an entry option, and users trade anonymity
 *   for guarantees. Per the ε-invariance principle this is one of three
 *   structurally distinct stories sharing the colloquial label 'digital money
 *   emergence'; the siblings are separate files with their own ε, linked
 *   through network.affects_constraints. ε's referent is this standing
 *   arrangement — the holdings boundary as enacted and maintained — assessed
 *   by this reading's own lights, never the arrangement a sibling reading
 *   would draw. Claim and metrics are independent: the tangled_rope claim
 *   states the structure judged true; the metrics describe observed operation
 *   without tuning toward any predicted engine verdict. KEY AGENTS (by
 *   structural relationship): - emi_ecb_monetary_authorities: agenda-setter
 *   and declared beneficiary (institutional/arbitrage) — drafted the 1994
 *   definition report, enacted the category in 2000, licenses and supervises
 *   issuers, maintains the deposits/e-money statistical seam, collects fees
 *   and jurisdictional authority, and can redraw the perimeter -
 *   fintech_e_money_issuers: declared beneficiary with payer costs
 *   (organized/constrained) — hold the charter the category confers; pay
 *   authorization, capital, and safeguarding for lawful issuance and
 *   passporting - major_wallet_platforms: beneficiary at scale
 *   (powerful/arbitrage) — take the largest share of the protected channel;
 *   redomicile freely across perimeters - small_prepaid_issuers: declared
 *   victim (moderate/constrained) — bear the same fixed compliance stack per
 *   unit of tiny volume; consolidate or shrink beneath thresholds -
 *   incumbent_commercial_banks: declared victim with beneficiary offset
 *   (institutional/mobile) — rebuilt statistics and absorbed competition,
 *   while holding the cheapest entry route into the category -
 *   anonymous_bearer_instrument_designers: declared victim
 *   (powerless/identity_locked) — Chaum-tradition builders of unlinkable
 *   digital cash; no lawful issuance route under the category; absent from
 *   the drafting tables - retail_stored_value_users: beneficiary with payer
 *   costs (powerless/constrained) — receive redemption guarantees and choice;
 *   surrender anonymity, accept identification, carry fees -
 *   monetary_historians: analytical observer (analytical/analytical) — see
 *   all three rival datings and how each relocates benefit and burden
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, 0.65).
domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, 0.6).
domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__consumer_holdings_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__consumer_holdings_reading, "Consumer-Holdings Boundary of Digital Money Emergence").
narrative_ontology:topic_domain(digital_money_emergence_boundary__consumer_holdings_reading, "economic/regulatory-history/technology-governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__consumer_holdings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__consumer_holdings_reading, 'd5ba1a3e-469f-4e14-9e3c-598d4ed0662c').
narrative_ontology:cs_kernel_codification('d5ba1a3e-469f-4e14-9e3c-598d4ed0662c', formalized).
narrative_ontology:cs_authority_grounding('d5ba1a3e-469f-4e14-9e3c-598d4ed0662c', expertise).
narrative_ontology:cs_interpretation_layer_present('d5ba1a3e-469f-4e14-9e3c-598d4ed0662c').
narrative_ontology:cs_reading_relation('d5ba1a3e-469f-4e14-9e3c-598d4ed0662c', digital_money_emergence_boundary__conceptualization_reading, forecloses).
narrative_ontology:cs_reading_relation('d5ba1a3e-469f-4e14-9e3c-598d4ed0662c', digital_money_emergence_boundary__infrastructure_reading, forecloses).
narrative_ontology:cs_axiom('d5ba1a3e-469f-4e14-9e3c-598d4ed0662c', foundational, digital_money_exists_only_when_directly_held_outside_bank_accounts).
narrative_ontology:cs_axiom_status(digital_money_exists_only_when_directly_held_outside_bank_accounts, holdable).
narrative_ontology:cs_axiom_grounding('d5ba1a3e-469f-4e14-9e3c-598d4ed0662c', digital_money_exists_only_when_directly_held_outside_bank_accounts, conventional).
narrative_ontology:cs_axiom('d5ba1a3e-469f-4e14-9e3c-598d4ed0662c', secondary, monetary_statistics_must_separate_e_money_from_deposits).
narrative_ontology:cs_axiom_status(monetary_statistics_must_separate_e_money_from_deposits, holdable).
narrative_ontology:cs_axiom_grounding('d5ba1a3e-469f-4e14-9e3c-598d4ed0662c', monetary_statistics_must_separate_e_money_from_deposits, conventional).
narrative_ontology:cs_reference_frame('d5ba1a3e-469f-4e14-9e3c-598d4ed0662c', direct_consumer_holdings_constitute_money).
narrative_ontology:cs_drift_state('d5ba1a3e-469f-4e14-9e3c-598d4ed0662c', post_stablecoin_legislation_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('d5ba1a3e-469f-4e14-9e3c-598d4ed0662c', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, emi_ecb_monetary_authorities).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, fintech_e_money_issuers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, incumbent_commercial_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, anonymous_bearer_instrument_designers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, small_prepaid_issuers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, major_wallet_platforms).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, incumbent_commercial_banks).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, retail_stored_value_users).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, fintech_e_money_issuers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, retail_stored_value_users).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__consumer_holdings_reading, holdings_constitute_existence_thesis).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__consumer_holdings_reading, m4_m5_separation_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produced the 1994 report that defined stored-value cards as a distinct monetary phenomenon, carried the category into law through the 2000 Electronic Money Directive, and maintains the statistical separation between bank deposits and e-money balances in the monetary aggregates. Licenses and supervises issuers, collects supervisory fees, and can amend the category's perimeter through legislation and regulation. Exit is effectively unlimited: as author of the category they can redraw, narrow, or dissolve the boundary, and their analytical apparatus predates and outlives any particular definition.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, emi_ecb_monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, continental).

% Firms whose business model consists of issuing consumer-held electronic value under the category the directive created. The charter gives them lawful issuance, cross-border passporting, and a lighter prudential regime than a banking license; they pay authorization fees, capital requirements, safeguarding obligations, and recurring reporting for it. Leaving the category means converting to a credit institution, selling the book, or winding down — the charter is simultaneously their asset and their tether.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, fintech_e_money_issuers, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__consumer_holdings_reading, fintech_e_money_issuers, payer).

% Large payment platforms that obtained e-money or banking charters in whichever jurisdiction offered the most favorable perimeter and then passport services across borders. They take the largest share of the protected issuance channel because fixed compliance costs favor scale, and they can relocate headquarters or restructure corporate domicile when a national regime turns unfavorable, as several did by redomiciling to accommodating jurisdictions.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, major_wallet_platforms, beneficiary,
    powerful, biographical, arbitrage, global).

% Sub-scale issuers of gift, transit, and closed-loop-adjacent prepaid products for whom authorization, capital, and safeguarding overheads are a fixed cost independent of volume. The same compliance stack a large platform amortizes over millions of transactions falls on them per product; many consolidated, sold to larger platforms, or narrowed their ranges to stay beneath category thresholds. Exiting means abandoning issuance entirely; there is no smaller license to retreat to.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, small_prepaid_issuers, payer,
    moderate, biographical, constrained, national).

% Had to rebuild statistical reporting to split deposit balances from e-money floats, absorb supervisory examination of new product lines, and watch licensed non-banks compete for payment flows previously theirs. They also hold the cheapest entry ticket into the category — a credit institution may issue e-money by notification rather than full authorization — and several run their own purse and wallet products, so the arrangement taxes them and admits them at once.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, incumbent_commercial_banks, payer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__consumer_holdings_reading, incumbent_commercial_banks, beneficiary).

% Cryptographers and engineers building offline-capable, blinding-based digital cash in the Chaum tradition, whose instruments are bearer-like and unlinked by design. The category as enacted recognizes prepaid, identified, redeemable-at-par value; anonymous bearer issuance has no lawful route in the jurisdictions that adopted it, and the community's founding commitments — unlinkability as a property of cash itself — make adopting the identified paradigm tantamount to abandoning the project. Principal firms failed before the category existed; the remainder publish, prototype, and litigate at the margins. They had no seat in the working groups that drafted the definition.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, anonymous_bearer_instrument_designers, payer,
    powerless, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__consumer_holdings_reading, anonymous_bearer_instrument_designers, excluded).

% People who top up prepaid cards and app wallets with everyday spending balances. They gain redemption guarantees if an issuer fails, dispute rights, and a wide choice of interchangeable issuers; they give up cash-style anonymity, accept identification at onboarding, and carry dormancy and service fees on small balances. Switching issuers is easy, but every lawful digital option inside the category shares the same identified, supervised shape, and stepping outside it means giving up legal protection altogether.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, retail_stored_value_users, beneficiary,
    powerless, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__consumer_holdings_reading, retail_stored_value_users, payer).

% Study the sequence of claims about when digital money began — theory, rails, consumer holdings — and can see that each dating relocates who benefits from and who bears the costs of the category built on it. They publish periodizations, advise standard-setters occasionally, and hold no stake in which boundary prevails beyond disciplinary argument.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__consumer_holdings_reading, major_wallet_platforms).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__consumer_holdings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authorized definition of consumer-held electronic value so that issuance can be permitted, supervised, counted, and redeemed at par across jurisdictions; separates e-money balances from bank deposits in monetary statistics; converts issuer-failure risk into a governed, guaranteed exposure.
% TRANSFER_FUNCTION: Moves authorization fees, capital, safeguarding obligations, and reporting labor from issuers to the supervisory system, with the fixed-cost share falling hardest on sub-scale issuers; moves market access and charter rents to licensed issuers, concentrated at scale; moves jurisdictional authority and fee income to the monetary authorities; moves identification data and fee burdens from users to issuers and supervisors.
% ABSENT_VOICES: Anonymous-bearer instrument designers and privacy advocates were not consulted in the EMI working groups or the directive's committee passages; ordinary cash users entered only through consumer-body proxies; closed-loop operators below the thresholds learned of the perimeter after enactment. All three would contest the definition's fit, and their objections survive in the margins of consultation records rather than in the text.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, issuers would lose lawful standing, floats would stand unguarded against issuer failure, monetary statistics would lose the deposits/e-money seam, and cross-border passports would void; the wallet economy would have to reconstitute authorization from scratch, and every actor's position — including the authorities' — is organized around the category existing.
% FOUNDING_PROBLEM: In the early 1990s, consumer-held electronic purses proliferated with no common legal treatment: issuer failure left unredeemed balances unprotected, no supervisory framework existed for non-bank stored value, monetary statistics were blind to prepaid floats, and it was uncertain whether non-banks could lawfully issue electronic value at all.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting set: incumbent commercial banks' consultation responses to the directive reviews and to the payment-services revisions attest that supervision of consumer-held balances remains necessary — banks compete with the beneficiaries and do not share their interest. BIS and FSB work on stablecoin oversight (2019–2021) independently attests the same need for a new instrument generation. National consumer-agency files document pre-directive balance-loss episodes in failed purse and voucher schemes. Academic monetary economists corroborate the statistical need for separating e-money from deposits while disputing the stronger existential reading. No source outside the category's own authors corroborates that the boundary DATE is a natural fact — that claim rests on the authorities who drew it.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__consumer_holdings_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__consumer_holdings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__consumer_holdings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores are authored from the arrangement's observed operation, independently of the tangled_rope claim. Extractiveness 0.65: the category converts issuer-failure risk into a supervised, guaranteed product (a real service) while charging authorization fees, capital, and safeguarding overhead whose fixed component falls regressively on sub-scale issuers, concentrating the protected channel at scale; the boundary's authors also collect fee income and jurisdictional authority from the category's existence. Suppression 0.60 is an unscaled structural property: since 2000, issuing consumer-held electronic value without authorization is unlawful across the adopting bloc, anonymous bearer issuance has no lawful route, and the remaining alternatives — operating below thresholds as non-money, non-adopting jurisdictions, or instruments outside the category such as later crypto-assets — are constrained but not closed. Theater 0.50: the licensing layer remains functional, but the statistical layer has partially atrophied — operational use of the aggregate framework receded after the 2003 abandonment of the M3 reference value, leaving a growing share of aggregate-maintenance activity ceremonial while compliance reporting grew formulaic. Accessibility collapse 0.42: understanding the perimeter collapses the 'issue stored value freely' alternative but leaves crypto, closed-loop, and offshore routes partly open. Resistance 0.58: the issuer lobby won material concessions in the 2009 directive revision, banks contested the perimeter throughout, and the crypto movement constitutes a standing refusal of the category itself. Receipt surface: the demonstrable pecuniary capture of the arrangement's rents concentrates at the scaled-platform seat, hence gain_flow names major_wallet_platforms; the authorities' gain is jurisdictional rather than pecuniary, and small issuers pay net. Fixing — dissolving or radically redrawing the category — is prohibitive for its author: unwinding charters, voiding passports, breaking statistical series, and reopening uninsured issuer-failure exposure outweigh any benefit a redrawing could collect. All three series share one eight-point grid (1990–2026) so no metric row is sampled against another's gaps; 2026 points are marked projected. The trajectories are monotone rather than cyclical: extraction accumulates, theater rises with statistical atrophy, and the suppression series shows enforcement build-up (pre-directive near-zero to directive machinery) followed by a post-2018 plateau at mature supervision — the plateau, not an oscillation, is the dynamic to read.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the boundary as public-interest category-building — risk converted into supervision, statistics made complete. The same structure reads differently from the seats bearing its costs: sub-scale issuers meet a compliance wall sized for platforms; anonymous-instrument designers meet the closure of their design space; banks meet a charge that also sells them an entry ticket. Because exit grades differ sharply inside one industry — platform arbitrage versus small-issuer captivity versus designer identity-lock — seats at nominally similar standing classify the arrangement differently. The engine computes that per-seat divergence from power, exit, and role data; this story only declares the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries anchor the low-d pole; declared victims anchor the high-d pole. Major wallet platforms sit nearest the beneficiary end: they take the protected channel and hold arbitrage-grade relocation options. Small prepaid issuers sit high despite belonging to the same industry: identical rules, opposite exit grades — their captivity, not their product, sets their position. Anonymous-bearer designers sit near the full-target end, amplified by identity lock: the commitment that makes their instruments what they are is the same commitment the category excludes, so exit would dissolve the agent's project. Retail users are near-symmetric: guarantees received against identification and fees paid. Incumbent banks are dual-declared (payer with a beneficiary secondary role); their net position leans payer because redefinition programs, supervisory load, and licensed-non-bank encroachment outweigh the notification-route entry option, while their mobility keeps them off the trapped end. The authorities' own d stays low: they wrote the perimeter and can redraw it, which is the strongest subsidy a boundary can pay its author. Suppression stays unscaled in this accounting — only extractiveness is amplified or damped by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ungoverned proliferation of consumer-held electronic value, issuer-failure exposure, statistical blindness — remains live in transformed form: it returned with app wallets and is returning with stablecoin issuance, which is why the category keeps absorbing new instrument classes rather than atrophying. The tangled_rope classification blocks two mislabels: reading the licensing regime as pure extraction ignores the trust function that made pan-European consumer e-money insurable and legible; reading it as pure coordination ignores the regressive fixed costs, the foreclosed anonymous-design space, and the category-authority rents. Drift risk is layered: the statistical layer shows classic Goodhart atrophy (theater rising toward half), while the licensing layer shows mandate renewal rather than decay — this is not a zombie arrangement; it is a mandate that keeps finding new objects. The R5 interview agrees: status live, verdict world_rearranges — no mismatch flag, and the corroboration comes from competitors and international standard-setters, not from the benefiting seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality_of_emergence_date,
    'This constraint is the consumer_holdings_reading of the kernel digital_money_emergence_boundary; how would instantiating a sibling reading change the classification?',
    'Generate the conceptualization_reading and infrastructure_reading as separate stories with their own ε and beneficiary sets, then compare computed types and family topology across the three.',
    'Sibling adoption re-dates the boundary by decades, relocates beneficiaries (bank consortia and processors under the rails reading; cryptographers and theorists under the conceptualization reading), deletes the M4/M5 separation necessity, and rewires the influence edges among the family members.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexicality_of_emergence_date, conceptual, 'Kernel-reading membership: the emergence date, beneficiary structure, and statistical consequences are indexical to this reading.').

omega_variable(
    constructed_vs_natural_boundary,
    'Is the consumer-holdings boundary a discovered feature of what money is, or a regulatory construction that serves the category-defining authorities and the licensed issuers?',
    'Comparative historiography across jurisdictions that never enacted the directive category, and counterfactual analysis of whether monetary-history dating would differ absent the statistical and supervisory demand for a holdings-based category.',
    'If constructed, the boundary''s apparent fixity dissolves and the arrangement stands fully as an enforced regulatory construct; if natural, the boundary approaches mountain-like stability and the extraction measured here is parasitic on a genuine natural kind.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_natural_boundary, empirical, 'Natural-kind versus regulatory-construction status of the holdings boundary.').

omega_variable(
    anonymous_cash_death_cause,
    'Was the disappearance of anonymous bearer digital cash (Chaum-line ecash, privacy-capable purses) caused by regulatory foreclosure under the category, or by market failure preceding it?',
    'Timing and natural-experiment analysis: the principal anonymous schemes failed before the 2000 directive, yet anonymous issuance remained unlawful afterward and still is; compare non-adopting jurisdictions and subsequent anonymous-instrument attempts.',
    'If market failure dominates, the suppression measure overstates the boundary''s coercive force; if regulation dominates, the boundary suppressed a competing design paradigm and the effective coercion is higher than the structural measure shows.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(anonymous_cash_death_cause, empirical, 'Attribution of anonymous-instrument extinction between market and regulatory causes.').

omega_variable(
    m_aggregate_functional_status,
    'Do the M4/M5 separations this reading necessitates still perform a live policy function, or are they maintained ceremonially?',
    'Textual-use analysis of post-2003 monetary-policy publications: frequency and decision-weight of aggregate references versus surface maintenance of the statistical tables.',
    'If vestigial, the theater trajectory continues upward and the statistical layer drifts toward inertial maintenance; if functional, theater is bounded and the separation earns its upkeep.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m_aggregate_functional_status, empirical, 'Live-function versus ceremonial status of the holdings-based statistical separation.').

omega_variable(
    boundary_absorption_of_stablecoins,
    'Will the consumer-holdings boundary absorb the stablecoin and app-wallet wave by extending the category, or will those instruments bypass it?',
    'Track classification outcomes under recent crypto-asset and e-money-token legislation: whether issuers seek the e-money charter, contest it, or route around it.',
    'Absorption confirms revival of this reading''s reference frame and extends its enforcement surface; bypass would mark the beginning of a fourth reading and erode this one''s claim to be the operative boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_absorption_of_stablecoins, empirical, 'Future-perimeter question: absorption versus bypass of post-prepaid consumer-held instruments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__consumer_holdings_reading, 1990, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1990, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement_basis(digi_tr_t1990, observed).
narrative_ontology:measurement(digi_tr_t1994, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1994, 0.2).
narrative_ontology:measurement_basis(digi_tr_t1994, observed).
narrative_ontology:measurement(digi_tr_t2000, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement_basis(digi_tr_t2000, observed).
narrative_ontology:measurement(digi_tr_t2007, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2007, 0.33).
narrative_ontology:measurement_basis(digi_tr_t2007, observed).
narrative_ontology:measurement(digi_tr_t2010, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement_basis(digi_tr_t2010, observed).
narrative_ontology:measurement(digi_tr_t2018, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2018, 0.44).
narrative_ontology:measurement_basis(digi_tr_t2018, observed).
narrative_ontology:measurement(digi_tr_t2023, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2023, 0.48).
narrative_ontology:measurement_basis(digi_tr_t2023, observed).
narrative_ontology:measurement(digi_tr_t2026, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2026, 0.5).
narrative_ontology:measurement_basis(digi_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(digi_be_t1990, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement_basis(digi_be_t1990, observed).
narrative_ontology:measurement(digi_be_t1994, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1994, 0.32).
narrative_ontology:measurement_basis(digi_be_t1994, observed).
narrative_ontology:measurement(digi_be_t2000, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement_basis(digi_be_t2000, observed).
narrative_ontology:measurement(digi_be_t2007, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2007, 0.54).
narrative_ontology:measurement_basis(digi_be_t2007, observed).
narrative_ontology:measurement(digi_be_t2010, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement_basis(digi_be_t2010, observed).
narrative_ontology:measurement(digi_be_t2018, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2018, 0.61).
narrative_ontology:measurement_basis(digi_be_t2018, observed).
narrative_ontology:measurement(digi_be_t2023, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2023, 0.63).
narrative_ontology:measurement_basis(digi_be_t2023, observed).
narrative_ontology:measurement(digi_be_t2026, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2026, 0.65).
narrative_ontology:measurement_basis(digi_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1990, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement_basis(digi_su_t1990, observed).
narrative_ontology:measurement(digi_su_t1994, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1994, 0.18).
narrative_ontology:measurement_basis(digi_su_t1994, observed).
narrative_ontology:measurement(digi_su_t2000, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement_basis(digi_su_t2000, observed).
narrative_ontology:measurement(digi_su_t2007, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2007, 0.52).
narrative_ontology:measurement_basis(digi_su_t2007, observed).
narrative_ontology:measurement(digi_su_t2010, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement_basis(digi_su_t2010, observed).
narrative_ontology:measurement(digi_su_t2018, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2018, 0.62).
narrative_ontology:measurement_basis(digi_su_t2018, observed).
narrative_ontology:measurement(digi_su_t2023, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2023, 0.6).
narrative_ontology:measurement_basis(digi_su_t2023, observed).
narrative_ontology:measurement(digi_su_t2026, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2026, 0.6).
narrative_ontology:measurement_basis(digi_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__consumer_holdings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__infrastructure_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'digital money emergence' decomposes into three structurally distinct claims per the ε-invariance principle: a thinkability claim (conceptualization_reading), a rails claim (infrastructure_reading), and a consumer-holdings claim (this story). Each carries its own ε, beneficiary set, and classification; no single story can span them without making ε observer-relative. Citation order runs upstream to downstream: the conceptualization literature supplies the formal models the purses implemented, and the rails-era infrastructure is cited as the enabling background of the 1990s purse wave — so the upstream stories influence this one without being entailed by it. Sibling files carry reciprocal edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
