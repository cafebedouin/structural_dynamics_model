% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__capital_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__capital_supremacy_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__capital_supremacy_reading
 *   human_readable: Treaty Text as Supreme Law Over Domestic Regulation — Capital Mobility as Mandatory Obligation (Capital-Supremacy Reading)
 *   domain: international trade law / political economy / regulatory federalism
 *
 * SUMMARY:
 *   A tri-national trade agreement's investment and market-access chapters
 *   operate — under the capital-supremacy understanding instantiated by this
 *   story — as a supreme-law layer: domestic regulatory measures touching
 *   foreign investors or traded-goods flows can be brought before treaty
 *   arbitration, struck down, diluted, or compensated against, while capital
 *   crosses the three jurisdictions as a treaty-mandated right and regulatory
 *   harmonization functions as an obligation rather than an option. The
 *   standing arrangement this story prices runs from ratification (T0, mapped
 *   to 1994) to the renegotiation settlement (TN, mapped to 2020). KEY AGENTS
 *   (by structural relationship): multinational_investors
 *   (powerful/arbitrage) — primary beneficiary seat, compensation and
 *   disciplinary leverage flow to them; export_oriented_producers
 *   (organized/mobile) — secondary beneficiaries of harmonized access;
 *   household_consumers (moderate/constrained) — dual-positioned, cheap goods
 *   in, eroded protections and tribunal payouts out;
 *   isds_tribunals_and_arbitration_complex (institutional/constrained) —
 *   administers enforcement and bills for it; national_trade_ministries
 *   (institutional/generational) — set and defend the commitments;
 *   domestic_regulatory_agencies (institutional/identity_locked) — primary
 *   bearer of jurisdictional loss, drafting standards they must pre-dilute;
 *   organized_labor_sectors (organized/trapped) and
 *   small_nonmobile_domestic_firms (moderate/trapped) — bear adjustment and
 *   compliance costs with no mobility offset; subnational_governments
 *   (organized/trapped) — lose instruments they never countersigned;
 *   civil_society_opposition_networks (organized/constrained) — excluded from
 *   operative decision channels; academic_trade_policy_analysts
 *   (analytical/analytical) — observe. Claim and metrics are authored
 *   independently: the arrangement's own tradition presents it as necessary
 *   coordination, while the authored metrics price its actual asymmetric
 *   operation; the engine computes each seat's classification from the
 *   structural data, and any divergence from the claim is signal, not error.
 *
 * KEY AGENTS:
 *   - multinational_investors: primary beneficiary (powerful/arbitrage) — collects compensation, protection, and disciplinary leverage
 *   - export_oriented_producers: secondary beneficiary (organized/mobile) — harmonized market access
 *   - household_consumers: dual-positioned (moderate/constrained) — price gains against protection erosion
 *   - isds_tribunals_and_arbitration_complex: enforcement administrator and fee collector (institutional/constrained)
 *   - national_trade_ministries: agenda setter (institutional/generational) — negotiates, defends, absorbs tribunal losses
 *   - domestic_regulatory_agencies: primary bearer of jurisdictional loss (institutional/identity_locked)
 *   - organized_labor_sectors: cost-bearing seat without mobility offset (organized/trapped)
 *   - small_nonmobile_domestic_firms: compliance-cost bearers without arbitrage (moderate/trapped)
 *   - subnational_governments: bound without consent (organized/trapped)
 *   - civil_society_opposition_networks: excluded voice (organized/constrained)
 *   - academic_trade_policy_analysts: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.74).
domain_priors:suppression_score(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.7).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "Treaty Text as Supreme Law Over Domestic Regulation — Capital Mobility as Mandatory Obligation (Capital-Supremacy Reading)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "international trade law / political economy / regulatory federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, '4ce8d55a-8e18-4836-8b8b-11549c2785c8').
narrative_ontology:cs_kernel_codification('4ce8d55a-8e18-4836-8b8b-11549c2785c8', fixed_text).
narrative_ontology:cs_authority_grounding('4ce8d55a-8e18-4836-8b8b-11549c2785c8', lineage).
narrative_ontology:cs_interpretation_layer_present('4ce8d55a-8e18-4836-8b8b-11549c2785c8').
narrative_ontology:cs_reading_relation('4ce8d55a-8e18-4836-8b8b-11549c2785c8', nafta_jurisdictional_boundary__embedded_liberalism_reading, forecloses).
narrative_ontology:cs_reading_relation('4ce8d55a-8e18-4836-8b8b-11549c2785c8', nafta_jurisdictional_boundary__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('4ce8d55a-8e18-4836-8b8b-11549c2785c8', foundational, treaty_text_overrides_domestic_regulatory_jurisdiction).
narrative_ontology:cs_axiom_status(treaty_text_overrides_domestic_regulatory_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('4ce8d55a-8e18-4836-8b8b-11549c2785c8', treaty_text_overrides_domestic_regulatory_jurisdiction, conventional).
narrative_ontology:cs_axiom('4ce8d55a-8e18-4836-8b8b-11549c2785c8', foundational, capital_mobility_mandatory_treaty_obligation).
narrative_ontology:cs_axiom_status(capital_mobility_mandatory_treaty_obligation, holdable).
narrative_ontology:cs_axiom_grounding('4ce8d55a-8e18-4836-8b8b-11549c2785c8', capital_mobility_mandatory_treaty_obligation, instrumental).
narrative_ontology:cs_reference_frame('4ce8d55a-8e18-4836-8b8b-11549c2785c8', treaty_text_supreme_law_baseline).
narrative_ontology:cs_drift_state('4ce8d55a-8e18-4836-8b8b-11549c2785c8', usmca_renegotiation_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4ce8d55a-8e18-4836-8b8b-11549c2785c8', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_investors).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, export_oriented_producers).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, household_consumers).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, isds_tribunals_and_arbitration_complex).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, organized_labor_sectors).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, small_nonmobile_domestic_firms).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, subnational_governments).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, household_consumers).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_dispute_resolution_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, regulatory_competition_efficiency_thesis).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, market_integration_irreversibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploys fixed capital across the three member states. Treaty investment chapters convert adverse regulatory changes into compensable events, and arbitration has delivered damages and measure withdrawals in their favor. Shapes negotiating agendas through business advisory councils. Because production can shift among jurisdictions, the threat of relocation disciplines host regulation even between formal disputes; that mobility is the leverage underlying their position.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_investors, beneficiary,
    powerful, generational, arbitrage, global).

% Sells into a single harmonized market instead of dozens of fragmented national rule-sets: predictable certification, reduced border frictions, uniform technical requirements. Bears little of the sovereignty cost that their market access rides on, and can relocate plants if any member's cost profile turns unfavorable.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, export_oriented_producers, beneficiary,
    organized, biographical, mobile, continental).

% Buys cheaper traded goods as tariff and regulatory-variance costs fall. Also absorbs the other side: thinner inspection capacity behind product safety, diluted environmental protections around resource projects, and public compensation payouts funded from tax revenue when tribunals award foreign investors. Cannot exit the consumption basket or the jurisdiction.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, household_consumers, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__capital_supremacy_reading, household_consumers, payer).

% Arbitrators, specialist counsel, and dispute institutions decide which domestic measures survive treaty challenge, case by case, through interpretive rulings that now define what the chapters effectively mean. Collects filing fees, arbitrator honoraria, and counsel billings that exist only because the dispute pipeline runs. Their practice has no existence outside the enforcement system they administer.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, isds_tribunals_and_arbitration_complex, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__capital_supremacy_reading, isds_tribunals_and_arbitration_complex, beneficiary).

% Negotiated the commitments and now administer and defend them domestically against parliamentary and civil-society challenge. Inherits tribunal losses as budget-line compensation payments. Cannot loosen any commitment unilaterally; adjustment requires trilateral renegotiation, which took years and conceded only a narrow labor annex.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, national_trade_ministries, agenda_setter,
    institutional, generational, constrained, national).

% Draft toxics, food-safety, energy, and procurement standards. Increasingly pre-clear proposals against anticipated tribunal liability, and have withdrawn or diluted measures after investor challenges — pesticide restrictions, hazardous-waste site permits, fuel-additive bans among the documented cases. Cannot exit the treaty's jurisdiction, and their organizational self-concept is constituted through the regulatory mission now being overridden, so they adapt by self-censoring rather than confronting.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_regulatory_agencies, payer,
    institutional, generational, identity_locked, national).

% Absorb wage and standards competition as production relocates toward the lowest-standard member. The labor side-agreement grants petition channels whose findings bind no one. Collective-bargaining coverage and employment in exposed manufacturing regions decline while the workforce stays geographically fixed.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, organized_labor_sectors, payer,
    organized, biographical, trapped, regional).

% Shoulder harmonization compliance costs — certification, reporting, process upgrades — sized for large-firm balance sheets, while lacking any ability to arbitrage jurisdictions the way multinationals do. Face intensified import competition under lowered barriers. Firm-specific assets and local ties anchor them in place.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, small_nonmobile_domestic_firms, payer,
    moderate, biographical, trapped, local).

% Provincial, state, and municipal governments legislate health, environment, and procurement measures that federal trade commitments sweep into treaty scope. Their instruments can be challenged as treaty violations without their having consented at any negotiating table. Territorially fixed; their objections register only through federal intermediaries.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, subnational_governments, payer,
    organized, generational, trapped, regional).

% Environmental coalitions, fair-trade networks, and mass movements demanded binding labor and environmental enforcement at ratification and again at renegotiation. Secured only consultative side-agreements and petition mechanisms with no sanction power; their substantive demands remain outside the operative chapters. Present at the margins of the conversation, unheard at its core.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, civil_society_opposition_networks, excluded,
    organized, biographical, constrained, continental).

% Code tribunal outcomes, document regulatory-chill episodes, and track the migration of jurisdiction and compensation flows. Produce the win-rate and chill evidence that every other seat argues over.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, academic_trade_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_investors).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__capital_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the cross-border credibility problem: investors will not commit fixed capital where political turnover can bring expropriation or arbitrary regulatory reversal, and traders will not build integrated supply chains across dozens of inconsistent national rule-sets. A treaty-level supremacy clause converts discretionary sovereign acts into contractually bounded ones and gives the member states one stable rule-set instead of a web of bilateral bargains.
% TRANSFER_FUNCTION: Moves regulatory jurisdiction and policy-setting authority upward, from domestic legislatures and agencies to treaty institutions and arbitration panels. Moves compensation payments from host-state treasuries to foreign investors upon award. Distributes market-access rents toward mobile capital and export sectors, paid for by non-mobile factors and by foregone regulatory options.
% ABSENT_VOICES: Domestic environmental and labor constituencies and subnational governments had no seat at the negotiating table, which drew its participants from trade ministries and business advisory councils. Civil society obtained post-hoc petition channels with no enforcement teeth. The populations bearing chill costs generally learned the chapters' reach only after ratification; they would object now from inside every affected regulatory domain.
% DISAPPEARANCE_RATIONALE: Overnight removal would re-fragment the continental market: investors would demand country-by-country risk premia and insurance, cross-border production networks would reprice within quarters, settled arbitration precedents governing thousands of contracts would evaporate, regulatory agencies would immediately exercise recovered jurisdiction and standards would diverge, and the arbitration industry would lose its case pipeline. Too many standing arrangements depend on the structure for the world to stay put.
% FOUNDING_PROBLEM: Late-twentieth-century volatility in host-state treatment of foreign investment — expropriations, abrupt regulatory reversals, contract repudiation — plus the transaction-cost chaos of fragmented national market rules blocking scale economies for emerging cross-border production networks.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists from outside the benefiting parties, on both halves. Host-state finance officials and development-economics literature attest the expropriation-risk problem was real (the historical record of nationalizations and contract repudiation is extensive). Labor federations, environmental NGOs, and a substantial body of trade-law scholarship attest from outside the beneficiary set that the narrow insurance problem never required blanket supremacy over ordinary domestic regulatory standards — that the supremacy extension outran the founding problem is their consistent finding.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__capital_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__capital_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__capital_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.74) because the arrangement transfers jurisdiction and compensation liability upward while the coordination service it renders — investment-credibility insurance and a single rule-set for market access — is real but priced far above its cost: tribunal win rates and documented measure withdrawals exceed what expropriation insurance alone would require. Suppression (0.70) is structural, not interpersonal: treaty lock-in, renegotiation ratchets, and liability exposure foreclose regulatory alternatives; roughly seventy percent of the suppressive force is external machinery (chapter obligations, tribunal reach, retaliation risk) and thirty percent internalized as pre-emptive self-censorship habituated inside regulatory agencies. Theater (0.38) concentrates in the labor/environment side-agreements — petition channels producing findings that bind no one — and in consultation rituals, while core arbitration remains functional. Accessibility collapse is moderate (0.58): alternatives survive as carve-outs, exceptions, and the costly exit of denunciation, so the structure does not present as natural law. Resistance (0.60) is real and recurring: ratification-era mobilization, the mid-interval anti-globalization peak, renegotiation fights, and a wider wave of investment-treaty terminations. Enforcement capacity ratcheted monotonically upward across the interval (suppression_requirement 0.52 to 0.70) as tribunal caseload and interpretive reach matured; the single shared time grid covers all tracked metrics at every point, mapping to 1994–2020. Suppression is authored as a raw structural property — engine-side scaling applies only to extractiveness. Coalition capacity exists among the cost-bearing seats: labor, civil society, and sympathetic legislators converted combined pressure during renegotiation into the one place the structure conceded (a labor-enforcement annex), evidence that the victim set is not inert. Claimed type is tangled_rope: genuine coordination and asymmetric extraction ride the same chapters, and the structure requires active enforcement to hold — all three conditions authored as structurally true, independently of predicted engine output.
 *
 * PERSPECTIVAL GAP:
 *   Seats should classify differently. From the multinational-investor and export-producer seats the chapters are insurance and subsidy — low effective extraction, coordination-framed. From the regulatory-agency seat the same chapters are jurisdictional dispossession backed by liability — high effective extraction, enforcement-framed. Organized labor and small non-mobile firms experience the costs without the mobility option that cushions capital, so their seats compute harsher than the investor seat despite identical nominal treaty membership. The arbitration complex experiences the arrangement as neutral adjudication — its professional frame insulates it even though its fee income depends on dispute volume. Household consumers straddle: net price gains against diffuse protection erosion. Subnational governments experience a representation gap: bound by commitments they never countersigned. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation. Multinational investors (declared beneficiary, arbitrage exit) sit nearest the beneficiary pole; export producers close behind. Household consumers appear in both arrays — beneficiary of price effects, victim of protection erosion and payout exposure — so their derived directionality lands near symmetric rather than at either pole; no override is authored because the dual declaration expresses the symmetry directly. Domestic regulatory agencies (victims, identity_locked) sit near the full-target end: identity lock amplifies their effective extraction because they cannot shed the mission being overridden — were the agencies' identity frame to break and the mission be redefined as treaty-compliance management, their exit would soften toward constrained and their resistance would migrate to legislatures. Labor, small firms, and subnational governments are trapped targets with no arbitrage. The arbitration complex declares fee income (beneficiary) alongside administrative control (agenda-setter), so its derived directionality blends collection with administration. No directionality overrides were needed: beneficiary/victim declarations plus exit atoms reproduce the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both mislabelings. Reading the arrangement as pure extraction erases the genuine service: cross-border investment did face arbitrary-expropriation risk, and a credibility instrument solves a real problem — corroborated by treasury and development-economics sources outside the beneficiary set. Reading it as pure coordination erases the transfer: the same chapters that insure against expropriation sweep ordinary regulatory discretion into compensable events, which no narrow insurance scheme requires. The tangled-rope classification keeps both faces legible on one structure. On obsolescence: the narrow insurance problem remains live, but the parties dispute whether it ever required blanket supremacy over domestic standards — hence founding_problem_status 'contested' rather than 'dead', asserting no zombie flag. Theatrical maintenance is confined to the side-agreement layer (theater_ratio 0.38) rather than the enforcement core; the enforcement core still does what it was built to do, which is precisely what makes the extraction durable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the capital_supremacy_reading of kernel nafta_jurisdictional_boundary; the sibling readings embedded_liberalism_reading and sovereignty_primacy_reading instantiate different constraints over the same standing arrangement. What exactly hangs on the disagreement?',
    'Locate the disputed structural element: the supremacy clause''s scope (categorical vs conditional vs subordinate) and the enumeration of domestic instruments inside treaty jurisdiction. Comparative analysis of which measures tribunals accept into scope under each reading''s preferred doctrine.',
    'Under the embedded-liberalism sibling, labor/environment standards leave the victim set and epsilon drops; under the sovereignty-primacy sibling, regulatory agencies recover agenda-setting authority and the enforcement machinery demotes to a coordination protocol. Per-seat classifications recompute accordingly; this file''s values hold only for the capital-supremacy instantiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of one kernel; sibling readings are separate constraints, not parts of this one.').

omega_variable(
    capital_mobility_naturalness,
    'Is capital mobility, as this reading''s tradition presents it, an invariant feature of modern economies (natural-law presentation), or a policy construct sustained by treaty guarantee and enforcement?',
    'Comparative institutional analysis: investment-flow behavior across capital-control regimes and across dyads that terminated investment treaties, versus treaty-bound dyads.',
    'If mobility is a construct, the arrangement''s persistence depends on maintained enforcement rather than economic necessity, and any natural-law presentation of it is a false summit — classification shifts from fixed-feature framings toward constructed, enforcement-dependent types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_mobility_naturalness, empirical, 'Whether the reading''s naturalization of capital mobility survives contact with cross-regime evidence.').

omega_variable(
    regulatory_chill_attribution,
    'How much of the observed regulatory withdrawal and pre-emptive self-censorship is attributable to tribunal enforcement specifically, versus the generic credible-commitment prudence any investor-protection regime would produce?',
    'Difference-in-differences across treaty-membership boundaries and across corridors later exempted from the investment chapter (the post-renegotiation US-Canada corridor offers a natural comparison).',
    'Separates enforced extraction (a driver of effective extraction) from baseline coordination cost (floor-side); a large enforced component supports the high-extraction reading of the metrics, a small one supports the coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chill_attribution, empirical, 'Attribution of documented chill between enforcement machinery and generic commitment effects.').

omega_variable(
    tribunal_neutrality_question,
    'Are tribunal outcomes systematically biased toward investor interests beyond selection effects and procedural noise?',
    'Blinded coding of award rates by measure type, respondent identity, and bench composition across a full case population.',
    'Demonstrated bias confirms enforcement operating as capture machinery (pushing the structure toward the extraction-dominated end of the spectrum); demonstrated neutrality supports the adjudication framing and the tangled classification''s coordination half.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tribunal_neutrality_question, empirical, 'Whether the enforcement layer adjudicates neutrally or operates as a captured mechanism.').

omega_variable(
    interpretive_layer_variability,
    'Effective jurisdictional reach is now set by tribunal interpretations rather than by the founding text — does realized extraction track the interpretation layer''s evolution rather than the codified chapters?',
    'Correlate extraction-relevant rulings and interpretive trends with text-revision history; isolate periods where text was static while reach expanded.',
    'If ruling-driven, drift concentrates in the interpretation layer beneath a fixed kernel — reform levers shift from renegotiation toward appointment and interpretive review, and the codified-kernel structure''s brittleness becomes the operative risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_layer_variability, empirical, 'Whether the constraint''s operative content lives in the fixed text or in its interpretive overlay.').

omega_variable(
    coercion_grid_level_uncertainty,
    'The authored coercion-grid values at the class and individual levels rest on conservative judgments where direct measurement is thin; how far do the level-resolved gradients deviate from these bounds?',
    'Sector-level wage and employment panels linked to treaty exposure, plus survey-based individual exposure and opposition-participation data across the interval.',
    'Refinement could steepen or flatten the level gradient; OPEN-track consumers should treat class-level and individual-level values as bounds pending panel data, and the structural/organizational rows are the load-bearing ones for this story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_grid_level_uncertainty, empirical, 'Measurement uncertainty attached to the leveled grid''s lower levels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 0, 26).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(naft_tr_t0, observed).
narrative_ontology:measurement(naft_tr_t4, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 4, 0.23).
narrative_ontology:measurement_basis(naft_tr_t4, observed).
narrative_ontology:measurement(naft_tr_t9, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 9, 0.27).
narrative_ontology:measurement_basis(naft_tr_t9, observed).
narrative_ontology:measurement(naft_tr_t13, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 13, 0.3).
narrative_ontology:measurement_basis(naft_tr_t13, observed).
narrative_ontology:measurement(naft_tr_t17, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 17, 0.33).
narrative_ontology:measurement_basis(naft_tr_t17, observed).
narrative_ontology:measurement(naft_tr_t21, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 21, 0.36).
narrative_ontology:measurement_basis(naft_tr_t21, observed).
narrative_ontology:measurement(naft_tr_t26, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 26, 0.38).
narrative_ontology:measurement_basis(naft_tr_t26, observed).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(naft_be_t0, observed).
narrative_ontology:measurement(naft_be_t4, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement_basis(naft_be_t4, observed).
narrative_ontology:measurement(naft_be_t9, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 9, 0.63).
narrative_ontology:measurement_basis(naft_be_t9, observed).
narrative_ontology:measurement(naft_be_t13, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 13, 0.66).
narrative_ontology:measurement_basis(naft_be_t13, observed).
narrative_ontology:measurement(naft_be_t17, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 17, 0.69).
narrative_ontology:measurement_basis(naft_be_t17, observed).
narrative_ontology:measurement(naft_be_t21, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 21, 0.72).
narrative_ontology:measurement_basis(naft_be_t21, observed).
narrative_ontology:measurement(naft_be_t26, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 26, 0.74).
narrative_ontology:measurement_basis(naft_be_t26, observed).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(naft_su_t0, observed).
narrative_ontology:measurement(naft_su_t4, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 4, 0.56).
narrative_ontology:measurement_basis(naft_su_t4, observed).
narrative_ontology:measurement(naft_su_t9, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 9, 0.61).
narrative_ontology:measurement_basis(naft_su_t9, observed).
narrative_ontology:measurement(naft_su_t13, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 13, 0.64).
narrative_ontology:measurement_basis(naft_su_t13, observed).
narrative_ontology:measurement(naft_su_t17, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 17, 0.67).
narrative_ontology:measurement_basis(naft_su_t17, observed).
narrative_ontology:measurement(naft_su_t21, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 21, 0.69).
narrative_ontology:measurement_basis(naft_su_t21, observed).
narrative_ontology:measurement(naft_su_t26, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 26, 0.7).
narrative_ontology:measurement_basis(naft_su_t26, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=26
narrative_ontology:measurement(naft_grid_01, nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse(class), 0, 0.2).
narrative_ontology:measurement(naft_grid_02, nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse(class), 26, 0.34).
narrative_ontology:measurement(naft_grid_03, nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse(individual), 0, 0.1).
narrative_ontology:measurement(naft_grid_04, nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse(individual), 26, 0.16).
narrative_ontology:measurement(naft_grid_05, nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse(organizational), 0, 0.24).
narrative_ontology:measurement(naft_grid_06, nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse(organizational), 26, 0.6).
narrative_ontology:measurement(naft_grid_07, nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse(structural), 0, 0.3).
narrative_ontology:measurement(naft_grid_08, nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse(structural), 26, 0.58).
narrative_ontology:measurement(naft_grid_09, nafta_jurisdictional_boundary__capital_supremacy_reading, resistance(class), 0, 0.36).
narrative_ontology:measurement(naft_grid_10, nafta_jurisdictional_boundary__capital_supremacy_reading, resistance(class), 26, 0.58).
narrative_ontology:measurement(naft_grid_11, nafta_jurisdictional_boundary__capital_supremacy_reading, resistance(individual), 0, 0.1).
narrative_ontology:measurement(naft_grid_12, nafta_jurisdictional_boundary__capital_supremacy_reading, resistance(individual), 26, 0.18).
narrative_ontology:measurement(naft_grid_13, nafta_jurisdictional_boundary__capital_supremacy_reading, resistance(organizational), 0, 0.22).
narrative_ontology:measurement(naft_grid_14, nafta_jurisdictional_boundary__capital_supremacy_reading, resistance(organizational), 26, 0.3).
narrative_ontology:measurement(naft_grid_15, nafta_jurisdictional_boundary__capital_supremacy_reading, resistance(structural), 0, 0.32).
narrative_ontology:measurement(naft_grid_16, nafta_jurisdictional_boundary__capital_supremacy_reading, resistance(structural), 26, 0.54).
narrative_ontology:measurement(naft_grid_17, nafta_jurisdictional_boundary__capital_supremacy_reading, stakes_inflation(class), 0, 0.3).
narrative_ontology:measurement(naft_grid_18, nafta_jurisdictional_boundary__capital_supremacy_reading, stakes_inflation(class), 26, 0.46).
narrative_ontology:measurement(naft_grid_19, nafta_jurisdictional_boundary__capital_supremacy_reading, stakes_inflation(individual), 0, 0.12).
narrative_ontology:measurement(naft_grid_20, nafta_jurisdictional_boundary__capital_supremacy_reading, stakes_inflation(individual), 26, 0.2).
narrative_ontology:measurement(naft_grid_21, nafta_jurisdictional_boundary__capital_supremacy_reading, stakes_inflation(organizational), 0, 0.28).
narrative_ontology:measurement(naft_grid_22, nafta_jurisdictional_boundary__capital_supremacy_reading, stakes_inflation(organizational), 26, 0.74).
narrative_ontology:measurement(naft_grid_23, nafta_jurisdictional_boundary__capital_supremacy_reading, stakes_inflation(structural), 0, 0.35).
narrative_ontology:measurement(naft_grid_24, nafta_jurisdictional_boundary__capital_supremacy_reading, stakes_inflation(structural), 26, 0.7).
narrative_ontology:measurement(naft_grid_25, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression(class), 0, 0.14).
narrative_ontology:measurement(naft_grid_26, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression(class), 26, 0.24).
narrative_ontology:measurement(naft_grid_27, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression(individual), 0, 0.08).
narrative_ontology:measurement(naft_grid_28, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression(individual), 26, 0.12).
narrative_ontology:measurement(naft_grid_29, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression(organizational), 0, 0.34).
narrative_ontology:measurement(naft_grid_30, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression(organizational), 26, 0.68).
narrative_ontology:measurement(naft_grid_31, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression(structural), 0, 0.42).
narrative_ontology:measurement(naft_grid_32, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression(structural), 26, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__capital_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% 'Trade-agreement jurisdiction' is a colloquial label spanning three structurally distinct constraints — three readings of one kernel (nafta_jurisdictional_boundary), linked as a constraint family. This file instantiates the capital_supremacy_reading: categorical treaty supremacy with mandatory capital-mobility obligations; its epsilon (0.74) prices jurisdiction transfer, chill, and upward compensation flows as extraction over the shared referent (the standing 1994–2020 arrangement). The sibling readings author different epsilon over the same referent: embedded_liberalism_reading through a policy-space-preserving frame (smaller victim set, lower epsilon), sovereignty_primacy_reading through a subordination frame (agencies as agenda-setters, lowest epsilon). Upstream/downstream structure: this reading's tribunal precedents supply the operative interpretation of the text, constraining what the sibling readings can claim the text permits — the enforcement interpretation is upstream of the doctrinal contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
