% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__developmental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_treaty_framework__developmental_reading, []).

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
 *   constraint_id: wto_treaty_framework__developmental_reading
 *   human_readable: WTO Development Compact — Policy Space, Permanent S&D, and Technology Transfer Obligations (Developmental Reading)
 *   domain: international_trade_law/development_economics/political_economy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'wto_treaty_framework': the developmental reading, under which the
 *   treaty's development provisions — special and differential treatment as
 *   permanent structural accommodation, policy space for industrial policy,
 *   tariff and subsidy flexibility, TRIPS flexibilities including compulsory
 *   licensing, and technology transfer obligations — are equal-status treaty
 *   commitments recognizing structurally asymmetric starting conditions, not
 *   transitional exceptions. The sibling reading (market_access) is a
 *   different constraint in a separate file and is not described or
 *   adjudicated here. Epsilon's referent is the standing arrangement this
 *   reading describes — the treaty framework as a development-inclusive
 *   compact — assessed by this reading's own lights; the reading's endorsed
 *   alternative (a fully realized development compact) is not the referent.
 *   KEY AGENTS (by structural relationship): global_south_developing_states:
 *   primary beneficiary and agenda-setter (organized/constrained) —
 *   self-designates, holds consensus leverage; least_developed_countries:
 *   deepest-tier beneficiary (powerless/trapped) — waiver protection with no
 *   alternative channel; infant_industries: protected beneficiary
 *   (moderate/trapped); generic_drug_manufacturers: operational beneficiary
 *   (moderate/constrained); multinational_ip_holders: primary payer
 *   (institutional/constrained) — patent rents bounded by flexibilities;
 *   developed_country_exporters: payer (organized/constrained);
 *   developed_country_governments: dual-positioned payer/beneficiary
 *   (institutional/mobile) — the seat whose drift away is the observed
 *   practice drift; wto_dispute_settlement_organs: agenda-setter
 *   (institutional/constrained) — interpretive and enforcement authority,
 *   contracting since 2019; development_policy_analysts: analytical observer
 *   (analytical/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__developmental_reading, 0.55).
domain_priors:suppression_score(wto_treaty_framework__developmental_reading, 0.38).
domain_priors:theater_ratio(wto_treaty_framework__developmental_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__developmental_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__developmental_reading, "WTO Development Compact — Policy Space, Permanent S&D, and Technology Transfer Obligations (Developmental Reading)").
narrative_ontology:topic_domain(wto_treaty_framework__developmental_reading, "international_trade_law/development_economics/political_economy").

domain_priors:requires_active_enforcement(wto_treaty_framework__developmental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__developmental_reading, '514545c3-f472-4ab7-b7be-866ca607d00c').
narrative_ontology:cs_kernel_codification('514545c3-f472-4ab7-b7be-866ca607d00c', fixed_text).
narrative_ontology:cs_authority_grounding('514545c3-f472-4ab7-b7be-866ca607d00c', lineage).
narrative_ontology:cs_interpretation_layer_present('514545c3-f472-4ab7-b7be-866ca607d00c').
narrative_ontology:cs_reading_relation('514545c3-f472-4ab7-b7be-866ca607d00c', wto_treaty_framework__market_access_reading, forecloses).
narrative_ontology:cs_axiom('514545c3-f472-4ab7-b7be-866ca607d00c', foundational, structural_asymmetry_warrants_permanent_accommodation).
narrative_ontology:cs_axiom_status(structural_asymmetry_warrants_permanent_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('514545c3-f472-4ab7-b7be-866ca607d00c', structural_asymmetry_warrants_permanent_accommodation, empirically_contingent).
narrative_ontology:cs_axiom('514545c3-f472-4ab7-b7be-866ca607d00c', secondary, technology_transfer_core_obligation).
narrative_ontology:cs_axiom_status(technology_transfer_core_obligation, holdable).
narrative_ontology:cs_axiom_grounding('514545c3-f472-4ab7-b7be-866ca607d00c', technology_transfer_core_obligation, instrumental).
narrative_ontology:cs_reference_frame('514545c3-f472-4ab7-b7be-866ca607d00c', doha_development_compact).
narrative_ontology:cs_drift_state('514545c3-f472-4ab7-b7be-866ca607d00c', contemporary_post_appellate_body_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('514545c3-f472-4ab7-b7be-866ca607d00c', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__developmental_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, global_south_developing_states).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, least_developed_countries).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, infant_industries).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, generic_drug_manufacturers).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, multinational_ip_holders).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, developed_country_exporters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, developed_country_governments).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, developed_country_governments).
narrative_ontology:constraint_vindicates(wto_treaty_framework__developmental_reading, infant_industry_protection_doctrine).
narrative_ontology:constraint_vindicates(wto_treaty_framework__developmental_reading, prebisch_singer_structuralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Self-designate for special and differential treatment under the Enabling Clause, bind tariffs at higher ceilings with room to raise them, claim subsidy space for industrial policy, and invoke TRIPS flexibilities including compulsory licensing. As coalitions (G77, African Group, G20) they drove development to the center of the Doha agenda and hold consensus leverage over any new round. Leaving the system would cost them guaranteed access to the major markets, so they bargain inside rather than exit.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, global_south_developing_states, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__developmental_reading, global_south_developing_states, agenda_setter).

% Sit in the deepest accommodation tier: duty-free quota-free access commitments, a TRIPS waiver running to 2034, and the longest transition periods. They negotiate as the LDC Group and African Group because individual capacity is minimal. They cannot leave — preferential access and waiver protection are their principal assets and no alternative channel offers comparable reach.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, least_developed_countries, beneficiary,
    powerless, generational, trapped, regional).

% Nascent manufacturing sectors in developing economies operating behind tariff flexibility and subsidy room. Their viability depends on the accommodation holding through their learning period; the protected market is their market, and there is no exit from it that is not failure.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, infant_industries, beneficiary,
    moderate, generational, trapped, national).

% Produce generics under compulsory licenses and the Article 31bis export mechanism, with the Indian industry the canonical case. Their production model is built on the flexibilities remaining operative; they can shift product lines but not the legal space they operate in.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, generic_drug_manufacturers, beneficiary,
    moderate, biographical, constrained, global).

% Pharmaceutical and technology majors whose patent rents in developing markets are bounded by compulsory licensing, the LDC waiver, and technology-transfer expectations. Treaty obligations attach to their patents in every member jurisdiction and cannot be opted out of. They arbitrage at the margins — price tiers, voluntary licenses, TRIPS-plus terms negotiated through home governments — but the treaty floor holds beneath them.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, multinational_ip_holders, payer,
    institutional, biographical, constrained, global).

% Agricultural and industrial exporters who face subsidized competition from developing-country policy space and conceded market access where preferences and carve-outs apply. They absorb the cost of the accommodation in contestable markets; abandoning those markets would hand them to rivals, so they contest the terms rather than leave.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, developed_country_exporters, payer,
    organized, biographical, constrained, global).

% Concede special and differential carve-outs, fund aid-for-trade, and accept technology-transfer pressure; in exchange they receive system legitimacy, most-favored-nation access everywhere, and the stability of rules-based trade. They hold the strongest outside options — bilateral and plurilateral channels, unilateral measures — and increasingly use them as the Doha agenda stalls.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, developed_country_governments, payer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__developmental_reading, developed_country_governments, beneficiary).

% Panels and, until the 2019 paralysis, the Appellate Body interpret the development provisions, review self-designation disputes, and authorize retaliation for noncompliance. They are bound by the covered agreements and by consensus appointment rules; since the paralysis their enforcement reach has contracted to first-instance panels and appeals into the void.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, wto_dispute_settlement_organs, agenda_setter,
    institutional, generational, constrained, global).

% UNCTAD, World Bank, and academic trade economists who measure whether the accommodation delivers structural transformation. They attest or dispute the founding problem's liveness from outside the negotiating seats and publish the convergence data on which the permanence question turns.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, development_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_treaty_framework__developmental_reading, global_south_developing_states).
narrative_ontology:fixing_cost_class(wto_treaty_framework__developmental_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps all states inside one rules-based trading system by pricing membership according to development level: common tariff bindings, a most-favored-nation baseline, and defined flexibilities for members with asymmetric industrial capacity. A purely symmetric regime would price most developing states out of membership and fragment world trade into blocs and power bargains; the accommodation is the device that makes universal membership affordable for the weak and tolerable for the strong.
% TRANSFER_FUNCTION: Moves binding obligation from poor to rich (developing states bind fewer tariffs, retain subsidy room, may license patents compulsorily) and moves rent and concession from rich to poor (patent holders forgo rents in licensed markets, exporters concede preference margins, developed members fund aid-for-trade). Nominally it also moves technology from holders to least developed members under Article 66.2.
% ABSENT_VOICES: Consumers in protected developing markets pay the infant-industry premium and tariff pass-through but hold no seat — diffuse and unorganized at a table occupied by state delegations responsive to organized producers. Would-be exporters inside developing countries harmed by their own states' protection are similarly unrepresented, as are future generations if protection entrenches inefficiency. All of these sit outside the negotiating coalitions entirely.
% DISAPPEARANCE_RATIONALE: If the accommodations vanished overnight, developing-country coalitions would withhold consensus on any new round, dispute litigation over subsidies and intellectual property would spike, several members would likely stop honoring IP commitments or withdraw, and the single undertaking — the framework's defining feature — would fragment into competing blocs and bilateral power bargains. The system's universality is organized around these commitments.
% FOUNDING_PROBLEM: The postwar trade rules were written by and for industrialized economies. Newly independent and developing states faced obligations that locked in colonial-era specialization, nascent industries, and a wide technological gap: symmetric rules would codify asymmetric starting conditions and entrench dependency. GATT Part IV (1964), the Enabling Clause (1979), the Uruguay Round transition provisions, and the Doha Declaration (2001) responded by building development accommodations into the framework.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties by the treaty record itself (GATT Part IV's preamble and the Enabling Clause's recitals acknowledge structural disadvantage), by UNCTAD and World Bank structural data on persistent productivity and technological gaps, and by the documented import-substitution strategies of most newly industrializing states. Developed-country trade ministries dispute the remedy — permanent accommodation — but not the existence of the founding asymmetry.
narrative_ontology:disappearance_verdict(wto_treaty_framework__developmental_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__developmental_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__developmental_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_treaty_framework__developmental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_treaty_framework__developmental_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_treaty_framework__developmental_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_treaty_framework__developmental_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_treaty_framework__developmental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.55 at interval end): the transfers are real and identifiable — patent rents bounded by compulsory licensing normalized after Doha, the LDC TRIPS waiver extended to 2034, preference margins and subsidy room conceded — but treaty-bounded, and partly the deliberate object of the accommodation rather than leakage from it. Suppression (0.38, falling) is structural, not internalized: it lives in treaty lock-in and the single undertaking, and its active machinery is dispute settlement, whose capacity has decayed since the Appellate Body paralysis of December 2019 — hence the falling suppression_requirement series, authored because enforcement-capacity change is the dynamic this story traces (enforcement decay, not an enforcement ratchet). Theater (0.48, rising) tracks the compact hollowing into rhetoric: Article 66.2 technology-transfer reporting is largely ritual, many of the roughly 155 S&D provisions are best-endeavor language, and the COVID-era TRIPS waiver negotiation consumed years to produce a narrow, largely symbolic instrument. Accessibility collapse is low (0.40) because alternatives persist — bilaterals, plurilaterals, GSP schemes, non-membership. Resistance (0.55) is sustained on both sides: payer-side TRIPS-plus lobbying and graduation pressure, beneficiary-side resistance to subsidy and fisheries discipline. The claimed type is authored from structure (a genuine coordination object — universal rules-based trade priced by development level — with identifiable persistent payers); the metrics are authored from observed operation, independently of the claim. All three series share one time grid (0, 5, 10, 15, 20, 25), each metric authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute different types from the same treaty text. From the IP holders' position the flexibilities are expropriation of patented rents under color of health exceptions; from the developing states' position the same provisions are corrective terms without which membership would be unaffordable; from the dispute organs' position they are interpretive material to be administered. Developed-country governments straddle: they pay the concessions and collect the legitimacy, and their mobile exit options make them the seat whose drift away is the observed practice drift. The engine computes this divergence from role, power, and exit data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end: developing states collect preference margins, policy space, and flexibilities; least developed members most deeply, with trapped exit meaning their beneficiary position is also their lock-in; infant industries and generic producers derive their operating models from the accommodation. Payers sit near the target end: IP holders cannot opt out of treaty application to their patents in any member jurisdiction, and their arbitrage (price tiers, voluntary licenses, TRIPS-plus lobbying) does not reach the treaty floor; developed-country exporters absorb subsidized competition and cannot abandon the markets without ceding them to rivals. The dual-positioned seat — developed-country governments, payer with beneficiary secondary role — derives a mid-range directionality from the secondary role rather than an override; no directionality overrides are authored because per-power-atom overrides would mis-cast same-atom agents (institutional payers versus institutional agenda-setters) that hold genuinely different relationships to the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification blocks two mislabels. As pure snare it fails: the extraction has a genuine coordination object — universal rules-based trade priced by development level — and the payers concede rather than exit precisely because the system they fund is the system they trade through. As pure rope it fails: the payers are persistent, identifiable, and do not capture the gains. On mandatrophy specifically: this reading asserts the founding problem — structural development asymmetry — is live, so the no-sunset design (has_sunset_clause: false) is the honest form, not a transitional support whose sunset never arrived. The sibling market_access reading reads the same provisions as exactly that expired transitional structure; the two readings disagree on whether this is a live accommodation or a mandatrophic remnant, and that disagreement is routed to the sd_permanence_status omega rather than settled here. The rising theater series is where the question would surface operationally: continued theater growth alongside enforcement decay would indicate the compact drifting toward theatrical maintenance of an eroding operative core.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wto_kernel_reading_contest,
    'This constraint is one reading of kernel wto_treaty_framework — what would the sibling market_access_reading change structurally, and which reading captures the framework''s operative interpretive authority?',
    'Observe where interpretive authority lands: Appellate Body restoration and its development-provision jurisprudence, ministerial outcomes on S&D review, and whether TRIPS-plus bilateral practice is ratified or resisted by the membership.',
    'If the market_access reading captures authority, this constraint''s beneficiaries convert to transitional exception-takers, has_sunset_clause flips true, and extraction redistributes from IP holders toward protected developing-market incumbents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wto_kernel_reading_contest, conceptual, 'Which reading of the WTO kernel holds operative authority.').

omega_variable(
    sd_permanence_status,
    'Is the development asymmetry this accommodation answers to structural and self-perpetuating (this reading''s premise) or transitional and self-correcting (the sibling''s premise)?',
    'Long-run convergence evidence: whether symmetric-rule exposure converges productivity and technological capability across members — structural transformation literature and natural experiments from graduated members such as Korea and Singapore.',
    'If transitional, the arrangement is a transitional support whose sunset never arrived — mandatrophy rather than accommodation — and has_sunset_clause=false is dishonest; if structural, the no-sunset design is the honest form.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sd_permanence_status, empirical, 'Whether S&D permanence rests on a true empirical premise about asymmetry.').

omega_variable(
    tech_transfer_obligation_reality,
    'Is the Article 66.2 technology transfer obligation an operative commitment or a performative one?',
    'Audit Article 66.2 reports against verifiable transfer flows — licensing volume, technology-content FDI, R&D localization in least developed members — rather than self-reported member submissions.',
    'If performative, theater_ratio is understated and the transfer component drifts toward theatrical maintenance; if operative, the coordination function is stronger than the theater series suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tech_transfer_obligation_reality, empirical, 'Operative versus performative status of the technology transfer obligation.').

omega_variable(
    compulsory_licensing_effective_scope,
    'Do TRIPS flexibilities deliver usable policy space across developing members, or only where manufacturing capability already exists (the 31bis export mechanism has been used effectively once)?',
    'Count compulsory licenses issued and 31bis importations by member capability class; compare intended beneficiaries against actual users.',
    'If capability-constrained, nominal extraction from IP holders overstates effective transfer, and the accommodation''s gains concentrate in a few capable states (India, Brazil, Thailand) — reshaping the declared beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compulsory_licensing_effective_scope, empirical, 'Whether flexibilities are usable policy space or capability-gated.').

omega_variable(
    authority_structure_framing,
    'Is the framework''s authority lineage (founding text adjudicated by dispute organs) or distributed (consensus sovereignty with post-2019 power-based compliance)? Two coherent framings of the same authority structure are available.',
    'Post-paralysis compliance behavior: whether members comply with unappealed panel reports (lineage intact) or settle disputes through power-based retaliation and bilateral deals (distributed).',
    'Under the distributed framing the drift reading changes from practice_drift under a weakened interpreter toward codification_collapse of the interpretive layer itself, and the foreclosure relation between readings weakens — with no functioning adjudicator, nothing is foreclosed, only outvoted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_structure_framing, conceptual, 'Framing of the framework''s authority structure and its effect on drift and foreclosure analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__developmental_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dev_reading_tr_t0, wto_treaty_framework__developmental_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(wto_dev_reading_tr_t0, observed).
narrative_ontology:measurement(wto_dev_reading_tr_t5, wto_treaty_framework__developmental_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(wto_dev_reading_tr_t5, observed).
narrative_ontology:measurement(wto_dev_reading_tr_t10, wto_treaty_framework__developmental_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(wto_dev_reading_tr_t10, observed).
narrative_ontology:measurement(wto_dev_reading_tr_t15, wto_treaty_framework__developmental_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(wto_dev_reading_tr_t15, observed).
narrative_ontology:measurement(wto_dev_reading_tr_t20, wto_treaty_framework__developmental_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement_basis(wto_dev_reading_tr_t20, observed).
narrative_ontology:measurement(wto_dev_reading_tr_t25, wto_treaty_framework__developmental_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(wto_dev_reading_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(wto_dev_reading_be_t0, wto_treaty_framework__developmental_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(wto_dev_reading_be_t0, observed).
narrative_ontology:measurement(wto_dev_reading_be_t5, wto_treaty_framework__developmental_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement_basis(wto_dev_reading_be_t5, observed).
narrative_ontology:measurement(wto_dev_reading_be_t10, wto_treaty_framework__developmental_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(wto_dev_reading_be_t10, observed).
narrative_ontology:measurement(wto_dev_reading_be_t15, wto_treaty_framework__developmental_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(wto_dev_reading_be_t15, observed).
narrative_ontology:measurement(wto_dev_reading_be_t20, wto_treaty_framework__developmental_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement_basis(wto_dev_reading_be_t20, observed).
narrative_ontology:measurement(wto_dev_reading_be_t25, wto_treaty_framework__developmental_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement_basis(wto_dev_reading_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto_dev_reading_su_t0, wto_treaty_framework__developmental_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(wto_dev_reading_su_t0, observed).
narrative_ontology:measurement(wto_dev_reading_su_t5, wto_treaty_framework__developmental_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement_basis(wto_dev_reading_su_t5, observed).
narrative_ontology:measurement(wto_dev_reading_su_t10, wto_treaty_framework__developmental_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(wto_dev_reading_su_t10, observed).
narrative_ontology:measurement(wto_dev_reading_su_t15, wto_treaty_framework__developmental_reading, suppression_requirement, 15, 0.46).
narrative_ontology:measurement_basis(wto_dev_reading_su_t15, observed).
narrative_ontology:measurement(wto_dev_reading_su_t20, wto_treaty_framework__developmental_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(wto_dev_reading_su_t20, observed).
narrative_ontology:measurement(wto_dev_reading_su_t25, wto_treaty_framework__developmental_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement_basis(wto_dev_reading_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__developmental_reading, resource_allocation).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, wto_treaty_framework__market_access_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the WTO treaty framework' covers two structurally distinct claims about the same text and is decomposed per the epsilon-invariance principle into two linked stories. This file instantiates the developmental_reading (asymmetry structural; S&D permanent; policy space equal-status; technology transfer core) with moderate epsilon, Global South beneficiaries, and IP holders/exporters as payers. The sibling wto_treaty_framework__market_access_reading instantiates the symmetric-obligation claim (S&D temporary transitional exception; non-discrimination primary) with a different epsilon, a different victim set (protected developing-market incumbents as the extraction's targets), and a sunset expectation. The upstream/downstream structure runs through the shared treaty text: whichever reading captures interpretive authority (dispute organs, ministerial outcomes) conditions the other's operating environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
