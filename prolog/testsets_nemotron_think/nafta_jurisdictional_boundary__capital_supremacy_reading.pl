% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__capital_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: NAFTA/USMCA Capital Supremacy Reading — Treaty Text as Supreme Law Overriding Domestic Regulation
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint story captures the capital_supremacy_reading of the NAFTA
 *   jurisdictional boundary kernel — the interpretation that treaty text
 *   (NAFTA Chapter 11, USMCA Chapter 14) establishes supreme law overriding
 *   domestic regulatory standards, with capital mobility and regulatory
 *   harmonization as mandatory obligations. The reading treats investor
 *   protections as creating a constitutional-style supremacy of capital
 *   rights over domestic police powers. The coordination function (credible
 *   commitment for cross-border investment) is real but the extraction is
 *   asymmetric: mobile capital gains enforceable rights against regulatory
 *   change while immobile populations (workers, communities, ecosystems) bear
 *   the costs of regulatory chill and downward harmonization. The
 *   claim/metric independence is deliberate: the treaty's own framing
 *   presents as rope (mutual coordination), while the authored metrics
 *   describe a tangled_rope — genuine coordination function coupled with
 *   substantial asymmetric extraction requiring active enforcement through
 *   ISDS.
 *
 * KEY AGENTS:
 *   - multinational_corporations: Primary beneficiary (powerful/arbitrage) — collects extraction via ISDS
 *   - financial_sector_actors: Beneficiary/agenda_setter (institutional/arbitrage) — shapes and benefits from interpretation
 *   - subnational_regulatory_agencies: Primary payer (moderate/constrained) — loses jurisdictional authority
 *   - domestic_workers: Primary payer (powerless/trapped) — bears wage/standard suppression
 *   - affected_communities: Primary payer (powerless/trapped) — bears environmental externalities
 *   - investor_state_arbitration_practitioners: Agenda_setter/beneficiary (organized/mobile) — constitutes enforcement machinery
 *   - trade_negotiators_and_state_parties: Agenda_setter (institutional/analytical) — architect and partial captive
 *   - civil_society_and_labor_movements: Excluded (organized/constrained) — structurally locked out
 *   - academic_observers_and_critics: Observer (analytical/analytical) — documents divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.68).
domain_priors:suppression_score(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.75).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "NAFTA/USMCA Capital Supremacy Reading — Treaty Text as Supreme Law Overriding Domestic Regulation").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, '8b477ddc-086e-4286-a911-d37436fd80ec').
narrative_ontology:cs_kernel_codification('8b477ddc-086e-4286-a911-d37436fd80ec', formalized).
narrative_ontology:cs_authority_grounding('8b477ddc-086e-4286-a911-d37436fd80ec', extraction).
narrative_ontology:cs_interpretation_layer_present('8b477ddc-086e-4286-a911-d37436fd80ec').
narrative_ontology:cs_reading_relation('8b477ddc-086e-4286-a911-d37436fd80ec', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b477ddc-086e-4286-a911-d37436fd80ec', nafta_jurisdictional_boundary__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('8b477ddc-086e-4286-a911-d37436fd80ec', foundational, treaty_text_as_supreme_law_over_domestic_regulation).
narrative_ontology:cs_axiom_status(treaty_text_as_supreme_law_over_domestic_regulation, holdable).
narrative_ontology:cs_axiom_grounding('8b477ddc-086e-4286-a911-d37436fd80ec', treaty_text_as_supreme_law_over_domestic_regulation, conventional).
narrative_ontology:cs_axiom('8b477ddc-086e-4286-a911-d37436fd80ec', foundational, capital_mobility_as_non_derogable_obligation).
narrative_ontology:cs_axiom_status(capital_mobility_as_non_derogable_obligation, holdable).
narrative_ontology:cs_axiom_grounding('8b477ddc-086e-4286-a911-d37436fd80ec', capital_mobility_as_non_derogable_obligation, conventional).
narrative_ontology:cs_axiom('8b477ddc-086e-4286-a911-d37436fd80ec', secondary, regulatory_harmonization_operates_upward_only).
narrative_ontology:cs_axiom_status(regulatory_harmonization_operates_upward_only, holdable).
narrative_ontology:cs_axiom_grounding('8b477ddc-086e-4286-a911-d37436fd80ec', regulatory_harmonization_operates_upward_only, empirically_contingent).
narrative_ontology:cs_reference_frame('8b477ddc-086e-4286-a911-d37436fd80ec', investment_protection_regime).
narrative_ontology:cs_drift_state('8b477ddc-086e-4286-a911-d37436fd80ec', post_nafta_isds_jurisprudence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8b477ddc-086e-4286-a911-d37436fd80ec', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, financial_sector_actors).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, capital_mobility_beneficiaries).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_arbitration_practitioners).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_standards).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_standards).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, subnational_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_workers).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, affected_communities).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, public_health_authorities).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_protection_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, regulatory_chill_hypothesis).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, capital_mobility_as_constitutional_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain enforceable rights against sovereign regulatory changes through investor-state dispute settlement (ISDS). Can challenge domestic labor, environmental, and health regulations that affect expected profits. Extract rents from the asymmetry between mobile capital and immobile regulatory authority.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_corporations, beneficiary,
    powerful, generational, arbitrage, global).

% Shape treaty negotiation and interpretation through industry advisory committees and revolving-door personnel. Benefit from capital mobility guarantees that prevent capital controls and financial regulation. The financial sector's structural position lets it influence both the agreement's text and its enforcement.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, financial_sector_actors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__capital_supremacy_reading, financial_sector_actors, agenda_setter).

% Investors and capital holders who benefit from guaranteed freedom of capital movement and protection from regulatory expropriation. Their exit option is capital flight — the threat of which disciplines domestic policy. They collect the difference between what regulations would cost and what the treaty shields them from.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, capital_mobility_beneficiaries, beneficiary,
    organized, biographical, mobile, global).

% Law firms, arbitrators, and experts who constitute the ISDS enforcement machinery. They interpret treaty text, develop precedent, and collect fees from both claimants and respondents. Their professional existence depends on the constraint's enforcement apparatus; they have institutional interest in expanding the scope of actionable claims.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_arbitration_practitioners, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_arbitration_practitioners, beneficiary).

% State/provincial environmental, labor, and health agencies that lose regulatory autonomy when treaty obligations are interpreted to preempt domestic standards. They bear compliance costs and face chill effects — regulations not adopted due to ISDS risk. Their exit is constrained by federal treaty obligations they did not negotiate.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, subnational_regulatory_agencies, payer,
    moderate, biographical, constrained, national).

% Workers whose bargaining power and protective standards are eroded by capital mobility guarantees and regulatory chill. They cannot exit the jurisdiction; their skills are location-specific. They bear the costs of downward harmonization through wage suppression, weakened safety protections, and precarized employment.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_workers, payer,
    powerless, biographical, trapped, local).

% Communities bearing environmental externalities from investment projects that cannot be effectively regulated due to treaty constraints. They lack mobility and political voice in trade negotiations. Extraction manifests as degraded water, air, and health outcomes that the regulatory system is treaty-prevented from addressing.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, affected_communities, payer,
    powerless, generational, trapped, local).

% Health regulators facing ISDS claims over tobacco control, pharmaceutical pricing, and pandemic measures. They bear the cost of defending regulations in arbitration and the chill of not adopting evidence-based policies. Their exit is constrained by constitutional duty to protect health.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, public_health_authorities, payer,
    moderate, biographical, constrained, national).

% The body of labor law (minimum wage, collective bargaining, occupational safety) that enters the victim set when treaty interpretation treats labor protections as indirect expropriation or violations of minimum standard of treatment. These standards bear the extraction but have no agency — they are the object, not the subject.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_standards, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_standards).

% Environmental regulations (emissions limits, toxic substance controls, habitat protection) that face regulatory chill and direct challenge under investment provisions. Like labor standards, they are the regulatory object bearing extraction, not an agent with exit options.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_standards, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_standards).

% Federal governments that negotiated, ratified, and administer the treaty. They set the agenda through dispute settlement participation and joint committee decisions. They are both architects and partial captives — the treaty's enforcement machinery constrains their domestic policy space even as they maintain formal control over amendments.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_negotiators_and_state_parties, agenda_setter,
    institutional, generational, analytical, national).

% Unions, environmental NGOs, consumer groups, and social movements excluded from treaty negotiation and ISDS proceedings. They would challenge the capital supremacy reading but lack standing in the enforcement forum. Their exclusion is structural — the treaty architecture has no mechanism for their participation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, civil_society_and_labor_movements, excluded,
    organized, generational, constrained, national).

% Scholars of international law, political economy, and regulatory governance who analyze the constraint's operation from outside. They document regulatory chill, ISDS jurisprudence, and the divergence between treaty text and lived outcomes. Their exit is analytical — they can change frameworks but not the constraint itself.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, academic_observers_and_critics, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective-action problem of cross-border investment coordination: provides credible commitment against expropriation, establishes common rules for capital movement, and creates a dispute resolution forum that reduces transaction costs for international capital allocation.
% TRANSFER_FUNCTION: Moves regulatory autonomy and policy space from subnational and national authorities to mobile capital holders, mediated through ISDS tribunals. Transfers the cost of regulatory compliance from investors to domestic publics (workers, communities, health systems). Transfers interpretive authority from domestic courts to international arbitration panels.
% ABSENT_VOICES: Subnational governments (states/provinces) that implement regulations but lack treaty-making power; Indigenous nations whose territorial rights are affected by investment protections; future generations who bear environmental costs; workers and communities in all three NAFTA countries who were not represented in negotiations. They are structurally excluded by the treaty's state-centric architecture and ISDS standing rules.
% DISAPPEARANCE_RATIONALE: If the capital supremacy reading vanished overnight, domestic regulatory agencies would regain policy space to strengthen labor, environmental, and health standards without ISDS risk. Capital mobility would still exist but without guaranteed protection against regulatory change. Investment flows would reorganize around domestic legal frameworks rather than treaty guarantees. The North American political economy would restructure toward embedded liberalism.
% FOUNDING_PROBLEM: Post-1980s capital flight and regulatory arbitrage undermined investor confidence in North American integration. The founding problem was creating credible commitment mechanisms to lock in market-oriented reforms and prevent sovereign backsliding, thereby enabling deeper continental investment integration.
% FOUNDING_PROBLEM_CORROBORATION: The original negotiating record (USTR archives, Canadian and Mexican government documents) attests the founding problem was investment confidence and market lock-in. Labor and environmental negotiators (side agreement drafters) attest the problem was narrowly framed to exclude regulatory autonomy concerns. Contemporary ISDS caselaw (Methanex, Metalclad, Eli Lilly) shows the constraint now operates well beyond the founding problem — corroborated by UNCTAD and OECD analyses from outside the beneficiary set.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__capital_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__capital_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__capital_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the measured gap between treaty-guaranteed investor protections and the marginal cost of the coordination function — ISDS awards and regulatory chill transfer value from domestic publics to capital. Suppression (0.75) is high because the constraint's persistence depends on active enforcement: ISDS tribunals, state-to-state dispute settlement, and the threat of retaliation maintain the supremacy interpretation against domestic resistance. Theater ratio (0.42) captures the growing share of enforcement activity that defends capital mobility guarantees rather than the coordination function — side agreements on labor/environment remain weakly enforced while investment provisions generate hundreds of claims. Accessibility collapse (0.72) is high because treaty obligations structurally close exit options: subnational governments cannot opt out, workers cannot move to avoid regulatory chill, communities cannot escape environmental sacrifice zones. Resistance (0.58) is moderate — there is political contestation (USMCA reforms, ISDS opt-outs, legislative pushback) but the constraint's architecture channels resistance into narrow procedural fights rather than structural change.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (corporations, finance) experience this as genuine coordination — a rules-based system that reduces risk and enables investment. The payer seats (regulators, workers, communities) experience the same structure as enforced extraction — a supra-constitutional constraint that removes policy tools they need to protect health, wages, and environment. The agenda_setter seats (ISDS practitioners, negotiators) experience it as a professional/institutional project to maintain and interpret. The engine computes per-seat classifications from these structural positions; the divergence between beneficiary-seat 'rope' and payer-seat 'snare' is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational corporations, financial sector actors, and capital mobility beneficiaries are structural beneficiaries — they collect the transfer (d near 0.0-0.2). ISDS practitioners are agenda_setters who also benefit (d near 0.15). Subnational regulatory agencies, public health authorities are payers with constrained exit (d near 0.7-0.8). Domestic workers and affected communities are payers with trapped exit (d near 0.9-1.0). Domestic labor/environmental standards as non-agent payers bear extraction without agency. Trade negotiators sit near symmetric (d ~0.5) — they built the constraint but are now constrained by it. Civil society is excluded (d undefined — not in the game). Observers have analytical exit (d=0.5 by convention). The derivation chain from beneficiary/victim declarations + exit options produces this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credible commitment for 1990s investment integration) is contested — partly solved (investment flows deepened) but the constraint now operates far beyond that problem. Regulatory chill on climate policy, tobacco control, pharmaceutical pricing, and pandemic response shows the arrangement has accumulated functions it was not built for. The mandatrophy is unresolved: the constraint persists because beneficiaries (capital) have concentrated incentive to maintain it, while payers (diffuse publics) face prohibitive fixing costs (treaty amendment requires consensus of all three parties). This is not pure extraction (coordination function remains) nor pure coordination (extraction is asymmetric and growing) — it is the tangled_rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the capital_supremacy_reading a faithful interpretation of the treaty text, or an expansive construction by ISDS tribunals and beneficiary actors?',
    'Comparative analysis of negotiating history, treaty text (Chapter 11/14), and ISDS jurisprudence — specifically whether ''minimum standard of treatment'' and ''indirect expropriation'' doctrines were intended to cover non-discriminatory public welfare regulation.',
    'If faithful interpretation, the constraint''s extraction is baked into the kernel itself — all readings inherit it. If expansive construction, the extraction is a property of this reading''s enforcement trajectory, not the kernel — sibling readings would have lower ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the capital supremacy reading''s extraction is intrinsic to the kernel or a reading-specific construction').

omega_variable(
    regulatory_chill_measurement,
    'What is the magnitude of regulatory chill — regulations not adopted due to ISDS risk — and how much of the measured extraction is attributable to chill versus direct awards?',
    'Counterfactual policy analysis: compare regulatory adoption rates in NAFTA parties vs. non-parties for equivalent issues; survey regulators on abandoned proposals; analyze drafting records for ISDS-motivated weakening.',
    'If chill dominates, the constraint''s effective extraction is far higher than award data suggests — the theater ratio understates the suppression of alternatives. If awards dominate, the theater ratio better captures the performative-to-functional shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_chill_measurement, empirical, 'Decomposition of extraction into direct awards vs. anticipatory regulatory suppression').

omega_variable(
    usmca_reform_efficacy,
    'Do USMCA''s reforms (limited ISDS scope, state-to-state dispute settlement for most claims, labor/environment chapters with enforcement) materially reduce the capital supremacy reading''s extraction, or merely reconfigure it?',
    'Track ISDS caseload and regulatory chill indicators post-2020; analyze whether remaining investment provisions (Chapter 14, sectoral annexes) sustain the capital supremacy logic for covered sectors (oil/gas, infrastructure, telecom).',
    'If reforms materially reduce extraction, the constraint may be transitioning toward rope or scaffold. If they reconfigure without reducing, the tangled_rope persists with shifted enforcement channels — the mandatrophy remains unresolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(usmca_reform_efficacy, empirical, 'Whether USMCA reforms alter the constraint''s structural classification or merely its enforcement machinery').

omega_variable(
    subnational_agency_coalition_potential,
    'Can subnational regulatory agencies across the three countries form a coalition capable of shifting the constraint''s interpretation toward embedded liberalism?',
    'Analyze existing intergovernmental forums (Commission for Environmental Cooperation, Labor Council), state/provincial litigation participation (amicus briefs in ISDS), and political alignment on regulatory autonomy.',
    'If coalition potential exists, the ''moderate/constrained'' payer seat could shift toward ''organized/mobile'' — altering the power/exit configuration and potentially the engine''s computed directionality. If not, the payer seats remain fragmented and the extraction persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subnational_agency_coalition_potential, empirical, 'Whether fragmented payer seats can coordinate to change their structural position').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nafta_capital_supremacy_tr_t1994, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 1994, 0.18).
narrative_ontology:measurement(nafta_capital_supremacy_tr_t2000, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(nafta_capital_supremacy_tr_t2006, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2006, 0.31).
narrative_ontology:measurement(nafta_capital_supremacy_tr_t2012, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2012, 0.37).
narrative_ontology:measurement(nafta_capital_supremacy_tr_t2018, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2018, 0.4).
narrative_ontology:measurement(nafta_capital_supremacy_tr_t2024, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(nafta_capital_supremacy_be_t1994, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 1994, 0.35).
narrative_ontology:measurement(nafta_capital_supremacy_be_t2000, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(nafta_capital_supremacy_be_t2006, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2006, 0.51).
narrative_ontology:measurement(nafta_capital_supremacy_be_t2012, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2012, 0.58).
narrative_ontology:measurement(nafta_capital_supremacy_be_t2018, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2018, 0.63).
narrative_ontology:measurement(nafta_capital_supremacy_be_t2024, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nafta_capital_supremacy_su_t1994, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 1994, 0.45).
narrative_ontology:measurement(nafta_capital_supremacy_su_t2000, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(nafta_capital_supremacy_su_t2006, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2006, 0.61).
narrative_ontology:measurement(nafta_capital_supremacy_su_t2012, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2012, 0.68).
narrative_ontology:measurement(nafta_capital_supremacy_su_t2018, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2018, 0.72).
narrative_ontology:measurement(nafta_capital_supremacy_su_t2024, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__capital_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, usmca_labor_chapter_enforcement).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, usmca_environment_chapter_enforcement).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_dispute_settlement_architecture).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the nafta_jurisdictional_boundary kernel. The capital_supremacy_reading asserts treaty supremacy over domestic regulation with mandatory capital mobility. The embedded_liberalism_reading asserts balanced framework with policy space. The sovereignty_primacy_reading asserts domestic law supremacy. They form a constraint family linked by network.affects_constraints. The ε values differ: capital_supremacy_reading ε≈0.68 (substantial extraction), embedded_liberalism_reading ε≈0.25 (coordination with limited extraction), sovereignty_primacy_reading ε≈0.05 (near-mountain). The kernel's label 'NAFTA jurisdictional boundary' conflates these structurally distinct claims — this decomposition follows the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nafta_jurisdictional_boundary__capital_supremacy_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
