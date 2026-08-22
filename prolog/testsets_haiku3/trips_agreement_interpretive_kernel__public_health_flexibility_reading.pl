% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__public_health_flexibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__public_health_flexibility_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: trips_agreement_interpretive_kernel__public_health_flexibility_reading
 *   human_readable: TRIPS Public Health Flexibility Interpretation
 *   domain: international_trade/public_health/intellectual_property
 *
 * SUMMARY:
 *   The TRIPS agreement (1995) embeds intellectual property obligations that
 *   member states must implement. Under one contested reading—the public
 *   health flexibility interpretation—the text's compulsory licensing
 *   (Article 31) and parallel import (Article 6) provisions are broad enough
 *   to permit WTO members to prioritize medicine access via generic
 *   production and price negotiation, especially in low-income settings.
 *   Under the opposing strong exclusivity reading, TRIPS mandates high
 *   uniform patent protections with narrow exceptions only for true
 *   emergencies, to preserve pharmaceutical R&D incentives. This story
 *   instantiates the public health flexibility reading as a single constraint
 *   with its own ε-invariance: the standing arrangement (TRIPS text
 *   interpreted through this reading) is assessed by this reading's own
 *   lights as substantially coordinating public health access, though
 *   asymmetrically at the expense of patent holders' expected returns. The
 *   strong exclusivity reading is a separate constraint (different ε,
 *   different beneficiary/victim structure) and is NOT described here.
 *
 * KEY AGENTS:
 *   - Generic manufacturers (organized, mobile exit): gain negotiating leverage under this reading; produce medicines where compulsory licensing permits
 *   - Low-income health ministries (moderate power, constrained exit): gain legal framework to authorize domestic/regional production or parallel imports for affordability
 *   - Pharmaceutical patent holders (institutional, constrained exit): bear erosion of exclusivity and pricing power; constrained by trade regime rules they cannot exit
 *   - WTO dispute panels (institutional, analytical): adjudicate scope of public health exceptions; their verdicts determine whether this reading holds or strong exclusivity prevails
 *   - Patients in resource-constrained settings (powerless, trapped): gain potential access but cannot advocate directly; their interests mediated through health ministries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.38).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.42).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "TRIPS Public Health Flexibility Interpretation").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "international_trade/public_health/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '956216a2-6fde-4b23-bdc4-3b2e3ceeeee1').
narrative_ontology:cs_kernel_codification('956216a2-6fde-4b23-bdc4-3b2e3ceeeee1', fixed_text).
narrative_ontology:cs_authority_grounding('956216a2-6fde-4b23-bdc4-3b2e3ceeeee1', extraction).
narrative_ontology:cs_interpretation_layer_present('956216a2-6fde-4b23-bdc4-3b2e3ceeeee1').
narrative_ontology:cs_reading_relation('956216a2-6fde-4b23-bdc4-3b2e3ceeeee1', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_axiom('956216a2-6fde-4b23-bdc4-3b2e3ceeeee1', foundational, compulsory_licensing_broad_public_health_exception).
narrative_ontology:cs_axiom_status(compulsory_licensing_broad_public_health_exception, holdable).
narrative_ontology:cs_axiom_grounding('956216a2-6fde-4b23-bdc4-3b2e3ceeeee1', compulsory_licensing_broad_public_health_exception, deontological).
narrative_ontology:cs_axiom('956216a2-6fde-4b23-bdc4-3b2e3ceeeee1', foundational, right_to_health_supersedes_patent_exclusivity_in_access_emergency).
narrative_ontology:cs_axiom_status(right_to_health_supersedes_patent_exclusivity_in_access_emergency, holdable).
narrative_ontology:cs_axiom_grounding('956216a2-6fde-4b23-bdc4-3b2e3ceeeee1', right_to_health_supersedes_patent_exclusivity_in_access_emergency, deontological).
narrative_ontology:cs_reference_frame('956216a2-6fde-4b23-bdc4-3b2e3ceeeee1', trips_text_public_health_protective_interpretation).
narrative_ontology:cs_drift_state('956216a2-6fde-4b23-bdc4-3b2e3ceeeee1', contemporary_wto_dispute_settlement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('956216a2-6fde-4b23-bdc4-3b2e3ceeeee1', '2026-06-11T00:00:00Z').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_manufacturers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, low_income_health_ministries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patients_in_resource_constrained_settings).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, research_driven_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_manufacturers).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, right_to_health_doctrine).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, public_health_exemption_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain negotiating leverage and legal pathway to produce medicines under compulsory license when patents block affordable access. They manufacture and distribute generic versions at lower cost once exclusivity barriers are interpreted narrowly. They also bear the cost of litigation risk and potential trade retaliation if WTO dispute panels side with patent holders on licensing scope.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_manufacturers, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_manufacturers, payer).

% Gain the legal framework to authorize generic production or direct import of patented medicines when prices exclude patients from care. They can invoke public health emergency provisions and compulsory licensing to negotiate lower prices with patent holders or authorize domestic/regional production. They remain constrained by WTO enforcement mechanisms and pharmaceutical industry pressure.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, low_income_health_ministries, beneficiary,
    moderate, biographical, constrained, national).

% Gain potential access to medicines that patent-driven pricing otherwise excludes. The compulsory licensing and parallel import flexibilities in this reading mean affordability becomes structurally possible when health ministries exercise them. They remain trapped by whatever decisions health authorities and manufacturers make; their voice is absent from WTO negotiations.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patients_in_resource_constrained_settings, beneficiary,
    powerless, immediate, trapped, national).

% Bear erosion of market exclusivity and pricing power as compulsory licensing is interpreted broadly and parallel imports reduce their control over territorial pricing. They face reduced revenue in markets where health ministries exercise public health exceptions, and must defend patent scope through costly WTO dispute proceedings. They cannot exit the global trade regime without abandoning market access to major economies.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders, payer,
    institutional, generational, constrained, global).

% Argue that broad compulsory licensing interpretation undermines the innovation incentive structure TRIPS was designed to create. They face reduced returns on R&D investment when the public health reading permits licensing that bypasses bilateral negotiation. They can arbitrage by shifting R&D focus to diseases with higher-income markets or pursuing alternative patent strategies in jurisdictions with narrower compulsory licensing interpretation.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, research_driven_firms, payer,
    powerful, generational, arbitrage, global).

% Adjudicate the scope of compulsory licensing and parallel import flexibilities through dispute settlement. Their interpretive choices determine whether TRIPS text permits broad public health exceptions (this reading) or narrow them to emergencies only (strong exclusivity reading). They enforce verdicts through trade retaliation authorization.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_dispute_panels, agenda_setter,
    institutional, generational, analytical, global).

% Are formally TRIPS signatories with voting influence but have largely deferred dispute settlement to pharmaceutical and trade representative coalitions. They would advocate for intellectual property enforcement if included in negotiation framing; their exclusion from the primary beneficiary/victim dyad reflects that this reading brackets their interests.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, high_income_governments, excluded,
    institutional, generational, arbitrage, global).

% Document the public health case for broad licensing interpretation: WHO analysis of medicine access gaps, epidemiological data on disease burden in resource-constrained settings, evidence of price barriers to treatment. They do not enforce the constraint but provide corroboration for the public health reading's legitimacy claim.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, international_health_bodies, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__public_health_flexibility_reading, diffuse).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__public_health_flexibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a global normative framework clarifying that TRIPS permits WTO members to prioritize public health access over patent exclusivity via compulsory licensing and parallel imports. Solves the coordination problem of allowing countries to act on health concerns without fear of trade retaliation, while maintaining predictable rules for intellectual property holders.
% TRANSFER_FUNCTION: Transfers negotiating power and market access from pharmaceutical patent holders to generic manufacturers and health ministries. Moves the locus of pricing control from unilateral patent holder decisions to bilateral negotiation under threat of compulsory licensing. Moves medicine supply from patented single-source to generic multi-source production in low-income markets.
% ABSENT_VOICES: Patients and health workers in resource-constrained settings have no direct representation in WTO dispute panels or TRIPS amendment processes; their interests are mediated through health ministries and NGOs. Smaller pharmaceutical firms that depend on patent protection in middle-income markets are largely silent in high-income policy spaces. Local manufacturing capacity advocates in developing countries struggle for voice against both patent holders and generic manufacturers with capital advantage.
% DISAPPEARANCE_RATIONALE: If this reading vanished and strong exclusivity interpretation took hold, pharmaceutical patent holders would reassert unilateral pricing power; medicine access in low-income countries would contract; health ministries would lose the legal framework to negotiate from strength; generic manufacturers would face trade litigation risk for production they currently undertake under compulsory license authority. The global medicine market would reorganize around narrower access and higher prices in all markets where this reading currently shields compulsory licensing.
% FOUNDING_PROBLEM: Post-TRIPS (1995), pharmaceutical prices rose sharply in developing countries, excluding patients from life-saving medicines while patent holders maintained monopoly pricing. The 2001 Doha Declaration and subsequent interpretations attempted to clarify that TRIPS does NOT require member states to sacrifice public health to patent enforcement.
% FOUNDING_PROBLEM_CORROBORATION: WHO, Médecins Sans Frontières, academic health economists, and developing-country health ministries document persistent medicine access gaps in resource-constrained settings. WHO analysis shows prices in low-income countries remain 5-10x higher in patent-protected markets than in generic-competitive markets. Doha Declaration signatories and subsequent WTO clarifications (post-2003 TRIPS flexibilities decision) affirm the public health framing. High-income pharmaceutical manufacturers contest that the problem persists with equal vigor, but the access gap is corroborated by independent epidemiological data outside the pharmaceutical industry.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__public_health_flexibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__public_health_flexibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).
:- end_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because this reading genuinely coordinates public health access—the compulsory licensing and parallel import provisions serve a coordination function, not pure extraction from patent holders. Suppression is correspondingly moderate (0.42) because the constraint's persistence depends on WTO dispute panels ruling narrowly on compulsory licensing scope; it is actively enforced through trade retaliation threat but is not maximally coercive because the legal framework is explicitly written (text-based, not hidden). Theater is low-moderate (0.28) because the public health framing is substantively grounded in epidemiological access gaps, though pharmaceutical industry rhetoric frames the same provisions as innovation-threatening abuse. The measurement series show declining extractiveness and suppression over the 30-year interval, reflecting increasing acceptance of the public health reading in dispute settlement practice (Doha clarifications 2003, India patent case 2013, and subsequent WTO panels favoring compulsory licensing scope). Accessibility collapse is moderate (0.48)—alternatives to TRIPS licensing exist (bilateral negotiation, direct price negotiation without formal licensing) but are less stable and less binding than the treaty interpretation. Resistance is high (0.72) because both pharmaceutical industry and some high-income governments actively resist this reading through dispute settlements, trade pressure, and intellectual property strengthening in bilateral agreements.
 *
 * PERSPECTIVAL GAP:
 *   Patent holders in strong-exclusivity-reading frame: this interpretation is extractive appropriation of their negotiated rights—TRIPS promised protection, this reading waters it down. Health ministries and generics in public-health-reading frame: patent prices are the extraction; this reading corrects it by re-centering medicine access. Dispute panels: interpretive authority seat—constrain scope and consistency of TRIPS application. Each seat should compute a different effective extraction (χ) because their directionality (d) is structurally different: beneficiary patents holders have low/negative χ (subsidy-like), while victim patent holders have high χ (extraction-like).
 *
 * DIRECTIONALITY LOGIC:
 *   Generic manufacturers and health ministries are beneficiaries because they gain negotiating leverage, legal standing for licensing, and pathways to affordable production that this reading creates. Patients are beneficiaries (gain access potential) though powerless and voiceless. Pharmaceutical patent holders are victims because their exclusivity is narrowed, their pricing power is compressed, and their defense costs are high—they bear the extraction of margin through margin-narrowing compulsory licensing and parallel imports. The WTO panels are agenda-setters (they interpret the constraint) but occupy an analytical seat for the purposes of directionality. Directionality for generics and health ministries should compute low (near 0.2–0.4) due to beneficiary status and mobile/constrained exit; for patent holders should compute high (near 0.7–0.9) due to victim status and constrained exit they cannot arbitrage out of without abandoning markets. No overrides needed: the structural derivation should align with the narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading classifies as tangled_rope (not rope, not snare) because: (1) it possesses a genuine coordination function—clarifying that TRIPS permits public health licensing solves a collective-action problem around access; (2) it exhibits asymmetric extraction—pharmaceutical patent holders bear specific costs (narrowed exclusivity, eroded pricing power) while generic manufacturers and health ministries gain (legal standing, negotiating leverage); (3) it requires active enforcement—WTO dispute panels must rule consistently narrowly on licensing scope, trade retaliation threat must back the constraint, and high-income governments must resist pharmaceutical industry pressure to strengthen patent protections. This classification prevents misreading it as pure rope (which would ignore the extraction from patent holders) or pure snare (which would ignore the genuine public health coordination function). The mandatrophy check: founding problem (medicine access gaps post-TRIPS) remains live; the constraint persists because beneficiary states and organizations actively defend this reading through dispute proceedings and norm-setting. No mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_health_vs_innovation_incentive_empirical,
    'Does broad compulsory licensing interpretation actually harm pharmaceutical innovation incentives, or do prices in high-income markets (unaffected by this reading) maintain sufficient R&D return?',
    'Pharmaceutical R&D funding and pipeline data: if R&D investment remains stable in high-income countries despite compulsory licensing in low-income settings, innovation is not undermined; if R&D pivots away from diseases affecting only poor populations, the incentive harm is selective and targetable.',
    'If innovation is not materially harmed, the strong exclusivity reading loses its core justification and this reading becomes dominant. If innovation is harmed, the trade-off between access and incentive becomes explicit and may warrant hybrid interpretation (broad licensing for generic-viable diseases, narrower for novel therapeutic classes).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_health_vs_innovation_incentive_empirical, empirical, 'Whether broad compulsory licensing erodes pharmaceutical R&D incentives measurably in practice.').

omega_variable(
    compulsory_licensing_scope_ambiguity,
    'Does ''public health emergency'' (Article 31 language) encompass endemic diseases (HIV, malaria, TB in low-income countries) or only acute crises (pandemic, war)?',
    'WTO dispute panel rulings on specific compulsory licensing invocations; evolving customary interpretation in state practice; amendment or clarification of TRIPS text.',
    'If scope is narrow (acute crises only), this reading provides less benefit and approaches the strong exclusivity reading empirically. If scope is broad (endemic diseases as structural health emergencies), this reading licenses sustained generic competition and price pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compulsory_licensing_scope_ambiguity, conceptual, 'The boundary between emergency and endemic in the public health flexibility interpretation.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression (0.42) structural (WTO retaliation threat, trade pressure from high-income governments, legal defense costs) or partially internalized (developing countries self-censor compulsory licensing invocation due to fear or norm internalization)?',
    'Post-narrowing analysis: if a country that stops invoking this reading''s protections later re-invokes them after retaliation proves mild or absent, suppression was partially internalized; if countries continue self-censoring despite low retaliation, suppression is internalized.',
    'If internalized, the constraint''s effective suppression is higher than measured and the constraint is closer to snare dynamics (targets carry suppression with them even after barriers lower). If structural, the suppression would collapse if WTO retaliation authority were removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression of compulsory licensing invocation is structural or internalized in health ministries.').

omega_variable(
    reading_kernel_interpretation_authority,
    'Which seat—WTO dispute panels, TRIPS signatories collectively, national courts, or customary state practice—holds binding interpretive authority over what TRIPS text permits?',
    'Tracking which authority''s rulings and interpretations states actually follow in practice; observing whether conflicting readings by different authorities persist or converge; monitoring whether WTO dispute settlement verdicts are accepted as binding or contested.',
    'If WTO panels hold authority, this reading persists only if panels rule narrowly on licensing scope; if national courts or state practice hold authority, this reading''s scope may expand (states interpret for themselves) or contract (states defer to pharmaceutical industry). Authority ambiguity is a core structural uncertainty for this kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_interpretation_authority, conceptual, 'Interpretive authority over TRIPS text—locus of binding constraint interpretation in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(trip_tr_t0, observed).
narrative_ontology:measurement(trip_tr_t5, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(trip_tr_t5, observed).
narrative_ontology:measurement(trip_tr_t10, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(trip_tr_t10, observed).
narrative_ontology:measurement(trip_tr_t15, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement_basis(trip_tr_t15, observed).
narrative_ontology:measurement(trip_tr_t20, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(trip_tr_t20, observed).
narrative_ontology:measurement(trip_tr_t30, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(trip_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(trip_be_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(trip_be_t0, observed).
narrative_ontology:measurement(trip_be_t5, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(trip_be_t5, observed).
narrative_ontology:measurement(trip_be_t10, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement_basis(trip_be_t10, observed).
narrative_ontology:measurement(trip_be_t15, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement_basis(trip_be_t15, observed).
narrative_ontology:measurement(trip_be_t20, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement_basis(trip_be_t20, observed).
narrative_ontology:measurement(trip_be_t30, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(trip_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(trip_su_t0, observed).
narrative_ontology:measurement(trip_su_t5, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(trip_su_t5, observed).
narrative_ontology:measurement(trip_su_t10, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement_basis(trip_su_t10, observed).
narrative_ontology:measurement(trip_su_t15, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(trip_su_t15, observed).
narrative_ontology:measurement(trip_su_t20, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement_basis(trip_su_t20, observed).
narrative_ontology:measurement(trip_su_t30, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(trip_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__public_health_flexibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.18).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_price_negotiation_power).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_drug_market_access).

% DUAL FORMULATION NOTE:
% This constraint (public health flexibility reading) and trips_agreement_interpretive_kernel__strong_exclusivity_reading are two readings of the same kernel (trips_agreement_interpretive_kernel). They share the referent (TRIPS text and its operation) but differ in what the text permits: this reading interprets broad compulsory licensing permission; the sibling reading interprets narrow exceptions. They are NOT two perspectives on a single constraint; they are structurally distinct constraints with different ε values, different beneficiary/victim sets, and different persistence mechanisms. Link them via network.affects_constraints because the dispute settlement verdict on one reading directly determines the scope and credibility of the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
