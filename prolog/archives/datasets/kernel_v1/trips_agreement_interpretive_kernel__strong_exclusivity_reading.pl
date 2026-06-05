% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__strong_exclusivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_strong_exclusivity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: trips_agreement_interpretive_kernel__strong_exclusivity_reading
 *   human_readable: TRIPS Strong Patent Exclusivity Reading: High Uniform Protection with Narrow Health Flexibilities
 *   domain: international_trade_law/intellectual_property/public_health_policy
 *
 * SUMMARY:
 *   The TRIPS Agreement (1994) mandates minimum intellectual property
 *   standards across WTO member states, including 20-year patent terms and
 *   narrow flexibilities for public health emergencies. This constraint story
 *   instantiates ONE reading of the TRIPS kernel: the strong patent
 *   exclusivity reading. Under this reading, TRIPS text mandates high uniform
 *   patent protections with narrow flexibilities construed to serve
 *   pharmaceutical innovation incentives, not health access. Patent holders
 *   (multinational pharma firms, high-income country governments) are
 *   beneficiaries; low-income populations and generic manufacturers are
 *   victims. The core tension: TRIPS Article 31 permits compulsory licensing
 *   for national emergencies, but in practice this flexibility is narrowly
 *   construed through dispute resolution and retaliation risk, producing a
 *   constraint that simultaneously coordinates pharmaceutical R&D investment
 *   AND extracts rents from low-income populations through high prices and
 *   constrained generic access. The constraint exhibits all the structural
 *   features of tangled rope at multiple perspectives: genuine coordination
 *   (the agreement does solve patent enforcement coordination problems)
 *   coexists with asymmetric extraction (the same agreement restricts access
 *   to medicines). The measurement trajectory shows increasing extractiveness
 *   (0.42→0.58) as TRIPS enforcement has strengthened, suppression has risen
 *   as dispute mechanisms have matured, and theater has remained moderate
 *   (indicating both functional enforcement AND some performative elements).
 *   The sibling reading (public_health_flexibility_reading) interprets the
 *   same TRIPS text differently: emphasizing Article 31 flexibilities as
 *   primary and patent scope as secondary. This constraint instantiates the
 *   alternative reading where exclusivity dominates.
 *
 * KEY AGENTS:
 *   - Multinational Pharmaceutical Firms: Primary beneficiary (powerful/arbitrage) — capture price premiums through patent enforcement; can shift production and licensing to optimize under TRIPS regime
 *   - High-Income Country Governments: Secondary beneficiary and institutional victim (institutional/constrained) — benefit from domestic pharma industry protection; bear costs through healthcare inflation and internal political pressure
 *   - Low-Income Populations: Primary victim (powerless/trapped) — cannot access medicines due to price barriers and generic constraints; legal barriers prevent exit via compulsory licensing due to retaliation risk
 *   - Low-Income Country Governments: Secondary victim (moderate/constrained) — face retaliation risk when invoking Article 31 compulsory licensing; constrained ability to deploy their own policy instruments
 *   - Generic Manufacturers (India, Brazil): Constrained victims (organized/constrained) — can manufacture but face patent enforcement and export restrictions; capability to serve low-income markets is under pressure from strong exclusivity enforcement
 *   - Coalition of Low-Income Countries and Health NGOs: Organized victim-advocates (organized/constrained) — generated Doha Declaration and Article 31 flexibilities but face suppression from dispute resolution risk and pharma lobbying
 *   - WTO Dispute Resolution Mechanism: Institutional enforcer (institutional/arbitrage) — maintains formal rule-of-law appearance while actual outcomes are negotiated bilaterally; functions as piton (degraded ritual persistence)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.58).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.68).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Strong Patent Exclusivity Reading: High Uniform Protection with Narrow Health Flexibilities").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade_law/intellectual_property/public_health_policy").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'ad21aaa9-1f34-4bc9-8b15-5763e7342a98').
narrative_ontology:cs_kernel_codification('ad21aaa9-1f34-4bc9-8b15-5763e7342a98', formalized).
narrative_ontology:cs_authority_grounding('ad21aaa9-1f34-4bc9-8b15-5763e7342a98', extraction).
narrative_ontology:cs_interpretation_layer_present('ad21aaa9-1f34-4bc9-8b15-5763e7342a98').
narrative_ontology:cs_reading_relation('ad21aaa9-1f34-4bc9-8b15-5763e7342a98', trips_agreement_interpretive_kernel__public_health_flexibility_reading, coexists_with).
narrative_ontology:cs_axiom('ad21aaa9-1f34-4bc9-8b15-5763e7342a98', foundational, patent_exclusivity_primary_innovation_mechanism).
narrative_ontology:cs_axiom_status(patent_exclusivity_primary_innovation_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('ad21aaa9-1f34-4bc9-8b15-5763e7342a98', patent_exclusivity_primary_innovation_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('ad21aaa9-1f34-4bc9-8b15-5763e7342a98', foundational, compulsory_licensing_exception_not_standard_right).
narrative_ontology:cs_axiom_status(compulsory_licensing_exception_not_standard_right, holdable).
narrative_ontology:cs_axiom_grounding('ad21aaa9-1f34-4bc9-8b15-5763e7342a98', compulsory_licensing_exception_not_standard_right, deontological).
narrative_ontology:cs_reference_frame('ad21aaa9-1f34-4bc9-8b15-5763e7342a98', patent_holder_enforcement_primacy).
narrative_ontology:cs_drift_state('ad21aaa9-1f34-4bc9-8b15-5763e7342a98', contemporary_post_doha_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ad21aaa9-1f34-4bc9-8b15-5763e7342a98', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, multinational_pharmaceutical_firms).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, high_income_country_governments).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_populations).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_country_governments).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME PATIENT ACCESS (SNARE) — Trapped by TRIPS enforcement mechanisms. Cannot exit the high-price regime without violating trade agreements; cannot access generics due to patent enforcement. Suppression is maximum: legal barriers (patent enforcement), economic barriers (high prices), and institutional barriers (trade retaliation against countries attempting compulsory licensing). No coordination function exists for this agent — the constraint extracts fully without providing offsetting benefit. Maximum experienced extraction.
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__strong_exclusivity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOW-INCOME COUNTRY GOVERNMENT (SNARE) — Constrained by trade dispute resolution mechanisms. Legally can invoke Article 31 compulsory licensing, but faces retaliation risk via WTO panels (US, EU have historically challenged compulsory licensing as excessive). Career/institutional pressure: health ministers who invoke compulsory licensing face trade sanctions and capital flow restrictions. Exit cost is high but nominally available — classifies as constrained exit within snare range. Extraction is severe: constrained ability to deploy own policy instruments to serve own population.
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__strong_exclusivity_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MULTINATIONAL PHARMACEUTICAL FIRMS (ROPE) — Experiences TRIPS as pure coordination mechanism. The constraint solves their licensing and enforcement problem: uniform patent rights enable price discrimination across markets, recoup R&D costs through high-income markets, and block generic competition. Arbitrage exit options are maximal: can shift production, licensing, and R&D location in response to policy changes. This agent perceives genuine coordination — TRIPS creates the enforcement infrastructure they require. No suppression from their perspective; the constraint enables their activity. Net beneficiary with maximum flexibility.
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__strong_exclusivity_reading, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-INCOME COUNTRY GOVERNMENTS (TANGLED ROPE) — Experience genuine coordination (TRIPS enables domestic pharma industry) AND asymmetric extraction (their own populations face higher-than-generics prices; patent-driven inefficiency in healthcare markets). Exit is constrained by trade agreements and domestic pharma lobbying. Time horizon is generational because TRIPS reforms require consensus among 164 WTO members. This agent perceives the constraint as serving both coordination (industry protection) and extraction (price premium capture) simultaneously. Moderate suppression from domestic political constraints around trade policy.
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: WTO DISPUTE RESOLUTION (PITON) — The formal mechanism for adjudicating TRIPS compliance has become largely performative. Most disputes occur in diplomatic backchannels before formal panels are convened; formal panel decisions are often technical rather than substantive (confirming what parties already negotiated). The theater persists due to institutional inertia and the requirement that countries maintain appearance of rule-of-law compliance, but actual constraint enforcement is negotiated bilaterally. Theater ratio is high because formal procedures are followed despite low actual influence on outcomes. The mechanism persists because no alternative adjudication system exists, not because it functions well.
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__strong_exclusivity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LOW-INCOME COUNTRY COALITION AND HEALTH NGOS (TANGLED ROPE) — Organized agents (WHO, Médecins Sans Frontières, coalition of low-income country trade ministers) have generated genuine coordination around health access: TRIPS flexibilities (compulsory licensing, parallel imports, Doha Declaration) emerged from their activism and provide some functional benefit. However, these flexibilities are narrowly construed by this reading's framework, producing asymmetric extraction. The coalition has agency but faces institutional suppression from dispute settlement risk and pharma lobbying. Exit is constrained but not trapped — coalition can influence but not determine WTO outcomes.
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INEVITABLE TRADE-OFF VIEW (MOUNTAIN) — From civilizational timescale, the strong-exclusivity reading frames patent protection as an immutable natural law of innovation: without exclusivity, pharmaceutical R&D investment cannot be recouped, so incentives collapse universally. This perspective sees the patent-access trade-off as structurally inevitable rather than contingent. However, this classification is a FALSE SUMMIT: empirical evidence (generic manufacturing in India, vaccine innovation without patent monopolies, open-source drug discovery) contradicts the universality claim. The mountain framing naturalizes what is actually an interpretive choice grounded in specific assumptions about incentive mechanisms.
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__strong_exclusivity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__strong_exclusivity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__strong_exclusivity_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, TR),
    TR >= 0.70.

:- end_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The strong exclusivity reading produces significant extraction asymmetry: patent holders capture monopoly rents (price premiums above generic manufacturing cost), generics are constrained or prohibited, and low-income populations face high prices or no access. The extractiveness is not maximal (0.70+) because some functional coordination exists: TRIPS does solve the legitimate multinational licensing problem, and some innovation incentive is genuine. The intermediate value reflects the tangled nature of the constraint. The measurement trajectory (0.42→0.58 over 20 years) shows accumulating extraction as TRIPS enforcement has matured through dispute panels and bilateral pressure, particularly around generic manufacturing restrictions and patent term extensions. Suppression (0.68): Moderate-high. The constraint maintains suppression through multiple mechanisms: legal barriers (patent enforcement + WTO dispute threat), economic barriers (high prices), institutional barriers (retaliation risk for countries invoking Article 31), and informational barriers (patent landscape complexity limits knowledge of flexibilities). The measurement trajectory (0.55→0.68) shows increasing enforcement suppression as WTO panels have intervened in disputes around compulsory licensing (Thailand, Brazil, South Africa challenges) and dispute threat has deterred deployment. Theater ratio (0.55): Moderate. WTO dispute resolution maintains formal rule-of-law appearance (formal panels, written decisions, appeals mechanism) but most outcomes are negotiated bilaterally before panels convene or after preliminary rulings. The theater persists because formal procedures satisfy legitimacy requirements and provide cover for negotiated settlements, not because they determine outcomes. Theater has been stable (0.48→0.55) because formal procedures are required but actual influence is exercised through other channels.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Multinational pharma firms see pure coordination (Rope) — TRIPS solves their licensing problem without visible suppression from their position. High-income governments see mixed coordination and extraction (Tangled Rope) — industry protection is real but healthcare costs are politically visible. Low-income governments see constrained snare (moderate/constrained) — they have legal exit options but retaliation risk makes them inaccessible. Low-income populations see maximum snare (powerless/trapped) — no legal exit, no resources for high prices, no voice in policy. The organized coalition sees tangled rope with activism function (organized/constrained) — they have generated real flexibilities (Doha Declaration, Article 31 precedents) but face continuing suppression. The WTO mechanism sees piton — formal procedures persist despite low actual influence. The analytical observer risks seeing mountain — patent-access trade-off as inevitable — but structural data reveals this as false summit: the reading itself makes the constraint appear inevitable; the sibling reading produces different structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's effective extraction (chi) is computed from base extractiveness (0.58), their power atom's directionality value (d), and scope modifier. Multinational pharma firms (powerful/arbitrage) have low d (≈0.48) and benefit from the constraint, producing negative chi (they are subsidized by the regime). Low-income populations (powerless/trapped) have high d (≈0.95) and bear costs, producing high chi (≈1.02 at global scope). Low-income governments (moderate/constrained, victim status) have d≈0.65, producing chi≈0.75 (severe experienced extraction). The magnitude gap between beneficiaries and victims reveals the asymmetric structure: the same constraint produces net subsidy for one group and severe extraction for another. The directionality derivation shows why this is tangled rope (mixed coordination + extraction) rather than pure rope or pure snare: genuine coordination for multinational pharma coexists with genuine extraction for low-income populations.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_incentive_magnitude_empirical,
    'What share of pharmaceutical innovation is causally driven by TRIPS-strength patent exclusivity versus other mechanisms (government R&D funding, university research, philanthropic grants, reputation/priority)?',
    'Comparative analysis of innovation rates in therapeutic areas with different patent regimes; counterfactual modeling of R&D investment under compulsory licensing; historical analysis of pre-TRIPS pharmaceutical innovation rates in high-income countries',
    'If TRIPS exclusivity drives >70% of innovation: strong exclusivity reading is justified by innovation necessity. If <40%: innovation incentive claim is overstated, and the constraint is primarily extractive rent-seeking, shifting classification toward snare. Current evidence suggests 30-50% causation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_magnitude_empirical, empirical, 'Share of pharmaceutical innovation causally driven by TRIPS-strength patent exclusivity').

omega_variable(
    compulsory_licensing_effectiveness_constraint,
    'What is the structural capacity of low-income countries to deploy Article 31 compulsory licensing? What proportion of attempts face WTO challenges or diplomatic retaliation?',
    'Audit of Article 31 invocations since 1995; tracking of WTO dispute filings and outcomes; analysis of trade/investment consequences for countries invoking compulsory licensing; comparison of theoretical vs actual availability of flexibilities',
    'If >80% of legitimate compulsory licensing attempts face retaliation: flexibilities are theater, and extraction is maximal (snare classification strengthens). If <20%: flexibilities are functional, and tangled_rope classification is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compulsory_licensing_effectiveness_constraint, empirical, 'Actual availability and cost of Article 31 compulsory licensing deployment').

omega_variable(
    reading_foreclosure_alternative_mechanism,
    'Would adoption of the public_health_flexibility_reading logically foreclose the strong_exclusivity_reading, or do both remain available for different parties?',
    'Formal analysis of whether the core premises of each reading are mutually exclusive within a single legal framework, or whether they represent different interpretive positions compatible with the same treaty text',
    'If readings foreclose each other: the kernel contains a genuine logical contradiction requiring resolution. If readings coexist: both are live interpretive positions (coexists_with relation). Current structure suggests coexistence with influence asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_alternative_mechanism, conceptual, 'Whether strong exclusivity and public health flexibility readings logically foreclose each other').

omega_variable(
    developing_country_generic_capacity_exogeneity,
    'Is the current capacity of low-income countries to manufacture generics a genuine structural capability or a contingent artifact of pre-TRIPS IP regimes? Would capacity persist if strong exclusivity reading is enforced for another generation?',
    'Historical analysis of generic manufacturing infrastructure development (India, Brazil pharmaceutical sectors); modeling of R&D investment patterns under extended TRIPS enforcement; identification of irreversible capability losses vs temporary capacity constraints',
    'If capacity is structurally contingent on continued access to imported APIs and technical knowledge: enforcing strong exclusivity reading will degrade capability, locking low-income countries into permanent drug importation (increases suppression and deepens snare). If capacity is self-sustaining: moderate impact on extraction levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_country_generic_capacity_exogeneity, empirical, 'Structural sustainability of generic manufacturing capacity in low-income countries under strong exclusivity').

omega_variable(
    reading_kernel_boundary_stability,
    'Is the TRIPS text itself sufficient to ground the strong exclusivity reading, or does enforcement require supplementary interpretation via dispute panels and diplomatic pressure?',
    'Textual analysis of TRIPS Articles 27, 28, 31 (patent scope and compulsory licensing); comparison of plain-text interpretation with dispute panel precedents; identification of divergence between treaty language and enforcement practice',
    'If plain text supports strong reading: authority grounding is ''fixed_text'', kernel_codification is ''formalized''. If plain text is ambiguous and enforcement depends on panel interpretation: authority grounding is ''extraction'' (adjudicating bodies extract legitimacy from text they interpret), kernel_codification is ''distributed''. Current evidence suggests distributed codification with extraction-grounded authority through WTO panels.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_boundary_stability, conceptual, 'Sufficiency of TRIPS text to ground strong exclusivity reading without supplementary interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 1995, 2015).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trips_theater_1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(trips_theater_2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(trips_theater_2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(trips_extractiveness_1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(trips_extractiveness_2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(trips_extractiveness_2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(trips_suppression_1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(trips_suppression_2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(trips_suppression_2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.18).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel__public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_price_discrimination).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, access_to_essential_medicines_framework).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, compulsory_licensing_deployment_capacity).

% DUAL FORMULATION NOTE:
% CONSTRAINT FAMILY: TRIPS Agreement Interpretive Kernel. This story is the STRONG_EXCLUSIVITY_READING (beneficiary-centered interpretation; high extractiveness). Sibling story trips_agreement_interpretive_kernel__public_health_flexibility_reading is the PUBLIC_HEALTH_FLEXIBILITY_READING (victim-centered interpretation; lower extractiveness). Both stories rest on identical treaty text but reach opposite conclusions about what TRIPS primarily mandates. Decomposition follows ε-invariance principle: strong exclusivity reading produces ε≈0.58 (tangled rope); public health flexibility reading produces lower ε (likely rope or piton depending on flexibilities' empirical effectiveness). The readings are not measurement alternatives — they are irreducibly different interpretive stances on the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
