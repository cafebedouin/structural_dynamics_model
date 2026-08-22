% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__strong_exclusivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__strong_exclusivity_reading, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__strong_exclusivity_reading
 *   human_readable: TRIPS Strong Exclusivity Reading: Uniform Patent Protection Mandate
 *   domain: international_trade_law/intellectual_property/public_health
 *
 * SUMMARY:
 *   The TRIPS agreement is a contested kernel: the text permits multiple
 *   readings about the balance between patent protection and public health
 *   flexibility. This story instantiates the STRONG EXCLUSIVITY READING,
 *   which interprets TRIPS as mandating high uniform patent standards with
 *   narrow, grudging flexibilities for public health — compulsory licensing
 *   is rare and difficult; parallel imports are blocked; the dispute
 *   settlement system enforces this reading through trade retaliation. Under
 *   this reading, multinational pharmaceutical firms are structural
 *   beneficiaries (collecting monopoly rents globally), and low-income
 *   countries and patients are victims (facing inflated prices and
 *   constrained generic access). The alternative PUBLIC HEALTH FLEXIBILITY
 *   READING reads the same TRIPS text as embedding broad compulsory licensing
 *   and differential pricing authority — that reading would classify
 *   differently and produce lower extraction. Both readings are live in the
 *   institutional and policy landscape; this story models only the strong
 *   exclusivity reading and its structural consequences.
 *
 * KEY AGENTS:
 *   - Multinational pharmaceutical firms: institutional beneficiaries, enforce patent exclusivity through dispute settlement, collect monopoly rents on essential medicines globally.
 *   - Patent-holder countries (US, EU, Japan): powerful beneficiaries, maintain high domestic prices while capturing rents from patent enforcement globally; shape TRIPS interpretation through trade power.
 *   - Low-income countries: moderate-power victims, constrained by TRIPS from using compulsory licensing or parallel imports, face depleted public health budgets and pressure from patent-holder countries.
 *   - Patients in low-income markets: powerless, identity-locked victims, cannot access medicines at monopoly prices; no exit from their geographic and health constraints.
 *   - Generic manufacturers: excluded actors, would compete on price but are barred from most markets by patent enforcement; their participation would collapse the rent stream TRIPS protects.
 *   - WTO dispute panels: agenda-setter institutions, interpret TRIPS narrowly in favor of patent holders, enforce through trade sanctions that make compulsory licensing economically inaccessible to low-income countries.
 *   - Public health authorities: observer seat, lack direct authority to override patents without dispute risk, navigate the narrow public health exceptions the strong reading permits.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.81).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.77).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Strong Exclusivity Reading: Uniform Patent Protection Mandate").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade_law/intellectual_property/public_health").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '97f88689-710c-488e-b6b6-197a867fe197').
narrative_ontology:cs_kernel_codification('97f88689-710c-488e-b6b6-197a867fe197', fixed_text).
narrative_ontology:cs_authority_grounding('97f88689-710c-488e-b6b6-197a867fe197', extraction).
narrative_ontology:cs_interpretation_layer_present('97f88689-710c-488e-b6b6-197a867fe197').
narrative_ontology:cs_reading_relation('97f88689-710c-488e-b6b6-197a867fe197', trips_agreement_interpretive_kernel__public_health_flexibility_reading, coexists_with).
narrative_ontology:cs_axiom('97f88689-710c-488e-b6b6-197a867fe197', foundational, patent_protection_necessary_for_innovation).
narrative_ontology:cs_axiom_status(patent_protection_necessary_for_innovation, holdable).
narrative_ontology:cs_axiom_grounding('97f88689-710c-488e-b6b6-197a867fe197', patent_protection_necessary_for_innovation, empirically_contingent).
narrative_ontology:cs_axiom('97f88689-710c-488e-b6b6-197a867fe197', foundational, uniform_global_protection_required_for_incentive).
narrative_ontology:cs_axiom_status(uniform_global_protection_required_for_incentive, holdable).
narrative_ontology:cs_axiom_grounding('97f88689-710c-488e-b6b6-197a867fe197', uniform_global_protection_required_for_incentive, empirically_contingent).
narrative_ontology:cs_reference_frame('97f88689-710c-488e-b6b6-197a867fe197', trips_uniform_patent_exclusivity_mandate).
narrative_ontology:cs_drift_state('97f88689-710c-488e-b6b6-197a867fe197', contemporary_public_health_crisis_2020_plus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('97f88689-710c-488e-b6b6-197a867fe197', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, multinational_pharmaceutical_firms).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patent_holder_countries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_countries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_constrained_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from uniform high patent protection across all WTO member states. Can price discriminate globally while using trade enforcement to prevent parallel imports and compulsory licensing workarounds. Shape TRIPS interpretation through trade associations and dispute settlement participation. Collect monopoly rents on essential medicines in markets where they have patent exclusivity.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, multinational_pharmaceutical_firms, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__strong_exclusivity_reading, multinational_pharmaceutical_firms, agenda_setter).

% Wealthy countries with strong domestic pharmaceutical sectors (US, EU, Japan) benefit from TRIPS enforcement as it protects their firms' intellectual property globally and prevents generic competition from lower-cost manufacturing jurisdictions. Maintain high prices for domestic markets while capturing rents from patent enforcement.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patent_holder_countries, beneficiary,
    powerful, generational, arbitrage, global).

% Face inflated medicine prices because TRIPS restricts their ability to manufacture or import generics. Cannot use compulsory licensing without facing WTO dispute risk and trade retaliation. Public health budgets are depleted by patent monopolies on essential medicines. Exit options are limited: withdrawing from TRIPS would trigger trade sanctions; domestic manufacturing often violates patent terms; parallel imports are blocked by enforcement mechanisms.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_countries, payer,
    moderate, biographical, constrained, national).

% Cannot access patented medicines at monopoly prices. No exit: disease does not wait for compulsory licensing approval; patients either pay prices set by patent holders or go without treatment. Death from treatable disease due to price is the enforcement mechanism. Identity-locked to their location and health condition.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_constrained_markets, payer,
    powerless, immediate, trapped, local).

% Would manufacture low-cost generics in South Asia and elsewhere if compulsory licensing were available or parallel import permitted. Instead, excluded from most markets by patent enforcement. Can operate only where compulsory licenses are granted (narrow, contested); their participation would compete on price and undermine the patent monopoly that TRIPS protects.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_manufacturers, excluded,
    organized, biographical, constrained, global).

% Interpret TRIPS text and enforce interpretations through dispute resolution. Under the strong exclusivity reading, panels construe compulsory licensing and public health flexibilities narrowly, ruling in favor of patent holders when challenged. Their decisions carry enforceable sanctions (trade retaliation) against countries that diverge from the strong reading.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_panels, agenda_setter,
    institutional, generational, analytical, global).

% Advocate for compulsory licensing and medicine access but lack direct authority to override patent enforcement without WTO dispute risk. Navigate the narrow corridor the strong exclusivity reading permits (emergency declarations, government use) while facing political pressure from patent-holder countries if they invoke public health exceptions.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_authorities, observer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__strong_exclusivity_reading, multinational_pharmaceutical_firms).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__strong_exclusivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes uniform minimum patent standards globally to reduce regulatory arbitrage: pharmaceutical firms gain incentive to innovate knowing their patents will be protected across all major markets, avoiding the coordination problem of fragmented protection regimes that would allow easy copying in low-protection jurisdictions.
% TRANSFER_FUNCTION: Transfers rents from patients and low-income country governments to multinational patent-holding firms. The transfer mechanism is the restriction on generic manufacturing and parallel imports, enforced through dispute settlement. Patent monopolies allow prices 10–100x above marginal manufacturing cost in constrained markets.
% ABSENT_VOICES: Generic manufacturers and patients in low-income countries are structurally excluded from the negotiation that set TRIPS terms. They bear costs but had no seat at the Uruguay Round table. Public health authorities in low-income countries participate in WTO but lack leverage equivalent to pharmaceutical industry lobbying in wealthy countries.
% DISAPPEARANCE_RATIONALE: If this reading (strong exclusivity) disappeared and were replaced by the public health flexibility reading, generic competition would surge in low-income markets, medicine prices would compress 50–90%, and the global drug development model would reorganize around differential pricing by market income level rather than uniform patent monopoly. Patent-holder firms would face immediate revenue loss; low-income countries would gain budget capacity for medicine access.
% FOUNDING_PROBLEM: Early TRIPS negotiation (1986–1994) was framed around incentivizing innovation by providing uniform global patent protection. Developed countries, under pharmaceutical industry pressure, sought to prevent developing countries from using compulsory licensing and local manufacturing to access medicines at affordable prices. The founding problem was stated as: innovation requires patent incentives; without global protection, firms will not invest in drug development.
% FOUNDING_PROBLEM_CORROBORATION: Patent-holder firms and developed country governments attest the problem is live: innovation is costly and risky. Public health organizations, generic manufacturers, and developing country governments attest the founding problem is overstated: patent incentives could coexist with compulsory licensing and differential pricing. Independent evidence (Ernst & Young studies, WHO reports, academic meta-analyses) shows innovation continues under flexible patent regimes and that price compression from generics does not eliminate development incentive in high-income markets. The founding problem's continued necessity is attested by beneficiaries, not by independent external sources.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__strong_exclusivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__strong_exclusivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) and rising because the constraint's effect is to transfer rents from patients and governments to patent holders via price monopolies; the rent transfer grows as global middle classes expand and as patent-holder firms extend exclusivity through dispute wins and evergreening. Suppression is high (0.77) because the constraint persists through active enforcement: WTO panels must rule, trade retaliation must be threatened, compulsory licensing must be narrowly construed, and generic manufacturers must be excluded from most markets. Theater is moderate (0.42) and rising: the rhetoric emphasizes innovation incentive and public health balance, but growing share of enforcement activity is pure rent defense against generic competition that would not harm innovation in high-income markets. The measurement series tracks TRIPS enforcement over 31 years, showing extraction accumulation as the strong exclusivity reading entrenches and alternative readings face institutional pressure. One shared time grid: every metric authored at every historical point.
 *
 * PERSPECTIVAL GAP:
 *   From the pharmaceutical firms' and patent-holder countries' seat, TRIPS is genuine coordination that incentivizes global innovation and solves the coordination problem of fragmented patent regimes. From the low-income country and patient seat, the same structure is enforced extraction: the constraint persists because it benefits patent holders, and alternatives (compulsory licensing, parallel imports, differential pricing) are suppressed by dispute settlement. The engine should compute different per-seat types: the beneficiary seats may score rope-ish (coordination justification), while the payer seats compute snare-ish (extraction, suppression, constrained exit). That divergence is the measurement the system exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Patent-holding firms and their home countries are structural beneficiaries: they collect monopoly rents globally and use dispute settlement to exclude competitors. Their directionality (d) is near 0.0 — the constraint subsidizes their position. Low-income countries and patients are structural targets: they pay through inflated prices and constrained access. Their d is near 1.0 — the constraint extracts from them. Low-income countries have moderate power (can negotiate, form coalitions) and constrained exit (TRIPS membership is economically necessary; withdrawal triggers retaliation). Patients are powerless and trapped (disease does not wait; identity-locked to geography and health condition). Generic manufacturers are excluded rather than coordinated — their exclusion is the enforcement object itself. WTO panels have institutional power and analytical perspective but are agenda-setters: their narrow TRIPS interpretations shape what extraction looks like on the ground.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (innovation incentive requires uniform patent protection) was live in 1995 but has become contested. Evidence since 2000 shows innovation continues in therapeutic areas where generic competition is permitted and pricing is regulated (EU model). Patent-holder firms continue to assert the founding problem is live; public health and development organizations assert it is dead or overstated. The constraint persists because it benefits patent-holder firms and their home countries, not because the founding problem is uncontested. This is a classic mandatrophy signature: founding problem status = dead (in the public health reading) or contested (across the two readings), but disappearance verdict = world_rearranges (low-income countries would reorganize around generics if the strong reading were replaced), yet the constraint persists via dispute enforcement and the attendant theater ratio (rising from 0.22 to 0.42) — the performance of 'innovation incentive' work grows as the founding problem's actual necessity diminishes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_empirical_status,
    'Is innovation in pharmaceuticals substantially dependent on uniform global patent protection at the TRIPS strength level, or would differential pricing and compulsory licensing regimes produce equivalent innovation in high-income markets while permitting generic access in low-income markets?',
    'Comparative analysis of innovation rates and drug development pipelines in jurisdictions with different patent regimes (EU regulated prices, India compulsory licensing pre-2005, Australia PBS differential access). Longitudinal tracking of drug approval rates and therapeutic advance under the public health flexibility reading (if adopted).',
    'If differential pricing + compulsory licensing sustains innovation in high-income markets (high confidence evidence), the founding problem becomes overstated or false, and the strong exclusivity reading loses its core legitimacy claim. The constraint would reclassify as pure extraction (snare) rather than coordination with extraction (tangled rope). If uniform protection is necessary for innovation, the reading''s coordination claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_empirical_status, empirical, 'Whether TRIPS-strength uniform patent protection is necessary for pharmaceutical innovation or whether differential pricing regimes permit equivalent innovation.').

omega_variable(
    reading_foreclosure_ambiguity,
    'Does the strong exclusivity reading logically foreclose the public health flexibility reading within a single legal framework, or do the two readings coexist as competing live interpretations of the same TRIPS text?',
    'Close reading of TRIPS Article 31–32 language and WTO jurisprudence on compulsory licensing. If WTO panels have ruled that Article 31 permits broad compulsory licensing (as in the Doha Declaration), the readings coexist; if panels have narrowly construed Article 31 to require conditions the strong reading imposes (rare, burdensome, emergency-only), the readings diverge on what the text actually permits.',
    'If the readings foreclose each other, this constraint (strong exclusivity) cannot coexist with the public health flexibility constraint in the same institutional framework — one reading must win. If they coexist, both are live constraints simultaneously instantiated in different parties'' interpretations, and the network between them is one of institutional contest and regulatory capture (patent-holder countries enforce the strong reading through dispute settlement dominance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_ambiguity, conceptual, 'Whether the strong exclusivity and public health flexibility readings of TRIPS logically foreclose each other or coexist as live interpretive alternatives.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.77) of compulsory licensing primarily structural (legal barriers, dispute enforcement, trade retaliation threat) or has it become partially internalized (low-income country governments have adopted the strong reading as legitimate, even when it harms their populations)?',
    'Post-dispute behavioral analysis: when low-income countries face high WTO dispute costs for compulsory licensing, do they abandon the practice because they accept the strong reading, or because the retaliation threat makes it economically irrational? Qualitative research with public health authorities in low-income countries on their acceptance of TRIPS constraints.',
    'If suppression is mostly structural, compulsory licensing would surge if dispute enforcement were removed or weakened — the constraint''s persistence depends on active threat. If partially internalized, even dispute enforcement removal might not restore compulsory licensing quickly — the internalized legitimacy would persist. This affects the exit trajectory: structural suppression offers faster escape paths than internalized suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether TRIPS suppression of compulsory licensing is primarily structural (legal/retaliation barriers) or partially internalized as legitimate constraint.').

omega_variable(
    committer_reading_asymmetry,
    'This constraint instantiates the strong exclusivity reading of the TRIPS kernel. The sibling public health flexibility reading would classify the same underlying TRIPS arrangement differently (lower extraction, broader compulsory licensing, higher accessibility_collapse resistance from generic availability). Which reading is ''correct'' — or are both simultaneously operative in different institutional seats?',
    'Observation of WTO dispute panel decisions (panels enforce strong reading), Doha Declaration history (signals public health flexibility), and actual compulsory licensing practice in low-income countries (narrow, rare, contested). If dispute panels enforce strong exclusivity while low-income countries invoke public health flexibility, both readings are live.',
    'If the public health flexibility reading is institutionally operative in some seats (WIPO negotiations, some national courts) while the strong exclusivity reading dominates WTO dispute settlement, the constraint family is contaminated by institutional capture: the reading that determines enforceability (strong, via WTO sanctions) is not the reading that has legitimacy in other institutions. This is the contamination network''s core structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_asymmetry, conceptual, 'Whether this strong exclusivity reading or its public health flexibility sibling is the ''correct'' interpretation of TRIPS, or whether both are simultaneously operative in different institutional seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 1995, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2001, 0.28).
narrative_ontology:measurement(trip_tr_t2008, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2008, 0.33).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(trip_tr_t2026, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2001, 0.68).
narrative_ontology:measurement(trip_be_t2008, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2008, 0.74).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2015, 0.78).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2020, 0.8).
narrative_ontology:measurement(trip_be_t2026, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2026, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2001, 0.64).
narrative_ontology:measurement(trip_su_t2008, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2008, 0.7).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2015, 0.74).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2020, 0.76).
narrative_ontology:measurement(trip_su_t2026, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2026, 0.77).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.12).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel__public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_settlement_pharmaceutical_enforcement).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_drug_market_access_low_income).

% DUAL FORMULATION NOTE:
% The TRIPS agreement is a contested kernel with two principal readings instantiated as separate constraint stories: (1) STRONG EXCLUSIVITY READING (this story) — high uniform patent protection, narrow public health flexibilities, enforcement via WTO dispute settlement; (2) PUBLIC HEALTH FLEXIBILITY READING (sibling constraint) — broad compulsory licensing authority, differential pricing, patient access prioritized. The two readings interpret the same TRIPS text (Article 27–34) but reach opposite conclusions about the scope of national discretion on compulsory licensing and the strength of patent duration requirements. This story models the structural consequences of the strong reading (high extraction, patent-holder beneficiaries, low-income victims); the sibling story models the structural consequences of the public health reading (lower extraction, broader access, generic manufacturer participation). Both readings are live in the policy landscape; WTO dispute panels enforce the strong reading through trade sanctions. The network link indicates contamination: the strong reading's institutional dominance in dispute settlement suppresses the public health reading's practical implementation, even when the public health reading has legitimacy in public health institutions and developing country governments. ε differs between the readings: strong exclusivity reading measures high extraction (0.81); public health reading would measure lower extraction (estimated 0.40–0.50) due to broader compulsory licensing availability. This is not measurement ambiguity — it is two different constraints reading the same kernel differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
