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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: trips_agreement_interpretive_kernel__strong_exclusivity_reading
 *   human_readable: TRIPS Strong Exclusivity Reading: Uniform Patent Protection with Narrow Flexibilities
 *   domain: international_trade_law/public_health_policy/intellectual_property
 *
 * SUMMARY:
 *   The TRIPS Agreement (1995) established minimum global patent standards
 *   under WTO enforcement. The 'strong exclusivity reading' interprets TRIPS
 *   as mandating robust 20-year pharmaceutical patent protection with
 *   flexibilities (compulsory licensing, parallel imports) narrowly construed
 *   as exceptional derogations. This reading has been cemented through WTO
 *   dispute rulings (Canada – Patent Protection 2000, EC – Trademarks 2004)
 *   that limit Article 30 exceptions and Article 31 compulsory licensing. The
 *   Doha Declaration (2001) and Article 31bis amendment (2017) attempted to
 *   rebalance but operate within the strong exclusivity frame — they are
 *   procedural exceptions, not interpretive shifts. The constraint extracts
 *   via: (1) monopoly pricing on essential medicines in low-income markets,
 *   (2) suppression of generic competition through legal and enforcement
 *   barriers, (3) policy space foreclosure for public health measures.
 *   Beneficiaries: patent-holding firms, their home governments, WTO dispute
 *   system. Victims: low-income governments, patients, generic manufacturers.
 *   The engine will compute per-seat types from this structural data; the
 *   claimed_type (tangled_rope) reflects genuine coordination (uniform IP
 *   floor) fused with asymmetric extraction (health access transferred to
 *   patent rents).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.78).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.85).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Strong Exclusivity Reading: Uniform Patent Protection with Narrow Flexibilities").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade_law/public_health_policy/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'c95be8b3-5db9-4c51-88f4-16635fa567b7').
narrative_ontology:cs_kernel_codification('c95be8b3-5db9-4c51-88f4-16635fa567b7', formalized).
narrative_ontology:cs_authority_grounding('c95be8b3-5db9-4c51-88f4-16635fa567b7', extraction).
narrative_ontology:cs_interpretation_layer_present('c95be8b3-5db9-4c51-88f4-16635fa567b7').
narrative_ontology:cs_reading_relation('c95be8b3-5db9-4c51-88f4-16635fa567b7', trips_agreement_interpretive_kernel__public_health_flexibility_reading, influences).
narrative_ontology:cs_axiom('c95be8b3-5db9-4c51-88f4-16635fa567b7', foundational, patent_exclusivity_primary_innovation_engine).
narrative_ontology:cs_axiom_status(patent_exclusivity_primary_innovation_engine, holdable).
narrative_ontology:cs_axiom_grounding('c95be8b3-5db9-4c51-88f4-16635fa567b7', patent_exclusivity_primary_innovation_engine, instrumental).
narrative_ontology:cs_axiom('c95be8b3-5db9-4c51-88f4-16635fa567b7', foundational, flexibilities_as_narrow_exceptions).
narrative_ontology:cs_axiom_status(flexibilities_as_narrow_exceptions, holdable).
narrative_ontology:cs_axiom_grounding('c95be8b3-5db9-4c51-88f4-16635fa567b7', flexibilities_as_narrow_exceptions, conventional).
narrative_ontology:cs_reference_frame('c95be8b3-5db9-4c51-88f4-16635fa567b7', trips_original_bargain_1995).
narrative_ontology:cs_drift_state('c95be8b3-5db9-4c51-88f4-16635fa567b7', post_doha_and_article31bis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c95be8b3-5db9-4c51-88f4-16635fa567b7', '2026-06-12T14:23:00Z').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, high_income_country_governments).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_settlement_body).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_country_governments).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_low_income_countries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_manufacturers_in_developing_countries).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patent_exclusivity_as_innovation_engine).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__strong_exclusivity_reading, uniform_global_ip_standard).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__strong_exclusivity_reading, dispute_settlement_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold patent portfolios covering essential medicines; collect monopoly rents through TRIPS-mandated 20-year exclusivity enforced by WTO dispute settlement. Can shift R&D pipelines, licensing strategies, and pricing across jurisdictions to maximize returns. Their exit from any single market is trivial — they hold global portfolios and serve high-income markets as primary revenue base.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Home to most patent-holding pharmaceutical firms; shaped TRIPS negotiation and continue to defend strong exclusivity through bilateral pressure and WTO dispute initiation. Benefit from innovation ecosystems and tax revenue. Their exit from the constraint is not applicable — they are among its authors and enforcers.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, high_income_country_governments, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__strong_exclusivity_reading, high_income_country_governments, agenda_setter).

% Holds binding interpretive authority over TRIPS text; panels have consistently narrowed Article 31 compulsory licensing and Article 30 limited exceptions in favor of patent holders (e.g., Canada – Patent Protection, EC – Trademarks). Enforcement via authorized trade retaliation gives the constraint teeth. Their situation is interpreting and applying the text — they do not pay or collect, but their rulings lock in the strong exclusivity reading.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, analytical, global).

% Obligated to implement TRIPS-compliant patent laws despite limited pharmaceutical manufacturing capacity and severe public health needs. Face WTO dispute risk if they use flexibilities broadly (e.g., compulsory licenses for non-emergencies, parallel imports). Health budgets absorb high patented drug prices; fiscal space for domestic generic production is legally constrained. Exit means trade retaliation or bilateral pressure — politically and economically costly.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_country_governments, payer,
    moderate, biographical, constrained, national).

% Face unaffordable prices for patented essential medicines (HIV/AIDS, TB, malaria, cancer, hepatitis C). No individual exit — cannot import generics personally, cannot negotiate prices, cannot substitute treatments. Depend on government compulsory licenses or donor programs, both constrained by the strong exclusivity reading's narrow interpretation of TRIPS flexibilities. Bear the full human cost of the extraction.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_low_income_countries, payer,
    powerless, immediate, trapped, local).

% Capable of producing quality-assured generic medicines at a fraction of patented prices (e.g., Indian, Brazilian firms). TRIPS Article 31(f) originally restricted export under compulsory license; the 2003 waiver and 2017 Article 31bis amendment created a cumbersome pathway. The strong exclusivity reading treats these as exceptional, not standard — legal uncertainty and compliance costs suppress generic supply. Exit means shifting to non-pharma products or serving only domestic markets.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_manufacturers_in_developing_countries, payer,
    moderate, biographical, constrained, regional).

% Advocate for broad compulsory licensing, parallel imports, and technology transfer under TRIPS flexibilities (Doha Declaration, Article 31bis). Their voices enter WTO deliberations as NGO submissions but carry no formal standing in dispute settlement. They document access failures and pressure governments — but the constraint's interpretive machinery does not recognize them as parties.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_ngos_and_activists, excluded,
    organized, biographical, mobile, global).

% Analyze TRIPS text, dispute rulings, and public health impact. Produce the interpretive literature that both readings draw on. Their work informs but does not determine outcomes — the constraint operates through state power and WTO enforcement, not scholarly consensus.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, academic_ip_and_trade_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform global minimum standard of patent protection to solve the coordination problem of fragmented national IP regimes that allowed free-riding on innovation investments and created uncertainty for cross-border technology transfer.
% TRANSFER_FUNCTION: Moves monopoly pricing power and enforcement authority from low-income country governments and patients to pharmaceutical patent holders and their home governments, via WTO-mandated patent standards backed by trade retaliation. The transfer is: affordable generic competition → suppressed; high patented drug prices → sustained; policy space for public health → narrowed.
% ABSENT_VOICES: Patients in low-income countries (trapped, no standing), generic manufacturers in developing countries (constrained by legal uncertainty), public health NGOs (excluded from dispute settlement), and future generations facing antimicrobial resistance with empty pipelines — all would object to the narrow flexibility reading but are not parties to WTO dispute proceedings.
% DISAPPEARANCE_RATIONALE: If the strong exclusivity reading vanished overnight, low-income countries would immediately expand compulsory licensing and parallel imports; generic manufacturers would scale production for export under Article 31bis; drug prices for essential medicines would fall 80-95% in many markets; patent holders would lose monopoly rents in those markets but retain them in high-income markets; the global innovation incentive structure would shift toward prize funds, advance market commitments, and public R&D funding as complements to patent exclusivity.
% FOUNDING_PROBLEM: Pre-TRIPS era: widely varying national patent laws allowed some countries to exclude pharmaceuticals from patentability entirely, enabling generic production without compensation. This created uncertainty for pharmaceutical firms investing in R&D and was framed as free-riding on innovation financed by high-income markets.
% FOUNDING_PROBLEM_CORROBORATION: Pharmaceutical industry and high-income governments attest the problem remains live — they cite ongoing R&D costs, pipeline risks, and the need for predictable global IP. Public health scholars, Médecins Sans Frontières, WHO, and low-income country governments attest the founding problem is substantially altered: the innovation model fails neglected diseases regardless of IP strength; the access crisis is structural; and TRIPS flexibilities were intended as the solution, not the exception. The Doha Declaration (2001) and Article 31bis amendment (2017) are corroborating acts by WTO members themselves that the original bargain required rebalancing.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__strong_exclusivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__strong_exclusivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.78) is high because the constraint transfers massive value — monopoly rents on essential medicines in markets that would otherwise be served by generics at marginal cost — and the transfer is sustained by WTO enforcement. Suppression (0.85) is very high because alternatives (compulsory licensing, parallel imports, government use) are legally available but practically suppressed through dispute risk, procedural complexity (Article 31bis), and bilateral pressure. Theater ratio (0.32) is moderate: the coordination function (uniform IP standard) is real but a growing share of enforcement activity defends the payment exclusivity rather than innovation incentives per se. Accessibility collapse (0.68) reflects that once a country understands TRIPS obligations, the space for independent pharmaceutical policy is severely narrowed — but not eliminated (Doha, Article 31bis create narrow pathways). Resistance (0.42) is moderate: low-income countries and NGOs resist but face asymmetric power; the constraint persists because the cost of resistance (trade retaliation) exceeds the capacity of any single victim group.
 *
 * PERSPECTIVAL GAP:
 *   From the patent holder/institutional seat, the constraint is a rope — it solves the coordination problem of global IP fragmentation and funds innovation. From the patient/low-income government seat, it is a snare — the coordination story is cover for extraction that kills people. The engine computes this divergence from the declared beneficiaries, victims, power, and exit options. The strong exclusivity reading is not a mountain (it is a constructed legal regime, not natural law), not a pure rope (extraction is structural and asymmetric), not a scaffold (no sunset, no transition logic), not a piton (the function — innovation incentive — is actively defended, not atrophied). Tangled rope is the honest claim: genuine coordination fused with asymmetric extraction, requiring active WTO enforcement to hold.
 *
 * DIRECTIONALITY LOGIC:
 *   Patent holders and high-income governments are structural beneficiaries (d near 0.0): they collect the monopoly rents and set the enforcement agenda. The WTO dispute body is an agenda_setter (d ~ 0.2): it interprets and enforces but does not directly collect. Low-income governments are payers with constrained exit (d ~ 0.7): they bear fiscal and health costs, can use flexibilities only at high political risk. Patients are trapped payers (d ~ 0.95): no exit, bear full human cost. Generic manufacturers are constrained payers (d ~ 0.65): they have capability but face legal suppression. Public health NGOs are excluded (d not applicable): they would challenge but lack standing. Academic observers are analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented IP, free-riding on innovation) is contested as still live. The strong exclusivity reading prevents mislabeling the coordination function as pure extraction — the uniform IP floor is real and valued by innovators. But it also prevents mislabeling the extraction as mere coordination cost — the narrow construction of flexibilities, the procedural hurdles of Article 31bis, and the dispute rulings limiting exceptions reveal that the constraint's persistence depends on suppressing alternatives, not merely providing a coordination floor. The mandatrophy tension: the arrangement was built for an innovation model that increasingly fails neglected diseases; the constraint now extracts from populations that were never the intended beneficiaries of the innovation bargain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_incentive_vs_access_tradeoff,
    'Does the strong exclusivity reading''s narrowing of flexibilities actually increase global pharmaceutical innovation, or does it merely shift rents to patent holders while failing neglected disease R&D?',
    'Counterfactual analysis: compare R&D investment patterns for diseases affecting low-income populations under strong vs. flexible IP regimes; measure whether monopoly rents in low-income markets (which are small commercially) materially affect global R&D portfolios.',
    'If monopoly rents in low-income markets do not drive meaningful R&D, the extraction is pure rent with no coordination justification — the constraint collapses toward snare. If they do drive R&D, the tangled rope classification holds with a higher coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_vs_access_tradeoff, empirical, 'Whether the measured extraction corresponds to a real innovation incentive or is deadweight rent.').

omega_variable(
    interpretive_authority_lock_in,
    'Is the WTO dispute settlement body''s interpretive authority over TRIPS a genuine coordination mechanism for legal certainty, or has it become an extraction tool that forecloses the public health flexibility reading?',
    'Analyze dispute rulings post-Doha (2001): count rulings that expand vs. contract flexibility space; measure whether panel reasoning engages public health objectives or treats them as textual exceptions to be minimized.',
    'If the dispute body systematically forecloses the public health reading, the constraint''s enforcement machinery is biased toward extraction — supporting snare or tangled_rope with high suppression. If it balances both, the coordination function is more genuine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_authority_lock_in, empirical, 'Whether the interpretive authority is neutral or structurally tilted toward exclusivity.').

omega_variable(
    kernel_reading_relation,
    'Does the strong exclusivity reading logically foreclose the public health flexibility reading, or do they coexist as competing interpretations within the same WTO framework?',
    'Legal analysis: can a single WTO member simultaneously comply with both readings? The strong reading treats flexibilities as exceptional; the public health reading treats them as standard. A member invoking broad compulsory licensing would violate the strong reading but comply with the public health reading — they cannot be simultaneously satisfied in the same legal act.',
    'If forecloses: the kernel has no stable equilibrium — one reading must displace the other. If coexists_with: the tension is permanent and the dispute body''s authority determines outcomes case-by-case. If influences: the strong reading''s dominance creates pressure that narrows the public health reading''s operational space without eliminating it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relation, conceptual, 'Structural relationship between the two kernel readings.').

omega_variable(
    article_31bis_operational_futility,
    'Is the Article 31bis amendment (2017) a genuine expansion of generic export access, or a procedural trap that renders the flexibility practically unusable?',
    'Track actual uses of Article 31bis since 2017: number of notifications, licenses granted, generic shipments executed. Compare to pre-2003 waiver practice. Assess whether the notification, good faith negotiation, and remuneration requirements create prohibitive transaction costs.',
    'If operationally futile, the theater ratio is higher than measured — the flexibility exists as performance but not function. If functional, the strong reading''s suppression is partially mitigated by a working escape valve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_31bis_operational_futility, empirical, 'Whether the flagship flexibility mechanism actually works or is performative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trips_strong_excl_tr_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(trips_strong_excl_tr_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(trips_strong_excl_tr_t2003, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2003, 0.25).
narrative_ontology:measurement(trips_strong_excl_tr_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(trips_strong_excl_tr_t2017, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2017, 0.3).
narrative_ontology:measurement(trips_strong_excl_tr_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2020, 0.31).
narrative_ontology:measurement(trips_strong_excl_tr_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(trips_strong_excl_be_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 1995, 0.65).
narrative_ontology:measurement(trips_strong_excl_be_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2001, 0.72).
narrative_ontology:measurement(trips_strong_excl_be_t2003, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2003, 0.7).
narrative_ontology:measurement(trips_strong_excl_be_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2010, 0.74).
narrative_ontology:measurement(trips_strong_excl_be_t2017, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2017, 0.76).
narrative_ontology:measurement(trips_strong_excl_be_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2020, 0.77).
narrative_ontology:measurement(trips_strong_excl_be_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(trips_strong_excl_su_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement(trips_strong_excl_su_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2001, 0.82).
narrative_ontology:measurement(trips_strong_excl_su_t2003, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2003, 0.8).
narrative_ontology:measurement(trips_strong_excl_su_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2010, 0.83).
narrative_ontology:measurement(trips_strong_excl_su_t2017, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2017, 0.84).
narrative_ontology:measurement(trips_strong_excl_su_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2020, 0.85).
narrative_ontology:measurement(trips_strong_excl_su_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.12).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel__public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, dispute_settlement_interpretive_authority).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, medicines_patent_pool_governance).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, covid19_trips_waiver_debate).

% DUAL FORMULATION NOTE:
% This constraint and the public_health_flexibility_reading form a constraint family decomposed from the single label 'TRIPS pharmaceutical patent flexibilities.' The strong_exclusivity_reading has high extraction (ε=0.78) because it narrows flexibilities to exceptional derogations; the public_health_flexibility_reading would have lower extraction because it treats flexibilities as standard operating procedure. They share the same TRIPS text but instantiate different constraints with different beneficiary/victim structures. Linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, institutional, 0.15).
constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, moderate, 0.65).
constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
