% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__strong_exclusivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: TRIPS Strong Exclusivity Reading — High Uniform Patent Protection with Narrow Flexibilities
 *   domain: international_trade_law/public_health_policy/intellectual_property_regime
 *
 * SUMMARY:
 *   The TRIPS Agreement (1995) established minimum global standards for
 *   intellectual property protection, including 20-year patent terms for
 *   pharmaceuticals. The strong_exclusivity_reading interprets TRIPS as
 *   mandating high uniform patent protections with narrowly construed
 *   flexibilities (compulsory licensing, parallel imports) — a reading
 *   advanced by research-based pharmaceutical companies and defended in WTO
 *   dispute settlement. This reading treats patent exclusivity as the primary
 *   engine of pharmaceutical innovation, positioning any dilution of patent
 *   rights as a threat to the global innovation system. The structural
 *   reality, however, reveals asymmetric extraction: patent holders
 *   (overwhelmingly headquartered in high-income countries) collect monopoly
 *   rents globally, while low-income country governments and patients bear
 *   the costs through unaffordable medicines and constrained public health
 *   budgets. The 2001 Doha Declaration attempted to rebalance by affirming
 *   flexibilities, but subsequent WTO jurisprudence and bilateral trade
 *   agreements have narrowed their practical scope. This constraint story
 *   captures the strong_exclusivity_reading as a kernel reading — one
 *   contested interpretation of the TRIPS text — distinct from the
 *   public_health_flexibility_reading that reads the same text as embedding
 *   broad public health safeguards.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.78).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.82).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Strong Exclusivity Reading — High Uniform Patent Protection with Narrow Flexibilities").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade_law/public_health_policy/intellectual_property_regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '0e03eccd-4e63-4c07-9a95-5acbf32b5492').
narrative_ontology:cs_kernel_codification('0e03eccd-4e63-4c07-9a95-5acbf32b5492', formalized).
narrative_ontology:cs_authority_grounding('0e03eccd-4e63-4c07-9a95-5acbf32b5492', lineage).
narrative_ontology:cs_interpretation_layer_present('0e03eccd-4e63-4c07-9a95-5acbf32b5492').
narrative_ontology:cs_reading_relation('0e03eccd-4e63-4c07-9a95-5acbf32b5492', trips_agreement_interpretive_kernel__public_health_flexibility_reading, coexists_with).
narrative_ontology:cs_axiom('0e03eccd-4e63-4c07-9a95-5acbf32b5492', foundational, uniform_high_patent_protection_mandated).
narrative_ontology:cs_axiom_status(uniform_high_patent_protection_mandated, holdable).
narrative_ontology:cs_axiom_grounding('0e03eccd-4e63-4c07-9a95-5acbf32b5492', uniform_high_patent_protection_mandated, conventional).
narrative_ontology:cs_axiom('0e03eccd-4e63-4c07-9a95-5acbf32b5492', foundational, narrow_flexibilities_exhaustive).
narrative_ontology:cs_axiom_status(narrow_flexibilities_exhaustive, holdable).
narrative_ontology:cs_axiom_grounding('0e03eccd-4e63-4c07-9a95-5acbf32b5492', narrow_flexibilities_exhaustive, conventional).
narrative_ontology:cs_axiom('0e03eccd-4e63-4c07-9a95-5acbf32b5492', secondary, patent_exclusivity_drives_pharma_innovation).
narrative_ontology:cs_axiom_status(patent_exclusivity_drives_pharma_innovation, holdable).
narrative_ontology:cs_axiom_grounding('0e03eccd-4e63-4c07-9a95-5acbf32b5492', patent_exclusivity_drives_pharma_innovation, instrumental).
narrative_ontology:cs_reference_frame('0e03eccd-4e63-4c07-9a95-5acbf32b5492', trips_uniform_protection_regime).
narrative_ontology:cs_drift_state('0e03eccd-4e63-4c07-9a95-5acbf32b5492', post_doha_declaration_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0e03eccd-4e63-4c07-9a95-5acbf32b5492', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, research_based_pharma_companies).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_country_governments).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_low_income_countries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_manufacturers).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patent_exclusivity_as_innovation_prerequisite).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__strong_exclusivity_reading, uniform_global_ip_standards_as_trade_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold patent portfolios covering essential medicines; lobby for and benefit from maximalist TRIPS interpretations at WTO and in bilateral agreements; enforce through trade dispute mechanisms and domestic courts; can shift R&D portfolios across jurisdictions to optimize returns.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders, beneficiary).

% Depend on patent monopoly periods to recoup R&D investments; fund clinical trials and regulatory approvals; benefit from TRIPS-mandated minimum 20-year terms and narrow compulsory licensing; can relocate manufacturing and pricing strategies across markets.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, research_based_pharma_companies, beneficiary,
    powerful, biographical, mobile, global).

% Face high drug prices for essential medicines; limited fiscal capacity to purchase patented drugs; constrained in issuing compulsory licenses by narrow TRIPS flexibilities and threat of trade retaliation; dependent on donor funding for treatment programs.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_country_governments, payer,
    organized, generational, constrained, national).

% Cannot afford patented medicines at monopoly prices; excluded from treatment when generics are blocked; no individual exit from the constraint — health needs are immediate and non-negotiable; reliant on government procurement or NGO supply chains that are themselves constrained by IP enforcement.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_low_income_countries, payer,
    powerless, biographical, trapped, local).

% Produce affordable versions of essential medicines; blocked from exporting to countries lacking manufacturing capacity by TRIPS Article 31(f) (pre-2003) and narrow Article 31bis implementation; face litigation and regulatory barriers in multiple jurisdictions; dependent on voluntary licenses or compulsory licensing windows.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_manufacturers, payer,
    moderate, biographical, constrained, global).

% Interpret TRIPS text authoritatively through binding dispute settlement; rulings shape the scope of flexibilities (e.g., Canada — Patent Protection, EC — Pharmaceutical Patents); enforcement backed by trade retaliation authorization; their interpretive stance determines whether flexibilities are broad or narrow in practice.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_panels, agenda_setter,
    institutional, generational, analytical, global).

% Campaign for broad TRIPS flexibilities, Doha Declaration implementation, and access to medicines; provide evidence at WTO and national levels; no direct enforcement power but shape political discourse and state positions.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_advocates, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a global minimum standard for patent protection that incentivizes pharmaceutical R&D investment by guaranteeing temporary monopoly returns, theoretically solving the underinvestment problem in drug development.
% TRANSFER_FUNCTION: Moves monopoly pricing power and rental income from low-income country health budgets and patients to patent-holding pharmaceutical companies, mediated through state enforcement of patent rights and trade dispute compliance.
% ABSENT_VOICES: Patients in low-income countries who cannot access medicines due to patent barriers are structurally excluded from WTO negotiations and TRIPS interpretation; their interests are represented only indirectly through NGOs and some government delegations. Generic manufacturers in exporting countries (e.g., India pre-2005) were excluded from the original TRIPS negotiating process.
% DISAPPEARANCE_RATIONALE: If the strong exclusivity reading vanished overnight, the interpretive space would shift to the public health flexibility reading — compulsory licensing would expand, generic competition would increase, drug prices in low-income countries would fall substantially, and the global pharmaceutical business model would reorganize around volume-based rather than monopoly-margin strategies.
% FOUNDING_PROBLEM: Pre-TRIPS, patent protection for pharmaceuticals varied wildly across countries — many developing nations excluded pharma from patentability entirely, creating free-rider problems and disincentivizing R&D for diseases affecting poor populations. TRIPS aimed to harmonize at a high standard to restore innovation incentives globally.
% FOUNDING_PROBLEM_CORROBORATION: Pharmaceutical industry and developed country governments attest the problem remains live — citing ongoing R&D costs and need for patent certainty. Public health experts, WHO, Médecins Sans Frontières, and generic manufacturers attest the founding problem is substantially solved for wealthy markets but the arrangement now extracts from poor populations without generating proportionate innovation for their disease burden; the 2001 Doha Declaration and 2017 UN High-Level Panel report corroborate the shifted-function reading from outside the beneficiary set.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__strong_exclusivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__strong_exclusivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
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
 *   Extractiveness is high (0.78) because the constraint transfers substantial monopoly rents from resource-constrained health systems to patent holders, with the gap between marginal production cost and monopoly price representing pure extraction. Suppression is very high (0.82) because the constraint's persistence depends on active enforcement: WTO dispute rulings (e.g., Canada — Patent Protection 2000, Brazil US dispute 2001), bilateral 'TRIPS-plus' provisions, and domestic IP enforcement machinery that block generic entry. Theater ratio (0.38) reflects that genuine innovation coordination exists (patent disclosure, R&D incentives) but a growing share of enforcement activity defends marginal extensions (evergreening, data exclusivity) rather than core innovation. Accessibility collapse (0.76) is high because alternatives — generic competition, parallel importation, government use licenses — are legally and politically constrained across most low-income jurisdictions. Resistance (0.68) is substantial: Doha Declaration, compulsory licensing uses (Thailand 2006-2008, Brazil 2007, India 2012), and sustained civil society campaigns, but has not shifted the dominant interpretive regime.
 *
 * PERSPECTIVAL GAP:
 *   From the patent holder seat, the constraint appears as a genuine coordination mechanism (rope) — they built the innovation ecosystem, TRIPS protects their investment, and flexibilities exist for emergencies. From the low-income government and patient seats, the same structure operates as a snare — coordination rhetoric covers extraction, flexibilities are theoretically available but practically unusable, and enforcement is asymmetric. The engine computes this divergence from the structural data: same constraint, different effective extraction (χ) because directionality (d) and exit options differ radically across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical patent holders and research-based companies are structural beneficiaries (d near 0.0): they collect the monopoly rents, set the enforcement agenda through industry associations (PhRMA, IFPMA), and have arbitrage-grade exit (global portfolios, jurisdictional shopping). Low-income country governments are payers with constrained exit (d ~0.7): they must comply or face trade retaliation, but can sometimes issue compulsory licenses at political cost. Patients in low-income countries are trapped payers (d ~0.95): no individual exit, immediate health needs, zero bargaining power. Generic manufacturers are constrained payers (d ~0.6): they bear compliance costs and litigation risk but retain some mobility across markets. WTO panels are agenda_setters with analytical exit — they interpret but do not directly collect rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (harmonizing patent standards to incentivize global pharma R&D) was live in 1995 but is now contested: the innovation incentives function for wealthy-market diseases but have demonstrably failed for neglected tropical diseases and antimicrobial resistance. The arrangement persists not because the founding problem remains uniformly live, but because the beneficiary coalition (pharma + high-income governments) has institutionalized the strong exclusivity reading through WTO jurisprudence and TRIPS-plus bilateralism. This is mandatrophy: the mandate (innovation incentive) has outlived its universal function, but the constraint persists through inertial maintenance by those who benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the strong_exclusivity_reading a genuine textual interpretation of TRIPS, or a constructed reading that serves pharmaceutical extraction interests?',
    'Comparative treaty interpretation analysis: examine Vienna Convention rules applied to TRIPS text, negotiating history, and subsequent practice. If the narrow-flexibility reading requires ignoring Article 8 (principles), Article 31 (compulsory licensing), and Doha Declaration paragraph 4, it is a constructed reading.',
    'If constructed, the reading is a false summit — claims mountain/rope status (inevitable treaty obligation) but operates as tangled_rope/snare with identifiable beneficiaries. Would trigger FSM reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the reading reflects treaty text or beneficiary interest').

omega_variable(
    innovation_evidence_gap,
    'Does strong patent protection in low-income countries actually generate proportional innovation for diseases affecting those populations?',
    'Empirical analysis of R&D pipelines: compare disease burden in low-income countries vs. clinical trial activity and new drug approvals for those conditions under strong vs. weak IP regimes.',
    'If no correlation, the coordination function (innovation incentive) is empirically unsubstantiated for the victim population — the constraint extracts without coordinating for their benefit, strengthening snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(innovation_evidence_gap, empirical, 'Whether the claimed coordination function operates for the extracted population').

omega_variable(
    flexibility_operationalization,
    'Are TRIPS flexibilities (compulsory licensing, parallel imports) practically usable under the strong_exclusivity_reading''s interpretive regime?',
    'Track compulsory licensing attempts and outcomes 1995-2024: success rates, time to implementation, trade retaliation incidence, and generic supply actually delivered.',
    'If flexibilities exist in text but are structurally inoperable (procedural hurdles, political pressure, supply chain barriers), the ''narrow but available'' claim is theater — the constraint is snare, not tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flexibility_operationalization, empirical, 'Whether narrow flexibilities are real or theoretical').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trips_strong_excl_tr_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(trips_strong_excl_tr_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2001, 0.18).
narrative_ontology:measurement(trips_strong_excl_tr_t2003, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2003, 0.22).
narrative_ontology:measurement(trips_strong_excl_tr_t2007, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2007, 0.28).
narrative_ontology:measurement(trips_strong_excl_tr_t2013, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2013, 0.33).
narrative_ontology:measurement(trips_strong_excl_tr_t2017, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2017, 0.36).
narrative_ontology:measurement(trips_strong_excl_tr_t2024, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(trips_strong_excl_be_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(trips_strong_excl_be_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2001, 0.52).
narrative_ontology:measurement(trips_strong_excl_be_t2003, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2003, 0.58).
narrative_ontology:measurement(trips_strong_excl_be_t2007, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2007, 0.65).
narrative_ontology:measurement(trips_strong_excl_be_t2013, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2013, 0.71).
narrative_ontology:measurement(trips_strong_excl_be_t2017, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2017, 0.75).
narrative_ontology:measurement(trips_strong_excl_be_t2024, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(trips_strong_excl_su_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(trips_strong_excl_su_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2001, 0.72).
narrative_ontology:measurement(trips_strong_excl_su_t2003, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2003, 0.75).
narrative_ontology:measurement(trips_strong_excl_su_t2007, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2007, 0.78).
narrative_ontology:measurement(trips_strong_excl_su_t2013, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2013, 0.8).
narrative_ontology:measurement(trips_strong_excl_su_t2017, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2017, 0.81).
narrative_ontology:measurement(trips_strong_excl_su_t2024, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2024, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.15).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel__public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, dispute_settlement_interpretive_authority).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, bilateral_trips_plus_provisions).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, essential_medicines_access_constraint).

% DUAL FORMULATION NOTE:
% TRIPS kernel decomposes into (at least) two readings: strong_exclusivity_reading (this story) and public_health_flexibility_reading. They share the same treaty text but instantiate different constraints with different ε, beneficiaries, and victims. The strong_exclusivity_reading claims the text mandates high uniform protection; the public_health_flexibility_reading claims the text embeds broad flexibilities. Their ε values differ substantially (this reading: ε≈0.78; sibling likely ε≈0.35). They are linked as a constraint family via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, institutional, 0.05).
constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, powerless, 0.95).
constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
