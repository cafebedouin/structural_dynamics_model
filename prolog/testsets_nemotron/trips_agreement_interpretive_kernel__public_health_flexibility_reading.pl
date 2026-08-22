% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__public_health_flexibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: trips_agreement_interpretive_kernel__public_health_flexibility_reading
 *   human_readable: TRIPS Public Health Flexibility Reading (Compulsory Licensing & Parallel Imports)
 *   domain: international_trade_law/public_health_policy/intellectual_property_regime
 *
 * SUMMARY:
 *   The TRIPS Agreement's public health flexibilities (Article 31 compulsory
 *   licensing, Article 6 exhaustion/parallel imports, Doha Declaration
 *   paragraph 4, paragraph 6 export system) constitute a constraint reading
 *   that reinterprets uniform IP obligations as containing embedded public
 *   health safeguards. This reading empowers governments to override patent
 *   monopolies for essential medicines, creating a coordination mechanism for
 *   global health access while extracting value from patent holders'
 *   exclusivity. The constraint is claimed as tangled_rope: genuine
 *   coordination (solving the access/innovation tension) combined with
 *   asymmetric extraction (patent holders bear costs, generic manufacturers
 *   and health systems benefit).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.38).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.42).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "TRIPS Public Health Flexibility Reading (Compulsory Licensing & Parallel Imports)").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "international_trade_law/public_health_policy/intellectual_property_regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '457921cd-758d-47fa-8b1e-7cd7a8b84775').
narrative_ontology:cs_kernel_codification('457921cd-758d-47fa-8b1e-7cd7a8b84775', formalized).
narrative_ontology:cs_authority_grounding('457921cd-758d-47fa-8b1e-7cd7a8b84775', lineage).
narrative_ontology:cs_interpretation_layer_present('457921cd-758d-47fa-8b1e-7cd7a8b84775').
narrative_ontology:cs_reading_relation('457921cd-758d-47fa-8b1e-7cd7a8b84775', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_reading_relation('457921cd-758d-47fa-8b1e-7cd7a8b84775', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('457921cd-758d-47fa-8b1e-7cd7a8b84775', foundational, public_health_primacy_over_ipr_in_essential_medicines).
narrative_ontology:cs_axiom_status(public_health_primacy_over_ipr_in_essential_medicines, holdable).
narrative_ontology:cs_axiom_grounding('457921cd-758d-47fa-8b1e-7cd7a8b84775', public_health_primacy_over_ipr_in_essential_medicines, deontological).
narrative_ontology:cs_axiom('457921cd-758d-47fa-8b1e-7cd7a8b84775', foundational, compulsory_licensing_as_structural_flexibility_not_exception).
narrative_ontology:cs_axiom_status(compulsory_licensing_as_structural_flexibility_not_exception, holdable).
narrative_ontology:cs_axiom_grounding('457921cd-758d-47fa-8b1e-7cd7a8b84775', compulsory_licensing_as_structural_flexibility_not_exception, conventional).
narrative_ontology:cs_reference_frame('457921cd-758d-47fa-8b1e-7cd7a8b84775', doha_declaration_paragraph_4_framework).
narrative_ontology:cs_drift_state('457921cd-758d-47fa-8b1e-7cd7a8b84775', post_covid19_trips_waiver_debate, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('457921cd-758d-47fa-8b1e-7cd7a8b84775', '2026-08-03T14:22:11Z').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_manufacturers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patient_populations).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, originator_pharma_companies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce affordable generic versions of patented medicines using compulsory licenses; gain market access in developing countries and export markets under paragraph 6 system. Must navigate complex regulatory approvals and legal challenges from patent holders. Pay licensing royalties (though often minimal) and bear production costs. Exit means abandoning product lines and regulatory investments.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_manufacturers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_manufacturers, payer).

% Use compulsory licensing and parallel import provisions to procure essential medicines at lower prices for national health programs. Negotiate with patent holders from stronger position (threat of CL). Administer the procedural requirements for issuing licenses. Exit means returning to monopoly pricing — politically costly but administratively simple.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries, agenda_setter).

% Gain access to life-saving treatments that would otherwise be unaffordable. No meaningful exit from the constraint — they are the ultimate beneficiaries but have no agency in its operation. Their situation improves when the flexibility is used; deteriorates when political pressure blocks its use.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patient_populations, beneficiary,
    powerless, immediate, trapped, local).

% Face erosion of market exclusivity and pricing power in jurisdictions that invoke flexibilities. Experience revenue loss from generic competition and parallel imports. Invest heavily in legal challenges, lobbying, and bilateral pressure to limit flexibility use. Exit means accepting generic competition or withdrawing from markets — both costly.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders, payer).

% Bear the brunt of R&D cost recovery arguments; claim flexibilities undermine innovation incentives. Deploy legal, diplomatic, and commercial strategies to narrow flexibility interpretation (e.g., challenging 'national emergency' thresholds, opposing paragraph 6 system). Can shift R&D portfolios, adjust global pricing tiers, or pursue trade secret strategies as partial exits — but patent system remains core to business model.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, originator_pharma_companies, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, originator_pharma_companies, agenda_setter).

% Adjudicate disputes over TRIPS flexibility interpretation (e.g., Canada – Patent Protection, Brazil – Patent Protection cases). Their rulings define the operational boundaries of the flexibility reading. Not directly extracting or paying, but their interpretive authority shapes the constraint's effective scope.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_dispute_panels, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_dispute_panels, observer).

% Advocate for broad flexibility interpretation; monitor and pressure governments to use CL provisions. Excluded from formal WTO dispute proceedings (no standing). Provide technical assistance to health ministries. Exit means shifting advocacy focus — low organizational cost but high mission cost.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, civil_society_ngos, observer,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, civil_society_ngos, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global access to essential medicines by establishing a legal baseline that permits governments to override patent monopolies for public health, while preserving the patent system's general structure. Solves the collective action problem of ensuring life-saving drugs are not priced beyond reach in low- and middle-income countries.
% TRANSFER_FUNCTION: Transfers pricing power and market exclusivity from pharmaceutical patent holders (originator companies) to generic manufacturers and health ministries, enabling lower drug prices that flow to patient populations. The transfer is partial and conditional — patent holders retain rights in non-flexibility-invoking jurisdictions and receive nominal royalties.
% ABSENT_VOICES: Patients in countries that do not use flexibilities (due to political pressure, lack of manufacturing capacity, or trade threats) — their absence from the constraint's operation means they bear monopoly pricing without the coordination benefit. Also absent: future patients who may face reduced innovation if R&D investment responds to weakened exclusivity (a contested empirical claim).
% DISAPPEARANCE_RATIONALE: If the public health flexibility reading vanished (e.g., WTO ruled flexibilities extremely narrow or abolished), generic manufacturers would lose legal basis for production, health ministries would lose negotiating leverage, and patient populations in LMICs would face monopoly pricing for patented essential medicines. The global access architecture built around Doha and paragraph 6 would collapse. Patent holders would regain uniform global exclusivity — a substantial rearrangement of the pharmaceutical political economy.
% FOUNDING_PROBLEM: The TRIPS Agreement (1995) imposed uniform minimum IP standards globally, creating immediate concern that patent monopolies would block access to essential medicines in developing countries — particularly HIV/AIDS antiretrovirals in the late 1990s/early 2000s. The founding problem was: how to preserve the patent system's innovation incentives while preventing it from becoming a barrier to life-saving treatment access?
% FOUNDING_PROBLEM_CORROBORATION: WHO, Médecins Sans Frontières, and the UN Special Rapporteur on the Right to Health attest the access problem persists (new diseases, antimicrobial resistance, COVID-19 vaccines). Originator pharma and some trade economists attest the problem is substantially solved (prices have dropped, voluntary licenses exist) and the arrangement now primarily enables free-riding. The Doha Declaration (2001) and paragraph 6 decision (2003) are the negotiated compromises — corroborated by WTO members' consensus adoption.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__public_health_flexibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__public_health_flexibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.38) reflects that patent holders lose pricing power but retain core patent rights and receive royalties; the transfer is significant but not total. Suppression (0.42) is moderate — the constraint requires active enforcement (WTO dispute settlement, domestic legal frameworks) but alternatives (voluntary licenses, tiered pricing) are not fully suppressed. Theater (0.22) captures procedural complexity of CL issuance and paragraph 6 notifications that exceeds functional necessity. Accessibility collapse (0.55) is partial — voluntary licenses and differential pricing provide alternative paths. Resistance (0.68) is high — originator pharma and allies (US, EU, Switzerland historically) actively resist broad interpretation through bilateral pressure, TRIPS-plus agreements, and litigation.
 *
 * PERSPECTIVAL GAP:
 *   From the health ministry/generic manufacturer seat, this is a rope/tangled_rope — genuine coordination enabling access. From the originator pharma seat, it reads as snare — extraction justified by a coordination story that they argue is overstated (voluntary mechanisms suffice). From the patient seat, it is mountain-like — the difference between treatment access and death. The engine computes per-seat types from these structural positions; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Generic manufacturers and health ministries are structural beneficiaries (d ~0.2-0.3): they gain legal leverage and market access. Patient populations are identity-locked beneficiaries (d ~0.1): they cannot exit the need for medicines but have no agency. Patent holders and originator companies are structural targets (d ~0.7-0.8): they bear the extraction through lost exclusivity and pricing power, with constrained exit (market withdrawal is costly). WTO panels are analytical/agenda-setting (d ~0.5). Civil society is excluded observer — would benefit from broader interpretation but lacks formal standing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (HIV/AIDS access crisis) has evolved but not disappeared — new diseases, AMR, and pandemic vaccine inequity show the access/innovation tension persists. The arrangement has not atrophied into piton: it is actively invoked (COVID-19 CL discussions, cancer drug CLs in Malaysia, Chile, etc.). However, theater is rising as procedural complexity grows (paragraph 6 system used once in 20 years) — suggesting some drift toward performative maintenance. The mandate is not resolved; the constraint remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    flexibility_scope_vs_innovation_impact,
    'Does broad compulsory licensing interpretation measurably reduce pharmaceutical R&D investment for neglected diseases, or is the innovation impact negligible compared to market failure in LMICs?',
    'Longitudinal econometric analysis of R&D pipelines before/after major CL events (e.g., Thailand 2006-2008 CLs, COVID-19 IP waiver discussions), controlling for market size and disease burden.',
    'If innovation impact is substantial, the tangled_rope''s coordination function is genuinely contested — the extraction may undermine the very innovation that creates future medicines. If negligible, the extraction is largely rent redistribution without dynamic efficiency loss.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(flexibility_scope_vs_innovation_impact, empirical, 'Whether the extraction from patent holders has dynamic efficiency costs that erode the coordination function over time.').

omega_variable(
    paragraph_6_system_functionality,
    'Is the paragraph 6 export system (allowing generics export to countries without manufacturing capacity) a functional coordination mechanism or a performative theater that has been used once (Rwanda-Canada 2008)?',
    'Track utilization of the paragraph 6 system vs. alternative procurement channels (voluntary licenses, tiered pricing, donations) over 2003-2025; assess whether the system''s design creates structural barriers to use.',
    'If the system is structurally non-functional, its theater_ratio contribution is higher than measured — the flexibility reading contains a large performative component. If functional but underused due to political pressure, the suppression is higher on the beneficiary side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paragraph_6_system_functionality, empirical, 'Whether a key operational component of the flexibility reading is genuine coordination or theatrical maintenance.').

omega_variable(
    committer_framing_ambiguity,
    'Is the public_health_flexibility_reading a genuine textual interpretation of TRIPS/Doha, or a political construction that uses textual ambiguity to achieve redistribution?',
    'Compare WTO panel jurisprudence (Canada – Patent Protection, EC – Trademarks) on Article 31 and Doha interpretation against the flexibility reading''s claims; assess whether panels have endorsed broad or narrow construction.',
    'If panels consistently endorse narrow construction, the reading''s claimed_type (tangled_rope) may overstate its legal stability — it could be a contested political claim rather than an operative legal constraint. If panels endorse broad construction, the reading is legally grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_ambiguity, conceptual, 'Whether this reading''s structural legitimacy derives from legal text or political contestation — a committer-frame ambiguity about the kernel''s interpretive stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trips_ph_flex_tr_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 1995, 0.05).
narrative_ontology:measurement(trips_ph_flex_tr_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2001, 0.1).
narrative_ontology:measurement(trips_ph_flex_tr_t2003, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2003, 0.12).
narrative_ontology:measurement(trips_ph_flex_tr_t2010, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2010, 0.16).
narrative_ontology:measurement(trips_ph_flex_tr_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(trips_ph_flex_tr_t2025, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(trips_ph_flex_be_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 1995, 0.15).
narrative_ontology:measurement(trips_ph_flex_be_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2001, 0.22).
narrative_ontology:measurement(trips_ph_flex_be_t2003, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2003, 0.28).
narrative_ontology:measurement(trips_ph_flex_be_t2010, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2010, 0.32).
narrative_ontology:measurement(trips_ph_flex_be_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2020, 0.35).
narrative_ontology:measurement(trips_ph_flex_be_t2025, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(trips_ph_flex_su_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 1995, 0.25).
narrative_ontology:measurement(trips_ph_flex_su_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2001, 0.3).
narrative_ontology:measurement(trips_ph_flex_su_t2003, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2003, 0.35).
narrative_ontology:measurement(trips_ph_flex_su_t2010, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement(trips_ph_flex_su_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(trips_ph_flex_su_t2025, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.12).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, doha_declaration_paragraph_4_implementation).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, paragraph_6_export_system_operation).

% DUAL FORMULATION NOTE:
% Part of the TRIPS interpretive kernel family. This reading (public_health_flexibility) and strong_exclusivity_reading are sibling constraints with different ε (0.38 vs ~0.65) and different beneficiary/victim structures. Both are influenced by dispute_settlement_interpretive_authority. The decomposition follows ε-invariance: the 'TRIPS flexibilities' label covers two structurally distinct claims about what the text permits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__public_health_flexibility_reading, powerless, 0.15).
constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__public_health_flexibility_reading, powerful, 0.75).
constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__public_health_flexibility_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
