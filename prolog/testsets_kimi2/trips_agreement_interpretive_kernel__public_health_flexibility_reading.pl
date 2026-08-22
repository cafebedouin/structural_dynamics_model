% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__public_health_flexibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: TRIPS Public Health Flexibility Reading (Compulsory Licensing and Parallel Importation)
 *   domain: international_trade_law/public_health/intellectual_property
 *
 * SUMMARY:
 *   This constraint instantiates the public health flexibility reading of the
 *   TRIPS agreement interpretive kernel: the claim that TRIPS Articles 6, 7,
 *   8, 30, and 31, as clarified by the Doha Declaration, embed broad
 *   permissive authority for compulsory licensing and parallel importation to
 *   protect public health. This reading is contested by the strong
 *   exclusivity reading, which treats the same text as mandating narrow
 *   exceptions to a high-protection default. The constraint operates through
 *   national health ministry action, WTO dispute panel interpretation, and
 *   generic manufacturer supply chains, transferring pricing power from
 *   pharmaceutical patent holders to public health systems.
 *
 * KEY AGENTS:
 *   - pharmaceutical_patent_holders: Primary payer (powerful/constrained) â bear lost exclusivity and pricing power
 *   - generic_manufacturers: Primary beneficiary (organized/constrained) â gain production rights under compulsory license
 *   - health_ministries: Agenda setter and beneficiary (institutional/constrained) â invoke flexibilities and capture budgetary relief
 *   - patients_in_need: Secondary beneficiary (powerless/trapped) â depend on flexibilities for access but lack agency
 *   - wto_dispute_panels: Analytical observer (institutional/analytical) â adjudicate compliance and stabilize the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.62).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.58).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "TRIPS Public Health Flexibility Reading (Compulsory Licensing and Parallel Importation)").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "international_trade_law/public_health/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '7ac85726-4b0d-4c0e-b4f0-9aecab085b87').
narrative_ontology:cs_kernel_codification('7ac85726-4b0d-4c0e-b4f0-9aecab085b87', fixed_text).
narrative_ontology:cs_authority_grounding('7ac85726-4b0d-4c0e-b4f0-9aecab085b87', lineage).
narrative_ontology:cs_interpretation_layer_present('7ac85726-4b0d-4c0e-b4f0-9aecab085b87').
narrative_ontology:cs_reading_relation('7ac85726-4b0d-4c0e-b4f0-9aecab085b87', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ac85726-4b0d-4c0e-b4f0-9aecab085b87', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, coexists_with).
narrative_ontology:cs_axiom('7ac85726-4b0d-4c0e-b4f0-9aecab085b87', foundational, compulsory_licensing_permissive_default).
narrative_ontology:cs_axiom_status(compulsory_licensing_permissive_default, holdable).
narrative_ontology:cs_axiom_grounding('7ac85726-4b0d-4c0e-b4f0-9aecab085b87', compulsory_licensing_permissive_default, conventional).
narrative_ontology:cs_axiom('7ac85726-4b0d-4c0e-b4f0-9aecab085b87', foundational, parallel_importation_non_violation).
narrative_ontology:cs_axiom_status(parallel_importation_non_violation, holdable).
narrative_ontology:cs_axiom_grounding('7ac85726-4b0d-4c0e-b4f0-9aecab085b87', parallel_importation_non_violation, conventional).
narrative_ontology:cs_reference_frame('7ac85726-4b0d-4c0e-b4f0-9aecab085b87', broad_flexibility_default).
narrative_ontology:cs_drift_state('7ac85726-4b0d-4c0e-b4f0-9aecab085b87', post_doha_declaration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ac85726-4b0d-4c0e-b4f0-9aecab085b87', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_manufacturers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patients_in_need).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, doha_declaration_on_trips_and_public_health).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, access_to_medicines_as_human_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Licensed under compulsory license regimes to produce patented medicines for domestic use and export to qualifying countries under the Paragraph 6 system. Their production is contingent on health ministry requests and WTO procedural compliance; they cannot independently invoke the flexibility.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_manufacturers, beneficiary,
    organized, biographical, constrained, global).

% Issue compulsory licenses and authorize parallel importation to reduce pharmaceutical expenditure and address public health emergencies. They operate under WTO procedural requirements and face bilateral trade pressure from developed countries when invoking flexibilities.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries, beneficiary).

% Hold patents on medicines subject to compulsory licensing and parallel importation under this reading. They experience revenue erosion and loss of market exclusivity in developing countries, and respond by lobbying for TRIPS-plus bilateral agreements that contract the flexibilities.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders, payer,
    powerful, biographical, constrained, global).

% Require patented medicines for treatment; depend on the invocation of TRIPS flexibilities to access affordable generic versions. They have no direct voice in WTO dispute proceedings or compulsory licensing negotiations.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patients_in_need, beneficiary,
    powerless, immediate, trapped, local).

% Interpret TRIPS text in disputes between member states; their rulings determine whether national compulsory licenses or parallel import regimes comply with WTO obligations, thereby stabilizing or destabilizing the public health flexibility reading.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_dispute_panels, observer,
    institutional, civilizational, analytical, global).

% Campaign for broad invocation of TRIPS flexibilities and document access gaps. They are not formal parties to WTO disputes or treaty interpretation but shape the political environment in which the reading operates.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, access_advocacy_ngos, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global access to essential medicines by allowing countries to override patent monopolies through compulsory licensing and parallel importation when public health is at risk, aligning generic production capacity with unmet health needs across borders.
% TRANSFER_FUNCTION: Transfers market exclusivity and pricing power from pharmaceutical patent holders to generic manufacturers and public health systems; moves affordable medicines from production sites to importing countries at prices below monopoly levels.
% ABSENT_VOICES: Patients in need without organized representation; generic manufacturers in least-developed countries lacking technical production capacity; public health advocates from states that acceded to TRIPS-plus bilateral agreements overriding the flexibilities.
% DISAPPEARANCE_RATIONALE: If this interpretive flexibility vanished, compulsory licensing would be legally untenable, generic production pipelines for patented drugs would close, and health ministries would lose formal leverage against monopoly pricing â the global access architecture would reorganize around patent-holder exclusivity.
% FOUNDING_PROBLEM: High prices of patented pharmaceuticals during the HIV/AIDS crisis placed life-saving medicines out of reach in developing countries, creating a public health emergency that strict patent exclusivity was seen as exacerbating.
% FOUNDING_PROBLEM_CORROBORATION: WHO, UNAIDS, and MÃ©decins Sans FrontiÃ¨res attest persistent access gaps and premature mortality from treatable conditions. Pharmaceutical industry associations and developed-country trade representatives contest the framing, arguing differential pricing and innovation incentives already address the problem; no outside corroboration is accepted by both sides.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__public_health_flexibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__public_health_flexibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects substantial but bounded transfer of monopoly rent: compulsory licensing requires adequate remuneration under Article 31(h), so extraction is not expropriation. Suppression (0.58) captures the active legal and political enforcement required to sustain the flexibility reading against patent-holder resistance and TRIPS-plus erosion. Theater ratio (0.42) acknowledges that declaratory flexibilities (Doha, Paragraph 6) exceed actual implementation due to manufacturing capacity gaps and political fear of retaliation. Accessibility collapse is low (0.30) because the strong exclusivity reading remains a live legal and political alternative. Resistance is high (0.72) due to sustained pharmaceutical industry lobbying, USTR 301 actions, and bilateral agreement proliferation.
 *
 * PERSPECTIVAL GAP:
 *   From the health ministry and generic manufacturer seats, the constraint is coordination â it solves the access problem by legalizing generic supply. From the pharmaceutical patent holder seat, the same structure operates as extraction â it strips exclusivity that incentivizes innovation. The engine computes this divergence from the structural role declarations: agenda_setter/beneficiary seats face low directionality while the payer seat faces high directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Generic manufacturers, health ministries, and patients are declared beneficiaries, placing their directionality near the subsidy end (low d). Pharmaceutical patent holders are declared victims (payer role), placing their directionality near the full-target end (high d). WTO dispute panels, as observers with analytical exit, sit near neutral. No override is needed: the beneficiary/victim declarations plus exit options (constrained for commercial parties, trapped for patients, analytical for panels) produce the correct structural derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids snare classification because it carries a genuine coordination function: without compulsory licensing and parallel importation, essential medicines would remain unaffordable in markets that patent holders do not serve at marginal cost. It avoids rope classification because the transfer is asymmetric: patent holders bear concentrated losses while beneficiaries are diffuse. The founding problem â HIV/AIDS access crises â is contested but not dead, so piton classification is inappropriate despite the theater ratio.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doha_constructed_vs_derived,
    'Does the broad public health flexibility reading derive from the original TRIPS treaty text, or was it constructed through the Doha Declaration and subsequent dispute settlement practice?',
    'Historical analysis of TRIPS negotiating records and textualist legal scholarship versus subsequent practice documentation in WTO disputes.',
    'If constructed, the constraint''s legitimacy depends on evolving interpretive authority rather than textual fixity; if derived, the reading is more robust against formalist challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doha_constructed_vs_derived, conceptual, 'Whether flexibility is textually embedded or interpretively constructed').

omega_variable(
    trips_plus_hollowing,
    'To what extent do bilateral and regional TRIPS-plus agreements functionally nullify the public health flexibilities at the national level?',
    'Empirical mapping of TRIPS-plus clauses in bilateral investment treaties and free trade agreements against actual compulsory licensing frequency.',
    'If hollowing is extensive, the constraint''s effective extraction is lower than its formal scope suggests, and its classification may drift toward piton or theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trips_plus_hollowing, empirical, 'Whether bilateral agreements hollow out TRIPS flexibilities').

omega_variable(
    innovation_access_tradeoff,
    'Does the transfer of market exclusivity to generic producers materially reduce pharmaceutical R&D investment for diseases prevalent in developing countries?',
    'Econometric analysis of R&D expenditure and patent linkage against compulsory licensing frequency and market size in affected countries.',
    'If R&D is materially reduced, the coordination function may be partially undermined by long-term extraction from future patients; if not, the extraction is more clearly bounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_access_tradeoff, empirical, 'Whether compulsory licensing tradeoffs reduce innovation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(trip_tr_t6, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(trip_tr_t12, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(trip_tr_t18, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(trip_tr_t24, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(trip_be_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(trip_be_t6, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(trip_be_t12, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(trip_be_t18, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 18, 0.6).
narrative_ontology:measurement(trip_be_t24, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(trip_su_t6, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(trip_su_t12, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(trip_su_t18, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 18, 0.57).
narrative_ontology:measurement(trip_su_t24, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__public_health_flexibility_reading, global_infrastructure).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% The trips_agreement_interpretive_kernel decomposes into three structurally distinct constraints: the public health flexibility reading (broad flexibilities, this file), the strong exclusivity reading (narrow exceptions), and the dispute settlement interpretive authority reading (panel supremacy). Each carries a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
