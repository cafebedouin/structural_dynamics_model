% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__public_health_flexibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: TRIPS Public Health Flexibility Reading (Compulsory Licensing & Parallel Import)
 *   domain: international_trade_law/public_health/intellectual_property
 *
 * SUMMARY:
 *   The TRIPS Agreement contains provisions for compulsory licensing and
 *   parallel importation of patented medicines. The public health flexibility
 *   reading construes these provisions broadly, asserting that the treaty
 *   text itself mandates access-protective interpretations. This reading was
 *   crystallized and reinforced by the 2001 Doha Declaration. It creates a
 *   legal framework in which health ministries and generic manufacturers gain
 *   leverage to override pharmaceutical patents, while patent holders lose
 *   exclusivity. The constraint is actively enforced through WTO dispute
 *   settlement when challenged. The reading coexists with competing
 *   exclusivist interpretations of the same treaty text.
 *
 * KEY AGENTS:
 *   - Generic manufacturers (beneficiary/organized/constrained): gain legal cover to produce patented medicines under license.
 *   - Health ministries (beneficiary/institutional/constrained): gain statutory leverage to issue compulsory licenses and import generics.
 *   - Pharmaceutical patent holders (payer/powerful/constrained): lose market exclusivity and pricing power where flexibilities are invoked.
 *   - WTO dispute panels (agenda_setter/institutional/analytical): validate the reading through binding interpretation in trade disputes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.62).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.58).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "TRIPS Public Health Flexibility Reading (Compulsory Licensing & Parallel Import)").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "international_trade_law/public_health/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '9e0a3d84-74a2-4df4-9dce-1a9759805a36').
narrative_ontology:cs_kernel_codification('9e0a3d84-74a2-4df4-9dce-1a9759805a36', fixed_text).
narrative_ontology:cs_authority_grounding('9e0a3d84-74a2-4df4-9dce-1a9759805a36', lineage).
narrative_ontology:cs_interpretation_layer_present('9e0a3d84-74a2-4df4-9dce-1a9759805a36').
narrative_ontology:cs_reading_relation('9e0a3d84-74a2-4df4-9dce-1a9759805a36', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_reading_relation('9e0a3d84-74a2-4df4-9dce-1a9759805a36', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, coexists_with).
narrative_ontology:cs_axiom('9e0a3d84-74a2-4df4-9dce-1a9759805a36', foundational, public_health_access_as_trip_mandate).
narrative_ontology:cs_axiom_status(public_health_access_as_trip_mandate, holdable).
narrative_ontology:cs_axiom_grounding('9e0a3d84-74a2-4df4-9dce-1a9759805a36', public_health_access_as_trip_mandate, conventional).
narrative_ontology:cs_axiom('9e0a3d84-74a2-4df4-9dce-1a9759805a36', foundational, compulsory_licensing_permissive_default).
narrative_ontology:cs_axiom_status(compulsory_licensing_permissive_default, holdable).
narrative_ontology:cs_axiom_grounding('9e0a3d84-74a2-4df4-9dce-1a9759805a36', compulsory_licensing_permissive_default, conventional).
narrative_ontology:cs_reference_frame('9e0a3d84-74a2-4df4-9dce-1a9759805a36', public_health_integrated_ip_framework).
narrative_ontology:cs_drift_state('9e0a3d84-74a2-4df4-9dce-1a9759805a36', post_doha_declaration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9e0a3d84-74a2-4df4-9dce-1a9759805a36', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_manufacturers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and export generic medicines under compulsory licenses and parallel import authorizations. Their ability to enter markets for patented essential medicines depends entirely on the legal certainty that this reading provides against patent-holder retaliation in WTO dispute settlement.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_manufacturers, beneficiary,
    organized, biographical, constrained, global).

% Issue compulsory licenses and authorize parallel imports to procure affordable medicines for public health programs. The reading expands their statutory negotiating leverage against patent holders, though they face diplomatic pressure and threats of trade retaliation when using these tools.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries, beneficiary,
    institutional, biographical, constrained, national).

% Hold patents on essential medicines. The flexibility reading erodes their market exclusivity by permitting generic competition under compulsory licensing and parallel importation, directly reducing pricing power, territorial segmentation, and expected returns on R&D.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders, payer,
    powerful, biographical, constrained, global).

% Interpret TRIPS provisions in state-to-state disputes. Under this reading, they validate national use of compulsory licenses and parallel imports against challenges from patent-holding states, providing binding legal cover that makes the flexibility reading enforceable international law.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_dispute_panels, agenda_setter,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global access to essential medicines by permitting governments to override pharmaceutical patents through compulsory licensing and parallel importation, solving the collective-action problem of monopoly pricing in public health emergencies and chronic disease burdens.
% TRANSFER_FUNCTION: Moves market exclusivity and pricing power from pharmaceutical patent holders to generic manufacturers and public health systems, transferring surplus from patent rents to medicine access and domestic health budgets.
% ABSENT_VOICES: Patient advocacy groups from least-developed countries and non-state humanitarian organizations are structurally absent from WTO dispute proceedings; their interests are represented only indirectly by delegations that may prioritize trade relationships over health access.
% DISAPPEARANCE_RATIONALE: If the public health flexibility reading vanished, compulsory licenses would revert to narrow, contested legal territory, generic production for export would face prohibitive legal risk, and millions would lose access to affordable medicines â the global pharmaceutical access architecture would rearrange toward monopoly exclusivity.
% FOUNDING_PROBLEM: Essential medicines were unaffordable and inaccessible in developing countries under strict patent monopoly, creating a persistent global health access gap.
% FOUNDING_PROBLEM_CORROBORATION: WHO, MÃ©decins Sans FrontiÃ¨res, and UNAIDS attest from outside the beneficiary set that the access gap persists and these flexibilities remain necessary; no independent global health authority claims the founding problem is resolved.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__public_health_flexibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__public_health_flexibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.62) reflects the substantial but not total transfer of value from patent holders to generic producers and health systems; remuneration under compulsory licenses tempers full expropriation. Suppression (0.58) captures the legal override of patent rights, which suppresses the patent holder's alternative of full exclusivity. Theater ratio (0.25) is moderate-low: the Doha Declaration generated significant rhetorical performance, but actual compulsory licenses and parallel imports produce real market effects. Resistance (0.72) is high because pharmaceutical industry coalitions and developed-country trade delegations actively resist the reading through TRIPS-plus bilateral agreements and lobbying. Measurements track a post-Doha interval on a single shared grid.
 *
 * PERSPECTIVAL GAP:
 *   From the health ministry and generic manufacturer seats, the constraint is life-saving coordination that corrects a market failure in essential medicines. From the pharmaceutical patent holder seat, the same constraint is expropriation of legal property rights dressed in public health rhetoric. The engine computes this divergence from the structural data; the authored claim of tangled_rope does not resolve the perspectival conflict but names it.
 *
 * DIRECTIONALITY LOGIC:
 *   Generic manufacturers and health ministries are structural beneficiaries (low d): the constraint subsidizes their access to markets and medicines. Pharmaceutical patent holders are structural victims (high d): the constraint extracts exclusivity and rents from them. WTO dispute panels sit near symmetric (d ~0.5) as analytical interpreters without direct financial stake; their directionality is analytical. The engine will amplify extraction for patent holders and damp it for beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling this arrangement as pure extraction (snare) because the constraint genuinely coordinates a collective-action problem â infectious disease treatment and chronic medicine access across borders. It prevents mislabeling as pure coordination (rope) because the transfer is asymmetric: patent holders bear concentrated, involuntary losses. The founding problem (medicine access gap) remains live, which blocks mandatrophy resolution; if the gap closed, the reading would likely atrophy toward piton or dissolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trips_plus_supersession,
    'Do bilateral and regional TRIPS-plus agreements functionally supersede the public health flexibility reading regardless of its WTO textual validity?',
    'Comparative legal analysis of bilateral investment treaties and free trade agreements to quantify incidence of IP provisions stricter than the TRIPS flexibility reading permits.',
    'If supersession is widespread, the constraint''s effective extractiveness is lower than its textual promise suggests, and the reading operates more as theater than enforceable law in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trips_plus_supersession, empirical, 'Whether bilateral agreements override the multilateral flexibility reading.').

omega_variable(
    compulsory_license_political_economy,
    'Does the low empirical use of compulsory licenses reflect adequate voluntary access, or political economy pressure that suppresses the flexibility''s deployment?',
    'Survey of health ministry legal capacity and pharmaceutical lobbying disclosure in middle-income countries; comparison of threatened versus actual trade retaliation.',
    'If pressure suppresses use, the constraint''s suppression metric undercounts internalized suppression, and the coordination function is weaker than claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compulsory_license_political_economy, empirical, 'Whether low compulsory license use reflects suppression or genuine adequacy.').

omega_variable(
    reading_separability,
    'Can the public health flexibility reading be held independently of the dispute settlement interpretive authority reading, or does its enforceability logically depend on a specific adjudication structure?',
    'Examination of national-level compulsory licenses issued without WTO dispute invocation to test whether the substantive reading operates absent the authority reading.',
    'If the readings are inseparable, the sibling relation should be influences rather than coexists_with, and the constraint''s enforcement profile changes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_separability, conceptual, 'Whether the substantive flexibility reading depends on the interpretive authority reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0, 22).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(trip_tr_t5, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(trip_tr_t11, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 11, 0.28).
narrative_ontology:measurement(trip_tr_t16, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(trip_tr_t22, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 22, 0.25).

% Extraction over time
narrative_ontology:measurement(trip_be_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(trip_be_t5, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(trip_be_t11, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 11, 0.56).
narrative_ontology:measurement(trip_be_t16, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(trip_be_t22, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 22, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(trip_su_t5, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(trip_su_t11, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 11, 0.55).
narrative_ontology:measurement(trip_su_t16, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(trip_su_t22, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 22, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resource_allocation).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the trips_agreement_interpretive_kernel. It is structurally coupled to the strong_exclusivity_reading because both derive classification-relevant facts from the same treaty text, yet their epsilon values diverge: this reading has moderate-high extractiveness directed at patent holders, while the exclusivity reading has negligible extraction from patent holders and high extraction from generic competitors and health systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
