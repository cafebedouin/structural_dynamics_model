% ============================================================================
% CONSTRAINT STORY: digital_money_origin__regulatory_recognition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__regulatory_recognition_reading, []).

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
 *   constraint_id: digital_money_origin__regulatory_recognition_reading
 *   human_readable: Digital Money as Regulatory-Recognition Event (Origin Reading)
 *   domain: monetary_history/institutional_economics/regulation
 *
 * SUMMARY:
 *   Central banks and financial regulators eventually incorporated digital
 *   balances, e-money, and later tokenized instruments into official monetary
 *   aggregates and supervisory regimes. This story treats that act of formal
 *   recognition as the ORIGIN of digital money — not the technology, not
 *   first use, but the moment the ledger of the state noticed. The claim is
 *   authored as tangled_rope: there is a real coordination function
 *   (comparable statistics, a supervisable perimeter for monetary policy)
 *   fused with an asymmetric extraction function (dating the phenomenon to
 *   institutional uptake retroactively delegitimizes everyone who built or
 *   used digital value before the paperwork existed, and channels legitimacy
 *   and market access to whoever already holds a license).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, 0.68).
domain_priors:suppression_score(digital_money_origin__regulatory_recognition_reading, 0.71).
domain_priors:theater_ratio(digital_money_origin__regulatory_recognition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__regulatory_recognition_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__regulatory_recognition_reading, "Digital Money as Regulatory-Recognition Event (Origin Reading)").
narrative_ontology:topic_domain(digital_money_origin__regulatory_recognition_reading, "monetary_history/institutional_economics/regulation").

domain_priors:requires_active_enforcement(digital_money_origin__regulatory_recognition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__regulatory_recognition_reading, '20911c56-bf6b-4485-aafd-faf3ff916bf1').
narrative_ontology:cs_kernel_codification('20911c56-bf6b-4485-aafd-faf3ff916bf1', distributed).
narrative_ontology:cs_authority_grounding('20911c56-bf6b-4485-aafd-faf3ff916bf1', extraction).
narrative_ontology:cs_interpretation_layer_present('20911c56-bf6b-4485-aafd-faf3ff916bf1').
narrative_ontology:cs_reading_relation('20911c56-bf6b-4485-aafd-faf3ff916bf1', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('20911c56-bf6b-4485-aafd-faf3ff916bf1', digital_money_origin__first_held_reading, coexists_with).
narrative_ontology:cs_axiom('20911c56-bf6b-4485-aafd-faf3ff916bf1', foundational, monetary_status_requires_institutional_recognition).
narrative_ontology:cs_axiom_status(monetary_status_requires_institutional_recognition, holdable).
narrative_ontology:cs_axiom_grounding('20911c56-bf6b-4485-aafd-faf3ff916bf1', monetary_status_requires_institutional_recognition, conventional).
narrative_ontology:cs_axiom('20911c56-bf6b-4485-aafd-faf3ff916bf1', secondary, statistical_aggregation_needs_precede_historical_dating_claims).
narrative_ontology:cs_axiom_status(statistical_aggregation_needs_precede_historical_dating_claims, holdable).
narrative_ontology:cs_axiom_grounding('20911c56-bf6b-4485-aafd-faf3ff916bf1', statistical_aggregation_needs_precede_historical_dating_claims, instrumental).
narrative_ontology:cs_reference_frame('20911c56-bf6b-4485-aafd-faf3ff916bf1', pre_digital_monetary_aggregate_framework).
narrative_ontology:cs_drift_state('20911c56-bf6b-4485-aafd-faf3ff916bf1', post_e_money_and_stablecoin_proliferation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('20911c56-bf6b-4485-aafd-faf3ff916bf1', '').
narrative_ontology:cs_kernel_id(digital_money_origin__regulatory_recognition_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, incumbent_deposit_taking_banks).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, central_bank_statistical_authorities).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, compliant_payment_licensees).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, unregulated_fintech_innovators).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, early_stablecoin_and_e_money_issuers).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, unbanked_users_of_informal_digital_value).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, compliant_payment_licensees).
narrative_ontology:constraint_vindicates(digital_money_origin__regulatory_recognition_reading, monetary_aggregate_completeness_doctrine).
narrative_ontology:constraint_vindicates(digital_money_origin__regulatory_recognition_reading, regulatory_perimeter_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which instruments count as 'money' for M1/M2/M3 aggregates and which issuers fall inside the regulatory perimeter. By formally recognizing digital balances as money only once they are captured in official statistics and supervisory frameworks, this authority effectively dates the origin of digital money to the moment of its own paperwork. It administers the recognition criteria and can revise them, but bears none of the transition costs itself.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, central_bank_statistical_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Already hold banking licenses and reporting infrastructure, so formal regulatory recognition of digital money confirms and legitimizes balances they already issue (deposits, digital transfers). The recognition event costs them almost nothing to comply with and forecloses competition from issuers who cannot meet the same licensing bar, effectively dating 'real' digital money to the moment their own instruments were counted.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, incumbent_deposit_taking_banks, beneficiary,
    institutional, generational, arbitrage, national).

% Payment and e-money firms that invested in obtaining licenses gain legitimacy and market access once regulators recognize their instruments as money-equivalents. They pay ongoing compliance costs but this cost doubles as an entry barrier that shields them from unlicensed competitors — a double-edged position.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, compliant_payment_licensees, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__regulatory_recognition_reading, compliant_payment_licensees, payer).

% Built working digital value-transfer systems before any regulator counted them in statistics. Under this reading, their instruments are treated as not-yet-money until formally recognized, which retroactively erases their functional priority and exposes them to enforcement action, forced registration, or shutdown for operating unlicensed money-like services.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unregulated_fintech_innovators, payer,
    moderate, biographical, constrained, global).

% Issued digital tokens or account balances that circulated and were held as stores of value well before any monetary authority classified them. This reading denies their instruments monetary status until regulatory absorption, meaning their operational history counts for nothing in the origin story and their present operations are subject to compliance costs calibrated to bank-grade risk frameworks they were never built for.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, early_stablecoin_and_e_money_issuers, payer,
    moderate, biographical, trapped, global).

% Used informal digital value systems (mobile airtime credit, community digital ledgers, unlicensed mobile wallets) as functional money long before any regulator recognized them. Under this reading their lived monetary practice is definitionally excluded from 'real' digital money until it passes through institutions they have no access to, leaving their historical usage unrecorded and their present tools legally precarious.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unbanked_users_of_informal_digital_value, payer,
    powerless, biographical, trapped, regional).

% Study when digital money 'really' began and note that regulatory-recognition dating systematically produces the latest possible origin date among competing accounts, because it ties existence to bureaucratic uptake rather than technical capability or lived use.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__regulatory_recognition_reading, incumbent_deposit_taking_banks).
narrative_ontology:fixing_cost_class(digital_money_origin__regulatory_recognition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Formal statistical and regulatory recognition solves a genuine coordination problem: it gives supervisors, other central banks, and market participants a common, auditable definition of what counts as money for macroprudential and monetary-policy purposes, preventing incompatible private definitions from fragmenting monetary aggregates.
% TRANSFER_FUNCTION: The recognition threshold moves legitimacy, market access, and enforcement risk from whoever built and used digital value systems first to whoever obtains the license or reporting status the authority recognizes — effectively transferring the 'first mover' credit and the associated regulatory shelter to incumbents and compliant licensees, at the cost of legal exposure for unrecognized issuers and erasure for informal users.
% ABSENT_VOICES: Unregulated fintech innovators, early token issuers, and unbanked informal-currency users are not represented in the standard-setting bodies that decide recognition criteria; their functional prior use of digital value is treated as pre-monetary rather than as evidence bearing on the true origin date.
% DISAPPEARANCE_RATIONALE: If regulatory recognition criteria vanished, official monetary statistics would lose their common definition and central banks would face measurement chaos in the short term — but the underlying digital value systems that already function as money for their users would continue operating exactly as before, since the recognition event never created the instruments, only counted them. Incumbents dispute this as understating recognition's necessity; unregulated issuers and historians dispute the reverse.
% FOUNDING_PROBLEM: Monetary authorities needed defensible, comparable statistical aggregates and a supervisable perimeter to conduct monetary policy and prudential oversight as private digital payment instruments proliferated outside traditional banking rails.
% FOUNDING_PROBLEM_CORROBORATION: Central bank statisticians and prudential supervisors attest the aggregation problem remains live and cite ongoing revisions to monetary aggregate definitions as evidence. Independent monetary historians and unregulated issuers corroborate that the underlying instruments existed and functioned as money well before recognition, supporting the claim that the 'origin' function of this reading is definitional dating rather than the actual coordination problem it presents itself as solving.
narrative_ontology:disappearance_verdict(digital_money_origin__regulatory_recognition_reading, contested).
narrative_ontology:founding_problem_status(digital_money_origin__regulatory_recognition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__regulatory_recognition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_origin__regulatory_recognition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__regulatory_recognition_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__regulatory_recognition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__regulatory_recognition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.42 to 0.68) as more of the recognition apparatus shifts from genuinely filling a statistical gap toward actively policing a perimeter that protects incumbents from unlicensed competition. Theater ratio also rises (0.20 to 0.42) — a growing share of 'recognition' activity is compliance-signaling and turf-definition among regulators rather than functional improvement in monetary measurement. Suppression is high and rising (0.45 to 0.71) because the constraint's bite depends on active enforcement against unlicensed issuers, not on voluntary adoption. Accessibility collapse (0.58) and resistance (0.60) reflect that alternatives (informal, unlicensed digital value systems) persist and are actively resisted by their users and builders, unlike a genuine mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Central bank statistical authorities set and administer the recognition criteria and bear none of the compliance cost — pure agenda-setter position. Incumbent banks and compliant licensees are structural beneficiaries: recognition validates instruments and infrastructure they already possess and raises the bar against new entrants, so directionality sits near the beneficiary end despite compliant licensees also carrying compliance costs (hence the dual role). Unregulated innovators, early token issuers, and unbanked informal users are targets: their prior functional use is definitionally discounted, and their present operations face enforcement risk they did not choose to accept — directionality sits near the full-target end, amplified by trapped or constrained exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (comparable monetary statistics, a supervisable perimeter) is still live — central banks genuinely need it — which is why this is authored as tangled_rope rather than snare: there is real coordination function underneath the extraction. But the specific act of dating digital money's ORIGIN to the recognition event, rather than treating recognition as a downstream administrative catch-up, is where mandatrophy risk concentrates: the recognition apparatus's mandate (accurate statistics) has drifted into an implicit mandate (adjudicating historical priority and legitimacy), which it was never built to do and which serves incumbents' interests far more than it serves measurement accuracy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recognition_reading_is_one_of_three,
    'Is dating digital money''s origin to formal regulatory recognition a defensible historical claim, or is it simply the institutional record''s own self-referential horizon — the earliest point at which the state''s ledger, not the phenomenon, becomes visible?',
    'Cross-reference against the became_thinkable_reading (conceptual/technical feasibility) and first_held_reading (first practical individual holding) constraints in the same kernel family; compare the gap between technical/practical origin dates and regulatory recognition dates across multiple jurisdictions and instrument types (e-money directives, stablecoin frameworks, CBDC pilots).',
    'If the gap is large and systematic, the regulatory_recognition_reading is best understood as dating institutional AWARENESS rather than the phenomenon''s actual origin, which would reclassify much of its claimed coordination function as retroactive legitimation of incumbents rather than genuine timely measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_reading_is_one_of_three, conceptual, 'Whether regulatory recognition dates the phenomenon or only the institution''s own visibility into it.').

omega_variable(
    coordination_extraction_separability,
    'Is the statistical-aggregate coordination function separable from the perimeter-enforcement function that disadvantages unregulated innovators, or are they structurally fused such that you cannot have reliable monetary statistics without also excluding unlicensed issuers?',
    'Examine jurisdictions that maintain statistical visibility into informal/unlicensed digital value flows (e.g., through survey-based estimation or voluntary reporting) without requiring licensure, and compare aggregate accuracy and enforcement intensity to jurisdictions that require licensure as a precondition for statistical inclusion.',
    'If separable, the enforcement/exclusion component is pure extraction riding on a genuine measurement need; if inseparable, some of the measured suppression is an unavoidable cost of accurate aggregation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether measurement and exclusion are structurally coupled or merely bundled by choice.').

omega_variable(
    beneficiary_capture_of_definition_authority,
    'To what extent do incumbent banks and licensed payment firms influence the recognition criteria themselves (via consultation processes, lobbying, industry advisory panels) versus regulators setting criteria independently from first principles?',
    'Review comment-period submissions, advisory panel composition, and revision histories of monetary aggregate definitions for evidence of incumbent participation in shaping recognition thresholds.',
    'High incumbent influence over definition-setting would support reclassifying this constraint''s beneficiary relationship as partially captured rather than incidental, likely warranting a directionality override raising incumbents'' d slightly above the pure-beneficiary derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_definition_authority, empirical, 'Whether incumbents merely benefit from recognition criteria or actively shape them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__regulatory_recognition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__regulatory_recognition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(digi_tr_t8, digital_money_origin__regulatory_recognition_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(digi_tr_t16, digital_money_origin__regulatory_recognition_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(digi_tr_t24, digital_money_origin__regulatory_recognition_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(digi_tr_t32, digital_money_origin__regulatory_recognition_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(digi_tr_t40, digital_money_origin__regulatory_recognition_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(digi_be_t8, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(digi_be_t16, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(digi_be_t24, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(digi_be_t32, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(digi_be_t40, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(digi_su_t8, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(digi_su_t16, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(digi_su_t24, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(digi_su_t32, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(digi_su_t40, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__regulatory_recognition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__first_held_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the digital_money_origin kernel. became_thinkable_reading dates origin to conceptual/technical feasibility (earliest, near-mountain, minimal extraction). first_held_reading dates origin to first practical individual holding (middle date, rope-like coordination among early users). regulatory_recognition_reading (this story) dates origin to formal statistical/regulatory incorporation (latest date, tangled_rope — genuine aggregation-coordination function fused with incumbent-protecting extraction). The three share no single epsilon value by design; each is authored independently per the ε-invariance principle and linked here for contamination and family analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
