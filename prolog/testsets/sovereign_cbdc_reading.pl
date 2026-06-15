% ============================================================================
% CONSTRAINT STORY: sovereign_cbdc_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_cbdc_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sovereign_cbdc_reading
 *   human_readable: Sovereign CBDC Reading: State-Issued Digital Money Legitimacy
 *   domain: monetary_policy/digital_currency/behavioral_economics
 *
 * SUMMARY:
 *   This constraint instantiates the sovereign CBDC reading of the digital
 *   money legitimacy kernel. Under this reading, legitimate digital money
 *   must be state-issued legal tender under central bank control, with full
 *   transaction visibility and programmable policy rules. The reading treats
 *   monetary sovereignty and illicit finance prevention as non-negotiable
 *   requirements that only state-issued currency can satisfy. Alternative
 *   digital money forms are framed as threats to policy effectiveness rather
 *   than as competing coordination mechanisms. The claim/metric independence
 *   is preserved: the constraint is claimed as tangled_rope (genuine
 *   coordination function with asymmetric extraction) while the metrics
 *   describe substantially extractive operation with rising enforcement
 *   requirements over the interval.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_cbdc_reading, 0.68).
domain_priors:suppression_score(sovereign_cbdc_reading, 0.72).
domain_priors:theater_ratio(sovereign_cbdc_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_cbdc_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(sovereign_cbdc_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sovereign_cbdc_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_cbdc_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(sovereign_cbdc_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_cbdc_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_cbdc_reading, "Sovereign CBDC Reading: State-Issued Digital Money Legitimacy").
narrative_ontology:topic_domain(sovereign_cbdc_reading, "monetary_policy/digital_currency/behavioral_economics").

domain_priors:requires_active_enforcement(sovereign_cbdc_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_cbdc_reading, '9ad2d6c4-6dc1-4396-a4c3-9b6b500bf311').
narrative_ontology:cs_kernel_codification('9ad2d6c4-6dc1-4396-a4c3-9b6b500bf311', formalized).
narrative_ontology:cs_authority_grounding('9ad2d6c4-6dc1-4396-a4c3-9b6b500bf311', lineage).
narrative_ontology:cs_interpretation_layer_present('9ad2d6c4-6dc1-4396-a4c3-9b6b500bf311').
narrative_ontology:cs_reading_relation('9ad2d6c4-6dc1-4396-a4c3-9b6b500bf311', digital_money_legitimacy__regulated_stablecoin_reading, influences).
narrative_ontology:cs_reading_relation('9ad2d6c4-6dc1-4396-a4c3-9b6b500bf311', digital_money_legitimacy__crypto_permissionless_reading, coexists_with).
narrative_ontology:cs_axiom('9ad2d6c4-6dc1-4396-a4c3-9b6b500bf311', foundational, state_monopoly_on_legitimate_issuance).
narrative_ontology:cs_axiom_status(state_monopoly_on_legitimate_issuance, holdable).
narrative_ontology:cs_axiom_grounding('9ad2d6c4-6dc1-4396-a4c3-9b6b500bf311', state_monopoly_on_legitimate_issuance, conventional).
narrative_ontology:cs_axiom('9ad2d6c4-6dc1-4396-a4c3-9b6b500bf311', secondary, transaction_visibility_required_for_policy).
narrative_ontology:cs_axiom_status(transaction_visibility_required_for_policy, holdable).
narrative_ontology:cs_axiom_grounding('9ad2d6c4-6dc1-4396-a4c3-9b6b500bf311', transaction_visibility_required_for_policy, instrumental).
narrative_ontology:cs_reference_frame('9ad2d6c4-6dc1-4396-a4c3-9b6b500bf311', bretton_woods_monetary_sovereignty).
narrative_ontology:cs_drift_state('9ad2d6c4-6dc1-4396-a4c3-9b6b500bf311', post_cryptocurrency_emergence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9ad2d6c4-6dc1-4396-a4c3-9b6b500bf311', '').
narrative_ontology:cs_kernel_id(sovereign_cbdc_reading, digital_money_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_cbdc_reading, central_banks).
narrative_ontology:constraint_beneficiary(sovereign_cbdc_reading, fiscal_authorities).
narrative_ontology:constraint_beneficiary(sovereign_cbdc_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(sovereign_cbdc_reading, privacy_seeking_individuals).
narrative_ontology:constraint_victim(sovereign_cbdc_reading, informal_economy_participants).
narrative_ontology:constraint_victim(sovereign_cbdc_reading, cross_border_remittance_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_cbdc_reading, commercial_banks).
narrative_ontology:constraint_victim(sovereign_cbdc_reading, commercial_banks).
narrative_ontology:constraint_vindicates(sovereign_cbdc_reading, monetary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(sovereign_cbdc_reading, anti_money_laundering_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and issue the CBDC, set programmable policy rules embedded in the currency itself, monitor all transactions in real-time. Justify the arrangement as necessary for monetary policy transmission, financial stability, and illicit finance prevention. Gain unprecedented visibility into economic activity and direct policy implementation capability.
narrative_ontology:constraint_stakeholder(sovereign_cbdc_reading, central_banks, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from programmable taxation, automatic compliance enforcement, and elimination of cash-based tax evasion. Can implement targeted fiscal transfers with embedded spending restrictions. Gain real-time economic data for policy calibration without depending on delayed statistical collection.
narrative_ontology:constraint_stakeholder(sovereign_cbdc_reading, fiscal_authorities, beneficiary,
    institutional, generational, mobile, national).

% Gain complete transaction visibility for anti-money laundering and counter-terrorism financing. Can freeze or reverse transactions programmatically. The CBDC architecture eliminates the surveillance gaps that cash and decentralized systems create.
narrative_ontology:constraint_stakeholder(sovereign_cbdc_reading, law_enforcement_agencies, beneficiary,
    institutional, biographical, mobile, national).

% Lose deposit base and payment processing revenue as CBDC disintermediates retail banking. Benefit from reduced compliance costs and fraud risk. Their business model is structurally threatened but they gain operational efficiency in what remains.
narrative_ontology:constraint_stakeholder(sovereign_cbdc_reading, commercial_banks, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_cbdc_reading, commercial_banks, beneficiary).

% Lose transactional privacy as every payment becomes visible to state authorities. Cannot opt out without exiting the formal economy entirely. Their financial behavior becomes legible to the state in ways cash never permitted, with no technical means to recover anonymity within the system.
narrative_ontology:constraint_stakeholder(sovereign_cbdc_reading, privacy_seeking_individuals, payer,
    moderate, biographical, identity_locked, national).

% Depend on cash for economic survival in contexts where formal banking is inaccessible or where income sources cannot withstand state visibility. CBDC eliminates their payment medium and forces them into either formal legibility or barter, neither of which their economic position supports.
narrative_ontology:constraint_stakeholder(sovereign_cbdc_reading, informal_economy_participants, payer,
    powerless, immediate, trapped, local).

% Face capital controls and surveillance embedded in CBDC cross-border protocols. Remittance corridors that operated through informal value transfer systems are closed. They pay higher costs and accept state monitoring as the price of sending money across borders.
narrative_ontology:constraint_stakeholder(sovereign_cbdc_reading, cross_border_remittance_users, payer,
    powerless, biographical, constrained, global).

% Argue for permissionless, censorship-resistant digital money as the legitimate form. Are structurally excluded from the policy conversation that defines legitimacy. Their alternative vision is treated as a threat to monetary sovereignty rather than a competing coordination mechanism.
narrative_ontology:constraint_stakeholder(sovereign_cbdc_reading, cryptocurrency_advocates, excluded,
    organized, generational, mobile, global).

% Study the transmission mechanisms, disintermediation risks, and privacy trade-offs. Produce evidence on whether programmable money enhances or undermines monetary policy effectiveness. Their analysis informs but does not adjudicate the legitimacy question.
narrative_ontology:constraint_stakeholder(sovereign_cbdc_reading, monetary_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified digital payment rail with instant settlement, eliminates counterparty risk in retail payments, enables direct monetary policy transmission without banking intermediation, and creates a single source of truth for economic activity measurement.
% TRANSFER_FUNCTION: Moves transactional privacy and financial autonomy from individuals to state authorities; moves seigniorage and payment processing revenue from commercial banks to central banks; moves economic legibility from statistical inference to real-time surveillance.
% ABSENT_VOICES: Cryptocurrency advocates, privacy technologists, informal economy representatives, and civil liberties organizations are structurally excluded from the design process. They would argue for privacy-preserving architectures, opt-out mechanisms, and limits on programmability, but the legitimacy framing treats their concerns as obstacles to policy effectiveness rather than as rights claims.
% DISAPPEARANCE_RATIONALE: If the CBDC legitimacy claim vanished overnight, alternative digital money forms would compete on their merits rather than being excluded by regulatory fiat. Commercial banks would retain deposit bases, privacy-preserving payment systems would emerge, informal economies would continue operating through cash or decentralized alternatives, and the state would lose real-time economic visibility and programmable policy tools.
% FOUNDING_PROBLEM: Cash is disappearing from advanced economies, creating a coordination problem: without a state-backed digital alternative, private payment systems could fragment monetary sovereignty and create systemic financial stability risks while enabling illicit finance at scale.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and financial regulators attest the problem is live and urgent, citing declining cash usage and rising cryptocurrency adoption. Privacy advocates and monetary economists outside the regulatory apparatus attest the founding problem conflates genuine coordination needs with state preference for surveillance, noting that many advanced economies function with predominantly private digital payments without sovereignty loss.
narrative_ontology:disappearance_verdict(sovereign_cbdc_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_cbdc_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_cbdc_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-15',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(sovereign_cbdc_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_cbdc_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_cbdc_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_cbdc_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.68 at interval end) because the arrangement transfers transactional privacy and financial autonomy from individuals to state authorities at a rate far exceeding the coordination benefit of unified settlement. Suppression is higher still (0.72) because the constraint's persistence depends on actively excluding alternative digital money forms through regulatory prohibition rather than on competitive superiority. Theater ratio is moderate-low (0.28): the financial stability and illicit finance prevention functions are real, but a growing share of the legitimacy narrative defends state surveillance capability rather than addressing genuine coordination failures. Accessibility collapse is moderate (0.48) because alternative payment forms remain technically possible but are legally suppressed. Resistance is substantial (0.58) because privacy advocates, cryptocurrency communities, and informal economy participants actively contest the legitimacy claim. The measurement series shows extraction and suppression intensifying as the CBDC architecture matures and regulatory exclusion of alternatives hardens.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently: from the central bank position the arrangement is necessary monetary infrastructure solving genuine coordination problems; from the privacy-seeking and informal economy seats the same structure operates as enforced surveillance extracting financial autonomy. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks and fiscal authorities are structural beneficiaries (gain policy tools and economic visibility with d near the beneficiary end). Privacy-seeking individuals and informal economy participants are targets (lose financial autonomy, identity-locked or trapped exit, d near the target end). Commercial banks are dual-positioned: they lose deposit base but gain operational efficiency, placing them near symmetric. Law enforcement agencies benefit from surveillance capability without bearing coordination costs. Cross-border remittance users face capital controls embedded in the architecture.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits tangled rope structure: genuine coordination function (unified settlement, instant finality, direct policy transmission) coexists with asymmetric extraction (privacy loss, financial autonomy transfer, informal economy exclusion). The coordination function prevents classification as pure snare, but the extraction is substantial and actively enforced. The founding problem (cash disappearance creating coordination vacuum) is contested: central banks attest urgency while outside observers note that private digital payments coordinate effectively in many jurisdictions without sovereignty loss. The status=contested + disappearance_verdict=world_rearranges pairing indicates the arrangement persists through regulatory enforcement rather than competitive superiority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    privacy_coordination_tradeoff,
    'Is complete transaction visibility structurally necessary for the CBDC''s coordination function, or is the surveillance capability separable from the payment settlement function?',
    'Natural experiment from jurisdictions implementing privacy-preserving CBDC architectures (zero-knowledge proofs, tiered anonymity): if coordination benefits hold while transaction-level visibility is limited, the functions are separable.',
    'If separable, the surveillance capability is extraction riding on genuine coordination rather than inherent to the coordination itself. If inseparable, part of the measured extraction is the unavoidable price of unified settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(privacy_coordination_tradeoff, empirical, 'Whether transaction visibility is structurally necessary for CBDC coordination function.').

omega_variable(
    monetary_sovereignty_boundary,
    'Does monetary sovereignty require state monopoly on digital money issuance, or can sovereignty coexist with competing private digital currencies under regulatory oversight?',
    'Historical analysis of monetary systems with competing private currencies; empirical study of jurisdictions permitting regulated stablecoins alongside CBDCs.',
    'If sovereignty is compatible with competition, the exclusion of alternative digital money forms is pure extraction. If monopoly is structurally necessary, the suppression of alternatives is coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(monetary_sovereignty_boundary, conceptual, 'Whether monetary sovereignty requires state monopoly on digital money.').

omega_variable(
    kernel_reading_ambiguity,
    'Is the sovereign CBDC reading the only coherent interpretation of digital money legitimacy, or do the sibling readings (regulated stablecoin, permissionless crypto) represent equally valid coordination mechanisms?',
    'Cross-reading empirical comparison: measure coordination effectiveness, extraction levels, and suppression requirements across jurisdictions instantiating different readings.',
    'If sibling readings coordinate effectively with lower extraction, the sovereign reading''s legitimacy claim is a cover story for state preference rather than a structural necessity. If the sovereign reading uniquely solves coordination problems the siblings cannot, its extraction is justified coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this reading is uniquely legitimate or one of several valid coordination mechanisms.').

omega_variable(
    programmability_scope_creep,
    'Will programmable money capabilities remain limited to monetary policy transmission, or will they expand to encompass broader behavioral control (spending restrictions, social credit integration, political conditionality)?',
    'Temporal monitoring of CBDC programmability features across jurisdictions; analysis of scope expansion patterns in early-adopter nations.',
    'If programmability scope expands beyond monetary policy, the extraction trajectory is steeper than current measurements suggest and the constraint migrates toward pure snare. If scope remains limited, current extraction levels are stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(programmability_scope_creep, empirical, 'Whether programmable money scope will remain limited or expand to broader control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_cbdc_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_cbdc_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sove_tr_t5, sovereign_cbdc_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(sove_tr_t10, sovereign_cbdc_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(sove_tr_t15, sovereign_cbdc_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(sove_tr_t20, sovereign_cbdc_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(sove_tr_t25, sovereign_cbdc_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_cbdc_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sove_be_t5, sovereign_cbdc_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(sove_be_t10, sovereign_cbdc_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(sove_be_t15, sovereign_cbdc_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(sove_be_t20, sovereign_cbdc_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(sove_be_t25, sovereign_cbdc_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_cbdc_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sove_su_t5, sovereign_cbdc_reading, suppression_requirement, 5, 0.56).
narrative_ontology:measurement(sove_su_t10, sovereign_cbdc_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(sove_su_t15, sovereign_cbdc_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(sove_su_t20, sovereign_cbdc_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(sove_su_t25, sovereign_cbdc_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_cbdc_reading, global_infrastructure).
narrative_ontology:affects_constraint(sovereign_cbdc_reading, regulated_stablecoin_reading).
narrative_ontology:affects_constraint(sovereign_cbdc_reading, crypto_permissionless_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the digital_money_legitimacy kernel. The sovereign_cbdc_reading, regulated_stablecoin_reading, and crypto_permissionless_reading decompose the natural-language concept 'legitimate digital money' into three structurally distinct claims with different beneficiary sets, victim sets, and extraction profiles. They are linked via network.affects_constraints because regulatory treatment of one reading (e.g., CBDC monopoly enforcement) structurally constrains the viability of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
