% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__infrastructure_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: digital_money_emergence_boundary__infrastructure_reading
 *   human_readable: Digital Money Emergence: Infrastructure Reading
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the infrastructure reading of the
 *   contested kernel 'digital_money_emergence_boundary': the claim that
 *   digital money emerged when infrastructure enabled electronic transfer
 *   (1967 ATMs, 1972 ACH, 1977 SWIFT). The reading positions interbank
 *   electronic transfer as the definitive origin of digital money,
 *   privileging infrastructure operators (SWIFT, ACH networks) as the
 *   foundational layer and constraining alternative narratives that would
 *   date emergence from theoretical conceptualization (1960s-1985 Chaum) or
 *   consumer direct holdings (1990s e-purses, 2000 EMD). As a contested
 *   kernel reading, this is one of three sibling constraints; the structural
 *   delta is a middle boundary that collapses M4/M5 monetary aggregates
 *   around electronic bank deposits.
 *
 * KEY AGENTS:
 *   - interbank_infrastructure_operators: Primary beneficiary and narrative promoter (institutional/global) — SWIFT, ACH operators who control payment rails and benefit from being positioned as digital money's originators.
 *   - central_banks: Agenda setter (institutional/global) — enforces the reading through monetary aggregate classification and regulatory taxonomy.
 *   - nonbank_payment_innovators: Primary target (moderate/constrained) — fintech and cryptographic innovators whose legitimacy is constrained by bank-centric historiography.
 *   - retail_users: Secondary target (powerless/trapped) — locked into intermediary-dependent digital money narratives that deny direct consumer agency.
 *   - consumer_advocates: Excluded voice (moderate/constrained) — would argue for consumer-holdings reading but excluded from canonical monetary history.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.65).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.7).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence: Infrastructure Reading").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, '5bd06e77-facc-4a85-9cf0-464894209ddd').
narrative_ontology:cs_kernel_codification('5bd06e77-facc-4a85-9cf0-464894209ddd', distributed).
narrative_ontology:cs_authority_grounding('5bd06e77-facc-4a85-9cf0-464894209ddd', expertise).
narrative_ontology:cs_interpretation_layer_present('5bd06e77-facc-4a85-9cf0-464894209ddd').
narrative_ontology:cs_reading_relation('5bd06e77-facc-4a85-9cf0-464894209ddd', digital_money_emergence_boundary__conceptualization_reading, influences).
narrative_ontology:cs_reading_relation('5bd06e77-facc-4a85-9cf0-464894209ddd', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('5bd06e77-facc-4a85-9cf0-464894209ddd', foundational, electronic_transfer_sufficiency).
narrative_ontology:cs_axiom_status(electronic_transfer_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('5bd06e77-facc-4a85-9cf0-464894209ddd', electronic_transfer_sufficiency, conventional).
narrative_ontology:cs_axiom('5bd06e77-facc-4a85-9cf0-464894209ddd', foundational, intermediary_primacy_over_holder).
narrative_ontology:cs_axiom_status(intermediary_primacy_over_holder, holdable).
narrative_ontology:cs_axiom_grounding('5bd06e77-facc-4a85-9cf0-464894209ddd', intermediary_primacy_over_holder, conventional).
narrative_ontology:cs_reference_frame('5bd06e77-facc-4a85-9cf0-464894209ddd', infrastructure_primacy_framework).
narrative_ontology:cs_drift_state('5bd06e77-facc-4a85-9cf0-464894209ddd', contemporary_fintech_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5bd06e77-facc-4a85-9cf0-464894209ddd', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, interbank_infrastructure_operators).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, nonbank_payment_innovators).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, retail_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% SWIFT, ACH networks, and ATM consortia that operate the core electronic transfer rails. They benefit from being positioned as the originators of digital money in monetary history and regulatory taxonomy, which legitimizes their ongoing control over cross-border and domestic payment infrastructure and defends against disintermediation by non-bank networks.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, interbank_infrastructure_operators, beneficiary,
    institutional, generational, mobile, global).

% Set and enforce monetary aggregate classifications (M4/M5) that incorporate electronic bank deposits. They adopt the infrastructure reading because it provides a stable technological origin point for digital money within bank-centric regulatory frameworks, though they are constrained by the historical narrative once it becomes established orthodoxy.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_banks, agenda_setter,
    institutional, generational, constrained, global).

% Fintech firms, e-money issuers, and cryptocurrency developers whose innovations are positioned as post-dating 'real' digital money. They face regulatory and historiographic headwinds because the infrastructure reading treats bank electronic transfer as the foundational form, making non-bank digital money appear derivative or parasitic.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, nonbank_payment_innovators, payer,
    moderate, biographical, constrained, global).

% Individual holders of money who interact with digital forms primarily through bank intermediaries. The infrastructure reading legitimates a system where they never 'hold' digital money directly until much later in history, reinforcing dependency on banking infrastructure and delaying recognition of direct consumer digital ownership.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, retail_users, payer,
    powerless, biographical, trapped, global).

% Organizations and researchers who argue that digital money should be dated from consumer direct access rather than interbank plumbing. They are excluded from central bank monetary history committees and standard-setting bodies that canonize the infrastructure narrative.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, consumer_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__infrastructure_reading, interbank_infrastructure_operators).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__infrastructure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent historical and taxonomic boundary for when 'digital money' entered the monetary system, enabling central banks to classify electronic bank deposits within existing monetary aggregates (M4/M5) and giving financial historians a stable technological origin point.
% TRANSFER_FUNCTION: Moves historical legitimacy and regulatory framing priority from consumer-facing and conceptual innovations to interbank infrastructure operators (SWIFT, ACH, ATM networks), positioning their systems as the foundational layer of digital monetary history.
% ABSENT_VOICES: Consumer digital wallet providers, cryptocurrency developers, and retail users who experienced electronic money primarily through later consumer interfaces; they would argue that money consumers cannot directly hold is not fully 'digital money' in the experiential sense, but are excluded from central bank historiography and monetary aggregate debates.
% DISAPPEARANCE_RATIONALE: If the infrastructure reading vanished overnight, central bank monetary aggregates would require reclassification of electronic deposits, fintech innovators would gain historical legitimacy as digital money pioneers, and the narrative authority of interbank infrastructure operators would weaken — the taxonomy of monetary history would reorganize around either conceptual or consumer-holdings boundaries.
% FOUNDING_PROBLEM: How to classify and regulate electronic bank deposits that lacked physical form but functioned as money within the interbank system, and how to date the technological transformation of money from physical to electronic form.
% FOUNDING_PROBLEM_CORROBORATION: Monetary historians in academic institutions corroborate the classification challenge from a seat independent of the infrastructure operators; consumer advocates and cryptographic money researchers corroborate that the problem was artificially narrowed to interbank transfers, excluding theoretical and consumer-holdings dimensions. No purely neutral corroborator exists — all attestation is seated.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The infrastructure reading scores high on extractiveness (0.65) because it structurally privileges bank-intermediated electronic transfer over direct consumer digital holdings, consolidating regulatory and historical authority in infrastructure operators. Suppression (0.70) is high because the reading must actively displace the conceptualization reading (which predates it intellectually) and the consumer-holdings reading (which challenges it democratically). Theater_ratio (0.45) reflects substantial performative maintenance: the narrative requires repeated academic and institutional reproduction (textbooks, central bank speeches, industry conferences) to sustain its boundary against visible counterexamples (Bitcoin, retail e-money). Accessibility_collapse (0.60) indicates that alternative readings become difficult to access once the infrastructure reading is installed in monetary economics curricula. Resistance (0.55) reflects ongoing challenges from cryptocurrency advocates and consumer fintech.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (central banks) experiences the constraint as a useful taxonomic convenience that solves M4/M5 classification; the beneficiary seat (infrastructure operators) experiences it as historical legitimation and market protection; the payer seats (nonbank innovators, retail users) experience it as a regulatory-historical gate that positions them as latecomers or derivatives. The engine will compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Interbank infrastructure operators are the structural beneficiaries of this reading (d near beneficiary end): they collect historical prestige, regulatory deference, and continued control over payment rails. Central banks sit near symmetric but slightly toward beneficiary: they gain a clean taxonomic boundary but lose flexibility as the reading hardens. Nonbank payment innovators are structural targets (d near target end): the reading constrains their legitimacy and regulatory standing by predating their innovations with 'already digital' bank money. Retail users are also targets: the reading legitimizes intermediary-dependent money, delaying direct digital ownership.
 *
 * MANDATROPHY ANALYSIS:
 *   The infrastructure reading risks mandatrophy if its founding problem — classifying electronic bank deposits — is treated as permanently solved while the monetary system has moved to consumer-direct and decentralized digital instruments. The reading would then persist as a piton: a degraded historical boundary maintained theatrically despite having lost its taxonomic utility. Current metrics do not yet show full piton levels (theater_ratio 0.45, not above 0.6), but temporal measurements show rising theater from 1977-2017, suggesting drift toward performative maintenance. The presence of live resistance (0.55) and contested founding problem status prevents full mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infrastructure_reading_kernel_position,
    'Is the infrastructure reading a genuine coordination device for monetary taxonomy, or does it primarily serve to legitimize bank-intermediated control over digital value transfer?',
    'Comparative historical analysis measuring whether the infrastructure reading pre-dates or post-dates the concentration of interbank payment infrastructure; if the reading emerged after infrastructure consolidation, it is likely retroactive justification.',
    'If retroactive justification, the constraint is primarily extractive (snare-flavored); if pre-existing taxonomy, it retains stronger coordination function (rope-flavored).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_reading_kernel_position, conceptual, 'Whether the infrastructure reading is coordination or retrospective legitimation').

omega_variable(
    retail_user_cost_ambiguity,
    'Do retail users bear measurable costs from the infrastructure reading''s dominance in regulatory and historical framing?',
    'Regulatory impact assessment comparing fintech licensing burdens and retail access to non-bank digital money in jurisdictions where the infrastructure reading dominates monetary historiography versus jurisdictions with pluralist readings.',
    'If retail access is constrained by bank-centric framing, the victim set is larger than nonbank innovators alone; if not, extraction is concentrated on the innovator class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retail_user_cost_ambiguity, empirical, 'Whether retail users are secondary victims of bank-centric digital money historiography').

omega_variable(
    reading_foreclosure_boundary,
    'Does adopting the infrastructure reading logically foreclose the consumer holdings reading, or can both coexist as phase distinctions within a single monetary history framework?',
    'Survey of academic monetary economics curricula: whether courses teach ''digital money emerged in 1967'' as exclusive origin or as ''wholesale phase'' of a multi-stage emergence.',
    'If taught as exclusive, the readings are in competitive foreclosure and the kernel generates zero-sum disciplinary politics; if phased, the kernel accommodates plural readings with lower extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Whether infrastructure and consumer readings are mutually exclusive or phase-compatible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(digi_tr_t10, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(digi_tr_t20, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(digi_tr_t30, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(digi_tr_t40, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(digi_tr_t50, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(digi_tr_t57, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 57, 0.45).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(digi_be_t10, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(digi_be_t20, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(digi_be_t30, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(digi_be_t40, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(digi_be_t50, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(digi_be_t57, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 57, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(digi_su_t10, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(digi_su_t20, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(digi_su_t30, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(digi_su_t40, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(digi_su_t50, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(digi_su_t57, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 57, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel digital_money_emergence_boundary, decomposed per the epsilon-invariance principle from the colloquial label 'when did digital money emerge?' The infrastructure reading (this file) dates emergence to interbank electronic transfer (1967-1977); sibling readings date it to theoretical conceptualization and consumer direct holdings respectively. Each reading has distinct epsilon, beneficiaries, and structural profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
