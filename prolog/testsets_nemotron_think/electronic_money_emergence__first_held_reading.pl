% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__first_held_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: electronic_money_emergence__first_held_reading
 *   human_readable: First Institutional Holding of Dematerialized Currency as Electronic Money Emergence Threshold
 *   domain: economic/monetary/technological
 *
 * SUMMARY:
 *   This constraint story instantiates the first_held_reading of the
 *   electronic_money_emergence kernel. The reading asserts that electronic
 *   money emerged as a discrete ontological category when the first
 *   institutional bearer (a central bank or chartered commercial bank) held
 *   dematerialized currency in a form legally distinguishable from physical
 *   notes — typically dated to the 1970s with the advent of electronic funds
 *   transfer systems and the first computer-mediated interbank settlements.
 *   The threshold is framed as a Mountain: a natural/institutional law of
 *   monetary ontology where legal recognition constitutes the thing itself.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.25).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.4).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, mountain).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "First Institutional Holding of Dematerialized Currency as Electronic Money Emergence Threshold").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic/monetary/technological").

domain_priors:emerges_naturally(electronic_money_emergence__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, '0d12228b-6acf-4d8d-be99-4f3eebfe0883').
narrative_ontology:cs_kernel_codification('0d12228b-6acf-4d8d-be99-4f3eebfe0883', formalized).
narrative_ontology:cs_authority_grounding('0d12228b-6acf-4d8d-be99-4f3eebfe0883', lineage).
narrative_ontology:cs_interpretation_layer_present('0d12228b-6acf-4d8d-be99-4f3eebfe0883').
narrative_ontology:cs_reading_relation('0d12228b-6acf-4d8d-be99-4f3eebfe0883', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d12228b-6acf-4d8d-be99-4f3eebfe0883', electronic_money_emergence__m4_m5_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('0d12228b-6acf-4d8d-be99-4f3eebfe0883', foundational, legal_recognition_constitutes_electronic_money).
narrative_ontology:cs_axiom_status(legal_recognition_constitutes_electronic_money, holdable).
narrative_ontology:cs_axiom_grounding('0d12228b-6acf-4d8d-be99-4f3eebfe0883', legal_recognition_constitutes_electronic_money, conventional).
narrative_ontology:cs_reference_frame('0d12228b-6acf-4d8d-be99-4f3eebfe0883', legal_threshold_ontology).
narrative_ontology:cs_drift_state('0d12228b-6acf-4d8d-be99-4f3eebfe0883', contemporary_digital_asset_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0d12228b-6acf-4d8d-be99-4f3eebfe0883', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, central_banks).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, commercial_banks).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, payment_system_operators).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, alternative_currency_issuers).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, unbanked_populations).
narrative_ontology:constraint_vindicates(electronic_money_emergence__first_held_reading, legal_recognition_constitutes_money).
narrative_ontology:constraint_vindicates(electronic_money_emergence__first_held_reading, institutional_threshold_ontology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and administer the legal threshold that distinguishes electronic money from other digital claims. Their recognition decisions determine which instruments access central bank facilities, settlement systems, and monetary policy transmission channels. They hold the authoritative interpretation of the threshold and face no exit from their own role as monetary sovereign.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, central_banks, agenda_setter,
    institutional, generational, analytical, universal).

% Gain privileged access to the electronic money franchise — the ability to issue liabilities that count as money — by meeting the institutional threshold. Their business models depend on the threshold's stability; exiting means abandoning the banking charter and deposit insurance framework.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, commercial_banks, beneficiary,
    organized, biographical, constrained, global).

% Operate the infrastructure that settles electronic money claims. The threshold defines their regulatory perimeter and competitive moat. They benefit from the clarity the threshold provides but are constrained by the same recognition regime that enables them.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, payment_system_operators, beneficiary,
    organized, biographical, constrained, global).

% Issue digital instruments (stablecoins, local currencies, community credits) that perform money-like functions but fall outside the legal threshold. They bear the cost of non-recognition: no central bank access, regulatory uncertainty, and limited interoperability. Exit means either seeking recognition (submitting to the threshold) or operating in the unrecognized fringe.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, alternative_currency_issuers, payer,
    moderate, biographical, constrained, global).

% Excluded from the electronic money system because the threshold requires institutional intermediation they cannot access. They pay in the form of higher transaction costs, reliance on cash, or predatory alternatives. Exit is not structurally available — the threshold is the barrier itself.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, unbanked_populations, payer,
    powerless, biographical, trapped, global).

% Analyze the historical emergence of electronic money across competing framings. They hold no stake in the threshold's operation but their interpretations shape the scholarly consensus that informs policy. Their exit is analytical — they can adopt any reading without material consequence.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, monetary_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The threshold coordinates the monetary system by establishing a single, legally authoritative boundary between what counts as electronic money and what does not, enabling interoperability, regulatory oversight, and monetary policy transmission across the payment system.
% TRANSFER_FUNCTION: The threshold moves monetary privileges — access to central bank settlement, deposit insurance, legal tender status, and regulatory recognition — from unrecognized digital claims to instruments that meet the institutional holding criterion, gated by the first institutional bearer's legal recognition.
% ABSENT_VOICES: Informal money users in the Global South, historical actors who used telegraphic transfers and book-entry systems before legal recognition, and crypto advocates who argue emergence occurred with the first cryptographic ledger — all would locate emergence earlier or in different structural conditions than the institutional holding event.
% DISAPPEARANCE_RATIONALE: If the legal threshold vanished overnight, the category of electronic money would dissolve into a continuum of digital claims; payment systems would reorganize around private recognition networks rather than public law; monetary policy transmission would fragment; and the privileged position of chartered banks would erode as stablecoins and alternative settlement layers filled the vacuum.
% FOUNDING_PROBLEM: The need for a clear legal boundary between physical currency and its dematerialized equivalents to enable electronic payment systems, prevent double-spending without physical tokens, and preserve monetary policy transmission in an era of computer-mediated finance.
% FOUNDING_PROBLEM_CORROBORATION: Central bank archives (Federal Reserve 1970s EFT policy records, BIS 1985 Red Book inaugural edition) and payments law scholars outside the beneficiary institutions (Benjamin Geva, Ross Buckley, Charles Kahn) attest the boundary problem motivated the threshold. IMF historical records on payment system development corroborate the founding problem independently of current institutional beneficiaries.
narrative_ontology:disappearance_verdict(electronic_money_emergence__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(electronic_money_emergence__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__first_held_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__first_held_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, ExtMetricName, E),
    domain_priors:suppression_score(electronic_money_emergence__first_held_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(electronic_money_emergence__first_held_reading),
    narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(electronic_money_emergence__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the threshold primarily coordinates rather than extracts — it creates the category that enables the payment system. Suppression (0.40) reflects the exclusion of alternative currency forms from monetary privileges. Theater ratio (0.20) is low because the threshold has genuine functional necessity for interoperability. Accessibility collapse (0.70) is high because once the legal boundary is drawn, alternatives cannot easily replicate the privileges of recognized electronic money. Resistance (0.40) is moderate — alternative issuers and crypto advocates contest the threshold but have not displaced it.
 *
 * PERSPECTIVAL GAP:
 *   From the central bank seat, the threshold is a genuine coordination Mountain — the ontology of money requires a sovereign anchor. From alternative issuer and unbanked seats, the same threshold operates as a Snare — a legal barrier that protects incumbents. The engine computes this divergence from the structural data; the authored claim (mountain) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks sit at the beneficiary extreme (d near 0) — they define and profit from the threshold. Commercial banks and payment operators are moderate beneficiaries (d ~ 0.3) — they gain franchise value but bear compliance costs. Alternative issuers and unbanked populations are targets (d > 0.7) — they bear exclusion costs with constrained or trapped exit. Historians are analytical (d = 0.5). The directionality derives from beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enabling electronic payments while preventing double-spending) remains live with CBDCs and stablecoins creating new boundary questions. The threshold has not atrophied into a Piton — it actively structures the emerging digital asset regulatory perimeter. However, the rising theater ratio suggests performative maintenance is increasing as the boundary becomes more contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (first_held_reading) of the contested electronic_money_emergence kernel. What structural elements distinguish it from sibling readings became_thinkable_reading and m4_m5_collapse_reading?',
    'Comparative analysis of each reading''s ε, beneficiary/victim structure, and temporal interval — the ε-invariance principle requires separate stories for each reading.',
    'If readings are not structurally distinct, they collapse into a single constraint with measurement-dependent ε, violating DP-001. The engine treats each reading as a separate constraint linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to the kernel-reading decomposition discipline.').

omega_variable(
    discrete_vs_gradual_threshold,
    'Is the institutional holding threshold genuinely discrete (a sharp legal boundary) or a gradual process of recognition accumulating over decades?',
    'Legal history of electronic money definitions across jurisdictions — tracing whether recognition occurred at a specific regulatory moment or evolved through case law and practice.',
    'If gradual, the Mountain claim (sharp natural threshold) fails; the constraint becomes a Scaffold (transitional recognition) or Tangled Rope (coordination with extraction). The discrete/gradual distinction changes ε and classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discrete_vs_gradual_threshold, empirical, 'Whether the emergence threshold is a step function or a ramp.').

omega_variable(
    constitutive_vs_declaratory_recognition,
    'Does legal recognition *constitute* electronic money (the reading''s claim) or merely *declare* a pre-existing functional reality?',
    'Philosophy of social ontology applied to monetary instruments — Searle-style constitutive rules vs. declaratory recognition. Test: would the instruments function as money without the legal status?',
    'If declaratory, the threshold is not constitutive — the Mountain claim fails. The constraint becomes a Rope (coordination of pre-existing function) or Snare (extraction via legal monopoly). The grounding_type of the foundational axiom shifts from conventional to empirically_contingent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutive_vs_declaratory_recognition, conceptual, 'The ontological status of legal recognition in monetary emergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eme_fhr_tr_t1970, electronic_money_emergence__first_held_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(eme_fhr_tr_t1980, electronic_money_emergence__first_held_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(eme_fhr_tr_t1990, electronic_money_emergence__first_held_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(eme_fhr_tr_t2000, electronic_money_emergence__first_held_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(eme_fhr_tr_t2010, electronic_money_emergence__first_held_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(eme_fhr_tr_t2020, electronic_money_emergence__first_held_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(eme_fhr_be_t1970, electronic_money_emergence__first_held_reading, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(eme_fhr_be_t1980, electronic_money_emergence__first_held_reading, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(eme_fhr_be_t1990, electronic_money_emergence__first_held_reading, base_extractiveness, 1990, 0.2).
narrative_ontology:measurement(eme_fhr_be_t2000, electronic_money_emergence__first_held_reading, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement(eme_fhr_be_t2010, electronic_money_emergence__first_held_reading, base_extractiveness, 2010, 0.24).
narrative_ontology:measurement(eme_fhr_be_t2020, electronic_money_emergence__first_held_reading, base_extractiveness, 2020, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(eme_fhr_su_t1970, electronic_money_emergence__first_held_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(eme_fhr_su_t1980, electronic_money_emergence__first_held_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(eme_fhr_su_t1990, electronic_money_emergence__first_held_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(eme_fhr_su_t2000, electronic_money_emergence__first_held_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(eme_fhr_su_t2010, electronic_money_emergence__first_held_reading, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement(eme_fhr_su_t2020, electronic_money_emergence__first_held_reading, suppression_requirement, 2020, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__first_held_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__first_held_reading, 0.1).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, central_bank_digital_currency_framework).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, payment_system_regulation).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, stablecoin_legal_status).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, monetary_policy_transmission_channel).

% DUAL FORMULATION NOTE:
% This reading (first_held_reading) claims discrete institutional event as emergence. The became_thinkable_reading claims conceptual possibility as emergence. The m4_m5_collapse_reading claims statistical measurement artifact as emergence. All three share the kernel electronic_money_emergence but instantiate different constraints with different ε, beneficiaries, and temporal profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(electronic_money_emergence__first_held_reading, institutional, 0.05).
constraint_indexing:directionality_override(electronic_money_emergence__first_held_reading, organized, 0.25).
constraint_indexing:directionality_override(electronic_money_emergence__first_held_reading, moderate, 0.75).
constraint_indexing:directionality_override(electronic_money_emergence__first_held_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
