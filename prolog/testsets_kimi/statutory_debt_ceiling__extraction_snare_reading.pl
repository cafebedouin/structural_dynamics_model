% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__extraction_snare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__extraction_snare_reading, []).

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
 *   constraint_id: statutory_debt_ceiling__extraction_snare_reading
 *   human_readable: Statutory Debt Ceiling as Weaponized Hostage Mechanism
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   The statutory debt ceiling imposes a fixed dollar limit on federal
 *   borrowing, requiring congressional authorization to raise. In this
 *   extraction_snare_reading, the ceiling operates not as a genuine fiscal
 *   brake but as a procedural weapon: a legislative minority threatens to
 *   block increase, forcing sovereign default, unless the majority yields
 *   policy concessions. The constraint's persistence depends on active
 *   legislative enforcement of the boundary and the suppression of
 *   alternative resolution mechanisms such as 14th Amendment nullity, clean
 *   repeal, or automatic escalation. Beneficiaries are the holdout minority
 *   factions who capture concessions; victims include the majority coalition,
 *   Treasury operations, federal beneficiaries, and global debt markets
 *   bearing default risk and credit downgrades.
 *
 * KEY AGENTS:
 *   - holdout_minority_faction: Primary beneficiary (organized/national) â extracts concessions via default threat
 *   - majority_legislative_coalition: Primary payer (powerful/constrained) â forced to concede despite nominal majority
 *   - treasury_department: Operational payer (institutional/constrained) â manages cash and default risk
 *   - federal_program_beneficiaries: Diffuse payer (powerless/trapped) â bear payment suspension risk
 *   - public_debt_market: Financial payer (organized/global) â absorbs volatility and downgrade risk
 *   - constitutional_scholars_14th_amendment: Excluded analytical voice arguing nullity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, 0.82).
domain_priors:suppression_score(statutory_debt_ceiling__extraction_snare_reading, 0.79).
domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__extraction_snare_reading, snare).
narrative_ontology:human_readable(statutory_debt_ceiling__extraction_snare_reading, "Statutory Debt Ceiling as Weaponized Hostage Mechanism").
narrative_ontology:topic_domain(statutory_debt_ceiling__extraction_snare_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__extraction_snare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__extraction_snare_reading, '4db9103b-c89e-4f43-9260-1adfe84c610d').
narrative_ontology:cs_kernel_codification('4db9103b-c89e-4f43-9260-1adfe84c610d', fixed_text).
narrative_ontology:cs_authority_grounding('4db9103b-c89e-4f43-9260-1adfe84c610d', lineage).
narrative_ontology:cs_interpretation_layer_present('4db9103b-c89e-4f43-9260-1adfe84c610d').
narrative_ontology:cs_reading_relation('4db9103b-c89e-4f43-9260-1adfe84c610d', statutory_debt_ceiling__coordination_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('4db9103b-c89e-4f43-9260-1adfe84c610d', statutory_debt_ceiling__constitutional_nullity_reading, coexists_with).
narrative_ontology:cs_axiom('4db9103b-c89e-4f43-9260-1adfe84c610d', foundational, minority_leverage_over_aggregate_debt).
narrative_ontology:cs_axiom_status(minority_leverage_over_aggregate_debt, holdable).
narrative_ontology:cs_axiom_grounding('4db9103b-c89e-4f43-9260-1adfe84c610d', minority_leverage_over_aggregate_debt, conventional).
narrative_ontology:cs_axiom('4db9103b-c89e-4f43-9260-1adfe84c610d', foundational, default_risk_is_acceptable_bargaining_cost).
narrative_ontology:cs_axiom_status(default_risk_is_acceptable_bargaining_cost, holdable).
narrative_ontology:cs_axiom_grounding('4db9103b-c89e-4f43-9260-1adfe84c610d', default_risk_is_acceptable_bargaining_cost, instrumental).
narrative_ontology:cs_reference_frame('4db9103b-c89e-4f43-9260-1adfe84c610d', legislative_supremacy_over_borrowing).
narrative_ontology:cs_drift_state('4db9103b-c89e-4f43-9260-1adfe84c610d', post_2011_weaponization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4db9103b-c89e-4f43-9260-1adfe84c610d', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, holdout_minority_faction).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, majority_legislative_coalition).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, treasury_department).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_program_beneficiaries).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, public_debt_market).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Withholds support for debt limit increase to extract policy concessions from the majority coalition. They face no direct personal cost from default risk and can exit the standoff unilaterally by agreeing to a clean increase, but choose to maintain the threat to secure spending cuts or regulatory changes.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, holdout_minority_faction, beneficiary,
    organized, biographical, mobile, national).

% Holds nominal governing majority but must negotiate with the minority to raise the ceiling. Bears political cost of either conceding policy goals or risking sovereign default. Exit is constrained by procedural rules and electoral incentives.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, majority_legislative_coalition, payer,
    powerful, biographical, constrained, national).

% Legally bound to keep borrowing under the statutory cap. Must implement extraordinary measures and cash-management maneuvers during standoffs, and faces operational default if the cap is not raised. Cannot unilaterally ignore the statute without legal jeopardy.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, treasury_department, payer,
    institutional, immediate, constrained, national).

% Rely on federal payments including Social Security, Medicare, and vendor contracts that become collateral in the standoff. They have no seat at the negotiating table and cannot individually exit the federal system.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_program_beneficiaries, payer,
    powerless, immediate, trapped, national).

% Hold and trade US sovereign debt instruments. During ceiling standoffs they face price volatility, liquidity stress, and rating downgrades triggered by manufactured political risk rather than fundamental credit deterioration. Exit is constrained by the dollar's reserve-currency role and the lack of equivalent safe assets at scale.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, public_debt_market, payer,
    organized, biographical, constrained, global).

% Publish legal analysis arguing that the debt ceiling is unconstitutional under Section 4 of the 14th Amendment, which they read as nullifying any statutory limit that would force default on valid public debt. They are seated outside the operational political constraint and their arguments are structurally excluded from executive crisis planning.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, constitutional_scholars_14th_amendment, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__extraction_snare_reading, holdout_minority_faction).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__extraction_snare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a mandatory legislative decision locus where Congress must explicitly address the stock of public debt before Treasury can continue borrowing, originally intended to force periodic fiscal reckoning.
% TRANSFER_FUNCTION: Moves policy concessionsâspending cuts, regulatory changes, and procedural reformsâfrom the majority legislative coalition and executive branch to the holdout minority faction, under threat of sovereign default and its downstream economic harms.
% ABSENT_VOICES: Federal program beneficiaries and public debt market participants bear the default risk but are not seated at the negotiation table; constitutional scholars arguing for 14th Amendment nullity are structurally excluded from the operational crisis framework.
% DISAPPEARANCE_RATIONALE: If the statutory ceiling vanished, Treasury could meet existing obligations without periodic legislative authorization, eliminating the default threat used for extraction; fiscal negotiations would shift to budget and appropriations processes without the synthetic crisis boundary.
% FOUNDING_PROBLEM: To create a procedural checkpoint forcing Congress to periodically confront and vote on the aggregate level of national debt, rather than implicitly authorizing it through separate spending and revenue decisions.
% FOUNDING_PROBLEM_CORROBORATION: The original 1917 and 1939 legislative histories indicate a coordination intent. Contemporary fiscal historians and public finance scholars outside the benefiting minority factions attest that the aggregate ceiling no longer serves that original function and has been repurposed as a bargaining chip; the Treasury Department and Government Accountability Office have documented that the limit does not constrain spending decisions, which are made separately through appropriations.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__extraction_snare_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__extraction_snare_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__extraction_snare_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statutory_debt_ceiling__extraction_snare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__extraction_snare_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the ceiling is systematically used to extract policy concessions unrelated to debt management, decoupled from any marginal fiscal coordination. Suppression is high (0.79) because alternativesânullity, repeal, platinum coin, Gephardt ruleâare actively suppressed or ruled out by the political coalition enforcing the constraint. Theater_ratio is moderate-high (0.45): much of the 'fiscal responsibility' discourse surrounding the vote is performative, masking the extraction function. Accessibility_collapse is 0.65 because while structural alternatives exist conceptually, they are politically inaccessible during a crisis. Resistance is 0.72 because the extracted-against parties resist, leading to recurrent standoffs.
 *
 * PERSPECTIVAL GAP:
 *   The holdout minority experiences the constraint as a source of leverage and coordinationâthey 'bring fiscal sanity to Washington'âwhile the majority coalition, Treasury, and debt markets experience it as a coercive extraction mechanism. The engine will compute seat divergence from these structural positions: the minority has mobile exit (they can end the standoff) and organized power, giving them low directionality; the trapped federal beneficiaries and constrained Treasury have high directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   The holdout minority faction is the structural beneficiary: they collect policy concessions without bearing default costs, giving them directionality near the beneficiary end. The majority legislative coalition is a payer (high directionality) because it yields policy goals under duress. Treasury and federal beneficiaries are full targets because they bear the operational and human costs of the threat without collecting offsetting benefits. Debt market participants are targets due to exogenous risk imposition.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by distinguishing the original 1917 coordination function (dead) from the current extraction function. The constraint is not a scaffold because it lacks a sunset clause and its current justification is the steady-state extraction, not a transition. It is not a tangled rope because the coordination story is cover: the debt ceiling does not actually coordinate debt reduction (spending and revenue are decided elsewhere); it coordinates only the timing of a crisis. Thus it fails the genuine coordination function test and classifies as snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_nullity_vs_enforced_binding,
    'Does the debt ceiling remain an operatively binding legal constraint, or has it become a theatrical boundary that Treasury and the executive could bypass via the 14th Amendment or monetary financing, with extraction sustained only by political convention?',
    'Judicial test case or executive branch legal memo explicitly asserting nullity and continuing issuance, measured against market and legislative reaction.',
    'If operatively null, the extraction is pure theater atop a non-binding rule (piton-like); if legally binding, the threat is structurally real and the snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_nullity_vs_enforced_binding, conceptual, 'Whether the constraint is legally binding or conventionally performed.').

omega_variable(
    extracted_concession_valuation,
    'What is the aggregate monetary and policy value of concessions extracted under debt ceiling threats, relative to the macroeconomic cost of near-default events and credit downgrades?',
    'Comprehensive scoring of policy riders and spending caps attached to debt ceiling increases against estimated GDP losses and borrowing cost increases from rating downgrades.',
    'If extracted value is small relative to economic damage, the constraint is irrational coercion rather than rational extraction; if large, it confirms systematic rent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extracted_concession_valuation, empirical, 'Net extraction value versus systemic cost of the threat mechanism.').

omega_variable(
    minority_veto_institutionalization,
    'Is the minority extraction a contingent feature of current partisan alignment and House rules, or an institutionalized property of the statutory ceiling across varying political configurations?',
    'Historical comparison of debt ceiling votes across unified versus divided government, and across different Speaker rule configurations.',
    'If contingent on specific House rules, the snare is institutionally localized and potentially removable by procedural reform; if persistent across configurations, the extraction is structurally embedded in the statute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_veto_institutionalization, empirical, 'Contingency of extraction on specific institutional configurations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__extraction_snare_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(statutory_debt_ceiling_extraction_tr_t0, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(statutory_debt_ceiling_extraction_tr_t2, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2, 0.3).
narrative_ontology:measurement(statutory_debt_ceiling_extraction_tr_t4, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(statutory_debt_ceiling_extraction_tr_t6, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement(statutory_debt_ceiling_extraction_tr_t8, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(statutory_debt_ceiling_extraction_tr_t10, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(statutory_debt_ceiling_extraction_tr_t12, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 12, 0.45).

% Extraction over time
narrative_ontology:measurement(statutory_debt_ceiling_extraction_be_t0, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(statutory_debt_ceiling_extraction_be_t2, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2, 0.62).
narrative_ontology:measurement(statutory_debt_ceiling_extraction_be_t4, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 4, 0.68).
narrative_ontology:measurement(statutory_debt_ceiling_extraction_be_t6, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(statutory_debt_ceiling_extraction_be_t8, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 8, 0.7).
narrative_ontology:measurement(statutory_debt_ceiling_extraction_be_t10, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(statutory_debt_ceiling_extraction_be_t12, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 12, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(statutory_debt_ceiling_extraction_su_t0, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(statutory_debt_ceiling_extraction_su_t2, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2, 0.58).
narrative_ontology:measurement(statutory_debt_ceiling_extraction_su_t4, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 4, 0.65).
narrative_ontology:measurement(statutory_debt_ceiling_extraction_su_t6, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(statutory_debt_ceiling_extraction_su_t8, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(statutory_debt_ceiling_extraction_su_t10, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(statutory_debt_ceiling_extraction_su_t12, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 12, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__constitutional_nullity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the statutory_debt_ceiling kernel, decomposed per the epsilon-invariance principle because the natural-language label conflates structurally distinct claims: procedural coordination, constitutional nullity, and weaponized extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
