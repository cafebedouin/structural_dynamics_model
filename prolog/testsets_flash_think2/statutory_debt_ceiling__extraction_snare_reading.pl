% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__extraction_snare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: statutory_debt_ceiling__extraction_snare_reading
 *   human_readable: Statutory Debt Ceiling (Extraction Snare Reading)
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This constraint story analyzes the statutory debt ceiling as an
 *   'extraction snare,' focusing on its contemporary use as a weaponized
 *   boundary by legislative minority factions to extract policy concessions
 *   under the threat of sovereign default. This reading emphasizes the high
 *   extractiveness, active suppression of alternatives, and significant
 *   theatricality involved in its operation, contrasting sharply with its
 *   original administrative purpose.
 *
 * KEY AGENTS:
 *   - legislative_minority_factions: Primary beneficiary and agenda-setter (organized/arbitrage) — extracts policy concessions.
 *   - us_treasury: Primary payer and excluded party (institutional/constrained) — manages default risk, bears operational burden.
 *   - federal_agencies: Payer (institutional/trapped) — faces budget cuts and operational uncertainty.
 *   - global_financial_markets: Payer (powerful/constrained) — experiences volatility and increased borrowing costs.
 *   - us_citizens: Payer (powerless/trapped) — bears economic instability and potential service cuts.
 *   - constitutional_scholars: Analytical observer (analytical/analytical) — critiques constitutional validity and use.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, 0.85).
domain_priors:suppression_score(statutory_debt_ceiling__extraction_snare_reading, 0.9).
domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__extraction_snare_reading, snare).
narrative_ontology:human_readable(statutory_debt_ceiling__extraction_snare_reading, "Statutory Debt Ceiling (Extraction Snare Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__extraction_snare_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__extraction_snare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__extraction_snare_reading, '8e5fbc3f-8682-47db-b90a-28b7d95dc596').
narrative_ontology:cs_kernel_codification('8e5fbc3f-8682-47db-b90a-28b7d95dc596', formalized).
narrative_ontology:cs_authority_grounding('8e5fbc3f-8682-47db-b90a-28b7d95dc596', extraction).
narrative_ontology:cs_interpretation_layer_present('8e5fbc3f-8682-47db-b90a-28b7d95dc596').
narrative_ontology:cs_reading_relation('8e5fbc3f-8682-47db-b90a-28b7d95dc596', statutory_debt_ceiling__coordination_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e5fbc3f-8682-47db-b90a-28b7d95dc596', statutory_debt_ceiling__constitutional_nullity_reading, forecloses).
narrative_ontology:cs_axiom('8e5fbc3f-8682-47db-b90a-28b7d95dc596', foundational, debt_ceiling_is_valid_statutory_limit).
narrative_ontology:cs_axiom_status(debt_ceiling_is_valid_statutory_limit, holdable).
narrative_ontology:cs_axiom_grounding('8e5fbc3f-8682-47db-b90a-28b7d95dc596', debt_ceiling_is_valid_statutory_limit, conventional).
narrative_ontology:cs_axiom('8e5fbc3f-8682-47db-b90a-28b7d95dc596', foundational, default_threat_is_legitimate_bargaining_tool).
narrative_ontology:cs_axiom_status(default_threat_is_legitimate_bargaining_tool, holdable).
narrative_ontology:cs_axiom_grounding('8e5fbc3f-8682-47db-b90a-28b7d95dc596', default_threat_is_legitimate_bargaining_tool, conventional).
narrative_ontology:cs_reference_frame('8e5fbc3f-8682-47db-b90a-28b7d95dc596', statutory_fiscal_control).
narrative_ontology:cs_drift_state('8e5fbc3f-8682-47db-b90a-28b7d95dc596', contemporary_weaponization_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('8e5fbc3f-8682-47db-b90a-28b7d95dc596', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_factions).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, us_treasury).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_agencies).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, global_financial_markets).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, us_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses the debt ceiling as a leverage point to extract policy concessions or spending cuts from the majority party and the executive branch, under the threat of a sovereign default. Benefits from the political capital and policy shifts achieved.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_factions, agenda_setter,
    organized, immediate, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_factions, beneficiary).

% Responsible for managing the nation's finances, it is forced to implement 'extraordinary measures' to avoid default when the debt ceiling is reached, diverting resources and creating operational uncertainty. It is excluded from the political negotiation over the ceiling itself.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, us_treasury, payer,
    institutional, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__extraction_snare_reading, us_treasury, excluded).

% Face potential budget cuts, hiring freezes, and operational disruptions as a result of concessions extracted during debt ceiling impasses. Their ability to deliver services is directly threatened.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_agencies, payer,
    institutional, biographical, trapped, national).

% React to the uncertainty of a potential US default with increased volatility, higher borrowing costs for the US government, and potential downgrades of US credit ratings. They bear the systemic risk.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, global_financial_markets, payer,
    powerful, immediate, constrained, global).

% Bear the ultimate costs of economic instability, potential cuts to social programs, and increased national debt servicing costs if borrowing rates rise. They have no direct means to resolve the impasse.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, us_citizens, payer,
    powerless, biographical, trapped, national).

% Analyze the constitutional implications of the debt ceiling, particularly in relation to the 14th Amendment, and the legality of using it as a bargaining chip. They provide critical commentary but have no direct power over its use.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_factions).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__extraction_snare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In this reading, the debt ceiling's coordination function is minimal or entirely subverted. It fails to coordinate fiscal policy and instead creates systemic instability.
% TRANSFER_FUNCTION: Transfers political power and policy outcomes from the legislative majority and executive branch to legislative minority factions, by leveraging the threat of economic catastrophe. It also transfers economic costs (e.g., higher borrowing rates, market volatility) to global financial markets and US citizens.
% ABSENT_VOICES: Future generations, who will inherit the consequences of fiscal instability and policy choices made under duress, are absent. Unrepresented constituencies whose programs are cut or whose economic stability is threatened also lack direct voice in the negotiations.
% DISAPPEARANCE_RATIONALE: If the statutory debt ceiling vanished overnight, the US government would be able to pay its bills without political hostage-taking. This would fundamentally alter the balance of power in fiscal policy, remove a major source of economic instability, and force legislative factions to negotiate policy on its merits rather than under default threat. The political economy of US fiscal governance would rearrange significantly.
% FOUNDING_PROBLEM: The debt ceiling was originally established to simplify Treasury operations by allowing the executive branch to issue debt up to a statutory limit without seeking individual congressional approval for each issuance.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and economic analysts widely corroborate that the original problem of simplifying Treasury operations is no longer the primary function. Instead, its use as a political weapon has superseded its administrative purpose, as evidenced by repeated impasses and near-defaults. The legislative minority factions, however, often contest this, framing its use as essential fiscal discipline.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__extraction_snare_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__extraction_snare_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__extraction_snare_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(statutory_debt_ceiling__extraction_snare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__extraction_snare_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.85) because the mechanism enables significant policy transfers and economic costs without a proportional coordination benefit. Suppression (0.90) is extreme, as the threat of default is a highly coercive tool that effectively suppresses alternatives to capitulation. Theater ratio (0.60) is high, reflecting the performative nature of political brinkmanship, where much of the 'negotiation' is for public consumption, while the underlying threat is real. Accessibility collapse (0.65) is moderate, as legislative alternatives exist but are severely constrained by the default threat. Resistance (0.80) is high, evidenced by widespread political and public opposition to the weaponization of the debt ceiling.
 *
 * PERSPECTIVAL GAP:
 *   The legislative minority factions perceive the debt ceiling as a legitimate tool for fiscal discipline and policy leverage, viewing its operation as a necessary (if dramatic) part of governance. In contrast, the US Treasury, federal agencies, and global financial markets experience it as a highly extractive and destabilizing snare, forcing costly concessions and creating systemic risk. Constitutional scholars often highlight the divergence from its original intent and question its constitutional legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Legislative minority factions are clear beneficiaries, using the constraint to achieve policy goals and gain political capital (low d). The US Treasury, federal agencies, global financial markets, and US citizens are targets, bearing the direct and indirect costs of the impasse and extracted concessions (high d). Constitutional scholars act as analytical observers, assessing the constraint's structural and constitutional integrity.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Snare prevents mislabeling the debt ceiling as a legitimate coordination mechanism (Rope/Scaffold) or a natural fiscal limit (Mountain). The high extractiveness, active enforcement (via default threat), and identifiable victims clearly indicate a structure designed to extract rather than coordinate. The high theater ratio further supports the Snare classification by highlighting the performative aspect of its weaponization, where the 'fiscal discipline' narrative often serves as cover for political extraction. The 'dead' status of its founding problem (simplifying Treasury operations) further underscores its current mandatrophy and weaponized function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_ceiling_function_ambiguity,
    'Is the statutory debt ceiling primarily a procedural coordination mechanism, a constitutional nullity, or a weaponized tool for political extraction?',
    'Legal precedent from a Supreme Court ruling on the 14th Amendment''s applicability, or a legislative reform that either abolishes the ceiling or redefines its operational parameters to prevent weaponization.',
    'If ruled a constitutional nullity, the constraint ceases to exist. If reformed into a pure coordination mechanism, its extractiveness and suppression would drop significantly, reclassifying it as a Rope or Scaffold. If its weaponized use is formally sanctioned, its Snare classification would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_ceiling_function_ambiguity, conceptual, 'Ambiguity regarding the debt ceiling''s fundamental structural function.').

omega_variable(
    economic_cost_attribution,
    'What proportion of the economic costs (e.g., increased borrowing costs, market volatility) during debt ceiling impasses is directly attributable to the political brinkmanship versus underlying fiscal concerns?',
    'Econometric studies isolating the impact of debt ceiling impasses from other fiscal and economic variables, or comparative analysis with countries lacking a similar statutory debt ceiling.',
    'If costs are primarily due to brinkmanship, the Snare classification is strengthened. If costs are largely due to underlying fiscal concerns, it suggests a more complex interaction, potentially shifting some extractiveness to a ''natural'' consequence of unsustainable spending, though the weaponization remains extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_cost_attribution, empirical, 'Attribution of economic costs to political use vs. fiscal fundamentals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__extraction_snare_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1970, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(stat_tr_t1985, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(stat_tr_t2000, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(stat_tr_t2010, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2010, 0.5).
narrative_ontology:measurement(stat_tr_t2020, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2020, 0.58).
narrative_ontology:measurement(stat_tr_t2024, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(stat_be_t1970, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(stat_be_t1985, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(stat_be_t2000, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(stat_be_t2010, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(stat_be_t2020, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2020, 0.82).
narrative_ontology:measurement(stat_be_t2024, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1970, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(stat_su_t1985, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(stat_su_t2000, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(stat_su_t2010, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(stat_su_t2020, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2020, 0.88).
narrative_ontology:measurement(stat_su_t2024, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__extraction_snare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, us_credit_rating).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, federal_budget_process).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'statutory_debt_ceiling' kernel. This 'extraction_snare_reading' focuses on its use as a political weapon, distinct from the 'coordination_scaffold_reading' (which views it as a procedural mechanism) and the 'constitutional_nullity_reading' (which argues it is void under the 14th Amendment).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
