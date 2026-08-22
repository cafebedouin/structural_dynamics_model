% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__automatic_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__automatic_constraint_reading, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Elimination of the Gold-Reserve Constraint on Money Creation (Automatic-Constraint Reading)
 *   domain: monetary_economics/political_economy
 *
 * SUMMARY:
 *   On this reading, the Nixon Shock (1971) is read as the removal of a
 *   genuinely automatic, non-discretionary physical constraint — the
 *   requirement that money creation stay bounded by gold reserves convertible
 *   on demand — and its replacement by an institutional, discretionary
 *   constraint administered by central banks. The change in kind matters: an
 *   automatic constraint requires no enforcement agent and collects no rents
 *   (it simply binds); a discretionary institutional constraint requires an
 *   agenda-setting authority whose choices about when and how much to expand
 *   the money supply now determine outcomes that the physical constraint used
 *   to determine mechanically. This reading treats that category shift itself
 *   — material to institutional — as the central fact, and reads high
 *   extraction into the resulting regime because the beneficiaries of
 *   discretion (monetary authorities, debtor governments) now administer a
 *   lever that previously administered itself, at cost to those holding fixed
 *   nominal claims.
 *
 * KEY AGENTS:
 *   - monetary_authorities: institutional agenda-setter who inherited the discretion the physical constraint used to exercise automatically
 *   - sovereign_fiscal_agents: institutional beneficiary gaining deficit-financing flexibility
 *   - creditor_class: organized payer who lost an automatic, apolitical debasement guard
 *   - fixed_income_savers: powerless payer, diffuse and largely unaware of the regime change
 *   - debtor_governments: institutional beneficiary via debt erosion
 *   - economic_historians: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.79).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.58).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Elimination of the Gold-Reserve Constraint on Money Creation (Automatic-Constraint Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "monetary_economics/political_economy").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, 'b3617005-0454-4d54-9780-1ae943d191af').
narrative_ontology:cs_kernel_codification('b3617005-0454-4d54-9780-1ae943d191af', distributed).
narrative_ontology:cs_authority_grounding('b3617005-0454-4d54-9780-1ae943d191af', extraction).
narrative_ontology:cs_interpretation_layer_present('b3617005-0454-4d54-9780-1ae943d191af').
narrative_ontology:cs_reading_relation('b3617005-0454-4d54-9780-1ae943d191af', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3617005-0454-4d54-9780-1ae943d191af', gold_fiat_transition_mechanism__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('b3617005-0454-4d54-9780-1ae943d191af', foundational, automatic_material_constraints_categorically_differ_from_discretionary_ones).
narrative_ontology:cs_axiom_status(automatic_material_constraints_categorically_differ_from_discretionary_ones, holdable).
narrative_ontology:cs_axiom_grounding('b3617005-0454-4d54-9780-1ae943d191af', automatic_material_constraints_categorically_differ_from_discretionary_ones, conventional).
narrative_ontology:cs_axiom('b3617005-0454-4d54-9780-1ae943d191af', secondary, discretion_absent_binding_rule_tends_toward_accumulated_extraction).
narrative_ontology:cs_axiom_status(discretion_absent_binding_rule_tends_toward_accumulated_extraction, holdable).
narrative_ontology:cs_axiom_grounding('b3617005-0454-4d54-9780-1ae943d191af', discretion_absent_binding_rule_tends_toward_accumulated_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('b3617005-0454-4d54-9780-1ae943d191af', gold_convertibility_automatic_floor).
narrative_ontology:cs_drift_state('b3617005-0454-4d54-9780-1ae943d191af', post_2008_unconventional_policy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b3617005-0454-4d54-9780-1ae943d191af', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, sovereign_fiscal_agents).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, fixed_income_savers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, debtor_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the money supply without the automatic check of gold convertibility. Can expand the monetary base, set reserve requirements, and conduct open-market operations at discretion, subject only to institutional norms (inflation targets, legislative mandates) it also helps write and can revise. Justifies the discretion as necessary for counter-cyclical stabilization; also uses it to accommodate fiscal deficits and finance war/crisis spending without the automatic gold-drain feedback that would previously have forced contraction.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, beneficiary).

% National treasuries gain the ability to run persistent deficits financed by debt monetization without a hard physical limit forcing default, austerity, or devaluation on a fixed schedule. The removal of the automatic constraint transfers fiscal flexibility to the state at the direct expense of anyone holding a fixed nominal claim.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, sovereign_fiscal_agents, beneficiary,
    institutional, generational, arbitrage, national).

% Holders of long-duration fixed-nominal claims (bonds, pensions, savings) who previously had an automatic, apolitical guarantee that the currency's purchasing power could not be debased below the gold-convertibility floor without triggering a redemption crisis that constrained the issuer. Under the new regime, purchasing power is eroded by discretionary policy choices they cannot veto; their only defenses are diversification into inflation-hedged assets, political lobbying for hard-money rules, or exit into other currencies/assets, all of which are partial and costly.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class, payer,
    organized, biographical, constrained, global).

% Ordinary households holding savings accounts, pensions, and insurance annuities denominated in domestic currency. Lack the sophistication or capital to hedge against discretionary inflation the way institutional creditors can, and are largely unaware that the automatic protection they once implicitly relied upon (via gold-linked currency stability) has been removed. Their exit options are essentially nil — currency substitution and inflation-indexed instruments require capital and financial literacy most do not have.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, fixed_income_savers, payer,
    powerless, biographical, trapped, national).

% Governments carrying large domestic-currency debt loads benefit from the removal of the automatic constraint because inflation and monetary expansion erode the real value of that debt without formal default. They set policy jointly with monetary authorities and share in the gains from discretion.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, debtor_governments, beneficiary,
    institutional, generational, arbitrage, national).

% Study the 1971 transition and its aftermath, comparing pre- and post-transition inflation variance, debt monetization patterns, and creditor-debtor wealth transfers. Do not participate in the constraint's operation but assess its structural consequences from outside.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Central bank discretion coordinates monetary policy responsively to economic conditions — recessions, financial panics, and asymmetric shocks — that a rigid, automatic physical constraint (gold convertibility) could not accommodate without forcing painful, undifferentiated contraction regardless of cause.
% TRANSFER_FUNCTION: Moves purchasing power from holders of fixed nominal claims (creditors, savers, pensioners) to the issuers of currency-denominated liabilities (governments, debtors, and by extension the monetary authorities who administer the expansion), via the erosion of real value that discretionary money creation permits without the automatic redemption check that previously bounded it.
% ABSENT_VOICES: Future generations of savers and fixed-income retirees who inherit a currency regime already stripped of its automatic floor had no vote in 1971; historical creditor constituencies (bondholders, foreign central banks holding dollar reserves under Bretton Woods) were negotiated around or presented with a fait accompli (the Nixon Shock) rather than consulted as a coordinate party.
% DISAPPEARANCE_RATIONALE: If discretionary authority were replaced overnight by a restored automatic physical constraint (a hard convertibility rule), fiscal deficits would face an immediate hard ceiling, counter-cyclical monetary response to shocks would be sharply limited, and the wealth transfer from creditors to debtors that has occurred under discretion would halt and partially reverse in expectation — bond markets, pension funding models, and sovereign debt sustainability calculations would all reorganize around the reintroduced constraint.
% FOUNDING_PROBLEM: The automatic gold-reserve constraint could not flex to accommodate the specific liquidity and reserve-currency demands placed on the dollar by expanding global trade and by domestic countercyclical policy needs; by 1971 foreign dollar claims on US gold exceeded what reserves could honor, forcing either constraint or abandonment.
% FOUNDING_PROBLEM_CORROBORATION: Monetary authorities and their academic allies attest the founding problem (rigid inflexibility incompatible with modern stabilization policy) remains live and the discretionary regime is still solving it. Independent economic historians outside the central-banking establishment, and creditor-side analysts, attest that the flexibility gained has been used substantially for deficit monetization and asset-price inflation unrelated to stabilization, and that the original problem (accommodating genuine external shocks) could have been solved with narrower, rule-bound discretion rather than the open-ended authority actually adopted.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__automatic_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__automatic_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__automatic_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.79, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.79 by 2024) and rising because the reading holds that discretion, once granted, has been used well beyond the narrow stabilization case that would justify pure coordination — the multi-decade rise in measured extraction tracks the accumulation of debt monetization episodes (1970s inflation, 2008 QE, 2020 pandemic expansion) that erode fixed claims without a corresponding automatic check. Suppression is moderate and rising (0.58) — not because dissent is criminalized, but because the institutional architecture (legal tender laws, central bank independence doctrine, the absence of a private convertibility exit) forecloses the option creditors once had: redeeming into a commodity outside the authority's control. Theater ratio is moderate (0.42): inflation-targeting frameworks, independence charters, and rules-based communication are partly genuine coordination technology and partly performance dressing discretionary choices in rule-like language. accessibility_collapse (0.62) reflects that alternatives (commodity money, currency competition) are not physically impossible but are institutionally and legally foreclosed. resistance (0.55) reflects real, organized pushback (hard-money movements, gold-standard advocacy, bondholder political action) that has not succeeded in restoring the automatic constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities and debtor governments derive low d (beneficiary end) — they gained a lever, they administer it, and they can adjust its use to their institutional advantage over generational time horizons with effectively arbitrage-grade exit (they are never on the losing side of their own decisions). Creditor_class and fixed_income_savers derive high d (target end) — the constraint's operation (discretionary expansion) extracts real value from their fixed claims, and their exit options are constrained-to-trapped respectively, which the engine's directionality derivation should push toward the full-target end, especially for the powerless, immobile savers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a genuinely inflexible physical constraint incompatible with counter-cyclical policy and reserve-currency demands — was real in 1971 and remains partly live (genuine stabilization needs still exist). But this reading holds that the scope of discretion actually exercised has substantially outrun that founding problem: the mandate to smooth shocks has been used to accommodate persistent deficit monetization unrelated to stabilization. This is exactly the mandatrophy pattern — a coordination justification (flexibility for shocks) persisting long after (and far beyond) the narrow function that justified it, now covering a much larger extractive footprint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automatic_vs_creditor_discipline_framing,
    'Is the 1971 transition better characterized as the loss of an automatic physical constraint (this reading) or as the loss of creditor veto power exercised through that constraint (the creditor_discipline_reading)?',
    'Compare the counterfactual: would restoring gold convertibility primarily restore an impersonal physical check, or primarily restore creditor-nation leverage over debtor-nation policy? Historical analysis of who actually exercised redemption threats (private creditors vs. foreign central banks vs. abstract ''the market'') would help locate which framing better matches the actual mechanism.',
    'If the creditor-discipline framing is more accurate, the beneficiary/victim structure inverts in part — the ''victim'' (creditor_class) becomes better described as a power-holder losing geopolitical leverage rather than a passive party losing automatic protection, which would lower the reading''s claimed extraction and shift it toward a Rope-with-losers or toward a Tangled Rope with different named parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automatic_vs_creditor_discipline_framing, conceptual, 'Whether the automatic-constraint framing or the creditor-discipline framing better captures the mechanism of loss.').

omega_variable(
    single_cause_vs_overdetermination,
    'Was the Nixon Shock a genuine causal node that removed the automatic constraint, or was it a symbolic marker for a convergence of independent structural changes (per the composite_overdetermination_reading) that would have produced a similar outcome regardless of the specific 1971 policy act?',
    'Counterfactual historical analysis: would telecommunications-enabled capital mobility and Bretton Woods peg collapse have eliminated the effective gold constraint even absent the formal 1971 suspension? Examine whether other countries with different formal transition dates show similar structural trajectories.',
    'If overdetermination is correct, attributing the constraint change to a single discrete institutional swap (this reading''s framing) overstates the discretionary authority''s causal weight and understates the role of technological and structural factors that would have eroded the automatic constraint''s binding force regardless of the formal policy decision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(single_cause_vs_overdetermination, conceptual, 'Whether the transition is a discrete causal event or a symbolic marker for convergent structural change.').

omega_variable(
    genuine_stabilization_vs_accumulated_rent,
    'What fraction of post-1971 monetary expansion represents genuine counter-cyclical stabilization (the founding problem) versus accumulated deficit monetization and asset-price inflation unrelated to stabilization (rent extraction riding on the discretionary authority)?',
    'Decompose historical monetary base growth by episode (recession response vs. deficit accommodation vs. asset purchase programs) and compare growth rates against output-gap-implied stabilization need under standard macroeconomic models.',
    'A high stabilization fraction would support treating this constraint as closer to Rope (genuine coordination cost); a low fraction (most growth unexplained by stabilization need) would support the high extractiveness this reading currently authors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_stabilization_vs_accumulated_rent, empirical, 'Whether discretionary monetary expansion tracks genuine stabilization need or has substantially exceeded it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1971, 0.2).
narrative_ontology:measurement(gold_tr_t1980, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(gold_tr_t1990, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(gold_tr_t2000, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(gold_tr_t2008, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2008, 0.35).
narrative_ontology:measurement(gold_tr_t2020, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(gold_tr_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1971, 0.35).
narrative_ontology:measurement(gold_be_t1980, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(gold_be_t1990, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(gold_be_t2000, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(gold_be_t2008, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2008, 0.62).
narrative_ontology:measurement(gold_be_t2020, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2020, 0.78).
narrative_ontology:measurement(gold_be_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2024, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1971, 0.3).
narrative_ontology:measurement(gold_su_t1980, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1980, 0.38).
narrative_ontology:measurement(gold_su_t1990, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(gold_su_t2000, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(gold_su_t2008, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2008, 0.5).
narrative_ontology:measurement(gold_su_t2020, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2020, 0.56).
narrative_ontology:measurement(gold_su_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.12).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the gold_fiat_transition_mechanism kernel. The automatic_constraint_reading (this file) authors high, rising ε on the theory that a self-executing physical constraint was replaced by discretionary institutional authority subsequently used for rent extraction beyond its founding stabilization function. The creditor_discipline_reading authors the same historical event with a different beneficiary/victim structure (geopolitical power shift from creditor to reserve-issuer, read as emancipation rather than loss). The composite_overdetermination_reading denies the transition is a discrete causal node at all, treating Nixon Shock as symbolic marker for independent convergent structural changes and authoring a correspondingly different, more diffuse extraction profile. Per DP-001 (ε-invariance), these are three distinct constraints sharing one kernel, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
