% ============================================================================
% CONSTRAINT STORY: bill_of_rights_1689__proto_rights_charter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bill_of_rights_1689__proto_rights_charter_reading, []).

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
 *   constraint_id: bill_of_rights_1689__proto_rights_charter_reading
 *   human_readable: Bill of Rights 1689 as Proto-Rights Charter (Reading)
 *   domain: legal/constitutional/doctrinal
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of a contested kernel: the
 *   Bill of Rights 1689. The kernel is the 1689 English text itself. Three
 *   structurally distinct readings compete: (1) the anti-Catholic-settlement
 *   reading treats it as a confessional instrument securing Protestant
 *   dominance and Catholic exclusion; (2) the parliamentary-privilege reading
 *   emphasizes Article 9 (parliamentary immunity from judicial review) as the
 *   living core; (3) THIS reading, the proto-rights-charter reading, treats
 *   the Bill of Rights as the ancestor of modern constitutional rights
 *   charters, specifically the excessive-bail, cruel-punishment, and
 *   jury-trial guarantees that appear nearly verbatim in the 1791 American
 *   Bill of Rights and subsequent human-rights instruments. Each reading
 *   extracts different provisions as central, identifies different
 *   beneficiaries and victims, and generates different structural
 *   classifications. This constraint story focuses exclusively on the
 *   proto-rights-charter reading: the Bill of Rights as a portable
 *   coordination template for proportional punishment.
 *
 * KEY AGENTS:
 *   - The Accused (1689-1791): Primary beneficiary (powerless/trapped) — the excessive-bail and cruel-punishment clauses provide explicit protection against prerogative extraction.
 *   - Constitutional Framers (1776-1791 America, elsewhere): Secondary beneficiary (institutional/arbitrage) — adopt the charter template and propagate it globally; experience it as a coordination good, not extraction.
 *   - Crown Prerogative (1689 English state): Victim of constraint (powerful/mobile) — loses discretion to inflict arbitrary punishment; experiences the constraint as hybrid coordination-extraction.
 *   - Prerogative Criminal Justice (pre-1689 counterfactual): Shadow victim — the constraint prevents a snare that would otherwise obtain.
 *   - Analytical Observer: Sees the genealogy — the charter is the ancestor of modern rights charters because its provisions (proportionality, jury trial, bail proportionality) recur across jurisdictions and centuries.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bill_of_rights_1689__proto_rights_charter_reading, 0.28).
domain_priors:suppression_score(bill_of_rights_1689__proto_rights_charter_reading, 0.35).
domain_priors:theater_ratio(bill_of_rights_1689__proto_rights_charter_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bill_of_rights_1689__proto_rights_charter_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(bill_of_rights_1689__proto_rights_charter_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(bill_of_rights_1689__proto_rights_charter_reading, theater_ratio, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bill_of_rights_1689__proto_rights_charter_reading, rope).
narrative_ontology:human_readable(bill_of_rights_1689__proto_rights_charter_reading, "Bill of Rights 1689 as Proto-Rights Charter (Reading)").
narrative_ontology:topic_domain(bill_of_rights_1689__proto_rights_charter_reading, "legal/constitutional/doctrinal").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bill_of_rights_1689__proto_rights_charter_reading, '6cdf97f7-3a18-43e3-8adf-24a3fee69de7').
narrative_ontology:cs_kernel_codification('6cdf97f7-3a18-43e3-8adf-24a3fee69de7', fixed_text).
narrative_ontology:cs_authority_grounding('6cdf97f7-3a18-43e3-8adf-24a3fee69de7', lineage).
narrative_ontology:cs_interpretation_layer_present('6cdf97f7-3a18-43e3-8adf-24a3fee69de7').
narrative_ontology:cs_reading_relation('6cdf97f7-3a18-43e3-8adf-24a3fee69de7', bill_of_rights_1689__anti_catholic_settlement_reading, coexists_with).
narrative_ontology:cs_reading_relation('6cdf97f7-3a18-43e3-8adf-24a3fee69de7', bill_of_rights_1689__parliamentary_privilege_reading, coexists_with).
narrative_ontology:cs_axiom('6cdf97f7-3a18-43e3-8adf-24a3fee69de7', foundational, punishment_proportionality_is_coordinable).
narrative_ontology:cs_axiom_status(punishment_proportionality_is_coordinable, holdable).
narrative_ontology:cs_axiom_grounding('6cdf97f7-3a18-43e3-8adf-24a3fee69de7', punishment_proportionality_is_coordinable, deontological).
narrative_ontology:cs_axiom('6cdf97f7-3a18-43e3-8adf-24a3fee69de7', foundational, constitutional_rights_portable_across_jurisdictions).
narrative_ontology:cs_axiom_status(constitutional_rights_portable_across_jurisdictions, holdable).
narrative_ontology:cs_axiom_grounding('6cdf97f7-3a18-43e3-8adf-24a3fee69de7', constitutional_rights_portable_across_jurisdictions, empirically_contingent).
narrative_ontology:cs_reference_frame('6cdf97f7-3a18-43e3-8adf-24a3fee69de7', proportional_punishment_regime).
narrative_ontology:cs_drift_state('6cdf97f7-3a18-43e3-8adf-24a3fee69de7', american_constitutional_adoption, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6cdf97f7-3a18-43e3-8adf-24a3fee69de7', '').
narrative_ontology:cs_kernel_id(bill_of_rights_1689__proto_rights_charter_reading, bill_of_rights_1689).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bill_of_rights_1689__proto_rights_charter_reading, accused_defendants).
narrative_ontology:constraint_beneficiary(bill_of_rights_1689__proto_rights_charter_reading, constitutional_framers_abroad).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: The Accused Under Criminal Procedure — experiences the Bill of Rights as pure coordination mechanism. Excessive bail and cruel punishment clauses directly protect against prerogative extraction. No ambiguity about beneficiary status. Trapped exit (no jurisdictional escape) but charter text provides explicit protection. Classification as Rope reflects that the constraint solves a genuine coordination problem (proportionality in punishment) with minimal theater.
constraint_indexing:constraint_classification(bill_of_rights_1689__proto_rights_charter_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 2: Constitutional Framers Adopting the Charter — see the 1689 Bill of Rights as a portable coordination template. The excessive bail, cruel punishment, and jury trial guarantees are extracted from English precedent and redeployed in American constitutions. Low extractiveness because the charter establishes a public coordination good (proportional punishment norms) that the framers adopt freely and propagate globally. Arbitrage exit: framers can adopt, adapt, or reject the 1689 template.
constraint_indexing:constraint_classification(bill_of_rights_1689__proto_rights_charter_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 3: The Crown's Residual Prerogative — experiences the Bill of Rights as a hybrid: genuine coordination function (establishes proportionality norms, reducing chaos) alongside asymmetric extraction (crown loses discretion to inflict arbitrary punishment). The constraint is active enforcement of a legitimacy boundary. The crown retains some mobility (prosecutorial discretion, sentencing variance within bounds) but loses the untrammeled extraction power that characterized pre-1689 procedure. Tangled Rope reflects both the coordination benefit (rule-of-law stabilization) and the extraction loss.
constraint_indexing:constraint_classification(bill_of_rights_1689__proto_rights_charter_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% Perspective 4: The Prerogative System (Historical Counterfactual) — if the Bill of Rights had not been enacted, the accused would experience criminal procedure as pure snare: arbitrary punishment, bail at crown discretion, no jury trial guarantee, extreme suppression. This perspective models what the constraint prevents — it is the shadow classification showing why the Rope/Tangled Rope readings are achievable.
constraint_indexing:constraint_classification(bill_of_rights_1689__proto_rights_charter_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 5: The Analytical Observer — traces the textual lineage from 1689 Bill of Rights to 1791 American Bill of Rights to 20th-century human rights charters. The constraint is a coordination mechanism: the charter provides a portable template for proportional punishment norms. Extractiveness is low because the coordination function is genuine — the charter solves a real problem (punishment excess) that recurs across jurisdictions and centuries. The charter itself does not extract; it protects. The analytical view is Rope because the constraint's primary function is coordination, not extraction.
constraint_indexing:constraint_classification(bill_of_rights_1689__proto_rights_charter_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bill_of_rights_1689__proto_rights_charter_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bill_of_rights_1689__proto_rights_charter_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bill_of_rights_1689__proto_rights_charter_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(bill_of_rights_1689__proto_rights_charter_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The Bill of Rights, read as proto-rights-charter, establishes a genuine coordination function (proportionality norms in punishment) that benefits the accused and later framers adopting the template. The constraint does not extract from either group; it protects. However, extractiveness is not zero because the crown loses prerogative discretion (a form of extraction loss, measured as the state's lost ability to arbitrarily punish). The modest value (0.28) reflects that the charter is fundamentally coordinative rather than extractive. Suppression (0.35): Moderate. The 1689 framework reduces suppression by guaranteeing jury trial and limiting bail, but suppression is not eliminated — the crown retains prosecutorial discretion, sentencing variance within bounds, and the ability to define crime itself. The charged with a crime still faces material barriers (legal costs, trial risk). Suppression is lower than the pre-1689 regime (where it would be ~0.75) but not zero. Theater ratio (0.50): Moderate-low. The charter's provisions (excessive-bail clause, cruel-punishment clause) are substantive protections, not merely performative — courts interpret them and apply them to constrain state action. However, some theater exists: the definitions of 'excessive,' 'cruel,' and 'unusual' are contested and have shifted over time, meaning the charter's protective force is not invariant.
 *
 * PERSPECTIVAL GAP:
 *   The accused and the constitutional framers experience the charter as Rope (pure coordination). The crown experiences it as Tangled Rope (mixed coordination and asymmetric extraction loss). The analytical observer, reading forward to the charter's global adoption, also sees Rope. The shadow counterfactual (prerogative criminal justice without the charter) would be Snare. The perspectival gap is small for this reading because the charter's function (proportionality) is genuinely coordinative — all agents except the crown (and the prerogative regime in counterfactual) benefit from it or see it as beneficial. The gap widens if other readings (anti-Catholic-settlement, parliamentary-privilege) are activated, which would show the charter as simultaneously confessional-protective and extractive-toward-Catholics, or as primarily about parliamentary immunity rather than accused protection. Within THIS reading alone, the gap is narrow.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from the agent's structural relationship to THIS reading of the kernel. The accused benefits directly from the proportionality clauses — d is low, experiencing negative effective extraction (protection). Constitutional framers adopt the charter freely — d is low (arbitrage exit). The crown loses prerogative extraction — d is moderately high from the crown's perspective (the constraint extracts the crown's discretion). The analytical observer sees the charter as a coordination template — d is balanced (neither fully beneficiary nor fully target). The derivation does not depend on abstract power atoms; it depends on structural position relative to the constraint's specific function (proportionality in punishment).
 *
 * MANDATROPHY ANALYSIS:
 *   The proto-rights-charter reading resolves mandatrophy by focusing the constraint's core function on proportionality in punishment, a genuine coordination problem. The constraint is Rope in the dominant perspectives and Tangled Rope from the crown's perspective. The contrast with the anti-Catholic-settlement reading is instructive: that reading would emphasize the confessional beneficiaries (Protestants) and victims (Catholics), shifting the classification toward Snare from the Catholic perspective. The contest between readings is not a perspectival ambiguity within a single constraint; it is a structural choice about what the kernel IS FOR. The proto-rights-charter reading is vindicated by the empirical circulatory fact: the charter's proportionality provisions do in fact become the template for modern rights charters globally. This functional vindication is not proof (other readings may also capture real aspects of the kernel), but it suggests that the charter's coordinative role (solving the punishment-proportionality problem) is real and enduring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    confessional_embedding_vs_secular_chartering,
    'Is the Bill of Rights'' rights-charter function separable from its embedded confessional constraints (Catholic exclusion, Protestant settlement)?',
    'Comparative textual analysis: identify which provisions are confessionally specific vs. universally applicable. Track adoption trajectories: do framers adopting the charter elsewhere retain or reject the confessional elements?',
    'If separable: the proto-rights-charter reading stands independently. If inseparable: the rights-charter function is contaminated by confessional extraction, and the reading is a partial naturalization of doctrinal settlement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(confessional_embedding_vs_secular_chartering, empirical, 'Whether rights-charter provisions are separable from confessional constraints').

omega_variable(
    reading_contest_kernel_stability,
    'Does treating the Bill of Rights as a contested kernel with three coexisting readings better capture the actual doctrinal dispute than treating it as a single constraint with observer-relative classification?',
    'Structural linguistics of doctrinal debate: do opposing parties assert different core premises (kernel contestation) or the same premise with different normative evaluations? Do legal traditions explicitly maintain multiple readings simultaneously, or do they suppress alternatives?',
    'If kernel contest is real: the three readings instantiate genuinely incompatible framings. If readings are merely perspectival: the Bill of Rights is a single constraint with multiple legitimate classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_kernel_stability, conceptual, 'Whether the Bill of Rights is a contested kernel or a single constraint with perspectival variation').

omega_variable(
    circulation_and_adoption_as_charter_evidence,
    'Does the documented global adoption of excessive-bail and cruel-punishment prohibitions (1791 US Bill of Rights, 1948 Universal Declaration, 1950 ECHR, 1966 ICCPR) constitute empirical evidence that the proto-rights-charter reading correctly identifies the structural function?',
    'Citation network analysis: track the explicit and implicit references to 1689 Bill of Rights in 18th- and 19th-century constitutional texts. Identify whether framers consciously adopted the charter as a template vs. convergently invented similar protections.',
    'If strong circulation evidence: the proto-rights-charter reading is vindicated by structural function (the charter IS the ancestor). If convergent reinvention: the reading may overstate the 1689 text''s causal role.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(circulation_and_adoption_as_charter_evidence, empirical, 'Whether global adoption patterns confirm the proto-rights-charter structural function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bill_of_rights_1689__proto_rights_charter_reading, 1689, 1791).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bor_charter_tr_t0, bill_of_rights_1689__proto_rights_charter_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(bor_charter_tr_t50, bill_of_rights_1689__proto_rights_charter_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(bor_charter_tr_t100, bill_of_rights_1689__proto_rights_charter_reading, theater_ratio, 100, 0.5).

% Extraction over time
narrative_ontology:measurement(bor_charter_be_t0, bill_of_rights_1689__proto_rights_charter_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(bor_charter_be_t50, bill_of_rights_1689__proto_rights_charter_reading, base_extractiveness, 50, 0.25).
narrative_ontology:measurement(bor_charter_be_t100, bill_of_rights_1689__proto_rights_charter_reading, base_extractiveness, 100, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bill_of_rights_1689__proto_rights_charter_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(bill_of_rights_1689__proto_rights_charter_reading, bill_of_rights_1689__anti_catholic_settlement_reading).
narrative_ontology:affects_constraint(bill_of_rights_1689__proto_rights_charter_reading, bill_of_rights_1689__parliamentary_privilege_reading).
narrative_ontology:affects_constraint(bill_of_rights_1689__proto_rights_charter_reading, american_bill_of_rights_1791__excessive_bail_cruel_punishment).
narrative_ontology:affects_constraint(bill_of_rights_1689__proto_rights_charter_reading, universal_declaration_human_rights_1948__punishment_proportionality).

% DUAL FORMULATION NOTE:
% The Bill of Rights 1689 is a contested kernel with three structurally distinct readings. This story models the proto_rights_charter_reading exclusively. The sibling readings (anti_catholic_settlement_reading, parliamentary_privilege_reading) are separate constraint stories with different ε values, beneficiary/victim structures, and classifications. The kernel affects downstream constraints (1791 American Bill, UDHR provisions) through the circulatory adoption of its proto-charter function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
