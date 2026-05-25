% ============================================================================
% CONSTRAINT STORY: judicial_review_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_judicial_review_authority, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: judicial_review_authority
 *   human_readable: Judicial Review Authority in Constitutional Systems
 *   domain: political/constitutional/institutional
 *
 * SUMMARY:
 *   Judicial review authority — the power of courts to invalidate legislation
 *   and executive action as unconstitutional — is a foundational
 *   institutional mechanism in many constitutional democracies. It presents a
 *   fundamental tension between constitutional fidelity and democratic
 *   accountability. From some perspectives, judicial review is an essential
 *   coordination mechanism that prevents constitutional erosion and protects
 *   minority rights against majoritarian tyranny. From other perspectives, it
 *   is an extractive mechanism by which an unelected institution imposes its
 *   policy preferences on elected branches while hiding behind constitutional
 *   rhetoric. The constraint exhibits all six DR types across perspectives,
 *   making it a diagnostic exemplar of how the same structural institution
 *   can be coordinating, extractive, or both simultaneously depending on the
 *   agent's position. The theater_ratio (0.58) reflects accumulated doctrinal
 *   complexity (rational basis, strict scrutiny, intermediate scrutiny) that
 *   obscures rather than clarifies constitutional application. The
 *   extractiveness (0.52) reflects moderate extraction through
 *   counter-majoritarian capacity coupled with genuine constitutional
 *   coordination function.
 *
 * KEY AGENTS:
 *   - Legislative Majority: Primary victim (powerless/trapped) — legitimately enacted laws can be invalidated by unelected judges; no mechanism for democratic reversal within normal timeframes
 *   - Individual Rights Holders: Secondary victim/beneficiary (moderate/constrained) — constrained by litigation costs and access barriers; benefit from judicial protection against majority tyranny
 *   - Judiciary Institutional Power: Primary beneficiary (institutional/constrained) — consolidates authority and prestige through judicial review; constrained by need to maintain legitimacy through restraint doctrine
 *   - Constitutional Framework: Beneficiary (institutional/arbitrage) — judicial review enforces separation of powers and prevents constitutional erosion
 *   - Democratic Process: Victim (powerless/trapped) — majority will constrained by judicial veto independent of electoral mandate
 *   - Analytical Observer: Neutral (analytical/analytical) — sees both coordination necessity and extraction mechanism; risks naturalizing contingent institutional form as logical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(judicial_review_authority, 0.52).
domain_priors:suppression_score(judicial_review_authority, 0.65).
domain_priors:theater_ratio(judicial_review_authority, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(judicial_review_authority, extractiveness, 0.52).
narrative_ontology:constraint_metric(judicial_review_authority, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(judicial_review_authority, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(judicial_review_authority, tangled_rope).
narrative_ontology:human_readable(judicial_review_authority, "Judicial Review Authority in Constitutional Systems").
narrative_ontology:topic_domain(judicial_review_authority, "political/constitutional/institutional").

domain_priors:requires_active_enforcement(judicial_review_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(judicial_review_authority, judiciary_institutional_power).
narrative_ontology:constraint_beneficiary(judicial_review_authority, constitutional_constraint_defenders).
narrative_ontology:constraint_victim(judicial_review_authority, legislative_majority_will).
narrative_ontology:constraint_victim(judicial_review_authority, executive_discretion).
narrative_ontology:constraint_victim(judicial_review_authority, democratic_responsiveness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEGISLATIVE MAJORITY (SNARE) — Cannot override judicial decisions without constitutional amendment (exit_options: trapped). Bears extraction cost of judicial veto on legitimately enacted laws. Maximum suppression via constitutional text. No mechanism for democratic reversal within normal political time horizons. The legislative will is captured by an unelected institution.
constraint_indexing:constraint_classification(judicial_review_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIVIDUAL RIGHTS HOLDERS (TANGLED ROPE) — Constrained by lack of resources to litigate; also benefit from judicial protection against majority tyranny. Experiences both extraction (litigation costs, uncertainty, delay) and coordination (access to rights defense). Medium suppression — high cost to exit (cannot enforce rights without courts) but genuine coordination benefit exists.
constraint_indexing:constraint_classification(judicial_review_authority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL DESIGN BENEFICIARY (ROPE) — Experiences judicial review as pure coordination mechanism: the judiciary enforces the separation of powers and prevents constitutional erosion. Net beneficiary — judicial restraint on legislative overreach protects the constitutional structure itself. Low extraction from this perspective because the coordination function is genuine and essential.
constraint_indexing:constraint_classification(judicial_review_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIARY INSTITUTIONAL POWER (TANGLED ROPE) — Genuinely coordinates constitutional interpretation but also consolidates institutional power and prestige. Benefits from judicial review authority through precedent expansion and counter-majoritarian capacity. Constrained exit because abdication of review authority would delegitimize the institution. Requires active enforcement of judicial supremacy doctrine.
constraint_indexing:constraint_classification(judicial_review_authority, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL INERTIA (PITON) — Over long time horizons, judicial review has accumulated layers of performative doctrine (rational basis review, strict scrutiny, intermediate scrutiny) that obscure rather than clarify constitutional application. The theater_ratio reflects that courts often invoke formalistic standards that yield predetermined outcomes. The original Marbury v. Madison rationale (preventing constitutional violation) has degraded into routine invalidation of majority preferences, maintained through institutional inertia. Piton classification derives from high theater_ratio combined with moderate extractiveness.
constraint_indexing:constraint_classification(judicial_review_authority, piton,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 6: LOGICAL NECESSITY (MOUNTAIN) — From a universal/civilizational perspective, some form of constitutional constraint mechanism is logically necessary: a written constitution without enforcement is merely advisory. The analytical observer risks naturalizing the specific institutional form (unelected judiciary with final authority) as immutable, when only the abstract principle (constitution must constrain power) is necessary. This perspective claims mountain status but the structural data will likely reveal it as a false summit — the necessity is logical but the implementation is contingent and extractive.
constraint_indexing:constraint_classification(judicial_review_authority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(judicial_review_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(judicial_review_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(judicial_review_authority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(judicial_review_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(judicial_review_authority, TR),
    TR >= 0.70.

:- end_tests(judicial_review_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Judicial review extracts counter-majoritarian power from elected branches; judges can and do invalidate legislation that passed through normal democratic processes. However, the extraction is not maximal (≥0.66 Snare threshold) because a genuine coordination function exists — courts do prevent constitutional violation and protect rights. The trajectory shows increasing extractiveness over time (0.38 → 0.52) as doctrinal proliferation enables more invasive judicial intervention and legislatures learn to anticipate judicial preferences. Suppression (0.65): High. The legislative majority faces severe barriers to override judicial decisions: constitutional amendment (supermajority + ratification), court-packing (politically costly and norm-violating), or strategic legislative work-around (expensive and uncertain). Trapped exit except through generation-scale constitutional change. Theater ratio (0.58): Moderate-high. Judicial review doctrine includes layers of formal standards (rational basis, strict scrutiny, intermediate scrutiny, proportionality) that claim to constrain judicial discretion but often yield predetermined outcomes. Over the historical interval (0 to 100), theater has increased (0.35 → 0.58) as doctrinal complexity accumulated without increasing clarity. Courts invoke formalistic standards that obscure rather than justify outcomes. This trajectory distinguishes judicial review from other constitutional mechanisms with lower theater.
 *
 * PERSPECTIVAL GAP:
 *   The core gap is between the beneficiary's experience (Rope/pure coordination) and the trapped agent's experience (Snare/pure extraction). The same institutional mechanism — judicial power to invalidate legislation — is coordinating from the constitutional framework's perspective (prevents erosion, enforces separation of powers) and extractive from the legislative majority's perspective (unelected veto on democratic will, no override mechanism). The moderate agents (judiciary, rights holders) occupy intermediate positions. The Piton perspective reveals that accumulated doctrinal theater obscures whether the function (constitutional enforcement) justifies the form (counter-majoritarian veto). The false Mountain perspective risks naturalizing what is actually a contingent institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation chains from power level, exit options, and beneficiary/victim status. Legislative majority: powerless + trapped + victim status → d ≈ 0.95 → maximum f(d) → maximum experienced extraction. Constitutional framework: institutional + arbitrage + beneficiary status → d ≈ 0.05 → minimum f(d) → negative effective extraction (receives coordination value). Judiciary: institutional + constrained (must maintain legitimacy) + beneficiary status → d ≈ 0.35 → intermediate f(d) → moderate positive extraction (consolidates power while providing coordination). Individual rights holders: moderate + constrained (litigation costs) + mixed victim/beneficiary status → d ≈ 0.55 → intermediate f(d) → moderate experienced extraction. The directionality derivation captures how different institutional positions experience the same judicial review authority differently, producing the perspectival gap between snare, rope, and tangled_rope classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that judicial review simultaneously coordinates (protects constitution) and extracts (constrains democratic will) from different agent perspectives. The mandatrophy is not 'is this coordination or extraction?' but 'for whom is it coordinating and from whom is it extracting?' The constitutional framework is coordinated; the legislative majority is extracted from. The judiciary benefits from both functions — its institutional power is extracted from the elected branches, while it provides genuine constitutional coordination. The omega variables (written vs unwritten constitutions, countermajoritarian threshold, accountability gap) establish that the balance between coordination and extraction is empirically contingent, not logically necessary. A constitutional system could in principle allocate judicial review differently (parliamentary supremacy, constitutional court with limited scope, supermajority override), suggesting that the current form is Tangled Rope (hybrid) rather than Mountain (necessary) or Rope (pure coordination). The piton trajectory (theater increasing from 0.35 to 0.58) suggests that institutional inertia is layering performance on top of the core constraint, potentially degrading functional enforcement in favor of theatrical legitimacy maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_necessity_vs_institutional_form,
    'Is judicial review authority a logically necessary consequence of constitutional governance, or a contingent institutional choice?',
    'Comparative constitutional analysis: do non-judicial-review systems (parliamentary supremacy, constitutional court with limited scope, supermajority-override mechanisms) achieve constitutional constraint? If yes, judicial review is contingent. If no, it is necessary.',
    'If contingent: judicial review is a Tangled Rope extracting counter-majoritarian power while providing coordination. If necessary: the mountain perspective is justified, and the extraction is cost of constitutional enforceability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_necessity_vs_institutional_form, conceptual, 'Whether judicial review is logically necessary or institutionally contingent').

omega_variable(
    countermajoritarian_threshold,
    'At what rate of judicial invalidation of legislation does the coordination function (protecting constitution) become dominated by extraction (imposing judicial will)?',
    'Historical analysis of invalidation rates across time periods and jurisdictions; correlation between invalidation frequency and public confidence in judiciary; tracking of legislative work-around strategies (strategic ambiguity, litigation anticipation)',
    'If threshold is low (< 1% of major legislation): most judicial review is extractive. If threshold is high (> 5%): coordination dominates. Different democracies show different patterns; identifies jurisdictions where equilibrium is shifting toward extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(countermajoritarian_threshold, empirical, 'Threshold rate at which judicial veto becomes predominantly extractive').

omega_variable(
    accountability_mechanism_gap,
    'Does the constraint that judges cannot be easily removed for decisions protect constitutional independence or enable unaccountable extraction?',
    'Comparative analysis of judicial removal procedures, impeachment success rates, and electoral (re)selection mechanisms; tracking of cases where judicial decisions contradicted public will and consequences to judges. Do judges facing removal ever reverse doctrinal positions?',
    'If removal impossibility is necessary for independence: suppression is justified coordination cost. If it enables routinized extraction against majority preferences: suppression is an extractive mechanism. The same structural fact (can''t remove judges) generates different ethical weight depending on whether independence is genuine or cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_mechanism_gap, empirical, 'Whether judicial independence protects or enables unaccountable extraction').

omega_variable(
    litigation_access_asymmetry,
    'Does the constraint that judicial review requires expensive litigation create systematic bias toward wealthy litigants, or does it appropriately filter claims by seriousness?',
    'Empirical analysis of litigant demographics (corporate vs individual; repeat players vs one-shot); tracking of pro bono litigation patterns; comparison of success rates by litigant wealth; analysis of which issues reach courts vs which are abandoned due to cost.',
    'If wealthy litigants systematically prevail: the constraint extracts from powerless majorities by converting majority preferences into resources-inaccessible legal claims. If cost filtering improves decisional quality: suppression is a justified coordination mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(litigation_access_asymmetry, empirical, 'Whether litigation cost creates systematic bias or appropriate filtering').

omega_variable(
    written_constitution_assumption,
    'Does the analysis change for systems with unwritten/evolutionary constitutions (e.g., UK, Canada) where judicial review has different scope and deference?',
    'Decompose into separate constraint stories for written vs unwritten constitutional contexts. Compare extractiveness and suppression across systems. If dramatically different: written constitution systems have higher extractiveness, suggesting the constraint itself (rather than judicial review specifically) is the driver.',
    'If unwritten constitutions show lower extractiveness: judicial review authority specifically (not constitutional constraint generally) is the extraction mechanism. If similar: the constraint is about constitutional governance broadly, and judicial review is one implementation among several.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(written_constitution_assumption, conceptual, 'Whether written vs unwritten constitution changes constraint structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(judicial_review_authority, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jrev_tr_t0, judicial_review_authority, theater_ratio, 0, 0.35).
narrative_ontology:measurement(jrev_tr_t50, judicial_review_authority, theater_ratio, 50, 0.5).
narrative_ontology:measurement(jrev_tr_t100, judicial_review_authority, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(jrev_be_t0, judicial_review_authority, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(jrev_be_t50, judicial_review_authority, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(jrev_be_t100, judicial_review_authority, base_extractiveness, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(judicial_review_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(judicial_review_authority, separation_of_powers_enforcement).
narrative_ontology:affects_constraint(judicial_review_authority, minority_rights_protection).
narrative_ontology:affects_constraint(judicial_review_authority, legislative_veto_override_mechanisms).

% DUAL FORMULATION NOTE:
% Judicial review authority is the structural mechanism; upstream constraints include the written constitutional text (which authority enforces) and downstream constraints include specific domains where judicial authority operates (voting rights, free speech, property protection, executive power). These are linked: the authority constraint depends on constitutional text for legitimacy, and domain-specific constraints depend on judicial review authority for enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(judicial_review_authority, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
