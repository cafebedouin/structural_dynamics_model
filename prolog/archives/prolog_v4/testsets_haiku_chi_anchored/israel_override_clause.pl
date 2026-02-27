% ============================================================================
% CONSTRAINT STORY: israel_override_clause
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israel_override_clause, []).

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
 *   constraint_id: israel_override_clause
 *   human_readable: The 61-Member Judicial Override Clause
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The 61-Member Judicial Override Clause represents a proposed
 *   constitutional mechanism that would allow the Knesset coalition majority
 *   to override Supreme Court rulings that invalidate legislation for
 *   violating Basic Laws. The constraint exhibits structural characteristics
 *   of a pure extraction (Snare) from the perspective of minorities and
 *   rights-protected populations, while appearing to the coalition majority
 *   as a coordination tool (Rope) enabling effective governance. The
 *   mechanism creates asymmetric power: a simple majority coalition can
 *   unilaterally strip judicial review from minorities who cannot exit the
 *   jurisdiction and have no countervailing institutional power. The
 *   extractiveness (0.58) reflects that the mechanism enables the coalition
 *   to capture legislative control entirely, while suppression (0.68)
 *   reflects the complete elimination of an exit route (judicial veto) for
 *   minorities. The moderate theater ratio (0.45) indicates that the
 *   mechanism is relatively transparent — not highly performative — but the
 *   underlying structure creates a coordination function for the majority
 *   while extracting from the minority.
 *
 * KEY AGENTS:
 *   - Coalition Executive and Majority Legislators (61+ MKs): Primary beneficiary (institutional/arbitrage) — gain unilateral power to implement legislative agenda without judicial veto
 *   - Minority Rights and Constitutional Protections: Primary victim (powerless/trapped) — depend entirely on judicial review; have no alternative exit or veto mechanism
 *   - Opposition Political Parties: Secondary actor (moderate/constrained) — excluded from coalition; can mobilize electorally but are subject to outcome of coalition legislation
 *   - Organized Civil Society and Rights Defenders: Victim (organized/trapped) — cannot exit jurisdiction; their primary advocacy mechanism (judicial petition) is directly undermined
 *   - Supreme Court: Institutional actor (institutional/constrained) — primary function (constitutional review) is degraded; persists with reduced structural authority
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees hybrid coordination-extraction structure; identifies mechanism as pure extraction for minorities despite coordination framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israel_override_clause, 0.58).
domain_priors:suppression_score(israel_override_clause, 0.68).
domain_priors:theater_ratio(israel_override_clause, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israel_override_clause, extractiveness, 0.58).
narrative_ontology:constraint_metric(israel_override_clause, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(israel_override_clause, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israel_override_clause, snare).
narrative_ontology:human_readable(israel_override_clause, "The 61-Member Judicial Override Clause").
narrative_ontology:topic_domain(israel_override_clause, "political/constitutional").

domain_priors:requires_active_enforcement(israel_override_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israel_override_clause, executive_coalition_majority).
narrative_ontology:constraint_beneficiary(israel_override_clause, coalition_legislative_interests).
narrative_ontology:constraint_victim(israel_override_clause, minority_rights_protection).
narrative_ontology:constraint_victim(israel_override_clause, constitutional_judicial_review).
narrative_ontology:constraint_victim(israel_override_clause, non_coalition_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY RIGHTS (SNARE) — Cannot exit the jurisdiction; has no veto over override mechanism; depends entirely on judicial review for protection. Suppression is high (0.68): no alternative exit, no political power to block majority coalitions. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.82. Full extraction.
constraint_indexing:constraint_classification(israel_override_clause, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OPPOSITION PARTIES (TANGLED ROPE) — Can mobilize electorally and legislatively but are excluded from coalition majority. The override clause both constrains them (cannot access judicial veto over coalition legislation) and creates coordination opportunity (can build electoral coalitions, utilize public discourse, petition courts before override). d≈0.70, f(d)≈1.08, σ=1.0 → χ≈0.63. Mixed extraction and coordination.
constraint_indexing:constraint_classification(israel_override_clause, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COALITION MAJORITY (ROPE) — Primary beneficiary. The override clause solves a coordination problem for the coalition: achieving legislative agenda without judicial obstruction. From their perspective, this is enabling coordination (passing laws), not pure extraction. They have arbitrage options (can govern under existing rules or push for override mechanism). d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Net coordination benefit.
constraint_indexing:constraint_classification(israel_override_clause, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL SOCIETY ORGANIZATIONS (SNARE) — Organized but cannot exit jurisdiction; their core function (defending rights through judicial petition) is directly undermined by override mechanism. Suppression high: no legislative voice proportional to their constituencies, judicial remedies stripped. d≈0.85, f(d)≈1.25, σ=1.0 → χ≈0.73. High extraction with organization.
constraint_indexing:constraint_classification(israel_override_clause, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: SUPREME COURT (PITON) — Institutional actor whose primary function (constitutional review) is degraded by override mechanism. Theater ratio (0.45) is moderate, but the court's performative aspect increases: rulings become advisory rather than binding. The court persists as an institution (constrained exit from political system) but with reduced structural authority. d≈0.65, f(d)≈0.98, σ=1.0 → χ≈0.57.
constraint_indexing:constraint_classification(israel_override_clause, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a constitutional design lens, the override clause represents a hybrid: it solves the coordination problem of enabling coalition governance (rope function) while simultaneously creating asymmetric extraction from minorities unable to exit or veto (snare function). The mechanism is explicitly designed to extract from the constrained majority in exchange for coordination benefits to the powerful minority (coalition). d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.38.
constraint_indexing:constraint_classification(israel_override_clause, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_override_clause_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israel_override_clause, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_override_clause, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(israel_override_clause, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(israel_override_clause, TR),
    TR >= 0.70.

:- end_tests(israel_override_clause_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): This reflects the degree to which the mechanism enables the majority coalition to extract compliance from minorities. The mechanism does not extract monetary value but political-constitutional submission: minorities must accept legislation even when it violates their constitutional protections. The value is moderate-high because the extraction is not total (minorities retain electoral and civil society options) but is substantial (judicial veto is completely removed). Suppression (0.68): High. The mechanism suppresses the primary alternative exit route for minorities: judicial review. There is no substitute mechanism offered. Suppression is not total (0.95) because minorities retain electoral mobilization options, but those options are long-term and uncertain compared to the immediate judicial remedy being stripped. Theater ratio (0.45): Moderate-low. The mechanism is relatively transparent about its function — not highly performative. The language is direct (override by 61 votes). However, framing as a 'correction' to judicial overreach adds some performative content. The theater increases slightly over time (from 0.38 to 0.45) as the coalition invokes coordination rhetoric to justify the mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a dramatic perspectival gap between the coalition majority and the minority populations. The majority sees coordination (Rope): a tool to unblock legislative gridlock and enable democratic governance. The minorities see pure extraction (Snare): loss of their primary protection mechanism. The opposition parties see tangled constraints: they benefit from judicial review while out of power, but would benefit from override power if they form a coalition (constrained exit reflecting this ambiguity). Civil society organizations see the constraint as a fundamental degradation of their advocacy capacity (Snare with organization). The Supreme Court sees its own institutional function degraded but persisting (Piton). The analytical observer sees the mechanism as a straightforward extraction mechanism disguised as coordination — the 'coordination function' (enabling coalition to pass laws) is not a genuine collective action problem but a preference of the majority to avoid minority constraints. The perspectival gap is maximal between beneficiary and victim.
 *
 * DIRECTIONALITY LOGIC:
 *   Coalition majority: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary; has multiple exit options (can govern under existing rules or push override mechanism). Minorities: Victim + trapped → d≈0.95, f(d)≈1.42. Maximal extraction; cannot exit jurisdiction, have no alternative veto mechanism. Opposition parties: Victim + constrained → d≈0.70, f(d)≈1.08. Can mobilize electorally but are excluded from immediate benefit of override mechanism; have constrained exit (electoral mobilization is slow). Civil society: Victim + trapped → d≈0.85, f(d)≈1.25. Cannot exit jurisdiction; primary advocacy mechanism is directly undermined. Supreme Court: Institutional + constrained → d≈0.65, f(d)≈0.98. Constrained by political system; cannot exit; primary function is degraded but institution persists. Analytical observer: Analytical → d≈0.50, f(d)≈0.65. Sees hybrid structure: coordination for majority, extraction for minority.
 *
 * MANDATROPHY ANALYSIS:
 *   The 61-Member Judicial Override Clause resolves mandatrophy in favor of SNARE as the primary type from the system's analytical perspective. While the coalition majority genuinely experiences a coordination function (solving their legislative gridlock), the mechanism is explicitly structured to extract from minorities. The coordination problem for the majority is not a genuine collective action problem (multiple independent actors needing coordination) but a preference of one coalition to avoid constraints from other parts of the constitutional system. The mechanism's ε=0.58 and suppression=0.68 place it squarely in extraction territory. The moderate theater ratio (0.45) prevents it from being misclassified as a mountain (natural law) or piton (purely performative); the mechanism is functionally designed for extraction with transparent logic. The mandatrophy resolution: the 'coordination' framing is political rhetoric. The structural truth is extraction. A Rope classification would require that the override mechanism solve a genuine coordination problem (multiple minorities and majority all needing to coordinate on legislation) — but the mechanism explicitly solves only the majority's problem at minorities' expense. Therefore: Snare, not Rope, is the correct primary classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    override_threshold_extraction,
    'At what frequency of overrides does the mechanism transition from coordination tool to permanent extraction mechanism?',
    'Empirical observation of override frequency over 10-year period post-enactment; comparison of judicial rulings struck down vs upheld; statistical analysis of override patterns across coalition types',
    'If overrides < 5% of rulings: mechanism appears as coordination tool (Rope dominates). If overrides > 25% of rulings: mechanism is extractive (Snare dominates). Current frequency around 0-1%, but expansion with enactment could shift dramatically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(override_threshold_extraction, empirical, 'Override frequency threshold for extractive classification').

omega_variable(
    minority_coalition_inclusion,
    'Can excluded minorities eventually build alternative coalitions to access override power, or is the 61-member threshold a structural lock?',
    'Electoral dynamics modeling; coalition formation history; analysis of whether excluded parties can achieve majority coalition in foreseeable electoral scenarios',
    'If minorities can realistically form alternative majorities: exit option exists (constrained → mobile), extraction weakens, classification shifts toward Rope. If threshold locks them out: trapped exit confirmed, extraction is permanent Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_coalition_inclusion, empirical, 'Whether minorities have realistic path to override power').

omega_variable(
    basic_law_scope_ambiguity,
    'What counts as a ''violation of Basic Laws''? Is scope broad (any legislative coherence concern) or narrow (only explicit constitutional conflicts)?',
    'Judicial doctrine analysis; Court precedent on Basic Law interpretation; comparison with similar constitutional courts in other democracies',
    'If broad interpretation: override clause applies to many rulings, extraction increases. If narrow interpretation: override applies rarely, extraction remains lower, mechanism functions as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(basic_law_scope_ambiguity, conceptual, 'Scope of Basic Law violations subject to override').

omega_variable(
    international_pressure_constraint,
    'Will international pressure (from allies, economic partners, human rights bodies) constrain use of the override mechanism, or does the mechanism remove internal constraints entirely?',
    'Monitoring of international responses; analysis of economic/diplomatic consequences if clause is invoked; tracking of coalition coalition positions on international pressure',
    'If international pressure is effective constraint: suppression (0.68) is overstated, mechanism is more constrained. If pressure is ignored: suppression stands, extraction is unmitigated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_pressure_constraint, preference, 'Whether international pressure constrains override use').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israel_override_clause, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ioc_theater_t0, israel_override_clause, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ioc_theater_t2, israel_override_clause, theater_ratio, 2, 0.42).
narrative_ontology:measurement(ioc_theater_t5, israel_override_clause, theater_ratio, 5, 0.45).

% Extraction over time
narrative_ontology:measurement(ioc_extract_t0, israel_override_clause, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ioc_extract_t2, israel_override_clause, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(ioc_extract_t5, israel_override_clause, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israel_override_clause, enforcement_mechanism).
narrative_ontology:affects_constraint(israel_override_clause, knesset_super_majority_requirements).
narrative_ontology:affects_constraint(israel_override_clause, basic_law_amendment_procedures).
narrative_ontology:affects_constraint(israel_override_clause, judicial_review_constitutional_scope).

% DUAL FORMULATION NOTE:
% The override clause is downstream of the Basic Law amendment procedures (which define what counts as a violation of Basic Laws) and upstream of specific legislative disputes that would invoke the override mechanism. The clause itself represents a distinct structural constraint on constitutional governance independent of any specific legislation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(israel_override_clause, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
