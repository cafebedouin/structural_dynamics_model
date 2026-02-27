% ============================================================================
% CONSTRAINT STORY: emergency_oversight_bureau
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emergency_oversight_bureau, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: emergency_oversight_bureau
 *   human_readable: Emergency Oversight Bureau Crisis Scaffold
 *   domain: political/emergency_administration
 *
 * SUMMARY:
 *   An emergency oversight bureau is a temporary administrative structure
 *   created during crisis conditions to coordinate response across fragmented
 *   agencies, distribute resources to affected populations, and provide
 *   unified decision-making authority. This constraint is the canonical
 *   exemplar of a Scaffold: it solves a genuine collective action problem
 *   (crisis response coordination) through centralized authority that
 *   explicitly incorporates its own termination. The theater ratio increases
 *   over time (0.35 to 0.62) as the emergency structure develops performative
 *   elements (briefings, reports, ceremonial coordination meetings) while
 *   maintaining genuine coordination function. The extractiveness remains
 *   moderate (0.18 to 0.28) because the centralization provides real benefits
 *   to most stakeholders, though regional leaders experience significant
 *   autonomy loss. The constraint's structure includes explicit sunset
 *   provisions: emergency authority terminates when crisis metrics improve,
 *   recovery milestones are reached, or a specified time horizon expires.
 *   This deliberate temporal boundary distinguishes Scaffolds from Pitons
 *   (which persist indefinitely) and Snares (which have no credible exit).
 *
 * KEY AGENTS:
 *   - Crisis-Affected Populations: Primary beneficiary (powerless/constrained) — receive coordinated resource distribution; cannot exit crisis condition
 *   - Emergency Bureau Administration: Primary actor (institutional/constrained) — exercises temporary authority; coordinates across agencies; benefits from expanded discretion
 *   - Civil Society Organizations: Organized beneficiary (organized/constrained) — tolerate centralized control because they see explicit sunset and alternatives are worse
 *   - Regional Political Leadership: Secondary victim (moderate/mobile) — lose local autonomy; have exit options (political resistance) but at costs
 *   - Legacy Government Agencies: Institutional bystander (institutional/arbitrage) — maintain performative cooperation while protecting jurisdictional authority
 *   - Analytical Observer: Systemwide perspective (analytical/analytical) — evaluates institutional design soundness of temporary emergency authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emergency_oversight_bureau, 0.28).
domain_priors:suppression_score(emergency_oversight_bureau, 0.45).
domain_priors:theater_ratio(emergency_oversight_bureau, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emergency_oversight_bureau, extractiveness, 0.28).
narrative_ontology:constraint_metric(emergency_oversight_bureau, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(emergency_oversight_bureau, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emergency_oversight_bureau, scaffold).
narrative_ontology:human_readable(emergency_oversight_bureau, "Emergency Oversight Bureau Crisis Scaffold").
narrative_ontology:topic_domain(emergency_oversight_bureau, "political/emergency_administration").

domain_priors:requires_active_enforcement(emergency_oversight_bureau).
narrative_ontology:has_sunset_clause(emergency_oversight_bureau).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emergency_oversight_bureau, recovery_constituencies).
narrative_ontology:constraint_beneficiary(emergency_oversight_bureau, institutional_stabilizers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CRISIS-AFFECTED POPULATIONS (ROPE) — Constrained by the emergency condition itself, not primarily by the oversight bureau. The bureau provides coordinated resource distribution and legitimacy for relief allocation. While suppression exists (populations cannot exit the crisis region easily), the constraint functions primarily as coordination rather than extraction. Benefits from centralized crisis response.
constraint_indexing:constraint_classification(emergency_oversight_bureau, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL SOCIETY AND NGO NETWORKS (SCAFFOLD) — Organized actors (humanitarian groups, local organizations, community networks) see the emergency bureau as temporary coordination infrastructure with explicit sunset. The constraint is real (centralized decision-making, reporting requirements, resource allocation protocols) but parties expect and plan for phase-out as recovery milestones are reached. Exit is delayed but explicit — parties tolerate suppression because the alternative (uncoordinated crisis response) is worse, and because the structure has a declared endpoint.
constraint_indexing:constraint_classification(emergency_oversight_bureau, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMERGENCY BUREAU ADMINISTRATION (TANGLED ROPE) — The bureau itself is both a coordination mechanism and an extractive structure. It benefits from emergency authority (expanded budgets, reduced oversight, rapid hiring) while simultaneously coordinating crisis response. The bureau has genuine coordination function (distributing resources, deconflicting agency actions, managing information) but also exhibits asymmetric extraction: bureau staff gain authority, career advancement, and discretionary power that would be impossible in normal administration. Active enforcement required to maintain authority over fragmented crisis response. Agents within the bureau experience this as mixed — coordinating essential function plus structural windfall.
constraint_indexing:constraint_classification(emergency_oversight_bureau, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGACY GOVERNMENT APPARATUS (PITON) — Existing federal agencies experience the emergency bureau as a threat to their normal jurisdictional authority, but they also engage in performative cooperation. Agencies maintain appearance of integration with the bureau while protecting internal budgets and authority domains through bureaucratic theater. The legacy apparatus invokes cooperation language while minimizing actual subordination. Theater ratio high because agencies go through the coordination motions while pursuing institutional preservation. No genuine sunset for legacy apparatus — they remain, waiting for the emergency structure to atrophy.
constraint_indexing:constraint_classification(emergency_oversight_bureau, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REGIONAL POLITICAL LEADERSHIP (SNARE) — Regional governors and local administrators experience the emergency bureau as extractive rather than coordinating. The bureau centralizes decision-making authority that previously belonged to regional actors, constrains their ability to allocate resources according to local priorities, and imposes reporting requirements that reduce their autonomy. They have some exit options (political resistance, institutional non-cooperation) but these carry costs (reduced federal resources, reputational damage, public blame for prolonging crisis). The suppression is significant — alternatives exist but are costly. Extractiveness moderate because regional leaders retain some agency, unlike powerless actors.
constraint_indexing:constraint_classification(emergency_oversight_bureau, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SCAFFOLD) — From a systems design perspective, temporary emergency authority is a structurally sound solution to crisis coordination problems. The constraint exhibits classic scaffold properties: it solves a collective action problem (coordinating fragmented crisis response) with explicit temporal boundaries, suppression declines as crisis conditions improve, and the intended sunset is structural (recovery milestones trigger authority transfer back to normal channels). Theater ratio (0.62) reflects performative elements of emergency declaration while maintaining genuine coordination function.
constraint_indexing:constraint_classification(emergency_oversight_bureau, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergency_oversight_bureau_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emergency_oversight_bureau, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emergency_oversight_bureau, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(emergency_oversight_bureau, TR),
    TR >= 0.70.

:- end_tests(emergency_oversight_bureau_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate. The emergency bureau centralizes decision-making authority and constrains regional actors, but this extraction is justified by genuine coordination benefits. Most stakeholders (populations, civil society) experience net benefit despite suppression. Regional leaders experience extraction, but they retain some agency and have visible exit options. Theater ratio (0.62): Moderate-high and rising. The bureau develops performative elements (status briefings, ceremonial agency coordination, formal reports) as it matures, but these overlay genuine coordination function rather than replacing it. The rise from 0.35 to 0.62 reflects Goodhart drift — as the bureau becomes established, performance metrics (reports filed, meetings held, briefings given) begin to substitute for actual coordination. Suppression (0.45): Moderate. Real barriers exist to normal governance and local autonomy, but these are largely temporary consequences of the crisis condition itself rather than purely institutional constraints. The emergency context justifies constraint. The claimed_type is Scaffold because: (1) requires_active_enforcement is true (central authority must coordinate fragmented agencies), (2) has_sunset_clause is true (authority explicitly terminates), (3) suppression is justified by crisis conditions and expected to decline as recovery metrics improve.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the bureau's own perspective (Tangled Rope) and regional leadership's perspective (Snare) reveals the extraction mechanism. From the bureau's view, centralization solves coordination and provides asymmetric authority benefits — genuine mixed constraint. From regional leadership's view, the same structure is pure extraction of their former authority. The Analytical Observer's Scaffold view emphasizes the structural feature (credible sunset) that determines classification, while the Legacy Apparatus's Piton view reveals the real risk: that performative cooperation masks institutional non-integration, and the bureau's authority may persist through justification cycles beyond its original mandate. The Crisis-Affected Populations' Rope classification is crucial: for the primary beneficiaries, the constraint is coordination, not extraction. This prevents misclassifying the emergency bureau as a Snare purely because regional leaders lose autonomy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit structure. Crisis populations benefit directly (low d → low χ) with no exit options (trapped status). Civil society organizations tolerate the constraint because they see explicit sunset (constrained exit, beneficiary status → moderate d). The Emergency Bureau Administration is neither pure beneficiary nor victim but rather both coordinator and extractor — the Tangled Rope classification captures this asymmetry. Regional leaders have better exit options (political resistance, institutional non-cooperation) and are victims of centralization (high d for moderate power), producing visible perspectival extraction. The analytical observer's canonical d (0.73) applies to the systemwide view. No directionality overrides needed; structural derivation captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that Scaffold is genuinely distinct from both Piton (no credible sunset, theater dominates) and Snare (no coordination function, pure extraction). The key discriminant is the explicit termination clause combined with genuine coordination benefit. The rising theater ratio (0.35→0.62) is a key lifecycle signal: if theater rises toward 0.70+ while extractiveness remains stable, the constraint is degrading from Scaffold toward Piton. The analysis should track whether the sunset deadline is extended without corresponding crisis metrics justifying extension — extension cycles indicate the structure's real function may be atrophying. The bureau's self-perception (Tangled Rope) is crucial: as long as the administration acknowledges both coordination function AND asymmetric authority benefits, the Scaffold status is stable. When leadership begins to naturalize authority as permanent governance necessity, the classification shifts toward Piton. The critical measurement is whether suppression declines as crisis metrics improve — this is the signature of genuine Scaffold behavior.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    crisis_severity_threshold,
    'What crisis severity threshold justifies temporary suspension of normal governance constraints, and how is it measured and verified?',
    'Comparative analysis of past emergency declarations; empirical correlation between declared crisis metrics and actual magnitude; audit of threshold application across multiple agencies',
    'If threshold is low/vague: emergency authority becomes easily invoked for non-crisis problems, converting Scaffold into Snare. If threshold is high/rigid: genuine crises lack adequate emergency coordination tools.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_severity_threshold, empirical, 'Threshold measurement for justifying emergency authority').

omega_variable(
    sunset_enforcement_mechanism,
    'What institutional mechanisms actually force the emergency structure to sunset? Is the deadline credible or performative?',
    'Historical case studies of past emergency bodies; analysis of deadline extensions and justifications; institutional design review of default reversion rules',
    'If sunset is credible: genuine Scaffold with real constraint termination. If sunset is performative: structure persists through extension/justification cycles, converting to Piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_enforcement_mechanism, empirical, 'Credibility of sunset enforcement and deadline mechanisms').

omega_variable(
    bureau_bureaucratization,
    'Does the emergency bureau develop its own self-perpetuating bureaucracy that extends constraints beyond the crisis period, converting temporary coordination into permanent extraction?',
    'Tracking of bureau staffing and budgets relative to crisis metrics; analysis of organizational culture and self-preservation incentives; post-sunset evaluation of how much authority actually reverts',
    'If bureaucratization occurs: Scaffold decays into Piton as theater increases and coordination function atrophies. If prevented: genuine sunset occurs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bureau_bureaucratization, empirical, 'Whether emergency bureau develops self-perpetuating bureaucracy').

omega_variable(
    coordination_necessity,
    'Is the centralized coordination actually necessary, or could crisis response work through lateral coordination among existing agencies without a new central authority?',
    'Comparative institutional analysis; simulation of crisis response using existing authority structures; international case studies of alternative coordination mechanisms',
    'If coordination is necessary: legitimate Scaffold. If lateral coordination is feasible: emergency bureau becomes unnecessary constraint, potentially Snare masquerading as Scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_necessity, conceptual, 'Whether centralized coordination is structurally necessary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emergency_oversight_bureau, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eob_tr_t0, emergency_oversight_bureau, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eob_tr_t2, emergency_oversight_bureau, theater_ratio, 2, 0.48).
narrative_ontology:measurement(eob_tr_t4, emergency_oversight_bureau, theater_ratio, 4, 0.62).

% Extraction over time
narrative_ontology:measurement(eob_be_t0, emergency_oversight_bureau, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(eob_be_t2, emergency_oversight_bureau, base_extractiveness, 2, 0.23).
narrative_ontology:measurement(eob_be_t4, emergency_oversight_bureau, base_extractiveness, 4, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emergency_oversight_bureau, enforcement_mechanism).
narrative_ontology:affects_constraint(emergency_oversight_bureau, regional_autonomy_suspension).
narrative_ontology:affects_constraint(emergency_oversight_bureau, emergency_authority_expansion).

% DUAL FORMULATION NOTE:
% The emergency oversight bureau is downstream of the crisis trigger but represents a distinct structural constraint. Upstream constraints include the crisis event itself and the fragmentation of normal governance authority. The bureau's extractiveness (0.28) and sunset structure reflect the institutional design response to those upstream problems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
