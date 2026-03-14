% ============================================================================
% CONSTRAINT STORY: hong_kong_electoral_system_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hong_kong_electoral_system_constraint, []).

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
 *   constraint_id: hong_kong_electoral_system_constraint
 *   human_readable: Hong Kong Electoral System Constraint
 *   domain: political_governance/electoral_systems
 *
 * SUMMARY:
 *   The Hong Kong electoral system constraint operates as a structural
 *   mechanism for maintaining Beijing's political control while preserving
 *   the institutional appearance of democratic governance. The system has
 *   evolved from 1997 (initial handover with broader electoral
 *   representation) through 2019 (pro-democracy landslide disrupting expected
 *   outcomes) to 2020-2024 (National Security Law and electoral reforms
 *   dramatically restricting candidate eligibility and political expression).
 *   The constraint manifests through multiple reinforcing mechanisms:
 *   candidate disqualification (Article 44 oath compliance testing), district
 *   boundary gerrymandering, functional constituency seats weighted toward
 *   business interests, and appointment mechanisms that bypass electoral
 *   will. The extractiveness has increased over the interval as Beijing has
 *   shifted from coordination framing ('One Country Two Systems') toward
 *   explicit control mechanisms. Theater ratio has similarly increased as the
 *   system has become less responsive to electoral outcomes, forcing greater
 *   reliance on legitimacy theater to maintain institutional appearance.
 *
 * KEY AGENTS:
 *   - Beijing Central Government: Primary beneficiary (institutional/arbitrage) — maintains political control over Hong Kong while preserving global legitimacy claims of autonomy
 *   - Pro-Democracy Movement: Primary victim (powerless/trapped) — cannot exit political struggle; faces disqualification, surveillance, legal jeopardy
 *   - Hong Kong Electorate: Primary victim (powerless/trapped) — voting options pre-filtered; meaningful choice eliminated
 *   - Pro-Beijing Political Parties: Secondary beneficiary (powerful/mobile) — guaranteed electoral dominance despite popular opposition
 *   - Hong Kong Establishment Elites: Secondary beneficiary (powerful/arbitrage) — wealth and status preserved through functional constituencies and appointment mechanisms
 *   - Electoral Commission: Institutional actor (institutional/arbitrage) — manages legitimacy theater while implementing political screening externally determined
 *   - Civil Society Organizations: Mixed status (organized/constrained) — can mobilize voters but face restrictions on political speech and organizing
 *   - International Democratic Community: External observer (analytical/analytical) — sees structural mismatch between electoral form and authoritarian substance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hong_kong_electoral_system_constraint, 0.68).
domain_priors:suppression_score(hong_kong_electoral_system_constraint, 0.72).
domain_priors:theater_ratio(hong_kong_electoral_system_constraint, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hong_kong_electoral_system_constraint, extractiveness, 0.68).
narrative_ontology:constraint_metric(hong_kong_electoral_system_constraint, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hong_kong_electoral_system_constraint, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hong_kong_electoral_system_constraint, snare).
narrative_ontology:human_readable(hong_kong_electoral_system_constraint, "Hong Kong Electoral System Constraint").
narrative_ontology:topic_domain(hong_kong_electoral_system_constraint, "political_governance/electoral_systems").

domain_priors:requires_active_enforcement(hong_kong_electoral_system_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hong_kong_electoral_system_constraint, beijing_central_government).
narrative_ontology:constraint_beneficiary(hong_kong_electoral_system_constraint, pro_beijing_political_parties).
narrative_ontology:constraint_beneficiary(hong_kong_electoral_system_constraint, hong_kong_establishment_elites).
narrative_ontology:constraint_victim(hong_kong_electoral_system_constraint, pro_democracy_movement).
narrative_ontology:constraint_victim(hong_kong_electoral_system_constraint, independent_candidates).
narrative_ontology:constraint_victim(hong_kong_electoral_system_constraint, hong_kong_electorate).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRO-DEMOCRACY ACTIVIST (SNARE) — Trapped within Hong Kong's constrained electoral system. Cannot exit political participation without abandoning advocacy. Bears maximum extraction: candidacy screening, disqualification rules, restricted political expression, surveillance. No effective alternative for effecting political change.
constraint_indexing:constraint_classification(hong_kong_electoral_system_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ORDINARY VOTER (SNARE) — Trapped in a system where voting options are pre-filtered. Cannot vote for banned parties or disqualified candidates. Participation appears free but is structurally constrained. Bears extraction through reduced meaningful choice and suppressed political voice.
constraint_indexing:constraint_classification(hong_kong_electoral_system_constraint, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: BEIJING CENTRAL GOVERNMENT (ROPE) — Experiences the constraint as coordination mechanism. The electoral system coordinates Hong Kong governance with mainland preferences. Benefits from extraction: maintains political control while preserving appearance of local autonomy. Has exit option: can modify the system at will. Net beneficiary with arbitrage capacity.
constraint_indexing:constraint_classification(hong_kong_electoral_system_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: PRO-BEIJING POLITICAL ESTABLISHMENT (ROPE) — Experiences the constraint as coordination beneficial to their interests. System guarantees their electoral dominance. Benefits from the structure while maintaining legitimacy through electoral theater. Can shift positions within the system with relatively low cost.
constraint_indexing:constraint_classification(hong_kong_electoral_system_constraint, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: CIVIL SOCIETY ORGANIZATIONS (TANGLED ROPE) — Experience both coordination and extraction. The system enables civic participation through voting and some forms of engagement, but constrains advocacy, organizing, and political expression. Benefits from electoral legitimacy while constrained by regulations and surveillance. Moderate agency but significant costs to more radical political action.
constraint_indexing:constraint_classification(hong_kong_electoral_system_constraint, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: ELECTORAL COMMISSION (PITON) — Manages performative electoral process. Conducts technical operations (counting votes, managing polling) while applying political screening criteria determined externally. Theater ratio high because the legitimacy function (free elections) is decoupled from the actual governance function (ensuring pro-Beijing outcomes). The institution maintains rituals divorced from substantive outcomes.
constraint_indexing:constraint_classification(hong_kong_electoral_system_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 7: INTERNATIONAL OBSERVER (SNARE) — From global/analytical context, the constraint is pure extraction masked by democratic forms. Electoral system extracts political control from Hong Kong society while maintaining the apparatus of electoral legitimacy. No genuine competing perspectives exist within the system's structure — all meaningful paths lead to same outcome. Classification is invariant across time horizons when exit options are considered.
constraint_indexing:constraint_classification(hong_kong_electoral_system_constraint, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hong_kong_electoral_system_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hong_kong_electoral_system_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hong_kong_electoral_system_constraint, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hong_kong_electoral_system_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hong_kong_electoral_system_constraint, TR),
    TR >= 0.70.

:- end_tests(hong_kong_electoral_system_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The system extracts political voice from Hong Kong society to serve Beijing's control objectives. The value reflects that extraction is not total (some voting still occurs, some dissent remains visible) but is extensive and systematic. The trajectory (0.35→0.68 over 14-year interval) shows escalation from coordination framing toward explicit control extraction as pro-democracy movements threatened outcomes. Suppression (0.72): High. Multiple mechanisms constrain political participation: candidacy screening (eliminating pro-independence candidates), NSL prosecutions (silencing opposition voices), surveillance infrastructure, restricted political speech, and reduced effective choice. Suppression is structural and institutionalized through law, not merely contingent on enforcement decisions. Theater ratio (0.65): Moderate-high. Electoral procedures are conducted (voting booths, ballot counting, official announcements) creating appearance of democratic process, but outcomes are predetermined by screening and control mechanisms. The theater has increased as actual electoral responsiveness has declined — the legitimacy apparatus must compensate for reduced substantive democratic function.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's key diagnostic feature is the complete absence of beneficiary perspectives perceiving extraction costs. In genuine Tangled Rope constraints, beneficiaries acknowledge real coordination burdens they bear. In Ropes, beneficiaries experience both coordination benefits and modest distribution costs. Here: Beijing/establishment perspectives report only benefits with no costs. This asymmetry is the hallmark of Snare-classified constraints — the extraction mechanism is asymmetric and the beneficiaries deny or minimize their extraction gains.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position in the extraction flow. Beijing (institutional/arbitrage) receives extraction — low d ≈ 0.10, resulting in negative f(d) ≈ -0.08. They experience the system as beneficial coordination. Pro-Beijing elites (powerful/mobile) receive extraction benefits with minor costs — moderate d ≈ 0.35, f(d) ≈ 0.35. They experience mixed but favorable relationship. Pro-democracy activists (powerless/trapped) bear maximum extraction — high d ≈ 0.95, f(d) ≈ 1.42. They experience maximum coercion. Ordinary voters (powerless/trapped) similarly bear extraction — d ≈ 0.92, f(d) ≈ 1.35. Civil society (organized/constrained) experience intermediate extraction — d ≈ 0.65, f(d) ≈ 1.00. The derivation is transparent: beneficiaries get low d (extraction toward them = negative experience), victims get high d (extraction from them = high experience). The structure is not ambiguous — the flow is unidirectional from powerless to institutional beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that the Hong Kong electoral system is NOT a coordination mechanism that has degraded or become extractive. It was designed as a control mechanism from inception (1997 Joint Declaration framed democracy narrowly) and has become progressively more explicit about its control function. The constraint is not a rope that became a snare — it is a snare masquerading as rope. The 'democracy' framing provides the mandatrophy cover: system defenders claim it IS coordination (elections DO occur, votes ARE counted) while evidence shows it is pure extraction (outcomes predetermined, dissent eliminated, choice illusory). The resolution is structural not reframing: the system functions as designed (extracting Hong Kong autonomy to serve Beijing) but cannot admit this function publicly. Suppression > 0.70 and extractiveness > 0.66 confirm the Snare classification against mandatrophy claims. The system has never been a legitimate coordination mechanism — calling it such is false naturalization, not tragic degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    electoral_legitimacy_vs_control,
    'Is the Hong Kong electoral system primarily designed for political control (snare) or for coordination between Beijing and local interests (rope)?',
    'Historical analysis of decision-making: do outcomes consistently favor Beijing''s preferences across diverse policy domains? Does the system adapt to local preferences or enforce predetermined results?',
    'If control-oriented: classification remains Snare across most perspectives. If coordination-oriented: primary beneficiary perspectives shift toward Rope. The distinction determines whether suppression is structural or contingent on Beijing''s preferences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(electoral_legitimacy_vs_control, empirical, 'Whether system is designed for control or coordination').

omega_variable(
    candidacy_screening_mechanism,
    'Are candidacy screening rules (Article 44 of electoral law, ''love the country'' oath) administrative efficiency measures or political veto mechanisms?',
    'Examination of screening criteria application: are they applied consistently across political orientations? Do disqualifications follow predictable patterns relative to Beijing preferences? Can independent candidates meet the criteria?',
    'If administrative: screening is a coordination cost (shifting classification toward Rope). If political veto: screening is pure extraction mechanism (maintaining Snare). Current evidence suggests political veto, but the ambiguity allows system defenders to maintain coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(candidacy_screening_mechanism, empirical, 'Whether candidacy screening is administrative or political filtering').

omega_variable(
    identity_locked_activist_exit,
    'Do pro-democracy activists remain trapped in the electoral system due to structural barriers or due to identity fusion with political struggle?',
    'Behavioral analysis: do activists maintain engagement with system despite personal cost? Would they accept alternative political channels (e.g., external advocacy, migration)? Is continued participation identity-constituting?',
    'If identity-locked: exit option should be reclassified from ''trapped'' to ''identity_locked'', changing biographical perspective classification from Snare to Rope. This reveals cognitive capture by the legitimacy frame. If trapped: current classification stands — structural barriers prevent exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_activist_exit, conceptual, 'Whether activist constraint is structural entrapment or identity-based cognitive lock').

omega_variable(
    theater_ratio_accuracy,
    'What proportion of electoral activity is performative vs functional? Are public hearings, debate events, and counting procedures genuine civic participation or theatrical legitimacy theater?',
    'Content analysis of electoral events: do they influence outcomes? Do vote counts matter for policy outcomes? Comparison with pre-determined governance structures that bypass electoral input.',
    'If theater_ratio > 0.75: classification shifts toward Piton (degraded institution). If theater_ratio < 0.55: classification shifts toward Tangled Rope (genuine but constrained coordination). Current estimate (0.65) reflects mixed evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_accuracy, empirical, 'Proportion of electoral activity that is performative vs functional').

omega_variable(
    voter_suppression_mechanism,
    'Does reduced voter turnout reflect voter apathy from degraded system (Piton) or active suppression preventing participation?',
    'Turnout trend analysis: declining participation due to disillusionment or active barriers? Comparison with pre-2020 (higher turnout) to post-NSL period. Exit polling on decision to vote/abstain.',
    'If apathy: classification shifts toward Piton with lower suppression metric. If active suppression: classification remains Snare with high suppression. Suppression value drives χ calculation directly — ambiguity here affects classification confidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voter_suppression_mechanism, empirical, 'Whether reduced turnout reflects apathy or active suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hong_kong_electoral_system_constraint, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hkec_tr_t0, hong_kong_electoral_system_constraint, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hkec_tr_t7, hong_kong_electoral_system_constraint, theater_ratio, 7, 0.52).
narrative_ontology:measurement(hkec_tr_t14, hong_kong_electoral_system_constraint, theater_ratio, 14, 0.65).
narrative_ontology:measurement(hkec_tr_t3, hong_kong_electoral_system_constraint, theater_ratio, 3, 0.42).
narrative_ontology:measurement(hkec_tr_t10, hong_kong_electoral_system_constraint, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(hkec_be_t0, hong_kong_electoral_system_constraint, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hkec_be_t7, hong_kong_electoral_system_constraint, base_extractiveness, 7, 0.55).
narrative_ontology:measurement(hkec_be_t14, hong_kong_electoral_system_constraint, base_extractiveness, 14, 0.68).
narrative_ontology:measurement(hkec_be_t3, hong_kong_electoral_system_constraint, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(hkec_be_t10, hong_kong_electoral_system_constraint, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hong_kong_electoral_system_constraint, enforcement_mechanism).
narrative_ontology:affects_constraint(hong_kong_electoral_system_constraint, hong_kong_national_security_law_constraint).
narrative_ontology:affects_constraint(hong_kong_electoral_system_constraint, hong_kong_press_freedom_constraint).
narrative_ontology:affects_constraint(hong_kong_electoral_system_constraint, hong_kong_civil_society_constraint).

% DUAL FORMULATION NOTE:
% Electoral system constraint is upstream of NSL implementation, press freedom restrictions, and civil society constraints. Electoral mechanism is the primary control architecture; NSL and speech restrictions are enforcement instruments supporting electoral control. The three downstream constraints have higher extractiveness values reflecting more specialized targeting (speech vs voting vs organizing), but all derive structural leverage from the electoral system's foundation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hong_kong_electoral_system_constraint, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
