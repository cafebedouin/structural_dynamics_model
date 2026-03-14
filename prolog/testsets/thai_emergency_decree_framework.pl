% ============================================================================
% CONSTRAINT STORY: thai_emergency_decree_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thai_emergency_decree_framework, []).

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
 *   constraint_id: thai_emergency_decree_framework
 *   human_readable: Thai Emergency Decree Framework: Institutional Power Concentration and Suppression
 *   domain: political/institutional/governance
 *
 * SUMMARY:
 *   Thailand's Emergency Decree framework represents an institutional
 *   mechanism for concentrating executive power under the formal
 *   justification of crisis management. Since 2014, successive military and
 *   quasi-military governments have invoked emergency decrees to suppress
 *   political opposition, restrict media freedom, disable judicial review,
 *   and consolidate administrative control. The constraint exhibits the full
 *   range of DR classification: the military apparatus perceives it as pure
 *   coordination (Rope), suppressed opposition experiences it as pure
 *   extraction (Snare), the weakened judiciary operates as a captured hybrid
 *   (Tangled Rope), the bureaucracy functions as degraded machinery (Piton),
 *   and international observers frame it as temporary crisis response
 *   (Scaffold). The extractiveness has increased from 0.45 to 0.68 over the
 *   measurement interval, indicating acceleration of extraction mechanisms —
 *   successive renewals have expanded scope and deepened suppression rather
 *   than ameliorating emergency. Theater ratio increasing from 0.42 to 0.65
 *   indicates growing performative content: administrative procedures are
 *   maintained as legitimacy facade while substantive power consolidates in
 *   decree authority. The constraint's mandatrophy is resolved by recognizing
 *   that the emergency framing itself is the cover story — what appears as
 *   crisis response is structurally an extractive mechanism for eliminating
 *   ordinary political participation and judicial oversight.
 *
 * KEY AGENTS:
 *   - Military Executive Apparatus: Primary beneficiary (institutional/arbitrage) — gains centralized decision-making authority, eliminates legislative/judicial constraint, captures regulatory power
 *   - Suppressed Opposition: Primary victim (powerless/trapped) — faces arrest, asset seizure, travel bans, workplace retaliation; zero exit options within national jurisdiction
 *   - Cautious Civil Society: Secondary victim (moderate/constrained) — faces funding withdrawal, organizational dissolution, member harassment; exits exist but severely constrained
 *   - Independent Judiciary: Institutional victim (institutional/constrained) — retains formal structure but enforcement authority is overridden; functions as legitimacy instrument rather than check
 *   - Media Freedom: Victim (institutional/trapped) — subject to censorship orders, publishing bans, journalist detention; no legal pathway for independent reporting
 *   - Bureaucratic Administration: Degraded institution (institutional/constrained) — maintains administrative procedures but subordinate to decree authority; operates as execution apparatus
 *   - International Community: External observer (powerful/mobile) — exerts pressure via sanctions and conditional aid but lacks enforcement leverage within Thai jurisdiction; frames constraint as temporary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thai_emergency_decree_framework, 0.68).
domain_priors:suppression_score(thai_emergency_decree_framework, 0.72).
domain_priors:theater_ratio(thai_emergency_decree_framework, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thai_emergency_decree_framework, extractiveness, 0.68).
narrative_ontology:constraint_metric(thai_emergency_decree_framework, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(thai_emergency_decree_framework, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thai_emergency_decree_framework, snare).
narrative_ontology:human_readable(thai_emergency_decree_framework, "Thai Emergency Decree Framework: Institutional Power Concentration and Suppression").
narrative_ontology:topic_domain(thai_emergency_decree_framework, "political/institutional/governance").

domain_priors:requires_active_enforcement(thai_emergency_decree_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(thai_emergency_decree_framework, military_executive_apparatus).
narrative_ontology:constraint_beneficiary(thai_emergency_decree_framework, bureaucratic_power_holders).
narrative_ontology:constraint_victim(thai_emergency_decree_framework, civilian_political_participation).
narrative_ontology:constraint_victim(thai_emergency_decree_framework, independent_judiciary).
narrative_ontology:constraint_victim(thai_emergency_decree_framework, media_freedom).
narrative_ontology:constraint_victim(thai_emergency_decree_framework, protest_organizers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUPPRESSED OPPOSITION (SNARE) — Political opponents, protest organizers, and civil society actors face material barriers to exit: arrest under emergency decree, asset seizure, travel bans, workplace retaliation. The decree creates legal pathways for indefinite detention without judicial review. No genuine alternatives to submission exist within the national jurisdiction. Maximum experienced extraction — the target bears full cost of constraint with zero agency.
constraint_indexing:constraint_classification(thai_emergency_decree_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CAUTIOUS CIVIL SOCIETY (SNARE) — NGOs, academic institutions, and community organizations face high costs of visible opposition: funding withdrawal, organizational dissolution, member harassment. The exits exist but are severely constrained by suppression mechanisms. Not fully trapped but experiencing near-maximum extraction relative to moderate power. Agency is limited to obedience or exile.
constraint_indexing:constraint_classification(thai_emergency_decree_framework, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPTURED JUDICIARY (TANGLED ROPE) — Courts operate within the emergency framework as both coordinating institutions (resolving disputes, providing procedural legitimacy) and extraction mechanisms (enforcing detention orders, blocking petitions). The judiciary benefits from institutional stability but is constrained by override authority of decree issuers. Mixed extraction and coordination — institutional survival depends on cooperation.
constraint_indexing:constraint_classification(thai_emergency_decree_framework, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MILITARY EXECUTIVE APPARATUS (ROPE) — Primary beneficiary. The emergency decree is experienced as a pure coordination mechanism: it centralizes decision-making, eliminates procedural delays, enables rapid executive action without legislative oversight. The apparatus faces no material extraction cost — it extracts from other agents. The constraint solves the coordination problem of governing without consent.
constraint_indexing:constraint_classification(thai_emergency_decree_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL COMMUNITY (SCAFFOLD) — From the perspective of foreign governments and international institutions, the emergency decree is a temporary extraordinary measure with an implicit sunset: periodic renewals signal crisis management, not permanent institutional design. International pressure, sanctions, and conditional aid create incentives for termination. The international observer sees this as crisis response, not permanent extraction — but the sunset is aspirational rather than structural.
constraint_indexing:constraint_classification(thai_emergency_decree_framework, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: BUREAUCRATIC ADMINISTRATIVE SYSTEM (PITON) — The formal civilian administration operates under the emergency decree as degraded institutional machinery. Administrative protocols persist (licensing, permitting, record-keeping) but their function is subordinate to decree authority. Theater ratio is high: administrative procedures maintain appearance of rule-of-law while substantive power flows through decree mechanisms. The bureaucracy is maintained through inertia and as a legitimacy facade.
constraint_indexing:constraint_classification(thai_emergency_decree_framework, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global perspective, the emergency decree framework demonstrates how legal instruments can be repurposed for indefinite extraction under crisis justification. The analytical observer perceives the structure as a Snare: suppression is high, extraction is sustained, alternatives are systematically eliminated, and the mechanism relies on suspending ordinary checks. The 'emergency' framing naturalizes what is structurally an extractive mechanism.
constraint_indexing:constraint_classification(thai_emergency_decree_framework, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thai_emergency_decree_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(thai_emergency_decree_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thai_emergency_decree_framework, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(thai_emergency_decree_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(thai_emergency_decree_framework, TR),
    TR >= 0.70.

:- end_tests(thai_emergency_decree_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The emergency decree creates explicit legal pathways for asset seizure, indefinite detention, publication bans, and organizational dissolution without due process. The extraction is not total (0.72+) because some administrative space remains for non-political activities — ordinary business, family life, non-activist civil society can continue. But for political opposition, extractiveness approaches maximum. The trajectory from 0.45 to 0.68 reflects successive renewals expanding scope from initial 'temporary' measures to sustained institutional control. Suppression (0.72): Very high. Multiple suppression mechanisms operate in concert: legal (arrest authority, detention without trial, asset seizure), enforcement (police/military capacity), epistemic (information control through media censorship), and social (workplace retaliation, community surveillance). The suppression is nearly total for overtly political activity. Theater ratio (0.65): Moderate-high and rising. The framework maintains formal procedural appearance: announced decrees, executive orders, administrative channels. But substantive procedure is displaced — parliamentary oversight is bypassed, judicial review is prohibited, administrative appeals are overridden. The theater increased as the mechanism matured — early emergency invocations appeared temporary; later renewals reveal the institutional character while maintaining crisis-response performance. The 0.65 value reflects that administrative machinery still operates (licensing, permitting, ordinary governance) alongside decree authority — not pure theater, but significantly performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival spread: the same legal framework produces Rope classification from one position and Snare from another. The military apparatus perceives genuine coordination functions (centralizing executive authority solves procedural problems — this is truthful from their perspective). Opposition actors perceive pure extraction — legal mechanisms designed to eliminate their agency (also truthful from their perspective). The gap reveals what is actually happening: the coordination function is asymmetric. The decree 'coordinates' by concentrating power, not by distributing it. This is coordination for the beneficiary and extraction for the victim. The Tangled Rope judiciary perspective is the diagnostic bridge: courts must perform coordination function (case resolution) while being subordinated to extraction authority (decree override). This reveals the hybrid structure: the constraint coordinates executive function while extracting from oversight institutions. The analytical observer's distance allows seeing what beneficiaries and victims cannot both acknowledge: that the same mechanism is coordination for some and extraction for others — the perspectival gap is not about disagreement on facts but about occupying opposite ends of the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) encode each agent's structural relationship to the extraction flow. The military apparatus is the beneficiary — it extracts authority, eliminates constraints, consolidates power — yielding low d (0.05-0.15) and negative effective extraction (χ < 0). Suppressed opposition is the primary target — they bear maximum extraction cost, have zero exit options, cannot resist within institutional boundaries — yielding high d (0.95+) and high f(d) ≈ 1.42, producing maximum χ. Civil society is secondary victim with constrained (not trapped) exits — high d but with exit cost factor, yielding d ≈ 0.75-0.85 and moderate-high χ. Judiciary is captured institution — structurally mobile but functionally constrained by override authority, suggesting d ≈ 0.60-0.70 and mixed χ (Tangled Rope rather than pure victim). Bureaucracy is institutional executor with degraded autonomy — d ≈ 0.50-0.65 reflecting neither pure beneficiary nor pure victim status. International community observes from outside with analytical position — d ≈ 0.72 (canonical analytical d), yielding χ in moderate range reflecting distance from direct extraction flow. The directionality spread (from -0.12 to 1.42) is maximal, indicating a Snare whose extraction flow is highly asymmetric and concentrated on powerless agents.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The emergency decree framework demonstrates how 'emergency' framing serves as the cover story preventing recognition of permanent extraction. Mandatrophy emerges when actors claim coordination (Rope) for what is structurally extraction (Snare). Resolution requires distinguishing: (1) Genuine temporary crisis response (Rope structure: costs distributed, alternatives exist after emergency ends) vs (2) Permanent extraction disguised as emergency (Snare structure: costs concentrated on opposition, alternatives systematically eliminated, renewals persist beyond crisis). The measurement trajectory (extractiveness rising from 0.45→0.68, theater ratio rising from 0.42→0.65) indicates this is permanent extraction with crisis framing, not crisis response. Each renewal should decrease extractiveness and theater if genuine crisis management were occurring — instead both increase, revealing mechanism escalation. The mandatrophy is resolved by mapping perspectives: the beneficiary's honest Rope classification ('this is coordination') is contradicted by the structural data (high suppression, rising extraction, systematic elimination of opposition channels). The true structure is Snare: beneficiary experiences coordination; targets experience extraction; the gap reveals that the beneficiary's frame naturalizes extraction as coordination. International Scaffold framing ('temporary crisis') is contradicted by renewal patterns — the sunset exists in aspiration, not structure. The resolved mandatrophy is: this is a Snare whose high extractiveness (0.68) appears as Rope to beneficiaries and as temporary crisis (Scaffold) to external observers, but structural data reveals permanent institutional extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergency_legitimacy_threshold,
    'At what point does repeated emergency decree renewal cross from crisis response to permanent institutional mechanism?',
    'Historical analysis of renewal patterns; comparison with other democracies'' emergency frameworks; measurement of substantive policy change during vs outside emergency periods',
    'If threshold is crossed early (< 2 years of renewals): constraint escalates from Scaffold to permanent Snare. If threshold never clearly establishes: emergency framing persists indefinitely, concealing permanent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_legitimacy_threshold, conceptual, 'Threshold for distinguishing temporary emergency from permanent institutional mechanism').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is measured suppression (0.72) primarily structural (legal barriers, enforcement capacity) or partially internalized (self-censorship, normalized fear)?',
    'Post-decree-termination behavioral tracking: if suppression persists (self-censorship, risk aversion) after legal enforcement mechanisms are removed, suggests internalization. Comparative analysis with post-emergency societies.',
    'If primarily structural: suppression could rapidly decline with regime change. If internalized: suppression of political participation persists even after legal framework removed, extending extraction mechanism beyond formal institutional change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural enforcement or internalized psychological mechanism').

omega_variable(
    international_pressure_effectiveness,
    'Does external pressure (sanctions, diplomatic isolation, conditional aid) create genuine sunset incentives or merely force concealment of decree mechanisms?',
    'Comparison of decree renewal patterns before/after international sanctions; analysis of whether pressure causes termination or migration of repressive mechanisms to non-decree legal instruments; tracking of declared vs actual emergency scope reduction',
    'If pressure is effective: international perspective''s Scaffold classification is correct — external actors have leverage. If ineffective: international perspective is aspirational; sunset is rhetorical, and external actors lack material leverage to force constraint termination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_pressure_effectiveness, empirical, 'Whether international pressure creates genuine institutional change or cosmetic adjustment').

omega_variable(
    administrative_capture_completeness,
    'Does the civilian bureaucratic system retain any autonomous policy-setting capacity or has it been fully captured as an execution apparatus subordinate to decree authority?',
    'Analysis of policy initiation patterns: which institutions propose substantive policy changes during emergency vs non-emergency periods; tracking of cases where bureaucracy resists or modifies decree implementation',
    'If bureaucracy retains autonomy: Piton classification is correct — degraded but functional institution. If fully captured: bureaucracy is better classified as an instrument of the extractive apparatus, strengthening the Snare classification across institutional perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_capture_completeness, empirical, 'Extent of bureaucratic autonomy under emergency decree framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thai_emergency_decree_framework, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(thai_ed_tr_t0, thai_emergency_decree_framework, theater_ratio, 0, 0.42).
narrative_ontology:measurement(thai_ed_tr_t3, thai_emergency_decree_framework, theater_ratio, 3, 0.55).
narrative_ontology:measurement(thai_ed_tr_t6, thai_emergency_decree_framework, theater_ratio, 6, 0.65).
narrative_ontology:measurement(thai_ed_tr_t9, thai_emergency_decree_framework, theater_ratio, 9, 0.65).

% Extraction over time
narrative_ontology:measurement(thai_ed_be_t0, thai_emergency_decree_framework, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(thai_ed_be_t3, thai_emergency_decree_framework, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(thai_ed_be_t6, thai_emergency_decree_framework, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(thai_ed_be_t9, thai_emergency_decree_framework, base_extractiveness, 9, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thai_emergency_decree_framework, enforcement_mechanism).
narrative_ontology:affects_constraint(thai_emergency_decree_framework, thai_judicial_independence).
narrative_ontology:affects_constraint(thai_emergency_decree_framework, thai_political_participation).
narrative_ontology:affects_constraint(thai_emergency_decree_framework, thai_press_freedom).

% DUAL FORMULATION NOTE:
% The emergency decree framework is upstream to specific constraints on judicial independence, political participation, and press freedom. Those constraints each have lower extractiveness (0.35-0.55) reflecting specific sectoral impacts. This framework story captures the meta-constraint that enables and sustains them. Decomposition: this story models the legal/institutional mechanism; downstream stories model effects on specific institutions and freedoms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(thai_emergency_decree_framework, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
