% ============================================================================
% CONSTRAINT STORY: sotu_1954_eisenhower_consolidated_foreign_assistance_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1954_eisenhower_consolidated_foreign_assistance_authority, []).

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
 *   constraint_id: sotu_1954_eisenhower_consolidated_foreign_assistance_authority
 *   human_readable: Consolidated Foreign Assistance Authority Under Defense (1954 Eisenhower Proposal)
 *   domain: governance/executive_power/institutional_design
 *
 * SUMMARY:
 *   The 1954 Eisenhower proposal to consolidate all foreign
 *   assistance—military, technical, and economic—under Department of Defense
 *   administration with presidential transfer authority represents a
 *   foundational shift in executive institutional design during the Cold War.
 *   The constraint addresses a coordination problem (fragmented civilian aid
 *   administration limiting rapid reallocation) while simultaneously
 *   concentrating executive discretion and reducing congressional oversight
 *   transparency. This is a canonical tangled_rope case: genuine coordination
 *   efficiency coexists with genuine asymmetric extraction of appropriations
 *   authority from Congress. The constraint benefits the executive branch and
 *   Defense Department by eliminating bureaucratic friction between aid
 *   categories and enabling rapid reallocation without congressional
 *   reauthorization. It harms congressional appropriations authority (which
 *   becomes hollow when funds are consolidated and transferable), aid
 *   transparency (subsumed into Defense security classifications), and
 *   regional aid recipients (who lose predictability and accountability). The
 *   theater_ratio (0.38) reflects that much of the institutional legitimacy
 *   rests on the State Department 'policy guidance' fiction: State
 *   articulates policy frameworks while Defense executes with operational
 *   discretion, making the coordination mechanism partly ceremonial as
 *   military strategic logic increasingly dominates allocation decisions.
 *
 * KEY AGENTS:
 *   - Executive Branch / President: Primary beneficiary (institutional/arbitrage) — gains rapid reallocation authority and consolidated control; highest structural benefit from consolidation
 *   - Department of Defense: Primary beneficiary (institutional/arbitrage) — gains operational control over civilian aid streams and strategic positioning authority; benefits from integration of military and economic tools
 *   - Congress / Appropriations Committees: Primary victim (institutional/trapped) — formal constitutional authority over spending becomes procedurally hollow when consolidated funds are transferable; constrained from meaningful reallocation without new legislation
 *   - State Department: Secondary institutional actor (institutional/constrained) — retains policy guidance authority but faces operational subordination to Defense; theater role increases as execution control shifts
 *   - Foreign aid recipients: Secondary victim (moderate/constrained) — face reduced predictability, increased Cold War conditionality, reduced transparency; exit options limited by geopolitical dependence on US support
 *   - Congress Opposition / Civil Society: Organized potential reformers (powerful/mobile) — perceive this as temporary institutional arrangement that will trigger legislative response and reform demands
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent features of executive governance under geopolitical threat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, 0.58).
domain_priors:suppression_score(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, 0.52).
domain_priors:theater_ratio(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, tangled_rope).
narrative_ontology:human_readable(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, "Consolidated Foreign Assistance Authority Under Defense (1954 Eisenhower Proposal)").
narrative_ontology:topic_domain(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, "governance/executive_power/institutional_design").

domain_priors:requires_active_enforcement(sotu_1954_eisenhower_consolidated_foreign_assistance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, executive_branch).
narrative_ontology:constraint_beneficiary(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, defense_department).
narrative_ontology:constraint_beneficiary(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, presidential_discretion).
narrative_ontology:constraint_victim(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, congressional_appropriations_authority).
narrative_ontology:constraint_victim(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, transparency_accountability).
narrative_ontology:constraint_victim(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, regional_aid_recipients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONGRESSIONAL APPROPRIATIONS AUTHORITY (SNARE) — Congress has constitutional power of the purse but faces institutional attrition through executive transfer authority. Once funds are appropriated and consolidated under Defense, Congress cannot reallocate without new legislation. The constraint traps the legislative branch in a structural position where formal authority is hollow — Congress appropriates but cannot control allocation. No exit mechanism exists short of constitutional amendment or legislative override during appropriation process.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL AID RECIPIENTS (SNARE) — Foreign governments receiving aid face loss of predictability and transparency. Military/economic assistance shifts without notice based on executive Cold War calculations. Recipients cannot depend on committed aid streams and cannot appeal to Congress for accountability. Suppression high: exit from dependence on US aid is costly for developing nations; institutional asymmetry prevents negotiation. Extraction of political deference accompanies economic dependence.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EXECUTIVE BRANCH (ROPE) — Consolidation under Defense represents pure coordination from the beneficiary perspective. Eliminates bureaucratic friction between military and economic aid streams. President and SecDef experience the constraint as solving the coordination problem of fragmented aid administration. Rapid reallocation capacity without congressional delay appears as coordination benefit with minimal coercive overhead. No suppression experienced by beneficiary — this perspective sees only efficiency gain.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONGRESSIONAL OVERSIGHT BODIES (TANGLED ROPE) — Organized agents (Senate Foreign Relations Committee, House Appropriations) retain some leverage: budget review hearings, public testimony, public scrutiny of aid allocation patterns. These create coordination function (forcing executive to articulate Cold War rationale to Congress) alongside extraction (executive avoids detailed justification by framing decisions as classified or urgent). Constrained exit: Congress could attempt legislative reversal, but faces executive veto and political cost of appearing obstructionist during Cold War. Both genuine coordination and genuine extraction present simultaneously.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: POLITICAL OPPOSITION (SCAFFOLD) — Powerful actors (opposition party, civil society advocates for aid transparency) perceive the consolidated authority as a temporary institutional arrangement that will trigger counter-institutional responses. Congressional Democrats and aid advocates see this as a crisis that demands sunset legislation, mandatory reporting requirements, or structural reform. The constraint has low effective extraction from this perspective because they have mobility and see exit paths: legislative reform, electoral change, institutional precedent-setting. Sunset mechanisms emerge as political counter-proposals.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: STATE DEPARTMENT POLICY GUIDANCE (PITON) — State Department retains nominal policy guidance authority but Defense executes allocation. The State/Defense coordination mechanism is substantially performative: State articulates policy; Defense implements with significant discretion. Over time, Defense operational logic (strategic positioning, military access, base negotiations) supersedes State policy guidance. The formal 'policy guidance' structure persists as theater — procedures are followed but Defense's military imperative drives actual allocation. Theater ratio high because the constraint persists through institutional ritual rather than functional necessity.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, the consolidation appears as an immutable feature of executive governance under conditions of perceived existential threat (Cold War). The perspective naturalizes executive centralization as inherent to geopolitical competition requiring rapid response. Frames consolidated authority as a law of executive organization under great-power rivalry. However, this classification is likely a false summit: the constraint benefits identifiable institutional actors (Defense, presidency) and can be reversed through institutional reform, making it contingent rather than natural.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1954_eisenhower_consolidated_foreign_assistance_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, TR),
    TR >= 0.70.

:- end_tests(sotu_1954_eisenhower_consolidated_foreign_assistance_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The consolidation generates measurable executive advantage through control consolidation and transfer authority. However, extraction is not as severe as a pure Snare (ε ≥ 0.66) because Congress retains appropriations authority and oversight mechanisms persist: budget reviews, testimony, public scrutiny of allocation patterns. The extraction is genuine (Congress's formal authority becomes procedurally limited) but not total (institutional constraints remain). The value reflects that consolidation solves a coordination problem while simultaneously enabling extraction — both are real. Suppression (0.52): Moderate-high. Significant barriers exist to congressional reclamation of spending control: executive veto authority, Cold War framing that makes congressional obstruction politically costly, integration of aid into Defense operational security. However, suppression is not absolute — Congress retains budget authority and can attempt legislative reform, though at political cost. Theater ratio (0.38): Moderate. The State Department policy guidance function provides institutional legitimacy to Defense-dominated allocation, but this theater is not yet dominant (as it would be in Piton, where theater ≥ 0.70). The coordination mechanism is genuinely mixed: some State guidance is operationally binding; some is preempted by Defense strategic logic. Over the measurement interval, theater ratio increases slightly (0.28 → 0.38) as the Defense operational logic becomes more institutionalized and State guidance becomes more formulaic.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits divergent classification across organizational positions despite identical base metrics. The executive beneficiary perceives pure coordination (Rope) — they see a genuine solution to bureaucratic fragmentation. Congress perceives near-total extraction (Snare-range) — their formal authority is institutionally emptied by transfer mechanisms. The State Department perceives mixed coordination and extraction (Tangled Rope) — they maintain policy guidance but lose operational control, creating a hybrid. Congressional oversight bodies perceive moderate extraction with retained leverage (Tangled Rope) — they can demand justification and impose reputational cost, limiting extraction to moderate levels. Political reformers perceive this as a temporary crisis arrangement (Scaffold) — they see legislative reversal pathways and emerging institutional opposition. The analytical observer from a civilizational perspective risks naturalizing executive centralization as inherent to Cold War governance (Mountain), but the structural beneficiary presence indicates this is a false summit. The perspectival gap reveals how the same institutional mechanism creates radically different structural positions for different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness derives from its structural position relative to the consolidated authority. The President and Defense Department (institutional/arbitrage) experience low or negative d — they benefit from consolidation; f(d) for beneficiaries is negative, reducing their effective extraction. Congress (institutional/trapped, post-consolidation) experiences high d — appropriations authority is procedurally constrained; f(d) for institutional actors with constrained exit is moderately high. Foreign aid recipients (moderate/constrained) experience d ≈ 0.55 — they face loss of negotiating leverage and predictability but retain some agency through geopolitical alternatives. Congressional oversight bodies (organized/constrained) experience moderate d because they have institutional leverage through budget review and public scrutiny, though exit options are constrained by political cost. The beneficiary/victim declarations map directly to structural extraction: Defense gains control (beneficiary), Congress loses it (victim), State's control becomes conditional (semi-victim), recipients face reduced leverage (victim).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED through perspectival decomposition: The apparent contradiction between 'genuine coordination mechanism solving bureaucratic fragmentation' and 'pure extraction of congressional appropriations authority' is resolved by recognizing that both are structurally real from different positions. From the executive perspective, consolidation solves a genuine coordination problem (fragmented civilian aid streams preventing rapid geopolitical response). From Congress's perspective, consolidation extracts control through procedural mechanisms (transferability authority that bypasses reauthorization). These are not contradictory characterizations — they are the same constraint experienced from opposed structural positions. The Tangled Rope classification captures this: the constraint genuinely coordinates military-economic aid allocation (benefiting executive/Defense efficiency) while genuinely extracting congressional appropriations authority (harming legislative branch). Suppression (0.52) and theater (0.38) are sufficiently high to confirm hybrid status: pure coordination would have lower suppression; pure extraction would have higher theater (Piton-range). The constraint is exactly what the metrics indicate: coordination with extraction asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    executive_transfer_authority_scope,
    'Does presidential transferability authority between aid categories create coordination efficiency or primarily expand extraction of executive discretion?',
    'Historical analysis of transfer requests: frequency, justification (legitimate coordination vs. circumventing appropriations intent), and policy coherence across reallocations. Comparative analysis of Cold War aid allocation under consolidated vs. fragmented systems.',
    'If transfers primarily address coordination failures: constraint is predominantly Rope (with oversight layer). If transfers primarily circumvent congressional intent: constraint is predominantly Snare with theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(executive_transfer_authority_scope, empirical, 'Whether presidential transfer authority functions as coordination or discretionary extraction').

omega_variable(
    state_defense_policy_control,
    'Does State Department policy guidance maintain functional control over aid allocation, or is it ceremonial while Defense operational logic dominates?',
    'Case analysis of specific aid allocation decisions: alignment with State policy directives vs. Defense strategic objectives; frequency of State guidance overrides; institutional evolution of policy control over 5-year, 10-year, and 20-year periods.',
    'If State retains control: constraint is Tangled Rope with genuine coordination. If Defense dominates: constraint shifts toward Snare with piton elements (performative State role).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_defense_policy_control, empirical, 'Functional control over aid allocation between State and Defense').

omega_variable(
    congressional_political_capacity_to_reverse,
    'What political and institutional barriers prevent Congress from reasserting appropriations authority over consolidated aid funds?',
    'Analysis of legislative attempts to restore congressional control; identification of veto points (executive veto, presidential framing as Cold War necessity); tracking of public support for transparency vs. executive efficiency framing over time.',
    'If Congress retains reversibility: constraint is temporary (Scaffold). If executive entrenchment is institutionally deep: constraint is Snare with piton elements (performative congressional authority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_political_capacity_to_reverse, empirical, 'Congressional political capacity to reverse consolidated authority').

omega_variable(
    aid_recipient_political_dependence,
    'To what degree does aid recipient dependence on consolidated US funding create coercive power asymmetry, vs. representing mutually beneficial coordination?',
    'Analysis of recipient country behavior: correlation between aid volatility/conditionality and policy alignment with US Cold War objectives; exit behavior (diversification of aid sources, geopolitical alignment shift) when consolidation creates unpredictability.',
    'If dependence creates pure extraction: victims experience high d (trapped). If coordination benefits exist: victims experience moderate d (constrained).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aid_recipient_political_dependence, empirical, 'Degree of aid recipient coercion vs. mutual benefit').

omega_variable(
    transparency_loss_mechanisms,
    'Does consolidation under Defense classified operations and military secrecy doctrine systematically reduce public and congressional visibility into aid allocation?',
    'Comparison of disclosure requirements and public accountability mechanisms: fragmented system (separate civilian agency reporting) vs. consolidated system (Defense security classification). Tracking of classified aid spending vs. disclosed aid spending over time.',
    'If consolidation enables systematic suppression of transparency: supports Snare classification and high suppression metric. If transparency mechanisms persist: supports Tangled Rope with oversight layer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transparency_loss_mechanisms, empirical, 'Consolidation''s effect on transparency and public visibility of aid allocation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eise_tr_t0, sotu_1954_eisenhower_consolidated_foreign_assistance_authority, theater_ratio, 0, 0.28).
narrative_ontology:measurement(eise_tr_t2, sotu_1954_eisenhower_consolidated_foreign_assistance_authority, theater_ratio, 2, 0.33).
narrative_ontology:measurement(eise_tr_t5, sotu_1954_eisenhower_consolidated_foreign_assistance_authority, theater_ratio, 5, 0.38).

% Extraction over time
narrative_ontology:measurement(eise_be_t0, sotu_1954_eisenhower_consolidated_foreign_assistance_authority, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(eise_be_t2, sotu_1954_eisenhower_consolidated_foreign_assistance_authority, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(eise_be_t5, sotu_1954_eisenhower_consolidated_foreign_assistance_authority, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, executive_war_powers_expansion).
narrative_ontology:affects_constraint(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, foreign_aid_conditionality_mechanisms).
narrative_ontology:affects_constraint(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, defense_department_institutional_expansion).

% DUAL FORMULATION NOTE:
% This constraint is downstream of Cold War institutional design imperatives but upstream of subsequent executive power expansions. The consolidated foreign assistance authority serves as a template for integrating military and economic tools under executive control, influencing later constraints on war powers, intelligence agency expansion, and national security state development. The institutional mechanism (presidential transfer authority without reauthorization) propagates to other policy domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1954_eisenhower_consolidated_foreign_assistance_authority, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
