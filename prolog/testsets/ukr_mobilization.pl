% ============================================================================
% CONSTRAINT STORY: ukr_mobilization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ukr_mobilization, []).

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
 *   constraint_id: ukr_mobilization
 *   human_readable: Ukrainian Mobilization Law and Forced Military Service
 *   domain: political/military
 *
 * SUMMARY:
 *   The Ukrainian Mobilization Law represents the state's attempt to organize
 *   collective military defense against Russian invasion through
 *   conscription. The constraint operates primarily as a snare: it extracts
 *   labor, physical risk, and years of life from conscripted citizens with
 *   suppression of exit alternatives through legal penalties, border
 *   controls, and social coercion. However, multiple perspectives reveal a
 *   more complex structure. The state apparatus views mobilization as tangled
 *   rope — genuine coordination for collective survival bundled with coercive
 *   extraction. Volunteer defenders and organized military units perceive the
 *   constraint as enabling collective defense while simultaneously bypassing
 *   consent mechanisms. International law frameworks legitimize the practice
 *   through institutional inertia while the actual operational structure
 *   remains highly coercive. The extractiveness has risen over the interval
 *   as initial volunteer enthusiasm has waned and conscription mechanisms
 *   have tightened. Theater ratio remains relatively low because the
 *   constraint's function is primarily operational (actually fielding
 *   military personnel) rather than performative.
 *
 * KEY AGENTS:
 *   - Conscripted Male Citizens (18-60): Primary victims (powerless/trapped) — bear extraction of military service, physical risk, and life disruption with minimal exit options
 *   - Reluctant Conscripts with Resources: Secondary victims (moderate/constrained) — can access escape routes but at high cost (bribery, emigration) and legal risk
 *   - Volunteer Defenders and Military Units: Organized beneficiaries (organized/constrained) — benefit from mobilization as coordination mechanism for collective defense; also constrained by conscription's coercive structure
 *   - Ukrainian State Apparatus: Primary beneficiary (institutional/arbitrage) — captures military capacity and maintains territorial defense; arbitrage exit reflects the state's ability to reformulate the constraint or demobilize
 *   - International Community: Secondary institutional actor (institutional/constrained) — legitimizes mobilization through IHL frameworks but cannot enforce demobilization or exit conditions; constrained by norms of state sovereignty
 *   - International Humanitarian Law System: Performative actor (institutional/arbitrage) — provides legal legitimacy for conscription; theater-high because the framework permits without constraining actual coercive operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ukr_mobilization, 0.68).
domain_priors:suppression_score(ukr_mobilization, 0.72).
domain_priors:theater_ratio(ukr_mobilization, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ukr_mobilization, extractiveness, 0.68).
narrative_ontology:constraint_metric(ukr_mobilization, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ukr_mobilization, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ukr_mobilization, snare).
narrative_ontology:human_readable(ukr_mobilization, "Ukrainian Mobilization Law and Forced Military Service").
narrative_ontology:topic_domain(ukr_mobilization, "political/military").

domain_priors:requires_active_enforcement(ukr_mobilization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ukr_mobilization, state_military_apparatus).
narrative_ontology:constraint_victim(ukr_mobilization, conscripted_male_citizens).
narrative_ontology:constraint_victim(ukr_mobilization, forced_service_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSCRIPTED SOLDIER (SNARE) — Men of military age cannot exit the mobilization system. Legal penalties, social ostracism, and border controls eliminate all alternatives. The constraint extracts labor, physical risk, and years of life with minimal negotiation or compensation relative to the extraction's cost. No coordination function exists from this perspective — only coercion.
constraint_indexing:constraint_classification(ukr_mobilization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RELUCTANT CONSCRIPT WITH ESCAPE OPTIONS (SNARE) — Some men can exit through bribery, emigration, or medical exemptions, but these are expensive and legally risky. The constraint remains snare-classified because the suppression of legitimate alternatives is high — proper exit channels are limited and the legal regime heavily penalizes non-compliance. Moderate power derives from partial access to escape mechanisms, but these do not rise to 'mobile' status because they carry severe sanctions.
constraint_indexing:constraint_classification(ukr_mobilization, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: VOLUNTEER DEFENDERS AND MILITARY COMMAND (TANGLED ROPE) — Military volunteers and organized defensive units benefit from the mobilization system in that it coordinates collective defense against invasion. The constraint does possess a genuine coordination function — organizing citizens for collective security. However, the enforcement mechanism is simultaneously extractive: conscription bypasses consent mechanisms and channels citizens into roles determined by the state. Organized agents see a hybrid structure with both coordination (collective defense necessity) and extraction (coercive implementation).
constraint_indexing:constraint_classification(ukr_mobilization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: UKRAINIAN STATE APPARATUS (TANGLED ROPE) — The state benefits from mobilization as a mechanism to coordinate national defense and maintain territorial integrity during existential threat. The constraint serves a genuine coordination function: without mobilization, collective defense capacity collapses. However, the state also extracts through coercive enforcement, legal penalties, and conscription without full consent. The institutional perspective captures the hybrid nature — coordination for collective security bundled with extraction of labor and compliance.
constraint_indexing:constraint_classification(ukr_mobilization, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL COMMUNITY AND POST-CONFLICT LOGIC (SCAFFOLD) — From the perspective of post-conflict reconstruction and international legal norms, mobilization is framed as temporary: it is explicitly tied to the existence of the military threat. International law recognizes temporary conscription during armed conflict as legitimate. The institutional framework has built-in sunset logic — mobilization should end when the conflict ends. However, this perspective assumes the war does terminate, making the sunset structural only if the time horizon is finite.
constraint_indexing:constraint_classification(ukr_mobilization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL HUMANITARIAN LAW (PITON) — The IHL framework treating conscription as a legitimate state practice during armed conflict is largely performative: the legal form permits the practice but does not substantially modify its coercive character or constrain its actual operation. The legitimating framework persists through institutional inertia (centuries of state practice) rather than because it solves the underlying coordination problem in any meaningful way. Theater ratio is high because compliance with IHL forms provides legal cover while operational coercion remains severe.
constraint_indexing:constraint_classification(ukr_mobilization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, mobilization may appear as an immutable constraint of statecraft: the state's capacity to command resources in existential conflict is a structural feature of political organization. However, this is a false summit. The engine's detection system will flag this as naturalization of a contingent institutional arrangement. The 'law of war' framing obscures that alternative coordination mechanisms (volunteer recruitment, mercenary forces, international military support) exist and are chosen.
constraint_indexing:constraint_classification(ukr_mobilization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ukr_mobilization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ukr_mobilization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ukr_mobilization, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ukr_mobilization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ukr_mobilization, TR),
    TR >= 0.70.

:- end_tests(ukr_mobilization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts military service, physical risk, years of life, and reproductive time from conscripted males. The extraction is severe because the payoff is not individual benefit but collective survival, meaning individual costs are not compensated through individual returns. The value of 0.68 reflects that while extraction is severe, it is not total (some coordination function exists, and some voluntary participation occurs). Suppression (0.72): High. Legal penalties for non-compliance are severe (criminal charges, asset seizure). Exit alternatives are suppressed: emigration is legally restricted; medical exemptions are scrutinized; bribery is illegal and expensive. The constraint explicitly blocks the primary exit option (refusing military service). Theater ratio (0.35): Low. The constraint's primary function is operational — actually recruiting, training, and fielding military personnel. The percentage of activity that is performative (legal compliance forms, IHL documentation, exemption hearings) is lower than in pure extraction constraints because the constraint must actually produce combat capability. As war duration extends and conscription tightens, theater ratio has remained relatively stable rather than increasing toward piton-like levels.
 *
 * PERSPECTIVAL GAP:
 *   The conscripted soldier perceives pure extraction (snare): coercion without coordination benefit. The state perceives tangled rope: coordination for collective defense bundled with coercive implementation necessary for military effectiveness. Volunteer defenders perceive mixed coordination and coercion (tangled rope): mobilization enables their defensive role but also conscripts non-volunteers. International humanitarian law perceives legitimacy (piton): the institutional framework treats conscription as lawful and temporary, but this framework is largely performative — it legitimizes rather than constrains the actual coercive operation. The analytical observer risks seeing naturalized inevitability (mountain): that states necessarily conscript during existential conflict. However, this is a false summit: alternative coordination mechanisms exist (volunteer recruitment, mercenary forces, international military support, humanitarian exemptions) and the choice of conscription reflects contingent institutional preferences rather than immutable law. The perspectival gap reveals that the snare classification is primary — the constraint's structure is fundamentally extractive, with coordination framing layered atop as justification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the structural position of each agent relative to the extraction flow. Conscripted males are victims with trapped exit options: high d → high f(d) → high experienced extraction (snare). The state is the beneficiary with arbitrage exit options: low d → low f(d) → low experienced extraction (rope from state perspective, but the victims' perspective dominates classification). Volunteer defenders are organized with constrained exit: medium-high d → moderate f(d) → moderate experienced extraction (tangled rope reflects mixed benefits and costs). International law institutions have arbitrage exit (can reformulate frameworks) but are downstream observers: medium d → moderate f(d). The snare classification reflects that the victims' structural position determines the constraint's primary character — the state cannot reframe extraction as pure coordination when it is operationally enforced through coercion on powerless agents.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE vs. TANGLED ROPE DISTINCTION: The constraint could be misclassified as tangled rope (coordination for collective defense) if the analysis emphasizes the genuine coordination function. The mandatrophy test resolves this: Does the constraint require suppression of alternatives AND coercive enforcement to maintain? Yes — without legal penalties for non-compliance, voluntary recruitment would be the primary mechanism. Does the constraint produce coordination benefits for the victims or primarily extracts from them? The benefits (national survival) are collective abstractions; individual conscripts bear concentrated costs. The snare classification is correct: the constraint is maintained primarily through extraction (legal coercion, border control, social penalty) not through coordination incentives. The tangled rope perspective reflects the state's experience, not the constraint's structural character. Mandatrophy resolved by differentiating victim experience (snare) from beneficiary experience (tangled rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temporal_boundary_postwar,
    'Will mobilization legally and practically end when the armed conflict concludes, or will the constraint persist through institutional inertia?',
    'Post-conflict compliance analysis: comparison of demobilization timelines and legal sunset enforcement in Ukraine vs. other post-conflict states (e.g., Georgia 2008, Israel ongoing); tracking of legislative proposals post-armistice',
    'If sunset is real: scaffold classification confirmed, suppression will decline post-war. If inertia persists: constraint drifts to piton (theater increases, extraction continues in attenuated form). Determines whether mobilization is fundamentally temporary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_boundary_postwar, empirical, 'Whether post-war mobilization ends as promised or persists through inertia').

omega_variable(
    voluntary_recruitment_sufficiency,
    'Could Ukraine''s defense needs be met through voluntary recruitment alone, or is conscription structurally necessary for the stated military capacity?',
    'Comparative analysis of volunteer recruitment rates vs. conscription gaps; modeling of defensive capability with 100% volunteer force; analysis of volunteer motivation trajectories during protracted conflict',
    'If voluntary sufficient: conscription is pure extraction disguised as necessity (snare confirmed, suppression unjustified). If genuinely insufficient: mobilization is required coordination (tangled rope confirmed, suppression is justified overhead). Determines whether the snare classification is structural or contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_recruitment_sufficiency, empirical, 'Whether voluntary recruitment could meet Ukraine''s defense needs').

omega_variable(
    exit_option_effectiveness,
    'How effective are the claimed exit routes (medical exemption, financial hardship, critical occupation designation, emigration)? Are they genuine alternatives or performative safety valves?',
    'Data on exemption grant rates, costs of bribery and legal challenge, emigration statistics by military-age males, follow-up on exemption claims (do they persist or are they later revoked?)',
    'If routes are genuine: exit_options should upgrade from ''trapped'' to ''constrained'' for broader population, reducing experienced extraction. If performative: trapped status confirmed, suppression values are understated. Determines whether the constraint is mountain-like inevitability or snare-like blockade.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_effectiveness, empirical, 'Whether exit routes provide genuine alternatives or are performative theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ukr_mobilization, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ukr_mob_tr_t0, ukr_mobilization, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ukr_mob_tr_t6, ukr_mobilization, theater_ratio, 6, 0.3).
narrative_ontology:measurement(ukr_mob_tr_t12, ukr_mobilization, theater_ratio, 12, 0.35).

% Extraction over time
narrative_ontology:measurement(ukr_mob_be_t0, ukr_mobilization, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(ukr_mob_be_t6, ukr_mobilization, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(ukr_mob_be_t12, ukr_mobilization, base_extractiveness, 12, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ukr_mobilization, enforcement_mechanism).
narrative_ontology:affects_constraint(ukr_mobilization, labor_market_depletion).
narrative_ontology:affects_constraint(ukr_mobilization, refugee_exodus_legal_framework).
narrative_ontology:affects_constraint(ukr_mobilization, male_demographic_decline).

% DUAL FORMULATION NOTE:
% Ukrainian mobilization is downstream of the Russian invasion constraint (which creates the existential threat) but represents a distinct structural constraint with its own extractiveness. The mobilization mechanism could be decomposed further into separate constraints: the legal conscription framework (snare) vs. the voluntary recruitment coordination system (rope), but they are operationally unified and the legal framework dominates, justifying a single story at snare classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
