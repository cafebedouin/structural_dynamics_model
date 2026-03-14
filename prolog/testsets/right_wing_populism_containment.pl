% ============================================================================
% CONSTRAINT STORY: right_wing_populism_containment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_right_wing_populism_containment, []).

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
 *   constraint_id: right_wing_populism_containment
 *   human_readable: Right-Wing Populism Containment via Institutional Gatekeeping
 *   domain: political/institutional
 *
 * SUMMARY:
 *   Right-wing populism containment represents a hybrid
 *   coordination-extraction constraint operating across media gatekeeping,
 *   institutional delegitimization, and platform censorship mechanisms. The
 *   constraint exhibits simultaneous genuine coordination function
 *   (preventing violent radicalization cascades, maintaining institutional
 *   stability) and asymmetric extraction (suppressing non-establishment
 *   political expression, preventing redistribution that threatens
 *   institutional prerogatives). Working-class economic grievances that
 *   fueled populist mobilization are both acknowledged and structurally
 *   suppressed. The theater_ratio (0.68) reflects that much of the
 *   containment operates through narrative framing and institutional status
 *   rather than direct coercion — the 'expertise-based governance'
 *   justification provides legitimacy for suppression. The extractiveness
 *   trajectory (0.35 → 0.58 over 10 years) shows acceleration as containment
 *   mechanisms intensified in response to rising populist mobilization. This
 *   constraint demonstrates how identity-locked agents experience suppression
 *   differently than trapped agents: non-establishment actors with structural
 *   mobility remain bound through identity fusion with oppositional political
 *   frames. The constraint is neither pure coordination (genuine grievances
 *   are being suppressed) nor pure extraction (institutional stability
 *   mechanisms do prevent destabilization), making tangled rope the
 *   appropriate primary classification.
 *
 * KEY AGENTS:
 *   - Economically Displaced Working Class: Primary victim (powerless/trapped) — deindustrialized regions with limited exit options; grievances suppressed through media delegitimization
 *   - Non-Establishment Political Expression: Secondary victim (powerless/identity_locked) — structurally mobile but cognitively bound by oppositional identity; identity frame prevents accepting institutional consensus
 *   - Institutional Media Gatekeepers: Primary beneficiary (institutional/arbitrage) — control information boundaries, extract through prestige and access; experience constraint as pure coordination
 *   - Center-Left Institutional Coalition: Secondary beneficiary (institutional/constrained) — maintain technocratic governance and policy prerogatives while appearing responsive to grievances
 *   - Right-Wing Political Entrepreneurs: Mixed participant (powerful/mobile) — benefit from populist mobilization but face institutional suppression, deplatforming, and coordinated media hostility
 *   - Technocratic Policy Establishment: Institutional actor (institutional/arbitrage) — maintain expertise-based authority through delegitimization of populist alternatives
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional containment mechanisms as necessary barriers to democratic instability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(right_wing_populism_containment, 0.58).
domain_priors:suppression_score(right_wing_populism_containment, 0.65).
domain_priors:theater_ratio(right_wing_populism_containment, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(right_wing_populism_containment, extractiveness, 0.58).
narrative_ontology:constraint_metric(right_wing_populism_containment, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(right_wing_populism_containment, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(right_wing_populism_containment, tangled_rope).
narrative_ontology:human_readable(right_wing_populism_containment, "Right-Wing Populism Containment via Institutional Gatekeeping").
narrative_ontology:topic_domain(right_wing_populism_containment, "political/institutional").

domain_priors:requires_active_enforcement(right_wing_populism_containment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(right_wing_populism_containment, institutional_media_gatekeepers).
narrative_ontology:constraint_beneficiary(right_wing_populism_containment, technocratic_policy_establishment).
narrative_ontology:constraint_beneficiary(right_wing_populism_containment, center_left_political_coalition).
narrative_ontology:constraint_victim(right_wing_populism_containment, working_class_economic_grievance).
narrative_ontology:constraint_victim(right_wing_populism_containment, non_establishment_political_expression).
narrative_ontology:constraint_victim(right_wing_populism_containment, direct_democratic_participation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECONOMICALLY DISPLACED WORKING CLASS (SNARE) — Trapped within deindustrialized regions with limited economic opportunity. Cannot exit the geographic/economic constraint. Populist rhetoric speaks to their grievances but containment mechanisms (media delegitimization, platform censorship, institutional mockery) prevent their political voice from translating to structural change. Maximum extraction: grievances are simultaneously acknowledged and suppressed.
constraint_indexing:constraint_classification(right_wing_populism_containment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NON-ESTABLISHMENT POLITICAL EXPRESSION (SNARE via IDENTITY_LOCKED) — Structurally mobile actors (can migrate, change communities) remain locked into grievance-expression through identity fusion. Their political identity is constituted through opposition to institutional consensus. Containment works not by blocking exit but by making exit unthinkable — accepting institutional framing would mean abandoning their core identity. The suppression is cognitive rather than material.
constraint_indexing:constraint_classification(right_wing_populism_containment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: CENTER-LEFT INSTITUTIONAL COALITION (TANGLED ROPE) — Benefits from populism containment (preserves technocratic governance, prevents redistribution threats, maintains institutional authority). But constrained by need to appear responsive to working-class concerns without enabling actual populist power. Genuine coordination function (preventing institutional instability, maintaining social cohesion) alongside asymmetric extraction (suppressing grievances that challenge their policy prerogatives).
constraint_indexing:constraint_classification(right_wing_populism_containment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL MEDIA GATEKEEPERS (ROPE) — Primary beneficiary. Coordination function: define information boundaries, establish narrative control, prevent radicalization cascades. No genuine exit cost — can move between media and tech organizations, consulting, government. Extract through advertising, access, and institutional prestige. Experience the constraint as pure coordination.
constraint_indexing:constraint_classification(right_wing_populism_containment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RIGHT-WING POLITICAL ENTREPRENEURS (TANGLED ROPE) — Mobile but constrained by media delegitimization and deplatforming. Benefit from populist mobilization (political power, funding, media attention) while bearing extraction costs (institutional opposition, coordinated media hostility, legal/regulatory targeting). Mixed experience: genuine political opportunity alongside structural suppression.
constraint_indexing:constraint_classification(right_wing_populism_containment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: TECHNOCRATIC INSTITUTIONAL RATIONALITY (PITON) — The 'expertise-based governance' frame that justifies populism containment as protection against irrational masses. Largely performative: policy decisions are as much political as technical, but the 'science/expertise' framing provides legitimacy for suppression. Maintained through institutional inertia and the absence of genuinely superior alternatives.
constraint_indexing:constraint_classification(right_wing_populism_containment, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Tempting frame: populism is inherently destabilizing and must be contained; expertise-based institutions are necessary barriers against democratic instability. This naturalizes contingent institutional arrangements as inevitable features of complex governance. Engine's false summit detector will identify this as naturalization rather than structural necessity.
constraint_indexing:constraint_classification(right_wing_populism_containment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(right_wing_populism_containment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(right_wing_populism_containment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(right_wing_populism_containment, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(right_wing_populism_containment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(right_wing_populism_containment, TR),
    TR >= 0.70.

:- end_tests(right_wing_populism_containment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts political voice and prevents structural redistribution that would challenge institutional authority, but the extraction is not as severe as pure snare (which would be 0.72+). The institutional coalition genuinely benefits from stability, and some populist actors benefit from platform attention and political mobilization opportunity. The asymmetry is real but not total. Suppression (0.65): High. Multiple mechanisms prevent non-establishment political expression from translating to policy change: media delegitimization, deplatforming, narrative framing as dangerous/irrational, institutional gatekeeping. These barriers are substantial but not absolute — populist movements have achieved electoral success despite suppression. Theater ratio (0.68): Moderate-high. A significant portion of containment operates through narrative framing ('this is what responsible governance looks like') and institutional status rather than direct coercion. Media coverage, expert opinion, and platform policies are the primary enforcement mechanisms. These have performative components — they establish legitimacy for suppression rather than directly preventing mobilization. The trajectory shows increasing theater over time as suppression mechanisms became more sophisticated and narrative-based. Beneficiary/victim structure: Institutional gatekeepers and the center-left coalition benefit by maintaining technocratic authority and preventing redistribution challenges. Working-class victims have their grievances acknowledged but structurally suppressed. Non-establishment political entrepreneurs occupy a mixed position — they benefit from mobilization but face coordinated institutional opposition.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence. Working-class victims classify it as snare (pure extraction with maximum suppression). Institutional beneficiaries classify it as rope (coordination preventing instability). Right-wing entrepreneurs classify it as tangled rope (mixed mobilization opportunity and suppression). The identity-locked oppositional perspective is particularly revealing: structurally these actors could exit the populist-oppositional frame (constrained or even mobile exit options), but their identity fusion prevents them from exercising that mobility. The Boltzmann analysis would reveal whether this is genuine coordination (preventing actual destabilization) or whether the 'stability' being protected is merely institutional authority. The false summit at the analytical level suggests the mountain classification is naturalization rather than structural necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position within the constraint. Working-class victims with trapped exit experience maximum d (~0.95), producing high f(d) and experiencing snare classification. Identity-locked non-establishment actors face different derivation: structurally mobile but cognitively bound, their d is derived from victim status + identity_locked exit, producing high d (~0.89) but with qualitatively different suppression mechanism (cognitive rather than material). Institutional gatekeepers with arbitrage exit have low d (~0.05), experiencing the constraint as coordination. Center-left institutional actors are constrained victims of the constraint's paradox (must appear responsive while maintaining authority), producing d ~0.50-0.55. Right-wing entrepreneurs with mobile exit but victim status face d ~0.70-0.75, the mixed experience of tangled rope. The piton perspective derives not from high d but from theater_ratio (0.68) exceeding the piton threshold (0.70 is the gate, but 0.68 suggests institutional performance is primarily theatrical). The mountain perspective risks naturalizing what is actually a high-extraction constraint — analytical positioning at civilizational scope creates temptation to see institutional containment as inevitable rather than contingent.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PRESENT BUT NOT RESOLVED: This constraint shows the classic mandatrophy pattern: is this coordination (protecting institutional stability, preventing radicalization) or extraction (suppressing grievances, maintaining technocratic authority)? The constraint genuinely coordinates against violent radicalization and institutional collapse. It also genuinely extracts political voice and prevents structural responsiveness to working-class grievances. The resolution requires specifying: (1) Is institutional stability itself a public good or a protection of elite prerogatives? (2) Do non-establishment grievances represent legitimate political demand or dangerous radicalization? (3) Is expertise-based governance genuinely superior at solving collective problems, or is its legitimacy purely authority-based? Until these empirical and normative questions are resolved, the constraint remains tangled rope rather than settling into rope (pure coordination) or snare (pure extraction). The mandatrophy is structurally irreducible without additional data on policy outcomes under different governance regimes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grievance_authenticity_vs_manipulation,
    'Are working-class economic grievances the genuine driver of right-wing populism, or are they being manipulated by political entrepreneurs for power consolidation?',
    'Temporal analysis: did populist rhetoric precede deindustrialization/stagnation, or follow it? Geographic correlation between economic displacement and populist voting. Counterfactual: would identical grievances mobilize without populist leadership?',
    'If grievances genuine: containment mechanism is suppressing legitimate political voice (snare). If manipulation-driven: containment mechanism is preventing dangerous mobilization (rope/scaffold). Classification shifts from snare/tangled_rope to rope/scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grievance_authenticity_vs_manipulation, empirical, 'Whether populism drivers are authentic grievance or elite manipulation').

omega_variable(
    containment_mechanism_efficacy,
    'Does institutional gatekeeping (media delegitimization, deplatforming, delegitimization) actually prevent populist mobilization, or does it amplify grievances through censorship narrative?',
    'Comparative analysis of populism prevalence in high-containment vs low-containment contexts. Correlation between suppression intensity and populist vote share over time. Network analysis of information flows during peak suppression.',
    'If containment effective: extraction value is lower (constraint serves real coordination function). If containment backfires: suppression generates backlash without preventing mobilization (snare with theater component higher than measured).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(containment_mechanism_efficacy, empirical, 'Whether institutional gatekeeping suppresses or amplifies populism').

omega_variable(
    identity_lock_reversibility,
    'For identity-locked non-establishment actors, what would require identity frame shift to enable exit from populist-oppositional identity?',
    'Case study of movement members who''ve abandoned populism: what triggered frame shift? Was it material change, cognitive reframing, social integration? Or is the identity lock structurally irreversible without complete social rupture?',
    'If reversible: constraint has potential sunset as institutional approaches shift. If irreversible: constraint becomes permanent feature of political structure (snare classification stands). Affects mandatrophy reasoning.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, conceptual, 'Whether identity-locked populist opposition can be reversed').

omega_variable(
    technocratic_legitimacy_grounding,
    'Is expertise-based governance genuinely more effective at solving collective problems, or does legitimacy rest on institutional authority rather than demonstrated outcomes?',
    'Comparative policy outcomes: technocratic vs populist-influenced governance across measurable domains (economic growth, inequality, institutional stability). Track whether outcomes improved under containment.',
    'If expertise genuinely superior: piton classification is false, constraint is mountain/rope. If legitimacy is authority-based: piton classification confirmed, technocratic rationality is theatrical maintenance of authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technocratic_legitimacy_grounding, empirical, 'Whether technocratic governance outperforms populist alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(right_wing_populism_containment, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(righ_tr_t0, right_wing_populism_containment, theater_ratio, 0, 0.45).
narrative_ontology:measurement(righ_tr_t5, right_wing_populism_containment, theater_ratio, 5, 0.58).
narrative_ontology:measurement(righ_tr_t10, right_wing_populism_containment, theater_ratio, 10, 0.68).
narrative_ontology:measurement(righ_tr_t15, right_wing_populism_containment, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(righ_be_t0, right_wing_populism_containment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(righ_be_t5, right_wing_populism_containment, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(righ_be_t10, right_wing_populism_containment, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(righ_be_t15, right_wing_populism_containment, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(right_wing_populism_containment, enforcement_mechanism).
narrative_ontology:affects_constraint(right_wing_populism_containment, democratic_representation_deficit).
narrative_ontology:affects_constraint(right_wing_populism_containment, media_narrative_gatekeeping).
narrative_ontology:affects_constraint(right_wing_populism_containment, platform_moderation_asymmetry).

% DUAL FORMULATION NOTE:
% Right-wing populism containment decomposes into three structurally distinct constraints: (1) enforcement_mechanism (institutional suppression via media/platforms, this story), (2) media_narrative_gatekeeping (information boundary control, distinct ε), (3) democratic_representation_deficit (institutional design that prevents non-establishment coalitions from translating votes to policy, distinct ε). This story focuses on the enforcement mechanism connecting institutional gatekeeping to suppression of political voice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(right_wing_populism_containment, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
