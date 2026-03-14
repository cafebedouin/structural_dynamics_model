% ============================================================================
% CONSTRAINT STORY: hong_kong_press_freedom_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hong_kong_press_freedom_suppression, []).

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
 *   constraint_id: hong_kong_press_freedom_suppression
 *   human_readable: Hong Kong Press Freedom Suppression
 *   domain: political/media_freedom
 *
 * SUMMARY:
 *   Hong Kong's press freedom suppression represents a shift from
 *   institutional constraint (prior regulatory capture within
 *   democratic-adjacent framework) to structural snare following 2020
 *   National Security Law implementation. The constraint extracts control of
 *   narrative asymmetrically from journalists and pro-democracy media outlets
 *   while providing no offsetting coordination benefit to the victims.
 *   Extractiveness has risen from 0.38 (2019, during mass protest period with
 *   greater pluralism) to 0.68 (2026, post-NSL enforcement acceleration) as
 *   legal jeopardy, economic pressure, and self-censorship have compounded.
 *   Theater ratio has increased from 0.35 to 0.55, indicating that formal
 *   press freedom institutions (press councils, regulatory review mechanisms,
 *   appeals processes) have become increasingly performative as enforcement
 *   outcomes are predetermined by political priorities. The constraint
 *   operates at regional scope (affects Hong Kong residents and
 *   organizations) but is directed from civilizational/global scope (Beijing
 *   central authority exporting institutional control models). This creates a
 *   perspectival gap: the suppression appears as necessary governance from
 *   the institutional beneficiary perspective and pure extraction from the
 *   victim perspectives.
 *
 * KEY AGENTS:
 *   - Independent Journalists: Primary victims (powerless/trapped) — face prosecution risk, economic collapse, forced emigration as only exit option
 *   - Pro-Democracy Media Outlets: Primary victims (powerless/trapped) — face closure through legal/regulatory pretexts, advertising pressure, facility access denial
 *   - Hong Kong Government: Primary beneficiary (institutional/arbitrage) — implements suppression; has discretion to relax enforcement within bounds set by central authority
 *   - Beijing Central Authority: Secondary beneficiary (institutional/arbitrage) — sets enforcement policy and strategic direction; benefits from unified messaging control across Hong Kong
 *   - International News Organizations: Constrained actors (institutional/constrained) — face operational barriers but retain some reporting capacity and external audience access
 *   - Press Freedom Institutional Framework: Degraded structure (institutional/arbitrage) — formal protections (Basic Law, press councils) persist but are hollowed by enforcement capture; maintains legitimacy theater
 *   - Public Information Access: Structural victim (powerless/trapped) — Hong Kong residents face information asymmetry and curated narratives; no organizing capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hong_kong_press_freedom_suppression, 0.68).
domain_priors:suppression_score(hong_kong_press_freedom_suppression, 0.78).
domain_priors:theater_ratio(hong_kong_press_freedom_suppression, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hong_kong_press_freedom_suppression, extractiveness, 0.68).
narrative_ontology:constraint_metric(hong_kong_press_freedom_suppression, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(hong_kong_press_freedom_suppression, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hong_kong_press_freedom_suppression, snare).
narrative_ontology:human_readable(hong_kong_press_freedom_suppression, "Hong Kong Press Freedom Suppression").
narrative_ontology:topic_domain(hong_kong_press_freedom_suppression, "political/media_freedom").

domain_priors:requires_active_enforcement(hong_kong_press_freedom_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hong_kong_press_freedom_suppression, hong_kong_government).
narrative_ontology:constraint_beneficiary(hong_kong_press_freedom_suppression, beijing_central_authority).
narrative_ontology:constraint_victim(hong_kong_press_freedom_suppression, independent_journalists).
narrative_ontology:constraint_victim(hong_kong_press_freedom_suppression, pro_democracy_media_outlets).
narrative_ontology:constraint_victim(hong_kong_press_freedom_suppression, public_information_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT JOURNALIST (SNARE) — Faces maximum extraction through legal jeopardy, self-censorship pressure, and declining economic viability. Exit options are severely constrained: fleeing Hong Kong means abandoning livelihood, sources, and identity; remaining means accepting criminalization risk under national security laws. Experiences the constraint as pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(hong_kong_press_freedom_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PRO-DEMOCRACY MEDIA OUTLET (SNARE) — Operates under existential threat: advertising revenue dries up due to political pressure, printing facilities face closure through regulatory pretexts, senior editors face prosecution. Self-censorship becomes operational necessity. No exit path within Hong Kong; closure or relocation overseas is the only alternative. Extraction is maximal and asymmetric.
constraint_indexing:constraint_classification(hong_kong_press_freedom_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: HONG KONG GOVERNMENT (ROPE) — Experiences the suppression apparatus as a coordination mechanism: the constraint solves a governance problem from their structural position. Unified control of information flow reduces criticism and resistance. Benefits from the constraint through unchallenged policy implementation and stable political control. Has arbitrage options (can relax enforcement if central authority permits). Perceives the constraint as enabling effective governance.
constraint_indexing:constraint_classification(hong_kong_press_freedom_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: INTERNATIONAL NEWS ORGANIZATIONS (TANGLED ROPE) — Face significant constraints (visa denials, reporting restrictions, legal threats to local staff) but also benefit from residual access and the ability to transmit information to global audiences. Constrained by cost of maintaining bureau operations in hostile environment but not trapped — can relocate or reduce presence. Experience both extraction (operational costs, access denial) and coordination benefits (information-gathering infrastructure).
constraint_indexing:constraint_classification(hong_kong_press_freedom_suppression, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: PRESS FREEDOM REGULATORY FRAMEWORK (PITON) — The formal institutional framework for press freedom (Basic Law Article 27, common law traditions) persists as a vestigial structure: nominally protective but functionally disabled by national security legislation and enforcement capture. Theater ratio high — regulatory review boards, press councils, and appeals mechanisms operate but produce predetermined outcomes. The framework is maintained to signal legitimacy internationally while being systematically hollowed by enforcement.
constraint_indexing:constraint_classification(hong_kong_press_freedom_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a global/civilizational view, the constraint extracts information asymmetrically from Hong Kong society while suppressing alternative narratives. Functions as a control mechanism without offsetting coordination benefits for the suppressed population. Unlike some snares, offers no redemptive framing (no safety or efficiency gained by the victims). Pure extraction with maximum visibility to external analysis.
constraint_indexing:constraint_classification(hong_kong_press_freedom_suppression, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hong_kong_press_freedom_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hong_kong_press_freedom_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hong_kong_press_freedom_suppression, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hong_kong_press_freedom_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hong_kong_press_freedom_suppression, TR),
    TR >= 0.70.

:- end_tests(hong_kong_press_freedom_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The suppression mechanism extracts informational control and narrative authority from independent journalists and media outlets. The measurement trajectory shows steady acceleration from 0.38 (2019) to 0.68 (2026) as legal tools (NSL articles, sedition prosecutions, visa denials) have been applied. The extraction is not maximal (0.95+) because some residual journalism continues through self-censorship and international outlets, but the trend is toward tightening. Suppression (0.78): Very high. Multiple barriers prevent exit and alternatives: legal jeopardy creates immediate costs; visa denials and facility access restrictions create structural barriers; economic collapse of ad-supported outlets creates financial traps; emigration requires abandoning sources, professional networks, and identity. The suppression is sustained by active enforcement (NSL prosecutions, licensing denials) and structural barriers (equipment confiscation, advertising pressure from government-aligned firms). Theater ratio (0.55): Moderate-high. Formal press freedom institutions exist (Press Freedom Index submissions, regulatory review boards, appeals mechanisms) but operate with predetermined outcomes. Theater has increased as the constraint has shifted from regulatory capture (where some real negotiation occurred) to overt suppression (where procedures are performative).
 *
 * PERSPECTIVAL GAP:
 *   Hong Kong Government (institutional/arbitrage) sees Rope: the suppression mechanism enables governance through information control. They experience benefits without costs — the constraint 'coordinates' messaging across government agencies and reduces harmful opposition. Beijing Central Authority (institutional/arbitrage) sees similar Rope: press control enables civilizational-scale political stability messaging. Independent Journalists (powerless/trapped) see Snare: pure extraction with legal jeopardy, economic collapse, and forced emigration. Pro-Democracy Media (powerless/trapped) see Snare: existential threat without offsetting benefit. International News Organizations (institutional/constrained) see Tangled Rope: constrained operations but retain some reporting capacity and external audience reach. The piton perspective reveals institutional decay — formal press freedom framework persists but is hollowed out. The analytical perspective confirms Snare: from a global/civilizational scope, the constraint extracts information asymmetrically without providing goods/services justifying the extraction cost to victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation traces from beneficiary/victim structure and exit options. Independent journalists are victims with trapped exit (no viable alternatives within Hong Kong or viable emigration options maintaining livelihood) — high d value (~0.92) → f(d) ≈ 1.38 → high experienced chi. Hong Kong Government benefits from arbitrage exit (can adjust enforcement within Beijing bounds) — low d value (~0.18) → f(d) ≈ 0.08 → negative chi (benefit). Public information access is a victim with trapped exit — high d → high chi. International organizations are constrained rather than trapped — moderate d (~0.62) → moderate f(d) ≈ 0.95 → moderate chi. The directionality structure confirms Snare classification: asymmetric extraction from powerless/trapped victims toward institutional beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that this constraint is a Snare from the victim and analytical perspectives, while appearing as Rope from the beneficiary perspective (Hong Kong Government, Beijing Central Authority). The constraint exhibits the classic snare signature: base extractiveness ≥ 0.46, suppression ≥ 0.60, χ ≥ 0.66, and multiple perspectives classifying as Snare. The false summit risk (naturalizing suppression as inherent to governance) is addressed by the analytical perspective, which identifies the constraint as a contingent institutional choice rather than a law of nature or inevitable coordination problem. The piton perspective provides diagnostic signal that formal press freedom institutions have become performative theater — they no longer function as constraints on power but as legitimacy-maintaining rituals. Resolution: Snare classification is correct. The constraint exists to extract political control, not to provide coordination goods. Beneficiaries perceive coordination (Rope) due to information control benefits, but this is perspectival asymmetry, not evidence that the constraint serves coordination. The constraint persists because it serves the interests of those with power to enforce it, not because it solves a genuine collective action problem for the suppressed population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_censorship_threshold,
    'At what point does legal threat severity cause self-censorship to become the dominant suppression mechanism rather than legal enforcement?',
    'Comparative analysis of actual prosecutions vs. withdrawal of coverage and outlets; measurement of chilling effect magnitude vs. enforcement magnitude',
    'If legal threat is primary driver: suppression value (0.78) is accurate. If self-censorship is primary: the measured suppression underestimates the internalized constraint — victims police themselves even without enforcement. Classification may shift from Snare (external enforcement) to include identity_locked mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_censorship_threshold, empirical, 'Relative magnitude of legal threat vs. self-censorship in suppression mechanism').

omega_variable(
    hong_kong_exceptionalism,
    'Is Hong Kong press suppression a structurally distinct constraint from mainland Chinese media control, or a continuation of the same system with different enforcement tempo?',
    'Comparative institutional analysis: degree of institutional autonomy remaining in Hong Kong judiciary, regulatory bodies, and media ownership; timeline of convergence with mainland practices',
    'If structurally distinct: separate story for Hong Kong constraint with regional scope. If continuation: should be modeled as part of larger China_media_control constraint family. Affects network decomposition and comparative analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hong_kong_exceptionalism, conceptual, 'Whether Hong Kong suppression is autonomous constraint or regional manifestation of broader system').

omega_variable(
    international_audience_circulation,
    'Does the ability of suppressed information to circulate internationally through diaspora networks and VPNs constitute a functional exit option for journalists, or does the primary audience loss (local readers) render this option insignificant?',
    'Measurement of audience reach and economic viability for outlets with international-only circulation; tracking of career alternatives for journalists targeting international audiences',
    'If meaningful exit option: some journalists have true mobility rather than trapped status — classification shifts from universal powerlessness to mixed powerless/moderate perspectives. If insignificant: trapped status confirmed, universal Snare from victim perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_audience_circulation, empirical, 'Whether international information circulation provides viable exit alternative').

omega_variable(
    central_authority_coordination_vs_extraction,
    'From Beijing''s structural perspective, does press suppression serve a coordination function (unified messaging for governance) or is it pure extraction (controlling narrative for political control without governance benefit)?',
    'Analysis of Beijing''s communications strategy; measurement of whether suppressed outlets'' closure reduces policy implementation difficulty or merely reduces criticism',
    'If coordination function: Beijing might perceive as Rope; Snare classification is victim-perspective rather than universal. If extraction: Beijing''s position is extractive even from its own governance standpoint — contradiction revealing institutional dysfunction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(central_authority_coordination_vs_extraction, conceptual, 'Whether press suppression serves coordination or pure extraction from Beijing authority perspective').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hong_kong_press_freedom_suppression, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hkpfs_tr_t0, hong_kong_press_freedom_suppression, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hkpfs_tr_t3, hong_kong_press_freedom_suppression, theater_ratio, 3, 0.42).
narrative_ontology:measurement(hkpfs_tr_t6, hong_kong_press_freedom_suppression, theater_ratio, 6, 0.49).
narrative_ontology:measurement(hkpfs_tr_t9, hong_kong_press_freedom_suppression, theater_ratio, 9, 0.55).

% Extraction over time
narrative_ontology:measurement(hkpfs_be_t0, hong_kong_press_freedom_suppression, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hkpfs_be_t3, hong_kong_press_freedom_suppression, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(hkpfs_be_t6, hong_kong_press_freedom_suppression, base_extractiveness, 6, 0.64).
narrative_ontology:measurement(hkpfs_be_t9, hong_kong_press_freedom_suppression, base_extractiveness, 9, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hong_kong_press_freedom_suppression, enforcement_mechanism).
narrative_ontology:affects_constraint(hong_kong_press_freedom_suppression, china_media_control_system).
narrative_ontology:affects_constraint(hong_kong_press_freedom_suppression, hong_kong_political_autonomy_erosion).

% DUAL FORMULATION NOTE:
% Hong Kong press suppression is downstream of China's broader media control system but exhibits distinct structural features: the suppression has accelerated post-2020 NSL following a prior period of greater pluralism. Separately modeled to capture the trajectory from institutional capture (2000-2019) to structural snare (2020-2026). The upstream China constraint operates at civilizational/global scope; this constraint operates at regional scope with global institutional observation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hong_kong_press_freedom_suppression, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
