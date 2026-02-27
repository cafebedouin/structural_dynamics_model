% ============================================================================
% CONSTRAINT STORY: overton_window
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_overton_window, []).

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
 *   constraint_id: overton_window
 *   human_readable: The Overton Window of Political Discourse
 *   domain: political/social
 *
 * SUMMARY:
 *   The Overton Window of political discourse represents a structural
 *   constraint on which policy positions and intellectual arguments can be
 *   articulated in mainstream forums without delegitimization. At any moment,
 *   a range of positions appears 'acceptable,' 'mainstream,' or 'reasonable'
 *   to the general population, while positions outside this range are framed
 *   as 'extreme,' 'fringe,' or 'unrealistic.' This constraint exhibits
 *   complex hybrid properties: it functions partially as a coordination
 *   mechanism (enabling efficient political discourse and coalition-building)
 *   and partially as an extraction mechanism (enabling elites to suppress
 *   alternatives and maintain structural advantage). The constraint's theater
 *   ratio has increased from 0.42 to 0.68 over the 20-year interval,
 *   indicating growing performativity — the apparatus of public opinion
 *   polling, media consensus-building, and academic legitimacy increasingly
 *   sustains the window's appearance of empirical reality rather than
 *   revealing actual opinion distributions. The extractiveness has risen from
 *   0.38 to 0.58, reflecting that the window's gatekeeping function has
 *   become more asymmetric and more resistant to dissident mobilization, even
 *   as digital platforms have created technical alternatives to traditional
 *   gatekeeping. The constraint operates distinctly at different levels:
 *   local activist coalitions experience constrained exit and mixed
 *   costs/benefits (Tangled Rope); institutional gatekeepers experience it as
 *   beneficial coordination (Rope); political establishments use it as a
 *   coordination mechanism for messaging (Rope); dissident advocates face
 *   systematic exclusion (Snare); and the measurement apparatus sustains its
 *   theatrical reality (Piton).
 *
 * KEY AGENTS:
 *   - Political Elites: Primary beneficiary (institutional/arbitrage) — control window positioning through media access, funding, and institutional legitimacy; can shift window through resource concentration
 *   - Media and Academic Gatekeepers: Primary beneficiary (powerful/mobile) — maintain institutional legitimacy by curating discourse within window bounds; mobile exit allows institutional repositioning
 *   - Status Quo Beneficiaries: Secondary beneficiary (institutional/arbitrage) — extract value from window stability and predictability in policy discourse
 *   - Activist Movements: Secondary actor (organized/constrained) — depend on window positioning for campaign viability; constrained exit because window-shifting requires massive resource mobilization
 *   - Dissident Advocates: Primary victim (powerless/trapped) — positions outside window face systematic media exclusion, academic delegitimization, and funding barriers; trapped because exiting the position compromises their advocacy
 *   - Marginalized Policy Communities: Secondary victim (moderate/constrained) — their policy preferences receive attention only when drawn into the window by larger movements; constrained exit because adopting window-acceptable versions of their positions often means compromise
 *   - Alternative Information Ecosystem: Organized actor (organized/mobile) — creating exit paths through decentralized platforms; mobile because digital platforms can relocate and reposition discourse outside traditional gates
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the window as inevitable feature of discourse rather than recognizing it as contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(overton_window, 0.58).
domain_priors:suppression_score(overton_window, 0.62).
domain_priors:theater_ratio(overton_window, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(overton_window, extractiveness, 0.58).
narrative_ontology:constraint_metric(overton_window, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(overton_window, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(overton_window, tangled_rope).
narrative_ontology:human_readable(overton_window, "The Overton Window of Political Discourse").
narrative_ontology:topic_domain(overton_window, "political/social").

domain_priors:requires_active_enforcement(overton_window).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(overton_window, political_elites).
narrative_ontology:constraint_beneficiary(overton_window, narrative_gatekeepers).
narrative_ontology:constraint_beneficiary(overton_window, status_quo_beneficiaries).
narrative_ontology:constraint_victim(overton_window, marginalized_policy_advocates).
narrative_ontology:constraint_victim(overton_window, dissident_intellectual_movements).
narrative_ontology:constraint_victim(overton_window, discourse_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISSIDENT ADVOCATE (SNARE) — Positions outside the window face systematic exclusion from mainstream media, academic platforms, and policy forums. The trapped advocate cannot exit the window-constrained discourse without abandoning their core position. Maximum extraction through suppression of alternatives and delegitimation.
constraint_indexing:constraint_classification(overton_window, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ACTIVIST MOVEMENT (TANGLED ROPE) — Organized groups benefit from the window's coordination function — it defines what campaigns are feasible and where energy can accumulate. But they also bear extraction costs: only positions within or adjacent to the window receive funding, media coverage, and political responsiveness. Constrained exit: movements can attempt to shift the window but face formidable institutional resistance.
constraint_indexing:constraint_classification(overton_window, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: POLITICAL ESTABLISHMENT (ROPE) — The window functions as a coordination mechanism for the establishment: it enables efficient campaign messaging, focus-grouped policy positioning, and predictable political dynamics. Establishments experience the constraint as beneficial coordination, not extraction. Arbitrage exit: they can shift the window through resources and media control.
constraint_indexing:constraint_classification(overton_window, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDIA AND ACADEMIC GATEKEEPERS (ROPE) — Publishers, editors, and academic journals use the window to coordinate editorial standards and intellectual legitimacy. The constraint enables efficient curation and reputation management. Mobile exit: gatekeepers can reposition institutions and narratives to reshape the window itself.
constraint_indexing:constraint_classification(overton_window, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE INFORMATION ECOSYSTEM (SCAFFOLD) — Digital platforms, independent media, and decentralized publishing are creating pathways outside traditional window constraints. These alternatives have a built-in sunset: as they mature and gain institutional legitimacy, they reduce the traditional window's exclusive gatekeeping power. Low effective extraction because this perspective sees structural exit mechanisms developing.
constraint_indexing:constraint_classification(overton_window, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: PUBLIC OPINION MEASUREMENT SYSTEM (PITON) — Polling, surveys, and public opinion research purport to measure the window empirically. In reality, polling methodology itself shapes what opinions are legible as 'mainstream.' The measurement ritual is substantially performative — it produces the window it claims to discover. Piton classification: theater_ratio high (0.68) because the public opinion apparatus sustains the window's appearance of empirical reality rather than revealing actual opinion distributions.
constraint_indexing:constraint_classification(overton_window, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some range of acceptable discourse is inherent to any society: not all positions can be simultaneously mainstream, and boundary-drawing is inevitable. This perspective risks naturalizing what is actually a contingent institutional arrangement — the specific positions inside/outside the window are structurally determined by power distributions, not laws of social physics. The engine's false summit detector will identify this as naturalization of institutional contingency.
constraint_indexing:constraint_classification(overton_window, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(overton_window_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(overton_window, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(overton_window, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(overton_window, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(overton_window, TR),
    TR >= 0.70.

:- end_tests(overton_window_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The window enables genuine coordination — political movements need clarity on which positions are electorally viable and where public attention can accumulate. However, the extraction component is substantial: elites use window boundaries to suppress alternatives, protect existing power structures, and prevent policy innovation outside their preferred range. The measurement over time (0.38→0.58) shows the extraction mechanism has strengthened even as digital alternatives created technical pathways outside the window, indicating institutional deepening rather than natural constraint. Suppression (0.62): High. Significant barriers to dissident positions include media gatekeeping (editorial boards controlling which voices reach audiences), academic gatekeeping (peer review systems filtering out heterodox positions), funding barriers (foundations preferring proposals aligned with window positions), and social delegitimization (framing outsiders as unreasonable). These barriers are enforced and maintained, not accidental. Theater ratio (0.68): High and rising. Public opinion polling, consensus journalism, and 'serious policy analysis' create the appearance that window boundaries reflect empirical public sentiment, when measurement methodology itself shapes what sentiments are legible as 'mainstream.' The rise from 0.42 to 0.68 reflects increased reliance on performative legitimation as traditional gatekeeping faces challenge from digital alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The Overton Window demonstrates the full range of DR types from the same structural constraint. Political elites genuinely experience it as coordination (Rope) — the constraint solves their problem of coalition-building and message consistency. Activists experience mixed coordination and extraction (Tangled Rope) — the window enables campaign focus but prevents strategic innovation outside its bounds. Dissidents experience it as pure extraction (Snare) — suppression with no coordination benefit. The measurement apparatus experiences it as performative (Piton) — polling and consensus journalism sustain its reality rather than revealing it. Digital alternatives experience it as temporary (Scaffold) — decentralized platforms create real exit paths that are reducing the window's exclusive gatekeeping power. The analytical observer may risk seeing it as inevitable (Mountain) — 'any society needs discourse boundaries' — but the false summit detector reveals this naturalizes what is actually an institutional arrangement contingent on specific power distributions. The mandatrophy is resolved by recognizing that all readings are perspectivally accurate: the window is genuinely useful coordination infrastructure for some actors and genuinely extractive suppression for others, and this hybrid property is structural to the constraint itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position relative to extraction flow. Beneficiaries (political elites, gatekeepers) with arbitrage options experience the window as low-cost coordination, producing negative or very low d values. They can reposition the window through resource concentration and media control. Institutional actors with constrained exit (activist movements) experience mixed costs and benefits — they depend on the window for visibility but are constrained by it strategically, producing moderate d values (0.50-0.65). Powerless actors with trapped exit (dissident advocates) experience maximum extraction — they cannot exit their position without compromising advocacy, and the window excludes them systematically, producing high d values (0.85-0.95). The alternative information ecosystem has mobile exit options (they can build platforms outside traditional gates), producing lower d values despite being organizationally positioned against the window, because their exit option is material rather than constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC HYBRID: The Overton Window resolves mandatrophy by showing that high extractiveness (0.58) is compatible with genuine coordination function (beneficiaries truly use it as coordination). The constraint is NOT mislabeled coordination-as-extraction or extraction-as-coordination. Instead, it is a Tangled Rope: it provides real coordination benefits for some actors (elites, gatekeepers, organized movements) while enabling asymmetric extraction from others (dissidents, marginalized advocates, discourse accessibility itself). The rising theater ratio (0.42→0.68) shows that the performative component is increasing over time, suggesting the constraint is acquiring piton characteristics even as it maintains tangled rope structure. The measured theater increase reflects that institutional gatekeepers are relying more on legitimation narratives (polls, consensus journalism, 'reasonable debate' framing) rather than direct suppression — a sign that the constraint's power is partly contested and requires ongoing theatrical maintenance. This is consistent with tangled rope dynamics under challenge from alternative platforms: the constraint persists through institutional depth and narrative control rather than through uncontested structural dominance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    window_empirical_reality,
    'Is the Overton Window an empirical regularity in how mainstream discourse self-organizes, or a self-fulfilling prophecy produced by gatekeeping institutions?',
    'Comparative analysis of discourse distributions pre/post-gatekeeping changes; measurement of opinion variance across gated vs ungated channels; historical case studies of window shifts correlating with institutional power changes vs exogenous shocks',
    'If empirical: window is structural fact constraining coordination (Rope from beneficiary perspective). If self-fulfilling: window is institutional control mechanism (Snare/Tangled Rope from victim perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(window_empirical_reality, empirical, 'Whether the window reflects actual opinion bounds or institutional gatekeeping').

omega_variable(
    dissident_exit_mechanisms,
    'Can dissident positions exit the window through sufficient organizing, resources, and alternative platforms, or are the barriers to window-shifting permanently asymmetric?',
    'Historical analysis of successful window shifts (civil rights, women''s suffrage, climate action adoption); correlation between resources, platform access, and window movement; measurement of barrier costs for insider vs outsider positions',
    'If exits are possible: constraint may be Scaffold (temporary) or Tangled Rope (mixed). If barriers are permanent: constraint is Snare for dissidents and Rope/Piton for gatekeepers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dissident_exit_mechanisms, empirical, 'Whether dissidents can structurally shift the window').

omega_variable(
    digital_decentralization_sunset,
    'Will decentralized information ecosystems (social media, independent publishing, AI-generated content) durably reduce the window''s gatekeeping power, or will new centralized nodes recreate the constraint at a different scale?',
    'Longitudinal tracking of narrative diversity pre/post-decentralization; measurement of barrier costs for outsider positions across 20-year horizon; identification of new gatekeeping concentrations emerging in decentralized systems',
    'If decentralization succeeds: scaffold sunset is real, window loses extractive force. If new centralization emerges: piton dynamics dominate — window persists as theatrical compliance with appearance of diversity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(digital_decentralization_sunset, empirical, 'Whether digital decentralization provides durable alternative to window gatekeeping').

omega_variable(
    coordination_necessity_threshold,
    'Below what threshold of discourse diversity does political coordination become impossible? Is the Overton Window a functional necessity or an extractive chokepoint?',
    'Comparative analysis of political systems with narrow windows vs wide windows; measurement of policy responsiveness and electoral volatility at different window widths; game-theoretic modeling of coalition formation under variable discourse constraints',
    'If coordination requires narrow window: constraint is Rope (legitimate). If wide windows support equal coordination: constraint is extraction mechanism (Snare/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_threshold, conceptual, 'Whether window narrowness is functionally necessary for coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(overton_window, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ow_tr_t0, overton_window, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ow_tr_t10, overton_window, theater_ratio, 10, 0.6).
narrative_ontology:measurement(ow_tr_t20, overton_window, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(ow_be_t0, overton_window, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ow_be_t10, overton_window, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(ow_be_t20, overton_window, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(overton_window, information_standard).
narrative_ontology:affects_constraint(overton_window, media_gatekeeping).
narrative_ontology:affects_constraint(overton_window, academic_consensus_formation).
narrative_ontology:affects_constraint(overton_window, electoral_viability_threshold).

% DUAL FORMULATION NOTE:
% The Overton Window is upstream of more specific constraints on political discourse (media gatekeeping, academic consensus, electoral viability). Each of those constraints has its own extractiveness value reflecting specific institutional mechanisms; the window represents the coordination/extraction hybrid at the highest level of discourse organization. Window shifts affect downstream constraints by changing which positions are viable for specific institutional actors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(overton_window, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
