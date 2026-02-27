% ============================================================================
% CONSTRAINT STORY: overton_window
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   The Overton Window defines the set of policies and ideas considered
 *   acceptable to the mainstream population at a given time. This constraint
 *   operates at the intersection of discourse norms, institutional power, and
 *   information architecture. It functions simultaneously as a coordination
 *   mechanism (parties, media, and audiences benefit from shared discourse
 *   boundaries), an extraction mechanism (those deemed 'outside' the window
 *   face dismissal and career consequences), and an increasingly contested
 *   temporal phenomenon (digital platforms are fragmenting unified windows
 *   into multiple competing spaces). The constraint exhibits substantial
 *   theater_ratio (0.68) because much of the enforcement happens through
 *   performative legitimacy-signaling (academic rigor standards, journalistic
 *   balance, political 'seriousness') rather than explicit censorship.
 *   However, explicit suppression is real: marginalized policy proposers face
 *   concrete barriers (media exclusion, employment risk, social sanction).
 *   The window has shifted measurably over the 20-year interval
 *   (extractiveness rose from 0.45 to 0.58, theater from 0.52 to 0.68),
 *   indicating that the institutional enforcement has both intensified and
 *   become more performative, possibly in response to digital disruption.
 *
 * KEY AGENTS:
 *   - Institutional Political Actors: Primary beneficiary (institutional/arbitrage) — political parties use the window to structure electoral competition within predictable bounds
 *   - Media Gatekeepers: Secondary beneficiary (organized/constrained) — news organizations coordinate around the window; face pressure from ownership, advertisers, and platform algorithms that pull toward polarization
 *   - Status Quo Coalitions: Tertiary beneficiary (institutional/arbitrage) — incumbent economic, military, and social interests benefit from window stability that marginalizes transformative proposals
 *   - Marginalized Policy Proposers: Primary victim (powerless/trapped) — excluded from mainstream discourse by window boundaries; face career and social consequences for violation
 *   - Grass-Roots Movements: Secondary victim (moderate/constrained) — organized constituencies constrained by what can be publicly advocated without becoming unelectable or socially isolated
 *   - Digital Native Communities: Organized disruptors (organized/mobile) — social media, forums, and decentralized networks create alternative discourse spaces; represent the scaffold perspective with a sunset
 *   - Academic/Think Tank Establishment: Piton actor (institutional/arbitrage) — maintains legitimacy gatekeeping through performative rigor standards that reproduce the window
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(overton_window, 0.58).
domain_priors:suppression_score(overton_window, 0.65).
domain_priors:theater_ratio(overton_window, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(overton_window, extractiveness, 0.58).
narrative_ontology:constraint_metric(overton_window, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(overton_window, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(overton_window, tangled_rope).
narrative_ontology:human_readable(overton_window, "The Overton Window of Political Discourse").
narrative_ontology:topic_domain(overton_window, "political/social").

domain_priors:requires_active_enforcement(overton_window).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(overton_window, institutional_political_actors).
narrative_ontology:constraint_beneficiary(overton_window, media_gatekeepers).
narrative_ontology:constraint_beneficiary(overton_window, status_quo_coalitions).
narrative_ontology:constraint_victim(overton_window, marginalized_policy_proposers).
narrative_ontology:constraint_victim(overton_window, structural_discourse_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RADICAL POLICY PROPOSER (SNARE) — Proposals deemed outside the window face systematic dismissal, ridicule, and career consequences. No legitimate pathway to main-stage discourse. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80. Pure extraction: forced silence or social cost for speech.
constraint_indexing:constraint_classification(overton_window, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GRASS-ROOTS MOVEMENT (TANGLED ROPE) — Organized constituencies benefit from the window's stability (predictability aids coalition-building) but are constrained by what can be publicly advocated. d≈0.70, f(d)≈1.08, σ=0.9 → χ≈0.57. Mixed: coordination through shared discourse norms, but extraction via suppression of their central claims.
constraint_indexing:constraint_classification(overton_window, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MAINSTREAM POLITICAL PARTY (ROPE) — Benefits from the window's existence through predictable electoral strategy and coalition stability. The window is a coordination mechanism that enables party strategy without explicit coercion. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary; sees the window as natural coordination.
constraint_indexing:constraint_classification(overton_window, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDIA INSTITUTION (TANGLED ROPE) — Media outlets coordinate around the window (reduces controversy, maintains advertising relationships) but also enforce it through gatekeeping. Faces organizational constraints from ownership, advertiser pressure, and audience polarization. d≈0.35, f(d)≈0.38, σ=1.0 → χ≈0.22. Low-moderate extraction because media has agency in choosing which ideas to amplify.
constraint_indexing:constraint_classification(overton_window, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DIGITAL NATIVE COMMUNITIES (SCAFFOLD) — Social media, decentralized forums, and digital networks create parallel discourse spaces with different windows. These communities experience the traditional window as a temporary constraint with a sunset: as digital natives mature, direct-to-audience communication bypasses traditional gatekeeping. d≈0.45, f(d)≈0.45, σ=1.2 → χ≈0.25. Moderate suppression (platform moderation, shadowbanning) but mobile exit (alternative platforms) makes it temporary.
constraint_indexing:constraint_classification(overton_window, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL DISCOURSE ESTABLISHMENT (PITON) — Universities, think tanks, and academic publishing maintain the window through citation networks, hiring decisions, and 'respectability' gatekeeping. The process is largely performative: they claim to be apolitical arbiters of what is 'scholarly' or 'rigorous,' but the categories reproduce the window. theater_ratio=0.68 indicates substantial performative content — the ritual of peer review and scholarly decorum maintains the window more than evidence-based evaluation. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(overton_window, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN CLAIM) — From a civilizational perspective, one might argue that discourse boundaries are inherent to human cognition and social coordination — some ideas will always seem radical because society has limited attention bandwidth and cultural assimilation capacity. ε=0.58 and suppression=0.65 violate mountain thresholds. This perspective is a false summit: the structural data shows the window is contingent (varies by time, geography, institutional context), not inherent to human nature.
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
    constraint_indexing:constraint_classification(overton_window, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.58): Moderate-high. The window creates asymmetric gains for those within it (elevated platform access, coalition credibility) and real costs for those outside (exclusion, ridicule, employment risk). However, the extraction is not as extreme as pure rent-seeking (0.75+) because the window does perform a genuine coordination function — political parties legitimately benefit from being able to credibly commit to 'moderate' positions. The rise from 0.45 to 0.58 over the interval reflects intensification of institutional enforcement, probably in response to digital disruption. Suppression (0.65): High. Multiple suppression mechanisms: media gatekeeping (exclusion from platforms), career consequences (academic/media employment barriers), social sanction (mockery, deplatforming), and algorithmic amplification that favors ideas already within the window. However, suppression is not total (0.95) because individuals can and do violate the window through blogs, podcasts, underground media, and digital organizing. Theater ratio (0.68): High and rising. The window is increasingly maintained through performance of legitimacy rather than explicit force. Gatekeepers claim to apply 'objectivity,' 'rigor,' 'balance,' or 'seriousness' standards that happen to reproduce the window — the standards feel neutral but are window-reinforcing. Academic peer review, journalistic editorial decisions, and policy expert gatekeeping all operate through performative legitimacy signals. The rise from 0.52 to 0.68 suggests that as explicit suppression becomes less viable (digital platforms erode monopoly control), institutional actors rely more on performative legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The Overton Window demonstrates the full perspectival gap between institutional beneficiaries and marginalized victims. The mainstream political party sees the window as natural coordination (Rope) — a legitimate framework for electoral strategy. The media institution sees it as mixed (Tangled Rope) — it enables coordination but also constrains editorial choices when institutional pressures conflict with truth-seeking. The grass-roots movement sees significant extraction (Tangled Rope) — they benefit from coordination norms but are constrained by what their constituents can advocate. The radical proposer sees pure extraction (Snare) — exclusion from discourse with no pathway inward. The digital native communities see a temporary problem (Scaffold) — social media and decentralized platforms bypass the window, and this creates a sunset where traditional gatekeeping loses power. The institutional establishment sees its own process as inevitable and apolitical (Piton) — peer review and editorial standards feel neutral, but their performative content has risen as explicit enforcement becomes less viable. The civilizational observer risks seeing the window as a natural law of human cognition (false Mountain) — but the structural data shows it is a contingent institutional arrangement that varies dramatically across time, geography, and information architecture.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional political actors: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiaries with low extraction experience. Marginalized proposers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum directionality — no exit option. Grass-roots movements: Victim + constrained → d≈0.70, f(d)≈1.08. Significant extraction but with some organizational capacity. Media institutions: Beneficiary + constrained (not arbitrage, because ownership and advertiser pressure create real constraints) → d≈0.35, f(d)≈0.38. Low-moderate extraction; media has less pure arbitrage power than traditional political parties. Digital natives: Organized + mobile → d≈0.45, f(d)≈0.45. Mobile exit (alternative platforms) keeps effective extraction moderate despite platform moderation. Academic establishment: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary; rigor standards maintain the window while feeling apolitical.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The Overton Window constraint resolves the mandatrophy by explicitly identifying beneficiaries (institutional political actors, media, status quo coalitions) and victims (marginalized proposers, grass-roots movements). The constraint is Tangled Rope because it exhibits BOTH a genuine coordination function (parties benefit from predictable electoral boundaries; audiences benefit from stable meaning-making frameworks) AND asymmetric extraction (those outside the window bear disproportionate costs). The extraction is not pure rent-seeking because the coordination benefit is real — the window does reduce transaction costs for political communication. However, the window also serves to exclude transformative alternatives and protect incumbent interests, confirming the asymmetric extraction component. The theater metric (0.68, rising) confirms that much of the enforcement is performative rather than explicit coercion, which is characteristic of Tangled Rope when institutional power is high and explicit suppression is constrained. The scaffold perspective (digital natives) indicates that the constraint's extractive function may be time-limited, which is consistent with the observation that the window weakens in information environments where gatekeeping is technically infeasible. The false summit perspective (natural law claim) is exposed as such because the window's ε and suppression values vary dramatically across contexts — the constraint is institutional, not inherent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    window_organic_vs_manufactured,
    'Is the Overton Window an organic emergent property of human discourse limitations or a manufactured constraint imposed by institutional actors?',
    'Historical comparison of discourse windows across decentralized vs centralized information environments; analysis of pre-mass-media discourse boundaries vs post-digital shift; examination of which proposals move the window and why',
    'If organic: constraint is closer to Mountain (natural law of social cognition). If manufactured: constraint is Snare or Tangled Rope with different beneficiary structure (institutional gatekeepers vs distributed elites).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(window_organic_vs_manufactured, conceptual, 'Whether the window is emergent or institutionally manufactured').

omega_variable(
    digital_shift_permanence,
    'Will digital platforms and decentralized communication permanently fracture the unified Overton Window or create multiple windows that eventually coalesce back into a single mainstream?',
    'Long-term trajectory of discourse polarization; comparative analysis of idea diffusion in fragmented vs unified media environments; observation of whether marginalized ideas gain mainstream traction faster or slower post-social-media',
    'If fragmentation persists: window constraint weakens to Scaffold type with permanent sunset. If coalesces: new window emerges but with different institutional structure. If multiple stable windows: we need separate stories for each window with different ε values.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(digital_shift_permanence, empirical, 'Whether digital fragmentation is permanent or temporary').

omega_variable(
    suppression_mechanism_visibility,
    'How much of the window''s suppression is explicit gatekeeping vs implicit self-censorship driven by fear of social/career consequences?',
    'Survey data on actual suppression (rejection, firing, deplatforming) vs perceived suppression (agents avoiding speech due to fear); analysis of cases where individuals violated window and actual vs imagined consequences',
    'If mostly implicit: suppression metric should be lower (0.45-0.55 range), classification shifts toward Rope. If mostly explicit: suppression confirmed at 0.65+, confirms Tangled Rope/Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_visibility, empirical, 'Ratio of explicit gatekeeping to implicit self-censorship').

omega_variable(
    extractive_beneficiary_identification,
    'Who actually benefits from the window''s stability? Is it institutional political parties, media corporations, status quo economic interests, or a coalition of all three with different temporal horizons?',
    'Analysis of which actors move the window and in which directions; historical tracking of window shifts correlated with electoral outcomes, media ownership changes, economic policy shifts; comparison of which proposals gain support after institutional backing',
    'If primarily electoral benefit: window serves coordination function (Rope). If primarily economic extraction: window serves wealth concentration (Snare). If mixed: confirms Tangled Rope analysis with composite beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_beneficiary_identification, empirical, 'Identity and motivation of primary beneficiaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(overton_window, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ow_tr_t0, overton_window, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ow_tr_t10, overton_window, theater_ratio, 10, 0.6).
narrative_ontology:measurement(ow_tr_t20, overton_window, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(ow_be_t0, overton_window, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ow_be_t10, overton_window, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(ow_be_t20, overton_window, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(overton_window, information_standard).
narrative_ontology:affects_constraint(overton_window, discourse_polarization).
narrative_ontology:affects_constraint(overton_window, institutional_legitimacy_gatekeeping).
narrative_ontology:affects_constraint(overton_window, policy_innovation_suppression).

% DUAL FORMULATION NOTE:
% The Overton Window is upstream of multiple institutional constraints (polarization, legitimacy gatekeeping, policy innovation suppression). Each affected constraint has its own ε value reflecting whether it is predominantly extractive or coordinative. The window itself at ε=0.58 (Tangled Rope) coordinates mainstream discourse but extracts costs from innovators and marginalized movements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(overton_window, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
