% ============================================================================
% CONSTRAINT STORY: pastoral_commons_enclosure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pastoral_commons_enclosure, []).

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
 *   constraint_id: pastoral_commons_enclosure
 *   human_readable: Pastoral Commons Enclosure and Land Access Restriction
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   Pastoral commons enclosure represents the large-scale transfer of grazing
 *   lands from collective use to private or state control, driven by
 *   agricultural intensification, colonial administrative systems, and
 *   capitalist land markets. Across global contexts — from the Scottish
 *   Highlands to East African pastoral zones to Inner Asian steppes —
 *   enclosure produces a consistent structural pattern: formal property
 *   rights concentration, suppression of traditional access, and extraction
 *   from subsistence communities toward commercial and state interests. The
 *   constraint exhibits all six classification types from different
 *   perspectives, making it a diagnostic exemplar of how the same
 *   institutional practice appears as natural law (to those naturalizing it),
 *   pure extraction (to those bearing its costs), necessary coordination (to
 *   beneficiaries), and degraded theater (to institutions maintaining it
 *   through inertia). The extractiveness has increased over a twenty-year
 *   measurement interval as enclosure boundaries have hardened, markets have
 *   integrated, and alternative pastoral livelihoods have contracted. Theater
 *   ratio remains moderate — enclosure is presented as law and economic
 *   progress rather than as political choice — but has not risen to high
 *   levels because the coercive mechanism is visible (armed enforcement,
 *   legal exclusion) and not fully obscured by bureaucratic procedure.
 *
 * KEY AGENTS:
 *   - Traditional Pastoralist Communities: Primary victims (powerless/trapped) — bear full extraction through land loss, subsistence reduction, cultural displacement, economic impoverishment
 *   - Enclosing Landowners: Primary beneficiaries (institutional/arbitrage) — capture land rents, productivity surplus, and capital appreciation from property consolidation
 *   - Commercial Agricultural Interests: Secondary beneficiaries (powerful/mobile) — benefit from access to enclosed lands, reduced competition from commons users, price stability from enclosed supply chains
 *   - The State: Mixed beneficiary and enforcer (institutional/arbitrage) — gains tax revenue, administrative simplification, and political power through land control; bears coordination costs of enforcement
 *   - Pastoral Coalitions: Organized agents (organized/constrained) — when they form, can negotiate partial restoration of access; constrained by power asymmetry and organizational difficulty
 *   - Land Survey and Registry Systems: Institutional actor (institutional/arbitrage) — perpetuates enclosure through bureaucratic procedure and formalization; benefits from continued property registration activity
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing contingent political outcome as inevitable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pastoral_commons_enclosure, 0.68).
domain_priors:suppression_score(pastoral_commons_enclosure, 0.75).
domain_priors:theater_ratio(pastoral_commons_enclosure, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pastoral_commons_enclosure, extractiveness, 0.68).
narrative_ontology:constraint_metric(pastoral_commons_enclosure, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(pastoral_commons_enclosure, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pastoral_commons_enclosure, snare).
narrative_ontology:human_readable(pastoral_commons_enclosure, "Pastoral Commons Enclosure and Land Access Restriction").
narrative_ontology:topic_domain(pastoral_commons_enclosure, "economic/political/social").

domain_priors:requires_active_enforcement(pastoral_commons_enclosure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pastoral_commons_enclosure, enclosing_landowners).
narrative_ontology:constraint_beneficiary(pastoral_commons_enclosure, commercial_agricultural_interests).
narrative_ontology:constraint_victim(pastoral_commons_enclosure, traditional_pastoralist_communities).
narrative_ontology:constraint_victim(pastoral_commons_enclosure, subsistence_herders).
narrative_ontology:constraint_victim(pastoral_commons_enclosure, commons_dependent_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPOSSESSED PASTORALIST (SNARE) — Trapped by legal exclusion from ancestral grazing lands, economic dependency on pastoral production, and lack of alternative livelihood pathways. Suppression is structural: formal title law, enforcement by constabulary, lack of political voice. Bears maximum extraction — loss of subsistence, cultural displacement, economic impoverishment. No meaningful exit options.
constraint_indexing:constraint_classification(pastoral_commons_enclosure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ENCLOSING LANDOWNER (ROPE) — Experiences the constraint as coordination: formal title, legal exclusivity, and enforceable boundaries enable productive land use, capital investment, and agricultural coordination. Benefits from state enforcement of property rights. Arbitrage exit — can always switch land use or lease rights. Net beneficiary; extraction flows toward this agent.
constraint_indexing:constraint_classification(pastoral_commons_enclosure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: AGRICULTURAL STATE (TANGLED ROPE) — State benefits from enclosure through tax revenue, increased agricultural output, and reduced collective-action enforcement costs (enclosed lands are easier to administer than commons). But state also bears coordination burden: must maintain enforcement infrastructure, manage pastoral grievances, prevent poaching and trespass. Mixed extraction and coordination — enforcement is both productive (stabilizes property) and coercive (excludes pastoralists).
constraint_indexing:constraint_classification(pastoral_commons_enclosure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: MARGINAL PASTORAL HOUSEHOLD (SNARE) — Constrained, not trapped: has some mobility (can migrate to other regions, attempt sedentarization, seek wage labor), but costs are high (loss of pastoral identity, social exile, economic precarity in wage labor). Suppression is both structural (legal barrier, armed enforcement) and cultural (community identity fused with pastoral practice). Experiences extraction as primary; some agency but limited. Asymmetry is severe.
constraint_indexing:constraint_classification(pastoral_commons_enclosure, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: IMPERIAL LAND SURVEY SYSTEM (PITON) — Colonial and post-colonial land formalization systems (cadastral surveying, title registration, property law codification) present enclosure as technical and neutral — 'bringing order to the frontier.' The theater ratio is high: formal title appears as law, but the real function (transferring control from commons users to wealthy landowners and the state) is obscured behind neutral bureaucratic procedure. The survey system persists through institutional inertia; alternatives (commons formalization, usufruct registration) exist but are marginalized.
constraint_indexing:constraint_classification(pastoral_commons_enclosure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PASTORALIST COALITION (TANGLED ROPE) — When pastoralist communities organize (through associations, unions, political advocacy), they can negotiate partial commons restoration, seasonal access rights, and grazing corridors. Organization reduces powerlessness but does not eliminate extraction — state and landowners still capture most surplus. Negotiated access is constrained and conditional, not restored. Coordinating collective action is difficult (pastoral populations dispersed, seasonal), creating persistent organizational asymmetry.
constraint_indexing:constraint_classification(pastoral_commons_enclosure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From a universal/analytical perspective, enclosure might appear as an inevitable stage of land-use intensification: 'commons are overexploited, enclosed lands are more productive, therefore enclosure is necessary and natural.' This naturalizes a contingent political outcome. The engine's false summit detector will identify this as naturalization of an institutional choice, not a law of nature. Enclosure serves extractive interests; alternatives (commons formalization, negotiated access) are politically foreclosed, not structurally impossible.
constraint_indexing:constraint_classification(pastoral_commons_enclosure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pastoral_commons_enclosure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pastoral_commons_enclosure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pastoral_commons_enclosure, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pastoral_commons_enclosure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pastoral_commons_enclosure, TR),
    TR >= 0.70.

:- end_tests(pastoral_commons_enclosure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint concentrates control of productive land from dispersed commons users to centralized owners and the state. The surplus extraction is substantial — pastoral communities lose subsistence production, cultural identity, and intergenerational wealth accumulation. The value reflects that extraction is not total (some communities retain marginal grazing, negotiated seasonal access), but the primary flow is clearly from pastoralists to beneficiaries. The measurement trajectory (0.35 → 0.68 over 20 years) reflects increasing enclosure penetration and market integration, tightening the extraction mechanism. Suppression (0.75): High. Barriers to pastoralist exit include legal title law, enforcement by state or private security, cultural displacement consequences, lack of alternative livelihood infrastructure, and economic dependency on pastoral production. Suppression is both structural (legal/material barriers) and cultural/psychological (identity fusion, community displacement). The high value reflects that exit is materially difficult and psychologically catastrophic. Theater ratio (0.35): Moderate-low. Enclosure is not heavily obscured — the legal and coercive mechanisms are visible, and the wealth transfer is apparent to those experiencing extraction. Theater reflects the presentation of enclosure as inevitable economic progress rather than political choice, and the bureaucratic formalization (survey, registration) that makes the transfer appear neutral and technical. The moderate value reflects that enclosure uses less performative cover than some constraints (not dressed as natural law or safety mechanism), but still employs institutional legitimacy and inevitability framing.
 *
 * PERSPECTIVAL GAP:
 *   Maximum gap exists between the trapped pastoralist (Snare classification) and the beneficiary landowner (Rope classification) — one sees pure extraction and immovable barrier, the other sees beneficial coordination and mobility. The analytical observer's false mountain reveals that what appears as natural economic law is actually a reversible institutional choice. The pastoral coalition's constrained-position Tangled Rope sits between pure extraction and beneficiary rope, capturing the partial agency that organization provides without reaching genuine coordination parity.
 *
 * DIRECTIONALITY LOGIC:
 *   Power atoms are derived from structural position in the extraction mechanism. Pastoralists are powerless because they have been excluded from the political process and lack alternatives. Landowners are institutional because they control the means of production and have state backing. The state is institutional and arbitrage because it controls enforcement and can switch policy. Pastoral coalitions are organized because they represent collective agency, but constrained because they operate within state-defined parameters. The land survey system is institutional because it maintains bureaucratic authority over formalization. The analytical observer is analytical because they hold no structural position in the constraint but can see across all positions.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates classification coherence across perspectives when beneficiary/victim positions are clearly declared. The mandatrophy is not present — the six types are consistent with a single underlying structure. The Snare (victims' experience), Rope (beneficiaries' experience), Tangled Rope (state experience), and Piton (institutional degradation) all resolve consistently when directionality is computed from explicit beneficiary/victim declarations and exit options. The false mountain at the analytical level is a true positive: it reveals that naturalizing enclosure as inevitable economic law is a false summit created by omitting the political choice from the analysis. When all perspectives are weighted according to power and access to reality-testing (the analytical observer's privileged position), the constraint resolves as Snare with Piton-level theater — a high-extraction mechanism dressed in neutral institutional language.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_productivity_baseline,
    'Do enclosed lands produce higher sustained yields than well-managed commons, or does the productivity advantage depend on measuring only immediate output while excluding ecological costs?',
    'Long-term yield comparison between enclosed and commons management in comparable ecosystems; inclusion of ecological parameters (soil health, forage regeneration, climate resilience)',
    'If enclosed yields are genuinely higher at all timescales: enclosure produces real coordination benefits (reclassifies toward Tangled Rope for the state). If productivity advantage disappears at 30+ year horizons: enclosure is pure wealth transfer, higher extraction classification justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_productivity_baseline, empirical, 'Whether enclosed lands provide sustained productivity advantages over commons').

omega_variable(
    pastoralist_exit_cost_distribution,
    'Are dispossessed pastoralists truly trapped (no realistic exit options) or constrained (exit is possible at high cost)?',
    'Historical tracking of post-enclosure household trajectories: what proportion achieve sedentarization, wage employment, or migration vs. poverty persistence? At what cost (asset loss, social status, child mortality)?',
    'If trapped (< 10% achieve sustainable alternative): classification as Snare is confirmed, suppression ≥ 0.75 justified. If constrained (30-50% achieve alternatives): reclassify as Tangled Rope for victims, suppression ≤ 0.60, exit_options changes to ''constrained''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pastoralist_exit_cost_distribution, empirical, 'Whether pastoralists are trapped or constrained by enclosure').

omega_variable(
    state_enforcement_intentionality,
    'Does the state enforce enclosure primarily to stabilize property markets and increase tax revenue (active extraction) or to prevent commons overgrazing and ecological degradation (genuine coordination)?',
    'Analysis of colonial and post-colonial administrative records: enforcement priorities (protecting property boundaries vs. managing pastoral access and forage sustainability); investment patterns (survey infrastructure vs. pastoral development infrastructure)',
    'If primary motivation is property enforcement: state classification as Tangled Rope confirmed, beneficiary status justified. If primary motivation is ecological management: reclassify toward Scaffold with sunset (ecological restoration alternatives), beneficiary status becomes secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_intentionality, empirical, 'Primary motivation for state enforcement of enclosure').

omega_variable(
    commons_governance_capacity,
    'Are commons truly vulnerable to tragedy-of-the-commons dynamics, or can they be managed sustainably with institutional design?',
    'Comparative analysis of commons collapse vs. sustainability outcomes; identification of governance mechanisms (rotational grazing, seasonal closure, herd quotas) that prevent overexploitation',
    'If commons are inherently unstable: enclosure framing as necessary coordination is vindicated, theater_ratio decreases. If commons can be sustainably governed: enclosure is revealed as choice to concentrate wealth and control, not ecological necessity, theater_ratio increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_governance_capacity, empirical, 'Whether commons can be sustainably managed without enclosure').

omega_variable(
    identity_lock_in_pastoralism,
    'Is pastoral identity constituted through land access such that enclosure forces not just economic displacement but identity dissolution, or is pastoral identity separable from specific land?',
    'Ethnographic and historical analysis: do dispossessed pastoralists maintain identity and cultural continuity in displacement, or does loss of pastoral practice/land result in identity erasure? What role does intergenerational transmission play?',
    'If identity is inseparable from land access: exit_options should be ''identity_locked'' not ''trapped'', indicating cognitive/identity binding rather than purely material barriers. Classification remains Snare but with identity-lock mechanism. If identity persists in displacement: material trap dominates, ''trapped'' exit_options confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_pastoralism, conceptual, 'Whether pastoral identity is constituted through land access').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pastoral_commons_enclosure, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pce_tr_t0, pastoral_commons_enclosure, theater_ratio, 0, 0.25).
narrative_ontology:measurement(pce_tr_t10, pastoral_commons_enclosure, theater_ratio, 10, 0.32).
narrative_ontology:measurement(pce_tr_t20, pastoral_commons_enclosure, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(pce_be_t0, pastoral_commons_enclosure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pce_be_t10, pastoral_commons_enclosure, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(pce_be_t20, pastoral_commons_enclosure, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pastoral_commons_enclosure, resource_allocation).
narrative_ontology:affects_constraint(pastoral_commons_enclosure, pastoral_livelihood_dependency).
narrative_ontology:affects_constraint(pastoral_commons_enclosure, state_pastoral_policy_capture).
narrative_ontology:affects_constraint(pastoral_commons_enclosure, commons_governance_institutional_capacity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pastoral_commons_enclosure, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
