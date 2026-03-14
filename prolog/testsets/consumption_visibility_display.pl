% ============================================================================
% CONSTRAINT STORY: consumption_visibility_display
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_consumption_visibility_display, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: consumption_visibility_display
 *   human_readable: Consumption Visibility Display Norm
 *   domain: social/behavioral/economic
 *
 * SUMMARY:
 *   Consumption visibility displays — the social norm that consumption should
 *   be publicly legible as a marker of status, taste, and identity — create a
 *   structural tension between genuine coordination (mutual recognition,
 *   community bonding, aesthetic communication) and extractive status
 *   competition. The constraint operates across all modern consumer societies
 *   but with varying intensity based on income inequality and digital
 *   platform penetration. What began as sumptuary laws (legal restrictions on
 *   who could display consumption markers, used to maintain class
 *   hierarchies) has evolved into decentralized social enforcement through
 *   visibility norms. The constraint now lacks formal authority but functions
 *   through peer pressure, career effects, and identity fusion. The
 *   extractiveness has increased over the interval (0.38 to 0.58) as digital
 *   platforms have made consumption visibility technically easier and
 *   socially normalized, while the theater ratio has increased (0.42 to 0.58)
 *   as the performative component has grown relative to genuine coordination.
 *
 * KEY AGENTS:
 *   - Lower-Income Consumers: Primary victims (powerless/trapped) — bear full cost of invisibility through social sanction; no exit option without abandoning communities
 *   - Environmental Commons: Abstract victim (powerless/trapped) — bears extraction through resource depletion driven by visibility-status competition
 *   - Conscious Consumers: Secondary victims (moderate/constrained) — face significant cost to exit but retain agency to reduce consumption visibility
 *   - Luxury Goods Producers: Primary beneficiaries (institutional/arbitrage) — capture profit from status-differentiated products; experience constraint as pure coordination
 *   - Advertising/Media Industries: Beneficiaries (institutional/arbitrage) — profit from visibility platforms enabling status signaling
 *   - High-Income Consumers: Mixed (powerful/mobile) — benefit from visibility but also face consumption treadmill extraction; have exit options
 *   - Historical Sumptuary System: Degraded institution (institutional/constrained) — maintained through social inertia rather than function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing status signaling as immutable human law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consumption_visibility_display, 0.58).
domain_priors:suppression_score(consumption_visibility_display, 0.62).
domain_priors:theater_ratio(consumption_visibility_display, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consumption_visibility_display, extractiveness, 0.58).
narrative_ontology:constraint_metric(consumption_visibility_display, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(consumption_visibility_display, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consumption_visibility_display, tangled_rope).
narrative_ontology:human_readable(consumption_visibility_display, "Consumption Visibility Display Norm").
narrative_ontology:topic_domain(consumption_visibility_display, "social/behavioral/economic").

domain_priors:requires_active_enforcement(consumption_visibility_display).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consumption_visibility_display, status_signaling_beneficiaries).
narrative_ontology:constraint_beneficiary(consumption_visibility_display, luxury_goods_producers).
narrative_ontology:constraint_beneficiary(consumption_visibility_display, advertising_industry).
narrative_ontology:constraint_victim(consumption_visibility_display, resource_conservationists).
narrative_ontology:constraint_victim(consumption_visibility_display, lower_income_consumers).
narrative_ontology:constraint_victim(consumption_visibility_display, environmental_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOWER-INCOME CONSUMER (SNARE) — Trapped in a consumption visibility hierarchy where not displaying consumption marks them as poor/unsuccessful. Exit would require either (a) abandoning social participation or (b) achieving higher income. No intermediate option: private consumption is not socially legible. Bears full cost of visibility norm enforcement through social sanction. Zero degrees of freedom within the constraint.
constraint_indexing:constraint_classification(consumption_visibility_display, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENVIRONMENTAL COMMONS (SNARE) — Bears extraction through unsustainable consumption driven by visibility norms. No exit option; no voice in coordination. Abstract victim with no agency. Extraction manifests as resource depletion, emissions, and waste concentrated in status-competition goods with minimal functional differentiation from lower-visibility alternatives.
constraint_indexing:constraint_classification(consumption_visibility_display, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CONSCIOUS CONSUMER (TANGLED ROPE) — Faces significant costs to exit (social penalty, reputational damage, identity friction) but retains agency. Can choose less-visible consumption at career/relationship risk. Benefits from social coordination function: consumption visibility enables mutual recognition and community bonding. Mixed extraction and genuine coordination.
constraint_indexing:constraint_classification(consumption_visibility_display, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LUXURY GOODS PRODUCERS (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: visibility norms solve the problem of distinguishing their products in a crowded market. Exit is costless (could market to function rather than status, but have no incentive to). Net beneficiary with arbitrage freedom.
constraint_indexing:constraint_classification(consumption_visibility_display, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ADVERTISING/MEDIA (ROPE) — Beneficiary. Visibility norms create demand for advertising platforms that enable status signaling. Pure coordination from their perspective: advertising solves the matching problem between consumers seeking visibility and status-differentiated products. Net beneficiary with exit freedom.
constraint_indexing:constraint_classification(consumption_visibility_display, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: HIGH-INCOME CONSUMER (TANGLED ROPE) — Mixed experience. Benefits from visibility norms (can afford visible consumption and reaps status gains). Also bears extraction through consumption treadmill: must continuously upgrade visibility markers to maintain relative status as others increase consumption. Has exit option (geographic arbitrage to lower-visibility communities) but mobile enough to arbitrage between visibility regimes. Net beneficiary with agency.
constraint_indexing:constraint_classification(consumption_visibility_display, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: SUMPTUARY LAW LEGACY (PITON) — Historical sumptuary laws (legal restrictions on consumption display) have been abolished in most jurisdictions but the functional constraint persists through social enforcement. Theater ratio high: the normative mechanism (social judgment, peer pressure) performs the regulation that formal law once did. The institution carries forward its historical form despite changed context. Degraded from pure coordination (community mutual recognition) into status maintenance ritual.
constraint_indexing:constraint_classification(consumption_visibility_display, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (FALSE SUMMIT) — From a civilizational view, the constraint might appear to be a natural law of human signaling: costly display is inherent to reputation mechanisms, observed across all human societies. However, this naturalizes contingent institutional arrangements (luxury goods markets, advertising media, income inequality enabling visible differentiation) as immutable. The base metrics contradict mountain classification: extractiveness 0.58, suppression 0.62, theater 0.58. The engine will identify this as a false summit — naturalization of a tangled rope.
constraint_indexing:constraint_classification(consumption_visibility_display, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consumption_visibility_display_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(consumption_visibility_display, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consumption_visibility_display, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(consumption_visibility_display, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(consumption_visibility_display, TR),
    TR >= 0.70.

:- end_tests(consumption_visibility_display_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint generates significant extraction through status competition (luxury spending driven by visibility rather than function, environmental cost concentration in status-goods, income-based social sanction). However, extraction is not total because genuine coordination functions persist: consumption visibility does enable mutual recognition, aesthetic communication, and community bonding. The measure reflects mixed extraction and coordination. Suppression (0.62): Moderate-high. Significant barriers to exit include social sanction (reputation damage, peer exclusion), career effects (promoters/hiring weighted toward visible status markers), and psychological costs (shame, identity friction). However, suppression is not total: agents with sufficient income can afford visibility costs, and some communities have lower visibility norms. Theater ratio (0.58): Moderate. The constraint operates partly through genuine coordination (aesthetic communication, mutual recognition) and partly through performative status ritual. The ratio has increased over time as digital platforms have enabled easier visibility performance and social media has standardized status display formats. The theater ratio increase mirrors extractiveness increase, suggesting Goodhart drift: as visibility becomes easier to perform, the functional signaling value decays and the performative component dominates.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how indexical position determines perceived type. The beneficiary's rope reflects their genuine experience of efficient status signaling. The trapped victim's snare reflects their genuine experience of unescapable social pressure. The conscious consumer's tangled rope reflects mixed coordination/extraction. The high-income consumer's tangled rope with agency reflects mixed experience with exit options. The institutional historian's piton reflects degradation of functional mechanisms. The civilizational analytical view risks false summitry (naturalizing status signaling as immutable) despite structural evidence of contingency (inequality-coupling, media-dependence, social enforcement). The perspectival gap is diagnostic: if all perspectives converged on the same type, the constraint would be mischaracterized. The divergence shows that the constraint genuinely operates across multiple structural levels (coordination, extraction, degradation) simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value derives from (a) beneficiary/victim status, (b) exit options, and (c) power level. Lower-income consumers are victims with trapped exit → high d → high f(d) → high experienced χ. Luxury producers are beneficiaries with arbitrage exit → low d → low f(d) → negative or near-zero experienced χ. Conscious consumers are victims with constrained exit → moderate d → moderate f(d) → moderate χ. High-income consumers are mixed (both beneficiary and victim) with mobile exit → balanced d → moderate f(d). The overrides are not needed: the derivation chain captures the structural relationships accurately through beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that tangled rope is the correct canonical type: the constraint has both genuine coordination (mutual recognition, aesthetic communication, status differentiation enabling market function) AND asymmetric extraction (status competition driving consumption above functional need, environmental burden, income-based social sanction). The coordination function cannot be separated from the extraction mechanism — they are operationally entangled. The beneficiary's rope is a perspectival artifact (they see only the coordination benefit). The victim's snare is also perspectival (they see only the extraction). The analytical tangled rope at organizational level is the structural truth: both functions exist, both are enforced, both contribute to the constraint's persistence. The mandatrophy is resolved by recognizing that perspectival gap is not a classification error — it is diagnostic data showing that different agents genuinely experience the constraint differently because they occupy different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    visibility_necessity_threshold,
    'What proportion of social coordination genuinely requires consumption visibility vs. what portion is rent-seeking layered onto coordination?',
    'Comparative analysis of societies/communities with low consumption visibility norms; measurement of coordination quality (social cohesion, mutual recognition) independent of visible consumption markers',
    'If low necessity (<30%): extractiveness increases (the constraint is primarily status competition, not coordination). If high necessity (>70%): extractiveness decreases (the constraint solves a genuine coordination problem). Classification may shift toward rope if coordination necessity is high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(visibility_necessity_threshold, conceptual, 'Necessity of consumption visibility for social coordination').

omega_variable(
    income_inequality_coupling,
    'Is the extraction mechanism fundamentally tied to income inequality, or would consumption visibility norms persist in egalitarian contexts?',
    'Analysis of consumption visibility dynamics in low-inequality societies (Nordic countries, intentional communities); historical analysis of visibility norms in pre-industrial egalitarian contexts',
    'If coupled to inequality: reducing inequality would degrade the constraint structure (victims gain arbitrage options). If independent: the constraint is robust to equality changes. Current measurement assumes inequality coupling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(income_inequality_coupling, empirical, 'Whether visibility norms depend on income inequality').

omega_variable(
    digital_transparency_feedback,
    'Do digital consumption tracking platforms (social media, purchase visibility, spending apps) increase or decrease the total extractiveness of the visibility norm?',
    'Longitudinal measurement of consumption visibility behavior pre/post social media adoption; survey data on perceived visibility pressure in high-digital-adoption cohorts vs low-adoption cohorts',
    'If digital amplification: extractiveness rising over time (theater ratio increasing, measurements should show upward trajectory). If transparency saturation: extractiveness plateauing or declining (norms may be inverting toward privacy/minimalism). Critical for measurement trajectory interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_transparency_feedback, empirical, 'Effect of digital consumption tracking on visibility norm extractiveness').

omega_variable(
    suppression_internalization_ratio,
    'What proportion of the measured suppression (0.62) is external (social sanctions, career/relationship damage) vs. internalized (shame, identity fusion with consumption markers)?',
    'Post-exit suppression analysis: measure continued consumption anxiety in agents who have physically left visibility-intense communities; cognitive reframing studies showing persistence of internalized visibility pressure after external sanctions removed',
    'If highly internalized (>60%): suppression is partly cognitive capture, classified as identity_locked component. If primarily external (>60%): suppression is material barrier, constraining exit cost but not making exit unthinkable. Affects interpersonal decomposition strategy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_ratio, empirical, 'Internalization vs. external suppression mechanism ratio').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consumption_visibility_display, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cvd_tr_t0, consumption_visibility_display, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cvd_tr_t20, consumption_visibility_display, theater_ratio, 20, 0.5).
narrative_ontology:measurement(cvd_tr_t40, consumption_visibility_display, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(cvd_be_t0, consumption_visibility_display, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cvd_be_t20, consumption_visibility_display, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(cvd_be_t40, consumption_visibility_display, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consumption_visibility_display, identity_coordination).
narrative_ontology:boltzmann_floor_override(consumption_visibility_display, 0.12).
narrative_ontology:affects_constraint(consumption_visibility_display, luxury_goods_production).
narrative_ontology:affects_constraint(consumption_visibility_display, advertising_market_structure).
narrative_ontology:affects_constraint(consumption_visibility_display, social_status_hierarchy).
narrative_ontology:affects_constraint(consumption_visibility_display, environmental_resource_extraction).

% DUAL FORMULATION NOTE:
% Consumption visibility display is upstream of multiple domain-specific constraints: it drives structure in luxury goods markets (which goods get produced), advertising markets (which platforms commodify visibility), social hierarchies (which markers signal status), and environmental extraction (resource intensity of status goods). Each downstream constraint has its own ε value reflecting domain-specific mechanisms. The visibility display itself is the coordination mechanism that couples these domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(consumption_visibility_display, powerless, 1.0).
constraint_indexing:directionality_override(consumption_visibility_display, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
