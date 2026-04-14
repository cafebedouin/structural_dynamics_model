% ============================================================================
% CONSTRAINT STORY: sotu_1965_johnson_alliance_for_progress
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1965_johnson_alliance_for_progress, []).

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
 *   constraint_id: sotu_1965_johnson_alliance_for_progress
 *   human_readable: Alliance for Progress: Cold War Development as Institutional Extraction
 *   domain: foreign_policy/development/geopolitics
 *
 * SUMMARY:
 *   The Alliance for Progress (1961-1970, extended through the 1970s) was
 *   framed as a multilateral development partnership between the United
 *   States and Latin American republics. Launched by President Kennedy, it
 *   promised $20 billion in capital flows, technical assistance, and
 *   institutional development to support economic growth and democratic
 *   governance across Latin America. Structurally, it functioned as a Cold
 *   War containment apparatus: redirecting revolutionary impulse and
 *   nationalist opposition toward U.S.-compatible development models, while
 *   displacing traditional patronage networks and suppressing leftist
 *   alternatives. The constraint exhibits classic tangled rope structure —
 *   genuine coordination functions (shared infrastructure development,
 *   educational capacity building, institutional modernization) coexist with
 *   asymmetric extraction (geopolitical subordination, policy conditionality,
 *   displacement of alternative governance models). The mechanism accumulates
 *   extraction over the interval as institutional dependency deepens and
 *   alternative political spaces close. Theater increases (rising
 *   therapy-to-function ratio) as initial development optimism gives way to
 *   recognition that outcomes lag promises, requiring intensified rhetorical
 *   performance to maintain legitimacy.
 *
 * KEY AGENTS:
 *   - U.S. Foreign Policy Establishment: Primary beneficiary (institutional/arbitrage) — gains Cold War containment, regional alignment, institutional reach expansion
 *   - Latin American Modernizing Elites and Technocrats: Secondary beneficiary (institutional/constrained) — gain development capital and bureaucratic positions within aid infrastructure
 *   - Rural Poor and Subsistence Populations: Primary victim (powerless/trapped) — displaced by land consolidation, wage labor precarity, integration into extractive commodity chains
 *   - Traditional Patronage Networks and Landed Elites: Secondary victim (organized/trapped) — displaced by development institutions, cannot maintain traditional extraction or access new systems
 *   - Leftist and Nationalist Political Movements: Primary victim (powerful/trapped) — structurally suppressed through Cold War framing, military aid, and aid conditionality
 *   - Latin American Nation-States (Institutional Collective): Mixed (institutional/constrained) — benefit from capital flows; constrained by geopolitical subordination and loss of policy autonomy
 *   - Cold War Ideological Framework: Maintenance structure (institutional/arbitrage) — persists through bureaucratic inertia after functional necessity declines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1965_johnson_alliance_for_progress, 0.52).
domain_priors:suppression_score(sotu_1965_johnson_alliance_for_progress, 0.65).
domain_priors:theater_ratio(sotu_1965_johnson_alliance_for_progress, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1965_johnson_alliance_for_progress, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1965_johnson_alliance_for_progress, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1965_johnson_alliance_for_progress, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1965_johnson_alliance_for_progress, tangled_rope).
narrative_ontology:human_readable(sotu_1965_johnson_alliance_for_progress, "Alliance for Progress: Cold War Development as Institutional Extraction").
narrative_ontology:topic_domain(sotu_1965_johnson_alliance_for_progress, "foreign_policy/development/geopolitics").

domain_priors:requires_active_enforcement(sotu_1965_johnson_alliance_for_progress).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1965_johnson_alliance_for_progress, u_s_foreign_policy_establishment).
narrative_ontology:constraint_beneficiary(sotu_1965_johnson_alliance_for_progress, latin_american_modernizing_elites).
narrative_ontology:constraint_beneficiary(sotu_1965_johnson_alliance_for_progress, development_bureaucracy).
narrative_ontology:constraint_victim(sotu_1965_johnson_alliance_for_progress, traditional_patronage_networks).
narrative_ontology:constraint_victim(sotu_1965_johnson_alliance_for_progress, leftist_political_movements).
narrative_ontology:constraint_victim(sotu_1965_johnson_alliance_for_progress, rural_subsistence_populations).
narrative_ontology:constraint_victim(sotu_1965_johnson_alliance_for_progress, latin_american_national_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL POOR (SNARE) — Trapped within aid-dependent development frameworks that displace subsistence economies. Face maximum extraction: land consolidation under 'modernization,' wage labor precarity, and integration into extractive commodity chains. No exit options; benefits accrue elsewhere. Cold War containment logic ensures suppression: 'development' displaces alternative social arrangements (cooperative agriculture, indigenous governance) treated as impediments to growth and thus as latent communism.
constraint_indexing:constraint_classification(sotu_1965_johnson_alliance_for_progress, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LATIN AMERICAN REFORMERS (TANGLED ROPE) — Benefit from development capital, technical education, and institutional modernization; constrained by geopolitical conditionality. Can advocate for reform but face structural limits: aid is contingent on suppressing leftist movements, maintaining pro-U.S. alignment, and adopting U.S.-prescribed economic models. Mixed experience — genuine opportunity and genuine subordination. Classified as tangled_rope because the coordination function (joint development agenda) is real, but asymmetric extraction runs through the conditionality mechanism.
constraint_indexing:constraint_classification(sotu_1965_johnson_alliance_for_progress, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. FOREIGN POLICY ESTABLISHMENT (ROPE) — Experiences the constraint as coordination of mutual strategic interest. Benefits from Cold War containment (redirects revolutionary energy toward development), gains Latin American alignment, expands institutional reach (AID, IADB, USAID). Can arbitrage between competing Latin American regimes, reallocate aid flows, and exit if geopolitical calculus shifts. From their perspective, the Alliance solves a genuine coordination problem: how to prevent Communist takeover while maintaining regional relationships. Low experienced extraction for this agent.
constraint_indexing:constraint_classification(sotu_1965_johnson_alliance_for_progress, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: TRADITIONAL PATRONAGE NETWORKS (SNARE) — The Alliance systematically displaces old-regime extraction mechanisms (feudal labor obligation, direct taxation of peasantry, monopolistic landholding) and replaces them with institutionalized development channels controlled by modernizing bureaucrats aligned with the U.S. These elites are trapped: they cannot exit the system (aid dependency is now structural), cannot maintain traditional power (displaced by development institutions), and cannot access new extraction mechanisms without elite realignment. Cold War logic ensures suppression: resistance to 'modernization' is labeled reactionary or communist, delegitimizing alternative governance forms. Unlike the rural poor, these agents have some organizational capacity, but it is deployed defending a losing structural position.
constraint_indexing:constraint_classification(sotu_1965_johnson_alliance_for_progress, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: LEFTIST/NATIONALIST MOVEMENTS (SNARE) — The Alliance creates maximum suppression for non-aligned political alternatives. Cold War framing treats any leftist or nationalist program as potential communism and thus as threat to the development agenda. Aid conditionality ensures military and police capacity to suppress organizing. Land reform promises are implemented in ways that preclude radical redistribution. Labor movements are constrained through alignment with U.S.-backed unions. These movements are trapped: they cannot exit the geopolitical field (it is global), cannot articulate alternative development visions (labeled communist), and cannot build power independent of the aid system (the aid system depletes organizational resources through cooptation and repression). Maximum extraction and suppression; zero agency within the constraint.
constraint_indexing:constraint_classification(sotu_1965_johnson_alliance_for_progress, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: LATIN AMERICAN NATION-STATES (TANGLED ROPE) — Benefit from capital flows, technical capacity, and institutional legitimacy; constrained by geopolitical subordination and loss of policy autonomy. The Alliance coordinates genuine development goals (infrastructure, education, health) while extracting through conditionality: maintain pro-U.S. stance, suppress leftist alternatives, adopt prescribed economic models, accept technological dependency. Nation-states experience this as mixed benefit-and-cost. Can negotiate aid flows and development priorities, but negotiations happen within non-negotiable geopolitical constraints. Exit is costly (loss of capital access, diplomatic isolation) but theoretically available; thus constrained rather than trapped.
constraint_indexing:constraint_classification(sotu_1965_johnson_alliance_for_progress, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: DEVELOPMENTAL STATE ARCHITECTS (SCAFFOLD) — See the Alliance as a transitional mechanism with a sunset: development institutions, capacity building, and technology transfer will eventually enable Latin American autonomy. The coordination function is genuine — joint infrastructure development, educational modernization, institutional building. If the sunset clause fires (development succeeds, nations gain capacity to set independent policy), the constraint dissolves. Constraint is temporary support with declining overhead. However, the sunset is contingent on geopolitical shifts outside the constraint's mechanism; thus this perspective is aspirational rather than structural. Classified as scaffold because the mechanism incorporates sunset logic and the agent has capacity to work toward sunset conditions.
constraint_indexing:constraint_classification(sotu_1965_johnson_alliance_for_progress, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: COLD WAR IDEOLOGICAL FRAMEWORK (PITON) — The Alliance persists largely through institutional inertia and ideological commitment rather than functional necessity. The theatrical performance of 'shared democratic development' masks structural subordination. As Cold War threat recedes, the constraint's primary function (containment) becomes obsolete, but institutional structures persist through bureaucratic momentum. The development theater (conferences, progress reports, celebratory statistics) maintains legitimacy regardless of outcomes. Classification reflects high theater_ratio (0.58) and declining functional coordination.
constraint_indexing:constraint_classification(sotu_1965_johnson_alliance_for_progress, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (FALSE SUMMIT CANDIDATE) — From a civilizational perspective, the constraint appears as an immutable feature of postcolonial geopolitics: developed nations necessarily structure development assistance to secure alignment and influence. The Alliance could be read as expressing an invariant law of international relations — hegemonies cannot tolerate alternatives. However, the structural data reveals this as a false summit: identifiable beneficiaries exist (U.S. foreign policy, modernizing elites), identifiable suppression mechanisms exist (military aid, Cold War framing), and identifiable victims exist (rural poor, leftist movements). The 'necessity' of the arrangement naturalizes what is actually a contingent institutional design serving particular interests.
constraint_indexing:constraint_classification(sotu_1965_johnson_alliance_for_progress, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1965_johnson_alliance_for_progress_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1965_johnson_alliance_for_progress, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1965_johnson_alliance_for_progress, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1965_johnson_alliance_for_progress, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1965_johnson_alliance_for_progress, TR),
    TR >= 0.70.

:- end_tests(sotu_1965_johnson_alliance_for_progress_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over interval. Initial value (0.35) reflects genuine coordination functions in early Alliance years — real capital flows, authentic development projects, and shared interest in anti-communist stability. Rising to 0.58 reflects institutional capture as aid dependency deepens, policy conditionality tightens, and alternative governance models become foreclosed. Suppression (0.65): High. Cold War logic ensures systematic suppression of non-aligned political alternatives. Development rhetoric legitimizes displacement of subsistence economies, land consolidation, and wage labor subordination as inevitable modernization. Traditional elites and leftist movements are both trapped: elites cannot maintain old extraction, movements cannot articulate alternatives without being labeled communist. Theater ratio (0.58): Moderate-high, rising from 0.42 to 0.61. Initial theater reflects development conferences and progress reporting (genuine coordination communication). Rising theater reflects increasing disconnect between promised outcomes and actual material change — bureaucratic expansion persists even as poverty reduction lags expectations. By 1975, the constraint persists largely through institutional inertia and ideological commitment.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The U.S. foreign policy establishment sees pure coordination (Rope) — they experience the constraint as solving a genuine problem of Cold War containment. Modernizing elites see mixed benefit-and-constraint (Tangled Rope) — they gain capital and institutional position but lose policy autonomy. Rural populations see pure extraction (Snare) — they are systematically displaced without compensation or alternatives. Traditional elites see predation (Snare) — their extraction mechanisms are dismantled without providing transition or preservation. Leftist movements see structural suppression (Snare) — they are trapped in a geopolitical field that forecloses their politics. The nation-state perspective is mixed (Tangled Rope) — genuine development capital coexists with loss of autonomy. The developmental state architect sees a temporary scaffold with potential sunset if development succeeds. The Cold War framework itself appears as an inertial piton — it persists through bureaucratic momentum after containing revolution becomes unnecessary. The analytical observer risks seeing this as an immutable law of international relations (mountain), but the structural data reveals false summit: identifiable beneficiaries, specific suppression mechanisms, and particular institutional arrangements that could be otherwise.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from four factors: beneficiary/victim status, power level, exit options, and time horizon. The U.S. foreign policy establishment is a beneficiary with institutional power and arbitrage options (can exit if geopolitical benefit disappears) → low d → low/negative effective extraction (χ). Rural populations are victims with powerless status and trapped exit options → high d → high effective extraction (χ). Latin American nation-states are mixed beneficiaries and victims with institutional power but constrained exit (can theoretically exit but at severe cost) → moderate d → moderate extraction. The directionality derivation captures the structural asymmetry: those who benefit can exit; those who bear costs cannot. This asymmetry is the mechanism of extraction. The measurable gap between beneficiary and victim experiences of the same constraint is not ambiguity — it is evidence of extraction. The constraint works because the beneficiary's choice to stay differs fundamentally from the victim's inability to leave.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The Alliance for Progress avoids mandatrophy (the incoherent classification as both coordination and extraction) by recognizing that it is genuinely both — that is precisely what tangled rope means. The coordination function is real: joint infrastructure development, technical education, institutional capacity building solve actual coordination problems. The extraction is also real: asymmetric power shapes whose interests the coordination serves, geopolitical conditionality restricts participating nations' autonomy, and Cold War logic suppresses alternative development models. The constraint is not coordination misclassified as extraction, nor extraction disguised as coordination. It is the hybrid: successful coordination of a shared interest (anti-communism) implemented through mechanisms that systematically extract from those without power to shape the coordination's terms. This resolves the mandatrophy by showing that the classification (tangled rope) correctly captures the structure. The perspective-dependent classifications (Rope for the beneficiary, Snare for the victim) are both correct — they are not contradictory, but perspectival readings of an asymmetric coordination mechanism. The false summit (mountain) classification at the analytical level reveals what analytical observers risk: naturalizing the coordination aspect while ignoring the extraction, or vice versa. The full structure requires the presheaf of perspectives, not a single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    development_versus_subordination_boundary,
    'At what point does development capital cease funding autonomous capacity-building and becomes a mechanism for structural subordination?',
    'Longitudinal analysis of aid flows paired with institutional autonomy metrics: do aid-receiving nations gain capacity to set independent policy, or does aid dependency increase? Comparison of development outcomes in aligned vs. non-aligned nations with similar initial conditions.',
    'If development is genuinely autonomous: constraint reclassifies toward Scaffold (temporary support with sunset). If aid dependency increases: constraint reclassifies toward Snare for nation-states (structurally subordinate). Critical for distinguishing Tangled Rope from pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(development_versus_subordination_boundary, empirical, 'Whether development funding creates autonomy or dependency').

omega_variable(
    suppression_mechanism_attribution,
    'How much of the measured suppression (0.65) is inherent to development institutions, and how much is attributable to Cold War geopolitical logic?',
    'Counterfactual historical analysis: would the same development institutions suppress leftist alternatives absent the Cold War framing? Comparative institutional analysis of development programs in non-aligned contexts (Yugoslav development, Non-Aligned Movement technical assistance).',
    'If suppression is inherent to development logic: constraint reclassifies as endogenous, not contingent. If suppression is Cold War-contingent: constraint''s extractiveness could decline with geopolitical shifts without institutional change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_attribution, empirical, 'Whether suppression is structural or contingent to Cold War context').

omega_variable(
    theater_ratio_legitimacy_function,
    'What portion of the theater_ratio (0.58) serves legitimate coordination functions (transparent goal-setting, accountability reporting) versus illegitimate performance (hiding extraction, substituting rhetorical progress for material outcomes)?',
    'Analysis of Alliance documentation: ratio of metric reporting to actual outcomes; comparison of claimed development goals to implemented programs; examination of whether reported metrics correlate with material changes in living standards vs. bureaucratic expansion.',
    'If theater legitimately serves coordination: Boltzmann coupling analysis may revise upward. If theater primarily masks extraction: supports Piton classification at civilizational time horizon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_legitimacy_function, empirical, 'Whether theater ratio masks extraction or enables coordination').

omega_variable(
    beneficiary_stability_over_interval,
    'Does the beneficiary set remain constant across the 1961-1975 interval, or does institutional capture gradually shift which actors benefit?',
    'Tracking of aid flow distribution over time; institutional analysis of which Latin American groups gain bureaucratic positions and resources under Alliance frameworks; comparison of intended beneficiaries (modernizing elites, middle class) to actual beneficiaries (technocrats, compradores, military).',
    'If beneficiaries shift toward compradores and military: constraint''s extractiveness may increase over interval, supporting measurement trajectory. If beneficiaries remain stable: measurement trend reflects institutional entrenchment rather than shifting capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_stability_over_interval, empirical, 'Whether beneficiary composition remains stable or shifts toward dependent elites').

omega_variable(
    national_sovereignty_loss_quantification,
    'What is the measurable loss of policy autonomy experienced by participating Latin American nation-states as a function of aid dependency?',
    'Institutional capacity analysis: tracking of policy decisions (land reform, labor law, currency, taxation) that diverge from U.S. preferences by aid-dependent vs. independent-financed nations; analysis of linkages between aid conditionality and domestic policy shifts.',
    'High measured loss of autonomy supports Snare classification for nation-states. Lower autonomy loss supports Tangled Rope (mixed benefit and constraint). Maps directly to exit_options directionality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(national_sovereignty_loss_quantification, empirical, 'Degree of policy autonomy loss correlated with aid dependency').

omega_variable(
    revolutionary_containment_mechanism,
    'Does the Alliance for Progress actually contain revolutionary impulse, or does it displace it into institutional channels where it resurfaces?',
    'Historical analysis of revolutionary activity pre- and post-Alliance: Does aid-receiving nations experience lower radical activity? Does suppression redirect organizing into clandestine or armed channels? Comparison of revolutionary outcomes in Alliance-participant vs. non-participant nations.',
    'If containment is effective: the U.S. beneficiary classification is accurate (extraction is cost of prevented revolution). If revolutionary impulse resurfaces in intensified form: containment mechanism fails, and extraction is pure rent-seeking without functional gain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revolutionary_containment_mechanism, empirical, 'Whether Cold War containment mechanism actually prevents revolutionary alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1965_johnson_alliance_for_progress, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(afp_tr_t0, sotu_1965_johnson_alliance_for_progress, theater_ratio, 0, 0.42).
narrative_ontology:measurement(afp_tr_t3, sotu_1965_johnson_alliance_for_progress, theater_ratio, 3, 0.5).
narrative_ontology:measurement(afp_tr_t7, sotu_1965_johnson_alliance_for_progress, theater_ratio, 7, 0.58).
narrative_ontology:measurement(afp_tr_t14, sotu_1965_johnson_alliance_for_progress, theater_ratio, 14, 0.61).

% Extraction over time
narrative_ontology:measurement(afp_be_t0, sotu_1965_johnson_alliance_for_progress, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(afp_be_t3, sotu_1965_johnson_alliance_for_progress, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(afp_be_t7, sotu_1965_johnson_alliance_for_progress, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(afp_be_t14, sotu_1965_johnson_alliance_for_progress, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1965_johnson_alliance_for_progress, resource_allocation).
narrative_ontology:affects_constraint(sotu_1965_johnson_alliance_for_progress, u_s_military_aid_latin_america).
narrative_ontology:affects_constraint(sotu_1965_johnson_alliance_for_progress, land_reform_rhetoric_versus_implementation).
narrative_ontology:affects_constraint(sotu_1965_johnson_alliance_for_progress, import_substitution_industrialization_dependency).

% DUAL FORMULATION NOTE:
% The Alliance for Progress decomposes into multiple structurally distinct constraints sharing the institutional framework but with different ε values. Land reform (high theater, low implementation, ε ≈ 0.70) versus development infrastructure (lower theater, moderate implementation, ε ≈ 0.35) operate through the same aid mechanism but produce different extraction profiles. Military aid conditionality (ε ≈ 0.78, high suppression) versus technical assistance (ε ≈ 0.28, low suppression) both flow through Alliance channels but have structurally different mechanisms. The present story models the Alliance's integrated constraint structure; decomposition into component mechanisms reveals that the headline coordination goal masks distinct extraction channels with different severity levels.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1965_johnson_alliance_for_progress, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
