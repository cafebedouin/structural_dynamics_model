% ============================================================================
% CONSTRAINT STORY: federal_asymmetry__union_bias_design_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federal_asymmetry__union_bias_design_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federal_asymmetry__union_bias_design_reading
 *   human_readable: Indian Federalism: Union Bias by Constitutional Design
 *   domain: constitutional_law/federal_governance
 *
 * SUMMARY:
 *   Indian federalism embodies a fundamental structural asymmetry: it is
 *   declared federal in form (Part XI of the Constitution, separate state
 *   legislatures and executives, enumerated state powers) but operates as
 *   union-centric in substance (Article 356 emergency takeover, all-India
 *   services that bypass state bureaucracies, the union list that reserves
 *   macroeconomic policy to Delhi). This constraint story instantiates ONE
 *   reading of a contested kernel — the reading that this asymmetry is BY
 *   CONSTITUTIONAL DESIGN, not accident or drift. The founders deliberately
 *   built mechanisms to subordinate states to the centre, justified as
 *   necessary for national integration and large-scale coordination. This
 *   reading contends with two sibling readings: (1) Article 370's special
 *   status, which represents the asymmetry's apex and eventual abrogation,
 *   and (2) the linguistic reorganization of 1956, which represented a
 *   concession to regional identity pressure that complicated the centre's
 *   command. This reading (union_bias_design) asserts that the subordination
 *   was intentional; the measurements show extraction accumulation over time
 *   as the centre operationalized its power; the omegas identify the key
 *   structural ambiguities (whether Article 356's threshold is objective or
 *   executive discretion, whether all-India services retain autonomy, whether
 *   the union list's power distribution is necessary or contingent).
 *
 * KEY AGENTS:
 *   - The National Centre (Union Government): Beneficiary (institutional/arbitrage) — gains policy coordination, macroeconomic control, security command, ability to enforce integration
 *   - The Union Bureaucracy (All-India Services, Planning Commission): Beneficiary (institutional/arbitrage) — retains control of state-level implementation, career advancement tied to centre approval, unified administrative norms
 *   - The Holdout State (Exemplar: Kerala, Tamil Nadu, Punjab in different periods): Victim (powerless/trapped) — faces Article 356 invocation if it sustains resistance, cannot constitutionally exit or protect autonomy
 *   - Regional Political Coalitions (State governments, regional parties): Mixed (organized/constrained) — benefit from coordination (infrastructure, macroeconomic policy, civil service expertise) but constrained by asymmetric extraction (emergency powers, centralized service control, union list preemption)
 *   - Subnational Holdout Communities (Linguistic/religious/cultural groups seeking autonomy): Victim (powerless/trapped) — face suppression of distinctiveness through uniformitarian policy, administrative centralization, and union list control
 *   - The Federal Form / Constitutional Legitimacy: Institutional artifact (piton) — the Constitution's federal promises (Part XI, state autonomy guarantees) are increasingly performative, maintained through legitimacy theater while substantive power is centralized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federal_asymmetry__union_bias_design_reading, 0.58).
domain_priors:suppression_score(federal_asymmetry__union_bias_design_reading, 0.68).
domain_priors:theater_ratio(federal_asymmetry__union_bias_design_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federal_asymmetry__union_bias_design_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(federal_asymmetry__union_bias_design_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(federal_asymmetry__union_bias_design_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federal_asymmetry__union_bias_design_reading, tangled_rope).
narrative_ontology:human_readable(federal_asymmetry__union_bias_design_reading, "Indian Federalism: Union Bias by Constitutional Design").
narrative_ontology:topic_domain(federal_asymmetry__union_bias_design_reading, "constitutional_law/federal_governance").

domain_priors:requires_active_enforcement(federal_asymmetry__union_bias_design_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federal_asymmetry__union_bias_design_reading, '77558712-839f-4153-8361-817afe05fd43').
narrative_ontology:cs_kernel_codification('77558712-839f-4153-8361-817afe05fd43', formalized).
narrative_ontology:cs_authority_grounding('77558712-839f-4153-8361-817afe05fd43', lineage).
narrative_ontology:cs_interpretation_layer_present('77558712-839f-4153-8361-817afe05fd43').
narrative_ontology:cs_reading_relation('77558712-839f-4153-8361-817afe05fd43', federal_asymmetry__article_370_special_status, influences).
narrative_ontology:cs_reading_relation('77558712-839f-4153-8361-817afe05fd43', federal_asymmetry__linguistic_reorganization_reading, coexists_with).
narrative_ontology:cs_axiom('77558712-839f-4153-8361-817afe05fd43', foundational, centre_subordination_by_deliberate_design).
narrative_ontology:cs_axiom_status(centre_subordination_by_deliberate_design, holdable).
narrative_ontology:cs_axiom_grounding('77558712-839f-4153-8361-817afe05fd43', centre_subordination_by_deliberate_design, empirically_contingent).
narrative_ontology:cs_axiom('77558712-839f-4153-8361-817afe05fd43', secondary, integration_justifies_subordination).
narrative_ontology:cs_axiom_status(integration_justifies_subordination, holdable).
narrative_ontology:cs_axiom_grounding('77558712-839f-4153-8361-817afe05fd43', integration_justifies_subordination, deontological).
narrative_ontology:cs_reference_frame('77558712-839f-4153-8361-817afe05fd43', union_subordinated_federalism).
narrative_ontology:cs_drift_state('77558712-839f-4153-8361-817afe05fd43', contemporary_coalition_politics_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('77558712-839f-4153-8361-817afe05fd43', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(federal_asymmetry__union_bias_design_reading, federal_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federal_asymmetry__union_bias_design_reading, national_centre).
narrative_ontology:constraint_beneficiary(federal_asymmetry__union_bias_design_reading, union_bureaucracy).
narrative_ontology:constraint_beneficiary(federal_asymmetry__union_bias_design_reading, integrative_policy_unity).
narrative_ontology:constraint_victim(federal_asymmetry__union_bias_design_reading, state_autonomy).
narrative_ontology:constraint_victim(federal_asymmetry__union_bias_design_reading, regional_holdout_capacity).
narrative_ontology:constraint_victim(federal_asymmetry__union_bias_design_reading, subnational_self_determination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE HOLDOUT STATE (SNARE) — A state that attempts sustained resistance to central directives faces Article 356 invoication (President's Rule), supersession of its government, and constitutional suspension. The state has no exit option: it cannot secede, cannot constitutionally resist emergency rule, and cannot protect its autonomy once the centre declares constitutional breakdown. The suppression is maximal and structural. The beneficiary (national integration, centre's policy unity) is enforced through coercion, not coordination.
constraint_indexing:constraint_classification(federal_asymmetry__union_bias_design_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE REGIONAL POLITICAL COALITION (TANGLED ROPE) — Regional parties and state governments benefit from genuine coordination functions: all-India services provide expertise transfer, union list coordination enables macroeconomic policy, administrative integration solves collective action problems for infrastructure and disaster response. But they are constrained by asymmetric extraction: the centre can invoke emergency powers unilaterally, all-India civil service officers are centrally recruited/transferred, and the union list reserves commanding heights (taxation, monetary policy, interstate commerce) to Delhi. The coalition has agency (electoral power, policy leverage) but operates under structural asymmetry.
constraint_indexing:constraint_classification(federal_asymmetry__union_bias_design_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE NATIONAL CENTRE (ROPE) — The union government experiences this constraint as pure coordination: the emergency powers, unified civil service, and union list enable India to function as a coherent macroeconomic and security entity. From the centre's perspective, Article 356 is a safety valve for constitutional breakdown, not an extractive mechanism. The all-India services enable merit-based administration across regional boundaries. The union list prevents a race-to-the-bottom in taxation and regulation. This perspective sees coordination benefits as primary and extraction as a side effect (or not extraction at all, but legitimate hierarchical authority).
constraint_indexing:constraint_classification(federal_asymmetry__union_bias_design_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ELITE COSMOPOLITAN CLASS (TANGLED ROPE) — High-income professionals, national-scale businesses, and Delhi-based elites benefit enormously from the union bias: unified labour rules, national capital markets, centralized tax policy, and unrestricted interstate mobility. They experience the constraint as coordination (benefits from macroeconomic policy unity) with minimal extraction (they can arbitrage between states, influence centre policy, or exit to urban metros). The constraint enables their mobility and wealth accumulation. They have some agency to shape policy through lobbying and political participation.
constraint_indexing:constraint_classification(federal_asymmetry__union_bias_design_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE SUBNATIONAL HOLDOUT COMMUNITY (SNARE) — Communities or states that attempt to sustain distinctiveness (linguistic autonomy, water rights, resource sovereignty, religious or cultural particularity) face suppression through the constitutional architecture: the union list preempts taxation and resource allocation, all-India services enforce uniform administrative norms, and Article 356 can suspend elected governments that resist. These communities have no constitutional exit or holdout mechanism. The constraint extracts regional autonomy in the name of integration.
constraint_indexing:constraint_classification(federal_asymmetry__union_bias_design_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 6: THE FEDERAL FORM / CONSTITUTIONAL LEGITIMACY (PITON) — The Constitution declares India a federal union with genuine state autonomy (Part XI, Schedules VII). This federal promise is largely performative: substantive autonomy has been hollowed by the union list, Article 356, and centralized civil service control. The federal form persists as legitimacy theater (allowing the centre to claim democratic federalism while operating as a unitary state) and as a compromise that satisfied founding fathers but has been operationally superseded. The piton classification reflects that the federal structure survives through institutional inertia and constitutional narrative, not through functional state power.
constraint_indexing:constraint_classification(federal_asymmetry__union_bias_design_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: THE NATURAL LAW VIEW / UNITARY NECESSITY (MOUNTAIN) — From a civilizational perspective, large plural democracies require unity of macroeconomic policy, security coordination, and administrative coherence — India's size, diversity, and development challenges necessitate a strong centre. This reading treats the union bias as an immutable requirement of statecraft, not as a contingent constitutional choice. However, the presence of identifiable beneficiaries (national centre, union bureaucracy) contradicts the mountain classification. The engine's false-summit detector will flag this as naturalization of a contingent institutional arrangement — the claim that unity *requires* this specific constitutional architecture (union list, Article 356, centralized services) conflates genuine coordination need with a particular institutional solution to that need.
constraint_indexing:constraint_classification(federal_asymmetry__union_bias_design_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federal_asymmetry__union_bias_design_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federal_asymmetry__union_bias_design_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federal_asymmetry__union_bias_design_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federal_asymmetry__union_bias_design_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(federal_asymmetry__union_bias_design_reading, TR),
    TR >= 0.70.

:- end_tests(federal_asymmetry__union_bias_design_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The centre extracts state autonomy through three mechanisms: Article 356 (unilateral emergency takeover), all-India services (centralized control of implementation), and the union list (pre-emption of commanding heights). Beneficiaries are clear: the centre gains policy unity, macroeconomic control, and the ability to enforce national integration. Victims are equally clear: states lose autonomy, regional holdouts face constitutional suspension, and subnational communities face suppression of distinctiveness. The extractiveness is not at the snare ceiling (0.70+) because genuine coordination functions exist (macroeconomic policy coordination, disaster response, civil service expertise transfer), so the constraint is hybrid rather than pure extraction. Suppression (0.68): High. States face structural barriers to sustained resistance: they cannot constitutionally challenge emergency rule, cannot secede, and have limited leverage against centre directives enforced through the all-India services. However, suppression is not maximal because regional political parties have achieved some bargaining power through electoral leverage and coalition politics. Theater ratio (0.45): Below-threshold. The constraint operates primarily through formal constitutional mechanism, not performative ritual. Article 356 is explicitly constitutional, the all-India services are formally established, the union list is textually clear. This is functional extraction, not theatrical substitution. The declining theater ratio over time (0.65 → 0.45) reflects that the centre has moved from needing to justify the constraint (early theatre) to operating it as standard procedure (contemporary functionality).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits dramatic perspectival divergence. The national centre sees pure coordination (Rope) — emergency powers are safety valves for breakdown, all-India services enable meritocracy, the union list prevents race-to-the-bottom. The holdout state sees pure extraction (Snare) — it has no constitutional exit, faces Article 356 takeover, and cannot protect its autonomy. Regional coalitions see mixed coordination and extraction (Tangled Rope at organized/constrained) — they benefit from macroeconomic coordination but operate under asymmetric power. Subnational communities see snare (powerless/trapped) — they cannot sustain distinctiveness against uniformitarian policy. The federal form itself is piton (institutional/arbitrage) — the Constitution's federal promises are increasingly theatrical, maintained through legitimacy language while substantive power is centralized. The analytical civilizational observer risks seeing mountain (unitary necessity) — large plural democracies require strong centres — but the presence of clear beneficiaries (centre, union bureaucracy) and structural subordination of regional actors contradicts the natural-law reading. The perspectival gap between the centre's rope and the holdout state's snare is maximum: same constraint, opposite experienced extractiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by agent structural position relative to the extraction flow. The centre benefits from policy unity and autonomy control (beneficiary, low d → negative or low chi). Holdout states have no exit options (trapped) and bear costs of potential Article 356 invocation (victim, high d → high chi). Regional coalitions with electoral power (organized) and constrained exit face moderate extraction (intermediate d). Subnational communities with no political leverage (powerless) and no exit face high extraction (high d). The all-India services occupy an intermediate position: they derive authority from the centre but implement at the state level, experiencing low extraction (they are institutional beneficiaries of centralization). The federal form's derivation involves high directionality uncertainty (omega variable) — the form is supposed to balance centre and states, but the metrics show centre dominance, suggesting the form's directionality calculation is asymmetric (benefits accrue to centre, costs to states).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the hybrid classification (Tangled Rope) correctly models the genuine coordination functions (macroeconomic policy, civil service expertise, national integration) alongside asymmetric extraction (state autonomy subordination, holdout suppression, subnational distinctiveness suppression). Pure rope would miss the extraction; pure snare would miss the coordination. The constraint is tangled: the coordination functions are real and valuable, but they are delivered through an institutional architecture that asymmetrically benefits the centre and subordinates the states. The perspecitival gap reveals that the mandatrophy resolution is perspectival — from the centre's view it is rope (coordination with the centre as beneficiary), from the holdout state's view it is snare (extraction with no reciprocal benefit), from the regional coalition's view it is tangled (mixed). The measurements showing extraction accumulation over time (0.42 → 0.58) suggest that the original compromise was intended as hybrid coordination (tangled rope with more rope than tangle) but has drifted toward tangle-dominant extraction as the centre has operationalized its power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_356_invocation_threshold,
    'What threshold of ''constitutional breakdown'' justifies Article 356 invocation? Is the threshold objective and law-grounded, or does executive discretion determine applicability?',
    'Historical analysis of Article 356 invocations: correlation between formal breakdown criteria and actual invocations; Supreme Court review standard for emergency declarations; comparative analysis with other federal constitutions'' emergency provisions',
    'If threshold is objective and narrow: Article 356 is a legitimate safety valve for genuine constitutional crises (tangled_rope reading holds). If threshold is discretionary or vague: Article 356 becomes an extraction mechanism under the guise of emergency (snare reading strengthens). If threshold has drifted toward executive convenience: demonstrates extraction accumulation over time (measurement support).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_356_invocation_threshold, empirical, 'Whether Article 356 invocation threshold is objective or discretionary').

omega_variable(
    all_india_services_autonomy_erosion,
    'Do all-India civil service officers retain functional independence from central political interference, or has this autonomy eroded to the point where they function as instruments of central power?',
    'Survey of IAS/IPS officer career trajectories: correlation between political alignment and promotion/posting patterns; analysis of state-level service officer postings over time; interview data on perceived political pressure and autonomy',
    'If autonomy retained: all-India services genuinely enable meritocratic coordination (rope/tangled_rope). If autonomy eroded: the services function as a centralization mechanism (snare extraction). If erosion is recent: demonstrates shift from coordination to control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(all_india_services_autonomy_erosion, empirical, 'Functional autonomy of all-India civil service officers').

omega_variable(
    union_list_commanding_heights_definition,
    'Is the union list''s reservation of taxation, monetary policy, and interstate commerce a necessary coordination requirement, or a contingent distribution of power that could be renegotiated without macroeconomic collapse?',
    'Comparative federalism: analysis of power distribution in other federal constitutions (US, Canada, Australia, Germany); examination of historical moments when union list was modified or reinterpreted; economic modeling of alternative distributions',
    'If necessary: union list is fundamental (mountain-ish). If contingent: the list is a negotiated allocation that benefits the centre (tangled_rope/snare reading strengthens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(union_list_commanding_heights_definition, conceptual, 'Whether union list power allocation is necessary or contingent').

omega_variable(
    regional_political_party_capacity_trend,
    'Have regional political parties'' capacity to hold the centre accountable and negotiate state interests increased, decreased, or remained stable over the post-1956 period?',
    'Quantitative analysis of regional party vote share, government formation success, coalition bargaining power over time; qualitative analysis of successful state-level policy resistance to centre directives; case studies of resource renegotiation (water, taxation, autonomy concessions)',
    'If capacity increased: regional holdout options have improved (snare reading weakens). If capacity decreased: extraction has accumulated (snare reading strengthens). If stable: constraint is persistent (piton reading fits).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regional_political_party_capacity_trend, empirical, 'Trend in regional political party bargaining capacity').

omega_variable(
    contest_kernel_framing_under_determination,
    'This reading (union_bias_design) frames the centre''s extraction as deliberate constitutional design. But the sibling reading (linguistic_reorganization) frames the 1956 reorganization as a concession TO regional identity — were the founders designing union bias, or were they incrementally losing control of federalism through identity-based pressure?',
    'Textual analysis of Constituent Assembly debates (Nehru, Ambedkar, state reorganization committee records); comparison of original drafts vs. final Constitution; historical sequencing of Article 356 usage vs. linguistic reorganization pressure',
    'If design-intent (this reading): centre deliberately built subordination into constitution (extractive framing). If incremental drift: centre initially designed symmetrical federalism but lost ground to regional identity movements (alternative framing shifts to influences/coexists_with relation rather than forecloses). The two readings may be empirically compatible if the design was intentional AND the centre subsequently had to accommodate identity-based pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contest_kernel_framing_under_determination, conceptual, 'Whether union bias was deliberate constitutional design or incremental drift from intended federalism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federal_asymmetry__union_bias_design_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fed_union_theater_1947, federal_asymmetry__union_bias_design_reading, theater_ratio, 0, 0.65).
narrative_ontology:measurement(fed_union_theater_1962, federal_asymmetry__union_bias_design_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(fed_union_theater_1982, federal_asymmetry__union_bias_design_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(fed_union_theater_2007, federal_asymmetry__union_bias_design_reading, theater_ratio, 60, 0.45).

% Extraction over time
narrative_ontology:measurement(fed_union_extract_1947, federal_asymmetry__union_bias_design_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fed_union_extract_1962, federal_asymmetry__union_bias_design_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(fed_union_extract_1982, federal_asymmetry__union_bias_design_reading, base_extractiveness, 35, 0.55).
narrative_ontology:measurement(fed_union_extract_2007, federal_asymmetry__union_bias_design_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fed_union_supp_1947, federal_asymmetry__union_bias_design_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fed_union_supp_1962, federal_asymmetry__union_bias_design_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(fed_union_supp_1982, federal_asymmetry__union_bias_design_reading, suppression_requirement, 35, 0.68).
narrative_ontology:measurement(fed_union_supp_2007, federal_asymmetry__union_bias_design_reading, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federal_asymmetry__union_bias_design_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federal_asymmetry__union_bias_design_reading, federal_asymmetry__article_370_special_status).
narrative_ontology:affects_constraint(federal_asymmetry__union_bias_design_reading, federal_asymmetry__linguistic_reorganization_reading).

% DUAL FORMULATION NOTE:
% The federal_asymmetry kernel has three distinct readings, each instantiating a different constraint with different ε values and different beneficiary/victim structures. This story (union_bias_design_reading, ε=0.58) asserts intentional constitutional subordination. The article_370 reading (ε=0.72, snare-dominant) focuses on the peak of asymmetry and its abrogation. The linguistic_reorganization reading (ε=0.45, tangled_rope) frames identity-based disruption of the original design. All three are linked via network.affects_constraints. They share the kernel (the federal structure and Article 356 mechanism) but differ in their reading of intent, trajectory, and extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federal_asymmetry__union_bias_design_reading, institutional, 0.15).
constraint_indexing:directionality_override(federal_asymmetry__union_bias_design_reading, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
