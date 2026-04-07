% ============================================================================
% CONSTRAINT STORY: arctic_resource_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arctic_resource_extraction, []).

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
 *   constraint_id: arctic_resource_extraction
 *   human_readable: Arctic Resource Extraction and Climate Feedback Lock-in
 *   domain: geopolitical/environmental/economic
 *
 * SUMMARY:
 *   Arctic resource extraction operates as a multi-layered constraint
 *   capturing benefits for hydrocarbon extractors, arctic nation-states, and
 *   global industrial supply chains while externalizing costs onto Arctic
 *   indigenous communities, ecosystems, and future climate stability. The
 *   constraint exhibits the full spectrum of Deferential Realism
 *   classifications depending on structural position: indigenous communities
 *   perceive it as an inescapable snare (trapped by colonial law and capital
 *   concentration); the climate system experiences it as thermodynamic
 *   lock-in (trapped by irreversible carbon); nation-states navigate it as
 *   mixed coordination and extraction (Arctic Council coordination alongside
 *   asymmetric benefit distribution); hydrocarbon firms experience it as
 *   enabling infrastructure (rope); renewable energy transitions see it as a
 *   temporary constraint with a sunset clause (scaffold); environmental
 *   regulators maintain it as degraded theater (piton); and analytical
 *   observers risk naturalizing political economy as immutable physical law
 *   (false mountain). The constraint's extractiveness has increased from 0.42
 *   to 0.68 over two decades as Arctic ice retreat has made extraction
 *   technically feasible and geopolitically contested; theater ratio has
 *   increased from 0.35 to 0.58 as environmental compliance frameworks have
 *   expanded while actual environmental outcomes have deteriorated. The
 *   fundamental structural tension is between short-term capital accumulation
 *   and long-term climate stability—Arctic extraction exemplifies how
 *   institutional arrangements create lock-in on timescales that hide
 *   consequences from the decision-making horizons of beneficiaries.
 *
 * KEY AGENTS:
 *   - Arctic Indigenous Communities: Primary victims (powerless/trapped) — face direct land dispossession, environmental degradation of subsistence resources, health impacts from extraction infrastructure, and colonial legal frameworks preventing genuine sovereignty
 *   - Global Climate System / Future Generations: Structural victim (powerless/trapped) — experience extraction consequences through irreversible carbon release and feedback mechanisms; no exit option or agency; trapped by thermodynamic constraints
 *   - Arctic Nation-States (Russia, Canada, USA, Nordic states): Mixed position (moderate/constrained to powerful/arbitrage) — benefit from resource revenues and strategic sovereignty assertion; constrained by geopolitical competition and climate risk; active enforcement required to maintain coordination and extraction simultaneously
 *   - Hydrocarbon Extractors (Oil/Gas Firms, Mining Companies): Primary beneficiaries (institutional/arbitrage) — capture capital gains during extraction window; can arbitrage to alternative resource sites; experience constraint as enabling infrastructure
 *   - Arctic Ecosystems: Structural victim (no power/trapped) — cannot exit or organize; experience direct extraction through habitat destruction, pollution, and climate feedback cascades
 *   - Energy Transition Coalition (Renewable firms, climate-aware states, investors): Organized agents (organized/constrained) — perceive Arctic extraction as temporary constraint with technological sunset; building alternative energy infrastructure; constrained by capital and political economy inertia
 *   - Environmental Regulators: Institutional theater-maintainers (institutional/arbitrage) — enforce compliance frameworks that persist through inertia despite documented inefficacy; see own role as degraded but continue performance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arctic_resource_extraction, 0.68).
domain_priors:suppression_score(arctic_resource_extraction, 0.72).
domain_priors:theater_ratio(arctic_resource_extraction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arctic_resource_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(arctic_resource_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(arctic_resource_extraction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arctic_resource_extraction, snare).
narrative_ontology:human_readable(arctic_resource_extraction, "Arctic Resource Extraction and Climate Feedback Lock-in").
narrative_ontology:topic_domain(arctic_resource_extraction, "geopolitical/environmental/economic").

domain_priors:requires_active_enforcement(arctic_resource_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arctic_resource_extraction, hydrocarbon_extractors).
narrative_ontology:constraint_beneficiary(arctic_resource_extraction, arctic_nation_states).
narrative_ontology:constraint_beneficiary(arctic_resource_extraction, industrial_supply_chains).
narrative_ontology:constraint_victim(arctic_resource_extraction, arctic_indigenous_communities).
narrative_ontology:constraint_victim(arctic_resource_extraction, global_climate_stability).
narrative_ontology:constraint_victim(arctic_resource_extraction, arctic_ecosystems).
narrative_ontology:constraint_victim(arctic_resource_extraction, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARCTIC INDIGENOUS COMMUNITIES (SNARE) — Trapped within traditional territories; cannot exit without abandoning ancestral lands, language, and cultural identity. Face direct extraction through resource development, environmental degradation of subsistence resources, and health impacts from extraction infrastructure. Maximum experienced extraction with no meaningful exit option. Suppression operates through colonial legal frameworks, capital concentration, and geopolitical power asymmetries.
constraint_indexing:constraint_classification(arctic_resource_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: GLOBAL CLIMATE SYSTEM / FUTURE GENERATIONS (SNARE) — Trapped by irreversible carbon releases and albedo feedback mechanisms; cannot exit from climate destabilization once triggered. Arctic methane releases and ice-albedo feedback create lock-in where extraction today determines atmospheric composition for centuries. Maximum extraction from an agent with zero agency and no alternative pathways. Theater ratio reflects climate denial and discounting future costs.
constraint_indexing:constraint_classification(arctic_resource_extraction, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ARCTIC NATION-STATES / ARCTIC ECONOMIES (TANGLED ROPE) — Constrained by geopolitical rivalry and economic dependency on resource revenues. Genuine coordination function exists: Arctic Council facilitates resource sharing and sovereignty negotiation. But asymmetric extraction occurs: powerful states (Russia, Canada, USA) capture disproportionate benefits while smaller economies and indigenous nations bear costs. Active enforcement required to maintain both coordination mechanisms (territorial claims, trade agreements) and extraction (environmental degradation, resource monopoly).
constraint_indexing:constraint_classification(arctic_resource_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HYDROCARBON EXTRACTORS AND SUPPLY CHAINS (ROPE) — Experiences the constraint as pure coordination: Arctic resources enable global energy markets, capital flows, and industrial supply chains. Can arbitrage—exit to other extraction sites (Middle East, deepwater, shale). Benefits from infrastructure coordination (pipelines, shipping routes, governance frameworks) that the extraction constraint provides. Minimal experienced extraction; sees constraint as enabling coordination.
constraint_indexing:constraint_classification(arctic_resource_extraction, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CLIMATE MITIGATION / ENERGY TRANSITION COALITION (SCAFFOLD) — Organized agents (IPCC, renewable energy firms, climate-aware states, investor coalitions) view Arctic extraction as a temporary constraint dissolving under energy transition pressure. Renewable capacity growth, battery technology, and divestment create an alternative to Arctic hydrocarbon dependency with a sunset clause: as renewable LCOE drops and grid integration improves, Arctic oil and gas loses economic viability. Suppression is moderate because exit pathways exist (technological substitution, policy change). Theater ratio reflects aspiration and greenwashing alongside genuine decarbonization progress.
constraint_indexing:constraint_classification(arctic_resource_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ENVIRONMENTAL COMPLIANCE AND REGULATORY THEATER (PITON) — Arctic extraction operates under extensive environmental impact assessments, permitting frameworks, and remediation mandates that are largely performative. Constraints are maintained through institutional inertia: regulations persist despite documented inefficacy (carbon intensity of Arctic oil, ecosystem damage beyond mitigation potential). Theater ratio is elevated by the gap between regulatory rigor and actual environmental outcomes. The constraint persists because alternatives (banning extraction, mandatory transition) haven't fully replaced it, not because compliance mechanisms work.
constraint_indexing:constraint_classification(arctic_resource_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / THERMODYNAMIC LIMITS (MOUNTAIN-ASPIRATION) — This perspective attempts to naturalize Arctic extraction as a constraint of human civilization—that resource competition and thermodynamic requirements for industrial societies create immutable pressures toward extraction. However, the structural data contradicts mountain classification: the extractiveness (0.68), suppression (0.72), and active enforcement requirements reveal contingent institutional arrangements (property rights, capital concentration, energy infrastructure design) rather than immutable laws. The engine identifies this as a false summit—naturalization of political economy as physics.
constraint_indexing:constraint_classification(arctic_resource_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arctic_resource_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arctic_resource_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arctic_resource_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arctic_resource_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(arctic_resource_extraction, TR),
    TR >= 0.70.

:- end_tests(arctic_resource_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Arctic extraction directly transfers resource value (oil, gas, minerals) worth hundreds of billions of dollars to hydrocarbon extractors while externalizing climate costs (estimated at trillions in future damages) onto powerless and uncompensated actors. The extraction is asymmetric, concentrated, and benefits a small set of institutional actors while harming dispersed future populations. Suppression (0.72): High. Multiple suppression mechanisms operate simultaneously: (1) Colonial legal frameworks treating indigenous territories as open-access resources rather than sovereign lands; (2) Capital concentration enabling large-scale extraction despite local opposition; (3) Geopolitical power asymmetries preventing indigenous and smaller-nation coordination; (4) Knowledge suppression around climate feedback mechanisms and extraction-driven acceleration of Arctic warming; (5) Time horizon mismatch where extraction benefits are immediate and concentrated while costs are delayed and distributed. Theater ratio (0.58): Moderate-high. Environmental impact assessments and permitting frameworks are extensive but largely performative—they create legitimacy narratives while Arctic extraction drives accelerating environmental change. Remediation mandates are inadequate relative to documented ecosystem damage. Climate compliance claims (carbon-intensive Arctic oil branded as 'responsibly sourced') are narratives masking thermodynamic reality. Theater has increased as environmental consciousness has risen and extractors have adopted more sophisticated compliance narratives.
 *
 * PERSPECTIVAL GAP:
 *   Arctic indigenous communities experience a snare with maximum perceived extraction and zero structural exit; global climate system experiences a thermodynamic lock-in where extraction today forecloses future climate stability. Arctic nation-states experience tangled rope where genuine coordination (Arctic Council, resource sharing) coexists with asymmetric extraction (resource benefits concentrated in capital-rich states). Hydrocarbon extractors experience rope where the constraint enables their operations and market access. Energy transition advocates experience scaffold where renewable alternatives create a sunset pathway. Environmental regulators experience piton where compliance rituals persist despite low functional efficacy. The analytical observer risks seeing a mountain (Arctic extraction as inevitable given human thermodynamic requirements) but the structural data reveals this as naturalization—extractiveness, suppression, and active enforcement all increase with institutional intensity, not with physical necessity. The perspectival gap reveals that Arctic extraction is not a constraint imposed by nature but a constraint maintained by institutional arrangements (property rights, capital concentration, political economy).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural position relative to the extraction flow. Arctic indigenous communities occupy d ≈ 0.95 (full victims, trapped exit) experiencing maximum f(d) ≈ 1.42 effective extraction. The climate system occupies d = 1.0 (pure target, no exit) experiencing maximum extraction from civilizational timescales. Arctic nation-states experience mixed directionality: Russia and Canada as extractors/beneficiaries occupy low d (0.15-0.25); smaller indigenous-majority territories occupy high d (0.80-0.90). Hydrocarbon extractors occupy d ≈ 0.05-0.15 (beneficiaries with arbitrage exit) experiencing negative or minimal f(d) values. Energy transition organizations occupy d ≈ 0.40-0.50 (organized agents with exit via technological substitution) experiencing moderate f(d). The constraint operates at continental-to-global scope (σ(S) = 1.1-1.2) which amplifies χ values across all agents. The scope modifier reflects that Arctic extraction drives global climate feedback and connects to planetary energy infrastructure.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: Arctic resource extraction classifies as snare across the powerless/victim perspectives and as snare at the global/civilizational analytical level (future generations trapped by climate lock-in). The mandatrophy is resolved by recognizing that the beneficiary perspectives (hydrocarbon extractors, resource-extracting nation-states) experience rope or scaffold—genuine coordination function exists (Arctic Council governance, international resource sharing), asymmetric extraction is intentional rather than accidental, and active enforcement maintains the asymmetry. The constraint is SNARE precisely because the beneficiary group has designed and maintained institutional arrangements (property rights, capital mobility, geopolitical power) that prevent victims from exercising exit or voice. This is not a labeling confusion but a structural description: snare classification identifies that the victims (indigenous communities, climate system, future generations) are trapped by intentional institutional design, not by natural law or unfortunate coordination failure. The high mandatrophy_resolved flag indicates that the analysis accepts snare classification despite potential ambiguity around whether extraction is 'truly coercive' or 'merely asymmetric'—the answer is that institutional arrangements make exit impossible, which is the definition of coercion in the Deferential Realism framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    climate_feedback_threshold_irreversibility,
    'At what extraction rate and accumulated atmospheric CO2 do Arctic methane releases and ice-albedo feedbacks become thermodynamically irreversible on civilizational timescales?',
    'Paleoclimate analysis of previous rapid warming events; high-resolution climate modeling of feedback cascade initiation; monitoring of methane release rates from thawing permafrost and continental shelves',
    'If threshold is near (e.g., 450 ppm CO2): Arctic extraction creates lock-in on timescales shorter than political action horizons. Future generations experience extraction as mountain (immutable consequence). If threshold is distant: extraction creates risk but not certainty, changing classification from snare (certain harm) to tangled_rope (mixed outcome).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_feedback_threshold_irreversibility, empirical, 'Whether Arctic extraction triggers irreversible climate feedback').

omega_variable(
    indigenous_sovereignty_versus_resource_rights,
    'Are indigenous land rights and resource sovereignty genuine structural exits (enabling indigenous-led alternative extraction models) or performative consultation rituals masking continued dispossession?',
    'Comparative analysis of regions with genuine indigenous veto power (e.g., Inuit Tapiriit Kanatami Arctic Council voting) versus consultation-only frameworks; tracking of resource control outcomes in communities with full decision authority vs advisory authority',
    'If genuine sovereignty: indigenous power changes from powerless/trapped to powerful/arbitrage, reclassifying the constraint from snare to tangled_rope. If performative: trapped classification confirmed; suppression mechanism confirmed as colonial legal structures masking dispossession.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_sovereignty_versus_resource_rights, empirical, 'Whether indigenous sovereignty frameworks provide genuine exit or ritual consultation').

omega_variable(
    renewable_energy_transition_timeline_feasibility,
    'Can renewable energy capacity, grid storage, and industrial electrification eliminate Arctic hydrocarbon dependency before climate tipping points are triggered?',
    'Techno-economic modeling of renewable LCOE curves, storage deployment rates, and industrial electrification feasibility; comparison against decarbonization pathways consistent with 1.5-2.0°C warming targets',
    'If feasible within climate constraints: scaffold perspective is structural (sunset is real). If infeasible: scaffold is aspirational; Arctic extraction remains a snare with no actual exit. Climate constraint shifts from abstract to immediate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_energy_transition_timeline_feasibility, empirical, 'Whether energy transition can substitute for Arctic extraction within climate constraints').

omega_variable(
    capital_stranding_versus_sunk_cost_escalation,
    'Will fossil fuel capital stocks and infrastructure investments drive continued Arctic extraction despite renewable alternatives, or will climate policy and divestment pressure force retirement of extractive infrastructure?',
    'Tracking of Arctic extraction project capital expenditure vs renewable capacity capital expenditure; monitoring of stranded asset writedowns; analysis of policy signals (carbon pricing, divestment commitments, treaty obligations)',
    'If capital forces continued extraction: snare classification confirmed; institutional actors experience high lock-in despite technical alternatives. If policy enables stranding: scaffold timeline becomes achievable; constraint moves toward retirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_stranding_versus_sunk_cost_escalation, empirical, 'Whether sunk costs lock in continued extraction or policy enables capital stranding').

omega_variable(
    geopolitical_sovereignty_versus_climate_survival,
    'Can Arctic nation-states coordinate on extraction limits when Arctic resource control represents strategic sovereignty assertion against peer powers?',
    'Analysis of Arctic Council negotiations on emission limits, resource sharing, and extraction moratoria; tracking of geopolitical Arctic competition (US vs Russia vs China claims); assessment of whether climate agreements override sovereignty competition',
    'If sovereignty dominates: tangled_rope coordination mechanism fails; constraint reverts to snare with enforced asymmetry. If climate crisis overrides: nation-states coordinate exit (scaffold). Otherwise: frozen conflict (piton) with extraction continuing at reduced theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_sovereignty_versus_climate_survival, preference, 'Whether geopolitical sovereignty or climate imperative dominates Arctic policy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arctic_resource_extraction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arctic_tr_t0, arctic_resource_extraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(arctic_tr_t10, arctic_resource_extraction, theater_ratio, 10, 0.48).
narrative_ontology:measurement(arctic_tr_t20, arctic_resource_extraction, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(arctic_be_t0, arctic_resource_extraction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(arctic_be_t10, arctic_resource_extraction, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(arctic_be_t20, arctic_resource_extraction, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arctic_resource_extraction, global_infrastructure).
narrative_ontology:boltzmann_floor_override(arctic_resource_extraction, 0.25).
narrative_ontology:affects_constraint(arctic_resource_extraction, climate_feedback_lock_in).
narrative_ontology:affects_constraint(arctic_resource_extraction, indigenous_sovereignty_and_resource_control).
narrative_ontology:affects_constraint(arctic_resource_extraction, renewable_energy_transition).
narrative_ontology:affects_constraint(arctic_resource_extraction, geopolitical_arctic_competition).

% DUAL FORMULATION NOTE:
% Arctic resource extraction is downstream of multiple structural constraints: climate physics (thermodynamic limits on carbon absorption), geopolitical competition (Arctic sovereignty claims), indigenous rights frameworks (formal vs effective sovereignty), and energy infrastructure (fossil fuel lock-in vs renewable substitution). Each upstream constraint has its own ε value and classification. Arctic extraction represents the institutional synthesis where these constraints interact—the family of constraints should be analyzed as coupled, with network decomposition tracing causal and reinforcing relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(arctic_resource_extraction, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
