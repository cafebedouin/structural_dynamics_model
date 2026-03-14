% ============================================================================
% CONSTRAINT STORY: arctic_resource_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arctic_resource_sovereignty, []).

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
 *   constraint_id: arctic_resource_sovereignty
 *   human_readable: Arctic Resource Sovereignty and Geopolitical Competition
 *   domain: geopolitical/resource_management
 *
 * SUMMARY:
 *   Arctic resource sovereignty represents a structural constraint that
 *   coordinates geopolitical competition between Arctic nation states while
 *   simultaneously extracting from indigenous communities and the global
 *   climate commons. The constraint operates through competing claims to
 *   territorial resource rights, framed within international law (UNCLOS
 *   extended continental shelf provisions, Arctic Council protocols) but
 *   enforced through de facto military capability and capital access. The
 *   coordination function is genuine: Arctic nations benefit from clear
 *   boundary definitions and predictable dispute resolution that prevents
 *   military conflict in a region where nuclear powers are adjacent. The
 *   extraction is equally real: indigenous communities lose decision-making
 *   authority over ancestral territories, and global emissions from Arctic
 *   fossil fuel extraction are concentrated in a climate-critical region
 *   where extraction gains flow to national actors while climate costs are
 *   distributed globally. The theater ratio reflects that Arctic sovereignty
 *   disputes are conducted largely through legal briefs, diplomatic
 *   statements, and scientific claims about continental shelf extent rather
 *   than through institutional enforcement mechanisms. The constraint
 *   exhibits a fundamental tension between coordinating powerful actors
 *   (nation states) while excluding powerless actors (indigenous communities)
 *   from the coordination framework.
 *
 * KEY AGENTS:
 *   - Indigenous Arctic Communities: Primary victims (powerless/trapped) — legally subordinate to national sovereignty claims; cannot exit territorial abandonment
 *   - Arctic Nation States: Primary beneficiaries and coordinators (powerful/constrained) — benefit from stabilized resource access and conflict prevention, constrained by international law and competing claims
 *   - Resource Extraction Companies: Secondary beneficiary (institutional/arbitrage) — benefit from stable sovereignty frameworks; have high exit options to other regions
 *   - Global Climate Commons: Diffuse victim (organized/constrained) — climate impacts from Arctic extraction are concentrated while benefits to extracting nations are localized
 *   - International Governance Institutions: Performative actors (institutional/constrained) — provide legal frameworks (UNCLOS, Arctic Council) but limited enforcement capability
 *   - Analytical Observer: Sees full mixed structure — both genuine coordination and irreducible extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arctic_resource_sovereignty, 0.58).
domain_priors:suppression_score(arctic_resource_sovereignty, 0.65).
domain_priors:theater_ratio(arctic_resource_sovereignty, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arctic_resource_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(arctic_resource_sovereignty, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(arctic_resource_sovereignty, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arctic_resource_sovereignty, tangled_rope).
narrative_ontology:human_readable(arctic_resource_sovereignty, "Arctic Resource Sovereignty and Geopolitical Competition").
narrative_ontology:topic_domain(arctic_resource_sovereignty, "geopolitical/resource_management").

domain_priors:requires_active_enforcement(arctic_resource_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arctic_resource_sovereignty, arctic_nation_states).
narrative_ontology:constraint_beneficiary(arctic_resource_sovereignty, resource_extraction_companies).
narrative_ontology:constraint_victim(arctic_resource_sovereignty, indigenous_arctic_communities).
narrative_ontology:constraint_victim(arctic_resource_sovereignty, global_climate_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS ARCTIC COMMUNITIES (SNARE) — Structurally trapped by colonial sovereignty frameworks that exclude them from resource decisions affecting their territories. Land rights are subordinate to national sovereignty claims. Exit is immaterial — these communities cannot migrate without abandoning ancestral territories. Suppression is near-total: legal frameworks, state enforcement, capital requirements, and epistemic dismissal of traditional knowledge all prevent meaningful participation in resource governance.
constraint_indexing:constraint_classification(arctic_resource_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ARCTIC NATION STATES (TANGLED ROPE) — Experience genuine coordination benefit (managing shared maritime boundaries, preventing conflict escalation) alongside asymmetric extraction (consolidating resource access, excluding other claimants). The Law of the Sea provides coordination rules, but those rules are contested and enforcement relies on de facto military capability. Nation states are constrained by international law but not trapped — they can credibly threaten non-compliance.
constraint_indexing:constraint_classification(arctic_resource_sovereignty, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: RESOURCE EXTRACTION COMPANIES (ROPE) — Experience the constraint as pure coordination: stable sovereignty frameworks enable long-term investment in Arctic infrastructure. Companies benefit from state-enforced property rights and dispute resolution mechanisms. Exit options are high (can shift operations to other regions with equal sovereignty clarity), so the constraint is not extractive toward this agent — it is enabling.
constraint_indexing:constraint_classification(arctic_resource_sovereignty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL CLIMATE INTERESTS (TANGLED ROPE) — Organized actors (climate science communities, environmental NGOs) experience the Arctic sovereignty constraint as both coordination failure and extraction. The constraint coordinates resource access for Arctic states but extracts from global climate by enabling rapid fossil fuel and mineral extraction in a climate-critical region. Suppression is high (economic and geopolitical incentives override climate targets), but not absolute — some mitigation policies are emerging.
constraint_indexing:constraint_classification(arctic_resource_sovereignty, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL GOVERNANCE INSTITUTIONS (PITON) — The UN Convention on the Law of the Sea, Arctic Council, and other institutional frameworks provide formal coordination but are largely performative in managing actual resource conflicts. These institutions persist through bureaucratic inertia and legitimacy theater rather than effective enforcement capability. Theater ratio is high because sovereignty claims are asserted and defended through political statements and legal briefs more than through actual institutional mechanisms.
constraint_indexing:constraint_classification(arctic_resource_sovereignty, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, Arctic sovereignty exhibits genuine coordination functions (preventing military conflict between nuclear powers, enabling efficient resource development) alongside irreducible extraction mechanisms (colonization of indigenous territories, climate externalities imposed on global commons). Both functions are structural, not contingent on policy choices. The classification remains tangled_rope across all time horizons and scopes because the mixed nature is inherent.
constraint_indexing:constraint_classification(arctic_resource_sovereignty, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arctic_resource_sovereignty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arctic_resource_sovereignty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arctic_resource_sovereignty, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arctic_resource_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(arctic_resource_sovereignty, TR),
    TR >= 0.70.

:- end_tests(arctic_resource_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts from indigenous communities (legal exclusion from resource decisions) and from the climate commons (enables rapid fossil fuel extraction in climate-critical region). The extractiveness is not as extreme as a pure snare because nation states genuinely do gain coordination benefits (conflict prevention, predictable maritime boundaries), and the extraction is not maximal because indigenous communities retain some agency (appeals to international law, NGO support, media attention). The rising trajectory from 0.35 to 0.58 reflects accelerating Arctic resource extraction as climate-driven ice loss opens new regions and economic viability increases. Suppression (0.65): High. Multiple mechanisms prevent indigenous participation: legal sovereignty frameworks subordinate indigenous claims, capital requirements for resource development exclude indigenous operators, epistemically dismissive treatment of traditional knowledge, and geopolitical military capacity that indigenous communities cannot match. Suppression is not absolute (some indigenous rights are recognized, some international support exists) but is substantial. Theater ratio (0.55): Moderate. Sovereignty claims are asserted through legal statements (continental shelf submissions to UNCLOS), diplomatic negotiation (Arctic Council statements), and scientific expertise (oceanographic surveys proving extended continental shelf). The institutional machinery appears to be doing real work (UNCLOS rulings, Arctic Council consensus statements), but actual resource allocation is driven more by capital, military capability, and bilateral negotiations than by institutional mechanisms. The theater has been stable rather than rising because institutions have always been somewhat performative in Arctic governance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives reveal a fundamental perspectival gap. Arctic nation states experience the constraint as successful coordination (Rope or Tangled Rope with coordination dominant) — they achieve their core goal of stable resource access and conflict prevention. Indigenous communities experience the same constraint as pure extraction (Snare) — they are excluded from decisions affecting their territories with no exit option. The resource extraction companies experience it as enabling coordination (Rope) because stable property rights are exactly what they need. The global climate commons experiences extraction (Tangled Rope with extraction dominant) because the constraint enables resource extraction that generates climate costs. International institutions experience it as a successful framework (Piton) that provides legitimacy and coordination, but the performative nature means actual resource allocation proceeds independently of institutional mechanics. The analytical observer sees the full mixed structure: coordination for powerful states, extraction for indigenous communities and climate, theater for institutions. No single perspective is complete.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are derived from beneficiary/victim declarations and exit options. Arctic nation states as beneficiaries with constrained exit (cannot easily exit the Arctic region or the need to coordinate there) have moderate directionality (d ≈ 0.40-0.50), producing moderate-high effective extraction because they are both extractors and coordinators. Indigenous communities as victims with trapped exit (cannot exit without abandoning territories) have maximum directionality (d ≈ 0.95), producing maximum experienced extraction. Resource companies as beneficiaries with arbitrage exit have low directionality (d ≈ 0.10), producing negative effective extraction (the constraint is enabling for them). The climate commons as a diffuse victim with constrained exit has high directionality (d ≈ 0.80). The analytical observer's canonical d (0.73) produces the baseline for tangled_rope classification at civilizational scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the 'coordination function' and 'extraction mechanism' are not alternatives but simultaneous. The constraint coordinates nation states (genuine gain from conflict prevention) while extracting from indigenous communities (genuine loss from decision exclusion). Both are structural; neither is reducible to the other. The challenge is not 'is this Rope or Snare?' but 'for whom is it Rope and for whom is it Snare?' The answer is that it is Tangled Rope viewed from the nation state and analytical perspectives, but Snare from the indigenous community perspective, and the mixture is not resolvable by choosing a different measurement methodology — it reflects the actual asymmetry of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_framework_inevitability,
    'Is the Westphalian sovereignty framework an immutable feature of large-scale geopolitical coordination, or is it a contingent institutional choice that could be replaced?',
    'Historical analysis of alternative governance systems; theoretical exploration of post-sovereign coordination mechanisms; empirical assessment of whether alternative frameworks have managed comparable resource and security challenges without sovereignty-based extraction',
    'If inevitable: the extraction of indigenous communities is an unavoidable cost of geopolitical stability (mountain-adjacent). If contingent: the constraint is a choice by powerful actors (snare from indigenous perspective fully justified).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_framework_inevitability, conceptual, 'Whether Westphalian sovereignty is immutable or contingent').

omega_variable(
    indigenous_coalitional_power,
    'Can indigenous Arctic communities organize sufficient collective power (through transnational networks, economic pressure, or climate alliances) to shift from trapped to constrained exit options?',
    'Longitudinal tracking of indigenous coalition-building; correlation between organizational capacity and policy concessions; analysis of successful indigenous governance models elsewhere (New Zealand, Canada treaty frameworks)',
    'If yes: trapped classification becomes constrained, snare becomes tangled_rope, and the extractiveness floor rises (indigenous power dampens extraction). If no: trapped persists as structural, snare classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_coalitional_power, empirical, 'Whether indigenous communities can achieve coalitional power').

omega_variable(
    climate_constraint_dominance_timeline,
    'At what point does climate constraint (Arctic ice loss making resources inaccessible or extraction economically unfeasible) overwhelm the sovereignty-based coordination and extraction mechanisms?',
    'Modeling of ice extent vs. extraction feasibility; tracking of carbon pricing and climate policy stringency; analysis of when climate-driven scarcity replaces sovereignty-driven competition',
    'If timeline < 20 years: Arctic sovereignty constraint becomes moot (replaced by environmental constraint). If timeline > 50 years: current sovereignty regime persists through multiple generations. Timing determines which perspective''s time horizon is most predictive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_constraint_dominance_timeline, empirical, 'Timeline for climate constraints to dominate sovereignty mechanisms').

omega_variable(
    extraction_asymmetry_measurement,
    'How much of the Arctic nations'' benefit derives from coordination (genuine shared gain from conflict prevention) vs. extraction (rent captured from indigenous displacement and resource monopoly)?',
    'Counterfactual analysis: compare actual resource distribution with hypothetical distribution under indigenous co-governance; calculate economic value of prevented conflict vs. value of monopoly extraction',
    'If coordination dominates (>60% of benefit): tangled_rope classification is appropriate. If extraction dominates (>60% of benefit): classification shifts toward snare-adjacent for nation states.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_asymmetry_measurement, empirical, 'Ratio of coordination benefit to extraction benefit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arctic_resource_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arctic_tr_t0, arctic_resource_sovereignty, theater_ratio, 0, 0.4).
narrative_ontology:measurement(arctic_tr_t15, arctic_resource_sovereignty, theater_ratio, 15, 0.48).
narrative_ontology:measurement(arctic_tr_t30, arctic_resource_sovereignty, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(arctic_be_t0, arctic_resource_sovereignty, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(arctic_be_t15, arctic_resource_sovereignty, base_extractiveness, 15, 0.47).
narrative_ontology:measurement(arctic_be_t30, arctic_resource_sovereignty, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arctic_resource_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(arctic_resource_sovereignty, indigenous_land_rights_recognition).
narrative_ontology:affects_constraint(arctic_resource_sovereignty, climate_mitigation_carbon_budgets).
narrative_ontology:affects_constraint(arctic_resource_sovereignty, arctic_military_escalation).

% DUAL FORMULATION NOTE:
% Arctic resource sovereignty is a parent constraint that shapes multiple downstream constraints: indigenous land rights disputes depend on the sovereignty framework's treatment of indigenous territorial claims; carbon budget allocation is affected by which actors have sovereignty over extraction; military escalation risk is driven by competing sovereignty claims. Each downstream constraint can be decomposed with its own ε value reflecting specific empirical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(arctic_resource_sovereignty, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
