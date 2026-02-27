% ============================================================================
% CONSTRAINT STORY: intertemporal_responsibility_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intertemporal_responsibility_gap, []).

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
 *   constraint_id: intertemporal_responsibility_gap
 *   human_readable: The Generational Accountability Void
 *   domain: environmental/economic/technological
 *
 * SUMMARY:
 *   The intertemporal responsibility gap is a structural constraint where
 *   benefits of resource extraction, carbon emissions, and debt accumulation
 *   are concentrated in the present (captured by current industries,
 *   consumers, and financial institutions), while costs and systemic risks
 *   are deferred to future generations and ecosystems. This creates a
 *   fundamental asymmetry: decision-makers who profit from extraction will
 *   not bear the consequences; those who bear the consequences have no voice
 *   in current decisions. The constraint exhibits classical snare
 *   characteristics — high extractiveness (0.68), high suppression (0.72
 *   reflecting information asymmetries, discounting mechanisms, and
 *   institutional silencing of future-focused voices), and increasing theater
 *   (0.65 reflecting the prevalence of climate pledges and net-zero targets
 *   that substitute for actual extraction reduction). The beneficiaries are
 *   current extractive industries, immediate consumers, and debt-financed
 *   institutions; the primary victims are future generations, non-human
 *   ecosystems, and climate/hydrological stability. The constraint is
 *   maintained through multiple suppression mechanisms: (1) Discount rates
 *   that render future costs negligible in present-value calculations, (2)
 *   Short institutional time horizons (political cycles, corporate earnings
 *   cycles), (3) Diffuse and temporally distant accountability (future actors
 *   cannot sue present decision-makers), (4) Externality pricing failures
 *   (atmospheric carbon, biodiversity, soil depletion not priced into market
 *   decisions), and (5) Epistemic suppression (uncertainty about precise
 *   future harms used to justify present inaction).
 *
 * KEY AGENTS:
 *   - Future Generations: Primary victim (powerless/trapped) — no voice in present decisions; bear irreversible consequences
 *   - Ecosystems & Species: Primary victim (non-agent) — face extinction and collapse with no escape mechanism
 *   - Climate & Hydrological Systems: Primary victim (non-agent) — accumulating carbon forcing and water stress; physically irreversible
 *   - Extractive Industries (fossil fuels, rare earth mining, deforestation): Primary beneficiary (institutional/arbitrage) — capture price premium during extraction window
 *   - Current Consumers & Corporations: Primary beneficiary (moderate/mobile) — benefit from cheap energy and resources; can exit through portfolio diversification
 *   - Debt-Financed Institutions (governments, corporations): Primary beneficiary (institutional/arbitrage) — benefit from present consumption financed by future taxation/resource depletion
 *   - Current Vulnerable Populations: Secondary victim (moderate/constrained) — depend on extractive industries for income but suffer disproportionate climate harms
 *   - Policy & Governance Institutions: Institutional mediator (institutional/constrained) — maintain performative accountability (theater) while lacking enforcement mechanisms
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing temporal asymmetry as inherent to causality rather than engineering flaw
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intertemporal_responsibility_gap, 0.68).
domain_priors:suppression_score(intertemporal_responsibility_gap, 0.72).
domain_priors:theater_ratio(intertemporal_responsibility_gap, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intertemporal_responsibility_gap, extractiveness, 0.68).
narrative_ontology:constraint_metric(intertemporal_responsibility_gap, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(intertemporal_responsibility_gap, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intertemporal_responsibility_gap, snare).
narrative_ontology:human_readable(intertemporal_responsibility_gap, "The Generational Accountability Void").
narrative_ontology:topic_domain(intertemporal_responsibility_gap, "environmental/economic/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intertemporal_responsibility_gap, current_extractive_industries).
narrative_ontology:constraint_beneficiary(intertemporal_responsibility_gap, immediate_consumers).
narrative_ontology:constraint_beneficiary(intertemporal_responsibility_gap, debt_financed_institutions).
narrative_ontology:constraint_victim(intertemporal_responsibility_gap, future_generations).
narrative_ontology:constraint_victim(intertemporal_responsibility_gap, ecosystems_and_species).
narrative_ontology:constraint_victim(intertemporal_responsibility_gap, climate_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) — Cannot exit; bears full cost of carbon debt, biodiversity loss, ecosystem collapse, and climate destabilization. No voice in current extraction decisions. Trapped by physical irreversibility (carbon atmosphere persistence, species extinction, soil degradation). d≈0.98, f(d)≈1.48, σ=1.2 → χ≈0.96.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CURRENT VULNERABLE POPULATIONS (TANGLED ROPE) — Constrained by economic dependency on extractive industries (fossil fuels, debt-financed infrastructure); also suffer disproportionate harms from climate impacts and pollution. Limited exit options but some coordination benefit through national climate policies and transition programs. d≈0.72, f(d)≈1.13, σ=1.0 → χ≈0.77.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXTRACTIVE INDUSTRIES & FINANCIAL INSTITUTIONS (ROPE) — Benefits from coordination logic of market access and capital flows; experiences the constraint as enabling coordination: fossil fuel extraction enables energy infrastructure, debt financing enables development. Arbitrage exit (can shift to alternative markets/sectors). d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.06. Net beneficiary; sees pure coordination.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLIMATE & ECOSYSTEM SYSTEMS (SNARE) — Non-agent victim with no escape mechanism. Accumulating carbon forcing, ocean acidification, biodiversity collapse, soil depletion are structurally irreversible over relevant timescales. Theater_ratio characterizes the constraint as performative (climate pledges, net-zero targets without corresponding extraction reduction). d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.92.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL GOVERNANCE & POLICY INSTITUTIONS (PITON) — Maintains performative accountability mechanisms (COP agreements, pledges, reporting) while extraction continues beneath them. Theater_ratio=0.65 reflects substantial gap between stated climate targets and actual emission reductions. Institutions persist through inertia — maintaining the appearance of coordination while lacking enforcement capacity. d≈0.55, f(d)≈0.75, σ=1.1 → χ≈0.48.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TEMPORAL PHYSICS VIEW (MOUNTAIN) — From a universal/civilizational perspective, the asymmetry between present action and future consequence appears as a fundamental feature of thermodynamics and causality: the arrow of time makes all extraction future-mortgaging. This perspective risks naturalizing what is actually a contingent institutional failure (inability to price future costs). However, the structural data (ε=0.68, suppression=0.72) contradicts true mountain status — the constraint is engineered, not inherent.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intertemporal_responsibility_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intertemporal_responsibility_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intertemporal_responsibility_gap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(intertemporal_responsibility_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(intertemporal_responsibility_gap, TR),
    TR >= 0.70.

:- end_tests(intertemporal_responsibility_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from future generations and ecosystems (primary victims) to benefit current extractive industries and consumers. The extraction is severe because future actors have zero exit options and zero voice in decisions. Trajectory shows acceleration: starting from 0.35 (lower extraction in 1970s when environmental awareness was emerging) to 0.68 (current state with entrenched fossil fuel infrastructure and debt dependencies). Suppression (0.72): High. Multiple institutional mechanisms suppress alternatives and silence future-oriented voices: (a) Discount rate conventions mathematically erase future costs, (b) Political cycles and earnings cycles create myopia, (c) Information asymmetries about climate impacts, (d) Externality pricing failures, (e) Diffuse accountability (no defendant for future harms). Theater ratio (0.65): Moderate-high. The past 50 years have seen proliferation of climate pledges, net-zero targets, COP agreements, and ESG frameworks that create appearance of accountability while actual emissions continue rising. Theater has increased from 0.25 (1970s, little acknowledgment of problem) to 0.65 (2020s, extensive performative commitments with weak enforcement). Claimed type: SNARE. High extractiveness (0.68), high suppression (0.72), and irreversible harms to powerless/trapped victims.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a radical perspectival gap between beneficiary and victim views. Extractive industries and current consumers see the constraint as enabling coordination (rope) — they are solving legitimate problems of energy, resources, and development. Future generations see pure extraction (snare) — they are trapped by decisions made without their participation, bearing costs they did not incur. The open-science/climate coalition sees a temporary problem with a sunset clause (scaffold) — believing that renewable energy and circular economy will eventually replace extractive dependence. International policy institutions see a degraded accountability mechanism (piton) — they maintain the rituals of climate negotiation and pledging while lacking the enforcement power to redirect extraction. Current vulnerable populations see a mixed system (tangled rope) — they depend on extractive industries for income but suffer disproportionate harms from climate impacts and pollution. The analytical observer risks seeing temporal asymmetry as a law of physics (mountain) — the arrow of time makes all present action future-mortgaging — but this naturalizes what is actually an institutional failure (inability to price future costs into present decisions).
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations: Victim + trapped → d≈0.98, f(d)≈1.48. Maximum extraction. Powerless, no voice, no exit option. Ecosystems/species: Non-agent victim + irreversible collapse → d≈0.95, f(d)≈1.42. Effectively maximum extraction. Extractive industries: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. Can exit by shifting to alternative markets. Current consumers: Beneficiary + mobile → d≈0.15, f(d)≈-0.01. Weak beneficiary; can exit by switching to alternatives (if available and affordable). Vulnerable populations: Victim + constrained → d≈0.72, f(d)≈1.13. Significant extraction but some coordination benefit through employment and development. Policy institutions: Institutional + constrained → d≈0.55, f(d)≈0.75. Piton classification from theater gate (0.65 ≥ 0.70 threshold not quite met, but trajectory trending toward full piton). Analytical observer: Analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival (observer naturalizes temporal asymmetry); false summit detector identifies this as engineering rather than inherent.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint exhibits clear snare characteristics across the primary perspectives (future generations, ecosystems, vulnerable populations all see high extractiveness with no coordination benefit), eliminating the risk that this is mislabeled pure coordination. The coordination logic — that present extraction enables energy, resources, and development — is real but asymmetrically distributed: those who benefit from the coordination are not those who bear the costs. This is the defining feature of a snare: coordination logic in the present (industries solve resource problems) married to extraction in the intertemporal dimension (future generations bear costs of pollution, climate, depletion). The theater ratio (0.65) is lower than the snare threshold but trending upward, indicating increasing performativity of climate commitments without corresponding reduction in extraction. The mandatrophy is resolved by noting that the constraint cannot be reframed as pure coordination — the temporal asymmetry is intrinsic, not perspectival.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discount_rate_normativity,
    'Is the discount rate that renders future costs negligible a mathematical artifact or a value choice embedded in economic institutions?',
    'Comparative analysis of discount rate assumptions across present-value calculations for long-term infrastructure vs. climate damage; philosophical grounding of temporal preference in agent utility vs. intergenerational justice frameworks',
    'If mathematical artifact: extraction is a genuine problem of valuation (snare confirmed). If value choice: extraction can be re-institutionalized with different discount rates — moves classification toward tangled_rope or scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discount_rate_normativity, preference, 'Whether discount rates reflect inherent time preference or institutional values').

omega_variable(
    irreversibility_threshold,
    'What carbon concentration, species extinction rate, or soil depletion level triggers irreversible ecosystem collapse?',
    'Paleoclimate reconstruction of tipping points; ecological modeling of biodiversity loss thresholds; soil science studies of degradation recovery timescales',
    'If already crossed: classification shifts from snare (future harm) to mountain (current/irreversible harm). If avoidable: snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_threshold, empirical, 'Identification of irreversibility thresholds in climate and ecosystems').

omega_variable(
    institutional_time_horizon_mismatch,
    'Can institutional decision-makers internalize costs beyond their own tenure or liability window? Is this a structural capacity limit or a governance failure?',
    'Longitudinal analysis of institutional investment horizons; case studies of long-term infrastructure projects (nuclear waste storage, aquifer depletion); comparison of present-day discount rates to historical intergenerational transfer practices',
    'If structural limit: extraction is inherent to finite-lived institutions (scaffold perspective). If governance failure: better institutional design can align incentives across generations (tangled_rope with active enforcement potential).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_time_horizon_mismatch, conceptual, 'Whether institutional time horizon mismatch is inherent or remediable').

omega_variable(
    future_substitution_possibility,
    'Can technological innovation sufficiently substitute for depleted natural capital, or are some losses fundamentally irreplaceable?',
    'Cost-benefit analysis of carbon capture vs. emissions reduction; studies of ecosystem service valuation; technology roadmaps for green energy/circular economy',
    'If substitution possible: extraction severity is reduced (less snare, more tangled_rope). If irreplaceable: snare classification and maximum extraction severity confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_substitution_possibility, empirical, 'Feasibility of technological substitution for natural capital').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intertemporal_responsibility_gap, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(itrg_tr_t0, intertemporal_responsibility_gap, theater_ratio, 0, 0.25).
narrative_ontology:measurement(itrg_tr_t50, intertemporal_responsibility_gap, theater_ratio, 50, 0.45).
narrative_ontology:measurement(itrg_tr_t100, intertemporal_responsibility_gap, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(itrg_be_t0, intertemporal_responsibility_gap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(itrg_be_t50, intertemporal_responsibility_gap, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(itrg_be_t100, intertemporal_responsibility_gap, base_extractiveness, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intertemporal_responsibility_gap, resource_allocation).
narrative_ontology:affects_constraint(intertemporal_responsibility_gap, carbon_price_discounting_mechanism).
narrative_ontology:affects_constraint(intertemporal_responsibility_gap, discount_rate_normativity).
narrative_ontology:affects_constraint(intertemporal_responsibility_gap, biodiversity_loss_externality).
narrative_ontology:affects_constraint(intertemporal_responsibility_gap, sovereign_debt_trajectory).

% DUAL FORMULATION NOTE:
% The intertemporal responsibility gap decomposes into multiple subordinate constraints: the mechanisms by which discount rates render future costs negligible, the pricing failures that externalize carbon/biodiversity costs, and the institutional time horizon mismatches that prevent long-term planning. Each has its own ε and classification; this story is the overarching structural constraint linking them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(intertemporal_responsibility_gap, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
