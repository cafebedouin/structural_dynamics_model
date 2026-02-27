% ============================================================================
% CONSTRAINT STORY: yangtze_fishing_ban
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_yangtze_fishing_ban, []).

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
 *   constraint_id: yangtze_fishing_ban
 *   human_readable: Yangtze River Permanent Fishing Ban
 *   domain: economic/environmental/political
 *
 * SUMMARY:
 *   The Yangtze River Fishing Ban, implemented in 2020-2021 across the entire
 *   6,300-kilometer river system, represents one of the world's largest
 *   ecological interventions — permanently eliminating artisanal and
 *   small-scale commercial fishing to restore biodiversity after decades of
 *   unsustainable extraction. The constraint exhibits a fundamental
 *   distributional tension: the ecological commons is collapsing and requires
 *   immediate intervention, but the institutional solution concentrates
 *   extraction costs on the poorest and most vulnerable populations (2-3
 *   million artisanal fishers and family dependents) while distributing
 *   benefits broadly across society and future generations. The ban
 *   classifies as Tangled Rope from the analytical baseline: it solves a
 *   genuine coordination problem (fishery collapse) but does so through
 *   mechanisms that extract heavily from localized, powerless populations.
 *   The structural gap between ecological necessity and distributional
 *   fairness creates divergent perspectives where the same policy appears as
 *   pure restoration (central government view), pure predation (fishing
 *   household view), or contingent institutional arrangement (livelihood
 *   program view). The theater ratio has increased slightly over the decade,
 *   reflecting that enforcement has shifted from initial strict
 *   implementation toward more performative compliance measurements and
 *   symbolic enforcement in some regions while enforcement effort
 *   concentrates in high-visibility areas.
 *
 * KEY AGENTS:
 *   - Artisanal Fishing Households: Primary victims (powerless/trapped) — 2-3 million people losing permanent occupation; face biographical timeline extraction with no exit options
 *   - Fishing Boat Operators: Primary victims (moderate/constrained) — fleet owners with some capital but constrained re-employment options; can relocate but at significant cost
 *   - Fish Processing Workers: Secondary victims (moderate/constrained) — factory workers dependent on fishing supply chains; some mobility through factory re-location but constrained by family ties
 *   - Rural County Governments: Institutional implementer (moderate/constrained) — charged with enforcement while managing constituent backlash; benefit from central compensation but constrained by legitimacy costs
 *   - Central Government / Environmental Ministry: Primary beneficiary (institutional/arbitrage) — can implement policy with political backing and exit if necessary; perceives constraint as pure coordination
 *   - Livelihood Transition Programs: Organized partial beneficiary (organized/constrained) — NGOs and provincial agencies implementing compensation; see sunset clause in transition pathways
 *   - Industrial Aquaculture & Agricultural Users: Secondary beneficiary (powerful/constrained) — benefit from water quality and fish stock recovery but face weak enforcement on their own pollution; constraint enforcement is performative from their perspective
 *   - Future Generations / Yangtze Ecosystem: Ultimate beneficiary (analytical/analytical) — abstract future benefits from ecosystem restoration; cannot organize or advocate in present decision-making
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(yangtze_fishing_ban, 0.58).
domain_priors:suppression_score(yangtze_fishing_ban, 0.72).
domain_priors:theater_ratio(yangtze_fishing_ban, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(yangtze_fishing_ban, extractiveness, 0.58).
narrative_ontology:constraint_metric(yangtze_fishing_ban, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(yangtze_fishing_ban, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(yangtze_fishing_ban, tangled_rope).
narrative_ontology:human_readable(yangtze_fishing_ban, "Yangtze River Permanent Fishing Ban").
narrative_ontology:topic_domain(yangtze_fishing_ban, "economic/environmental/political").

domain_priors:requires_active_enforcement(yangtze_fishing_ban).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(yangtze_fishing_ban, future_generations).
narrative_ontology:constraint_beneficiary(yangtze_fishing_ban, riverine_ecosystem).
narrative_ontology:constraint_beneficiary(yangtze_fishing_ban, downstream_water_users).
narrative_ontology:constraint_victim(yangtze_fishing_ban, artisanal_fishing_communities).
narrative_ontology:constraint_victim(yangtze_fishing_ban, fishing_boat_operators).
narrative_ontology:constraint_victim(yangtze_fishing_ban, fish_processing_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FISHING HOUSEHOLD (SNARE) — Artisanal fishing families with 2-4 generations of river dependence face a permanent livelihood ban with no economically viable alternative in rural river counties. Exit options are structurally trapped: relocation to urban labor markets requires skills capital they lack, welfare payments are insufficient for subsistence, and 're-skilling' programs are under-resourced. The constraint extracts their occupation, their cultural identity, and their access to the commons they inhabited. Maximum structural extraction with high suppression — alternatives exist in theory only.
constraint_indexing:constraint_classification(yangtze_fishing_ban, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RURAL COUNTY GOVERNMENT (TANGLED ROPE) — County administrations benefit from the ban through central government compensation (subsidy transfers, ecological credits, political performance metrics favoring environmental compliance) but are constrained by enforcement burden and legitimacy costs. They face simultaneous coordination (implementing central mandate) and extraction (taking resources from constituent fishing populations to satisfy national policy). Suppression is high (police enforcement, boat confiscation) but benefits are real (compensation funds, cadre promotions for compliance). Mixed experience: coordination tool for national policy + extraction mechanism for local populations.
constraint_indexing:constraint_classification(yangtze_fishing_ban, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CENTRAL GOVERNMENT (ROPE) — The Ministry of Agriculture and Rural Affairs and Ministry of Ecology view the ban as pure coordination: solving the collective action problem of fishery commons collapse. The constraint enables coordinated transition from unsustainable extraction to ecosystem restoration with measurable fish stock recovery. Exit options are present (arbitrage: withdraw the ban if costs rise or political winds shift). Experiences the ban as coordination mechanism solving a genuine tragedy of the commons. No experienced extraction from this structural position — benefits are clear (restored fisheries, avoided ecosystem collapse, international environmental credibility).
constraint_indexing:constraint_classification(yangtze_fishing_ban, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LIVELIHOOD TRANSITION PROGRAM OPERATORS (SCAFFOLD) — NGOs, provincial governments, and training programs implementing the compensation/re-skilling side of the ban see it as a temporary coordination problem with an implicit sunset: once alternative livelihoods are established and fish stocks recover, the extraction burden on fishing communities should decline. Theater ratio is moderate (some training programs function; many are performative check-boxes). The scaffold has a structural exit path: if alternative employment materializes and compensation adequacy increases, the constraint's extraction force diminishes. Current status: mid-transition, sunset timeline contested (5-10 years in policy documents, 15-20 years in actual program outcomes).
constraint_indexing:constraint_classification(yangtze_fishing_ban, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INDUSTRIAL AQUACULTURE & DOWNSTREAM USERS (PITON) — Agricultural users dependent on Yangtze water quality nominally benefit from the ban, but the constraint on their actual behavior is minimal — they face weak enforcement of pollution controls, siltation management, and water allocation. The ban is largely performative from this perspective: it restricts artisanal fishing (low-cost target) while failing to address industrial pollution, dam operations, and agricultural runoff (high-cost targets). Theater ratio is high; functional extraction of artisanal fishers continues while industrial externalities are left largely unresolved. The piton classification reflects that the constraint's primary function (ecosystem restoration) persists in the language and policy framing but its actual enforcement has atrophied toward capturing small-scale fishers while leaving large-scale degradation mechanisms untouched.
constraint_indexing:constraint_classification(yangtze_fishing_ban, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational timescale, the ban reflects an irreducible natural constraint: the Yangtze's fish stocks cannot sustain industrial-scale extraction without biological collapse. At this scale, the 'ban' is merely humanity's recognition of an immutable ecological limit — the carrying capacity floor. Overfishing violates a structural natural law. However, the base properties contradict the mountain classification: suppression (0.72), extractiveness (0.58), and the requirement for active enforcement all indicate this is not a natural law but a contingent institutional arrangement responding to policy choices about resource allocation. The engine's false summit detection reveals that the 'immutable ecological limit' framing naturalizes what is actually a political economy problem: WHO bears the costs of restoration, and whether those costs are distributed equitably or concentrated on powerless populations.
constraint_indexing:constraint_classification(yangtze_fishing_ban, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(yangtze_fishing_ban_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(yangtze_fishing_ban, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(yangtze_fishing_ban, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(yangtze_fishing_ban, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(yangtze_fishing_ban, TR),
    TR >= 0.70.

:- end_tests(yangtze_fishing_ban_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts permanent livelihoods from artisanal fishing populations with compensation that is politically declared but functionally insufficient. Household income replacement for fishing families averages 40-60% of pre-ban fishing income according to provincial reports; actual outcomes are contested. The extraction is real and concentrated, but not total — some households receive compensation, some find alternative employment, and some occupy ambiguous categories (part-time fishers, processors who find new work). Suppression (0.72): High. Structural barriers include boat confiscation, permit revocation, criminal penalties for illegal fishing, restricted access to river infrastructure, and limited alternative livelihood options in rural river counties. Alternative employment requires skills capital and geographic mobility that most artisanal fishers lack. Career paths in manufacturing, services, or agriculture require 5-10 year re-training cycles with opportunity costs borne by households. Suppression is not absolute (informal fishing persists, some transitions occur) but is severe. Theater ratio (0.48): Moderate. The constraint has substantial functional content — fish stocks are genuinely recovering and enforcement is material (over 280,000 fishing boats decommissioned). However, theater is present: compliance metrics emphasize symbolic enforcement (high-profile arrests, boat destruction ceremonies) while enforcement intensity varies by region and political cycles; compensation programs include performative 're-skilling' in sectors without actual job openings; environmental monitoring focuses on metrics favorable to the policy (fish species counts) while under-measuring industrial pollution and dam impacts that would complicate the narrative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a maximal perspectival divergence. From the artisanal fishing household perspective, the ban is a Snare: a coercive elimination of their livelihood with insufficient compensation and no genuine exit options. From the central government perspective, it is pure Rope: coordinating a solution to unsustainable commons exploitation with clear ecological benefits. From the rural county government perspective, it is Tangled Rope: simultaneously implementing central coordination mandate while extracting from local populations and bearing enforcement costs. From the livelihood transition program perspective, it is Scaffold: a temporary coordination problem with an implicit sunset once alternative employment materializes (though the sunset timeline is highly contested). From the industrial aquaculture/pollution perspective, it is Piton: performative ecological policy that restricts artisanal fishing (easy enforcement target) while leaving industrial extraction mechanisms largely untouched. The analytical observer risks a Mountain perspective (ecological collapse is immutable natural law) but this is a false summit — the constraint is fundamentally a distributional problem about WHO bears ecological restoration costs, not an inevitability of nature. The engine's false summit detector should flag this naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint operates through radically asymmetric structural positions. Artisanal fishing households are trapped (no exit) with powerless status — they derive no benefit from the ban and bear maximum cost. Their derived directionality d approaches 1.0 (full target), producing experienced extractiveness χ near maximum (f(d) ≈ 1.42 at the sigmoid ceiling). Rural county governments are moderate power with constrained exit — they receive central compensation for implementation costs but face legitimacy erosion among constituent fishers. Their derived d is around 0.60-0.70, producing moderate χ. The central government is institutional with arbitrage exit — they can adjust or withdraw the policy, and they perceive genuine benefits (ecological restoration, international credibility). Their derived d is near 0.0 (beneficiary), producing negative or near-zero χ. The analytical observer at the civilizational scale has d around 0.50 (symmetric: costs to current fishers, benefits to future ecosystem users), producing moderate χ. The perspectival gap is the entire range: full extraction experienced by victims, pure coordination experienced by beneficiaries, mixed experience by intermediate implementers.
 *
 * MANDATROPHY ANALYSIS:
 *   The Yangtze fishing ban resolves mandatrophy by making the distributional conflict explicit. The mandate to restore Yangtze ecosystems is not in question — ecological necessity is genuine. The structural tension is how restoration is implemented: (A) as pure coordination with costs distributed across society and future generations (Rope perspective), or (B) as extraction concentrated on artisanal fishing populations with compensation theater masking permanent impoverishment (Snare/Tangled Rope perspective). The constraint's high extractiveness (0.58) and suppression (0.72) prevent misclassification as pure Rope. The requirement for active enforcement, beneficiary/victim declarations, and theater-ratio tracking establish that this is not a natural law but a policy choice about resource distribution. The false summit detector identifies the 'immutable ecological necessity' framing as a political move that naturalizes contingent implementation choices. A genuine Rope version of this constraint would exist if: (1) alternative livelihoods were actually sufficient (compensation + employment providing 90%+ of pre-ban household income), (2) enforcement were distributed across industrial and artisanal sectors proportionally, and (3) theater ratio remained below 0.40. The current constraint fails these gates, confirming Tangled Rope as the baseline classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    livelihood_transition_sufficiency,
    'Do the implemented compensation and re-skilling programs provide economically viable alternatives for fishing households, or do they constitute theater masking permanent impoverishment?',
    '5-year and 10-year longitudinal household income tracking; comparison of re-trained worker earnings to pre-ban fishing household incomes; employment uptake rates and wage sustainability in re-skilling sectors',
    'If sufficient: constraint transitions to Scaffold with real sunset. If insufficient: constraint remains Snare/Tangled Rope with extraction perpetuated indefinitely despite policy language claiming support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(livelihood_transition_sufficiency, empirical, 'Whether livelihood transition programs provide viable alternatives or mask permanent impoverishment').

omega_variable(
    enforcement_distributional_capture,
    'Is enforcement concentrated on artisanal fishing (low-cost enforcement target) while industrial pollution, dam operations, and agricultural pollution remain largely uncontrolled?',
    'Comparative enforcement metrics: confiscations per boat-hour for artisanal vs industrial vessels; pollution violation citation rates for agricultural runoff vs fishing activity; dam operation compliance vs fishing ban compliance; spatial analysis of enforcement presence in artisanal zones vs industrial zones',
    'If concentrated: piton classification confirmed — performative ecological policy targeting powerless actors while leaving structural degradation mechanisms untouched. If distributed: genuine ecological restoration constraint, snare perspective is misattributed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_distributional_capture, empirical, 'Whether enforcement targets artisanal fishing disproportionately to industrial pollution sources').

omega_variable(
    fish_stock_recovery_trajectory,
    'Are Yangtze fish stocks recovering at rates consistent with artisanal fishing removal alone, or do recovery trajectories require additional industrial pollution controls and dam removal interventions?',
    'Longitudinal fish species population genetics and catch surveys; decomposition of recovery drivers (artisanal fishing removal vs industrial pollution reduction vs water management changes); counterfactual modeling of recovery with vs without complementary industrial controls',
    'If artisanal ban is sufficient: constraint is genuine ecosystem restoration mechanism (Rope/Mountain view justified). If additional controls needed: piton classification confirmed — ban is necessary but insufficient and conceals need for costly industrial-sector interventions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fish_stock_recovery_trajectory, empirical, 'Whether fish stock recovery depends solely on artisanal fishing ban or requires industrial pollution controls').

omega_variable(
    political_capture_by_development_interests,
    'Has the ban''s implementation become instrumentalized to favor large-scale aquaculture operators, dam developers, or hydropower interests over ecological restoration?',
    'Institutional analysis of compensation fund distribution; tracking of aquaculture permits and industrial water rights granted post-ban; interviews with policy implementers about trade-offs between artisanal livelihood protection and industrial development',
    'If captured: constraint is Tangled Rope concealing structural advantage for industrial actors. If not captured: genuine coordination constraint with real beneficiary diversity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_capture_by_development_interests, conceptual, 'Whether ban implementation favors large-scale aquaculture and industrial interests over ecology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(yangtze_fishing_ban, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(yrfb_tr_t0, yangtze_fishing_ban, theater_ratio, 0, 0.35).
narrative_ontology:measurement(yrfb_tr_t5, yangtze_fishing_ban, theater_ratio, 5, 0.42).
narrative_ontology:measurement(yrfb_tr_t10, yangtze_fishing_ban, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(yrfb_be_t0, yangtze_fishing_ban, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(yrfb_be_t5, yangtze_fishing_ban, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(yrfb_be_t10, yangtze_fishing_ban, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(yangtze_fishing_ban, resource_allocation).
narrative_ontology:affects_constraint(yangtze_fishing_ban, aquaculture_expansion_china).
narrative_ontology:affects_constraint(yangtze_fishing_ban, yangtze_dam_cascade_operations).
narrative_ontology:affects_constraint(yangtze_fishing_ban, rural_income_inequality_china).

% DUAL FORMULATION NOTE:
% The fishing ban decomposes into at least two structurally distinct claims: (1) ecological restoration necessity (high epistemic confidence, low ε ≈ 0.15), which is Mountain or Rope depending on implementation; (2) the specific implementation through artisanal fishing elimination with compensation theater (contested, high ε ≈ 0.58), which is Tangled Rope from baseline. These stories share a constraint family relationship: implementation choices drive extractiveness. The upstream ecological claim (fish stock collapse) structures the downstream implementation claim (whose livelihoods absorb restoration costs).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(yangtze_fishing_ban, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
