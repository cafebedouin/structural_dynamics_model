% ============================================================================
% CONSTRAINT STORY: preparedness_persistence_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence_flat_control, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_persistence_flat_control
 *   human_readable: Flood Preparedness as Sustained Institutional Commitment Across Generations Without Catastrophe Reinforcement
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   Flood preparedness as sustained institutional commitment across
 *   generations without catastrophe reinforcement represents a canonical
 *   commitment system failure mode: the gap between disasters exceeds the
 *   institutional memory horizon, and the beneficiaries of preparedness
 *   (future generations who avoid the flood) are temporally distant from the
 *   cost-bearers (current taxpayers funding invisible infrastructure). The
 *   constraint exhibits scaffold characteristics at the system design level —
 *   preparedness is meant to be transitional, building capacity that
 *   internalizes into routine civil infrastructure and private risk pricing.
 *   However, the measurements show theater_ratio rising from 0.35 to 0.68
 *   over a 30-year gap, indicating that the functional components atrophy
 *   while performative activities (hazard mapping, insurance administration,
 *   disaster planning) persist through institutional inertia. The analytical
 *   observer risks naturalizing this commitment gap as an immutable feature
 *   of human temporal discounting, but the structural data reveals
 *   identifiable beneficiaries (civil engineering bureaus maintaining stable
 *   budgets, emergency management agencies justifying existence) and
 *   correctable design failures (no automatic review triggers, no liability
 *   extending across administrations, no financial instruments making future
 *   costs present). This is not a law of nature — it is a piton in formation,
 *   a scaffold whose sunset clause was never operationalized.
 *
 * KEY AGENTS:
 *   - Current Taxpayers: Primary victims (powerless/trapped at immediate horizon) — bear ongoing cost with no experiential confirmation of need; maximum extraction during low-salience periods
 *   - Future Generations: Primary beneficiaries (powerless/trapped but in future) — avoided flood damage, but temporally distant and non-legible in current resource allocation
 *   - Civil Engineering Bureaus: Institutional beneficiaries (institutional/arbitrage) — sustained budgets and professional continuity; genuine coordination function but also extraction through mandate persistence
 *   - Emergency Management Agencies: Organized actors (organized/constrained) — building transitional structures with scaffold logic, but facing pressure to maintain permanent apparatus
 *   - Adjacent Land Use Claimants: Secondary victims (moderate/constrained) — coordination benefit from watershed protection but asymmetric extraction through development restrictions
 *   - Flood Insurance Administration: Institutional actor (institutional/mobile) — sees own process as theatrical; actuarial function has atrophied but ritual persists
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing commitment gap as inherent to human psychology rather than contingent institutional design failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence_flat_control, 0.35).
domain_priors:suppression_score(preparedness_persistence_flat_control, 0.4).
domain_priors:theater_ratio(preparedness_persistence_flat_control, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence_flat_control, extractiveness, 0.35).
narrative_ontology:constraint_metric(preparedness_persistence_flat_control, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(preparedness_persistence_flat_control, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence_flat_control, scaffold).
narrative_ontology:human_readable(preparedness_persistence_flat_control, "Flood Preparedness as Sustained Institutional Commitment Across Generations Without Catastrophe Reinforcement").
narrative_ontology:topic_domain(preparedness_persistence_flat_control, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_persistence_flat_control).
narrative_ontology:has_sunset_clause(preparedness_persistence_flat_control).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence_flat_control, 'fde1993e-b038-4b3b-abe3-49124656dc03').
narrative_ontology:cs_kernel_codification('fde1993e-b038-4b3b-abe3-49124656dc03', formalized).
narrative_ontology:cs_authority_grounding('fde1993e-b038-4b3b-abe3-49124656dc03', extraction).
narrative_ontology:cs_interpretation_layer_present('fde1993e-b038-4b3b-abe3-49124656dc03').
narrative_ontology:cs_reference_frame('fde1993e-b038-4b3b-abe3-49124656dc03', post_disaster_high_commitment).
narrative_ontology:cs_drift_state('fde1993e-b038-4b3b-abe3-49124656dc03', generation_gap_no_reinforcement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fde1993e-b038-4b3b-abe3-49124656dc03', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(preparedness_persistence_flat_control, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence_flat_control, future_generations).
narrative_ontology:constraint_beneficiary(preparedness_persistence_flat_control, civil_engineering_bureaus).
narrative_ontology:constraint_beneficiary(preparedness_persistence_flat_control, emergency_management_agencies).
narrative_ontology:constraint_victim(preparedness_persistence_flat_control, current_taxpayers).
narrative_ontology:constraint_victim(preparedness_persistence_flat_control, adjacent_land_use_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CURRENT TAXPAYERS (SNARE) — Trapped in immediate time horizon, powerless to exit mandatory tax assessment for invisible threat. Bears ongoing cost (levee maintenance, flood insurance subsidies, infrastructure upgrades) with no experiential confirmation of need. Maximum extraction: paying for future others' protection while memory of last event fades.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ADJACENT LAND USE CLAIMANTS (TANGLED ROPE) — Constrained by floodplain development restrictions and setback requirements that protect the collective but limit individual land use options. Benefits from regional flood protection coordination while bearing asymmetric cost of forgone development opportunity. Mixed coordination (shared watershed protection) and extraction (concentrated regulatory burden on floodplain-adjacent parcels).
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CIVIL ENGINEERING BUREAUS (ROPE) — Primary institutional beneficiaries with arbitrage-level exit options (can shift to other infrastructure domains). Experiences the constraint as coordination: sustained budgets, professional continuity, institutional stability. The preparedness mandate solves the genuine coordination problem of maintaining technical capacity and institutional knowledge across the gap between disasters.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EMERGENCY MANAGEMENT COALITIONS (SCAFFOLD) — Organized actors (FEMA, state emergency management agencies, insurance industry coalitions) building transitional structures with explicit sunset logic: preparedness is meant to be internalized into routine civil infrastructure maintenance and private risk assessment, not maintained as a separate perpetual mandate. Sees the constraint as temporary support for norm-building — once climate adaptation becomes routine planning rather than emergency response, the dedicated preparedness apparatus should dissolve into standard practice.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FLOOD INSURANCE ADMINISTRATION (PITON) — Sees its own process as substantially theatrical: actuarial rate-setting for 100-year events is performed but systematically underpriced due to political pressure; hazard maps are maintained but not updated at pace with development or climate shift; risk communication is ritualized but not internalized by policyholders. The verification function has atrophied — insurance performs risk transfer on paper while actual exposure accumulates unchecked. Maintained through institutional inertia and federal backstop, not functional risk pricing.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal perspective, the commitment gap between disasters is framed as an immutable feature of human temporal discounting and institutional memory decay: 'societies always forget between catastrophes; preparedness persistence is fighting human nature.' This perspective naturalizes the commitment problem as a law of collective action rather than a contingent institutional design choice. However, the structural data contradicts mountain classification — the engine will compute this as a false summit, revealing that the 'inherent to human nature' framing naturalizes what is actually a failure of institutional architecture (no iterative reinforcement mechanism, no embedded memory, no skin-in-the-game for decision-makers who won't experience the flood).
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(preparedness_persistence_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(preparedness_persistence_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(preparedness_persistence_flat_control, TR),
    TR >= 0.70.

:- end_tests(preparedness_persistence_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35 at T=20): Moderate and rising. The extraction is not from the preparedness function itself — flood protection is a genuine public good — but from the asymmetry between who pays (current generation) and who benefits (future generation), combined with institutional beneficiaries (engineering bureaus, emergency agencies) who collect rents from sustained mandate. The value reflects that much of the spending is still functional at T=20, but the rising trajectory (0.15 → 0.42 over 30 years) indicates accumulating extraction as theater displaces function. Suppression (0.40 at T=20, rising to 0.48 at T=30): Moderate-high. Significant coercion through mandatory tax assessment, compulsory flood insurance in designated zones, and land use restrictions with limited appeal mechanisms. Suppression rises over the interval as memory fades and political resistance to 'invisible' spending increases, requiring more enforcement to maintain compliance. Theater ratio (0.68 at T=20): High and accelerating. This is the constraint's diagnostic signature: theater_ratio rises from 0.35 (post-disaster, high functional activity) to 0.68 at T=20 to 0.75 at T=30, modeling Goodhart drift as preparedness activities shift from engineering function (levee maintenance, pump testing, infrastructure hardening) to performative compliance (hazard maps published but not acted upon, disaster drills without resource commitment, insurance pricing divorced from actual risk exposure). The rising theater ratio indicates piton formation — the constraint is degrading from scaffold (temporary support with sunset) toward piton (degraded function maintained as performance).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates classification divergence driven by temporal position and exit options. Current taxpayers at immediate horizon see snare — trapped, paying for invisible benefit, no experiential confirmation. Adjacent land use claimants at biographical horizon see tangled_rope — constrained by regulations that both coordinate (shared watershed) and extract (forgone development). Civil engineering bureaus at generational horizon see rope — coordination that sustains professional capacity. Emergency management coalitions see scaffold — temporary structure with sunset logic as preparedness internalizes into routine practice. Flood insurance administration sees piton — its own actuarial function has atrophied but the ritual persists. The analytical observer sees mountain — commitment gaps are natural law — but this is a false summit: the 'human nature' framing obscures the identifiable institutional beneficiaries (bureaus that gain from mandate persistence) and correctable design features (no reinforcement mechanism, no embedded memory, no skin-in-the-game for decision-makers). The perspectival gap is not 'which type is correct' but 'which structural position are you measuring from' — and the rising theater_ratio indicates the constraint is drifting from scaffold toward piton as the generation gap widens.
 *
 * DIRECTIONALITY LOGIC:
 *   Current taxpayers are victims (extraction flows away from them toward institutional beneficiaries and temporally distant future generations) with trapped exit at immediate horizon — they experience maximum extraction. Adjacent land use claimants are victims (development restrictions) but also beneficiaries (watershed protection) — mixed directionality typical of tangled_rope, with constrained exit producing moderate effective extraction. Civil engineering bureaus are beneficiaries (sustained budgets, professional continuity) with arbitrage exit — low to negative effective extraction; they experience the constraint as coordination. Emergency management agencies are beneficiaries building transitional structures — organized power and constrained exit produce moderate directionality, reflecting that they gain from current mandate but also face pressure to demonstrate sunset progress. Flood insurance administration is a beneficiary in the sense of maintaining institutional existence, but with mobile exit (can shift to other insurance domains) — the piton classification derives from the theater gate, not from high extraction. Future generations are beneficiaries but with no current agency — their interests are represented (or not) by current decision-makers, creating the temporal discount problem that drives extraction. The analytical observer's mountain perspective is a false summit: framing the commitment gap as natural law (human nature always forgets) naturalizes what is actually an institutional design failure with identifiable beneficiaries who profit from non-solution.
 *
 * MANDATROPHY ANALYSIS:
 *   SCAFFOLD WITH PITON DRIFT: The constraint resolves mandatrophy by showing that the classification is time-dependent within a single story. At T=0 (post-disaster), the constraint is a functioning scaffold: high coordination (building levees, hardening infrastructure, establishing protocols), low theater (activities are functional), clear sunset logic (preparedness internalizes into routine civil engineering once norms mature). At T=20-30 (generation gap, no reinforcing event), the constraint exhibits piton characteristics: rising theater_ratio (0.68-0.75), atrophied function (insurance pricing divorced from risk, hazard maps not updated, drills without resources), institutional inertia (bureaus maintain budgets through performance rather than engineering necessity). The mandate has outlived its acute-response function but persists because no reinforcement mechanism exists to either re-justify the commitment or trigger sunset. The mandatrophy resolution is that scaffold-to-piton drift is the constraint's actual lifecycle trajectory in the absence of catastrophe reinforcement — this is not a classification error but a description of how preparedness mandates degrade across generation gaps. The open question (omega: catastrophe_necessity_threshold) is whether institutional design can prevent this drift or whether the commitment gap is structurally unavoidable without experiential learning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophe_necessity_threshold,
    'Is catastrophe reinforcement structurally necessary for preparedness persistence, or can institutional design substitute for experiential learning?',
    'Cross-national comparison: Netherlands (continuous commitment without major flood since 1953) vs US Gulf Coast (repeated catastrophe with persistent under-preparation). Identification of design features that sustain commitment in flat periods.',
    'If catastrophe is necessary: preparedness persistence without reinforcement is aspirational scaffold, not achievable steady state — classification shifts toward piton (theater maintained between inevitable lapses). If institutional design suffices: scaffold sunset is achievable — preparedness internalizes into routine infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_necessity_threshold, empirical, 'Whether institutional design can substitute for catastrophe reinforcement').

omega_variable(
    beneficiary_temporal_discount,
    'At what temporal distance do ''future generations'' stop being a legible beneficiary class for current resource allocation decisions?',
    'Behavioral economics of intergenerational discounting; political economy analysis of infrastructure investment horizons; comparison of discount rates applied to flood preparedness vs other long-term public goods.',
    'If discount threshold < 30 years: current taxpayer extraction is lower than modeled (beneficiaries are within plausible lifetime concern). If threshold > 100 years: extraction is higher (current generation paying for abstract future with no experiential connection).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_temporal_discount, preference, 'Temporal distance at which future beneficiaries become too abstract for current commitment').

omega_variable(
    theater_vs_functional_decomposition,
    'Which components of the preparedness apparatus are functional (levees, pumps, evacuation infrastructure) vs theatrical (hazard map updates not acted upon, insurance pricing divorced from risk, disaster drills without resource commitment)?',
    'Component-by-component audit: correlation between stated function and realized outcome during actual flood events; identification of activities maintained through compliance ritual vs engineering necessity.',
    'If theater dominates: piton classification spreads from insurance to broader preparedness apparatus. If functional components dominate: scaffold classification is justified — the apparatus is building real capacity, not performing preparation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_vs_functional_decomposition, empirical, 'Functional vs theatrical decomposition of preparedness activities').

omega_variable(
    naturalization_vs_design_failure,
    'Is the commitment gap between disasters an immutable feature of human psychology (mountain) or a contingent failure of institutional architecture (snare/tangled_rope with design remedies)?',
    'Existence proof: identification of institutional designs that successfully sustain commitment across multiple generation gaps without catastrophe reinforcement (e.g., Dutch flood management post-1953, Japanese earthquake preparedness, Swiss avalanche defense). If such cases exist and are replicable, the mountain framing is a false summit.',
    'If mountain (immutable): preparedness mandates face unavoidable decay curve; policy adjusts expectations downward. If design failure: investment in institutional memory mechanisms (embedded sensors triggering automatic review, legal liability extending across administration changes, financial instruments making future costs present) can solve the commitment problem.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalization_vs_design_failure, conceptual, 'Whether commitment gap is natural law or institutional design failure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence_flat_control, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_persist_theater_post_disaster, preparedness_persistence_flat_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(prep_persist_tr_t10, preparedness_persistence_flat_control, theater_ratio, 10, 0.52).
narrative_ontology:measurement(prep_persist_tr_t20, preparedness_persistence_flat_control, theater_ratio, 20, 0.68).
narrative_ontology:measurement(prep_persist_tr_t30, preparedness_persistence_flat_control, theater_ratio, 30, 0.75).

% Extraction over time
narrative_ontology:measurement(prep_persist_extraction_post_disaster, preparedness_persistence_flat_control, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(prep_persist_be_t10, preparedness_persistence_flat_control, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(prep_persist_be_t20, preparedness_persistence_flat_control, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(prep_persist_be_t30, preparedness_persistence_flat_control, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(prep_persist_suppression_post_disaster, preparedness_persistence_flat_control, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(prep_persist_su_t10, preparedness_persistence_flat_control, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(prep_persist_su_t20, preparedness_persistence_flat_control, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(prep_persist_su_t30, preparedness_persistence_flat_control, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence_flat_control, resource_allocation).
narrative_ontology:affects_constraint(preparedness_persistence_flat_control, intergenerational_climate_adaptation).
narrative_ontology:affects_constraint(preparedness_persistence_flat_control, pandemic_preparedness_mandates).
narrative_ontology:affects_constraint(preparedness_persistence_flat_control, nuclear_waste_disposal_commitment).

% DUAL FORMULATION NOTE:
% Flood preparedness is the canonical case of a broader pattern: long-horizon public goods with temporally distant beneficiaries and generation-gap commitment problems. The upstream constraints (climate adaptation, pandemic preparedness, nuclear waste) share the same structural features — extraction from current generation, benefit to future generation, institutional beneficiaries in the gap, rising theater as memory fades. Network edges model that preparedness mandate designs inform each other, and that commitment failures propagate across domains (if flood preparedness decays to theater, other long-horizon mandates face legitimacy erosion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
